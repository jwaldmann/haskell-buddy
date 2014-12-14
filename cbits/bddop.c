/*========================================================================
               Copyright (C) 1996-2002 by Jorn Lind-Nielsen
                            All rights reserved

    Permission is hereby granted, without written agreement and without
    license or royalty fees, to use, reproduce, prepare derivative
    works, distribute, and display this software and its documentation
    for any purpose, provided that (1) the above copyright notice and
    the following two paragraphs appear in all copies of the source code
    and (2) redistributions, including without limitation binaries,
    reproduce these notices in the supporting documentation. Substantial
    modifications to this software may be copyrighted by their authors
    and need not follow the licensing terms described here, provided
    that the new terms are clearly indicated in all files where they apply.

    IN NO EVENT SHALL JORN LIND-NIELSEN, OR DISTRIBUTORS OF THIS
    SOFTWARE BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT, SPECIAL,
    INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OF THIS
    SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE AUTHORS OR ANY OF THE
    ABOVE PARTIES HAVE BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

    JORN LIND-NIELSEN SPECIFICALLY DISCLAIM ANY WARRANTIES, INCLUDING,
    BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
    FITNESS FOR A PARTICULAR PURPOSE. THE SOFTWARE PROVIDED HEREUNDER IS
    ON AN "AS IS" BASIS, AND THE AUTHORS AND DISTRIBUTORS HAVE NO
    OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
    MODIFICATIONS.
========================================================================*/

/*************************************************************************
  $Header$
  FILE:  bddop.c
  DESCR: BDD operators
  AUTH:  Jorn Lind
  DATE:  (C) nov 1997
*************************************************************************/

#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include <assert.h>
#include "config.h"
#include "kernel.h"
#include "cache.h"

   /* Hash value modifiers to distinguish between entries in misccache */
#define CACHEID_CONSTRAIN   0x0
#define CACHEID_RESTRICT    0x1
#define CACHEID_SATCOU      0x2
#define CACHEID_SATCOULN    0x3
#define CACHEID_PATHCOU     0x4

   /* Hash value modifiers for replace/compose */
#define CACHEID_REPLACE      0x0
#define CACHEID_COMPOSE      0x1
#define CACHEID_VECCOMPOSE   0x2

   /* Hash value modifiers for quantification */
#define CACHEID_EXIST        0x0
#define CACHEID_FORALL       0x1
#define CACHEID_UNIQUE       0x2
#define CACHEID_APPEX        0x3
#define CACHEID_APPAL        0x4
#define CACHEID_APPUN        0x5


   /* Number of boolean operators */
#define OPERATOR_NUM    11

   /* Operator results - entry = left<<1 | right  (left,right in {0,1}) */
static int oprres[OPERATOR_NUM][4] = 
{ {0,0,0,1},  /* and                       ( & )         */
  {0,1,1,0},  /* xor                       ( ^ )         */
  {0,1,1,1},  /* or                        ( | )         */
  {1,1,1,0},  /* nand                                    */
  {1,0,0,0},  /* nor                                     */
  {1,1,0,1},  /* implication               ( >> )        */
  {1,0,0,1},  /* bi-implication                          */
  {0,0,1,0},  /* difference /greater than  ( - ) ( > )   */
  {0,1,0,0},  /* less than                 ( < )         */
  {1,0,1,1},  /* inverse implication       ( << )        */
  {1,1,0,0}   /* not                       ( ! )         */
};


   /* Variables needed for the operators */
static int applyop;                 /* Current operator for apply */
static int appexop;                 /* Current operator for appex */
static int appexid;                 /* Current cache id for appex */
static int quantid;                 /* Current cache id for quantifications */
static int *quantvarset;            /* Current variable set for quant. */
static int quantvarsetID;           /* Current id used in quantvarset */
static int quantlast;               /* Current last variable to be quant. */
static int replaceid;               /* Current cache id for replace */
static int *replacepair;            /* Current replace pair */
static int replacelast;             /* Current last var. level to replace */
static int composelevel;            /* Current variable used for compose */
static int miscid;                  /* Current cache id for other results */
static int *varprofile;             /* Current variable profile */
static int supportID;               /* Current ID (true value) for support */
static int supportMin;              /* Min. used level in support calc. */
static int supportMax;              /* Max. used level in support calc. */
static int* supportSet;             /* The found support set */
static BddCache applycache;         /* Cache for apply results */
static BddCache itecache;           /* Cache for ITE results */
static BddCache quantcache;         /* Cache for exist/forall results */
static BddCache appexcache;         /* Cache for appex/appall results */
static BddCache replacecache;       /* Cache for replace results */
static BddCache misccache;          /* Cache for other results */
static int cacheratio;
static BDD satPolarity;
static int firstReorder;            /* Used instead of local variable in order
				       to avoid compiler warning about 'first'
				       being clobbered by setjmp */

static char*            allsatProfile; /* Variable profile for bdd_allsat() */
static bddallsathandler allsatHandler; /* Callback handler for bdd_allsat() */

extern bddCacheStat bddcachestats;

   /* Internal prototypes */
static BDD    not_rec(BDD);
static BDD    apply_rec(BDD, BDD);
static BDD    ite_rec(BDD, BDD, BDD);
static int    simplify_rec(BDD, BDD);
static int    quant_rec(int);
static int    appquant_rec(int, int);
static int    restrict_rec(int);
static BDD    constrain_rec(BDD, BDD);
static BDD    replace_rec(BDD);
static BDD    bdd_correctify(int, BDD, BDD);
static BDD    compose_rec(BDD, BDD);
static BDD    veccompose_rec(BDD);
static void   support_rec(int, int*);
static BDD    satone_rec(BDD);
static BDD    satoneset_rec(BDD, BDD);
static int    fullsatone_rec(int);
static void   allsat_rec(BDD r);
static double satcount_rec(int);
static double satcountln_rec(int);
static void   varprofile_rec(int);
static double bdd_pathcount_rec(BDD);
static int    varset2vartable(BDD);
static int    varset2svartable(BDD);


   /* Hashvalues */
#define NOTHASH(r)           (r)
#define APPLYHASH(l,r,op)    (TRIPLE(l,r,op))
#define ITEHASH(f,g,h)       (TRIPLE(f,g,h))
#define RESTRHASH(r,var)     (PAIR(r,var))
#define CONSTRAINHASH(f,c)   (PAIR(f,c))
#define QUANTHASH(r)         (r)
#define REPLACEHASH(r)       (r)
#define VECCOMPOSEHASH(f)    (f)
#define COMPOSEHASH(f,g)     (PAIR(f,g))
#define SATCOUHASH(r)        (r)
#define PATHCOUHASH(r)       (r)
#define APPEXHASH(l,r,op)    (PAIR(l,r))

#ifndef M_LN2
#define M_LN2 0.69314718055994530942
#endif

#define log1p(a) (log(1.0+a))

#define INVARSET(a) (quantvarset[a] == quantvarsetID) /* unsigned check */
#define INSVARSET(a) (abs(quantvarset[a]) == quantvarsetID) /* signed check */

/*************************************************************************
  Setup and shutdown
*************************************************************************/

int bdd_operator_init(int cachesize)
{
   if (BddCache_init(&applycache,cachesize) < 0)
      return bdd_error(BDD_MEMORY);
   
   if (BddCache_init(&itecache,cachesize) < 0)
      return bdd_error(BDD_MEMORY);
   
   if (BddCache_init(&quantcache,cachesize) < 0)
      return bdd_error(BDD_MEMORY);

   if (BddCache_init(&appexcache,cachesize) < 0)
      return bdd_error(BDD_MEMORY);

   if (BddCache_init(&replacecache,cachesize) < 0)
      return bdd_error(BDD_MEMORY);

   if (BddCache_init(&misccache,cachesize) < 0)
      return bdd_error(BDD_MEMORY);

   quantvarsetID = 0;
   quantvarset = NULL;
   cacheratio = 0;
   supportSet = NULL;
   
   return 0;
}


void bdd_operator_done(void)
{
   if (quantvarset != NULL)
      free(quantvarset);
   quantvarset = NULL;
   
   BddCache_done(&applycache);
   BddCache_done(&itecache);
   BddCache_done(&quantcache);
   BddCache_done(&appexcache);
   BddCache_done(&replacecache);
   BddCache_done(&misccache);

   if (supportSet != NULL)
     free(supportSet);
   supportSet = NULL;
}


void bdd_operator_reset(void)
{
   BddCache_reset(&applycache);
   BddCache_reset(&itecache);
   BddCache_reset(&quantcache);
   BddCache_reset(&appexcache);
   BddCache_reset(&replacecache);
   BddCache_reset(&misccache);
}


void bdd_operator_varresize(void)
{
   if (quantvarset != NULL)
      free(quantvarset);

   if ((quantvarset=NEW(int,bddvarnum)) == NULL)
      bdd_error(BDD_MEMORY);

   memset(quantvarset, 0, sizeof(int)*bddvarnum);
   quantvarsetID = 0;

   /* Bug fix: */
   /* After changing number of vars, satcount results are no longer valid. */
   BddCache_reset(&misccache);
}


static void bdd_operator_noderesize(void)
{
   if (cacheratio > 0)
   {
      int newcachesize = bddnodesize / cacheratio;
      
      BddCache_resize(&applycache, newcachesize);
      BddCache_resize(&itecache, newcachesize);
      BddCache_resize(&quantcache, newcachesize);
      BddCache_resize(&appexcache, newcachesize);
      BddCache_resize(&replacecache, newcachesize);
      BddCache_resize(&misccache, newcachesize);
   }
}


/*************************************************************************
  Other
*************************************************************************/

int bdd_setcacheratio(int r)
{
   int old = cacheratio;
   
   if (r <= 0)
      return bdd_error(BDD_RANGE);
   if (bddnodesize == 0)
      return old;
   
   cacheratio = r;
   bdd_operator_noderesize();
   return old;
}


/*************************************************************************
  Operators
*************************************************************************/

static void checkresize(void)
{
   if (bddresized)
      bdd_operator_noderesize();
   bddresized = 0;
}


/*=== BUILD A CUBE =====================================================*/

BDD bdd_buildcube(int value, int width, BDD *variables)
{
   BDD result = BDDONE;
   int z;

   for (z=0 ; z<width ; z++, value>>=1)
   {
      BDD tmp;
      BDD v;
      
      if (value & 0x1)
	 v = bdd_addref( variables[width-z-1] );
      else
	 v = bdd_addref( bdd_not(variables[width-z-1]) );

      bdd_addref(result);
      tmp = bdd_apply(result,v,bddop_and);
      bdd_delref(result);
      bdd_delref(v);

      result = tmp;
   }

   return result;
}


BDD bdd_ibuildcube(int value, int width, int *variables)
{
   BDD result = BDDONE;
   int z;

   for (z=0 ; z<width ; z++, value>>=1)
   {
      BDD tmp;
      BDD v;
      
      if (value & 0x1)
	 v = bdd_ithvar(variables[width-z-1]);
      else
	 v = bdd_nithvar(variables[width-z-1]);

      bdd_addref(result);
      tmp = bdd_apply(result,v,bddop_and);
      bdd_delref(result);

      result = tmp;
   }

   return result;
}


/*=== NOT ==============================================================*/

BDD bdd_not(BDD r)
{
   BDD res;
   firstReorder = 1;
   CHECKa(r, bddfalse);

 again:
   if (setjmp(bddexception) == 0)
   {
      INITREF;
      
      if (!firstReorder)
	 bdd_disable_reorder();
      res = not_rec(r);
      if (!firstReorder)
	 bdd_enable_reorder();
   }
   else
   {
      bdd_checkreorder();
      if (firstReorder-- == 1)
	 goto again;
      res = BDDZERO;  /* avoid warning about res being uninitialized */
   }

   checkresize();
   return res;
}


static BDD not_rec(BDD r)
{
   BddCacheData *entry;
   BDD res;

   if (ISZERO(r))
      return BDDONE;
   if (ISONE(r))
      return BDDZERO;
   
   entry = BddCache_lookup(&applycache, NOTHASH(r));
      
   if (entry->a == r  &&  entry->c == bddop_not)
   {
#ifdef CACHESTATS
      bddcachestats.opHit++;
#endif
      return entry->r.res;
   }
#ifdef CACHESTATS
   bddcachestats.opMiss++;
#endif
   
   PUSHREF( not_rec(LOW(r)) );
   PUSHREF( not_rec(HIGH(r)) );
   res = bdd_makenode(LEVEL(r), READREF(2), READREF(1));
   POPREF(2);
   
   entry->a = r;
   entry->c = bddop_not;
   entry->r.res = res;

   return res;
}


/*=== APPLY ============================================================*/

BDD bdd_apply(BDD l, BDD r, int op)
{
   BDD res;
   firstReorder = 1;
   
   CHECKa(l, bddfalse);
   CHECKa(r, bddfalse);

   if (op<0 || op>bddop_invimp)
   {
      bdd_error(BDD_OP);
      return bddfalse;
   }

 again:
   if (setjmp(bddexception) == 0)
   {
      INITREF;
      applyop = op;
      
      if (!firstReorder)
	 bdd_disable_reorder();
      res = apply_rec(l, r);
      if (!firstReorder)
	 bdd_enable_reorder();
   }
   else
   {
      bdd_checkreorder();

      if (firstReorder-- == 1)
	 goto again;
      res = BDDZERO;  /* avoid warning about res being uninitialized */
   }
   
   checkresize();
   return res;
}


static BDD apply_rec(BDD l, BDD r)
{
   BddCacheData *entry;
   BDD res;
   
   switch (applyop)
   {
    case bddop_and:
       if (l == r)
	  return l;
       if (ISZERO(l)  ||  ISZERO(r))
	  return 0;
       if (ISONE(l))
	  return r;
       if (ISONE(r))
	  return l;
       break;
    case bddop_or:
       if (l == r)
	  return l;
       if (ISONE(l)  ||  ISONE(r))
	  return 1;
       if (ISZERO(l))
	  return r;
       if (ISZERO(r))
	  return l;
       break;
    case bddop_xor:
       if (l == r)
	  return 0;
       if (ISZERO(l))
	  return r;
       if (ISZERO(r))
	  return l;
       break;
    case bddop_nand:
       if (ISZERO(l) || ISZERO(r))
	  return 1;
       break;
    case bddop_nor:
       if (ISONE(l)  ||  ISONE(r))
	  return 0;
       break;
   case bddop_imp:
      if (ISZERO(l))
	 return 1;
      if (ISONE(l))
	 return r;
      if (ISONE(r))
	 return 1;
      break;
   }

   if (ISCONST(l)  &&  ISCONST(r))
      res = oprres[applyop][l<<1 | r];
   else
   {
      entry = BddCache_lookup(&applycache, APPLYHASH(l,r,applyop));
      
      if (entry->a == l  &&  entry->b == r  &&  entry->c == applyop)
      {
#ifdef CACHESTATS
	 bddcachestats.opHit++;
#endif
	 return entry->r.res;
      }
#ifdef CACHESTATS
      bddcachestats.opMiss++;
#endif
      
      if (LEVEL(l) == LEVEL(r))
      {
	 PUSHREF( apply_rec(LOW(l), LOW(r)) );
	 PUSHREF( apply_rec(HIGH(l), HIGH(r)) );
	 res = bdd_makenode(LEVEL(l), READREF(2), READREF(1));
      }
      else
      if (LEVEL(l) < LEVEL(r))
      {
	 PUSHREF( apply_rec(LOW(l), r) );
	 PUSHREF( apply_rec(HIGH(l), r) );
	 res = bdd_makenode(LEVEL(l), READREF(2), READREF(1));
      }
      else
      {
	 PUSHREF( apply_rec(l, LOW(r)) );
	 PUSHREF( apply_rec(l, HIGH(r)) );
	 res = bdd_makenode(LEVEL(r), READREF(2), READREF(1));
      }

      POPREF(2);

      entry->a = l;
      entry->b = r;
      entry->c = applyop;
      entry->r.res = res;
   }

   return res;
}


BDD bdd_and(BDD l, BDD r)
{
   return bdd_apply(l,r,bddop_and);
}


BDD bdd_or(BDD l, BDD r)
{
   return bdd_apply(l,r,bddop_or);
}


BDD bdd_xor(BDD l, BDD r)
{
   return bdd_apply(l,r,bddop_xor);
}


BDD bdd_imp(BDD l, BDD r)
{
   return bdd_apply(l,r,bddop_imp);
}


BDD bdd_biimp(BDD l, BDD r)
{
   return bdd_apply(l,r,bddop_biimp);
}


/*=== ITE ==============================================================*/

BDD bdd_ite(BDD f, BDD g, BDD h)
{
   BDD res;
   firstReorder = 1;
   
   CHECKa(f, bddfalse);
   CHECKa(g, bddfalse);
   CHECKa(h, bddfalse);

 again:
   if (setjmp(bddexception) == 0)
   {
      INITREF;
      
      if (!firstReorder)
	 bdd_disable_reorder();
      res = ite_rec(f,g,h);
      if (!firstReorder)
	 bdd_enable_reorder();
   }
   else
   {
      bdd_checkreorder();

      if (firstReorder-- == 1)
	 goto again;
      res = BDDZERO;  /* avoid warning about res being uninitialized */
   }

   checkresize();
   return res;
}


static BDD ite_rec(BDD f, BDD g, BDD h)
{
   BddCacheData *entry;
   BDD res;

   if (ISONE(f))
      return g;
   if (ISZERO(f))
      return h;
   if (g == h)
      return g;
   if (ISONE(g) && ISZERO(h))
      return f;
   if (ISZERO(g) && ISONE(h))
      return not_rec(f);

   entry = BddCache_lookup(&itecache, ITEHASH(f,g,h));
   if (entry->a == f  &&  entry->b == g  &&  entry->c == h)
   {
#ifdef CACHESTATS
      bddcachestats.opHit++;
#endif
      return entry->r.res;
   }
#ifdef CACHESTATS
   bddcachestats.opMiss++;
#endif
      
   if (LEVEL(f) == LEVEL(g))
   {
      if (LEVEL(f) == LEVEL(h))
      {
	 PUSHREF( ite_rec(LOW(f), LOW(g), LOW(h)) );
	 PUSHREF( ite_rec(HIGH(f), HIGH(g), HIGH(h)) );
	 res = bdd_makenode(LEVEL(f), READREF(2), READREF(1));
      }
      else
      if (LEVEL(f) < LEVEL(h))
      {
	 PUSHREF( ite_rec(LOW(f), LOW(g), h) );
	 PUSHREF( ite_rec(HIGH(f), HIGH(g), h) );
	 res = bdd_makenode(LEVEL(f), READREF(2), READREF(1));
      }
      else /* f > h */
      {
	 PUSHREF( ite_rec(f, g, LOW(h)) );
	 PUSHREF( ite_rec(f, g, HIGH(h)) );
	 res = bdd_makenode(LEVEL(h), READREF(2), READREF(1));
      }
   }
   else
   if (LEVEL(f) < LEVEL(g))
   {
      if (LEVEL(f) == LEVEL(h))
      {
	 PUSHREF( ite_rec(LOW(f), g, LOW(h)) );
	 PUSHREF( ite_rec(HIGH(f), g, HIGH(h)) );
	 res = bdd_makenode(LEVEL(f), READREF(2), READREF(1));
      }
      else
      if (LEVEL(f) < LEVEL(h))
      {
	 PUSHREF( ite_rec(LOW(f), g, h) );
	 PUSHREF( ite_rec(HIGH(f), g, h) );
	 res = bdd_makenode(LEVEL(f), READREF(2), READREF(1));
      }
      else /* f > h */
      {
	 PUSHREF( ite_rec(f, g, LOW(h)) );
	 PUSHREF( ite_rec(f, g, HIGH(h)) );
	 res = bdd_makenode(LEVEL(h), READREF(2), READREF(1));
      }
   }
   else /* f > g */
   {
      if (LEVEL(g) == LEVEL(h))
      {
	 PUSHREF( ite_rec(f, LOW(g), LOW(h)) );
	 PUSHREF( ite_rec(f, HIGH(g), HIGH(h)) );
	 res = bdd_makenode(LEVEL(g), READREF(2), READREF(1));
      }
      else
      if (LEVEL(g) < LEVEL(h))
      {
	 PUSHREF( ite_rec(f, LOW(g), h) );
	 PUSHREF( ite_rec(f, HIGH(g), h) );
	 res = bdd_makenode(LEVEL(g), READREF(2), READREF(1));
      }
      else /* g > h */
      {
	 PUSHREF( ite_rec(f, g, LOW(h)) );
	 PUSHREF( ite_rec(f, g, HIGH(h)) );
	 res = bdd_makenode(LEVEL(h), READREF(2), READREF(1));
      }
   }

   POPREF(2);

   entry->a = f;
   entry->b = g;
   entry->c = h;
   entry->r.res = res;

   return res;
}


/*=== RESTRICT =========================================================*/

BDD bdd_restrict(BDD r, BDD var)
{
   BDD res;
   firstReorder = 1;
   
   CHECKa(r,bddfalse);
   CHECKa(var,bddfalse);
   
   if (var < 2)  /* Empty set */
      return r;
   
 again:
   if (setjmp(bddexception) == 0)
   {
      if (varset2svartable(var) < 0)
	 return bddfalse;

      INITREF;
      miscid = (var << 3) | CACHEID_RESTRICT;
      
      if (!firstReorder)
	 bdd_disable_reorder();
      res = restrict_rec(r);
      if (!firstReorder)
	 bdd_enable_reorder();
   }
   else
   {
      bdd_checkreorder();

      if (firstReorder-- == 1)
	 goto again;
      res = BDDZERO;  /* avoid warning about res being uninitialized */
   }

   checkresize();
   return res;
}


static int restrict_rec(int r)
{
   BddCacheData *entry;
   int res;
   
   if (ISCONST(r)  ||  LEVEL(r) > quantlast)
      return r;

   entry = BddCache_lookup(&misccache, RESTRHASH(r,miscid));
   if (entry->a == r  &&  entry->c == miscid)
   {
#ifdef CACHESTATS
      bddcachestats.opHit++;
#endif
      return entry->r.res;
   }
#ifdef CACHESTATS
   bddcachestats.opMiss++;
#endif
   
   if (INSVARSET(LEVEL(r)))
   {
      if (quantvarset[LEVEL(r)] > 0)
	 res = restrict_rec(HIGH(r));
      else
	 res = restrict_rec(LOW(r));
   }
   else
   {
      PUSHREF( restrict_rec(LOW(r)) );
      PUSHREF( restrict_rec(HIGH(r)) );
      res = bdd_makenode(LEVEL(r), READREF(2), READREF(1));
      POPREF(2);
   }

   entry->a = r;
   entry->c = miscid;
   entry->r.res = res;

   return res;
}


/*=== GENERALIZED COFACTOR =============================================*/

BDD bdd_constrain(BDD f, BDD c)
{
   BDD res;
   firstReorder = 1;
   
   CHECKa(f,bddfalse);
   CHECKa(c,bddfalse);
   
 again:
   if (setjmp(bddexception) == 0)
   {
      INITREF;
      miscid = CACHEID_CONSTRAIN;
      
      if (!firstReorder)
	 bdd_disable_reorder();
      res = constrain_rec(f, c);
      if (!firstReorder)
	 bdd_enable_reorder();
   }
   else
   {
      bdd_checkreorder();

      if (firstReorder-- == 1)
	 goto again;
      res = BDDZERO;  /* avoid warning about res being uninitialized */
   }

   checkresize();
   return res;
}


static BDD constrain_rec(BDD f, BDD c)
{
   BddCacheData *entry;
   BDD res;

   if (ISONE(c))
      return f;
   if (ISCONST(f))
      return f;
   if (c == f)
      return BDDONE;
   if (ISZERO(c))
      return BDDZERO;

   entry = BddCache_lookup(&misccache, CONSTRAINHASH(f,c));
   if (entry->a == f  &&  entry->b == c  &&  entry->c == miscid)
   {
#ifdef CACHESTATS
      bddcachestats.opHit++;
#endif
      return entry->r.res;
   }
#ifdef CACHESTATS
   bddcachestats.opMiss++;
#endif

   if (LEVEL(f) == LEVEL(c))
   {
      if (ISZERO(LOW(c)))
	 res = constrain_rec(HIGH(f), HIGH(c));
      else if (ISZERO(HIGH(c)))
	 res = constrain_rec(LOW(f), LOW(c));
      else
      {
	 PUSHREF( constrain_rec(LOW(f), LOW(c)) );
	 PUSHREF( constrain_rec(HIGH(f), HIGH(c)) );
	 res = bdd_makenode(LEVEL(f), READREF(2), READREF(1));
	 POPREF(2);
      }
   }
   else
   if (LEVEL(f) < LEVEL(c))
   {
      PUSHREF( constrain_rec(LOW(f), c) );
      PUSHREF( constrain_rec(HIGH(f), c) );
      res = bdd_makenode(LEVEL(f), READREF(2), READREF(1));
      POPREF(2);
   }
   else
   {
      if (ISZERO(LOW(c)))
	 res = constrain_rec(f, HIGH(c));
      else if (ISZERO(HIGH(c)))
	 res = constrain_rec(f, LOW(c));
      else
      {
	 PUSHREF( constrain_rec(f, LOW(c)) );
	 PUSHREF( constrain_rec(f, HIGH(c)) );
	 res = bdd_makenode(LEVEL(c), READREF(2), READREF(1));
	 POPREF(2);
      }
   }

   entry->a = f;
   entry->b = c;
   entry->c = miscid;
   entry->r.res = res;

   return res;
}


/*=== REPLACE ==========================================================*/

BDD bdd_replace(BDD r, bddPair *pair)
{
   BDD res;
   firstReorder = 1;
   
   CHECKa(r, bddfalse);
   
 again:
   if (setjmp(bddexception) == 0)
   {
      INITREF;
      replacepair = pair->result;
      replacelast = pair->last;
      replaceid = (pair->id << 2) | CACHEID_REPLACE;
      
      if (!firstReorder)
	 bdd_disable_reorder();
      res = replace_rec(r);
      if (!firstReorder)
	 bdd_enable_reorder();
   }
   else
   {
      bdd_checkreorder();

      if (firstReorder-- == 1)
	 goto again;
      res = BDDZERO;  /* avoid warning about res being uninitialized */
   }

   checkresize();
   return res;
}


static BDD replace_rec(BDD r)
{
   BddCacheData *entry;
   BDD res;
   
   if (ISCONST(r)  ||  LEVEL(r) > replacelast)
      return r;

   entry = BddCache_lookup(&replacecache, REPLACEHASH(r));
   if (entry->a == r  &&  entry->c == replaceid)
   {
#ifdef CACHESTATS
      bddcachestats.opHit++;
#endif
      return entry->r.res;
   }
#ifdef CACHESTATS
   bddcachestats.opMiss++;
#endif

   PUSHREF( replace_rec(LOW(r)) );
   PUSHREF( replace_rec(HIGH(r)) );

   res = bdd_correctify(LEVEL(replacepair[LEVEL(r)]), READREF(2), READREF(1));
   POPREF(2);

   entry->a = r;
   entry->c = replaceid;
   entry->r.res = res;

   return res;
}


static BDD bdd_correctify(int level, BDD l, BDD r)
{
   BDD res;
   
   if (level < LEVEL(l)  &&  level < LEVEL(r))
      return bdd_makenode(level, l, r);

   if (level == LEVEL(l)  ||  level == LEVEL(r))
   {
      bdd_error(BDD_REPLACE);
      return 0;
   }

   if (LEVEL(l) == LEVEL(r))
   {
      PUSHREF( bdd_correctify(level, LOW(l), LOW(r)) );
      PUSHREF( bdd_correctify(level, HIGH(l), HIGH(r)) );
      res = bdd_makenode(LEVEL(l), READREF(2), READREF(1));
   }
   else
   if (LEVEL(l) < LEVEL(r))
   {
      PUSHREF( bdd_correctify(level, LOW(l), r) );
      PUSHREF( bdd_correctify(level, HIGH(l), r) );
      res = bdd_makenode(LEVEL(l), READREF(2), READREF(1));
   }
   else
   {
      PUSHREF( bdd_correctify(level, l, LOW(r)) );
      PUSHREF( bdd_correctify(level, l, HIGH(r)) );
      res = bdd_makenode(LEVEL(r), READREF(2), READREF(1));
   }
   POPREF(2);
   
   return res; /* FIXME: cache ? */
}


/*=== COMPOSE ==========================================================*/

BDD bdd_compose(BDD f, BDD g, int var)
{
   BDD res;
   firstReorder = 1;
   
   CHECKa(f, bddfalse);
   CHECKa(g, bddfalse);
   if (var < 0 || var >= bddvarnum)
   {
      bdd_error(BDD_VAR);
      return bddfalse;
   }
   
 again:
   if (setjmp(bddexception) == 0)
   {
      INITREF;
      composelevel = bddvar2level[var];
      replaceid = (composelevel << 2) | CACHEID_COMPOSE;
      
      if (!firstReorder)
	 bdd_disable_reorder();
      res = compose_rec(f, g);
      if (!firstReorder)
	 bdd_enable_reorder();
   }
   else
   {
      bdd_checkreorder();

      if (firstReorder-- == 1)
	 goto again;
      res = BDDZERO;  /* avoid warning about res being uninitialized */
   }

   checkresize();
   return res;
}


static BDD compose_rec(BDD f, BDD g)
{
   BddCacheData *entry;
   BDD res;

   if (LEVEL(f) > composelevel)
      return f;

   entry = BddCache_lookup(&replacecache, COMPOSEHASH(f,g));
   if (entry->a == f  &&  entry->b == g  &&  entry->c == replaceid)
   {
#ifdef CACHESTATS
      bddcachestats.opHit++;
#endif
      return entry->r.res;
   }
#ifdef CACHESTATS
   bddcachestats.opMiss++;
#endif

   if (LEVEL(f) < composelevel)
   {
      if (LEVEL(f) == LEVEL(g))
      {
	 PUSHREF( compose_rec(LOW(f), LOW(g)) );
	 PUSHREF( compose_rec(HIGH(f), HIGH(g)) );
	 res = bdd_makenode(LEVEL(f), READREF(2), READREF(1));
      }
      else
      if (LEVEL(f) < LEVEL(g))
      {
	 PUSHREF( compose_rec(LOW(f), g) );
	 PUSHREF( compose_rec(HIGH(f), g) );
	 res = bdd_makenode(LEVEL(f), READREF(2), READREF(1));
      }
      else
      {
	 PUSHREF( compose_rec(f, LOW(g)) );
	 PUSHREF( compose_rec(f, HIGH(g)) );
	 res = bdd_makenode(LEVEL(g), READREF(2), READREF(1));
      }
      POPREF(2);
   }
   else
      /*if (LEVEL(f) == composelevel) changed 2-nov-98 */
   {
      res = ite_rec(g, HIGH(f), LOW(f));
   }

   entry->a = f;
   entry->b = g;
   entry->c = replaceid;
   entry->r.res = res;

   return res;
}


BDD bdd_veccompose(BDD f, bddPair *pair)
{
   BDD res;
   firstReorder = 1;
   
   CHECKa(f, bddfalse);
   
 again:
   if (setjmp(bddexception) == 0)
   {
      INITREF;
      replacepair = pair->result;
      replaceid = (pair->id << 2) | CACHEID_VECCOMPOSE;
      replacelast = pair->last;
      
      if (!firstReorder)
	 bdd_disable_reorder();
      res = veccompose_rec(f);
      if (!firstReorder)
	 bdd_enable_reorder();
   }
   else
   {
      bdd_checkreorder();

      if (firstReorder-- == 1)
	 goto again;
      res = BDDZERO;  /* avoid warning about res being uninitialized */
   }

   checkresize();
   return res;
}


static BDD veccompose_rec(BDD f)
{
   BddCacheData *entry;
   register BDD res;
   
   if (LEVEL(f) > replacelast)
      return f;
   
   entry = BddCache_lookup(&replacecache, VECCOMPOSEHASH(f));
   if (entry->a == f  &&  entry->c == replaceid)
   {
#ifdef CACHESTATS
      bddcachestats.opHit++;
#endif
      return entry->r.res;
   }
#ifdef CACHESTATS
   bddcachestats.opMiss++;
#endif

   PUSHREF( veccompose_rec(LOW(f)) );
   PUSHREF( veccompose_rec(HIGH(f)) );
   res = ite_rec(replacepair[LEVEL(f)], READREF(1), READREF(2));
   POPREF(2);

   entry->a = f;
   entry->c = replaceid;
   entry->r.res = res;

   return res;
}


/*=== SIMPLIFY =========================================================*/

BDD bdd_simplify(BDD f, BDD d)
{
   BDD res;
   firstReorder = 1;
   
   CHECKa(f, bddfalse);
   CHECKa(d, bddfalse);
   
 again:
   if (setjmp(bddexception) == 0)
   {
      INITREF;
      applyop = bddop_or;
      
      if (!firstReorder)
	 bdd_disable_reorder();
      res = simplify_rec(f, d);
      if (!firstReorder)
	 bdd_enable_reorder();
   }
   else
   {
      bdd_checkreorder();

      if (firstReorder-- == 1)
	 goto again;
      res = BDDZERO;  /* avoid warning about res being uninitialized */
   }

   checkresize();
   return res;
}


static BDD simplify_rec(BDD f, BDD d)
{
   BddCacheData *entry;
   BDD res;

   if (ISONE(d)  ||  ISCONST(f))
      return f;
   if (d == f)
      return BDDONE;
   if (ISZERO(d))
      return BDDZERO;

   entry = BddCache_lookup(&applycache, APPLYHASH(f,d,bddop_simplify));
   
   if (entry->a == f  &&  entry->b == d  &&  entry->c == bddop_simplify)
   {
#ifdef CACHESTATS
      bddcachestats.opHit++;
#endif
      return entry->r.res;
   }
#ifdef CACHESTATS
   bddcachestats.opMiss++;
#endif
   
   if (LEVEL(f) == LEVEL(d))
   {
      if (ISZERO(LOW(d)))
	 res = simplify_rec(HIGH(f), HIGH(d));
      else
      if (ISZERO(HIGH(d)))
	 res = simplify_rec(LOW(f), LOW(d));
      else
      {
	 PUSHREF( simplify_rec(LOW(f),	LOW(d)) );
	 PUSHREF( simplify_rec(HIGH(f), HIGH(d)) );
	 res = bdd_makenode(LEVEL(f), READREF(2), READREF(1));
	 POPREF(2);
      }
   }
   else
   if (LEVEL(f) < LEVEL(d))
   {
      PUSHREF( simplify_rec(LOW(f), d) );
      PUSHREF( simplify_rec(HIGH(f), d) );
      res = bdd_makenode(LEVEL(f), READREF(2), READREF(1));
      POPREF(2);
   }
   else /* LEVEL(d) < LEVEL(f) */
   {
      PUSHREF( apply_rec(LOW(d), HIGH(d)) ); /* Exist quant */
      res = simplify_rec(f, READREF(1));
      POPREF(1);
   }

   entry->a = f;
   entry->b = d;
   entry->c = bddop_simplify;
   entry->r.res = res;

   return res;
}


/*=== QUANTIFICATION ===================================================*/

BDD bdd_exist(BDD r, BDD var)
{
   BDD res;
   firstReorder = 1;
   
   CHECKa(r, bddfalse);
   CHECKa(var, bddfalse);
   
   if (var < 2)  /* Empty set */
      return r;

 again:
   if (setjmp(bddexception) == 0)
   {
      if (varset2vartable(var) < 0)
	 return bddfalse;

      INITREF;
      quantid = (var << 3) | CACHEID_EXIST; /* FIXME: range */
      applyop = bddop_or;

      if (!firstReorder)
	 bdd_disable_reorder();
      res = quant_rec(r);
      if (!firstReorder)
	 bdd_enable_reorder();
   }
   else
   {
      bdd_checkreorder();

      if (firstReorder-- == 1)
	 goto again;
      res = BDDZERO;  /* avoid warning about res being uninitialized */
   }

   checkresize();
   return res;
}


BDD bdd_forall(BDD r, BDD var)
{
   BDD res;
   firstReorder = 1;
   
   CHECKa(r, bddfalse);
   CHECKa(var, bddfalse);
   
   if (var < 2)  /* Empty set */
      return r;

 again:
   if (setjmp(bddexception) == 0)
   {
      if (varset2vartable(var) < 0)
	 return bddfalse;

      INITREF;
      quantid = (var << 3) | CACHEID_FORALL;
      applyop = bddop_and;
      
      if (!firstReorder)
	 bdd_disable_reorder();
      res = quant_rec(r);
      if (!firstReorder)
	 bdd_enable_reorder();
   }
   else
   {
      bdd_checkreorder();

      if (firstReorder-- == 1)
	 goto again;
      res = BDDZERO;  /* avoid warning about res being uninitialized */
   }

   checkresize();
   return res;
}


BDD bdd_unique(BDD r, BDD var)
{
   BDD res;
   firstReorder = 1;
   
   CHECKa(r, bddfalse);
   CHECKa(var, bddfalse);
   
   if (var < 2)  /* Empty set */
      return r;

 again:
   if (setjmp(bddexception) == 0)
   {
      if (varset2vartable(var) < 0)
	 return bddfalse;

      INITREF;
      quantid = (var << 3) | CACHEID_UNIQUE;
      applyop = bddop_xor;
      
      if (!firstReorder)
	 bdd_disable_reorder();
      res = quant_rec(r);
      if (!firstReorder)
	 bdd_enable_reorder();
   }
   else
   {
      bdd_checkreorder();

      if (firstReorder-- == 1)
	 goto again;
      res = BDDZERO;  /* avoid warning about res being uninitialized */
   }

   checkresize();
   return res;
}


static int quant_rec(int r)
{
   BddCacheData *entry;
   int res;
   
   if (r < 2  ||  LEVEL(r) > quantlast)
      return r;

   entry = BddCache_lookup(&quantcache, QUANTHASH(r));
   if (entry->a == r  &&  entry->c == quantid)
   {
#ifdef CACHESTATS
      bddcachestats.opHit++;
#endif
      return entry->r.res;
   }
#ifdef CACHESTATS
   bddcachestats.opMiss++;
#endif

   PUSHREF( quant_rec(LOW(r)) );
   PUSHREF( quant_rec(HIGH(r)) );
   
   if (INVARSET(LEVEL(r)))
      res = apply_rec(READREF(2), READREF(1));
   else
      res = bdd_makenode(LEVEL(r), READREF(2), READREF(1));

   POPREF(2);
   
   entry->a = r;
   entry->c = quantid;
   entry->r.res = res;

   return res;
}


/*=== APPLY & QUANTIFY =================================================*/

BDD bdd_appex(BDD l, BDD r, int opr, BDD var)
{
   BDD res;
   firstReorder = 1;
   
   CHECKa(l, bddfalse);
   CHECKa(r, bddfalse);
   CHECKa(var, bddfalse);
   
   if (opr<0 || opr>bddop_invimp)
   {
      bdd_error(BDD_OP);
      return bddfalse;
   }
   
   if (var < 2)  /* Empty set */
      return bdd_apply(l,r,opr);

 again:
   if (setjmp(bddexception) == 0)
   {
      if (varset2vartable(var) < 0)
	 return bddfalse;
   
      INITREF;
      applyop = bddop_or;
      appexop = opr;
      appexid = (var << 5) | (appexop << 1); /* FIXME: range! */
      quantid = (appexid << 3) | CACHEID_APPEX;
      
      if (!firstReorder)
	 bdd_disable_reorder();
      res = appquant_rec(l, r);
      if (!firstReorder)
	 bdd_enable_reorder();
   }
   else
   {
      bdd_checkreorder();

      if (firstReorder-- == 1)
	 goto again;
      res = BDDZERO;  /* avoid warning about res being uninitialized */
   }
   
   checkresize();
   return res;
}


BDD bdd_appall(BDD l, BDD r, int opr, BDD var)
{
   BDD res;
   firstReorder = 1;
   
   CHECKa(l, bddfalse);
   CHECKa(r, bddfalse);
   CHECKa(var, bddfalse);
   
   if (opr<0 || opr>bddop_invimp)
   {
      bdd_error(BDD_OP);
      return bddfalse;
   }
   
   if (var < 2)  /* Empty set */
      return bdd_apply(l,r,opr);

 again:
   if (setjmp(bddexception) == 0)
   {
      if (varset2vartable(var) < 0)
	 return bddfalse;

      INITREF;
      applyop = bddop_and;
      appexop = opr;
      appexid = (var << 5) | (appexop << 1) | 1; /* FIXME: range! */
      quantid = (appexid << 3) | CACHEID_APPAL;
      
      if (!firstReorder)
	 bdd_disable_reorder();
      res = appquant_rec(l, r);
      if (!firstReorder)
	 bdd_enable_reorder();
   }
   else
   {
      bdd_checkreorder();

      if (firstReorder-- == 1)
	 goto again;
      res = BDDZERO;  /* avoid warning about res being uninitialized */
   }

   checkresize();
   return res;
}


BDD bdd_appuni(BDD l, BDD r, int opr, BDD var)
{
   BDD res;
   firstReorder = 1;
   
   CHECKa(l, bddfalse);
   CHECKa(r, bddfalse);
   CHECKa(var, bddfalse);
   
   if (opr<0 || opr>bddop_invimp)
   {
      bdd_error(BDD_OP);
      return bddfalse;
   }
   
   if (var < 2)  /* Empty set */
      return bdd_apply(l,r,opr);

 again:
   if (setjmp(bddexception) == 0)
   {
      if (varset2vartable(var) < 0)
	 return bddfalse;

      INITREF;
      applyop = bddop_xor;
      appexop = opr;
      appexid = (var << 5) | (appexop << 1) | 1; /* FIXME: range! */
      quantid = (appexid << 3) | CACHEID_APPUN;
      
      if (!firstReorder)
	 bdd_disable_reorder();
      res = appquant_rec(l, r);
      if (!firstReorder)
	 bdd_enable_reorder();
   }
   else
   {
      bdd_checkreorder();

      if (firstReorder-- == 1)
	 goto again;
      res = BDDZERO;  /* avoid warning about res being uninitialized */
   }

   checkresize();
   return res;
}


static int appquant_rec(int l, int r)
{
   BddCacheData *entry;
   int res;

   switch (appexop)
   {
    case bddop_and:
       if (l == 0  ||  r == 0)
	  return 0;
       if (l == r)
	  return quant_rec(l);
       if (l == 1)
	  return quant_rec(r);
       if (r == 1)
	  return quant_rec(l);
       break;
    case bddop_or:
       if (l == 1  ||  r == 1)
	  return 1;
       if (l == r)
	  return quant_rec(l);
       if (l == 0)
	  return quant_rec(r);
       if (r == 0)
	  return quant_rec(l);
       break;
    case bddop_xor:
       if (l == r)
	  return 0;
       if (l == 0)
	  return quant_rec(r);
       if (r == 0)
	  return quant_rec(l);
       break;
    case bddop_nand:
       if (l == 0  ||  r == 0)
	  return 1;
       break;
    case bddop_nor:
       if (l == 1  ||  r == 1)
	  return 0;
       break;
   }
   
   if (ISCONST(l)  &&  ISCONST(r))
      res = oprres[appexop][(l<<1) | r];
   else
   if (LEVEL(l) > quantlast  &&  LEVEL(r) > quantlast)
   {
      int oldop = applyop;
      applyop = appexop;
      res = apply_rec(l,r);
      applyop = oldop;
   }
   else
   {
      entry = BddCache_lookup(&appexcache, APPEXHASH(l,r,appexop));
      if (entry->a == l  &&  entry->b == r  &&  entry->c == appexid)
      {
#ifdef CACHESTATS
	 bddcachestats.opHit++;
#endif
	 return entry->r.res;
      }
#ifdef CACHESTATS
      bddcachestats.opMiss++;
#endif

      if (LEVEL(l) == LEVEL(r))
      {
	 PUSHREF( appquant_rec(LOW(l), LOW(r)) );
	 PUSHREF( appquant_rec(HIGH(l), HIGH(r)) );
	 if (INVARSET(LEVEL(l)))
	    res = apply_rec(READREF(2), READREF(1));
	 else
	    res = bdd_makenode(LEVEL(l), READREF(2), READREF(1));
      }
      else
      if (LEVEL(l) < LEVEL(r))
      {
	 PUSHREF( appquant_rec(LOW(l), r) );
	 PUSHREF( appquant_rec(HIGH(l), r) );
	 if (INVARSET(LEVEL(l)))
	    res = apply_rec(READREF(2), READREF(1));
	 else
	    res = bdd_makenode(LEVEL(l), READREF(2), READREF(1));
      }
      else
      {
	 PUSHREF( appquant_rec(l, LOW(r)) );
	 PUSHREF( appquant_rec(l, HIGH(r)) );
	 if (INVARSET(LEVEL(r)))
	    res = apply_rec(READREF(2), READREF(1));
	 else
	    res = bdd_makenode(LEVEL(r), READREF(2), READREF(1));
      }

      POPREF(2);
      
      entry->a = l;
      entry->b = r;
      entry->c = appexid;
      entry->r.res = res;
   }

   return res;
}


/*************************************************************************
  Informational functions
*************************************************************************/

/*=== SUPPORT ==========================================================*/

BDD bdd_support(BDD r)
{
   static int  supportSize = 0;
   int n;
   int res=1;

   CHECKa(r, bddfalse);

   /* Variable sets are conjunctions, so the empty support is bddtrue.  */
   if (r < 2)
      return bddtrue;

      /* On-demand allocation of support set */
   if (supportSize < bddvarnum)
   {
     if (supportSet)
       free(supportSet);
     if ((supportSet=(int*)malloc(bddvarnum*sizeof(int))) == NULL)
     {
       bdd_error(BDD_MEMORY);
       return bddfalse;
     }
     memset(supportSet, 0, bddvarnum*sizeof(int));
     supportSize = bddvarnum;
     supportID = 0;
   }

      /* Update global variables used to speed up bdd_support()
       * - instead of always memsetting support to zero, we use
       *   a change counter.
       * - and instead of reading the whole array afterwards, we just
       *   look from 'min' to 'max' used BDD variables.
       */
   if (supportID == 0x0FFFFFFF)
   {
        /* We probably don't get here -- but let's just be sure */
     memset(supportSet, 0, bddvarnum*sizeof(int));
     supportID = 0;
   }
   ++supportID;
   supportMin = LEVEL(r);
   supportMax = supportMin;

   support_rec(r, supportSet);
   bdd_unmark(r);

   bdd_disable_reorder();

   for (n=supportMax ; n>=supportMin ; --n)
      if (supportSet[n] == supportID)
      {
	 register BDD tmp;
	 bdd_addref(res);
	 tmp = bdd_makenode(n, 0, res);
	 bdd_delref(res);
	 res = tmp;
      }
   
   bdd_enable_reorder();

   return res;
}


static void support_rec(int r, int* support)
{
   BddNode *node;
   
   if (r < 2)
      return;

   node = &bddnodes[r];
   if (LEVELp(node) & MARKON  ||  LOWp(node) == -1)
      return;

   support[LEVELp(node)] = supportID;
   
   if (LEVELp(node) > supportMax)
     supportMax = LEVELp(node);
   
   LEVELp(node) |= MARKON;
   
   support_rec(LOWp(node), support);
   support_rec(HIGHp(node), support);
}


/*=== ONE SATISFYING VARIABLE ASSIGNMENT ===============================*/

BDD bdd_satone(BDD r)
{
   BDD res;

   CHECKa(r, bddfalse);
   if (r < 2)
      return r;

   bdd_disable_reorder();
   
   INITREF;
   res = satone_rec(r);

   bdd_enable_reorder();

   checkresize();
   return res;
}


static BDD satone_rec(BDD r)
{
   if (ISCONST(r))
      return r;

   if (ISZERO(LOW(r)))
   {
      BDD res = satone_rec(HIGH(r));
      return PUSHREF( bdd_makenode(LEVEL(r), BDDZERO, res) );
   }
   else
   {
      BDD res = satone_rec(LOW(r));
      return PUSHREF( bdd_makenode(LEVEL(r), res, BDDZERO) );
   }
}


BDD bdd_satoneset(BDD r, BDD var, BDD pol)
{
   BDD res;

   CHECKa(r, bddfalse);
   if (ISZERO(r))
      return r;
   if (!ISCONST(pol))
   {
      bdd_error(BDD_ILLBDD);
      return bddfalse;
   }

   bdd_disable_reorder();
   
   INITREF;
   satPolarity = pol;
   res = satoneset_rec(r, var);

   bdd_enable_reorder();

   checkresize();
   return res;
}


static BDD satoneset_rec(BDD r, BDD var)
{
   if (ISCONST(r)  &&  ISCONST(var))
      return r;

   if (LEVEL(r) < LEVEL(var))
   {
      if (ISZERO(LOW(r)))
      {
	 BDD res = satoneset_rec(HIGH(r), var);
	 return PUSHREF( bdd_makenode(LEVEL(r), BDDZERO, res) );
      }
      else
      {
	 BDD res = satoneset_rec(LOW(r), var);
	 return PUSHREF( bdd_makenode(LEVEL(r), res, BDDZERO) );
      }
   }
   else if (LEVEL(var) < LEVEL(r))
   {
      BDD res = satoneset_rec(r, HIGH(var));
      if (satPolarity == BDDONE)
	 return PUSHREF( bdd_makenode(LEVEL(var), BDDZERO, res) );
      else
	 return PUSHREF( bdd_makenode(LEVEL(var), res, BDDZERO) );
   }
   else /* LEVEL(r) == LEVEL(var) */
   {
      if (ISZERO(LOW(r)))
      {
	 BDD res = satoneset_rec(HIGH(r), HIGH(var));
	 return PUSHREF( bdd_makenode(LEVEL(r), BDDZERO, res) );
      }
      else
      {
	 BDD res = satoneset_rec(LOW(r), HIGH(var));
	 return PUSHREF( bdd_makenode(LEVEL(r), res, BDDZERO) );
      }
   }
   
}


/*=== EXACTLY ONE SATISFYING VARIABLE ASSIGNMENT =======================*/

BDD bdd_fullsatone(BDD r)
{
   BDD res;
   int v;

   CHECKa(r, bddfalse);
   if (r == 0)
      return 0;

   bdd_disable_reorder();
   
   INITREF;
   res = fullsatone_rec(r);

   for (v=LEVEL(r)-1 ; v>=0 ; v--)
   {
      res = PUSHREF( bdd_makenode(v, res, 0) );
   }
   
   bdd_enable_reorder();

   checkresize();
   return res;
}


static int fullsatone_rec(int r)
{
   if (r < 2)
      return r;
   
   if (LOW(r) != 0)
   {
      int res = fullsatone_rec(LOW(r));
      int v;
      
      for (v=LEVEL(LOW(r))-1 ; v>LEVEL(r) ; v--)
      {
	 res = PUSHREF( bdd_makenode(v, res, 0) );
      }

      return PUSHREF( bdd_makenode(LEVEL(r), res, 0) );
   }
   else
   {
      int res = fullsatone_rec(HIGH(r));
      int v;
      
      for (v=LEVEL(HIGH(r))-1 ; v>LEVEL(r) ; v--)
      {
	 res = PUSHREF( bdd_makenode(v, res, 0) );
      }

      return PUSHREF( bdd_makenode(LEVEL(r), 0, res) );
   }
}


/*=== ALL SATISFYING VARIABLE ASSIGNMENTS ==============================*/

void bdd_allsat(BDD r, bddallsathandler handler)
{
   int v;
  
   CHECKn(r);

   if ((allsatProfile=(char*)malloc(bddvarnum)) == NULL)
   {
      bdd_error(BDD_MEMORY);
      return;
   }

   for (v=LEVEL(r)-1 ; v>=0 ; --v)
     allsatProfile[bddlevel2var[v]] = -1;
   
   allsatHandler = handler;
   INITREF;
   
   allsat_rec(r);

   free(allsatProfile);
}


static void allsat_rec(BDD r)
{
   if (ISONE(r))
   {
      allsatHandler(allsatProfile, bddvarnum);
      return;
   }
  
   if (ISZERO(r))
      return;
   
   if (!ISZERO(LOW(r)))
   {
      int v;

      allsatProfile[bddlevel2var[LEVEL(r)]] = 0;
	 
      for (v=LEVEL(LOW(r))-1 ; v>LEVEL(r) ; --v)
      {
	 allsatProfile[bddlevel2var[v]] = -1;
      }
      
      allsat_rec(LOW(r));
   }
   
   if (!ISZERO(HIGH(r)))
   {
      int v;

      allsatProfile[bddlevel2var[LEVEL(r)]] = 1;
	 
      for (v=LEVEL(HIGH(r))-1 ; v>LEVEL(r) ; --v)
      {
	 allsatProfile[bddlevel2var[v]] = -1;
      }
      
      allsat_rec(HIGH(r));
   }
}


/*=== COUNT NUMBER OF SATISFYING ASSIGNMENT ============================*/

double bdd_satcount(BDD r)
{
   double size=1;

   CHECKa(r, 0.0);

   miscid = CACHEID_SATCOU;
   size = pow(2.0, (double)LEVEL(r));
   
   return size * satcount_rec(r);
}


double bdd_satcountset(BDD r, BDD varset)
{
   double unused = bddvarnum;
   BDD n;

   if (ISCONST(varset)  ||  ISZERO(r)) /* empty set */
      return 0.0;

   for (n=varset ; !ISCONST(n) ; n=HIGH(n))
      unused--;

   unused = bdd_satcount(r) / pow(2.0,unused);

   return unused >= 1.0 ? unused : 1.0;
}


static double satcount_rec(int root)
{
   BddCacheData *entry;
   BddNode *node;
   double size, s;
   
   if (root < 2)
      return root;

   entry = BddCache_lookup(&misccache, SATCOUHASH(root));
   if (entry->a == root  &&  entry->c == miscid)
      return entry->r.dres;

   node = &bddnodes[root];
   size = 0;
   s = 1;

   s *= pow(2.0, (float)(LEVEL(LOWp(node)) - LEVELp(node) - 1));
   size += s * satcount_rec(LOWp(node));

   s = 1;
   s *= pow(2.0, (float)(LEVEL(HIGHp(node)) - LEVELp(node) - 1));
   size += s * satcount_rec(HIGHp(node));

   entry->a = root;
   entry->c = miscid;
   entry->r.dres = size;
   
   return size;
}


double bdd_satcountln(BDD r)
{
   double size;

   CHECKa(r, 0.0);

   miscid = CACHEID_SATCOULN;
   size = satcountln_rec(r);

   if (size >= 0.0)
      size += LEVEL(r);

   return size;
}


 double bdd_satcountlnset(BDD r, BDD varset)
{
   double unused = bddvarnum;
   BDD n;

   if (ISCONST(varset)) /* empty set */
      return 0.0;

   for (n=varset ; !ISCONST(n) ; n=HIGH(n))
      unused--;

   unused = bdd_satcountln(r) - unused;

   return unused >= 0.0 ? unused : 0.0;
}


static double satcountln_rec(int root)
{
   BddCacheData *entry;
   BddNode *node;
   double size, s1,s2;
   
   if (root == 0)
      return -1.0;
   if (root == 1)
      return 0.0;

   entry = BddCache_lookup(&misccache, SATCOUHASH(root));
   if (entry->a == root  &&  entry->c == miscid)
      return entry->r.dres;

   node = &bddnodes[root];

   s1 = satcountln_rec(LOWp(node));
   if (s1 >= 0.0)
      s1 += LEVEL(LOWp(node)) - LEVELp(node) - 1;
   
   s2 = satcountln_rec(HIGHp(node));
   if (s2 >= 0.0)
      s2 += LEVEL(HIGHp(node)) - LEVELp(node) - 1;
   
   if (s1 < 0.0)
      size = s2;
   else if (s2 < 0.0)
      size = s1;
   else if (s1 < s2)
      size = s2 + log1p(pow(2.0,s1-s2)) / M_LN2;
   else
      size = s1 + log1p(pow(2.0,s2-s1)) / M_LN2;
   
   entry->a = root;
   entry->c = miscid;
   entry->r.dres = size;
   
   return size;
}


/*=== COUNT NUMBER OF ALLOCATED NODES ==================================*/

int bdd_nodecount(BDD r)
{
   int num=0;

   CHECK(r);
   
   bdd_markcount(r, &num);
   bdd_unmark(r);

   return num;
}


int bdd_anodecount(BDD *r, int num)
{
   int n;
   int cou=0;

   for (n=0 ; n<num ; n++)
      bdd_markcount(r[n], &cou);
   
   for (n=0 ; n<num ; n++)
      bdd_unmark(r[n]);

   return cou;
}


/*=== NODE PROFILE =====================================================*/

int *bdd_varprofile(BDD r)
{
   CHECKa(r, NULL);
   
   if ((varprofile=(int*)malloc(sizeof(int)*bddvarnum)) == NULL)
   {
      bdd_error(BDD_MEMORY);
      return NULL;
   }

   memset(varprofile, 0, sizeof(int)*bddvarnum);
   varprofile_rec(r);
   bdd_unmark(r);
   return varprofile;
}


static void varprofile_rec(int r)
{
   BddNode *node;
   
   if (r < 2)
      return;

   node = &bddnodes[r];
   if (LEVELp(node) & MARKON)
      return;

   varprofile[bddlevel2var[LEVELp(node)]]++;
   LEVELp(node) |= MARKON;
   
   varprofile_rec(LOWp(node));
   varprofile_rec(HIGHp(node));
}


/*=== COUNT NUMBER OF PATHS ============================================*/

double bdd_pathcount(BDD r)
{
   CHECKa(r, 0.0);

   miscid = CACHEID_PATHCOU;

   return bdd_pathcount_rec(r);
}


static double bdd_pathcount_rec(BDD r)
{
   BddCacheData *entry;
   double size;

   if (ISZERO(r))
      return 0.0;
   if (ISONE(r))
      return 1.0;

   entry = BddCache_lookup(&misccache, PATHCOUHASH(r));
   if (entry->a == r  &&  entry->c == miscid)
      return entry->r.dres;

   size = bdd_pathcount_rec(LOW(r)) + bdd_pathcount_rec(HIGH(r));

   entry->a = r;
   entry->c = miscid;
   entry->r.dres = size;
   
   return size;
}


/*************************************************************************
  Other internal functions
*************************************************************************/

static int varset2vartable(BDD r)
{
   BDD n;
   
   if (r < 2)
      return bdd_error(BDD_VARSET);
   
   quantvarsetID++;
   
   if (quantvarsetID == INT_MAX)
   {
      memset(quantvarset, 0, sizeof(int)*bddvarnum);
      quantvarsetID = 1;
   }

   for (n=r ; n > 1 ; n=HIGH(n))
   {
      quantvarset[LEVEL(n)] = quantvarsetID;
      quantlast = LEVEL(n);
   }
   
   return 0;
}


static int varset2svartable(BDD r)
{
   BDD n;
   
   if (r < 2)
      return bdd_error(BDD_VARSET);
   
   quantvarsetID++;
   
   if (quantvarsetID == INT_MAX/2)
   {
      memset(quantvarset, 0, sizeof(int)*bddvarnum);
      quantvarsetID = 1;
   }

   for (n=r ; !ISCONST(n) ; )
   {
      if (ISZERO(LOW(n)))
      {
	 quantvarset[LEVEL(n)] = quantvarsetID;
	 n = HIGH(n);
      }
      else
      {
	 quantvarset[LEVEL(n)] = -quantvarsetID;
	 n = LOW(n);
      }
      quantlast = LEVEL(n);
   }
   
   return 0;
}


/* EOF */
