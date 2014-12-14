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
  FILE:  kernel.c
  DESCR: implements the bdd kernel functions.
  AUTH:  Jorn Lind
  DATE:  (C) june 1997

  WARNING: Do not use pointers to nodes across makenode calls,
           as makenode may resize/move the nodetable.

*************************************************************************/

#include "config.h"
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include <assert.h>

#include "kernel.h"
#include "cache.h"
#include "prime.h"

/*************************************************************************
  Various definitions and global variables
*************************************************************************/

/*=== EXTERNAL VARIABLES FOR PACKAGE USERS =============================*/
const BDD bddtrue=1;                     /* The constant true bdd */
const BDD bddfalse=0;                    /* The constant false bdd */


/*=== INTERNAL DEFINITIONS =============================================*/

/* Min. number of nodes (%) that has to be left after a garbage collect
   unless a resize should be done. */
static int minfreenodes=20;


/*=== GLOBAL KERNEL VARIABLES ==========================================*/

int          bddrunning;            /* Flag - package initialized */
int          bdderrorcond;          /* Some error condition */
int          bddnodesize;           /* Number of allocated nodes */
int          bddmaxnodesize;        /* Maximum allowed number of nodes */
int          bddmaxnodeincrease;    /* Max. # of nodes used to inc. table */
BddNode*     bddnodes;          /* All of the bdd nodes */
int          bddfreepos;        /* First free node */
int          bddfreenum;        /* Number of free nodes */
long int     bddproduced;       /* Number of new nodes ever produced */
int          bddvarnum;         /* Number of defined BDD variables */
int*         bddrefstack;       /* Internal node reference stack */
int*         bddrefstacktop;    /* Internal node reference stack top */
int*         bddvar2level;      /* Variable -> level table */
int*         bddlevel2var;      /* Level -> variable table */
jmp_buf      bddexception;      /* Long-jump point for interrupting calc. */
int          bddresized;        /* Flag indicating a resize of the nodetable */

bddCacheStat bddcachestats;


/*=== PRIVATE KERNEL VARIABLES =========================================*/

static BDD*     bddvarset;             /* Set of defined BDD variables */
static int      gbcollectnum;          /* Number of garbage collections */
static int      cachesize;             /* Size of the operator caches */
static long int gbcclock;              /* Clock ticks used in GBC */
static int      usednodes_nextreorder; /* When to do reorder next time */
static bddinthandler  err_handler;     /* Error handler */
static bddgbchandler  gbc_handler;     /* Garbage collection handler */
static bdd2inthandler resize_handler;  /* Node-table-resize handler */


   /* Strings for all error mesages */
static char *errorstrings[BDD_ERRNUM] =
{ "Out of memory", "Unknown variable", "Value out of range",
  "Unknown BDD root dereferenced", "bdd_init() called twice",
  "File operation failed", "Incorrect file format",
  "Variables not in ascending order", "User called break",
  "Mismatch in size of variable sets",
  "Cannot allocate fewer nodes than already in use",
  "Unknown operator", "Illegal variable set",
  "Bad variable block operation",
  "Trying to decrease the number of variables",
  "Trying to replace with variables already in the bdd",
  "Number of nodes reached user defined maximum",
  "Unknown BDD - was not in node table",
  "Bad size argument",
  "Mismatch in bitvector size",
  "Illegal shift-left/right parameter",
  "Division by zero" };


/*=== OTHER INTERNAL DEFINITIONS =======================================*/

#define NODEHASH(lvl,l,h) (TRIPLE(lvl,l,h) % bddnodesize)


/*************************************************************************
  BDD misc. user operations
*************************************************************************/


int bdd_init(int initnodesize, int cs)
{
   int n, err;
   
   srand48( SRAND48SEED ) ;

   if (bddrunning)
      return bdd_error(BDD_RUNNING);
   
   bddnodesize = bdd_prime_gte(initnodesize);
   
   if ((bddnodes=(BddNode*)malloc(sizeof(BddNode)*bddnodesize)) == NULL)
      return bdd_error(BDD_MEMORY);

   bddresized = 0;
   
   for (n=0 ; n<bddnodesize ; n++)
   {
      bddnodes[n].refcou = 0;
      LOW(n) = -1;
      bddnodes[n].hash = 0;
      LEVEL(n) = 0;
      bddnodes[n].next = n+1;
   }
   bddnodes[bddnodesize-1].next = 0;

   bddnodes[0].refcou = bddnodes[1].refcou = MAXREF;
   LOW(0) = HIGH(0) = 0;
   LOW(1) = HIGH(1) = 1;
   
   if ((err=bdd_operator_init(cs)) < 0)
   {
      bdd_done();
      return err;
   }

   bddfreepos = 2;
   bddfreenum = bddnodesize-2;
   bddrunning = 1;
   bddvarnum = 0;
   gbcollectnum = 0;
   gbcclock = 0;
   cachesize = cs;
   usednodes_nextreorder = bddnodesize;
   bddmaxnodeincrease = DEFAULTMAXNODEINC;

   bdderrorcond = 0;
   
   bddcachestats.uniqueAccess = 0;
   bddcachestats.uniqueChain = 0;
   bddcachestats.uniqueHit = 0;
   bddcachestats.uniqueMiss = 0;
   bddcachestats.opHit = 0;
   bddcachestats.opMiss = 0;
   bddcachestats.swapCount = 0;
 
   bdd_gbc_hook(bdd_default_gbchandler);
   bdd_error_hook(bdd_default_errhandler);
   bdd_resize_hook(NULL);
   bdd_pairs_init();
   bdd_reorder_init();
   bdd_fdd_init();
   
   if (setjmp(bddexception) != 0)
      assert(0);

   return 0;
}


void bdd_done(void)
{
   /*sanitycheck(); FIXME */
   bdd_fdd_done();
   bdd_reorder_done();
   bdd_pairs_done();
   
   free(bddnodes);
   free(bddrefstack);
   free(bddvarset);
   free(bddvar2level);
   free(bddlevel2var);
   
   bddnodes = NULL;
   bddrefstack = NULL;
   bddvarset = NULL;
   bddvar2level = NULL;
   bddlevel2var = NULL;

   bdd_operator_done();

   bddrunning = 0;
   bddnodesize = 0;
   bddmaxnodesize = 0;
   bddvarnum = 0;
   bddproduced = 0;
   
   err_handler = NULL;
   gbc_handler = NULL;
   resize_handler = NULL;
}


int bdd_setvarnum(int num)
{
   int bdv;
   int oldbddvarnum = bddvarnum;

   bdd_disable_reorder();
      
   if (num < 1  ||  num > MAXVAR)
   {
      bdd_error(BDD_RANGE);
      return bddfalse;
   }

   if (num < bddvarnum)
      return bdd_error(BDD_DECVNUM);
   if (num == bddvarnum)
      return 0;

   if (bddvarset == NULL)
   {
      if ((bddvarset=(BDD*)malloc(sizeof(BDD)*num*2)) == NULL)
	 return bdd_error(BDD_MEMORY);
      if ((bddlevel2var=(int*)malloc(sizeof(int)*(num+1))) == NULL)
      {
	 free(bddvarset);
	 return bdd_error(BDD_MEMORY);
      }
      if ((bddvar2level=(int*)malloc(sizeof(int)*(num+1))) == NULL)
      {
	 free(bddvarset);
	 free(bddlevel2var);
	 return bdd_error(BDD_MEMORY);
      }
   }
   else
   {
      BDD *tmp_ptr = (BDD*)realloc(bddvarset,sizeof(BDD)*num*2);
      if (tmp_ptr == NULL)
	 return bdd_error(BDD_MEMORY);
      bddvarset = tmp_ptr;

      int *tmp_ptr2 = (int*)realloc(bddlevel2var,sizeof(int)*(num+1));
      if (tmp_ptr2 == NULL)
	 return bdd_error(BDD_MEMORY);
      bddlevel2var = tmp_ptr2;

      tmp_ptr2 = (int*)realloc(bddvar2level,sizeof(int)*(num+1));
      if (tmp_ptr2 == NULL)
	 return bdd_error(BDD_MEMORY);
      bddvar2level = tmp_ptr2;
   }

   if (bddrefstack != NULL)
      free(bddrefstack);
   bddrefstack = bddrefstacktop = (int*)malloc(sizeof(int)*(num*2+4));

   for(bdv=bddvarnum ; bddvarnum < num; bddvarnum++)
   {
      bddvarset[bddvarnum*2] = PUSHREF( bdd_makenode(bddvarnum, 0, 1) );
      bddvarset[bddvarnum*2+1] = bdd_makenode(bddvarnum, 1, 0);
      POPREF(1);
      
      if (bdderrorcond)
      {
	 bddvarnum = bdv;
	 return -bdderrorcond;
      }
      
      bddnodes[bddvarset[bddvarnum*2]].refcou = MAXREF;
      bddnodes[bddvarset[bddvarnum*2+1]].refcou = MAXREF;
      bddlevel2var[bddvarnum] = bddvarnum;
      bddvar2level[bddvarnum] = bddvarnum;
   }

   LEVEL(0) = num;
   LEVEL(1) = num;
   bddvar2level[num] = num;
   bddlevel2var[num] = num;
   
   bdd_pairs_resize(oldbddvarnum, bddvarnum);
   bdd_operator_varresize();
   
   bdd_enable_reorder();
   
   return 0;
}


int bdd_extvarnum(int num)
{
   int start = bddvarnum;
   
   if (num < 0  ||  num > 0x3FFFFFFF)
      return bdd_error(BDD_RANGE);

   bdd_setvarnum(bddvarnum+num);
   return start;
}


bddinthandler bdd_error_hook(bddinthandler handler)
{
   bddinthandler tmp = err_handler;
   err_handler = handler;
   return tmp;
}


void bdd_clear_error(void)
{
   bdderrorcond = 0;
   bdd_operator_reset();
}


bddgbchandler bdd_gbc_hook(bddgbchandler handler)
{
   bddgbchandler tmp = gbc_handler;
   gbc_handler = handler;
   return tmp;
}


bdd2inthandler bdd_resize_hook(bdd2inthandler handler)
{
   bdd2inthandler tmp = handler;
   resize_handler = handler;
   return tmp;
}


int bdd_setmaxincrease(int size)
{
   int old = bddmaxnodeincrease;
   
   if (size < 0)
      return bdd_error(BDD_SIZE);

   bddmaxnodeincrease = size;
   return old;
}


int bdd_setmaxnodenum(int size)
{
   if (size > bddnodesize  ||  size == 0)
   {
      int old = bddmaxnodesize;
      bddmaxnodesize = size;
      return old;
   }

   return bdd_error(BDD_NODES);
}


int bdd_setminfreenodes(int mf)
{
   int old = minfreenodes;
   
   if (mf<0 || mf>100)
      return bdd_error(BDD_RANGE);

   minfreenodes = mf;
   return old;
}


int bdd_getnodenum(void)
{
   return bddnodesize - bddfreenum;
}


int bdd_getallocnum(void)
{
   return bddnodesize;
}


int bdd_isrunning(void)
{
   return bddrunning;
}


char *bdd_versionstr(void)
{
   static char str[] = "BuDDy -  release " PACKAGE_VERSION;
   return str;
}


int bdd_versionnum(void)
{
   return MAJOR_VERSION * 10 + MINOR_VERSION;
}


void bdd_stats(bddStat *s)
{
   s->produced = bddproduced;
   s->nodenum = bddnodesize;
   s->maxnodenum = bddmaxnodesize;
   s->freenodes = bddfreenum;
   s->minfreenodes = minfreenodes;
   s->varnum = bddvarnum;
   s->cachesize = cachesize;
   s->gbcnum = gbcollectnum;
}



void bdd_cachestats(bddCacheStat *s)
{
   *s = bddcachestats;
}


void bdd_fprintstat(FILE *ofile)
{
   bddCacheStat s;
   bdd_cachestats(&s);
   
   fprintf(ofile, "\nCache statistics\n");
   fprintf(ofile, "----------------\n");
   
   fprintf(ofile, "Unique Access:  %ld\n", s.uniqueAccess);
   fprintf(ofile, "Unique Chain:   %ld\n", s.uniqueChain);
   fprintf(ofile, "Unique Hit:     %ld\n", s.uniqueHit);
   fprintf(ofile, "Unique Miss:    %ld\n", s.uniqueMiss);
   fprintf(ofile, "=> Hit rate =   %.2f\n",
	   (s.uniqueHit+s.uniqueMiss > 0) ? 
	   ((float)s.uniqueHit)/((float)s.uniqueHit+s.uniqueMiss) : 0);
   fprintf(ofile, "Operator Hits:  %ld\n", s.opHit);
   fprintf(ofile, "Operator Miss:  %ld\n", s.opMiss);
   fprintf(ofile, "=> Hit rate =   %.2f\n",
	   (s.opHit+s.opMiss > 0) ? 
	   ((float)s.opHit)/((float)s.opHit+s.opMiss) : 0);
   fprintf(ofile, "Swap count =    %ld\n", s.swapCount);
}


void bdd_printstat(void)
{
   bdd_fprintstat(stdout);
}


/*************************************************************************
  Error handler
*************************************************************************/

const char *bdd_errstring(int e)
{
   e = abs(e);
   if (e<1 || e>BDD_ERRNUM)
      return NULL;
   return errorstrings[e-1];
}


void bdd_default_errhandler(int e)
{
   fprintf(stderr, "BDD error: %s\n", bdd_errstring(e));
   abort();
}


int bdd_error(int e)
{
   if (err_handler != NULL)
      err_handler(e);
   
   return e;
}


/*************************************************************************
  BDD primitives
*************************************************************************/

BDD bdd_true(void)
{
   return 1;
}


BDD bdd_false(void)
{
   return 0;
}


BDD bdd_ithvar(int var)
{
   if (var < 0  ||  var >= bddvarnum)
   {
      bdd_error(BDD_VAR);
      return bddfalse;
   }

   return bddvarset[var*2];
}


BDD bdd_nithvar(int var)
{
   if (var < 0  ||  var >= bddvarnum)
   {
      bdd_error(BDD_VAR);
      return bddfalse;
   }
   
   return bddvarset[var*2+1];
}


int bdd_varnum(void)
{
   return bddvarnum;
}


int bdd_var(BDD root)
{
   CHECK(root);
   if (root < 2)
      return bdd_error(BDD_ILLBDD);

   return (bddlevel2var[LEVEL(root)]);
}


BDD bdd_low(BDD root)
{
   CHECK(root);
   if (root < 2)
      return bdd_error(BDD_ILLBDD);

   return (LOW(root));
}


BDD bdd_high(BDD root)
{
   CHECK(root);
   if (root < 2)
      return bdd_error(BDD_ILLBDD);

   return (HIGH(root));
}



/*************************************************************************
  Garbage collection and node referencing
*************************************************************************/

void bdd_default_gbchandler(int pre, bddGbcStat *s)
{
   if (!pre)
   {
      printf("Garbage collection #%d: %d nodes / %d free",
	     s->num, s->nodes, s->freenodes);
      printf(" / %.1fs / %.1fs total\n",
	     (float)s->time/(float)(CLOCKS_PER_SEC),
	     (float)s->sumtime/(float)CLOCKS_PER_SEC);
   }
}


static void bdd_gbc_rehash(void)
{
   int n;

   bddfreepos = 0;
   bddfreenum = 0;

   for (n=bddnodesize-1 ; n>=2 ; n--)
   {
      register BddNode *node = &bddnodes[n];

      if (LOWp(node) != -1)
      {
	 register unsigned int hash;

	 hash = NODEHASH(LEVELp(node), LOWp(node), HIGHp(node));
	 node->next = bddnodes[hash].hash;
	 bddnodes[hash].hash = n;
      }
      else
      {
	 node->next = bddfreepos;
	 bddfreepos = n;
	 bddfreenum++;
      }
   }
}


void bdd_gbc(void)
{
   int *r;
   int n;
   long int c2, c1 = clock();

   if (gbc_handler != NULL)
   {
      bddGbcStat s;
      s.nodes = bddnodesize;
      s.freenodes = bddfreenum;
      s.time = 0;
      s.sumtime = gbcclock;
      s.num = gbcollectnum;
      gbc_handler(1, &s);
   }
   
   for (r=bddrefstack ; r<bddrefstacktop ; r++)
      bdd_mark(*r);

   for (n=0 ; n<bddnodesize ; n++)
   {
      if (bddnodes[n].refcou > 0)
	 bdd_mark(n);
      bddnodes[n].hash = 0;
   }
   
   bddfreepos = 0;
   bddfreenum = 0;

   for (n=bddnodesize-1 ; n>=2 ; n--)
   {
      register BddNode *node = &bddnodes[n];

      if ((LEVELp(node) & MARKON)  &&  LOWp(node) != -1)
      {
	 register unsigned int hash;

	 LEVELp(node) &= MARKOFF;
	 hash = NODEHASH(LEVELp(node), LOWp(node), HIGHp(node));
	 node->next = bddnodes[hash].hash;
	 bddnodes[hash].hash = n;
      }
      else
      {
	 LOWp(node) = -1;
	 node->next = bddfreepos;
	 bddfreepos = n;
	 bddfreenum++;
      }
   }

   bdd_operator_reset();

   c2 = clock();
   gbcclock += c2-c1;
   gbcollectnum++;

   if (gbc_handler != NULL)
   {
      bddGbcStat s;
      s.nodes = bddnodesize;
      s.freenodes = bddfreenum;
      s.time = c2-c1;
      s.sumtime = gbcclock;
      s.num = gbcollectnum;
      gbc_handler(0, &s);
   }
}


BDD bdd_addref(BDD root)
{
   if (root < 2  ||  !bddrunning)
      return root;
   if (root >= bddnodesize)
      return bdd_error(BDD_ILLBDD);
   if (LOW(root) == -1)
      return bdd_error(BDD_ILLBDD);

   INCREF(root);
   return root;
}


BDD bdd_delref(BDD root)
{
   if (root < 2  ||  !bddrunning)
      return root;
   if (root >= bddnodesize)
      return bdd_error(BDD_ILLBDD);
   if (LOW(root) == -1)
      return bdd_error(BDD_ILLBDD);

   /* if the following line is present, fails there much earlier */ 
   if (!HASREF(root)) bdd_error(BDD_BREAK); /* distinctive */
   
   DECREF(root);
   return root;
}


/*=== RECURSIVE MARK / UNMARK ==========================================*/

void bdd_mark(int i)
{
   BddNode *node;
   
   if (i < 2)
      return;

   node = &bddnodes[i];
   if (LEVELp(node) & MARKON  ||  LOWp(node) == -1)
      return;
   
   LEVELp(node) |= MARKON;
   
   bdd_mark(LOWp(node));
   bdd_mark(HIGHp(node));
}


void bdd_mark_upto(int i, int level)
{
   BddNode *node = &bddnodes[i];
   
   if (i < 2)
      return;
   
   if (LEVELp(node) & MARKON  ||  LOWp(node) == -1)
      return;
   
   if (LEVELp(node) > level)
      return;

   LEVELp(node) |= MARKON;

   bdd_mark_upto(LOWp(node), level);
   bdd_mark_upto(HIGHp(node), level);
}


void bdd_markcount(int i, int *cou)
{
   BddNode *node;
   
   if (i < 2)
      return;

   node = &bddnodes[i];
   if (MARKEDp(node)  ||  LOWp(node) == -1)
      return;
   
   SETMARKp(node);
   *cou += 1;
   
   bdd_markcount(LOWp(node), cou);
   bdd_markcount(HIGHp(node), cou);
}


void bdd_unmark(int i)
{
   BddNode *node;
   
   if (i < 2)
      return;

   node = &bddnodes[i];

   if (!MARKEDp(node)  ||  LOWp(node) == -1)
      return;
   UNMARKp(node);
   
   bdd_unmark(LOWp(node));
   bdd_unmark(HIGHp(node));
}


void bdd_unmark_upto(int i, int level)
{
   BddNode *node = &bddnodes[i];

   if (i < 2)
      return;
   
   if (!(LEVELp(node) & MARKON))
      return;
   
   LEVELp(node) &= MARKOFF;
   
   if (LEVELp(node) > level)
      return;

   bdd_unmark_upto(LOWp(node), level);
   bdd_unmark_upto(HIGHp(node), level);
}


/*************************************************************************
  Unique node table functions
*************************************************************************/

int bdd_makenode(unsigned int level, int low, int high)
{
   register BddNode *node;
   register unsigned int hash;
   register int res;

#ifdef CACHESTATS
   bddcachestats.uniqueAccess++;
#endif
   
      /* check whether childs are equal */
   if (low == high)
      return low;

      /* Try to find an existing node of this kind */
   hash = NODEHASH(level, low, high);
   res = bddnodes[hash].hash;

   while(res != 0)
   {
      if (LEVEL(res) == level  &&  LOW(res) == low  &&  HIGH(res) == high)
      {
#ifdef CACHESTATS
	 bddcachestats.uniqueHit++;
#endif
	 return res;
      }

      res = bddnodes[res].next;
#ifdef CACHESTATS
      bddcachestats.uniqueChain++;
#endif
   }
   
      /* No existing node -> build one */
#ifdef CACHESTATS
   bddcachestats.uniqueMiss++;
#endif

      /* Any free nodes to use ? */
   if (bddfreepos == 0)
   {
      if (bdderrorcond)
	 return 0;
      
         /* Try to allocate more nodes */
      bdd_gbc();

      if ((bddnodesize-bddfreenum) >= usednodes_nextreorder  &&
	   bdd_reorder_ready())
      {
	 longjmp(bddexception,1);
      }

      if ((bddfreenum*100) / bddnodesize <= minfreenodes)
      {
	 bdd_noderesize(1);
	 hash = NODEHASH(level, low, high);
      }

         /* Panic if that is not possible */
      if (bddfreepos == 0)
      {
	 bdd_error(BDD_NODENUM);
	 bdderrorcond = abs(BDD_NODENUM);
	 return 0;
      }
   }

      /* Build new node */
   res = bddfreepos;
   bddfreepos = bddnodes[bddfreepos].next;
   bddfreenum--;
   bddproduced++;
   
   node = &bddnodes[res];
   LEVELp(node) = level;
   LOWp(node) = low;
   HIGHp(node) = high;
   
      /* Insert node */
   node->next = bddnodes[hash].hash;
   bddnodes[hash].hash = res;

   return res;
}


int bdd_noderesize(int doRehash)
{
   BddNode *newnodes;
   int oldsize = bddnodesize;
   int n;

   if (bddnodesize >= bddmaxnodesize  &&  bddmaxnodesize > 0)
      return -1;
   
   bddnodesize = bddnodesize << 1;

   if (bddnodesize > oldsize + bddmaxnodeincrease)
      bddnodesize = oldsize + bddmaxnodeincrease;

   if (bddnodesize > bddmaxnodesize  &&  bddmaxnodesize > 0)
      bddnodesize = bddmaxnodesize;

   bddnodesize = bdd_prime_lte(bddnodesize);
   
   if (resize_handler != NULL)
      resize_handler(oldsize, bddnodesize);

   newnodes = (BddNode*)realloc(bddnodes, sizeof(BddNode)*bddnodesize);
   if (newnodes == NULL)
      return bdd_error(BDD_MEMORY);
   bddnodes = newnodes;

   if (doRehash)
      for (n=0 ; n<oldsize ; n++)
	 bddnodes[n].hash = 0;
   
   for (n=oldsize ; n<bddnodesize ; n++)
   {
      bddnodes[n].refcou = 0;
      bddnodes[n].hash = 0;
      LEVEL(n) = 0;
      LOW(n) = -1;
      bddnodes[n].next = n+1;
   }
   bddnodes[bddnodesize-1].next = bddfreepos;
   bddfreepos = oldsize;
   bddfreenum += bddnodesize - oldsize;

   if (doRehash)
      bdd_gbc_rehash();

   bddresized = 1;
   
   return 0;
}


void bdd_checkreorder(void)
{
   bdd_reorder_auto();

      /* Do not reorder before twice as many nodes have been used */
   usednodes_nextreorder = 2 * (bddnodesize - bddfreenum);
   
      /* And if very little was gained this time (< 20%) then wait until
       * even more nodes (upto twice as many again) have been used */
   if (bdd_reorder_gain() < 20)
      usednodes_nextreorder +=
	 (usednodes_nextreorder * (20-bdd_reorder_gain())) / 20;
}


/*************************************************************************
  Variable sets
*************************************************************************/

int bdd_scanset(BDD r, int **varset, int *varnum)
{
   int n, num;

   CHECK(r);
   if (r < 2)
   {
      *varnum = 0;
      *varset = NULL;
      return 0;
   }
   
   for (n=r, num=0 ; n > 1 ; n=HIGH(n))
      num++;

   if (((*varset) = (int *)malloc(sizeof(int)*num)) == NULL)
      return bdd_error(BDD_MEMORY);
   
   for (n=r, num=0 ; n > 1 ; n=HIGH(n))
      (*varset)[num++] = bddlevel2var[LEVEL(n)];

   *varnum = num;

   return 0;
}


BDD bdd_makeset(int *varset, int varnum)
{
   int v, res=1;
   
   for (v=varnum-1 ; v>=0 ; v--)
   {
      BDD tmp;
      bdd_addref(res);
      tmp = bdd_apply(res, bdd_ithvar(varset[v]), bddop_and);
      bdd_delref(res);
      res = tmp;
   }

   return res;
}


/* EOF */
