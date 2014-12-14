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
  FILE:  fdd.c
  DESCR: Finite domain extensions to BDD package
  AUTH:  Jorn Lind
  DATE:  (C) june 1997

  NOTE: If V1,...,Vn is BDD vars for a FDD, then Vn is the Least Sign. Bit
*************************************************************************/

#include <stdlib.h>
#include <string.h>
#include "kernel.h"
#include "fdd.h"


static void fdd_printset_rec(FILE *, int, int *);

/*======================================================================*/
/* NOTE: ALL FDD operations works with LSB in top of the variable order */
/*       and in index zero of the domain tables                         */
/*======================================================================*/

typedef struct s_Domain
{
   int realsize;   /* The specified domain (0...N-1) */
   int binsize;    /* The number of BDD variables representing the domain */
   int *ivar;      /* Variable indeces for the variable set */
   BDD var;        /* The BDD variable set */
} Domain;


static void Domain_allocate(Domain*, int);
static void Domain_done(Domain*);

static int    firstbddvar;
static int    fdvaralloc;         /* Number of allocated domains */
static int    fdvarnum;           /* Number of defined domains */
static Domain *domain;            /* Table of domain sizes */

static bddfilehandler filehandler;

/*************************************************************************
  Domain definition
*************************************************************************/

void bdd_fdd_init(void)
{
   domain = NULL;
   fdvarnum = fdvaralloc = 0;
   firstbddvar = 0;
}


void bdd_fdd_done(void)
{
   int n;
   
   if (domain != NULL)
   {
      for (n=0 ; n<fdvarnum ; n++)
	 Domain_done(&domain[n]);
      free(domain);
   }

   domain = NULL;
}


int fdd_extdomain(int *dom, int num)
{
   int offset = fdvarnum;
   int binoffset;
   int extravars = 0;
   int n, bn, more;

   if (!bddrunning)
      return bdd_error(BDD_RUNNING);
   
      /* Build domain table */
   if (domain == NULL)  /* First time */
   {
      fdvaralloc = num;
      if ((domain=(Domain*)malloc(sizeof(Domain)*num)) == NULL)
	 return bdd_error(BDD_MEMORY);
   }
   else  /* Allocated before */
   {
      if (fdvarnum + num > fdvaralloc)
      {
         fdvaralloc += (num > fdvaralloc) ? num : fdvaralloc;
	 
	 Domain *tmp_ptr = (Domain*)realloc(domain, sizeof(Domain)*fdvaralloc);
	 if (tmp_ptr == NULL)
	    return bdd_error(BDD_MEMORY);
	 domain = tmp_ptr;
      }
   }

      /* Create bdd variable tables */
   for (n=0 ; n<num ; n++)
   {
      Domain_allocate(&domain[n+fdvarnum], dom[n]);
      extravars += domain[n+fdvarnum].binsize;
   }

   binoffset = firstbddvar;
   if (firstbddvar + extravars > bddvarnum)
      bdd_setvarnum(firstbddvar + extravars);

      /* Set correct variable sequence (interleaved) */
   for (bn=0,more=1 ; more ; bn++)
   {
      more = 0;

      for (n=0 ; n<num ; n++)
	 if (bn < domain[n+fdvarnum].binsize)
	 {
	    more = 1;
	    domain[n+fdvarnum].ivar[bn] = binoffset++;
	 }
   }

   for (n=0 ; n<num ; n++)
   {
      domain[n+fdvarnum].var = bdd_makeset(domain[n+fdvarnum].ivar,
					   domain[n+fdvarnum].binsize);
      bdd_addref(domain[n+fdvarnum].var);
   }

   fdvarnum += num;
   firstbddvar += extravars;
   
   return offset;
}


int fdd_overlapdomain(int v1, int v2)
{
   Domain *d;
   int n;
   
   if (!bddrunning)
      return bdd_error(BDD_RUNNING);
   
   if (v1 < 0  ||  v1 >= fdvarnum  ||  v2 < 0  ||  v2 >= fdvarnum)
      return bdd_error(BDD_VAR);

   if (fdvarnum + 1 > fdvaralloc)
   {
      fdvaralloc += fdvaralloc;
      
      Domain *tmp_ptr = (Domain*)realloc(domain, sizeof(Domain)*fdvaralloc);
      if (tmp_ptr == NULL)
	 return bdd_error(BDD_MEMORY);
      domain = tmp_ptr;
   }

   d = &domain[fdvarnum];
   d->realsize = domain[v1].realsize * domain[v2].realsize;
   d->binsize = domain[v1].binsize + domain[v2].binsize;
   d->ivar = (int *)malloc(sizeof(int)*d->binsize);

   for (n=0 ; n<domain[v1].binsize ; n++)
      d->ivar[n] = domain[v1].ivar[n];
   for (n=0 ; n<domain[v2].binsize ; n++)
      d->ivar[domain[v1].binsize+n] = domain[v2].ivar[n];
	 
   d->var = bdd_makeset(d->ivar, d->binsize);
   bdd_addref(d->var);
   
   return fdvarnum++;
}


void fdd_clearall(void)
{
   bdd_fdd_done();
   bdd_fdd_init();
}


/*************************************************************************
  FDD helpers
*************************************************************************/

int fdd_domainnum(void)
{
   if (!bddrunning)
      return bdd_error(BDD_RUNNING);
   
   return fdvarnum;
}


int fdd_domainsize(int v)
{
   if (!bddrunning)
      return bdd_error(BDD_RUNNING);
   
   if (v < 0  ||  v >= fdvarnum)
      return bdd_error(BDD_VAR);
   return domain[v].realsize;
}


int fdd_varnum(int v)
{
   if (!bddrunning)
      return bdd_error(BDD_RUNNING);
   
   if (v >= fdvarnum  ||  v < 0)
      return bdd_error(BDD_VAR);
   return domain[v].binsize;
}


int *fdd_vars(int v)
{
   if (!bddrunning)
   {
      bdd_error(BDD_RUNNING);
      return NULL;
   }
   
   if (v >= fdvarnum  ||  v < 0)
   {
      bdd_error(BDD_VAR);
      return NULL;
   }

   return domain[v].ivar;
}



/*************************************************************************
  FDD primitives
*************************************************************************/

BDD fdd_ithvar(int var, int val)
{
   int n;
   int v=1, tmp;
   
   if (!bddrunning)
   {
      bdd_error(BDD_RUNNING);
      return bddfalse;
   }
   
   if (var < 0  ||  var >= fdvarnum)
   {
      bdd_error(BDD_VAR);
      return bddfalse;
   }

   if (val < 0  ||  val >= domain[var].realsize)
   {
      bdd_error(BDD_RANGE);
      return bddfalse;
   }

   for (n=0 ; n<domain[var].binsize ; n++)
   {
      bdd_addref(v);
      
      if (val & 0x1)
	 tmp = bdd_apply(bdd_ithvar(domain[var].ivar[n]), v, bddop_and);
      else
	 tmp = bdd_apply(bdd_nithvar(domain[var].ivar[n]), v, bddop_and);

      bdd_delref(v);
      v = tmp;
      val >>= 1;
   }

   return v;
}


int fdd_scanvar(BDD r, int var)
{
   int *allvar;
   int res;

   CHECK(r);
   if (r == bddfalse)
      return -1;
   if (var < 0  ||  var >= fdvarnum)
      return bdd_error(BDD_VAR);

   allvar = fdd_scanallvar(r);
   res = allvar[var];
   free(allvar);

   return res;
}


int* fdd_scanallvar(BDD r)
{
   int n;
   char *store;
   int *res;
   BDD p = r;
   
   CHECKa(r,NULL);
   if (r == bddfalse)
      return NULL;
   
   store = NEW(char,bddvarnum);
   for (n=0 ; n<bddvarnum ; n++)
      store[n] = 0;

   while (!ISCONST(p))
   {
      if (!ISZERO(LOW(p)))
      {
	 store[bddlevel2var[LEVEL(p)]] = 0;
	 p = LOW(p);
      }
      else
      {
	 store[bddlevel2var[LEVEL(p)]] = 1;
	 p = HIGH(p);
      }
   }

   res = NEW(int, fdvarnum);

   for (n=0 ; n<fdvarnum ; n++)
   {
      int m;
      int val=0;
      
      for (m=domain[n].binsize-1 ; m>=0 ; m--)
	 if ( store[domain[n].ivar[m]] )
	    val = val*2 + 1;
         else
	    val = val*2;
      
      res[n] = val;
   }
   
   free(store);
   
   return res;
}


BDD fdd_ithset(int var)
{
   if (!bddrunning)
   {
      bdd_error(BDD_RUNNING);
      return bddfalse;
   }
   
   if (var < 0  ||  var >= fdvarnum)
   {
      bdd_error(BDD_VAR);
      return bddfalse;
   }

   return domain[var].var;
}


BDD fdd_domain(int var)
{
   int n,val;
   Domain *dom;
   BDD d;
      
   if (!bddrunning)
   {
      bdd_error(BDD_RUNNING);
      return bddfalse;
   }
   
   if (var < 0  ||  var >= fdvarnum)
   {
      bdd_error(BDD_VAR);
      return bddfalse;
   }

      /* Encode V<=X-1. V is the variables in 'var' and X is the domain size */
   
   dom = &domain[var];
   val = dom->realsize-1;
   d = bddtrue;
   
   for (n=0 ; n<dom->binsize ; n++)
   {
      BDD tmp;
      
      if (val & 0x1)
	 tmp = bdd_apply( bdd_nithvar(dom->ivar[n]), d, bddop_or );
      else
	 tmp = bdd_apply( bdd_nithvar(dom->ivar[n]), d, bddop_and );

      val >>= 1;

      bdd_addref(tmp);
      bdd_delref(d);
      d = tmp;
   }

   return d;
}


BDD fdd_equals(int left, int right)
{
   BDD e = bddtrue, tmp1, tmp2;
   int n;
   
   if (!bddrunning)
   {
      bdd_error(BDD_RUNNING);
      return bddfalse;
   }
   
   if (left < 0  ||  left >= fdvarnum  ||  right < 0  ||  right >= fdvarnum)
   {
      bdd_error(BDD_VAR);
      return bddfalse;
   }
   if (domain[left].realsize != domain[right].realsize)
   {
      bdd_error(BDD_RANGE);
      return bddfalse;
   }
   
   for (n=0 ; n<domain[left].binsize ; n++)
   {
      tmp1 = bdd_addref( bdd_apply(bdd_ithvar(domain[left].ivar[n]),
				   bdd_ithvar(domain[right].ivar[n]),
				   bddop_biimp) );
      
      tmp2 = bdd_addref( bdd_apply(e, tmp1, bddop_and) );
      bdd_delref(tmp1);
      bdd_delref(e);
      e = tmp2;
   }

   bdd_delref(e);
   return e;
}


/*************************************************************************
  File IO
*************************************************************************/

bddfilehandler fdd_file_hook(bddfilehandler h)
{
   bddfilehandler old = filehandler;
   filehandler = h;
   return old;
}


void fdd_printset(BDD r)
{
   CHECKn(r);
   fdd_fprintset(stdout, r);
}


void fdd_fprintset(FILE *ofile, BDD r)
{
   int *set;
   
   if (!bddrunning)
   {
      bdd_error(BDD_RUNNING);
      return;
   }
   
   if (r < 2)
   {
      fprintf(ofile, "%s", r == 0 ? "F" : "T");
      return;
   }

   set = (int *)malloc(sizeof(int)*bddvarnum);
   if (set == NULL)
   {
      bdd_error(BDD_MEMORY);
      return;
   }
   
   memset(set, 0, sizeof(int) * bddvarnum);
   fdd_printset_rec(ofile, r, set);
   free(set);
}


static void fdd_printset_rec(FILE *ofile, int r, int *set)
{
   int n,m,i;
   int used = 0;
   int *var;
   int *binval;
   int ok, first;
   
   if (r == 0)
      return;
   else
   if (r == 1)
   {
      fprintf(ofile, "<");
      first=1;

      for (n=0 ; n<fdvarnum ; n++)
      {
	 int firstval=1;
	 used = 0;

	 for (m=0 ; m<domain[n].binsize ; m++)
	    if (set[domain[n].ivar[m]] != 0)
	       used = 1;
	 
	 if (used)
	 {
	    if (!first)
	       fprintf(ofile, ", ");
	    first = 0;
	    if (filehandler)
	       filehandler(ofile, n);
	    else
	       fprintf(ofile, "%d", n);
	       
	    fprintf(ofile,":");

	    var = domain[n].ivar;
	    
	    for (m=0 ; m<(1<<domain[n].binsize) ; m++)
	    {
	       binval = fdddec2bin(n, m);
	       ok=1;
	       
	       for (i=0 ; i<domain[n].binsize && ok ; i++)
		  if (set[var[i]] == 1  &&  binval[i] != 0)
		     ok = 0;
		  else
		  if (set[var[i]] == 2  &&  binval[i] != 1)
		     ok = 0;

	       if (ok)
	       {
		  if (firstval)
		     fprintf(ofile, "%d", m);
		  else
		     fprintf(ofile, "/%d", m);
		  firstval = 0;
	       }

	       free(binval);
	    }
	 }
      }

      fprintf(ofile, ">");
   }
   else
   {
      set[bddlevel2var[LEVEL(r)]] = 1;
      fdd_printset_rec(ofile, LOW(r), set);
      
      set[bddlevel2var[LEVEL(r)]] = 2;
      fdd_printset_rec(ofile, HIGH(r), set);
      
      set[bddlevel2var[LEVEL(r)]] = 0;
   }
}


/*======================================================================*/

int fdd_scanset(BDD r, int **varset, int *varnum)
{
   int *fv, fn;
   int num,n,m,i;
      
   if (!bddrunning)
      return bdd_error(BDD_RUNNING);
   
   if ((n=bdd_scanset(r, &fv, &fn)) < 0)
      return n;

   for (n=0,num=0 ; n<fdvarnum ; n++)
   {
      int found=0;
      
      for (m=0 ; m<domain[n].binsize && !found ; m++)
      {
	 for (i=0 ; i<fn && !found ; i++)
	    if (domain[n].ivar[m] == fv[i])
	    {
	       num++;
	       found=1;
	    }
      }
   }

   if ((*varset=(int*)malloc(sizeof(int)*num)) == NULL)
      return bdd_error(BDD_MEMORY);

   for (n=0,num=0 ; n<fdvarnum ; n++)
   {
      int found=0;
      
      for (m=0 ; m<domain[n].binsize && !found ; m++)
      {
	 for (i=0 ; i<fn && !found ; i++)
	    if (domain[n].ivar[m] == fv[i])
	    {
	       (*varset)[num++] = n;
	       found=1;
	    }
      }
   }

   *varnum = num;

   return 0;
}


/*======================================================================*/

BDD fdd_makeset(int *varset, int varnum)
{
   BDD res=bddtrue, tmp;
   int n;

   if (!bddrunning)
   {
      bdd_error(BDD_RUNNING);
      return bddfalse;
   }
   
   for (n=0 ; n<varnum ; n++)
      if (varset[n] < 0  ||  varset[n] >= fdvarnum)
      {
	 bdd_error(BDD_VAR);
	 return bddfalse;
      }
	  
   for (n=0 ; n<varnum ; n++)
   {
      bdd_addref(res);
      tmp = bdd_apply(domain[varset[n]].var, res, bddop_and);
      bdd_delref(res);
      res = tmp;
   }

   return res;
}


int fdd_intaddvarblock(int first, int last, int fixed)
{
   bdd res = bddtrue, tmp;
   int n, err;
   
   if (!bddrunning)
      return bdd_error(BDD_RUNNING);
   
   if (first > last ||  first < 0  ||  last >= fdvarnum)
      return bdd_error(BDD_VARBLK);

   for (n=first ; n<=last ; n++)
   {
      bdd_addref(res);
      tmp = bdd_apply(domain[n].var, res, bddop_and);
      bdd_delref(res);
      res = tmp;
   }

   err = bdd_addvarblock(res, fixed);
   
   bdd_delref(res);
   return err;
}


int fdd_setpair(bddPair *pair, int p1, int p2)
{
   int n,e;

   if (!bddrunning)
      return bdd_error(BDD_RUNNING);
   
   if (p1<0 || p1>=fdvarnum || p2<0 || p2>=fdvarnum)
      return bdd_error(BDD_VAR);
   
   if (domain[p1].binsize != domain[p2].binsize)
      return bdd_error(BDD_VARNUM);

   for (n=0 ; n<domain[p1].binsize ; n++)
      if ((e=bdd_setpair(pair, domain[p1].ivar[n], domain[p2].ivar[n])) < 0)
	 return e;

   return 0;
}


int fdd_setpairs(bddPair *pair, int *p1, int *p2, int size)
{
   int n,e;

   if (!bddrunning)
      return bdd_error(BDD_RUNNING);
   
   for (n=0 ; n<size ; n++)
      if (p1[n]<0 || p1[n]>=fdvarnum || p2[n]<0 || p2[n]>=fdvarnum)
	 return bdd_error(BDD_VAR);
   
   for (n=0 ; n<size ; n++)
      if ((e=fdd_setpair(pair, p1[n], p2[n])) < 0)
	 return e;

   return 0;
}


/*************************************************************************
  Domain storage "class"
*************************************************************************/

static void Domain_done(Domain* d)
{
   free(d->ivar);
   bdd_delref(d->var);
}


static void Domain_allocate(Domain* d, int range)
{
   int calcsize = 2;
   
   if (range <= 0  || range > INT_MAX/2)
   {
      bdd_error(BDD_RANGE);
      return;
   }

   d->realsize = range;
   d->binsize = 1;

   while (calcsize < range)
   {
      d->binsize++;
      calcsize <<= 1;
   }

   d->ivar = (int *)malloc(sizeof(int)*d->binsize);
   d->var = bddtrue;
}


int *fdddec2bin(int var, int val)
{
   int *res;
   int n = 0;

   res = (int *)malloc(sizeof(int)*domain[var].binsize);
   memset(res, 0, sizeof(int)*domain[var].binsize);

   while (val > 0)
   {
      if (val & 0x1)
	 res[n] = 1;
      val >>= 1;
      n++;
   }

   return res;
}


/* EOF */
