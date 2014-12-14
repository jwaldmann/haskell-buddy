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
  FILE:  bddio.c
  DESCR: File I/O routines for BDD package
  AUTH:  Jorn Lind
  DATE:  (C) june 1997
*************************************************************************/


#include <string.h>
#include <stdlib.h>
#include <fcntl.h>
#include <assert.h>
#include <sys/stat.h>
#include "kernel.h"

static void bdd_printset_rec(FILE *, int, int *);
static void bdd_fprintdot_rec(FILE*, BDD);
static int  bdd_save_rec(FILE*, int);
static int  bdd_loaddata(FILE *);
static int  loadhash_get(int);
static void loadhash_add(int, int);

static bddfilehandler filehandler;

typedef struct s_LoadHash
{
   int key;
   int data;
   int first;
   int next;
} LoadHash;

static LoadHash *lh_table;
static int       lh_freepos;
static int       lh_nodenum;
static int      *loadvar2level;

/*=== PRINTING ========================================================*/


bddfilehandler bdd_file_hook(bddfilehandler handler)
{
   bddfilehandler old = filehandler;
   filehandler = handler;
   return old;
}


void bdd_printall(void)
{
   bdd_fprintall(stdout);
}


void bdd_fprintall(FILE *ofile)
{
   int n;
   
   for (n=0 ; n<bddnodesize ; n++)
   {
      if (LOW(n) != -1)
      {
	 fprintf(ofile, "[%5d - %2d] ", n, bddnodes[n].refcou);
	 if (filehandler)
	    filehandler(ofile, bddlevel2var[LEVEL(n)]);
	 else
	    fprintf(ofile, "%3d", bddlevel2var[LEVEL(n)]);

	 fprintf(ofile, ": %3d", LOW(n));
	 fprintf(ofile, " %3d", HIGH(n));
	 fprintf(ofile, "\n");
      }
   }
}


void bdd_printtable(BDD r)
{
   bdd_fprinttable(stdout, r);
}


void bdd_fprinttable(FILE *ofile, BDD r)
{
   BddNode *node;
   int n;
   
   fprintf(ofile, "ROOT: %d\n", r);
   if (r < 2)
      return;

   bdd_mark(r);
   
   for (n=0 ; n<bddnodesize ; n++)
   {
      if (LEVEL(n) & MARKON)
      {
	 node = &bddnodes[n];
	 
	 LEVELp(node) &= MARKOFF;

	 fprintf(ofile, "[%5d] ", n);
	 if (filehandler)
	    filehandler(ofile, bddlevel2var[LEVELp(node)]);
	 else
	    fprintf(ofile, "%3d", bddlevel2var[LEVELp(node)]);

	 fprintf(ofile, ": %3d", LOWp(node));
	 fprintf(ofile, " %3d", HIGHp(node));
	 fprintf(ofile, "\n");
      }
   }
}


void bdd_printset(BDD r)
{
   bdd_fprintset(stdout, r);
}


void bdd_fprintset(FILE *ofile, BDD r)
{
   int *set;
   
   if (r < 2)
   {
      fprintf(ofile, "%s", r == 0 ? "F" : "T");
      return;
   }

   if ((set=(int *)malloc(sizeof(int)*bddvarnum)) == NULL)
   {
      bdd_error(BDD_MEMORY);
      return;
   }
   
   memset(set, 0, sizeof(int) * bddvarnum);
   bdd_printset_rec(ofile, r, set);
   free(set);
}


static void bdd_printset_rec(FILE *ofile, int r, int *set)
{
   int n;
   int first;
   
   if (r == 0)
      return;
   else
   if (r == 1)
   {
      fprintf(ofile, "<");
      first = 1;
      
      for (n=0 ; n<bddvarnum ; n++)
      {
	 if (set[n] > 0)
	 {
	    if (!first)
	       fprintf(ofile, ", ");
	    first = 0;
	    if (filehandler)
	       filehandler(ofile, bddlevel2var[n]);
	    else
	       fprintf(ofile, "%d", bddlevel2var[n]);
	    fprintf(ofile, ":%d", (set[n]==2 ? 1 : 0));
	 }
      }

      fprintf(ofile, ">");
   }
   else
   {
      set[LEVEL(r)] = 1;
      bdd_printset_rec(ofile, LOW(r), set);
      
      set[LEVEL(r)] = 2;
      bdd_printset_rec(ofile, HIGH(r), set);
      
      set[LEVEL(r)] = 0;
   }
}


void bdd_printdot(BDD r)
{
   bdd_fprintdot(stdout, r);
}


int bdd_fnprintdot(char *fname, BDD r)
{
   FILE *ofile = fopen(fname, "w");
   if (ofile == NULL)
      return bdd_error(BDD_FILE);
   bdd_fprintdot(ofile, r);
   fclose(ofile);
   return 0;
}


void bdd_fprintdot(FILE* ofile, BDD r)
{
   fprintf(ofile, "digraph G {\n");
   fprintf(ofile, "0 [shape=box, label=\"0\", style=filled, shape=box, height=0.3, width=0.3];\n");
   fprintf(ofile, "1 [shape=box, label=\"1\", style=filled, shape=box, height=0.3, width=0.3];\n");

   bdd_fprintdot_rec(ofile, r);

   fprintf(ofile, "}\n");

   bdd_unmark(r);
}


static void bdd_fprintdot_rec(FILE* ofile, BDD r)
{
   if (ISCONST(r) || MARKED(r))
      return;

   fprintf(ofile, "%d [label=\"", r);
   if (filehandler)
      filehandler(ofile, bddlevel2var[LEVEL(r)]);
   else
      fprintf(ofile, "%d", bddlevel2var[LEVEL(r)]);
   fprintf(ofile, "\"];\n");

   fprintf(ofile, "%d -> %d [style=dotted];\n", r, LOW(r));
   fprintf(ofile, "%d -> %d [style=filled];\n", r, HIGH(r));

   SETMARK(r);
   
   bdd_fprintdot_rec(ofile, LOW(r));
   bdd_fprintdot_rec(ofile, HIGH(r));
}


/*=== SAVE =============================================================*/

int bdd_fnsave(char *fname, BDD r)
{
   FILE *ofile;
   int ok;

   if ((ofile=fopen(fname,"w")) == NULL)
      return bdd_error(BDD_FILE);

   ok = bdd_save(ofile, r);
   fclose(ofile);
   return ok;
}


int bdd_save(FILE *ofile, BDD r)
{
   int err, n=0;

   if (r < 2)
   {
      fprintf(ofile, "0 0 %d\n", r);
      return 0;
   }
   
   bdd_markcount(r, &n);
   bdd_unmark(r);
   fprintf(ofile, "%d %d\n", n, bddvarnum);

   for (n=0 ; n<bddvarnum ; n++)
      fprintf(ofile, "%d ", bddvar2level[n]);
   fprintf(ofile, "\n");
   
   err = bdd_save_rec(ofile, r);
   bdd_unmark(r);

   return err;
}


static int bdd_save_rec(FILE *ofile, int root)
{
   BddNode *node = &bddnodes[root];
   int err;
   
   if (root < 2)
      return 0;

   if (LEVELp(node) & MARKON)
      return 0;
   LEVELp(node) |= MARKON;
   
   if ((err=bdd_save_rec(ofile, LOWp(node))) < 0)
      return err;
   if ((err=bdd_save_rec(ofile, HIGHp(node))) < 0)
      return err;

   fprintf(ofile, "%d %d %d %d\n",
	   root, bddlevel2var[LEVELp(node) & MARKHIDE],
	   LOWp(node), HIGHp(node));

   return 0;
}


/*=== LOAD =============================================================*/

int bdd_fnload(char *fname, BDD *root)
{
   FILE *ifile;
   int ok;

   if ((ifile=fopen(fname,"r")) == NULL)
      return bdd_error(BDD_FILE);

   ok = bdd_load(ifile, root);
   fclose(ifile);
   return ok;
}


int bdd_load(FILE *ifile, BDD *root)
{
   int n, vnum, tmproot;

   if (fscanf(ifile, "%d %d", &lh_nodenum, &vnum) != 2)
      return bdd_error(BDD_FORMAT);

      /* Check for constant true / false */
   if (lh_nodenum==0  &&  vnum==0)
   {
      fscanf(ifile, "%d", root);
      return 0;
   }

   if ((loadvar2level=(int*)malloc(sizeof(int)*vnum)) == NULL)
      return bdd_error(BDD_MEMORY);
   for (n=0 ; n<vnum ; n++)
      fscanf(ifile, "%d", &loadvar2level[n]);
   
   if (vnum > bddvarnum)
      bdd_setvarnum(vnum);

   if ((lh_table=(LoadHash*)malloc(lh_nodenum*sizeof(LoadHash))) == NULL)
      return bdd_error(BDD_MEMORY);
   
   for (n=0 ; n<lh_nodenum ; n++)
   {
      lh_table[n].first = -1;
      lh_table[n].next = n+1;
   }
   lh_table[lh_nodenum-1].next = -1;
   lh_freepos = 0;

   tmproot = bdd_loaddata(ifile);

   for (n=0 ; n<lh_nodenum ; n++)
      bdd_delref(lh_table[n].data);
   
   free(lh_table);
   free(loadvar2level);
   
   *root = 0;
   if (tmproot < 0)
      return tmproot;
   else
      *root = tmproot;
   
   return 0;
}


static int bdd_loaddata(FILE *ifile)
{
   int key,var,low,high,root=0,n;
   
   for (n=0 ; n<lh_nodenum ; n++)
   {
      if (fscanf(ifile,"%d %d %d %d", &key, &var, &low, &high) != 4)
	 return bdd_error(BDD_FORMAT);

      if (low >= 2)
	 low = loadhash_get(low);
      if (high >= 2)
	 high = loadhash_get(high);

      if (low<0 || high<0 || var<0)
	 return bdd_error(BDD_FORMAT);

      root = bdd_addref( bdd_ite(bdd_ithvar(var), high, low) );

      loadhash_add(key, root);
   }

   return root;
}


static void loadhash_add(int key, int data)
{
   int hash = key % lh_nodenum;
   int pos = lh_freepos;

   lh_freepos = lh_table[pos].next;
   lh_table[pos].next = lh_table[hash].first;
   lh_table[hash].first = pos;

   lh_table[pos].key = key;
   lh_table[pos].data = data;
}


static int loadhash_get(int key)
{
   int hash = lh_table[key % lh_nodenum].first;

   while (hash != -1  &&  lh_table[hash].key != key)
      hash = lh_table[hash].next;

   if (hash == -1)
      return -1;
   return lh_table[hash].data;
}


/* EOF */
