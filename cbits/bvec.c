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
  FILE:  bvec.c
  DESCR: Boolean vector arithmetics using BDDs
  AUTH:  Jorn Lind
  DATE:  (C) may 1999
*************************************************************************/

#include <stdlib.h>
#include "kernel.h"
#include "bvec.h"

#define DEFAULT(v) { v.bitnum=0; v.bitvec=NULL; }

/*************************************************************************
*************************************************************************/

static bvec bvec_build(int bitnum, int isTrue)
{
   bvec vec;
   int n;
   
   vec.bitvec = NEW(BDD,bitnum);
   vec.bitnum = bitnum;
   if (!vec.bitvec)
   {
      bdd_error(BDD_MEMORY);
      vec.bitnum = 0;
      return vec;
   }

   for (n=0 ; n<bitnum ; n++)
      if (isTrue)
	 vec.bitvec[n] = BDDONE;
      else
	 vec.bitvec[n] = BDDZERO;

   return vec;
}


#if 0
int bvec_val2bitnum(int val)
{
   int bitnum=0;

   while (val > 0)
   {
      val >>= 1;
      bitnum++;
   }

   return bitnum;
}
#endif


bvec bvec_copy(bvec src)
{
   bvec dst;
   int n;
   
   if (src.bitnum == 0)
   {
      DEFAULT(dst);
      return dst;
   }

   dst = bvec_build(src.bitnum,0);
   
   for (n=0 ; n<src.bitnum ; n++)
      dst.bitvec[n] = bdd_addref( src.bitvec[n] );
   dst.bitnum = src.bitnum;

   return dst;
}


bvec bvec_true(int bitnum)
{
   return bvec_build(bitnum, 1);
}


bvec bvec_false(int bitnum)
{
   return bvec_build(bitnum, 0);
}


bvec bvec_con(int bitnum, int val)
{
   bvec v = bvec_build(bitnum,0);
   int n;

   for (n=0 ; n<v.bitnum ; n++)
   {
      if (val & 0x1)
	 v.bitvec[n] = bddtrue;
      else
	 v.bitvec[n] = bddfalse;

      val = val >> 1;
   }

   return v;
}


bvec bvec_var(int bitnum, int offset, int step)
{
   bvec v;
   int n;

   v = bvec_build(bitnum,0);
   
   for (n=0 ; n<bitnum ; n++)
      v.bitvec[n] = bdd_ithvar(offset+n*step);

   return v;
}


bvec bvec_varfdd(int var)
{
   bvec v;
   int *bddvar = fdd_vars(var);
   int varbitnum = fdd_varnum(var);
   int n;

   if (bddvar == NULL)
   {
      DEFAULT(v);
      return v;
   }
   
   v = bvec_build(varbitnum,0);
   
   for (n=0 ; n<v.bitnum ; n++)
      v.bitvec[n] = bdd_ithvar(bddvar[n]);

   return v;
}


bvec bvec_varvec(int bitnum, int *var)
{
   bvec v;
   int n;

   v = bvec_build(bitnum,0);
   
   for (n=0 ; n<bitnum ; n++)
      v.bitvec[n] = bdd_ithvar(var[n]);

   return v;
}


bvec bvec_coerce(int bitnum, bvec v)
{
   bvec res = bvec_build(bitnum,0);
   int minnum = MIN(bitnum, v.bitnum);
   int n;

   for (n=0 ; n<minnum ; n++)
      res.bitvec[n] = bdd_addref( v.bitvec[n] );

   return res;
}


int bvec_isconst(bvec e)
{
   int n;

   for (n=0 ; n<e.bitnum ; n++)
      if (!ISCONST(e.bitvec[n]))
	 return 0;

   return 1;
}


int bvec_val(bvec e)
{
   int n, val=0;

   for (n=e.bitnum-1 ; n>=0 ; n--)
      if (ISONE(e.bitvec[n]))
	 val = (val << 1) | 1;
      else if (ISZERO(e.bitvec[n]))
	 val = val << 1;
      else
	 return 0;

   return val;
}


/*======================================================================*/

void bvec_free(bvec v)
{
   bvec_delref(v);
   free(v.bitvec);
}


bvec bvec_addref(bvec v)
{
   int n;

   for (n=0 ; n<v.bitnum ; n++)
      bdd_addref(v.bitvec[n]);

   return v;
}


bvec bvec_delref(bvec v)
{
   int n;

   for (n=0 ; n<v.bitnum ; n++)
      bdd_delref(v.bitvec[n]);

   return v;
}


/*======================================================================*/

bvec bvec_map1(bvec a, BDD (*fun)(BDD))
{
   bvec res;
   int n;
   
   res = bvec_build(a.bitnum,0);
   for (n=0 ; n < a.bitnum ; n++)
      res.bitvec[n] = bdd_addref( fun(a.bitvec[n]) );

   return res;
}


bvec bvec_map2(bvec a, bvec b, BDD (*fun)(BDD,BDD))
{
   bvec res;
   int n;

   DEFAULT(res);
   if (a.bitnum != b.bitnum)
   {
      bdd_error(BVEC_SIZE);
      return res;
   }
   
   res = bvec_build(a.bitnum,0);
   for (n=0 ; n < a.bitnum ; n++)
      res.bitvec[n] = bdd_addref( fun(a.bitvec[n], b.bitvec[n]) );

   return res;
}


bvec bvec_map3(bvec a, bvec b, bvec c, BDD (*fun)(BDD,BDD,BDD))
{
   bvec res;
   int n;

   DEFAULT(res);
   if (a.bitnum != b.bitnum  ||  b.bitnum != c.bitnum)
   {
      bdd_error(BVEC_SIZE);
      return res;
   }
   
   res = bvec_build(a.bitnum,0);
   for (n=0 ; n < a.bitnum ; n++)
      res.bitvec[n] = bdd_addref( fun(a.bitvec[n], b.bitvec[n], c.bitvec[n]) );

   return res;
}


/*======================================================================*/

bvec bvec_add(bvec l, bvec r)
{
   bvec res;
   BDD c = bddfalse;
   int n;


   if (l.bitnum == 0  ||  r.bitnum == 0)
   {
      DEFAULT(res);
      return res;
   }
   
   if (l.bitnum != r.bitnum)
   {
      bdd_error(BVEC_SIZE);
      DEFAULT(res);
      return res;
   }
   
   res = bvec_build(l.bitnum,0);
   
   for (n=0 ; n<res.bitnum ; n++)
   {
      BDD tmp1, tmp2, tmp3;

         /* bitvec[n] = l[n] ^ r[n] ^ c; */
      tmp1 = bdd_addref( bdd_apply(l.bitvec[n], r.bitvec[n], bddop_xor) );
      tmp2 = bdd_addref( bdd_apply(tmp1, c, bddop_xor) );
      bdd_delref(tmp1);
      res.bitvec[n] = tmp2;

         /* c = (l[n] & r[n]) | (c & (l[n] | r[n])); */
      tmp1 = bdd_addref( bdd_apply(l.bitvec[n], r.bitvec[n], bddop_or) );
      tmp2 = bdd_addref( bdd_apply(c, tmp1, bddop_and) );
      bdd_delref(tmp1);
      
      tmp1 = bdd_addref( bdd_apply(l.bitvec[n], r.bitvec[n], bddop_and) );
      tmp3 = bdd_addref( bdd_apply(tmp1, tmp2, bddop_or) );
      bdd_delref(tmp1);
      bdd_delref(tmp2);
      
      bdd_delref(c);
      c = tmp3;
   }

   bdd_delref(c);
   
   return res;
}


bvec bvec_sub(bvec l, bvec r)
{
   bvec res;
   BDD c = bddfalse;
   int n;

   if (l.bitnum == 0  ||  r.bitnum == 0)
   {
      DEFAULT(res);
      return res;
   }
   
   if (l.bitnum != r.bitnum)
   {
      bdd_error(BVEC_SIZE);
      DEFAULT(res);
      return res;
   }
   
   res = bvec_build(l.bitnum,0);
   
   for (n=0 ; n<res.bitnum ; n++)
   {
      BDD tmp1, tmp2, tmp3;

         /* bitvec[n] = l[n] ^ r[n] ^ c; */
      tmp1 = bdd_addref( bdd_apply(l.bitvec[n], r.bitvec[n], bddop_xor) );
      tmp2 = bdd_addref( bdd_apply(tmp1, c, bddop_xor) );
      bdd_delref(tmp1);
      res.bitvec[n] = tmp2;

         /* c = (l[n] & r[n] & c) | (!l[n] & (r[n] | c)); */
      tmp1 = bdd_addref( bdd_apply(r.bitvec[n], c, bddop_or) );
      tmp2 = bdd_addref( bdd_apply(l.bitvec[n], tmp1, bddop_less) );
      bdd_delref(tmp1);
      
      tmp1 = bdd_addref( bdd_apply(l.bitvec[n], r.bitvec[n], bddop_and) );
      tmp3 = bdd_addref( bdd_apply(tmp1, c, bddop_and) );
      bdd_delref(tmp1);

      tmp1 = bdd_addref( bdd_apply(tmp3, tmp2, bddop_or) );
      bdd_delref(tmp2);
      bdd_delref(tmp3);
      
      bdd_delref(c);
      c = tmp1;
   }

   bdd_delref(c);
   
   return res;
}


bvec bvec_mulfixed(bvec e, int c)
{
   bvec res, next, rest;
   int n;

   if (e.bitnum == 0)
   {
      DEFAULT(res);
      return res;
   }
   
   if (c == 0)
      return bvec_build(e.bitnum,0);  /* return false array (base case) */

   next = bvec_build(e.bitnum,0);
   for (n=1 ; n<e.bitnum ; n++)
         /* e[] is never deleted, so no ref.cou. */
      next.bitvec[n] = e.bitvec[n-1];    
      
   rest = bvec_mulfixed(next, c>>1);
   
   if (c & 0x1)
   {
      res = bvec_add(e, rest);
      bvec_free(rest);
   }
   else
      res = rest;

   bvec_free(next);

   return res;
}


bvec bvec_mul(bvec left, bvec right)
{
   int n;
   int bitnum = left.bitnum + right.bitnum;
   bvec res;
   bvec leftshifttmp;
   bvec leftshift;

   if (left.bitnum == 0  ||  right.bitnum == 0)
   {
      DEFAULT(res);
      return res;
   }

   res = bvec_false(bitnum);
   leftshifttmp = bvec_copy(left);
   leftshift = bvec_coerce(bitnum, leftshifttmp);
   
   /*bvec_delref(leftshifttmp);*/
   bvec_free(leftshifttmp);
   
   for (n=0 ; n<right.bitnum ; n++)
   {
      bvec added = bvec_add(res, leftshift);
      int m;

      for (m=0 ; m<bitnum ; m++)
      {
	 bdd tmpres = bdd_addref( bdd_ite(right.bitvec[n],
					  added.bitvec[m], res.bitvec[m]) );
	 bdd_delref(res.bitvec[m]);
	 res.bitvec[m] = tmpres;
      }

         /* Shift 'leftshift' one bit left */
      bdd_delref(leftshift.bitvec[leftshift.bitnum-1]);
      for (m=bitnum-1 ; m>=1 ; m--)
	 leftshift.bitvec[m] = leftshift.bitvec[m-1];
      leftshift.bitvec[0] = bddfalse;
      
      /*bvec_delref(added);*/
      bvec_free(added);
   }

   /*bvec_delref(leftshift);*/
   bvec_free(leftshift);
   
   return res;
}

static void bvec_div_rec(bvec divisor, bvec *remainder, bvec *result, int step)
{
   int n;
   BDD isSmaller = bdd_addref( bvec_lte(divisor, *remainder) );
   bvec newResult = bvec_shlfixed( *result, 1, isSmaller );
   bvec zero = bvec_build(divisor.bitnum, bddfalse);
   bvec newRemainder, tmp, sub = bvec_build(divisor.bitnum, bddfalse);

   for (n=0 ; n<divisor.bitnum ; n++)
      sub.bitvec[n] = bdd_ite(isSmaller, divisor.bitvec[n], zero.bitvec[n]);

   tmp = bvec_sub( *remainder, sub );
   newRemainder = bvec_shlfixed(tmp, 1, result->bitvec[divisor.bitnum-1]);

   if (step > 1)
      bvec_div_rec( divisor, &newRemainder, &newResult, step-1 );
   
   bvec_free(tmp);
   bvec_free(sub);
   bvec_free(zero);
   bdd_delref(isSmaller);
   
   bvec_free(*remainder);
   bvec_free(*result);
   *result = newResult;
   *remainder = newRemainder;
}


int bvec_divfixed(bvec e, int c, bvec *res, bvec *rem)
{
   if (c > 0)
   {
      bvec divisor = bvec_con(e.bitnum, c);
      bvec tmp = bvec_build(e.bitnum, 0);
      bvec tmpremainder = bvec_shlfixed(tmp, 1, e.bitvec[e.bitnum-1]);
      bvec result = bvec_shlfixed(e, 1, bddfalse);
      bvec remainder;
      
      bvec_div_rec(divisor, &tmpremainder, &result, divisor.bitnum);
      remainder = bvec_shrfixed(tmpremainder, 1, bddfalse);

      bvec_free(tmp);
      bvec_free(tmpremainder);
      bvec_free(divisor);
      
      *res = result;
      *rem = remainder;
      
      return 0;
   }

   return bdd_error(BVEC_DIVZERO);
}

#if 0
void p(bvec x)
{
   int n;
   for (n=0 ; n<x.bitnum ; n++)
   {
      printf("  %d: ", n);
      fdd_printset(x.bitvec[n]);
      printf("\n");
   }
}
#endif

int bvec_div(bvec left, bvec right, bvec *result, bvec *remainder)
{
   int n, m;
   int bitnum = left.bitnum + right.bitnum;
   bvec res;
   bvec rem;
   bvec div, divtmp;

   if (left.bitnum == 0  ||  right.bitnum == 0  ||
       left.bitnum != right.bitnum)
   {
      return bdd_error(BVEC_SIZE);
   }

   rem = bvec_coerce(bitnum, left);
   divtmp = bvec_coerce(bitnum, right);
   div = bvec_shlfixed(divtmp, left.bitnum, bddfalse);

   /*bvec_delref(divtmp);*/
   bvec_free(divtmp);

   res = bvec_false(right.bitnum);

   for (n=0 ; n<right.bitnum+1 ; n++)
   {
      bdd divLteRem = bdd_addref( bvec_lte(div, rem) );
      bvec remSubDiv = bvec_sub(rem, div);

      for (m=0 ; m<bitnum ; m++)
      {
	 bdd remtmp = bdd_addref( bdd_ite(divLteRem,
					  remSubDiv.bitvec[m],rem.bitvec[m]) );
	 bdd_delref( rem.bitvec[m] );
	 rem.bitvec[m] = remtmp;
      }

      if (n > 0)
	 res.bitvec[right.bitnum-n] = divLteRem;

         /* Shift 'div' one bit right */
      bdd_delref(div.bitvec[0]);
      for (m=0 ; m<bitnum-1 ; m++)
	 div.bitvec[m] = div.bitvec[m+1];
      div.bitvec[bitnum-1] = bddfalse;

      /*bvec_delref(remSubDiv);*/
      bvec_free(remSubDiv);
   }

   /*bvec_delref(*result);*/
   bvec_free(*result);
   /*bvec_delref(*remainder);*/
   bvec_free(*remainder);

   *result = res;
   *remainder = bvec_coerce(right.bitnum, rem);

   /*bvec_delref(rem);*/
   bvec_free(rem);
   
   return 0;
}


bvec bvec_ite(bdd a, bvec b, bvec c)
{
  bvec res;
  int n;

  DEFAULT(res);
  if (b.bitnum != c.bitnum)
  {
    bdd_error(BVEC_SIZE);
    return res;
  }

  res = bvec_build(b.bitnum, 0);
  
  for (n=0 ; n<b.bitnum ; ++n)
  {
    res.bitvec[n] = bdd_addref( bdd_ite(a, b.bitvec[n], c.bitvec[n]) );
  }

  return res;
}


bvec bvec_shlfixed(bvec e, int pos, BDD c)
{
   bvec res;
   int n, minnum = MIN(e.bitnum,pos);

   if (pos < 0)
   {
      bdd_error(BVEC_SHIFT);
      DEFAULT(res);
      return res;
   }
   
   if (e.bitnum == 0)
   {
      DEFAULT(res);
      return res;
   }
   
   res = bvec_build(e.bitnum,0);

   for (n=0 ; n<minnum ; n++)
      res.bitvec[n] = bdd_addref(c);
   
   for (n=minnum ; n<e.bitnum ; n++)
      res.bitvec[n] = bdd_addref(e.bitvec[n-pos]);
   
   return res;
}


BVEC bvec_shl(BVEC l, BVEC r, BDD c)
{
   BVEC res, val;
   bdd tmp1, tmp2, rEquN;
   int n, m;

   if (l.bitnum == 0  ||  r.bitnum == 0)
   {
      DEFAULT(res);
      return res;
   }
   
   res = bvec_build(l.bitnum, 0);

   for (n=0 ; n<=l.bitnum ; n++)
   {
      val = bvec_con(r.bitnum, n);
      rEquN = bdd_addref( bvec_equ(r, val) );
      
      for (m=0 ; m<l.bitnum ; m++)
      {
   	    /* Set the m'th new location to be the (m+n)'th old location */
	 if (m-n >= 0)
	    tmp1 = bdd_addref( bdd_and(rEquN, l.bitvec[m-n]) );
	 else
	    tmp1 = bdd_addref( bdd_and(rEquN, c) );
	 tmp2 = bdd_addref( bdd_or(res.bitvec[m], tmp1) );
	 bdd_delref(tmp1);

	 bdd_delref(res.bitvec[m]);
	 res.bitvec[m] = tmp2;
      }

      bdd_delref(rEquN);
      /*bvec_delref(val);*/
      bvec_free(val);
   }

      /* At last make sure 'c' is shiftet in for r-values > l-bitnum */
   val = bvec_con(r.bitnum, l.bitnum);
   rEquN = bvec_gth(r, val);
   tmp1 = bdd_addref( bdd_and(rEquN, c) );

   for (m=0 ; m<l.bitnum ; m++)
   {
      tmp2 = bdd_addref( bdd_or(res.bitvec[m], tmp1) );
      
      bdd_delref(res.bitvec[m]);
      res.bitvec[m] = tmp2;
   }

   bdd_delref(tmp1);
   bdd_delref(rEquN);
   /*bvec_delref(val);*/
   bvec_free(val);
   
   return res;
}


bvec bvec_shrfixed(bvec e, int pos, BDD c)
{
   bvec res;
   int n, maxnum = MAX(0,e.bitnum-pos);
   
   if (pos < 0)
   {
      bdd_error(BVEC_SHIFT);
      DEFAULT(res);
      return res;
   }
   
   if (e.bitnum == 0)
   {
      DEFAULT(res);
      return res;
   }
   
   res = bvec_build(e.bitnum,0);

   for (n=maxnum ; n<e.bitnum ; n++)
      res.bitvec[n] = bdd_addref(c);
   
   for (n=0 ; n<maxnum ; n++)
      res.bitvec[n] = bdd_addref(e.bitvec[n+pos]);

   return res;
}


BVEC bvec_shr(BVEC l, BVEC r, BDD c)
{
   BVEC res, val;
   bdd tmp1, tmp2, rEquN;
   int n, m;

   if (l.bitnum == 0  ||  r.bitnum == 0)
   {
      DEFAULT(res);
      return res;
   }
   
   res = bvec_build(l.bitnum, 0);

   for (n=0 ; n<=l.bitnum ; n++)
   {
      val = bvec_con(r.bitnum, n);
      rEquN = bdd_addref( bvec_equ(r, val) );
      
      for (m=0 ; m<l.bitnum ; m++)
      {
   	    /* Set the m'th new location to be the (m+n)'th old location */
	 if (m+n <= 2)
	    tmp1 = bdd_addref( bdd_and(rEquN, l.bitvec[m+n]) );
	 else
	    tmp1 = bdd_addref( bdd_and(rEquN, c) );
	 tmp2 = bdd_addref( bdd_or(res.bitvec[m], tmp1) );
	 bdd_delref(tmp1);

	 bdd_delref(res.bitvec[m]);
	 res.bitvec[m] = tmp2;
      }

      bdd_delref(rEquN);
      /*bvec_delref(val);*/
      bvec_free(val);
   }

      /* At last make sure 'c' is shiftet in for r-values > l-bitnum */
   val = bvec_con(r.bitnum, l.bitnum);
   rEquN = bvec_gth(r, val);
   tmp1 = bdd_addref( bdd_and(rEquN, c) );

   for (m=0 ; m<l.bitnum ; m++)
   {
      tmp2 = bdd_addref( bdd_or(res.bitvec[m], tmp1) );
      
      bdd_delref(res.bitvec[m]);
      res.bitvec[m] = tmp2;
   }

   bdd_delref(tmp1);
   bdd_delref(rEquN);
   /*bvec_delref(val);*/
   bvec_free(val);
   
   return res;
}


bdd bvec_lth(bvec l, bvec r)
{
   BDD p = bddfalse;
   int n;
   
   if (l.bitnum == 0  ||  r.bitnum == 0)
      return bddfalse;
   
   if (l.bitnum != r.bitnum)
   {
      bdd_error(BVEC_SIZE);
      return p;
   }
   
   for (n=0 ; n<l.bitnum ; n++)
   {
      /* p = (!l[n] & r[n]) |
       *     bdd_apply(l[n], r[n], bddop_biimp) & p; */
      
      BDD tmp1 = bdd_addref(bdd_apply(l.bitvec[n],r.bitvec[n],bddop_less));
      BDD tmp2 = bdd_addref(bdd_apply(l.bitvec[n],r.bitvec[n],bddop_biimp));
      BDD tmp3 = bdd_addref( bdd_apply(tmp2, p, bddop_and) );
      BDD tmp4 = bdd_addref( bdd_apply(tmp1, tmp3, bddop_or) );
      bdd_delref(tmp1);
      bdd_delref(tmp2);
      bdd_delref(tmp3);
      bdd_delref(p);
      p = tmp4;
   }

   return bdd_delref(p);
}


bdd bvec_lte(bvec l, bvec r)
{
   BDD p = bddtrue;
   int n;
   
   if (l.bitnum == 0  ||  r.bitnum == 0)
      return bddfalse;
   
   if (l.bitnum != r.bitnum)
   {
      bdd_error(BVEC_SIZE);
      return p;
   }
   
   for (n=0 ; n<l.bitnum ; n++)
   {
      /* p = (!l[n] & r[n]) |
       *     bdd_apply(l[n], r[n], bddop_biimp) & p; */
      
      BDD tmp1 = bdd_addref( bdd_apply(l.bitvec[n], r.bitvec[n], bddop_less) );
      BDD tmp2 = bdd_addref( bdd_apply(l.bitvec[n], r.bitvec[n], bddop_biimp) );
      BDD tmp3 = bdd_addref( bdd_apply(tmp2, p, bddop_and) );
      BDD tmp4 = bdd_addref( bdd_apply(tmp1, tmp3, bddop_or) );
      bdd_delref(tmp1);
      bdd_delref(tmp2);
      bdd_delref(tmp3);
      bdd_delref(p);
      p = tmp4;
   }

   return bdd_delref(p);
}


bdd bvec_gth(bvec l, bvec r)
{
   BDD tmp = bdd_addref( bvec_lte(l,r) );
   BDD p = bdd_not(tmp);
   bdd_delref(tmp);
   return p;
}


bdd bvec_gte(bvec l, bvec r)
{
   BDD tmp = bdd_addref( bvec_lth(l,r) );
   BDD p = bdd_not(tmp);
   bdd_delref(tmp);
   return p;
}


bdd bvec_equ(bvec l, bvec r)
{
   BDD p = bddtrue;
   int n;
   
   if (l.bitnum == 0  ||  r.bitnum == 0)
      return bddfalse;
   
   if (l.bitnum != r.bitnum)
   {
      bdd_error(BVEC_SIZE);
      return p;
   }
   
   for (n=0 ; n<l.bitnum ; n++)
   {
      BDD tmp1, tmp2;
      tmp1 = bdd_addref( bdd_apply(l.bitvec[n], r.bitvec[n], bddop_biimp) );
      tmp2 = bdd_addref( bdd_apply(tmp1, p, bddop_and) );
      bdd_delref(tmp1);
      bdd_delref(p);
      p = tmp2;
   }

   return bdd_delref(p);
}


bdd bvec_neq(bvec l, bvec r)
{
   BDD tmp = bdd_addref( bvec_equ(l,r) );
   BDD p = bdd_not(tmp);
   bdd_delref(tmp);
   return p;
}


/* EOF */
