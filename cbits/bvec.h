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
  FILE:  bvec.h
  DESCR: Boolean (BDD) vector handling
  AUTH:  Jorn Lind
  DATE:  (C) may 1999
*************************************************************************/
/** \file bvec.h
 */

#ifndef _BVEC_H
#define _BVEC_H

#include "fdd.h"

   /* Boolean (BDD) vector */
/**
 * \ingroup bvec
 * \brief A boolean vector.
 *
 * This data structure is used to store boolean vectors. The field \a bitnum is the number of
 * elements in the vector and the field \a bitvec contains the actual BDDs in the vector. The C++
 * version of \a bvec is documented at the beginning of this document.
 * 
 */
typedef struct s_bvec
{
   int bitnum;
   BDD *bitvec;
} BVEC;

#ifndef CPLUSPLUS
typedef BVEC bvec;
#endif


#ifdef CPLUSPLUS
extern "C" {
#endif
   
   /* Prototypes for bvec.c */

/**
 * \ingroup bvec
 * \brief Create a copy of a bvec.
 *
 * Returns a copy of \a src. The result is reference counted.
 * 
 * \see bvec_con
 */
extern BVEC bvec_copy(BVEC v);


/**
 * \ingroup bvec
 * \brief Build a vector of constant true bdds.
 *
 * Builds a boolean vector with \a bitnum elements, each of which are the constant true BDD.
 * 
 * \return The boolean vector (which is already reference counted).
 * \see bvec_false, bvec_con, bvec_var
 */
extern BVEC bvec_true(int bitnum);


/**
 * \ingroup bvec
 * \brief Build a vector of constant false bdds.
 *
 * Builds a boolean vector with \a bitnum elements, each of which are the constant false BDD.
 * 
 * \return The boolean vector (which is already reference counted).
 * \see bvec_true, bvec_con, bvec_var
 */
extern BVEC bvec_false(int bitnum);


/**
 * \ingroup bvec
 * \brief Build a boolean vector representing an integer value.
 *
 * Builds a boolean vector that represents the value \a val using \a bitnum bits. The value will
 * be represented with the LSB at the position 0 and the MSB at position \a bitnum - 1.
 * 
 * \return The boolean vector (which is already reference counted).
 * \see bvec_true, bvec_false, bvec_var
 */
extern BVEC bvec_con(int bitnum, int val);


/**
 * \ingroup bvec
 * \brief Build a boolean vector with bdd variables.
 *
 * Builds a boolean vector with the BDD variables \f$v_1, \ldots, v_n\f$ as the elements. Each
 * variable will be the the variable numbered \a offset + \a N * \a step where \a N ranges from 0 to \a
 * bitnum - 1.
 * 
 * \return The boolean vector (which is already reference counted).
 * \see bvec_true, bvec_false, bvec_con
 */
extern BVEC bvec_var(int bitnum, int offset, int step);


/**
 * \ingroup bvec
 * \brief Build a boolean vector from a fdd variable block.
 *
 * Builds a boolean vector which will include exactly the variables used to define the FDD
 * variable block \a var. The vector will have the LSB at position zero.
 * 
 * \return The boolean vector (which is already reference counted).
 * \see bvec_var
 */
extern BVEC bvec_varfdd(int var);


/**
 * \ingroup bvec
 * \brief Build a boolean vector with the variables passed in an array.
 *
 * Builds a boolean vector with the BDD variables listed in the array \a var. The array must be of
 * size \a bitnum.
 * 
 * \return The boolean vector (which is already reference counted).
 * \see bvec_var
 */
extern BVEC bvec_varvec(int bitnum, int *var);


/**
 * \ingroup bvec
 * \brief Adjust the size of a boolean vector.
 *
 * Build a boolean vector with \a bitnum elements copied from \a v. If the number of elements in
 * \a v is greater than \a bitnum then the most significant bits are removed, otherwise if
 * number is smaller then the vector is padded with constant false BDDs (zeros).
 * 
 * \return The new boolean vector (which is already reference counted).
 */
extern BVEC bvec_coerce(int bitnum, BVEC v);


/**
 * \ingroup bvec
 * \brief Test a vector for constant true/false bdds.
 *
 * Returns non-zero if the vector \a v consists of only constant true or false BDDs. Otherwise
 * zero is returned. This test should precede any call to ::bvec_val.
 * 
 * \see bvec_val, bvec_con
 */
extern int  bvec_isconst(BVEC e);   


/**
 * \ingroup bvec
 * \brief Calculate the integer value represented by a boolean vector.
 *
 * Calculates the value represented by the bits in \a v assuming that the vector \a v consists of
 * only constant true or false BDDs. The LSB is assumed to be at position zero.
 * 
 * \return The integer value represented by \a v.
 * \see bvec_isconst, bvec_con
 */
extern int  bvec_val(BVEC e);   


/**
 * \ingroup bvec
 * \brief Frees all memory used by a boolean vector.
 *
 * Use this function to release any unused boolean vectors. The decrease of the reference
 * counts on the BDDs in \a v is done by ::bvec_free.
 * 
 */
extern void bvec_free(BVEC v);


/**
 * \ingroup bvec
 * \brief Increase reference count of a boolean vector.
 *
 * Use this function to increase the reference count of all BDDs in \a v. Please note that all
 * boolean vectors returned from BuDDy are reference counted from the beginning.
 * 
 * \return The boolean vector \a v.
 * \see bvec_delref
 */
extern BVEC bvec_addref(BVEC v);


/**
 * \ingroup bvec
 * \brief Decrease the reference count of a boolean vector.
 *
 * Use this function to decrease the reference count of all the BDDs in \a v.
 * 
 * \return The boolean vector \a v.
 * \see bvec_addref
 */
extern BVEC bvec_delref(BVEC v);


/**
 * \ingroup bvec
 * \brief Map a function onto a boolean vector.
 *
 * Maps the function \a fun onto all the elements in \a a. The value returned from \a fun is stored
 * in a new vector which is then returned. An example of a mapping function is ::bdd_not which
 * can be used like this 
 * \code
 * bvec res = bvec_map1(a, bdd_not)
 * \endcode
 * to negate all the BDDs in \a a.
 * 
 * \return The new vector (which is already reference counted).
 * \see bvec_map2, bvec_map3
 */
extern BVEC bvec_map1(BVEC a, BDD (*fun)(BDD));


/**
 * \ingroup bvec
 * \brief Map a function onto a boolean vector.
 *
 * Maps the function \a fun onto all the elements in \a a and \a b. The value returned from \a fun is
 * stored in a new vector which is then returned. An example of a mapping function is ::bdd_and
 * which can be used like this
 * \code
 * bvec res = bvec_map2(a, b, bdd_and)
 * \endcode
 * to calculate the logical 'and' of all the BDDs in \a a and \a b.
 * 
 * \return The new vector (which is already reference counted).
 * \see bvec_map1, bvec_map3
 */
extern BVEC bvec_map2(BVEC a, BVEC b, BDD (*fun)(BDD,BDD));


/**
 * \ingroup bvec
 * \brief Map a function onto a boolean vector.
 *
 * Maps the function \a fun onto all the elements in \a a, \a b and \a c. The value returned from \a
 * fun is stored in a new vector which is then returned. An example of a mapping function is
 * ::bdd_ite which can be used like this
 * \code
 * bvec res = bvec_map3(a, b, c, bdd_ite)
 * \endcode
 * to calculate the if-then-else function for each element in \a a, \a b and \a c.
 * 
 * \return The new vector (which is already reference counted).
 * \see bvec_map1, bvec_map2
 */
extern BVEC bvec_map3(BVEC a, BVEC b, BVEC c, BDD (*fun)(BDD,BDD,BDD));


/**
 * \ingroup bvec
 * \brief Builds a boolean vector for addition.
 *
 * Builds a new boolean vector that represents the addition of two other vectors. Each element
 * \f$x_i\f$ in the result will represent the function 
 * \f[ x_i = l_i\ \mbox{xor}\ r_i\ \mbox{xor}\ c_{i-1} \f] 
 * where the carry in \f$c_i\f$ is 
 * \f[ c_i = (l_i\ \mbox{and}\ r_i)\ \mbox{or}\ (c_{i-1}\ \mbox{and} \ (l_i\ \mbox{or}\ r_i)) \f] 
 * It is important for efficency that the BDD
 * variables used in \a l and \a r are interleaved.
 * 
 * \return The result of the addition (which is already reference counted).
 * \see bvec_sub, bvec_mul, bvec_shl
 */
extern BVEC bvec_add(BVEC left, BVEC right);


/**
 * \ingroup bvec
 * \brief Builds a boolean vector for subtraction.
 *
 * Builds a new boolean vector that represents the subtraction of two other vectors. Each
 * element \f$x_i\f$ in the result will represent the function 
 * \f[ x_i = l_i\ \mbox{xor}\ r_i\ \mbox{xor}\ c_{i-1} \f] 
 * where the carry in \f$c_i\f$ is 
 * \f[ c_i = (l_i\ \mbox{and}\ r_i\ \mbox{and}\ c_{i-1})\ \mbox{or}\ (\mbox{not}\ l_i\ \mbox{and}\ (r_i\ \mbox{or}\ c_{i-1})) \f] 
 * It is important for efficency that the BDD variables used in \a l and \a r are
 * interleaved.
 * 
 * \return The result of the subtraction (which is already reference counted).
 * \see bvec_add, bvec_mul, bvec_shl
 */
extern BVEC bvec_sub(BVEC left, BVEC right);


/**
 * \ingroup bvec
 * \brief Builds a boolean vector for multiplication with a constant.
 *
 * Builds a boolean vector representing the multiplication of \a e and \a c.
 * 
 * \return The result of the multiplication (which is already reference counted).
 * \see bvec_mul, bvec_div, bvec_add, bvec_shl
 */
extern BVEC bvec_mulfixed(BVEC e, int c);


/**
 * \ingroup bvec
 * \brief Builds a boolean vector for multiplication.
 *
 * Builds a boolean vector representing the multiplication of \a l and \a r.
 * 
 * \return The result of the multiplication (which is already reference counted).
 * \see bvec_mulfixed, bvec_div, bvec_add, bvec_shl
 */
extern BVEC bvec_mul(BVEC left, BVEC right);


/**
 * \ingroup bvec
 * \brief Builds a boolean vector for division by a constant.
 *
 * Builds a new boolean vector representing the integer division of \a e with \a c. The result of
 * the division will be stored in \a res and the remainder of the division will be stored in \a
 * rem. Both vectors should be initialized as the function will try to release the nodes used by
 * them. If an error occurs then the nodes will \em not be freed.
 * 
 * \return Zero on success or a negative error code on error.
 * \see bvec_div, bvec_mul, bvec_add, bvec_shl
 */
extern int  bvec_divfixed(BVEC e, int c, BVEC *res, BVEC *rem);


/**
 * \ingroup bvec
 * \brief Builds a boolean vector for division.
 *
 * Builds a new boolean vector representing the integer division of \a l with \a r. The result of
 * the division will be stored in \a res and the remainder of the division will be stored in \a rem.
 * Both vectors should be initialized as the function will try to release the nodes used by
 * them. If an error occurs then the nodes will \em not be freed.
 * 
 * \return Zero on success or a negative error code on error.
 * \see bvec_mul, bvec_divfixed, bvec_add, bvec_shl
 */
extern int  bvec_div(BVEC left, BVEC right, BVEC *res, BVEC *rem);
extern BVEC bvec_ite(BDD a, BVEC b, BVEC c);


/**
 * \ingroup bvec
 * \brief Shift left operation (fixed number of bits).
 *
 * Builds a boolean vector that represents \a v shifted \a pos times to the left. The new empty
 * elements will be set to \a c.
 * 
 * \return The result of the operation (which is already reference counted).
 * \see bvec_add, bvec_mul, bvec_shl, bvec_shr
 */
extern BVEC bvec_shlfixed(BVEC e, int pos, BDD c);


/**
 * \ingroup bvec
 * \brief Shift left operation (symbolic).
 *
 * Builds a boolean vector that represents \a l shifted \a r times to the left. The new empty
 * elements will be set to \a c. The shift operation is fully symbolic and the number of bits
 * shifted depends on the current value encoded by \a r.
 * 
 * \return The result of the operation (which is already reference counted).
 * \see bvec_add, bvec_mul, bvec_shlfixed, bvec_shr
 */
extern BVEC bvec_shl(BVEC l, BVEC r, BDD c);


/**
 * \ingroup bvec
 * \brief Shift right operation.
 *
 * Builds a boolean vector that represents \a v shifted \a pos times to the right. The new empty
 * elements will be set to \a c.
 * 
 * \return The result of the operation (which is already reference counted).
 * \see bvec_add, bvec_mul, bvec_shr, bvec_shl
 */
extern BVEC bvec_shrfixed(BVEC e, int pos, BDD c);


/**
 * \ingroup bvec
 * \brief Shift right operation (symbolic).
 *
 * Builds a boolean vector that represents \a l shifted \a r times to the right. The new empty
 * elements will be set to \a c. The shift operation is fully symbolic and the number of bits
 * shifted depends on the current value encoded by \a r.
 * 
 * \return The result of the operation (which is already reference counted).
 * \see bvec_add, bvec_mul, bvec_shl, bvec_shrfixed
 */
extern BVEC bvec_shr(BVEC l, BVEC r, BDD c);


/**
 * \ingroup bvec
 * \brief Calculates the truth value of \f$x < y\f$.
 *
 * Returns the BDD representing \a l \< r (\em not reference counted). Both vectors must have
 * the same number of bits.
 * 
 * \see bvec_lte, bvec_gth, bvec_gte, bvec_equ, bvec_neq
 */
extern BDD  bvec_lth(BVEC left, BVEC right);


/**
 * \ingroup bvec
 * \brief Calculates the truth value of \f$x \leq y\f$.
 *
 * Returns the BDD representing \a l \f$\leq\f$ \a r (\em not reference counted). Both vectors
 * must have the same number of bits.
 * 
 * \see bvec_lth, bvec_gth, bvec_gte, bvec_equ, bvec_neq
 */
extern BDD  bvec_lte(BVEC left, BVEC right);


/**
 * \ingroup bvec
 * \brief Calculates the truth value of \f$x > y\f$.
 *
 * Returns the BDD representing \a l \> r (\em not reference counted). Both vectors must have
 * the same number of bits.
 * 
 * \see bvec_lth, bvec_lte, bvec_gte, bvec_equ, bvec_neq
 */
extern BDD  bvec_gth(BVEC left, BVEC right);


/**
 * \ingroup bvec
 * \brief Calculates the truth value of \f$x \geq y \f$.
 *
 * Returns the BDD representing \a l \f$\geq\f$ \a r (\em not reference counted). Both vectors
 * must have the same number of bits.
 * 
 * \see bvec_lth, bvec_gth, bvec_gth, bvec_equ, bvec_neq
 */
extern BDD  bvec_gte(BVEC left, BVEC right);


/**
 * \ingroup bvec
 * \brief Calculates the truth value of \f$x = y \f$.
 *
 * Returns the BDD representing \a l = r (\em not reference counted). Both vectors must have
 * the same number of bits.
 * 
 * \see bvec_lth, bvec_lte, bvec_gth, bvec_gte, bvec_neq
 */
extern BDD  bvec_equ(BVEC left, BVEC right);


/**
 * \ingroup bvec
 * \brief Calculates the truth value of \f$x \neq y\f$.
 *
 * Returns the BDD representing \a l\f$\neq\f$\a r (\em not reference counted). Both vectors
 * must have the same number of bits.
 * 
 * \see bvec_lte, bvec_lth, bvec_gth, bvec_gth, bvec_equ
 */
extern BDD  bvec_neq(BVEC left, BVEC right);

#ifdef CPLUSPLUS
}
#endif


/*************************************************************************
   If this file is included from a C++ compiler then the following
   classes, wrappers and hacks are supplied.
*************************************************************************/
#ifdef CPLUSPLUS

/*=== User BVEC class ==================================================*/

class bvec
{
 public:

   bvec(void)                { roots.bitvec=NULL; roots.bitnum=0; }
   bvec(int bitnum)          { roots=bvec_false(bitnum); }
   bvec(int bitnum, int val) { roots=bvec_con(bitnum,val); }
   bvec(const bvec &v)       { roots=bvec_copy(v.roots); }
   ~bvec(void)               { bvec_free(roots); }

   void set(int i, const bdd &b);
   bdd operator[](int i)  const { return roots.bitvec[i]; }
   int bitnum(void) const       { return roots.bitnum; }
   int empty(void) const        { return roots.bitnum==0; }
   bvec operator=(const bvec &src);
   
private:
   BVEC roots;

   bvec(const BVEC &v) { roots=v; } /* NOTE: Must be a shallow copy! */

   friend bvec bvec_truepp(int bitnum);
   friend bvec bvec_falsepp(int bitnum);
   friend bvec bvec_conpp(int bitnum, int val);
   friend bvec bvec_varpp(int bitnum, int offset, int step);
   friend bvec bvec_varfddpp(int var);
   friend bvec bvec_varvecpp(int bitnum, int *var);
   friend bvec bvec_coerce(int bitnum, const bvec &v);
   friend int  bvec_isconst(const bvec &e);   
   friend int  bvec_val(const bvec &e);   
   friend bvec bvec_copy(const bvec &v);
   friend bvec bvec_map1(const bvec &a,
			 bdd (*fun)(const bdd &));
   friend bvec bvec_map2(const bvec &a, const bvec &b,
			 bdd (*fun)(const bdd &, const bdd &));
   friend bvec bvec_map3(const bvec &a, const bvec &b, const bvec &c,
			 bdd (*fun)(const bdd &, const bdd &, const bdd &));
   friend bvec bvec_add(const bvec &left, const bvec &right);
   friend bvec bvec_sub(const bvec &left, const bvec &right);
   friend bvec bvec_mulfixed(const bvec &e, int c);
   friend bvec bvec_mul(const bvec &left, const bvec &right);
   friend int  bvec_divfixed(const bvec &e, int c, bvec &res, bvec &rem);
   friend int  bvec_div(const bvec &l, const bvec &r, bvec &res, bvec &rem);
   friend bvec bvec_ite(const bdd& a, const bvec& b, const bvec& c);
   friend bvec bvec_shlfixed(const bvec &e, int pos, const bdd &c);
   friend bvec bvec_shl(const bvec &left, const bvec &right, const bdd &c);
   friend bvec bvec_shrfixed(const bvec &e, int pos, const bdd &c);
   friend bvec bvec_shr(const bvec &left, const bvec &right, const bdd &c);
   friend bdd  bvec_lth(const bvec &left, const bvec &right);
   friend bdd  bvec_lte(const bvec &left, const bvec &right);
   friend bdd  bvec_gth(const bvec &left, const bvec &right);
   friend bdd  bvec_gte(const bvec &left, const bvec &right);
   friend bdd  bvec_equ(const bvec &left, const bvec &right);
   friend bdd  bvec_neq(const bvec &left, const bvec &right);

public:
   bvec operator&(const bvec &a) const { return bvec_map2(*this, a, bdd_and); }
   bvec operator^(const bvec &a) const { return bvec_map2(*this, a, bdd_xor); }
   bvec operator|(const bvec &a) const { return bvec_map2(*this, a, bdd_or); }
   bvec operator!(void) const          { return bvec_map1(*this, bdd_not); }
   bvec operator<<(int a)   const   { return bvec_shlfixed(*this,a,bddfalse); }
   bvec operator<<(const bvec &a) const  { return bvec_shl(*this,a,bddfalse); }
   bvec operator>>(int a)   const   { return bvec_shrfixed(*this,a,bddfalse); }
   bvec operator>>(const bvec &a) const  { return bvec_shr(*this,a,bddfalse); }
   bvec operator+(const bvec &a) const { return bvec_add(*this, a); }
   bvec operator-(const bvec &a) const { return bvec_sub(*this, a); }
   bvec operator*(int a) const         { return bvec_mulfixed(*this, a); }
   bvec operator*(const bvec a) const  { return bvec_mul(*this, a); }
   bdd operator<(const bvec &a) const  { return bvec_lth(*this, a); }
   bdd operator<=(const bvec &a) const { return bvec_lte(*this, a); }
   bdd operator>(const bvec &a) const  { return bvec_gth(*this, a); }
   bdd operator>=(const bvec &a) const { return bvec_gte(*this, a); }
   bdd operator==(const bvec &a) const { return bvec_equ(*this, a); }
   bdd operator!=(const bvec &a) const { return bvec_neq(*this, a); }
};

std::ostream &operator<<(std::ostream &, const bvec &);

inline bvec bvec_truepp(int bitnum)
{ return bvec_true(bitnum); }

inline bvec bvec_falsepp(int bitnum)
{ return bvec_false(bitnum); }

inline bvec bvec_conpp(int bitnum, int val)
{ return bvec_con(bitnum, val); }

inline bvec bvec_varpp(int bitnum, int offset, int step)
{ return bvec_var(bitnum, offset, step); }

inline bvec bvec_varfddpp(int var)
{ return bvec_varfdd(var); }

inline bvec bvec_varvecpp(int bitnum, int *var)
{ return bvec_varvec(bitnum, var); }

inline bvec bvec_coerce(int bitnum, const bvec &v)
{ return bvec_coerce(bitnum, v.roots); }

inline int  bvec_isconst(const bvec &e)
{ return bvec_isconst(e.roots); }

inline int  bvec_val(const bvec &e)
{ return bvec_val(e.roots); }

inline bvec bvec_copy(const bvec &v)
{ return bvec_copy(v.roots); }

inline bvec bvec_add(const bvec &left, const bvec &right)
{ return bvec_add(left.roots, right.roots); }

inline bvec bvec_sub(const bvec &left, const bvec &right)
{ return bvec_sub(left.roots, right.roots); }

inline bvec bvec_mulfixed(const bvec &e, int c)
{ return bvec_mulfixed(e.roots, c); }

inline bvec bvec_mul(const bvec &left, const bvec &right)
{ return bvec_mul(left.roots, right.roots); }

inline int bvec_divfixed(const bvec &e, int c, bvec &res, bvec &rem)
{ return bvec_divfixed(e.roots, c, &res.roots, &rem.roots); }

inline int bvec_div(const bvec &l, const bvec &r, bvec &res, bvec &rem)
{ return bvec_div(l.roots, r.roots, &res.roots, &rem.roots); }

inline bvec bvec_ite(const bdd& a, const bvec& b, const bvec& c)
{ return bvec_ite(a.root, b.roots, c.roots); }

inline bvec bvec_shlfixed(const bvec &e, int pos, const bdd &c)
{ return bvec_shlfixed(e.roots, pos, c.root); }

inline bvec bvec_shl(const bvec &left, const bvec &right, const bdd &c)
{ return bvec_shl(left.roots, right.roots, c.root); }

inline bvec bvec_shrfixed(const bvec &e, int pos, const bdd &c)
{ return bvec_shrfixed(e.roots, pos, c.root); }

inline bvec bvec_shr(const bvec &left, const bvec &right, const bdd &c)
{ return bvec_shr(left.roots, right.roots, c.root); }

inline bdd  bvec_lth(const bvec &left, const bvec &right)
{ return bvec_lth(left.roots, right.roots); }

inline bdd  bvec_lte(const bvec &left, const bvec &right)
{ return bvec_lte(left.roots, right.roots); }

inline bdd  bvec_gth(const bvec &left, const bvec &right)
{ return bvec_gth(left.roots, right.roots); }

inline bdd  bvec_gte(const bvec &left, const bvec &right)
{ return bvec_gte(left.roots, right.roots); }

inline bdd  bvec_equ(const bvec &left, const bvec &right)
{ return bvec_equ(left.roots, right.roots); }

inline bdd  bvec_neq(const bvec &left, const bvec &right)
{ return bvec_neq(left.roots, right.roots); }


   /* Hack to allow for overloading */
#define bvec_var(a,b,c)  bvec_varpp(a,b,c)
#define bvec_varfdd(a)   bvec_varfddpp(a)
#define bvec_varvec(a,b) bvec_varvecpp(a,b)
#define bvec_true(a)     bvec_truepp(a)
#define bvec_false(a)    bvec_falsepp(a)
#define bvec_con(a,b)    bvec_conpp((a),(b))

   
#endif /* CPLUSPLUS */

#endif /* _BVEC_H */

/* EOF */
