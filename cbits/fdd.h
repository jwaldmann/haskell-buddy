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
  FILE:  fdd.h
  DESCR: Finite domain data with BDDs
  AUTH:  Jorn Lind
  DATE:  (C) february 1999
*************************************************************************/

/** \file fdd.h
 */

#ifndef _FDD_H
#define _FDD_H

#include "bdd.h"


#ifdef CPLUSPLUS
extern "C" {
#endif

/* In file fdd.c */
   
/**
 * \ingroup fdd
 * \brief Adds another set of finite domain blocks.
 *
 * Extends the set of finite domain blocks with the \a num domains in \a dom. Each entry in \a dom
 * defines the size of a new finite domain which later on can be used for finite state machine
 * traversal and other operations on finte domains. Each domain allocates
 * \f$\log_2(|dom[i]|)\f$ BDD variables to be used later. The ordering is interleaved for the
 * domains defined in each call to ::fdd_extdomain. This means that assuming domain \f$D_0\f$
 * needs 2 BDD variables \f$x_1\f$ and \f$x_2\f$, and another domain \f$D_1\f$ needs 4 BDD variables
 * \f$y_1,y_2,y_3\f$ and \f$y_4\f$, then the order will be \f$x_1,y_1,x_2,y_2,y_3,y_4\f$. The index of
 * the first domain in \a dom is returned. The index of the other domains are offset from this
 * index with the same offset as in \a dom. The BDD variables needed to encode the domain are
 * created for the purpose and do not interfere with the BDD variables already in use.
 * 
 * \return The index of the first domain or a negative error code.
 * \see fdd_ithvar, fdd_equals, fdd_overlapdomain
 */
extern int  fdd_extdomain(int*, int);


/**
 * \ingroup fdd
 * \brief Combine two fdd blocks into one.
 *
 * This function takes two FDD blocks and merges them into a new one, such that the new one is
 * encoded using both sets of BDD variables. If \a v1 is encoded using the BDD variables \f$a_1,  \ldots, a_n\f$ 
 * and has a domain of \f$[0,N_1]\f$, and \a v2 is encoded using \f$b_1, \ldots, b_n\f$ and
 * has a domain of \f$[0,N_2]\f$, then the result will be encoded using the BDD variables 
 * \f$a_1,  \ldots, a_n, b_1, \ldots, b_n\f$
 * and have the domain \f$[0,N_1*N_2]\f$. The use of this function
 * may result in strange output from ::fdd_printset.
 * 
 * \return The index of the finite domain block.
 * \see fdd_extdomain
 */
extern int  fdd_overlapdomain(int, int);


/**
 * \ingroup fdd
 * \brief Clear all allocated fdd blocks.
 *
 * Removes all defined finite domain blocks defined by ::fdd_extdomain() and 
 * ::fdd_overlapdomain().
 * 
 */
extern void fdd_clearall(void);


/**
 * \ingroup fdd
 * \brief Number of defined finite domain blocks.
 *
 * Returns the number of finite domain blocks define by calls to ::fdd_extdomain.
 * 
 * \return The number of defined finite domain blocks or a negative error code.
 * \see fdd_domainsize, fdd_extdomain
 */
extern int  fdd_domainnum(void);


/**
 * \ingroup fdd
 * \brief Real size of a finite domain block.
 *
 * Returns the size of the domain for the finite domain block \a var.
 * 
 * \return The size or a negative error code.
 * \see fdd_domainnum
 */
extern int  fdd_domainsize(int);


/**
 * \ingroup fdd
 * \brief Binary size of a finite domain block.
 *
 * Returns the number of BDD variables used for the finite domain block \a var.
 * 
 * \return The number of variables or a negative error code.
 * \see fdd_vars
 */
extern int  fdd_varnum(int);


/**
 * \ingroup fdd
 * \brief All bdd variables associated with a finite domain block.
 *
 * Returns an integer array containing the BDD variables used to define the finite domain
 * block \a var. The size of the array is the number of variables used to define the finite domain
 * block. The array will have the Least Significant Bit at pos 0. The array must \em not be
 * deallocated.
 * 
 * \return Integer array contaning the variable numbers or NULL if \a v is an unknown block.
 * \see fdd_varnum
 */
extern int* fdd_vars(int);


/**
 * \ingroup fdd
 * \brief The bdd for the i'th fdd set to a specific value.
 *
 * Returns the BDD that defines the value \a val for the finite domain block \a var. The encoding
 * places the Least Significant Bit at the top of the BDD tree (which means they will have the
 * lowest variable index). The returned BDD will be \f$V_0 \land V_1 \land \ldots \land V_N\f$
 * where each \f$V_i\f$ will be in positive or negative form depending on the value of \a val.
 * 
 * \return The correct BDD or the constant false BDD on error.
 * \see fdd_ithset
 */
extern BDD  fdd_ithvar(int, int);


/**
 * \ingroup fdd
 * \brief Finds one satisfying value of a fdd variable.
 *
 * Finds one satisfying assignment of the FDD variable \a var in the BDD \a r and returns this
 * value.
 * 
 * \return The value of a satisfying assignment of \a var. If \a r is the trivially false BDD, 
 * then a negative value is returned.
 * \see fdd_scanallvar
 */
extern int  fdd_scanvar(BDD, int);


/**
 * \ingroup fdd
 * \brief Finds one satisfying value of all fdd variables.
 *
 * Finds one satisfying assignment in \a r of all the defined FDD variables. Each value is
 * stored in an array which is returned. The size of this array is exactly the number of FDD
 * variables defined. It is the user's responsibility to free this array using \c free().
 * 
 * \return An array with all satisfying values. If \a r is the trivially false BDD, then \c NULL is returned.
 * \see fdd_scanvar
 */
extern int* fdd_scanallvar(BDD);


/**
 * \ingroup fdd
 * \brief The variable set for the i'th finite domain block.
 *
 * Returns the variable set that contains the variables used to define the finite domain block
 * \a var.
 * 
 * \return The variable set or the constant false BDD on error.
 * \see fdd_ithvar
 */
extern BDD  fdd_ithset(int);


/**
 * \ingroup fdd
 * \brief Bdd encoding of the domain of a fdd variable.
 *
 * Returns what corresponds to a disjunction of all possible values of the variable \a var.
 * This is more efficient than doing <tt> fdd_ithvar(var,0) OR fdd_ithvar(var,1) ... </tt>
 * explicitly for all values in the domain of \a var.
 * 
 * \return The encoding of the domain.
 */
extern BDD  fdd_domain(int);


/**
 * \ingroup fdd
 * \brief Returns a bdd setting two fd. blocks equal.
 *
 * Builds a BDD which is true for all the possible assignments to the variable blocks \a f and \a g
 * that makes the blocks equal. This is more or less just a shorthand for calling ::fdd_equals().
 * 
 * \return The correct BDD or the constant false on errors.
 */
extern BDD  fdd_equals(int, int);


/**
 * \ingroup fdd
 * \brief Specifies a printing callback handler.
 *
 * A printing callback handler for use with FDDs is used to convert the FDD integer identifier
 * into something readable by the end user. Typically the handler will print a string name
 * instead of the identifier. A handler could look like this: 
 * \code
 * void * printhandler(FILE *o, int var) 
 * { 
 *   extern char **names; 
 *   fprintf(o, "%s", names[var]); 
 * }
 * \endcode
 * The handler can then be passed to BuDDy like this: 
 * \code
 * fdd_file_hook(printhandler)
 * \endcode
 * No default handler is supplied. The argument \a handler
 * may be \c NULL if no handler is needed.
 * 
 * \return The old handler.
 * \see fdd_printset, bdd_file_hook
 */
extern bddfilehandler fdd_file_hook(bddfilehandler);
#ifdef CPLUSPLUS
extern bddstrmhandler fdd_strm_hook(bddstrmhandler);
#endif


/**
 * \ingroup fdd
 * \brief Prints a bdd for a finite domain block to \c stdout.
 *
 * Prints the BDD \a r using a set notation as in ::bdd_printset but with the index of the finite
 * domain blocks included instead of the BDD variables. It is possible to specify a printing
 * callback function with ::fdd_file_hook or ::fdd_strm_hook which can be used to print the
 * FDD identifier in a readable form.
 * 
 * \see bdd_printset, fdd_file_hook, fdd_strm_hook
 */
extern void fdd_printset(BDD);


/**
 * \ingroup fdd
 * \brief Prints a bdd for a finite domain block to a file.
 *
 * Prints the BDD \a r to \a ofile using a set notation as in ::bdd_printset but with the index of the finite
 * domain blocks included instead of the BDD variables. It is possible to specify a printing
 * callback function with ::fdd_file_hook or ::fdd_strm_hook which can be used to print the
 * FDD identifier in a readable form.
 * 
 * \see bdd_printset, fdd_file_hook, fdd_strm_hook
 */
extern void fdd_fprintset(FILE*, BDD);


/**
 * \ingroup fdd
 * \brief Scans a variable set.
 *
 * Scans the BDD \a r to find all occurences of FDD variables and then stores these in \a varset.
 * \a varset will be set to point to an array of size \a varnum which will contain the indices of
 * the found FDD variables. It is the users responsibility to free \a varset after use.
 * 
 * \return Zero on success or a negative error code on error.
 * \see fdd_makeset
 */
extern int  fdd_scanset(BDD, int**, int*);


/**
 * \ingroup fdd
 * \brief Creates a variable set for n finite domain blocks.
 *
 * Returns a BDD defining all the variable sets used to define the variable blocks in the array
 * \a varset. The argument \a varnum defines the size of \a varset.
 * 
 * \return The correct BDD or the constant false on errors.
 * \see fdd_ithset, bdd_makeset
 */
extern BDD  fdd_makeset(int*, int);


/**
 * \ingroup fdd
 * \brief Adds a new variable block for reordering.
 *
 * Works exactly like ::bdd_addvarblock except that ::fdd_intaddvarblock takes a range of
 * FDD variables instead of BDD variables.
 * 
 * \return Zero on success, otherwise a negative error code.
 * \see bdd_addvarblock, bdd_intaddvarblock, bdd_reorder
 */
extern int  fdd_intaddvarblock(int, int, int);


/**
 * \ingroup fdd
 * \brief Defines a pair for two finite domain blocks.
 *
 * Defines each variable in the finite domain block \a p1 to be paired with the corresponding
 * variable in \a p2. The result is stored in \a pair which must be allocated using
 * ::bdd_makepair.
 * 
 * \return Zero on success or a negative error code on error.
 * \see fdd_setpairs
 */
extern int  fdd_setpair(bddPair*, int, int);


/**
 * \ingroup fdd
 * \brief Defines n pairs for finite domain blocks.
 *
 * Defines each variable in all the finite domain blocks listed in the array \a p1 to be paired
 * with the corresponding variable in \a p2. The result is stored in \a pair which must be
 * allocated using ::bdd_makeset.
 * 
 * \return Zero on success or a negative error code on error.
 * \see bdd_setpair
 */
extern int  fdd_setpairs(bddPair*, int*, int*, int);

#ifdef CPLUSPLUS
}
#endif

/*************************************************************************
   If this file is included from a C++ compiler then the following
   classes, wrappers and hacks are supplied.
*************************************************************************/
#ifdef CPLUSPLUS

   /* FDD extensions */

inline bdd fdd_ithvarpp(int var, int val)
{ return fdd_ithvar(var, val); }

inline bdd fdd_ithsetpp(int var)
{ return fdd_ithset(var); }

inline bdd fdd_domainpp(int var)
{ return fdd_domain(var); }

inline int fdd_scanvar(const bdd &r, int var)
{ return fdd_scanvar(r.root, var); }

inline int* fdd_scanallvar(const bdd &r)
{ return fdd_scanallvar(r.root); }

inline bdd fdd_equalspp(int left, int right)
{ return fdd_equals(left, right); }

inline void fdd_printset(const bdd &r)
{ fdd_printset(r.root); }

inline void fdd_fprintset(FILE* ofile, const bdd &r)
{ fdd_fprintset(ofile, r.root); }

inline int fdd_scanset(const bdd &r, int *&v, int &n)
{ return fdd_scanset(r.root, &v, &n); }

inline bdd fdd_makesetpp(int *v, int n)
{ return fdd_makeset(v,n); }

#if 0
inline bdd* fdd_conpp(int bitnum, int var)
{ return fdd_transfer( bitnum, fdd_con(bitnum, var) ); }

inline bdd* fdd_varpp(int bitnum, int var)
{ return fdd_transfer( bitnum, fdd_var(bitnum, var) ); }

extern int fdd_isconst(int bitnum, bdd *e);
extern int fdd_val(int bitnum, bdd *e);

inline bdd* fdd_add(int bitnum, bdd *left, bdd *right)
{ return fdd_termopr(bitnum, left, right,bdd::fddAdd); }

inline bdd* fdd_sub(int bitnum, bdd *left, bdd *right)
{ return fdd_termopr(bitnum, left, right,bdd::fddSub); }

inline bdd* fdd_shl(int bitnum, bdd *expr, bdd c)
{ return fdd_shift(bitnum, expr, c, bdd::fddShl); }

inline bdd* fdd_shr(int bitnum, bdd *expr, bdd c)
{ return fdd_shift(bitnum, expr, c, bdd::fddShr); }

inline bdd fdd_lth(int bitnum, bdd *left, bdd *right)
{ return fdd_relopr(bitnum, left, right, bdd::fddLth); }

inline bdd fdd_lte(int bitnum, bdd *left, bdd *right)
{ return fdd_relopr(bitnum, left, right, bdd::fddLte); }

inline bdd fdd_gth(int bitnum, bdd *left, bdd *right)
{ return fdd_relopr(bitnum, left, right, bdd::fddGth); }

inline bdd fdd_gte(int bitnum, bdd *left, bdd *right)
{ return fdd_relopr(bitnum, left, right, bdd::fddGte); }

inline bdd fdd_equ(int bitnum, bdd *left, bdd *right)
{ return fdd_relopr(bitnum, left, right, bdd::fddEqu); }

inline bdd fdd_neq(int bitnum, bdd *left, bdd *right)
{ return fdd_relopr(bitnum, left, right, bdd::fddNeq); }
#endif

   /* Hacks to allow for overloading of return-types only */
#define fdd_ithvar fdd_ithvarpp
#define fdd_ithset fdd_ithsetpp
#define fdd_domain fdd_domainpp
#define fdd_equals fdd_equalspp
#define fdd_makeset fdd_makesetpp
#define fdd_con fdd_conpp
#define fdd_var fdd_varpp


#endif /* CPLUSPLUS */

#endif /* _FDD_H */


/* EOF */
