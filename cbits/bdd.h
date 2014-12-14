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
  FILE:  bdd.h
  DESCR: C,C++ User interface for the BDD package
  AUTH:  Jorn Lind
  DATE:  (C) feb 1997
*************************************************************************/
/** \file bdd.h
 */

#ifndef _BDD_H
#define _BDD_H

   /* Allow this headerfile to define C++ constructs if requested */
#ifdef __cplusplus
#define CPLUSPLUS
#endif

#include <stdio.h>

/*=== Defined operators for apply calls ================================*/

#define bddop_and       0
#define bddop_xor       1
#define bddop_or        2
#define bddop_nand      3
#define bddop_nor       4
#define bddop_imp       5
#define bddop_biimp     6
#define bddop_diff      7
#define bddop_less      8
#define bddop_invimp    9

   /* Should *not* be used in bdd_apply calls !!! */
#define bddop_not      10
#define bddop_simplify 11


/*=== User BDD types ===================================================*/

/**
 * Data type for representing BDDs.  
 */
typedef int BDD;

#ifndef CPLUSPLUS

/**
 * Alternative name for data type for representing BDDs. 
 */
typedef BDD bdd;
#endif /* CPLUSPLUS */

/**
 * Data type for representing a set of variable substitutions for use with ::bdd_replace. 
 */
typedef struct s_bddPair
{
   BDD *result;
   int last;
   int id;
   struct s_bddPair *next;
} bddPair;


/*=== Status information ===============================================*/

/**
 * \ingroup kernel
 * 
 * Status information about the bdd package.
 * 
 * \see bdd_stats
 */
typedef struct s_bddStat
{
   long int produced;		/**< Total number of new nodes ever produced.*/	
   int nodenum;				/**< Currently allocated number of bdd nodes. */
   int maxnodenum;			/**< User defined maximum number of bdd nodes. */
   int freenodes;			/**< Number of currently free nodes. */
   int minfreenodes;		/**< Minimum number of nodes that should be left after a garbage collection. */
   int varnum;				/**< Number of defined bdd variables. */
   int cachesize;			/**< Number of entries in the internal caches. */
   int gbcnum;				/**< Number of garbage collections done until now. */   
} bddStat;


/**
 * \ingroup kernel
 * 
 * Status information about garbage collections.
 *
 * \see bdd_gbc_hook
 */
typedef struct s_bddGbcStat
{
   int nodes;		/**< Total number of allocated nodes in the nodetable. */
   int freenodes;	/**< Number of free nodes in the nodetable. */
   long time;		/**< Time used for garbage collection this time. */
   long sumtime;	/**< Total time used for garbage collection. */
   int num;			/**< Number of garbage collections done until now. */
} bddGbcStat;


/**
 * \ingroup kernel
 * 
 * Status information about cache usage.
 * 
 * \see bdd_cachestats
 */
typedef struct s_bddCacheStat
{
   long unsigned int uniqueAccess;	/**< Number of accesses to the unique node table. */
   long unsigned int uniqueChain;	/**< Number of iterations through the cache chains in the unique node table. */
   long unsigned int uniqueHit;		/**< Number of entries actually found in the the unique node table. */
   long unsigned int uniqueMiss;	/**< Number of entries not found in the the unique node table. */
   long unsigned int opHit;			/**< Number of entries found in the operator caches. */
   long unsigned int opMiss;		/**< Number of entries not found in the operator caches. */
   long unsigned int swapCount;		/**< Number of variable swaps in reordering. */
} bddCacheStat;

/*=== BDD interface prototypes =========================================*/

/**
 * \ingroup operator
 * \brief Relational product.
 *
 * Calculates the relational product of \a a and \a b as \a a AND b with the variables in \a var
 * quantified out afterwards.
 * 
 * \return The relational product or ::bddfalse on errors.
 * \see bdd_appex
 */
#define bdd_relprod(a,b,var) bdd_appex((a),(b),bddop_and,(var))


  /* In file "kernel.c" */

#ifdef CPLUSPLUS
extern "C" {
#endif

/**
 * Data type for error handlers for use with ::bdd_error_hook.
 */
typedef void (*bddinthandler)(int);
/**
 * Data type for garbage collection handlers for use with ::bdd_gbc_hook.
 */
typedef void (*bddgbchandler)(int,bddGbcStat*);
/**
 * Data type for node table resize handlers for use with ::bdd_resize_hook. 
 */
typedef void (*bdd2inthandler)(int,int);
/**
 * Data type for BDD minimization handlers for use with ::bdd_resize_hook.
 */
typedef int  (*bddsizehandler)(void);
/**
 * Data type for printing handlers for use with ::bdd_file_hook.
 */
typedef void (*bddfilehandler)(FILE *, int);
/**
 * Data type for satisfying assignment handlers for use with ::bdd_allsat.
 */
typedef void (*bddallsathandler)(char*, int);
   

/**
 * \ingroup kernel
 * \brief Set a handler for error conditions.
 *
 * Whenever an error occurs in the bdd package a test is done to see if an error handler is
 * supplied by the user and if such exists then it will be called with an error code in the
 * variable \a errcode. The handler may then print any usefull information and return or exit
 * afterwards. This function sets the handler to be \a handler. If a \c NULL argument is
 * supplied then no calls are made when an error occurs. Possible error codes are found in
 * bdd.h. The default handler is ::bdd_default_errhandler which will use \c abort() to
 * terminate the program. Any handler should be defined like this: 
 * \code
 * void my_error_handler(int errcode) { ... } 
 * \endcode
 * 
 * \return The previous handler.
 * \see bdd_errstring
 */
extern bddinthandler  bdd_error_hook(bddinthandler handler);


/**
 * \ingroup kernel
 * \brief Set a handler for garbage collections.
 *
 * Whenever a garbage collection is required, a test is done to see if a handler for this event is
 * supplied by the user and if such exists then it is called, both before and after the garbage
 * collection takes places. This is indicated by an integer flag \a pre passed to the handler,
 * which will be one before garbage collection and zero after garbage collection. This
 * function sets the handler to be \a handler. If a \c NULL argument is supplied then no calls are
 * made when a garbage collection takes place. The argument \a pre indicates pre vs. post
 * garbage collection and the argument \a stat contains information about the garbage
 * collection. The default handler is ::bdd_default_gbchandler. Any handler should be
 * defined like this: 
 * \code
 * void my_gbc_handler(int pre, bddGbcStat *stat) { ... } 
 * \endcode
 * 
 * \return The previous handler.
 * \see bdd_resize_hook, bdd_reorder_hook
 */
extern bddgbchandler  bdd_gbc_hook(bddgbchandler handler);


/**
 * \ingroup kernel
 * \brief Set a handler for nodetable resizes.
 *
 * Whenever it is impossible to get enough free nodes by a garbage collection then the node
 * table is resized and a test is done to see if a handler is supllied by the user for this event. If
 * so then it is called with \a oldsize being the old nodetable size and \a newsize being the new
 * nodetable size. This function sets the handler to be \a handler. If a \c NULL argument is
 * supplied then no calls are made when a table resize is done. No default handler is supplied.
 * Any handler should be defined like this: 
 * \code
 * void my_resize_handler(int * oldsize, int newsize) { ... } 
 * \endcode
 * 
 * \return The previous handler.
 * \see bdd_gbc_hook, bdd_reorder_hook, bdd_setminfreenodes
 */
extern bdd2inthandler bdd_resize_hook(bdd2inthandler handler);


/**
 * \ingroup reorder
 * \brief Sets a handler for automatic reorderings.
 *
 * Whenever automatic reordering is done, a check is done to see if the user has supplied a
 * handler for that event. If so then it is called with the argument \a prestate being 1 if the
 * handler is called immediately \em before reordering and \a prestate being 0 if it is
 * called immediately after. The default handler is ::bdd_default_reohandler which will
 * print information about the reordering. A typical handler could look like this:
 * \code
 * void reorderhandler(int prestate) 
 * { 
 *   if (prestate) 
 *     printf("Start reordering"); 
 *   else 
 *     printf("End reordering"); 
 * }
 * \endcode
 * 
 * \return The previous handler.
 * \see bdd_reorder, bdd_autoreorder, bdd_resize_hook
 */
extern bddinthandler  bdd_reorder_hook(bddinthandler handler);


/**
 * \ingroup kernel
 * \brief Specifies a printing callback handler.
 *
 * A printing callback handler for use with BDDs is used to convert the BDD variable number into
 * something readable by the end user. Typically the handler will print a string name instead
 * of the number. A handler could look like this: 
 * 
 * \code
 * void printhandler(FILE **o, int var) 
 * { 
 *   extern char **names; fprintf(o, "%s", names[var]); 
 * } 
 * \endcode
 * 
 * The handler can then be passed to BuDDy like this:
 * 
 * \code
 * bdd_file_hook(printhandler);
 * \endcode
 * 
 * No default handler is supplied. The argument \a handler
 * may be \c NULL if no handler is needed.
 * 
 * \return The old handler.
 * \see bdd_printset, bdd_strm_hook, fdd_file_hook
 */
extern bddfilehandler bdd_file_hook(bddfilehandler handler);


/**
 * \ingroup kernel
 * \brief Initializes the bdd package.
 *
 * This function initiates the bdd package and \em must be called before any bdd operations
 * are done. The argument \a nodesize is the initial number of nodes in the nodetable and \a
 * cachesize is the fixed size of the internal caches. Typical values for \a nodesize are 10000
 * nodes for small test examples and up to 1000000 nodes for large examples. A cache size of
 * 10000 seems to work good even for large examples, but lesser values should do it for smaller
 * examples. The number of cache entries can also be set to depend on the size of the nodetable
 * using a call to ::bdd_setcacheratio. The initial number of nodes is not critical for any bdd
 * operation as the table will be resized whenever there are to few nodes left after a garbage
 * collection. But it does have some impact on the efficency of the operations.
 * 
 * \return If no errors occur then 0 is returned, otherwise a negative error code.
 * \see bdd_done, bdd_resize_hook
 */
extern int      bdd_init(int nodesize, int cachesize);


/**
 * \ingroup kernel
 * \brief Resets the bdd package.
 *
 * This function frees all memory used by the bdd package and resets the package to it's initial
 * state.
 * 
 * \see bdd_init
 */
extern void     bdd_done(void);


/**
 * \ingroup kernel
 * \brief Set the number of used bdd variables.
 *
 * This function is used to define the number of variables used in the bdd package. It may be
 * called more than one time, but only to increase the number of variables. The argument \a num
 * is the number of variables to use.
 * 
 * \return Zero on succes, otherwise a negative error code.
 * \see bdd_ithvar, bdd_varnum, bdd_extvarnum
 */
extern int      bdd_setvarnum(int num);


/**
 * \ingroup kernel
 * \brief Add extra bdd variables.
 *
 * Extends the current number of allocated BDD variables with \a num extra variables.
 * 
 * \return The old number of allocated variables or a negative error code.
 * \see bdd_setvarnum, bdd_ithvar, bdd_nithvar
 */
extern int      bdd_extvarnum(int num);


/**
 * \ingroup kernel
 * \brief Test whether the package is started or not.
 *
 * This function tests the internal state of the package and returns a status.
 * 
 * \return 1 (true) if the package has been started, otherwise 0.
 * \see bdd_init, bdd_done
 */
extern int      bdd_isrunning(void);


/**
 * \ingroup kernel
 * \brief Set the maximum available number of bdd nodes.
 *
 * This function sets the maximal number of bdd nodes the package may allocate before it gives
 * up a bdd operation. The argument \a size is the absolute maximal number of nodes there may be
 * allocated for the nodetable. Any attempt to allocate more nodes results in the constant
 * false being returned and the error handler being called until some nodes are deallocated. A
 * value of 0 is interpreted as an unlimited amount. It is \em not possible to specify fewer
 * nodes than there has already been allocated.
 * 
 * \return The old threshold on success, otherwise a negative error code.
 * \see bdd_setmaxincrease, bdd_setminfreenodes
 */
extern int      bdd_setmaxnodenum(int size);


/**
 * \ingroup kernel
 * \brief Set maximum number of nodes used to increase node table.
 *
 * The node table is expanded by doubling the size of the table when no more free nodes can be
 * found, but a maximum for the number of new nodes added can be set with ::bdd_setmaxincrease to 
 * \a size nodes. The default is 50000 nodes (1 Mb).
 * 
 * \return The old threshold on succes, otherwise a negative error code.
 * \see bdd_setmaxnodenum, bdd_setminfreenodes
 */
extern int      bdd_setmaxincrease(int size);


/**
 * \ingroup kernel
 * \brief Set minimum number of nodes to be reclaimed after gbc (as a percentage).
 *
 * Whenever a garbage collection is executed the number of free nodes left are checked to see if
 * a resize of the node table is required. 
 * If (::bdd_getnodenum() - ::bdd_getallocnum())*100/::bdd_getallocnum() <= \a mf then a resize is
 * initiated. The range of \a mf is of course \f$0\ldots 100\f$ and has some influence on how fast the
 * package is. A low number means harder attempts to avoid resizing and saves space, and a high
 * number reduces the time used in garbage collections. The default value is 20.
 * 
 * \return The old threshold on success, otherwise a negative error code.
 * \see bdd_setmaxnodenum, bdd_setmaxincrease
 */
extern int      bdd_setminfreenodes(int mf);


/**
 * \ingroup kernel
 * \brief Get the number of active nodes in use.
 *
 * Returns the number of nodes in the nodetable that are currently in use. Note that dead nodes
 * that have not been reclaimed yet by a garbage collection are counted as active.
 * 
 * \return The number of nodes.
 * \see bdd_getallocnum, bdd_setmaxnodenum
 */
extern int      bdd_getnodenum(void);


/**
 * \ingroup kernel
 * \brief Get the number of allocated nodes.
 *
 * Returns the number of nodes currently allocated. This includes both dead and active nodes.
 * 
 * \return The number of nodes.
 * \see bdd_getnodenum, bdd_setmaxnodenum
 */
extern int      bdd_getallocnum(void);


/**
 * \ingroup kernel
 * \brief Returns a text string with version information.
 *
 * This function returns a text string with information about the version of the bdd package.
 * 
 * \see bdd_versionnum
 */
extern char*    bdd_versionstr(void);


/**
 * \ingroup kernel
 * \brief Returns the version number of the bdd package.
 *
 * This function returns the version number of the bdd package. The number is in the range 10-99
 * for version 1.0 to 9.9.
 * 
 * \see bdd_versionstr
 */
extern int      bdd_versionnum(void);


/**
 * \ingroup kernel
 * \brief Returns some status information about the bdd package.
 *
 * This function acquires information about the internal state of the bdd package. The status
 * information is written into the \a stat argument.
 * 
 * \see bddStat
 */
extern void     bdd_stats(bddStat *stat);


/**
 * \ingroup kernel
 * \brief Fetch cache access usage.
 *
 * Fetches cache usage information and stores it in \a s. The fields of \a s can be found in the
 * documentation for ::bddCacheStat. This function may or may not be compiled into the BuDDy
 * package - depending on the setup at compile time of BuDDy.
 * 
 * \see bddCacheStat, bdd_printstat
 */
extern void     bdd_cachestats(bddCacheStat *s);


/**
 * \ingroup kernel
 * \brief Print cache statistics to a file.
 *
 * Prints information about the cache performance to the supplied file \a f.
 * The information contains the number of accesses to the unique node table, the number of
 * times a node was (not) found there and how many times a hash chain had to traversed. Hit and
 * miss count is also given for the operator caches.
 * 
 * \see bddCacheStat, bdd_cachestats, bdd_printstat
 */
extern void     bdd_fprintstat(FILE *f);


/**
 * \ingroup kernel
 * \brief Print cache statistics to \c stdout.
 *
 * Prints information about the cache performance on standard output.
 * The information contains the number of accesses to the unique node table, the number of
 * times a node was (not) found there and how many times a hash chain had to traversed. Hit and
 * miss count is also given for the operator caches.
 * 
 * \see bddCacheStat, bdd_cachestats, bdd_fprintstat
 */
extern void     bdd_printstat(void);


/**
 * \ingroup kernel
 * \brief Default garbage collection handler.
 * 
 * The default garbage collection handler prints to \c stdout:
 * - The number of garbage collections done.
 * - The total number of nodes allocated.
 * - The total number of free nodes.
 * - The time taken by garbage collecting this time.
 * - The total time taken by all garbage collections.
 * 
 * \see bdd_gbc_hook
 */
extern void     bdd_default_gbchandler(int, bddGbcStat *);


/** 
 * \ingroup kernel
 * \brief Default error handler.
 * 
 * The default error handler prints the current error message corresponding to
 * \a e on \c stdout and calls \c abort().
 * 
 * \see bdd_error_hook
 */
extern void     bdd_default_errhandler(int e);


/**
 * \ingroup kernel
 * \brief Converts an error code to a string.
 *
 * Converts a negative error code \a errorcode to a descriptive string that can be used for
 * error handling.
 * 
 * \return An error description string if \a e is known, otherwise \c NULL.
 * \see bdd_error_hook
 */
extern const char *bdd_errstring(int);


/**
 * \ingroup kernel
 * \brief Clears an error condition in the kernel.
 *
 * The BuDDy kernel may at some point run out of new ROBDD nodes if a maximum limit is set with
 * ::bdd_setmaxnodenum. In this case the current error handler is called and an internal
 * error flag is set. Further calls to BuDDy will always return ::bddfalse. From here BuDDy
 * must either be restarted or ::bdd_clear_error may be called after action is taken to let
 * BuDDy continue. This may not be especially usefull since the default error handler exits
 * the program - other needs may of course exist.
 * 
 * \see bdd_error_hook, bdd_setmaxnodenum
 */
extern void     bdd_clear_error(void);


#ifndef CPLUSPLUS


/**
 * \ingroup kernel
 * \brief Returns the constant true bdd.
 *
 * This function returns the constant true bdd and can freely be used together with the
 * ::bddtrue and ::bddfalse constants.
 * 
 * \return The constant true bdd.
 * \see bdd_false, bddtrue, bddfalse
 */
extern BDD      bdd_true(void);


/**
 * \ingroup kernel
 * \brief Returns the constant false bdd.
 *
 * This function returns the constant false bdd and can freely be used together with the
 * ::bddtrue and ::bddfalse constants.
 * 
 * \return The constant false bdd.
 * \see bdd_true, bddtrue, bddfalse
 */
extern BDD      bdd_false(void);


#endif


/**
 * \ingroup kernel
 * \brief Returns the number of defined variables.
 *
 * This function returns the number of variables defined by a call to ::bdd_setvarnum.
 * 
 * \return The number of defined variables.
 * \see bdd_setvarnum, bdd_ithvar
 */
extern int      bdd_varnum(void);


/**
 * \ingroup kernel
 * \brief Returns a bdd representing the i'th variable.
 *
 * This function is used to get a bdd representing the i'th variable (one node with the children
 * true and false). The requested variable must be in the range define by ::bdd_setvarnum
 * starting with 0 being the first. For ease of use then the bdd returned from ::bdd_ithvar does
 * not have to be referenced counted with a call to ::bdd_addref. The initial variable order is
 * defined by the index \a var that also defines the position in the variable order --
 * variables with lower indices are before those with higher indices.
 * 
 * \return The i'th variable on success, otherwise the constant false bdd.
 * \see bdd_setvarnum, bdd_nithvar, bddtrue, bddfalse
 */
extern BDD      bdd_ithvar(int var);


/**
 * \ingroup kernel
 * \brief Returns a bdd representing the negation of the i'th variable.
 *
 * This function is used to get a bdd representing the negation of the i'th variable (one node
 * with the children false and true). The requested variable must be in the range defined by
 * ::bdd_setvarnum starting with 0 being the first. For ease of use then the bdd returned from
 * ::bdd_nithvar does not have to be referenced counted with a call to ::bdd_addref.
 * 
 * \return The negated i'th variable on success, otherwise the constant false bdd.
 * \see bdd_setvarnum, bdd_ithvar, bddtrue, bddfalse
 */
extern BDD      bdd_nithvar(int var);


/**
 * \ingroup info
 * \brief Gets the variable labeling the bdd.
 *
 * Gets the variable labeling the bdd \a r.
 * 
 * \return The variable number.
 */
extern int      bdd_var(BDD r);


/**
 * \ingroup info
 * \brief Gets the false branch of a bdd.
 *
 * Gets the false branch of the bdd \a r.
 * 
 * \return The bdd of the false branch.
 * \see bdd_high
 */
extern BDD      bdd_low(BDD r);


/**
 * \ingroup info
 * \brief Gets the true branch of a bdd.
 *
 * Gets the true branch of the bdd \a r.
 * 
 * \return The bdd of the true branch.
 * \see bdd_low
 */
extern BDD      bdd_high(BDD r);


extern int      bdd_varlevel(int);


/**
 * \ingroup kernel
 * \brief Increases the reference count on a node.
 *
 * Reference counting is done on externaly referenced nodes only and the count for a specific
 * node \a r can and must be increased using this function to avoid loosing the node in the next
 * garbage collection.
 * 
 * \see bdd_delref
 * \return The BDD node \a r.
 */
extern BDD      bdd_addref(BDD r);


/**
 * \ingroup kernel
 * \brief Decreases the reference count on a node.
 *
 * Reference counting is done on externaly referenced nodes only and the count for a specific
 * node \a r can and must be decreased using this function to make it possible to reclaim the node
 * in the next garbage collection.
 * 
 * \see bdd_addref
 * \return The BDD node \a r.
 */
extern BDD      bdd_delref(BDD r);

/**
 * \ingroup kernel
 * \brief Performs a manual garbage collection.
 * 
 * This function can be used to perform a manual, explicit (non-automatic)
 * garbage collection of all unreferenced bdd nodes.
 * 
 * \see bdd_gbc_hook
 */
extern void     bdd_gbc(void);


/**
 * \ingroup kernel
 * \brief Returns an integer representation of a variable set.
 *
 * Scans a variable set \a r and copies the stored variables into an integer array of variable
 * numbers. The argument \a varset is the address of an integer pointer where the array is stored and
 * \a varnum is a pointer to an integer where the number of elements are stored. It is the user's
 * responsibility to make sure the array is deallocated by a call to \c free(v). The numbers
 * returned are guaranteed to be in ascending order.
 * 
 * \see bdd_makeset
 * \return Zero on success, otherwise a negative error code.
 */
extern int      bdd_scanset(BDD a, int **varset, int *varnum);


/**
 * \ingroup kernel
 * \brief Builds a bdd variable set from an integer array.
 *
 * Reads a set of variable numbers from the integer array \a varset which must hold exactly \a varnum
 * integers and then builds a BDD representing the variable set. The BDD variable set is
 * represented as the conjunction of all the variables in their positive form and may just as
 * well be made that way by the user. The user should keep a reference to the returned BDD instead
 * of building it every time the set is needed.
 * 
 * \see bdd_scanset
 * \return A BDD variable set.
 */
extern BDD      bdd_makeset(int *varset, int varnum);


/**
 * \ingroup kernel
 * \brief Creates an empty variable pair table.
 *
 * Variable pairs of the type ::bddPair are used in ::bdd_replace to define which variables to
 * replace with other variables. This function allocates such an empty table. The table can be
 * freed by a call to ::bdd_freepair.
 * 
 * \return Returns a new table of pairs.
 * \see bdd_freepair, bdd_replace, bdd_setpair, bdd_setpairs
 */
extern bddPair* bdd_newpair(void);


/**
 * \ingroup kernel
 * \brief Set one variable pair.
 *
 * Adds the pair (\a oldvar,\a newvar) to the table of pairs \a pair. This results in \a oldvar
 * being substituted with \a newvar in a call to ::bdd_replace. The parameter \a newvar
 * is an integer representing the variable to be replaced with the old variable. 
 * 
 * \return Zero on success, otherwise a negative error code.
 * \see bdd_newpair, bdd_setpairs, bdd_resetpair, bdd_replace, bdd_compose
 */
extern int      bdd_setpair(bddPair *pair, int oldvar, int newvar);


/**
 * \ingroup kernel
 * \brief Defines a whole set of pairs.
 *
 * Like ::bdd_setpair but with \a oldvar and \a newvar being arrays of variables (BDDs) of
 * size \a size.
 * 
 * \return Zero on success, otherwise a negative error code.
 * \see bdd_newpair, bdd_setpair, bdd_replace, bdd_compose
 */
extern int      bdd_setpairs(bddPair *pair, int *oldvar, int *newvar, int size);


/**
 * \ingroup kernel
 * \brief Set one variable pair.
 *
 * Adds the pair (\a oldvar,\a newvar) to the table of pairs \a pair. This results in \a oldvar
 * being substituted with \a newvar in a call to ::bdd_replace. 
 * The variable \a oldvar is substituted with the BDD \a newvar. 
 * The possibility to substitute with any BDD as \a newvar is utilized in ::bdd_compose,
 * whereas only the topmost variable in the BDD is used in ::bdd_replace.
 * 
 * \return Zero on success, otherwise a negative error code.
 * \see bdd_newpair, bdd_setpairs, bdd_resetpair, bdd_replace, bdd_compose
 */
extern int      bdd_setbddpair(bddPair *pair, int oldvar, BDD newvar);


/**
 * \ingroup kernel
 * \brief Defines a whole set of pairs.
 *
 * Like ::bdd_setpairs but with \a newvar being an array of BDDs of
 * size \a size.
 * 
 * \return Zero on success, otherwise a negative error code.
 * \see bdd_newpair, bdd_setpair, bdd_replace, bdd_compose
 */
extern int      bdd_setbddpairs(bddPair *pair, int *olvar, BDD *newvar, int size);


/**
 * \ingroup kernel
 * \brief Clear all variable pairs.
 *
 * Resets the table of pairs \a p by setting all substitutions to their default values (that
 * is no change).
 * 
 * \see bdd_newpair, bdd_setpair, bdd_freepair
 */
extern void     bdd_resetpair(bddPair *p);


/**
 * \ingroup kernel
 * \brief Frees a table of pairs.
 *
 * Frees the table of pairs \a p that has been allocated by a call to ::bdd_newpair.
 * 
 * \see bdd_replace, bdd_newpair, bdd_setpair, bdd_resetpair
 */
extern void     bdd_freepair(bddPair *p);

  /* In bddop.c */

/**
 * \ingroup kernel
 * \brief Sets the cache ratio for the operator caches.
 *
 * The ratio between the number of nodes in the nodetable and the number of entries in the
 * operator cachetables is called the cache ratio. So a cache ratio of say, four, allocates one
 * cache entry for each four unique node entries. This value can be set with
 * ::bdd_setcacheratio to any positive value. When this is done the caches are resized
 * instantly to fit the new ratio. The default is a fixed cache size determined at
 * initialization time.
 * 
 * \return The previous cache ratio or a negative number on error.
 * \see bdd_init
 */
extern int      bdd_setcacheratio(int r);


/**
 * \ingroup operator
 * \brief Build a cube from an array of variables specified by a BDD array.
 *
 * This function builds a cube from the variables in \a var. It does so by interpreting the 
 * \a width low order bits of \a value as a bit mask - a set bit indicates that the variable should be
 * added in it's positive form, and a cleared bit the opposite. The most significant bits are
 * encoded with the first variables in \a variables. Consider as an example the call
 * \code
 * bdd_buildcube(0xB, 4, var);
 * \endcode
 * This corresponds to the expression: \f$var[0] \land \lnot var[1] \land var[2] \land var[3]\f$. 
 * This version of the function takes an array of BDDs as \a var.
 * 
 * \return The resulting cube.
 * \see bdd_ithvar, fdd_ithvar
 */
extern BDD      bdd_buildcube(int value, int width, BDD *var);


/**
 * \ingroup operator
 * \brief Build a cube from an array of variables.
 *
 * This function builds a cube from the variables in \a var. It does so by interpreting the 
 * \a width low order bits of \a value as a bit mask - a set bit indicates that the variable should be
 * added in it's positive form, and a cleared bit the opposite. The most significant bits are
 * encoded with the first variables in \a var. Consider as an example the call
 * \code
 * bdd_buildcube(0xB, 4, var);
 * \endcode
 * This corresponds to the expression: \f$var[0] \land \lnot var[1] \land var[2] \land var[3]\f$. 
 * This version of the function takes an array of variable numbers as used in ::bdd_ithvar.
 * 
 * \return The resulting cube.
 * \see bdd_ithvar, fdd_ithvar
 */
extern BDD      bdd_ibuildcube(int value, int width, int *var);


/**
 * \ingroup operator
 * \brief Negates a bdd.
 *
 * Negates the BDD \a r by exchanging all references to the zero-terminal with references to
 * the one-terminal and vice versa.
 * 
 * \return The negated bdd.
 */
extern BDD      bdd_not(BDD r);


/**
 * \ingroup operator
 * \brief Basic bdd operations.
 *
 * The ::bdd_apply function performs all of the basic bdd operations with two operands, such
 * as AND, OR etc. The \a l argument is the left bdd operand and \a r is the right operand.
 * The \a op argument is the requested operation and must be one of the following
 * <TABLE>
 * <TR> <TD>Identifier</TD>     <TD>Description</TD>                    <TD>Truth table</TD> <TD>C++ opr</TD> </TR>
   <TR> <TD>::bddop_and</TD>    <TD>logical and (\f$A \wedge B\f$)</TD> <TD>[0,0,0,1]</TD> <TD>\&</TD> </TR> 
   <TR> <TD>::bddop_xor</TD>    <TD>logical xor (\f$A \oplus B\f$)</TD> <TD>[0,1,1,0]</TD> <TD>\^</TD> </TR> 
   <TR> <TD>::bddop_or</TD>     <TD>logical or (\f$A \vee B\f$)</TD>    <TD>[0,1,1,1]</TD> <TD>|</TD> </TR> 
   <TR> <TD>::bddop_nand</TD>   <TD>logical not-and</TD>                <TD>[1,1,1,0] <TD></TD> </TR>
   <TR> <TD>::bddop_nor</TD>    <TD>logical not-or</TD>                 <TD>[1,0,0,0] <TD></TD> </TR> 
   <TR> <TD>::bddop_imp</TD>    <TD>implication (\f$A \Rightarrow B\f$)</TD> <TD>[1,1,0,1]</TD> <TD>\>\></TD> </TR> 
   <TR> <TD>::bddop_biimp</TD>  <TD>bi-implication (\f$A \Leftrightarrow B\f$)</TD> <TD>[1,0,0,1]</TD> <TD></TD> </TR>
   <TR> <TD>::bddop_diff</TD>   <TD>set difference (\f$A \setminus B\f$)</TD> <TD>[0,0,1,0]</TD> <TD>-</TD> </TR> 
   <TR> <TD>::bddop_less</TD>   <TD>less than (\f$A < B\f$)</TD>        <TD>[0,1,0,0]</TD> <TD>\<</TD> </TR> 
   <TR> <TD>::bddop_invimp</TD> <TD>reverse implication (\f$A \Leftarrow B\f$)</TD> <TD>[1,0,1,1]</TD> <TD>\<\<</TD> </TR> 
   </TABLE>
 * 
 * \return The result of the operation.
 * \see bdd_ite
 */
extern BDD      bdd_apply(BDD l, BDD r, int op);


/**
 * \ingroup operator
 * \brief The logical 'and' of two bdds.
 *
 * This a wrapper that calls 
 * \code bdd_apply(l,r,bddop_and) \endcode
 * 
 * \return The logical 'and' of \a l and \a r.
 * \see bdd_apply, bdd_or, bdd_xor
 */
extern BDD      bdd_and(BDD l, BDD r);


/**
 * \ingroup operator
 * \brief The logical 'or' of two bdds.
 *
 * This a wrapper that calls \code bdd_apply(l,r,bddop_or) \endcode
 * 
 * \return The logical 'or' of \a l and \a r.
 * \see bdd_apply, bdd_xor, bdd_and
 */
extern BDD      bdd_or(BDD l, BDD r);


/**
 * \ingroup operator
 * \brief The logical 'xor' of two bdds.
 *
 * This a wrapper that calls \code bdd_apply(l,r,bddop_xor) \endcode
 * 
 * \return The logical 'xor' of \a l and \a r.
 * \see bdd_apply, bdd_or, bdd_and
 */
extern BDD      bdd_xor(BDD l, BDD r);


/**
 * \ingroup operator
 * \brief The logical 'implication' between two bdds.
 *
 * This a wrapper that calls \code bdd_apply(l,r,bddop_imp) \endcode
 * 
 * \return The logical 'implication' of \a l and \a r (\f$l \Rightarrow r\f$).
 * \see bdd_apply, bdd_biimp
 */
extern BDD      bdd_imp(BDD l, BDD r);


/**
 * \ingroup operator
 * \brief The logical 'bi-implication' between two bdds.
 *
 * This a wrapper that calls \code bdd_apply(l,r,bddop_biimp) \endcode
 * 
 * \return The logical 'bi-implication' of \a l and \a r (\f$l \Leftrightarrow r\f$).
 * \see bdd_apply, bdd_imp
 */
extern BDD      bdd_biimp(BDD l, BDD r);


/**
 * \ingroup operator
 * \brief If-then-else operator.
 *
 * Calculates the BDD for the expression \f$(f \land g) \lor (\lnot f \land h)\f$ more efficiently
 * than doing the three operations separately. ::bdd_ite can also be used for conjunction,
 * disjunction and any other boolean operator, but is not as efficient for the binary and unary
 * operations.
 * 
 * \return The BDD for \f$(f \land g) \lor (\lnot f \land h)\f$.
 * \see bdd_apply
 */
extern BDD      bdd_ite(BDD f, BDD g, BDD h);


/**
 * \ingroup operator
 * \brief Restric a set of variables to constant values.
 *
 * This function restricts the variables in \a r to constant true or false. How this is done
 * depends on how the variables are included in the variable set \a var. If they are included in
 * their positive form then they are restricted to true and vice versa. Unfortunately it is not
 * possible to insert variables in their negated form using ::bdd_makeset, so the variable
 * set has to be build manually as a conjunction of the variables. Example: Assume variable 1
 * should be restricted to true and variable 3 to false. 
 * \code
 * bdd X = make_user_bdd(); 
 * bdd R1 = bdd_ithvar(1); 
 * bdd R2 = bdd_nithvar(3); 
 * bdd R = bdd_addref(bdd_apply(R1,R2, bddop_and) ); 
 * bdd RES = bdd_addref( bdd_restrict(X,R) );
 * \endcode
 * 
 * \return The restricted bdd.
 * \see bdd_makeset, bdd_exist, bdd_forall
 */
extern BDD      bdd_restrict(BDD r, BDD var);


/**
 * \ingroup operator
 * \brief Generalized cofactor.
 *
 * Computes the generalized cofactor of \a f with respect to \a c.
 * 
 * \return The constrained BDD.
 * \see bdd_restrict, bdd_simplify
 */
extern BDD      bdd_constrain(BDD f, BDD c);


/**
 * \ingroup operator
 * \brief Replaces variables with other variables.
 *
 * Replaces all variables in the BDD \a r with the variables defined by \a pair. Each entry in \a pair 
 * consists of an old and a new variable. Whenever the old variable is found in \a r then a new
 * node with the new variable is inserted instead.
 * 
 * \see bdd_newpair, bdd_setpair, bdd_setpairs
 * \return The result of the operation.
 */
extern BDD      bdd_replace(BDD r, bddPair *pair);


/**
 * \ingroup operator
 * \brief Functional composition.
 *
 * Substitutes the variable \a var with the BDD \a g in the BDD \a f: result \f$= f[g/var]\f$.
 * 
 * \return The composed BDD.
 * \see bdd_veccompose, bdd_replace, bdd_restrict
 */
extern BDD      bdd_compose(BDD f, BDD g, BDD v);


/**
 * \ingroup operator
 * \brief Simultaneous functional composition.
 *
 * Uses the pairs of variables and BDDs in \a pair to make the simultaneous substitution:
 * \f$[g_1/V_1, \ldots, g_n/V_n]\f$. 
 * In this way one or more BDDs may be substituted in one step.
 * The BDDs in \a pair may depend on the variables they are substituting. ::bdd_compose may be
 * used instead of ::bdd_replace but is not as efficient when \f$g_i\f$ is a single variable, the
 * same applies to ::bdd_restrict. Note that simultaneous substitution is not necessarily
 * the same as repeated substitution. Example: \f[(x_1 \lor x_2)[x_3/x_1,x_4/x_3] = (x_3
 * \lor x_2) \neq ((x_1 \lor x_2)[x_3/x_1])[x_4/x_3] = (x_4 \lor x_2)\f]
 * 
 * \return The composed BDD.
 * \see bdd_compose, bdd_replace, bdd_restrict
 */
extern BDD      bdd_veccompose(BDD g, bddPair *pair);


/**
 * \ingroup operator
 * \brief Coudert and madre's restrict function.
 *
 * Tries to simplify the BDD \a f by restricting it to the domain covered by \a d. No checks are
 * done to see if the result is actually smaller than the input. This can be done by the user with a
 * call to ::bdd_nodecount.
 * 
 * \see bdd_restrict
 * \return The simplified BDD.
 */
extern BDD      bdd_simplify(BDD f, BDD d);


/**
 * \ingroup operator
 * \brief Existential quantification of variables.
 *
 * Removes all occurences in \a r of variables in the set \a var by existential quantification.
 * 
 * \see bdd_forall, bdd_unique, bdd_makeset
 * \return The quantified BDD.
 */
extern BDD      bdd_exist(BDD r, BDD var);


/**
 * \ingroup operator
 * \brief Universal quantification of variables.
 *
 * Removes all occurences in \a r of variables in the set \a var by universal quantification.
 * 
 * \see bdd_exist, bdd_unique, bdd_makeset
 * \return The quantified BDD.
 */
extern BDD      bdd_forall(BDD r, BDD var);


/**
 * \ingroup operator
 * \brief Unique quantification of variables.
 *
 * Removes all occurences in \a r of variables in the set \a var by unique quantification. This
 * type of quantification uses a XOR operator instead of an OR operator as in the existential
 * quantification, and an AND operator as in the universal quantification.
 * 
 * \see bdd_exist, bdd_forall, bdd_makeset
 * \return The quantified BDD.
 */
extern BDD      bdd_unique(BDD, BDD);


/**
 * \ingroup operator
 * \brief Apply operation and existential quantification.
 *
 * Applies the binary operator \a opr to the arguments \a l and \a r and then performs an
 * existential quantification of the variables from the variable set \a var. This is done in a
 * bottom up manner such that both the apply and quantification is done on the lower nodes
 * before stepping up to the higher nodes. This makes the ::bdd_appex function much more
 * efficient than an apply operation followed by a quantification. If the operator is a
 * conjunction then this is similar to the relational product of the two BDDs.
 * 
 * \see bdd_appall, bdd_appuni, bdd_apply, bdd_exist, bdd_forall, bdd_unique, bdd_makeset
 * \return The result of the operation.
 */
extern BDD      bdd_appex(BDD l, BDD r, int opr, BDD var);


/**
 * \ingroup operator
 * \brief Apply operation and universal quantification.
 *
 * Applies the binary operator \a opr to the arguments \a l and \a r and then performs an
 * universal quantification of the variables from the variable set \a var. This is done in a
 * bottom up manner such that both the apply and quantification is done on the lower nodes
 * before stepping up to the higher nodes. This makes the ::bdd_appall function much more
 * efficient than an apply operation followed by a quantification.
 * 
 * \see bdd_appex, bdd_appuni, bdd_apply, bdd_exist, bdd_forall, bdd_unique, bdd_makeset
 * \return The result of the operation.
 */
extern BDD      bdd_appall(BDD l, BDD r, int opr, BDD var);


/**
 * \ingroup operator
 * \brief Apply operation and unique quantification.
 *
 * Applies the binary operator \a opr to the arguments \a l and \a r and then performs a
 * unique quantification of the variables from the variable set \a var. This is done in a bottom
 * up manner such that both the apply and quantification is done on the lower nodes before
 * stepping up to the higher nodes. This makes the ::bdd_appuni function much more efficient
 * than an apply operation followed by a quantification.
 * 
 * \see bdd_appex, bdd_appall, bdd_apply, bdd_exist, bdd_unique, bdd_forall, bdd_makeset
 * \return The result of the operation.
 */
extern BDD      bdd_appuni(BDD l, BDD r, int opr, BDD var);


/**
 * \ingroup info
 * \brief Returns the variable support of a bdd.
 *
 * Finds all the variables that \a r depends on. That is the support of \a r.
 * 
 * \see bdd_makeset
 * \return A BDD variable set.
 */
extern BDD      bdd_support(BDD r);


/**
 * \ingroup operator
 * \brief Finds one satisfying variable assignment.
 *
 * Finds a BDD with at most one variable at each level. This BDD implies \a r and is not false
 * unless \a r is false.
 * 
 * \see bdd_allsat bdd_satoneset, bdd_fullsatone, bdd_satcount, bdd_satcountln
 * \return The result of the operation.
 */
extern BDD      bdd_satone(BDD r);


/**
 * \ingroup operator
 * \brief Finds one satisfying variable assignment.
 *
 * Finds a minterm in \a r. The \a var argument is a variable set that defines a set of variables
 * that \em must be mentioned in the result. The polarity of these variables in result --- in
 * case they are undefined in \a r --- are defined by the \a pol parameter. If \a pol is the false
 * BDD then the variables will be in negative form, and otherwise they will be in positive form.
 * 
 * \see bdd_allsat bdd_satone, bdd_fullsatone, bdd_satcount, bdd_satcountln
 * \return The result of the operation.
 */
extern BDD      bdd_satoneset(BDD r, BDD var, BDD pol);


/**
 * \ingroup operator
 * \brief Finds one satisfying variable assignment.
 *
 * Finds a BDD with exactly one variable at all levels. This BDD implies \a r and is not false
 * unless \a r is false.
 * 
 * \see bdd_allsat bdd_satone, bdd_satoneset, bdd_satcount, bdd_satcountln
 * \return The result of the operation.
 */
extern BDD      bdd_fullsatone(BDD r);


/**
 * \ingroup operator
 * \brief Finds all satisfying variable assignments.
 *
 * Iterates through all legal variable assignments (those that make the BDD come true) for the
 * bdd \a r and calls the callback handler \a handler for each of them. The array passed to \a
 * handler contains one entry for each of the globally defined variables. Each entry is either
 * 0 if the variable is false, 1 if it is true, and -1 if it is a don't care. The following is an
 * example of a callback handler that prints 'X' for don't cares, '0' for zero, and '1' for one:
 * \code
 * void allsatPrintHandler(char* varset, int size) 
 * { 
 *   for (int v=0; v<size; ++v) 
 *   { 
 *      cout << (varset[v] < 0 ? 'X' : (char)('0' + varset[v])); 
 *   } 
 *   cout << endl; 
 * }
 * \endcode
 * The handler can be used like this:
 * \code
 * bdd_allsat(r, * allsatPrintHandler);
 * \endcode
 * 
 * \see bdd_satone bdd_satoneset, bdd_fullsatone, bdd_satcount, bdd_satcountln
 */
extern void     bdd_allsat(BDD r, bddallsathandler handler);


/**
 * \ingroup info
 * \brief Calculates the number of satisfying variable assignments.
 *
 * Calculates how many possible variable assignments there exists such that \a r is satisfied
 * (true). 
 * 
 * \see bdd_satone, bdd_fullsatone, bdd_satcountln, bdd_stacountset
 * \return The number of possible assignments.
 */
extern double   bdd_satcount(BDD r);


/**
 * \ingroup info
 * \brief Calculates the number of satisfying variable assignments for a given set of variables.
 *
 * Calculates how many possible variable assignments there exists such that \a r is satisfied
 * (true). Only the variables in the variable set \a varset are considered. This makes the function a
 * \em lot slower than ::bdd_satcount.
 * 
 * \see bdd_satone, bdd_fullsatone, bdd_satcount, bdd_satcountln
 * \return The number of possible assignments.
 */
extern double   bdd_satcountset(BDD, BDD);


/**
 * \ingroup info
 * \brief Calculates the logarithm of the number of satisfying variable assignments.
 *
 * Calculates how many possible variable assignments there exists such that \a r is satisfied
 * (true) and returns the logarithm of this. The result is calculated in such a manner that it is
 * practically impossible to get an overflow, which is very possible for ::bdd_satcount if
 * the number of defined variables is too large. 
 * 
 * \see bdd_satone, bdd_fullsatone, bdd_satcount, bdd_satcountlnset
 * \return The logarithm of the number of possible assignments.
 */
extern double   bdd_satcountln(BDD r);


/**
 * \ingroup info
 * \brief Calculates the logarithm of the number of satisfying variable assignments for a given set of variables.
 *
 * Calculates how many possible variable assignments there exists such that \a r is satisfied
 * (true) and returns the logarithm of this. The result is calculated in such a manner that it is
 * practically impossible to get an overflow, which is very possible for ::bdd_satcount if
 * the number of defined variables is too large. Only the variables in the 
 * variable set \a varset are considered. This makes the function a \em lot slower! than ::bdd_satcountln.
 * 
 * \see bdd_satone, bdd_fullsatone, bdd_satcount, bdd_satcountln
 * \return The logarithm of the number of possible assignments.
 */
extern double   bdd_satcountlnset(BDD r, BDD varset);


/**
 * \ingroup info
 * \brief Counts the number of nodes used for a bdd.
 *
 * Traverses the BDD and counts all distinct nodes that are used for the BDD.
 * 
 * \return The number of nodes.
 * \see bdd_pathcount, bdd_satcount, bdd_anodecount
 */
extern int      bdd_nodecount(BDD r);


/**
 * \ingroup info
 * \brief Counts the number of shared nodes in an array of bdds.
 *
 * Traverses all of the BDDs in \a r and counts all distinct nodes that are used in the BDDs -- if a
 * node is used in more than one BDD then it only counts once. The \a num parameter holds the size
 * of the array.
 * 
 * \return The number of nodes.
 * \see bdd_nodecount
 */
extern int      bdd_anodecount(BDD *r, int num);


/**
 * \ingroup info
 * \brief Returns a variable profile.
 *
 * Counts the number of times each variable occurs in the bdd \a r. The result is stored and
 * returned in an integer array where the i'th position stores the number of times the i'th
 * variable occured in the BDD. It is the users responsibility to free the array again using a
 * call to \c free.
 * 
 * \return A pointer to an integer array with the profile or \c NULL if an error occured.
 */
extern int*     bdd_varprofile(BDD r);


/**
 * \ingroup info
 * \brief Count the number of paths leading to the true terminal.
 *
 * Counts the number of paths from the root node \a r leading to the terminal true node.
 * 
 * \return The number of paths.
 * \see bdd_nodecount, bdd_satcount
 */
extern double   bdd_pathcount(BDD r);

   
/* In file "bddio.c" */

/**
 * \ingroup fileio
 * \brief Prints all used entries in the node table to \c stdout.
 *
 * Prints to \c stdout all the used entries in the main node table. The
 * format is: 
 * \verbatim [Nodenum] Var/Level Low High \endverbatim
 * Where \c Nodenum is the position in the node table and \c Level is the position in the current 
 * variable order.
 * 
 * \see bdd_fprintall, bdd_printtable, bdd_printset, bdd_printdot
 */
extern void     bdd_printall(void);


/**
 * \ingroup fileio
 * \brief Prints all used entries in the node table to a file.
 *
 * Prints to the file \a ofile all the used entries in the main node table. The
 * format is: 
 * \verbatim [Nodenum] Var/Level Low High \endverbatim
 * Where \c Nodenum is the position in the node table and \c Level is the position in the current 
 * variable order.
 * 
 * \see bdd_printall, bdd_printtable, bdd_printset, bdd_printdot
 */
extern void     bdd_fprintall(FILE *ofile);


/**
 * \ingroup fileio
 * \brief Prints the node table entries used by a bdd to a file.
 *
 * Prints to the file \a ofile all the entries in the main node table used by \a r.
 * The format is: 
 * \verbatim [Nodenum] Var/Level : Low High \endverbatim
 * Where \c Nodenum is the position in the node table and Level is the position
 * in the current variable order.
 * 
 * \see bdd_printtable, bdd_printall, bdd_printset, bdd_printdot
 */
extern void     bdd_fprinttable(FILE *ofile, BDD r);


/**
 * \ingroup fileio
 * \brief Prints the node table entries used by a bdd to \c stdout.
 *
 * Prints to \c stdout all the entries in the main node table used by \a r.
 * The format is: 
 * \verbatim [Nodenum] Var/Level : Low High \endverbatim
 * Where \c Nodenum is the position in the node table and Level is the position
 * in the current variable order.
 * 
 * \see bdd_printall, bdd_printset, bdd_printdot
 */
extern void     bdd_printtable(BDD r);


/**
 * \ingroup fileio
 * \brief Prints the set of truth assignments specified by a bdd to a file.
 *
 * Prints all the truth assignments for \a r that would yield it true to \a ofile. The format is:
 * \f{eqnarray*}
 * \langle x_{1,1}:c_{1,1},\ldots,x_{1,n_1}:c_{1,n_1} \rangle \\
 * \langle x_{2,1}:c_{2,1},\ldots,x_{2,n_2}:c_{2,n_2} \rangle \\ 
 * \vdots \\
 * \langle x_{N,1}:c_{N,1},\ldots,x_{N,n_3}:c_{N,n_3} \rangle
 * \f}
 * Where the \f$x\f$'s are variable numbers (and the position in the current order) 
 * and the \f$c\f$'s are the possible assignments to these. Each set of brackets designates one 
 * possible assignment to the set of variables that make up the BDD. All variables not shown 
 * are don't cares. It is possible to specify a callback handler for printing of the variables 
 * using ::bdd_file_hook or ::bdd_strm_hook.
 * 
 * \see bdd_printset, bdd_printall, bdd_printtable, bdd_printdot, bdd_file_hook, bdd_strm_hook
 */
extern void     bdd_fprintset(FILE *ofile, BDD r);


/**
 * \ingroup fileio
 * \brief Prints the set of truth assignments specified by a bdd to \c stdout.
 *
 * Prints all the truth assignments for \a r that would yield it true to \c stdout. The format is:
 * \f{eqnarray*}
 * \langle x_{1,1}:c_{1,1},\ldots,x_{1,n_1}:c_{1,n_1} \rangle \\
 * \langle x_{2,1}:c_{2,1},\ldots,x_{2,n_2}:c_{2,n_2} \rangle \\ 
 * \vdots \\
 * \langle x_{N,1}:c_{N,1},\ldots,x_{N,n_3}:c_{N,n_3} \rangle
 * \f}
 * Where the \f$x\f$'s are variable numbers (and the position in the current order) 
 * and the \f$c\f$'s are the possible assignments to these. Each set of brackets designates one 
 * possible assignment to the set of variables that make up the BDD. All variables not shown 
 * are don't cares. It is possible to specify a callback handler for printing of the variables 
 * using ::bdd_file_hook or ::bdd_strm_hook.
 * 
 * \see bdd_fprintset, bdd_printall, bdd_printtable, bdd_printdot, bdd_file_hook, bdd_strm_hook
 */
extern void     bdd_printset(BDD r);


/**
 * \ingroup fileio
 * \brief Prints a description of a bdd in dot format to a file specified by filename.
 *
 * Prints a BDD in a format suitable for use with the graph drawing program DOT to 
 * the file named by \a fname. The file will be
 * opened for writing, any previous contents destroyed and then closed again.
 * 
 * \see bdd_printdot, bdd_fprintdot, bdd_printall, bdd_printtable, bdd_printset
 */
extern int      bdd_fnprintdot(char *fname, BDD r);


/**
 * \ingroup fileio
 * \brief Prints a description of a bdd in dot format to a file.
 *
 * Prints a BDD in a format suitable for use with the graph drawing program DOT to 
 * a designated file \a ofile.
 * 
 * \see bdd_printdot, bdd_fnprintdot, bdd_printall, bdd_printtable, bdd_printset
 */
extern void     bdd_fprintdot(FILE *ofile, BDD r);


/**
 * \ingroup fileio
 * \brief Prints a description of a bdd in dot format to \c stdout.
 *
 * Prints a BDD in a format suitable for use with the graph drawing program DOT to \c stdout.
 * 
 * \see bdd_fprintdot, bdd_fnprintdot, bdd_printall, bdd_printtable, bdd_printset
 */
extern void     bdd_printdot(BDD r);


/**
 * \ingroup fileio
 * \brief Saves a bdd to a file specified by filename.
 *
 * Saves the nodes used by \a r to the
 * file named \a fname. In the last case the file will be truncated and opened for writing.
 * 
 * \see bdd_save, bdd_load
 * \return Zero on success, otherwise an error code from ::bdd.h.
 */
extern int      bdd_fnsave(char *fname, BDD r);


/**
 * \ingroup fileio
 * \brief Saves a bdd to a file.
 *
 * Saves the nodes used by \a r to the file \a ofile which must be opened for writing.
 * 
 * \see bdd_fnsave, bdd_load
 * \return Zero on success, otherwise an error code from ::bdd.h.
 */
extern int      bdd_save(FILE *ofile, BDD r);


/**
 * \ingroup fileio
 * \brief Loads a bdd from a file specified by filename.
 *
 * Loads a BDD from a file into the BDD pointed to by \a r. The file is specified by the filename
 * \a fname which will be opened
 * automatically for reading. The input file format consists of integers arranged in the
 * following manner. First the number of nodes \f$N\f$ used by the BDD and then the number of
 * variables \f$V\f$ allocated and the variable ordering in use at the time the BDD was saved. If \f$N\f$
 * and \f$V\f$ are both zero then the BDD is either the constant true or false BDD, indicated by a 1
 * or a 0 as the next integer. In any other case the next \f$N\f$ sets of 4 integers will describe
 * the nodes used by the BDD. Each set consists of first the node number, then the variable
 * number and then the low and high nodes. The nodes \a must be saved in a order such that any
 * low or high node must be defined before it is mentioned.
 * 
 * \see bdd_load, bdd_save
 * \return Zero on success, otherwise an error code from ::bdd.h.
 */
extern int      bdd_fnload(char *fname, BDD *r);


/**
 * \ingroup fileio
 * \brief Loads a bdd from a file.
 *
 * Loads a BDD from a file into the BDD pointed to by \a r. The file \a ifile
 * which must be opened for reading. The input file format consists of integers arranged in the
 * following manner. First the number of nodes \f$N\f$ used by the BDD and then the number of
 * variables \f$V\f$ allocated and the variable ordering in use at the time the BDD was saved. If \f$N\f$
 * and \f$V\f$ are both zero then the BDD is either the constant true or false BDD, indicated by a 1
 * or a 0 as the next integer. In any other case the next \f$N\f$ sets of 4 integers will describe
 * the nodes used by the BDD. Each set consists of first the node number, then the variable
 * number and then the low and high nodes. The nodes \a must be saved in a order such that any
 * low or high node must be defined before it is mentioned.
 * 
 * \see bdd_fnload, bdd_save
 * \return Zero on success, otherwise an error code from ::bdd.h.
 */
extern int      bdd_load(FILE *ifile, BDD *r);

/* In file reorder.c */

/**
 * \ingroup reorder
 * \brief Swap two bdd variables.
 *
 * Use ::bdd_swapvar to swap the position (in the current variable order) of the two BDD
 * variables \a v1 and \a v2. There are no constraints on the position of the two variables
 * before the call. This function may \em not be used together with user defined variable
 * blocks. The swap is done by a series of adjacent variable swaps and requires the whole node
 * table to be rehashed twice for each call to ::bdd_swapvar. It should therefore not be used
 * were efficiency is a major concern.
 * 
 * \return Zero on success and a negative error code otherwise.
 * \see bdd_reorder, bdd_addvarblock
 */
extern int      bdd_swapvar(int v1, int v2);


/**
 * \ingroup reorder
 * \brief Default reorder handler.
 * 
 * The default reorder handler prints information about the 
 * reorder process to \c stdout if \c bdd_reorder_verbose(1) has been
 * called previously.
 * 
 * The information printed includes a message before the reorder runs and 
 * a message after it has run, indicating how many nodes were in use before and after
 * the reorder as well as how long (in seconds) it took to run the reorder.
 * 
 * \see bdd_reorder_hook
 */
extern void     bdd_default_reohandler(int prestate);


/**
 * \ingroup reorder
 * \brief Start dynamic reordering.
 *
 * This function initiates dynamic reordering using the heuristic defined by \a method,
 * which may be one of the following:
 * - \a BDD_REORDER_WIN2 \n
 *   Reordering using a sliding window of size 2. This algorithm swaps two adjacent variable
 *   blocks and if this results in more nodes then the two blocks are swapped back again.
 *   Otherwise the result is kept in the variable order. This is then repeated for all variable
 *   blocks. 
 * - \a BDD_REORDER_WIN2ITE \n
 *   The same as above but the process is repeated until
 *   no further progress is done. Usually a fast and efficient method. 
 * - \a BDD_REORDER_WIN3 \n
 *   The same as above but with a window size of 3. 
 * - \a BDD_REORDER_WIN2ITE \n
 *   The same as above but with a window size of 3. 
 * - \a BDD_REORDER_SIFT \n 
 *   Reordering where each block is moved through all possible positions.
 *   The best of these is then used as the new position. Potentially a very slow but good method.
 * - \a BDD_REORDER_SIFTITE \n
 *   The same as above but the process is repeated until no
 *   further progress is done. Can be extremely slow.
 * - \a BDD_REORDER_RANDOM \n
 *   Mostly used for debugging purpose, but may be usefull for others. Selects a random position for
 *   each variable.
 * 
 * \see bdd_autoreorder, bdd_reorder_verbose, bdd_addvarblock, bdd_clrvarblocks
 */
extern void     bdd_reorder(int method);


/**
 * \ingroup reorder
 * \brief Calculate the gain in size after a reordering.
 *
 * Returns the gain in percent of the previous number of used nodes. The value returned is 
 * \f[ (100 * (A - B)) / A \f] 
 * Where \f$A\f$ is previous number of used nodes and \f$B\f$ is current number of used
 * nodes.
 * 
 */
extern int      bdd_reorder_gain(void);


/**
 * \ingroup reorder
 * \brief Define a handler for minimization of bdds.
 *
 * Reordering is typically done to minimize the global number of BDD nodes in use, but it may in
 * some cases be usefull to minimize with respect to a specific BDD. With ::bdd_reorder_probe
 * it is possible to define a callback function that calculates the size of a specific BDD (or
 * anything else in fact). This handler will then be called by the reordering functions to get
 * the current size information. A typical handle could look like this: 
 * \code 
 * int * sizehandler(void) 
 * { 
 *   extern BDD mybdd; 
 *   return bdd_nodecount(mybdd); 
 * } 
 * \endcode 
 * No default handler is supplied. The argument \a handler may be \c NULL if no handler is needed.
 * 
 * \return The old handler.
 * \see bdd_reorder
 */
extern bddsizehandler bdd_reorder_probe(bddsizehandler handler);


/**
 * \ingroup reorder
 * \brief Clears all variable blocks.
 *
 * Clears all the variable blocks that has been defined by calls to ::bdd_addvarblock.
 * 
 * \see bdd_addvarblock
 */
extern void     bdd_clrvarblocks(void);


/**
 * \ingroup reorder
 * \brief Adds a new variable block for reordering.
 *
 * Creates a new variable block with the variables in the variable set \a b. The variables in
 * \a b must be contiguous. This order does
 * not depend on current variable order. The variable blocks are ordered as a tree, with the
 * largest ranges at top and the smallest at the bottom. 
 * 
 * Example: Assume the block 0-9 is added
 * as the first block and then the block 0-6. This yields the 0-9 block at the top, with the 0-6
 * block as a child. If now the block 2-4 was added, it would become a child of the 0-6 block. A
 * block of 0-8 would be a child of the 0-9 block and have the 0-6 block as a child. 
 * 
 * Partially
 * overlapping blocks are not allowed. The \a fixed parameter sets the block to be fixed (no
 * reordering of its child blocks is allowed) or free, using the constants 
 * \c BDD_REORDER_FIXED and \c BDD_REORDER_FREE. Reordering is always done on the top most
 * blocks first and then recursively downwards. The return value is an integer that can be used
 * to identify the block later on - with for example ::bdd_blockfile_hook. The values
 * returned will be in the sequence \f$0,1,2,3,\ldots\f$.
 * 
 * \return A non-negative identifier on success, otherwise a negative error code.
 * \see bdd_varblockall, fdd_intaddvarblock, bdd_clrvarblocks
 */
extern int      bdd_addvarblock(BDD b, int fixed);


/**
 * \ingroup reorder
 * \brief Adds a new variable block for reordering.
 *
 * Creates a new variable block, where the argument \a first is the first variable
 * included in the block and \a last is the last variable included in the block. This order does
 * not depend on current variable order. The variable blocks are ordered as a tree, with the
 * largest ranges at top and the smallest at the bottom. 
 * 
 * Example: Assume the block 0-9 is added
 * as the first block and then the block 0-6. This yields the 0-9 block at the top, with the 0-6
 * block as a child. If now the block 2-4 was added, it would become a child of the 0-6 block. A
 * block of 0-8 would be a child of the 0-9 block and have the 0-6 block as a child. 
 * 
 * Partially
 * overlapping blocks are not allowed. The \a fixed parameter sets the block to be fixed (no
 * reordering of its child blocks is allowed) or free, using the constants \a
 * BDD_REORDER_FIXED and \a BDD_REORDER_FREE. Reordering is always done on the top most
 * blocks first and then recursively downwards. The return value is an integer that can be used
 * to identify the block later on - with for example ::bdd_blockfile_hook. The values
 * returned will be in the sequence \f$0,1,2,3,\ldots\f$.
 * 
 * \return A non-negative identifier on success, otherwise a negative error code.
 * \see bdd_varblockall, fdd_intaddvarblock, bdd_clrvarblocks
 */
extern int      bdd_intaddvarblock(int first, int last, int fixed);


/**
 * \ingroup reorder
 * \brief Add a variable block for all variables.
 *
 * Adds a variable block for all BDD variables declared so far. Each block contains one
 * variable only. More variable blocks can be added later with the use of ::bdd_addvarblock --
 * in this case the tree of variable blocks will have the blocks of single variables as the
 * leafs.
 * 
 * \see bdd_addvarblock, bdd_intaddvarblock
 */
extern void     bdd_varblockall(void);


/**
 * \ingroup reorder
 * \brief Specifies a printing callback handler.
 *
 * A printing callback handler is used to convert the variable block identifiers into
 * something readable by the end user. Use ::bdd_blockfile_hook to pass a handler to BuDDy. A
 * typical handler could look like this: 
 * \code 
 * void printhandler(FILE *o, int * block)
 * { 
 *   extern char **blocknames; 
 *   fprintf(o, "%s", blocknames[block]); 
 * }
 * \endcode
 * The handler is then called from ::bdd_printorder and
 * ::bdd_reorder (depending on the verbose level) with the block numbers returned by
 * ::bdd_addvarblock as arguments. No default handler is supplied. The argument \a handler
 * may be \c NULL if no handler is needed.
 * 
 * \return The old handler.
 * \see bdd_printorder
 */
extern bddfilehandler bdd_blockfile_hook(bddfilehandler handler);


/**
 * \ingroup reorder
 * \brief Enables automatic reordering.
 *
 * Enables automatic reordering using \a method as the reordering method. If \a method is 
 * \c BDD_REORDER_NONE then automatic reordering is disabled. Automatic reordering is done
 * every time the number of active nodes in the node table has been doubled and works by
 * interrupting the current BDD operation, doing the reordering and the retrying the
 * operation. Values for \a method can be found under ::bdd_reorder.
 * 
 * \return Returns the old value of \a method.
 * \see bdd_reorder
 */
extern int      bdd_autoreorder(int method);


/**
 * \ingroup reorder
 * \brief Enables automatic reordering.
 *
 * Enables automatic reordering using \a method as the reordering method. If \a method is 
 * \c BDD_REORDER_NONE then automatic reordering is disabled. Automatic reordering is done
 * every time the number of active nodes in the node table has been doubled and works by
 * interrupting the current BDD operation, doing the reordering and the retrying the
 * operation. In this form the argument \a num specifies the allowed number of
 * reorderings. So if for example a "one shot" reordering is needed, then the \a num argument
 * would be set to one. Values for \a method can be found under ::bdd_reorder.
 * 
 * \return Returns the old value of \a method.
 * \see bdd_reorder
 */
extern int      bdd_autoreorder_times(int method, int num);


/**
 * \ingroup reorder
 * \brief Fetch the level of a specific bdd variable.
 *
 * Returns the position of the variable \a var in the current variable order.
 * 
 * \see bdd_reorder, bdd_level2var
 */
extern int      bdd_var2level(int var);


/**
 * \ingroup reorder
 * \brief Fetch the variable number of a specific level.
 *
 * Returns the variable placed at position \a level in the current variable order.
 * 
 * \see bdd_reorder, bdd_var2level
 */
extern int      bdd_level2var(int level);


/**
 * \ingroup reorder
 * \brief Fetch the current number of allowed reorderings.
 *
 * Returns the current number of allowed reorderings left. This value can be defined by
 * ::bdd_autoreorder_times.
 * 
 * \see bdd_reorder_times, bdd_getreorder_method
 */
extern int      bdd_getreorder_times(void);


/**
 * \ingroup reorder
 * \brief Fetch the current reorder method.
 *
 * Returns the current reorder method as defined by ::bdd_autoreorder.
 * 
 * \see bdd_reorder, bdd_getreorder_times
 */
extern int      bdd_getreorder_method(void);


/**
 * \ingroup reorder
 * \brief Enables automatic reordering.
 *
 * Re-enables reordering after a call to ::bdd_disable_reorder.
 * 
 * \see bdd_disable_reorder
 */
extern void     bdd_enable_reorder(void);


/**
 * \ingroup reorder
 * \brief Disable automatic reordering.
 *
 * Disables automatic reordering until ::bdd_enable_reorder is called. Reordering is
 * enabled by default as soon as any variable blocks have been defined.
 * 
 * \see bdd_enable_reorder
 */
extern void     bdd_disable_reorder(void);


/**
 * \ingroup reorder
 * \brief Enables verbose information about reorderings.
 *
 * With ::bdd_reorder_verbose it is possible to set the level of information which should be
 * printed during reordering. A value of zero means no information, a value of one means some
 * information and any greater value will result in a lot of reordering information. The
 * default value is zero.
 * 
 * \return The old verbose level.
 * \see bdd_reorder
 */
extern int      bdd_reorder_verbose(int value);


/**
 * \ingroup reorder
 * \brief Set a specific variable order.
 *
 * This function sets the current variable order to be the one defined by \a neworder. The
 * parameter \a neworder is interpreted as a sequence of variable indices and the new
 * variable order is exactly this sequence. The array \em must contain all the variables
 * defined so far. If for instance the current number of variables is 3 and \a neworder contains
 * \f$[1,0,2]\f$ then the new variable order is \f$v_1 < v_0 < v_2\f$.
 * 
 * \see bdd_reorder, bdd_printorder
 */
extern void     bdd_setvarorder(int *neworder);


/**
 * \ingroup reorder
 * \brief Prints the current order to \c stdout.
 *
 * Prints an indented list of the variable blocks, showing the top most blocks to the left and
 * the lower blocks to the right. Example:
 * \verbatim
 * 2{ 0 1 2} 3 4 
 * \endverbatim
 * This shows 5 variable blocks. The first one added is block zero, which is on the same level as block
 * one. These two blocks are then sub-blocks of block two and block two is on the same level as
 * block three and four. The numbers are the identifiers returned from ::bdd_addvarblock.
 * The block levels depends on the variables included in the blocks.
 * 
 * \see bdd_fprintorder, bdd_reorder, bdd_addvarblock
 */
extern void     bdd_printorder(void);


/**
 * \ingroup reorder
 * \brief Prints the current order to a file.
 *
 * Prints an indented list of the variable blocks to \a ofile, showing the top most blocks to the left and
 * the lower blocks to the right. Example:
 * \verbatim
 * 2{ 0 1 2} 3 4 
 * \endverbatim
 * This shows 5 variable blocks. The first one added is block zero, which is on the same level as block
 * one. These two blocks are then sub-blocks of block two and block two is on the same level as
 * block three and four. The numbers are the identifiers returned from ::bdd_addvarblock.
 * The block levels depends on the variables included in the blocks.
 * 
 * \see bdd_printorder, bdd_reorder, bdd_addvarblock
 */
extern void     bdd_fprintorder(FILE *ofile);

#ifdef CPLUSPLUS
}
#endif


/*=== BDD constants ====================================================*/

#ifndef CPLUSPLUS


/**
 * \ingroup kernel
 * \brief The constant false bdd.
 *
 * This bdd holds the constant false value.
 * 
 * \see bddtrue, bdd_true, bdd_false
 */
extern const BDD bddfalse;


/**
 * \ingroup kernel
 * \brief The constant true bdd.
 *
 * This bdd holds the constant true value.
 * 
 * \see bddfalse, bdd_true, bdd_false
 */
extern const BDD bddtrue;

#endif /* CPLUSPLUS */


/*=== Reordering algorithms ============================================*/

#define BDD_REORDER_NONE     0
#define BDD_REORDER_WIN2     1
#define BDD_REORDER_WIN2ITE  2
#define BDD_REORDER_SIFT     3
#define BDD_REORDER_SIFTITE  4
#define BDD_REORDER_WIN3     5
#define BDD_REORDER_WIN3ITE  6
#define BDD_REORDER_RANDOM   7

#define BDD_REORDER_FREE     0
#define BDD_REORDER_FIXED    1


/*=== Error codes ======================================================*/

#define BDD_MEMORY (-1)   /**< Out of memory */
#define BDD_VAR (-2)      /**< Unknown variable */
#define BDD_RANGE (-3)    /**< Variable value out of range (not in domain) */
#define BDD_DEREF (-4)    /**< Removing external reference to unknown node */
#define BDD_RUNNING (-5)  /**< Called bdd_init() twice whithout bdd_done() */
#define BDD_FILE (-6)     /**< Some file operation failed */
#define BDD_FORMAT (-7)   /**< Incorrect file format */
#define BDD_ORDER (-8)    /**< Vars. not in order for vector based functions */
#define BDD_BREAK (-9)    /**< User called break */
#define BDD_VARNUM (-10)  /**< Different number of vars. for vector pair */
#define BDD_NODES (-11)   /**< Tried to set max. number of nodes to be fewer */
                          /**< than there already has been allocated */
#define BDD_OP (-12)      /**< Unknown operator */
#define BDD_VARSET (-13)  /**< Illegal variable set */
#define BDD_VARBLK (-14)  /**< Bad variable block operation */
#define BDD_DECVNUM (-15) /**< Trying to decrease the number of variables */
#define BDD_REPLACE (-16) /**< Replacing to already existing variables */
#define BDD_NODENUM (-17) /**< Number of nodes reached user defined maximum */
#define BDD_ILLBDD (-18)  /**< Illegal bdd argument */
#define BDD_SIZE (-19)    /**< Illegal size argument */

#define BVEC_SIZE (-20)    /**< Mismatch in bitvector size */
#define BVEC_SHIFT (-21)   /**< Illegal shift-left/right parameter */
#define BVEC_DIVZERO (-22) /**< Division by zero */

#define BDD_ERRNUM 24

/*************************************************************************
   If this file is included from a C++ compiler then the following
   classes, wrappers and hacks are supplied.
*************************************************************************/
#ifdef CPLUSPLUS
#include <iostream>

/*=== User BDD class ===================================================*/

class bvec;

class bdd
{
 public:

   bdd(void)         { root=0; }
   bdd(const bdd &r) { bdd_addref(root=r.root); }
   ~bdd(void)        { bdd_delref(root); }

   int id(void) const;
   
   bdd operator=(const bdd &r);
   
   bdd operator&(const bdd &r) const;
   bdd operator&=(const bdd &r);
   bdd operator^(const bdd &r) const;
   bdd operator^=(const bdd &r);
   bdd operator|(const bdd &r) const;
   bdd operator|=(const bdd &r);
   bdd operator!(void) const;
   bdd operator>>(const bdd &r) const;
   bdd operator>>=(const bdd &r);
   bdd operator-(const bdd &r) const;
   bdd operator-=(const bdd &r);
   bdd operator>(const bdd &r) const;
   bdd operator<(const bdd &r) const;
   bdd operator<<(const bdd &r) const;
   bdd operator<<=(const bdd &r);
   int operator==(const bdd &r) const;
   int operator!=(const bdd &r) const;
   
private:
   BDD root;

   bdd(BDD r) { bdd_addref(root=r); }
   bdd operator=(BDD r);

   friend int      bdd_init(int, int);
   friend int      bdd_setvarnum(int);
   friend bdd      bdd_true(void);
   friend bdd      bdd_false(void);
   friend bdd      bdd_ithvarpp(int);
   friend bdd      bdd_nithvarpp(int);
   friend int      bdd_var(const bdd &);
   friend bdd      bdd_low(const bdd &);
   friend bdd      bdd_high(const bdd &);
   friend int      bdd_scanset(const bdd &, int *&, int &);
   friend bdd      bdd_makesetpp(int *, int);
   friend int      bdd_setbddpair(bddPair*, int, const bdd &);
   friend int      bdd_setbddpairs(bddPair*, int*, const bdd *, int);
   friend bdd      bdd_buildcube(int, int, const bdd *);
   friend bdd      bdd_ibuildcubepp(int, int, int *);
   friend bdd      bdd_not(const bdd &);
   friend bdd      bdd_simplify(const bdd &, const bdd &);
   friend bdd      bdd_apply(const bdd &, const bdd &, int);
   friend bdd      bdd_and(const bdd &, const bdd &);
   friend bdd      bdd_or(const bdd &, const bdd &);
   friend bdd      bdd_xor(const bdd &, const bdd &);
   friend bdd      bdd_imp(const bdd &, const bdd &);
   friend bdd      bdd_biimp(const bdd &, const bdd &);
   friend bdd      bdd_ite(const bdd &, const bdd &, const bdd &);
   friend bdd      bdd_restrict(const bdd &, const bdd &);
   friend bdd      bdd_constrain(const bdd &, const bdd &);
   friend bdd      bdd_exist(const bdd &, const bdd &);
   friend bdd      bdd_forall(const bdd &, const bdd &);
   friend bdd      bdd_unique(const bdd &, const bdd &);
   friend bdd      bdd_appex(const bdd &, const bdd &, int, const bdd &);
   friend bdd      bdd_appall(const bdd &, const bdd &, int, const bdd &);
   friend bdd      bdd_appuni(const bdd &, const bdd &, int, const bdd &);
   friend bdd      bdd_replace(const bdd &, bddPair*);
   friend bdd      bdd_compose(const bdd &, const bdd &, int);
   friend bdd      bdd_veccompose(const bdd &, bddPair*);
   friend bdd      bdd_support(const bdd &);
   friend bdd      bdd_satone(const bdd &);
   friend bdd      bdd_satoneset(const bdd &, const bdd &, const bdd &);
   friend bdd      bdd_fullsatone(const bdd &);
   friend void     bdd_allsat(const bdd &r, bddallsathandler handler);
   friend double   bdd_satcount(const bdd &);
   friend double   bdd_satcountset(const bdd &, const bdd &);
   friend double   bdd_satcountln(const bdd &);
   friend double   bdd_satcountlnset(const bdd &, const bdd &);
   friend int      bdd_nodecount(const bdd &);
   friend int      bdd_anodecountpp(const bdd *, int);
   friend int*     bdd_varprofile(const bdd &);
   friend double   bdd_pathcount(const bdd &);
   
   friend void   bdd_fprinttable(FILE *, const bdd &);
   friend void   bdd_printtable(const bdd &);
   friend void   bdd_fprintset(FILE *, const bdd &);
   friend void   bdd_printset(const bdd &);
   friend void   bdd_printdot(const bdd &);
   friend int    bdd_fnprintdot(char*, const bdd &);
   friend void   bdd_fprintdot(FILE*, const bdd &);
   friend std::ostream &operator<<(std::ostream &, const bdd &);
   friend int    bdd_fnsave(char*, const bdd &);
   friend int    bdd_save(FILE*, const bdd &);
   friend int    bdd_fnload(char*, bdd &);
   friend int    bdd_load(FILE*, bdd &);
   
   friend bdd    fdd_ithvarpp(int, int);
   friend bdd    fdd_ithsetpp(int);
   friend bdd    fdd_domainpp(int);
   friend int    fdd_scanvar(const bdd &, int);
   friend int*   fdd_scanallvar(const bdd &);
   friend bdd    fdd_equalspp(int, int);
   friend void   fdd_printset(const bdd &);
   friend void   fdd_fprintset(FILE*, const bdd &);
   friend bdd    fdd_makesetpp(int*, int);
   friend int    fdd_scanset(const bdd &, int *&, int &);

   friend int    bdd_addvarblock(const bdd &, int);

   friend class bvec;
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
};


/*=== BDD constants ====================================================*/

extern const bdd bddfalsepp;
extern const bdd bddtruepp;

#define bddtrue bddtruepp
#define bddfalse bddfalsepp

/*=== C++ interface ====================================================*/

extern int bdd_cpp_init(int, int);

inline void bdd_stats(bddStat& s)
{ bdd_stats(&s); }

inline bdd bdd_ithvarpp(int v)
{ return bdd_ithvar(v); }

inline bdd bdd_nithvarpp(int v)
{ return bdd_nithvar(v); }

inline int bdd_var(const bdd &r)
{ return bdd_var(r.root); }

inline bdd bdd_low(const bdd &r)
{ return bdd_low(r.root); }

inline bdd bdd_high(const bdd &r)
{ return bdd_high(r.root); }

inline int bdd_scanset(const bdd &r, int *&v, int &n)
{ return bdd_scanset(r.root, &v, &n); }

inline bdd bdd_makesetpp(int *v, int n)
{ return bdd(bdd_makeset(v,n)); }

inline int bdd_setbddpair(bddPair *p, int ov, const bdd &nv)
{ return bdd_setbddpair(p,ov,nv.root); }

   /* In bddop.c */

inline bdd bdd_replace(const bdd &r, bddPair *p)
{ return bdd_replace(r.root, p); }

inline bdd bdd_compose(const bdd &f, const bdd &g, int v)
{ return bdd_compose(f.root, g.root, v); }

inline bdd bdd_veccompose(const bdd &f, bddPair *p)
{ return bdd_veccompose(f.root, p); }

inline bdd bdd_restrict(const bdd &r, const bdd &var)
{ return bdd_restrict(r.root, var.root); }

inline bdd bdd_constrain(const bdd &f, const bdd &c)
{ return bdd_constrain(f.root, c.root); }

inline bdd bdd_simplify(const bdd &d, const bdd &b)
{ return bdd_simplify(d.root, b.root); }

inline bdd bdd_ibuildcubepp(int v, int w, int *a)
{ return bdd_ibuildcube(v,w,a); }

inline bdd bdd_not(const bdd &r)
{ return bdd_not(r.root); }

inline bdd bdd_apply(const bdd &l, const bdd &r, int op)
{ return bdd_apply(l.root, r.root, op); }

inline bdd bdd_and(const bdd &l, const bdd &r)
{ return bdd_apply(l.root, r.root, bddop_and); }

inline bdd bdd_or(const bdd &l, const bdd &r)
{ return bdd_apply(l.root, r.root, bddop_or); }

inline bdd bdd_xor(const bdd &l, const bdd &r)
{ return bdd_apply(l.root, r.root, bddop_xor); }

inline bdd bdd_imp(const bdd &l, const bdd &r)
{ return bdd_apply(l.root, r.root, bddop_imp); }

inline bdd bdd_biimp(const bdd &l, const bdd &r)
{ return bdd_apply(l.root, r.root, bddop_biimp); }

inline bdd bdd_ite(const bdd &f, const bdd &g, const bdd &h)
{ return bdd_ite(f.root, g.root, h.root); }

inline bdd bdd_exist(const bdd &r, const bdd &var)
{ return bdd_exist(r.root, var.root); }

inline bdd bdd_forall(const bdd &r, const bdd &var)
{ return bdd_forall(r.root, var.root); }

inline bdd bdd_unique(const bdd &r, const bdd &var)
{ return bdd_unique(r.root, var.root); }

inline bdd bdd_appex(const bdd &l, const bdd &r, int op, const bdd &var)
{ return bdd_appex(l.root, r.root, op, var.root); }

inline bdd bdd_appall(const bdd &l, const bdd &r, int op, const bdd &var)
{ return bdd_appall(l.root, r.root, op, var.root); }

inline bdd bdd_appuni(const bdd &l, const bdd &r, int op, const bdd &var)
{ return bdd_appuni(l.root, r.root, op, var.root); }

inline bdd bdd_support(const bdd &r)
{ return bdd_support(r.root); }

inline bdd bdd_satone(const bdd &r)
{ return bdd_satone(r.root); }

inline bdd bdd_satoneset(const bdd &r, const bdd &var, const bdd &pol)
{ return bdd_satoneset(r.root, var.root, pol.root); }

inline bdd bdd_fullsatone(const bdd &r)
{ return bdd_fullsatone(r.root); }

inline void bdd_allsat(const bdd &r, bddallsathandler handler)
{ bdd_allsat(r.root, handler); }

inline double bdd_satcount(const bdd &r)
{ return bdd_satcount(r.root); }

inline double bdd_satcountset(const bdd &r, const bdd &varset)
{ return bdd_satcountset(r.root, varset.root); }

inline double bdd_satcountln(const bdd &r)
{ return bdd_satcountln(r.root); }

inline double bdd_satcountlnset(const bdd &r, const bdd &varset)
{ return bdd_satcountlnset(r.root, varset.root); }

inline int bdd_nodecount(const bdd &r)
{ return bdd_nodecount(r.root); }

inline int* bdd_varprofile(const bdd &r)
{ return bdd_varprofile(r.root); }

inline double bdd_pathcount(const bdd &r)
{ return bdd_pathcount(r.root); }


   /* I/O extensions */

inline void bdd_fprinttable(FILE *file, const bdd &r)
{ bdd_fprinttable(file, r.root); }

inline void bdd_printtable(const bdd &r)
{ bdd_printtable(r.root); }

inline void bdd_fprintset(FILE *file, const bdd &r)
{ bdd_fprintset(file, r.root); }

inline void bdd_printset(const bdd &r)
{ bdd_printset(r.root); }

inline void bdd_printdot(const bdd &r)
{ bdd_printdot(r.root); }

inline void bdd_fprintdot(FILE* ofile, const bdd &r)
{ bdd_fprintdot(ofile, r.root); }

inline int bdd_fnprintdot(char* fname, const bdd &r)
{ return bdd_fnprintdot(fname, r.root); }

inline int bdd_fnsave(char *fname, const bdd &r)
{ return bdd_fnsave(fname, r.root); }

inline int bdd_save(FILE *ofile, const bdd &r)
{ return bdd_save(ofile, r.root); }

inline int bdd_fnload(char *fname, bdd &r)
{ int lr,e; e=bdd_fnload(fname, &lr); r=bdd(lr); return e; }

inline int bdd_load(FILE *ifile, bdd &r)
{ int lr,e; e=bdd_load(ifile, &lr); r=bdd(lr); return e; }

inline int bdd_addvarblock(const bdd &v, int f)
{ return bdd_addvarblock(v.root, f); }

   /* Hack to allow for overloading */
#define bdd_init bdd_cpp_init
#define bdd_ithvar bdd_ithvarpp
#define bdd_nithvar bdd_nithvarpp
#define bdd_makeset bdd_makesetpp
#define bdd_ibuildcube bdd_ibuildcubepp
#define bdd_anodecount bdd_anodecountpp

/*=== Inline C++ functions =============================================*/

inline int bdd::id(void) const
{ return root; }

inline bdd bdd::operator&(const bdd &r) const
{ return bdd_apply(*this,r,bddop_and); }

inline bdd bdd::operator&=(const bdd &r)
{ return (*this=bdd_apply(*this,r,bddop_and)); }

inline bdd bdd::operator^(const bdd &r) const
{ return bdd_apply(*this,r,bddop_xor); }

inline bdd bdd::operator^=(const bdd &r)
{ return (*this=bdd_apply(*this,r,bddop_xor)); }

inline bdd bdd::operator|(const bdd &r) const
{ return bdd_apply(*this,r,bddop_or); }

inline bdd bdd::operator|=(const bdd &r)
{ return (*this=bdd_apply(*this,r,bddop_or)); }

inline bdd bdd::operator!(void) const
{ return bdd_not(*this);}

inline bdd bdd::operator>>(const bdd &r) const
{ return bdd_apply(*this,r,bddop_imp); }

inline bdd bdd::operator>>=(const bdd &r)
{ return (*this=bdd_apply(*this,r,bddop_imp)); }

inline bdd bdd::operator-(const bdd &r) const
{ return bdd_apply(*this,r,bddop_diff); }

inline bdd bdd::operator-=(const bdd &r)
{ return (*this=bdd_apply(*this,r,bddop_diff)); }

inline bdd bdd::operator>(const bdd &r) const
{ return bdd_apply(*this,r,bddop_diff); }

inline bdd bdd::operator<(const bdd &r) const
{ return bdd_apply(*this,r,bddop_less); }

inline bdd bdd::operator<<(const bdd &r) const
{ return bdd_apply(*this,r,bddop_invimp); }

inline bdd bdd::operator<<=(const bdd &r)
{ return (*this=bdd_apply(*this,r,bddop_invimp)); }

inline int bdd::operator==(const bdd &r) const
{ return r.root==root; }

inline int bdd::operator!=(const bdd &r) const
{ return r.root!=root; }

inline bdd bdd_true(void)
{ return 1; }

inline bdd bdd_false(void)
{ return 0; }


/*=== Iostream printing ================================================*/

class bdd_ioformat
{
 public:
   bdd_ioformat(int f) { format=f; }
 private:
   bdd_ioformat(void)  { }
   int format;
   static int curformat;

   friend std::ostream &operator<<(std::ostream &, const bdd_ioformat &);
   friend std::ostream &operator<<(std::ostream &, const bdd &);
};

std::ostream &operator<<(std::ostream &, const bdd &);
std::ostream &operator<<(std::ostream &, const bdd_ioformat &);

extern bdd_ioformat bddset;
extern bdd_ioformat bddtable;
extern bdd_ioformat bdddot;
extern bdd_ioformat bddall;
extern bdd_ioformat fddset;

typedef void (*bddstrmhandler)(std::ostream &, int);

extern bddstrmhandler bdd_strm_hook(bddstrmhandler);

#endif /* CPLUSPLUS */

#endif /* _BDD_H */

/* EOF */
