/*******************************************************************************
*                              INTEL CONFIDENTIAL
*   Copyright(C) 2004-2010 Intel Corporation. All Rights Reserved.
*   The source code contained  or  described herein and all documents related to
*   the source code ("Material") are owned by Intel Corporation or its suppliers
*   or licensors.  Title to the  Material remains with  Intel Corporation or its
*   suppliers and licensors. The Material contains trade secrets and proprietary
*   and  confidential  information of  Intel or its suppliers and licensors. The
*   Material  is  protected  by  worldwide  copyright  and trade secret laws and
*   treaty  provisions. No part of the Material may be used, copied, reproduced,
*   modified, published, uploaded, posted, transmitted, distributed or disclosed
*   in any way without Intel's prior express written permission.
*   No license  under any  patent, copyright, trade secret or other intellectual
*   property right is granted to or conferred upon you by disclosure or delivery
*   of the Materials,  either expressly, by implication, inducement, estoppel or
*   otherwise.  Any  license  under  such  intellectual property  rights must be
*   express and approved by Intel in writing.
*
********************************************************************************
*   Content : MKL DSS C header file
*
*           Contains more detailed information on internal datatypes and
*           constants used by DSS interface to PARDISO.
*
********************************************************************************
*/
#if !defined( __MKL_DSS_PARDISO_H )

#define __MKL_DSS_PARDISO_H

#include "mkl_dss.h"
#include "mkl_types.h"

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/******************************************************************************
**
**                         Data Definitions
**
** The primary data structures for laying the user interface on top of PARDISO
** are the user handle and the solver handle.  The user handle is a standard
** C data structure (as defined below) and is read and written *ONLY* by the
** user interface code.  It contains all of the user and state information
** necessary to interact with PARDISO and is an opaque type to both users and
** PARDISO.
**
** The solver handle is an array of fortran integers and is the data structure
** that PARDISO uses to communicate with the user (the interface routines in
** this case) and the solver itself.  Within the solver handle, there are two
** important substructures - the s_basic and auxInfo.  From the point of view
** of the user interface code, the important part of the s_basic structure
** is the integer array comi.  comi[11] is the PARDISO enumeration of the
** matrix type and comi[36] is the size of the work array, tmpsize, needed by
** PARDISO.
**
** The integer array, auxInfo, is used by PARDISO to hold user defined
** preferences for the operation of the solver
**
** The important part of the handle structure is the first 3 elements:
**
**	handle[0]		Initially, this location is set to zero to
**				indicate initialization is required.  After
**				that it becomes the base address of all PARDISO
**				memory references and consequently should never
**				be touched after initialization
**
**   	handle[1]		The address of 'basic' structure
**
**   	handle[2]...		Free locations that can be used by the
**	handle[*freehandle-1]	interface code.  The default value of freehandle
**				is 3, so by default there is one free location.
**				DSS uses this location to link back to the
**				user handle.
**
**	handle[*freehandle]...	PARDISO internal stuff
**	handle[*freehandle+NARRAYS-1]
*/

#define SOLVER_HANDLE_DIMENSION                      64
#define SOLVER_HANDLE_INIT_LOCATION                   0
#define SOLVER_HANDLE_S_BASIC_LOCATION                1
#define SOLVER_HANDLE_BACKLINK_LOCATION               2
#define SOLVER_HANDLE_FACT_ADR_LOCATION               3
#define SOLVER_HANDLE_STRUC_FI_LOCATION               7
#define SOLVER_HANDLE_OOC_ADR_LOCATION               16

#define AUXINFO_DIMENSION                            64
#define S_BASIC_COMI_MATRIX_TYPE_LOCATION            11
#define S_BASIC_COMI_TMPSIZ_LOCATION                 36
#define S_BASIC_COMI_NSUP_LOCATION                   30
#define S_BASIC_COMI_SYM_LOCATION                    18
#define S_BASIC_COMI_GSSUB_LOCATION                  35
#define S_BASIC_COMR_TIME_ADJ_LOCATION               0
#define S_BASIC_COMR_TIME_REORD_LOCATION             1
#define S_BASIC_COMR_TIME_SYMFCT_LOCATION            2
#define S_BASIC_COMR_TIME_SCAT_A_LOCATION            4
#define S_BASIC_COMR_TIME_NUMFCT_LOCATION            5
#define S_BASIC_COMR_TIME_SOLVE_LOCATION             6
#define S_BASIC_COMR_NUMFLOP_LOCATION                14
#define S_BASIC_DISP_XLNZ_LOCATION                   63
#define S_BASIC_DISP_XSUP_LOCATION                   60
#define S_BASIC_DISP_PIVOT_LOCATION                  67
#define S_BASIC_DISP_XUNZ_LOCATION                   64
#define S_BASIC_DISP_XLINDX_LOCATION                 61
#define S_BASIC_DISP_LINDX_LOCATION                  62
#define S_BASIC_DISP_FIN_INT_LOCATION                73
#define S_BASIC_DISP_FIN_NMOD_LOCATION               65
#define S_BASIC_DISP_FIN_SNODE_LOCATION              68

#define L_COMI  100
#define L_COMR  20
#define L_DISP  100
typedef struct
{
	_LONG_t comi[L_COMI];
	double comr[L_COMR];
	_LONG_t disp[L_DISP];
} s_basic;

/*
** The user handle contains an instantiation of the solver handle as well as
** all of the other data requird for communicating with PARDISO and the user.
** In order to be consistent with the user definitions of the data type, we
** use the type names defined in mkl_dss.h
*/

typedef struct _sparseDataStruct {
        MKL_INT          facilityHandle;
        _INTEGER_t   nRows;
        _INTEGER_t   nCols;
        _INTEGER_t   nNonZeros;
        _INTEGER_t   origNNonZeros;
        _INTEGER_t * majorIndex;
        _INTEGER_t * minorIndex;
        _INTEGER_t * permVector;
        _INTEGER_t * origMajorIndex;
        _INTEGER_t * origMinorIndex;
        _INTEGER_t * fill;

        MKL_INT          matrixType;
        MKL_INT          matrixStructure;
        MKL_INT          valueStructure;
        MKL_INT          dataType;
        void       * matrixValues;
        void       * origMatrixValues;
        _INTEGER_t   nRhs;
        MKL_INT          maxFacStore;
        MKL_INT          matrixNumber;
        void       * rhsValues;
        void       * computedSolution;
        void       * solverHandle[SOLVER_HANDLE_DIMENSION];
        MKL_INT          nCpus;
        _INTEGER_t   phase;
        _INTEGER_t   auxInfoArray[AUXINFO_DIMENSION];
        MKL_INT          solverMessageLevel;
        MKL_INT          messageLevel;
        MKL_INT          terminationLevel;
        MKL_INT          option1;		/* for debug only */
        MKL_INT          solverState;
        MKL_INT          detSign;
        double       detPow;
        double       detRealBase;
        double       detImgBase;
        MKL_INT          nPosEig;
        MKL_INT          nNegEig;
        MKL_INT          nZeroEig;
} typeDssHandle;

/*
** Many of the DSS routines invoke the underlying solver, PARDISO.  Because
** the call to PARDISO requires so many arguments, almost all of which are
** stored in the dssHandle data structure, we use a macro for the call.
** Additionally, for the interface, we want to redefine the behavior of PARDISO,
** we create a macro here that contains the platform specific name of the
** PARDISO routine.
*/

#if defined( _WIN32 ) || defined( _WIN64 )
#define pardiso_ PARDISO
#else
#define PARDISO pardiso_
#endif

extern  void pardiso_(
	_MKL_DSS_HANDLE_t *,   _INTEGER_t *,     _INTEGER_t *,
	_INTEGER_t *,           _INTEGER_t *,     _INTEGER_t *,
	void *,  _INTEGER_t *,     _INTEGER_t *,
	_INTEGER_t *,           _INTEGER_t *,     _INTEGER_t *,
	_INTEGER_t *,           void *, void *,
        _INTEGER_t *);

#define CALL_PARDISO(h, p, error) (h)->phase = (p); pardiso_ (              \
	 (h)->solverHandle,      &(h)->maxFacStore, &(h)->matrixNumber,     \
	&(h)->matrixType,	 &(h)->phase,       &(h)->nCols,	    \
	 (h)->matrixValues,       (h)->majorIndex,   (h)->minorIndex,	    \
	 (h)->permVector,	 &(h)->nRhs,         (h)->auxInfoArray,     \
	&(h)->solverMessageLevel, (h)->rhsValues,    (h)->computedSolution, \
	&error)

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif

