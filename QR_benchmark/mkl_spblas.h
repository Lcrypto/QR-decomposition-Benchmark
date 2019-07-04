/*******************************************************************************
!                             INTEL CONFIDENTIAL
!   Copyright(C) 2005-2010 Intel Corporation. All Rights Reserved.
!   The source code contained  or  described herein and all documents related to
!   the source code ("Material") are owned by Intel Corporation or its suppliers
!   or licensors.  Title to the  Material remains with  Intel Corporation or its
!   suppliers and licensors. The Material contains trade secrets and proprietary
!   and  confidential  information of  Intel or its suppliers and licensors. The
!   Material  is  protected  by  worldwide  copyright  and trade secret laws and
!   treaty  provisions. No part of the Material may be used, copied, reproduced,
!   modified, published, uploaded, posted, transmitted, distributed or disclosed
!   in any way without Intel's prior express written permission.
!   No license  under any  patent, copyright, trade secret or other intellectual
!   property right is granted to or conferred upon you by disclosure or delivery
!   of the Materials,  either expressly, by implication, inducement, estoppel or
!   otherwise.  Any  license  under  such  intellectual property  rights must be
!   express and approved by Intel in writing.
!
!*******************************************************************************
!  Content:
!    Intel(R) Math Kernel Library (MKL) interface for Sparse BLAS level 2,3 routines
!******************************************************************************/

#ifndef _MKL_SPBLAS_H_
#define _MKL_SPBLAS_H_

#include "mkl_types.h"

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/*Float*/
/* Sparse BLAS Level2 lower case */
void mkl_scsrmv(char *transa, MKL_INT *m, MKL_INT *k, float *alpha, char *matdescra, float  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, float *x, float *beta, float *y);
void mkl_scsrsv(char *transa, MKL_INT *m, float *alpha, char *matdescra, float  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, float *x, float *y);
void mkl_scsrgemv(char *transa, MKL_INT *m, float *a, MKL_INT *ia,  MKL_INT *ja, float *x,  float *y);
void mkl_cspblas_scsrgemv(char *transa, MKL_INT *m, float *a, MKL_INT *ia,  MKL_INT *ja, float *x,  float *y);
void mkl_scsrsymv(char *uplo, MKL_INT *m, float *a, MKL_INT *ia,  MKL_INT *ja, float *x,  float *y);
void mkl_cspblas_scsrsymv(char *uplo, MKL_INT *m, float *a, MKL_INT *ia,  MKL_INT *ja, float *x,  float *y);
void mkl_scsrtrsv(char *uplo, char *transa, char *diag, MKL_INT *m, float *a, MKL_INT *ia,  MKL_INT *ja, float *x,  float *y);
void mkl_cspblas_scsrtrsv(char *uplo, char *transa, char *diag, MKL_INT *m, float *a, MKL_INT *ia,  MKL_INT *ja, float *x,  float *y);

void mkl_scscmv(char *transa, MKL_INT *m, MKL_INT *k, float *alpha, char *matdescra, float  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, float *x, float *beta, float *y);
void mkl_scscsv(char *transa, MKL_INT *m, float *alpha, char *matdescra, float  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, float *x, float *y);

void mkl_scoomv(char *transa, MKL_INT *m, MKL_INT *k, float *alpha, char *matdescra, float  *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, float *x, float *beta, float *y);
void mkl_scoosv(char *transa, MKL_INT *m, float *alpha, char *matdescra, float  *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, float *x, float *y);
void mkl_scoogemv(char *transa, MKL_INT *m, float *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, float *x,  float *y);
void mkl_cspblas_scoogemv(char *transa, MKL_INT *m, float *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, float *x,  float *y);
void mkl_scoosymv(char *uplo, MKL_INT *m, float *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, float *x,  float *y);
void mkl_cspblas_scoosymv(char *uplo, MKL_INT *m, float *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, float *x,  float *y);
void mkl_scootrsv(char *uplo, char *transa, char *diag, MKL_INT *m, float *val, MKL_INT *rowind, MKL_INT *colind, MKL_INT *nnz, float *x,  float *y);
void mkl_cspblas_scootrsv(char *uplo, char *transa, char *diag, MKL_INT *m, float *val, MKL_INT *rowind, MKL_INT *colind, MKL_INT *nnz, float *x,  float *y);

void mkl_sdiamv (char *transa, MKL_INT *m, MKL_INT *k, float *alpha, char *matdescra, float  *val, MKL_INT *lval, MKL_INT *idiag,  MKL_INT *ndiag, float *x, float *beta, float *y);
void mkl_sdiasv (char *transa, MKL_INT *m, float *alpha, char *matdescra, float  *val, MKL_INT *lval, MKL_INT *idiag,  MKL_INT *ndiag, float *x, float *y);
void mkl_sdiagemv(char *transa, MKL_INT *m, float *val, MKL_INT *lval,  MKL_INT *idiag, MKL_INT *ndiag, float *x,  float *y);
void mkl_sdiasymv(char *uplo, MKL_INT *m, float *val, MKL_INT *lval,  MKL_INT *idiag, MKL_INT *ndiag, float *x,  float *y);
void mkl_sdiatrsv(char *uplo, char *transa, char *diag, MKL_INT *m, float *val, MKL_INT *lval,  MKL_INT  *idiag, MKL_INT *ndiag, float *x,  float *y);

void mkl_sskymv (char *transa, MKL_INT *m, MKL_INT *k, float *alpha, char *matdescra, float  *val, MKL_INT *pntr, float *x, float *beta, float *y);
void mkl_sskysv(char *transa, MKL_INT *m, float *alpha, char *matdescra, float  *val, MKL_INT *pntr,  float *x, float *y);

void mkl_sbsrmv (char *transa, MKL_INT *m, MKL_INT *k, MKL_INT *lb, float *alpha, char *matdescra, float  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, float *x, float *beta, float *y);
void mkl_sbsrsv(char *transa, MKL_INT *m, MKL_INT *lb, float *alpha, char *matdescra, float  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, float *x, float *y);
void mkl_sbsrgemv(char *transa, MKL_INT *m, MKL_INT *lb, float *a, MKL_INT *ia,  MKL_INT *ja, float *x,  float *y);
void mkl_cspblas_sbsrgemv(char *transa, MKL_INT *m, MKL_INT *lb, float *a, MKL_INT *ia,  MKL_INT *ja, float *x,  float *y);
void mkl_sbsrsymv(char *uplo, MKL_INT *m, MKL_INT *lb, float *a, MKL_INT *ia,  MKL_INT *ja, float *x,  float *y);
void mkl_cspblas_sbsrsymv(char *uplo, MKL_INT *m, MKL_INT *lb, float *a, MKL_INT *ia,  MKL_INT *ja, float *x,  float *y);
void mkl_sbsrtrsv(char *uplo, char *transa, char *diag, MKL_INT *m, MKL_INT *lb, float *a, MKL_INT *ia,  MKL_INT *ja, float *x,  float *y);
void mkl_cspblas_sbsrtrsv(char *uplo, char *transa, char *diag, MKL_INT *m, MKL_INT *lb, float *a, MKL_INT *ia,  MKL_INT *ja, float *x,  float *y);
/* Sparse BLAS Level3 lower case */

void mkl_scsrmm(char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, float *alpha, char *matdescra, float  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, float *b, MKL_INT *ldb, float *beta, float *c, MKL_INT *ldc);
void mkl_scsrsm(char *transa, MKL_INT *m, MKL_INT *n, float *alpha, char *matdescra, float  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, float *b, MKL_INT *ldb,  float *c, MKL_INT *ldc);

void mkl_scscmm(char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, float *alpha, char *matdescra, float  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, float *b, MKL_INT *ldb, float *beta, float *c, MKL_INT *ldc);
void mkl_scscsm(char *transa, MKL_INT *m, MKL_INT *n, float *alpha, char *matdescra, float  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, float *b, MKL_INT *ldb,  float *c, MKL_INT *ldc);

void mkl_scoomm(char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, float *alpha, char *matdescra, float  *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, float *b, MKL_INT *ldb, float *beta, float *c, MKL_INT *ldc);
void mkl_scoosm(char *transa, MKL_INT *m, MKL_INT *n, float *alpha, char *matdescra, float  *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, float *b, MKL_INT *ldb,  float *c, MKL_INT *ldc);

void mkl_sdiamm (char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, float *alpha, char *matdescra, float  *val, MKL_INT *lval, MKL_INT *idiag,  MKL_INT *ndiag, float *b, MKL_INT *ldb, float *beta, float *c, MKL_INT *ldc);
void mkl_sdiasm (char *transa, MKL_INT *m, MKL_INT *n, float *alpha, char *matdescra, float  *val, MKL_INT *lval, MKL_INT *idiag,  MKL_INT *ndiag, float *b, MKL_INT *ldb, float *c, MKL_INT *ldc);

void mkl_sskysm (char *transa, MKL_INT *m, MKL_INT *n, float *alpha, char *matdescra, float  *val, MKL_INT *pntr,  float *b, MKL_INT *ldb, float *c, MKL_INT *ldc);
void mkl_sskymm (char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, float *alpha, char *matdescra, float  *val, MKL_INT *pntr, float *b, MKL_INT *ldb, float *beta, float *c, MKL_INT *ldc);

void mkl_sbsrmm(char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_INT *lb, float *alpha, char *matdescra, float  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, float *b, MKL_INT *ldb, float *beta, float *c, MKL_INT *ldc);
void mkl_sbsrsm(char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *lb, float *alpha, char *matdescra, float  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, float *b, MKL_INT *ldb,  float *c, MKL_INT *ldc);

/* Upper case declaration */
/* Sparse BLAS Level2 upper case */
void MKL_SCSRMV (char *transa, MKL_INT *m, MKL_INT *k, float *alpha, char *matdescra, float  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, float *x, float *beta, float *y);
void MKL_SCSRSV(char *transa, MKL_INT *m, float *alpha, char *matdescra, float  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, float *x, float *y);
void MKL_SCSRGEMV(char *transa, MKL_INT *m, float *a, MKL_INT *ia,  MKL_INT *ja, float *x,  float *y);
void MKL_CSPBLAS_SCSRGEMV(char *transa, MKL_INT *m, float *a, MKL_INT *ia,  MKL_INT *ja, float *x,  float *y);
void MKL_SCSRSYMV(char *uplo, MKL_INT *m, float *a, MKL_INT *ia,  MKL_INT *ja, float *x,  float *y);
void MKL_CSPBLAS_SCSRSYMV(char *uplo, MKL_INT *m, float *a, MKL_INT *ia,  MKL_INT *ja, float *x,  float *y);
void MKL_SCSRTRSV(char *uplo, char *transa, char *diag, MKL_INT *m, float *a, MKL_INT *ia,  MKL_INT *ja, float *x,  float *y);
void MKL_CSPBLAS_SCSRTRSV(char *uplo, char *transa, char *diag, MKL_INT *m, float *a, MKL_INT *ia,  MKL_INT *ja, float *x,  float *y);

void MKL_SCSCMV(char *transa, MKL_INT *m, MKL_INT *k, float *alpha, char *matdescra, float  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, float *x, float *beta, float *y);
void MKL_SCSCSV(char *transa, MKL_INT *m, float *alpha, char *matdescra, float  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, float *x, float *y);

void MKL_SCOOMV(char *transa, MKL_INT *m, MKL_INT *k, float *alpha, char *matdescra, float  *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, float *x, float *beta, float *y);
void MKL_SCOOSV(char *transa, MKL_INT *m, float *alpha, char *matdescra, float  *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, float *x, float *y);
void MKL_SCOOGEMV(char *transa, MKL_INT *m, float *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, float *x,  float *y);
void MKL_CSPBLAS_SCOOGEMV(char *transa, MKL_INT *m, float *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, float *x,  float *y);
void MKL_SCOOSYMV(char *uplo, MKL_INT *m, float *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, float *x,  float *y);
void MKL_CSPBLAS_SCOOSYMV(char *uplo, MKL_INT *m, float *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, float *x,  float *y);
void MKL_SCOOTRSV(char *uplo, char *transa, char *diag, MKL_INT *m, float *val, MKL_INT *rowind, MKL_INT *colind, MKL_INT *nnz, float *x,  float *y);
void MKL_CSPBLAS_SCOOTRSV(char *uplo, char *transa, char *diag, MKL_INT *m, float *val, MKL_INT *rowind, MKL_INT *colind, MKL_INT *nnz, float *x,  float *y);

void MKL_SDIAMV (char *transa, MKL_INT *m, MKL_INT *k, float *alpha, char *matdescra, float  *val, MKL_INT *lval, MKL_INT *idiag,  MKL_INT *ndiag, float *x, float *beta, float *y);
void MKL_SDIASV (char *transa, MKL_INT *m, float *alpha, char *matdescra, float  *val, MKL_INT *lval, MKL_INT *idiag,  MKL_INT *ndiag, float *x, float *y);
void MKL_SDIAGEMV(char *transa, MKL_INT *m, float *val, MKL_INT *lval,  MKL_INT *idiag, MKL_INT *ndiag, float *x,  float *y);
void MKL_SDIASYMV(char *uplo, MKL_INT *m, float *val, MKL_INT *lval,  MKL_INT *idiag, MKL_INT *ndiag, float *x,  float *y);
void MKL_SDIATRSV(char *uplo, char *transa, char *diag, MKL_INT *m, float *val, MKL_INT *lval,  MKL_INT  *idiag, MKL_INT *ndiag, float *x,  float *y);

void MKL_SSKYMV (char *transa, MKL_INT *m, MKL_INT *k, float *alpha, char *matdescra, float  *val, MKL_INT *pntr, float *x, float *beta, float *y);
void MKL_SSKYSV(char *transa, MKL_INT *m, float *alpha, char *matdescra, float  *val, MKL_INT *pntr,  float *x, float *y);

void MKL_SBSRMV (char *transa, MKL_INT *m, MKL_INT *k, MKL_INT *lb, float *alpha, char *matdescra, float  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, float *x, float *beta, float *y);
void MKL_SBSRSV(char *transa, MKL_INT *m, MKL_INT *lb, float *alpha, char *matdescra, float  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, float *x, float *y);
void MKL_SBSRGEMV(char *transa, MKL_INT *m, MKL_INT *lb, float *a, MKL_INT *ia,  MKL_INT *ja, float *x,  float *y);
void MKL_CSPBLAS_SBSRGEMV(char *transa, MKL_INT *m, MKL_INT *lb, float *a, MKL_INT *ia,  MKL_INT *ja, float *x,  float *y);
void MKL_SBSRSYMV(char *uplo, MKL_INT *m, MKL_INT *lb, float *a, MKL_INT *ia,  MKL_INT *ja, float *x,  float *y);
void MKL_CSPBLAS_SBSRSYMV(char *uplo, MKL_INT *m, MKL_INT *lb, float *a, MKL_INT *ia,  MKL_INT *ja, float *x,  float *y);
void MKL_SBSRTRSV(char *uplo, char *transa, char *diag, MKL_INT *m, MKL_INT *lb, float *a, MKL_INT *ia,  MKL_INT *ja, float *x,  float *y);
void MKL_CSPBLAS_SBSRTRSV(char *uplo, char *transa, char *diag, MKL_INT *m, MKL_INT *lb, float *a, MKL_INT *ia,  MKL_INT *ja, float *x,  float *y);

/* Sparse BLAS Level3 upper case */

void MKL_SCSRMM(char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, float *alpha, char *matdescra, float  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, float *b, MKL_INT *ldb, float *beta, float *c, MKL_INT *ldc);
void MKL_SCSRSM(char *transa, MKL_INT *m, MKL_INT *n, float *alpha, char *matdescra, float  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, float *b, MKL_INT *ldb,  float *c, MKL_INT *ldc);

void MKL_SCSCMM(char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, float *alpha, char *matdescra, float  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, float *b, MKL_INT *ldb, float *beta, float *c, MKL_INT *ldc);
void MKL_SCSCSM(char *transa, MKL_INT *m, MKL_INT *n, float *alpha, char *matdescra, float  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, float *b, MKL_INT *ldb,  float *c, MKL_INT *ldc);

void MKL_SCOOMM(char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, float *alpha, char *matdescra, float  *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, float *b, MKL_INT *ldb, float *beta, float *c, MKL_INT *ldc);
void MKL_SCOOSM(char *transa, MKL_INT *m, MKL_INT *n, float *alpha, char *matdescra, float  *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, float *b, MKL_INT *ldb,  float *c, MKL_INT *ldc);

void MKL_SDIAMM (char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, float *alpha, char *matdescra, float  *val, MKL_INT *lval, MKL_INT *idiag,  MKL_INT *ndiag, float *b, MKL_INT *ldb, float *beta, float *c, MKL_INT *ldc);
void MKL_SDIASM (char *transa, MKL_INT *m, MKL_INT *n, float *alpha, char *matdescra, float  *val, MKL_INT *lval, MKL_INT *idiag,  MKL_INT *ndiag, float *b, MKL_INT *ldb, float *c, MKL_INT *ldc);

void MKL_SSKYSM (char *transa, MKL_INT *m, MKL_INT *n, float *alpha, char *matdescra, float  *val, MKL_INT *pntr,  float *b, MKL_INT *ldb, float *c, MKL_INT *ldc);
void MKL_SSKYMM (char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, float *alpha, char *matdescra, float  *val, MKL_INT *pntr, float *b, MKL_INT *ldb, float *beta, float *c, MKL_INT *ldc);

void MKL_SBSRMM(char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_INT *lb, float *alpha, char *matdescra, float  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, float *b, MKL_INT *ldb, float *beta, float *c, MKL_INT *ldc);
void MKL_SBSRSM(char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *lb, float *alpha, char *matdescra, float  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, float *b, MKL_INT *ldb,  float *c, MKL_INT *ldc);

/*Double*/
/* Sparse BLAS Level2 lower case */
void mkl_dcsrmv(char *transa, MKL_INT *m, MKL_INT *k, double *alpha, char *matdescra, double  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, double *x, double *beta, double *y);
void mkl_dcsrsv(char *transa, MKL_INT *m, double *alpha, char *matdescra, double  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, double *x, double *y);
void mkl_dcsrgemv(char *transa, MKL_INT *m, double *a, MKL_INT *ia,  MKL_INT *ja, double *x,  double *y);
void mkl_cspblas_dcsrgemv(char *transa, MKL_INT *m, double *a, MKL_INT *ia,  MKL_INT *ja, double *x,  double *y);
void mkl_dcsrsymv(char *uplo, MKL_INT *m, double *a, MKL_INT *ia,  MKL_INT *ja, double *x,  double *y);
void mkl_cspblas_dcsrsymv(char *uplo, MKL_INT *m, double *a, MKL_INT *ia,  MKL_INT *ja, double *x,  double *y);
void mkl_dcsrtrsv(char *uplo, char *transa, char *diag, MKL_INT *m, double *a, MKL_INT *ia,  MKL_INT *ja, double *x,  double *y);
void mkl_cspblas_dcsrtrsv(char *uplo, char *transa, char *diag, MKL_INT *m, double *a, MKL_INT *ia,  MKL_INT *ja, double *x,  double *y);

void mkl_dcscmv(char *transa, MKL_INT *m, MKL_INT *k, double *alpha, char *matdescra, double  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, double *x, double *beta, double *y);
void mkl_dcscsv(char *transa, MKL_INT *m, double *alpha, char *matdescra, double  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, double *x, double *y);

void mkl_dcoomv(char *transa, MKL_INT *m, MKL_INT *k, double *alpha, char *matdescra, double  *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, double *x, double *beta, double *y);
void mkl_dcoosv(char *transa, MKL_INT *m, double *alpha, char *matdescra, double  *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, double *x, double *y);
void mkl_dcoogemv(char *transa, MKL_INT *m, double *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, double *x,  double *y);
void mkl_cspblas_dcoogemv(char *transa, MKL_INT *m, double *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, double *x,  double *y);
void mkl_dcoosymv(char *uplo, MKL_INT *m, double *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, double *x,  double *y);
void mkl_cspblas_dcoosymv(char *uplo, MKL_INT *m, double *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, double *x,  double *y);
void mkl_dcootrsv(char *uplo, char *transa, char *diag, MKL_INT *m, double *val, MKL_INT *rowind, MKL_INT *colind, MKL_INT *nnz, double *x,  double *y);
void mkl_cspblas_dcootrsv(char *uplo, char *transa, char *diag, MKL_INT *m, double *val, MKL_INT *rowind, MKL_INT *colind, MKL_INT *nnz, double *x,  double *y);

void mkl_ddiamv (char *transa, MKL_INT *m, MKL_INT *k, double *alpha, char *matdescra, double  *val, MKL_INT *lval, MKL_INT *idiag,  MKL_INT *ndiag, double *x, double *beta, double *y);
void mkl_ddiasv (char *transa, MKL_INT *m, double *alpha, char *matdescra, double  *val, MKL_INT *lval, MKL_INT *idiag,  MKL_INT *ndiag, double *x, double *y);
void mkl_ddiagemv(char *transa, MKL_INT *m, double *val, MKL_INT *lval,  MKL_INT *idiag, MKL_INT *ndiag, double *x,  double *y);
void mkl_ddiasymv(char *uplo, MKL_INT *m, double *val, MKL_INT *lval,  MKL_INT *idiag, MKL_INT *ndiag, double *x,  double *y);
void mkl_ddiatrsv(char *uplo, char *transa, char *diag, MKL_INT *m, double *val, MKL_INT *lval,  MKL_INT  *idiag, MKL_INT *ndiag, double *x,  double *y);

void mkl_dskymv (char *transa, MKL_INT *m, MKL_INT *k, double *alpha, char *matdescra, double  *val, MKL_INT *pntr, double *x, double *beta, double *y);
void mkl_dskysv(char *transa, MKL_INT *m, double *alpha, char *matdescra, double  *val, MKL_INT *pntr,  double *x, double *y);

void mkl_dbsrmv (char *transa, MKL_INT *m, MKL_INT *k, MKL_INT *lb, double *alpha, char *matdescra, double  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, double *x, double *beta, double *y);
void mkl_dbsrsv(char *transa, MKL_INT *m, MKL_INT *lb, double *alpha, char *matdescra, double  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, double *x, double *y);
void mkl_dbsrgemv(char *transa, MKL_INT *m, MKL_INT *lb, double *a, MKL_INT *ia,  MKL_INT *ja, double *x,  double *y);
void mkl_cspblas_dbsrgemv(char *transa, MKL_INT *m, MKL_INT *lb, double *a, MKL_INT *ia,  MKL_INT *ja, double *x,  double *y);
void mkl_dbsrsymv(char *uplo, MKL_INT *m, MKL_INT *lb, double *a, MKL_INT *ia,  MKL_INT *ja, double *x,  double *y);
void mkl_cspblas_dbsrsymv(char *uplo, MKL_INT *m, MKL_INT *lb, double *a, MKL_INT *ia,  MKL_INT *ja, double *x,  double *y);
void mkl_dbsrtrsv(char *uplo, char *transa, char *diag, MKL_INT *m, MKL_INT *lb, double *a, MKL_INT *ia,  MKL_INT *ja, double *x,  double *y);
void mkl_cspblas_dbsrtrsv(char *uplo, char *transa, char *diag, MKL_INT *m, MKL_INT *lb, double *a, MKL_INT *ia,  MKL_INT *ja, double *x,  double *y);
/* Sparse BLAS Level3 lower case */

void mkl_dcsrmm(char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, double *alpha, char *matdescra, double  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, double *b, MKL_INT *ldb, double *beta, double *c, MKL_INT *ldc);
void mkl_dcsrsm(char *transa, MKL_INT *m, MKL_INT *n, double *alpha, char *matdescra, double  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, double *b, MKL_INT *ldb,  double *c, MKL_INT *ldc);

void mkl_dcscmm(char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, double *alpha, char *matdescra, double  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, double *b, MKL_INT *ldb, double *beta, double *c, MKL_INT *ldc);
void mkl_dcscsm(char *transa, MKL_INT *m, MKL_INT *n, double *alpha, char *matdescra, double  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, double *b, MKL_INT *ldb,  double *c, MKL_INT *ldc);

void mkl_dcoomm(char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, double *alpha, char *matdescra, double  *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, double *b, MKL_INT *ldb, double *beta, double *c, MKL_INT *ldc);
void mkl_dcoosm(char *transa, MKL_INT *m, MKL_INT *n, double *alpha, char *matdescra, double  *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, double *b, MKL_INT *ldb,  double *c, MKL_INT *ldc);

void mkl_ddiamm (char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, double *alpha, char *matdescra, double  *val, MKL_INT *lval, MKL_INT *idiag,  MKL_INT *ndiag, double *b, MKL_INT *ldb, double *beta, double *c, MKL_INT *ldc);
void mkl_ddiasm (char *transa, MKL_INT *m, MKL_INT *n, double *alpha, char *matdescra, double  *val, MKL_INT *lval, MKL_INT *idiag,  MKL_INT *ndiag, double *b, MKL_INT *ldb, double *c, MKL_INT *ldc);

void mkl_dskysm (char *transa, MKL_INT *m, MKL_INT *n, double *alpha, char *matdescra, double  *val, MKL_INT *pntr,  double *b, MKL_INT *ldb, double *c, MKL_INT *ldc);
void mkl_dskymm (char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, double *alpha, char *matdescra, double  *val, MKL_INT *pntr, double *b, MKL_INT *ldb, double *beta, double *c, MKL_INT *ldc);

void mkl_dbsrmm(char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_INT *lb, double *alpha, char *matdescra, double  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, double *b, MKL_INT *ldb, double *beta, double *c, MKL_INT *ldc);
void mkl_dbsrsm(char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *lb, double *alpha, char *matdescra, double  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, double *b, MKL_INT *ldb,  double *c, MKL_INT *ldc);

/* Upper case declaration */
/* Sparse BLAS Level2 upper case */
void MKL_DCSRMV (char *transa, MKL_INT *m, MKL_INT *k, double *alpha, char *matdescra, double  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, double *x, double *beta, double *y);
void MKL_DCSRSV(char *transa, MKL_INT *m, double *alpha, char *matdescra, double  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, double *x, double *y);
void MKL_DCSRGEMV(char *transa, MKL_INT *m, double *a, MKL_INT *ia,  MKL_INT *ja, double *x,  double *y);
void MKL_CSPBLAS_DCSRGEMV(char *transa, MKL_INT *m, double *a, MKL_INT *ia,  MKL_INT *ja, double *x,  double *y);
void MKL_DCSRSYMV(char *uplo, MKL_INT *m, double *a, MKL_INT *ia,  MKL_INT *ja, double *x,  double *y);
void MKL_CSPBLAS_DCSRSYMV(char *uplo, MKL_INT *m, double *a, MKL_INT *ia,  MKL_INT *ja, double *x,  double *y);
void MKL_DCSRTRSV(char *uplo, char *transa, char *diag, MKL_INT *m, double *a, MKL_INT *ia,  MKL_INT *ja, double *x,  double *y);
void MKL_CSPBLAS_DCSRTRSV(char *uplo, char *transa, char *diag, MKL_INT *m, double *a, MKL_INT *ia,  MKL_INT *ja, double *x,  double *y);

void MKL_DCSCMV(char *transa, MKL_INT *m, MKL_INT *k, double *alpha, char *matdescra, double  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, double *x, double *beta, double *y);
void MKL_DCSCSV(char *transa, MKL_INT *m, double *alpha, char *matdescra, double  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, double *x, double *y);

void MKL_DCOOMV(char *transa, MKL_INT *m, MKL_INT *k, double *alpha, char *matdescra, double  *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, double *x, double *beta, double *y);
void MKL_DCOOSV(char *transa, MKL_INT *m, double *alpha, char *matdescra, double  *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, double *x, double *y);
void MKL_DCOOGEMV(char *transa, MKL_INT *m, double *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, double *x,  double *y);
void MKL_CSPBLAS_DCOOGEMV(char *transa, MKL_INT *m, double *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, double *x,  double *y);
void MKL_DCOOSYMV(char *uplo, MKL_INT *m, double *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, double *x,  double *y);
void MKL_CSPBLAS_DCOOSYMV(char *uplo, MKL_INT *m, double *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, double *x,  double *y);
void MKL_DCOOTRSV(char *uplo, char *transa, char *diag, MKL_INT *m, double *val, MKL_INT *rowind, MKL_INT *colind, MKL_INT *nnz, double *x,  double *y);
void MKL_CSPBLAS_DCOOTRSV(char *uplo, char *transa, char *diag, MKL_INT *m, double *val, MKL_INT *rowind, MKL_INT *colind, MKL_INT *nnz, double *x,  double *y);

void MKL_DDIAMV (char *transa, MKL_INT *m, MKL_INT *k, double *alpha, char *matdescra, double  *val, MKL_INT *lval, MKL_INT *idiag,  MKL_INT *ndiag, double *x, double *beta, double *y);
void MKL_DDIASV (char *transa, MKL_INT *m, double *alpha, char *matdescra, double  *val, MKL_INT *lval, MKL_INT *idiag,  MKL_INT *ndiag, double *x, double *y);
void MKL_DDIAGEMV(char *transa, MKL_INT *m, double *val, MKL_INT *lval,  MKL_INT *idiag, MKL_INT *ndiag, double *x,  double *y);
void MKL_DDIASYMV(char *uplo, MKL_INT *m, double *val, MKL_INT *lval,  MKL_INT *idiag, MKL_INT *ndiag, double *x,  double *y);
void MKL_DDIATRSV(char *uplo, char *transa, char *diag, MKL_INT *m, double *val, MKL_INT *lval,  MKL_INT  *idiag, MKL_INT *ndiag, double *x,  double *y);

void MKL_DSKYMV (char *transa, MKL_INT *m, MKL_INT *k, double *alpha, char *matdescra, double  *val, MKL_INT *pntr, double *x, double *beta, double *y);
void MKL_DSKYSV(char *transa, MKL_INT *m, double *alpha, char *matdescra, double  *val, MKL_INT *pntr,  double *x, double *y);

void MKL_DBSRMV (char *transa, MKL_INT *m, MKL_INT *k, MKL_INT *lb, double *alpha, char *matdescra, double  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, double *x, double *beta, double *y);
void MKL_DBSRSV(char *transa, MKL_INT *m, MKL_INT *lb, double *alpha, char *matdescra, double  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, double *x, double *y);
void MKL_DBSRGEMV(char *transa, MKL_INT *m, MKL_INT *lb, double *a, MKL_INT *ia,  MKL_INT *ja, double *x,  double *y);
void MKL_CSPBLAS_DBSRGEMV(char *transa, MKL_INT *m, MKL_INT *lb, double *a, MKL_INT *ia,  MKL_INT *ja, double *x,  double *y);
void MKL_DBSRSYMV(char *uplo, MKL_INT *m, MKL_INT *lb, double *a, MKL_INT *ia,  MKL_INT *ja, double *x,  double *y);
void MKL_CSPBLAS_DBSRSYMV(char *uplo, MKL_INT *m, MKL_INT *lb, double *a, MKL_INT *ia,  MKL_INT *ja, double *x,  double *y);
void MKL_DBSRTRSV(char *uplo, char *transa, char *diag, MKL_INT *m, MKL_INT *lb, double *a, MKL_INT *ia,  MKL_INT *ja, double *x,  double *y);
void MKL_CSPBLAS_DBSRTRSV(char *uplo, char *transa, char *diag, MKL_INT *m, MKL_INT *lb, double *a, MKL_INT *ia,  MKL_INT *ja, double *x,  double *y);

/* Sparse BLAS Level3 upper case */

void MKL_DCSRMM(char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, double *alpha, char *matdescra, double  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, double *b, MKL_INT *ldb, double *beta, double *c, MKL_INT *ldc);
void MKL_DCSRSM(char *transa, MKL_INT *m, MKL_INT *n, double *alpha, char *matdescra, double  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, double *b, MKL_INT *ldb,  double *c, MKL_INT *ldc);

void MKL_DCSCMM(char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, double *alpha, char *matdescra, double  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, double *b, MKL_INT *ldb, double *beta, double *c, MKL_INT *ldc);
void MKL_DCSCSM(char *transa, MKL_INT *m, MKL_INT *n, double *alpha, char *matdescra, double  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, double *b, MKL_INT *ldb,  double *c, MKL_INT *ldc);

void MKL_DCOOMM(char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, double *alpha, char *matdescra, double  *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, double *b, MKL_INT *ldb, double *beta, double *c, MKL_INT *ldc);
void MKL_DCOOSM(char *transa, MKL_INT *m, MKL_INT *n, double *alpha, char *matdescra, double  *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, double *b, MKL_INT *ldb,  double *c, MKL_INT *ldc);

void MKL_DDIAMM (char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, double *alpha, char *matdescra, double  *val, MKL_INT *lval, MKL_INT *idiag,  MKL_INT *ndiag, double *b, MKL_INT *ldb, double *beta, double *c, MKL_INT *ldc);
void MKL_DDIASM (char *transa, MKL_INT *m, MKL_INT *n, double *alpha, char *matdescra, double  *val, MKL_INT *lval, MKL_INT *idiag,  MKL_INT *ndiag, double *b, MKL_INT *ldb, double *c, MKL_INT *ldc);

void MKL_DSKYSM (char *transa, MKL_INT *m, MKL_INT *n, double *alpha, char *matdescra, double  *val, MKL_INT *pntr,  double *b, MKL_INT *ldb, double *c, MKL_INT *ldc);
void MKL_DSKYMM (char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, double *alpha, char *matdescra, double  *val, MKL_INT *pntr, double *b, MKL_INT *ldb, double *beta, double *c, MKL_INT *ldc);

void MKL_DBSRMM(char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_INT *lb, double *alpha, char *matdescra, double  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, double *b, MKL_INT *ldb, double *beta, double *c, MKL_INT *ldc);
void MKL_DBSRSM(char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *lb, double *alpha, char *matdescra, double  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, double *b, MKL_INT *ldb,  double *c, MKL_INT *ldc);

/*MKL_Complex8*/
/* Sparse BLAS Level2 lower case */
void mkl_ccsrmv(char *transa, MKL_INT *m, MKL_INT *k, MKL_Complex8 *alpha, char *matdescra, MKL_Complex8  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, MKL_Complex8 *x, MKL_Complex8 *beta, MKL_Complex8 *y);
void mkl_ccsrsv(char *transa, MKL_INT *m, MKL_Complex8 *alpha, char *matdescra, MKL_Complex8  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, MKL_Complex8 *x, MKL_Complex8 *y);
void mkl_ccsrgemv(char *transa, MKL_INT *m, MKL_Complex8 *a, MKL_INT *ia,  MKL_INT *ja, MKL_Complex8 *x,  MKL_Complex8 *y);
void mkl_cspblas_ccsrgemv(char *transa, MKL_INT *m, MKL_Complex8 *a, MKL_INT *ia,  MKL_INT *ja, MKL_Complex8 *x,  MKL_Complex8 *y);
void mkl_ccsrsymv(char *uplo, MKL_INT *m, MKL_Complex8 *a, MKL_INT *ia,  MKL_INT *ja, MKL_Complex8 *x,  MKL_Complex8 *y);
void mkl_cspblas_ccsrsymv(char *uplo, MKL_INT *m, MKL_Complex8 *a, MKL_INT *ia,  MKL_INT *ja, MKL_Complex8 *x,  MKL_Complex8 *y);
void mkl_ccsrtrsv(char *uplo, char *transa, char *diag, MKL_INT *m, MKL_Complex8 *a, MKL_INT *ia,  MKL_INT *ja, MKL_Complex8 *x,  MKL_Complex8 *y);
void mkl_cspblas_ccsrtrsv(char *uplo, char *transa, char *diag, MKL_INT *m, MKL_Complex8 *a, MKL_INT *ia,  MKL_INT *ja, MKL_Complex8 *x,  MKL_Complex8 *y);

void mkl_ccscmv(char *transa, MKL_INT *m, MKL_INT *k, MKL_Complex8 *alpha, char *matdescra, MKL_Complex8  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, MKL_Complex8 *x, MKL_Complex8 *beta, MKL_Complex8 *y);
void mkl_ccscsv(char *transa, MKL_INT *m, MKL_Complex8 *alpha, char *matdescra, MKL_Complex8  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, MKL_Complex8 *x, MKL_Complex8 *y);

void mkl_ccoomv(char *transa, MKL_INT *m, MKL_INT *k, MKL_Complex8 *alpha, char *matdescra, MKL_Complex8  *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, MKL_Complex8 *x, MKL_Complex8 *beta, MKL_Complex8 *y);
void mkl_ccoosv(char *transa, MKL_INT *m, MKL_Complex8 *alpha, char *matdescra, MKL_Complex8  *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, MKL_Complex8 *x, MKL_Complex8 *y);
void mkl_ccoogemv(char *transa, MKL_INT *m, MKL_Complex8 *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, MKL_Complex8 *x,  MKL_Complex8 *y);
void mkl_cspblas_ccoogemv(char *transa, MKL_INT *m, MKL_Complex8 *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, MKL_Complex8 *x,  MKL_Complex8 *y);
void mkl_ccoosymv(char *uplo, MKL_INT *m, MKL_Complex8 *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, MKL_Complex8 *x,  MKL_Complex8 *y);
void mkl_cspblas_ccoosymv(char *uplo, MKL_INT *m, MKL_Complex8 *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, MKL_Complex8 *x,  MKL_Complex8 *y);
void mkl_ccootrsv(char *uplo, char *transa, char *diag, MKL_INT *m, MKL_Complex8 *val, MKL_INT *rowind, MKL_INT *colind, MKL_INT *nnz, MKL_Complex8 *x,  MKL_Complex8 *y);
void mkl_cspblas_ccootrsv(char *uplo, char *transa, char *diag, MKL_INT *m, MKL_Complex8 *val, MKL_INT *rowind, MKL_INT *colind, MKL_INT *nnz, MKL_Complex8 *x,  MKL_Complex8 *y);

void mkl_cdiamv (char *transa, MKL_INT *m, MKL_INT *k, MKL_Complex8 *alpha, char *matdescra, MKL_Complex8  *val, MKL_INT *lval, MKL_INT *idiag,  MKL_INT *ndiag, MKL_Complex8 *x, MKL_Complex8 *beta, MKL_Complex8 *y);
void mkl_cdiasv (char *transa, MKL_INT *m, MKL_Complex8 *alpha, char *matdescra, MKL_Complex8  *val, MKL_INT *lval, MKL_INT *idiag,  MKL_INT *ndiag, MKL_Complex8 *x, MKL_Complex8 *y);
void mkl_cdiagemv(char *transa, MKL_INT *m, MKL_Complex8 *val, MKL_INT *lval,  MKL_INT *idiag, MKL_INT *ndiag, MKL_Complex8 *x,  MKL_Complex8 *y);
void mkl_cdiasymv(char *uplo, MKL_INT *m, MKL_Complex8 *val, MKL_INT *lval,  MKL_INT *idiag, MKL_INT *ndiag, MKL_Complex8 *x,  MKL_Complex8 *y);
void mkl_cdiatrsv(char *uplo, char *transa, char *diag, MKL_INT *m, MKL_Complex8 *val, MKL_INT *lval,  MKL_INT  *idiag, MKL_INT *ndiag, MKL_Complex8 *x,  MKL_Complex8 *y);

void mkl_cskymv (char *transa, MKL_INT *m, MKL_INT *k, MKL_Complex8 *alpha, char *matdescra, MKL_Complex8  *val, MKL_INT *pntr, MKL_Complex8 *x, MKL_Complex8 *beta, MKL_Complex8 *y);
void mkl_cskysv(char *transa, MKL_INT *m, MKL_Complex8 *alpha, char *matdescra, MKL_Complex8  *val, MKL_INT *pntr,  MKL_Complex8 *x, MKL_Complex8 *y);

void mkl_cbsrmv (char *transa, MKL_INT *m, MKL_INT *k, MKL_INT *lb, MKL_Complex8 *alpha, char *matdescra, MKL_Complex8  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, MKL_Complex8 *x, MKL_Complex8 *beta, MKL_Complex8 *y);
void mkl_cbsrsv(char *transa, MKL_INT *m, MKL_INT *lb, MKL_Complex8 *alpha, char *matdescra, MKL_Complex8  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, MKL_Complex8 *x, MKL_Complex8 *y);
void mkl_cbsrgemv(char *transa, MKL_INT *m, MKL_INT *lb, MKL_Complex8 *a, MKL_INT *ia,  MKL_INT *ja, MKL_Complex8 *x,  MKL_Complex8 *y);
void mkl_cspblas_cbsrgemv(char *transa, MKL_INT *m, MKL_INT *lb, MKL_Complex8 *a, MKL_INT *ia,  MKL_INT *ja, MKL_Complex8 *x,  MKL_Complex8 *y);
void mkl_cbsrsymv(char *uplo, MKL_INT *m, MKL_INT *lb, MKL_Complex8 *a, MKL_INT *ia,  MKL_INT *ja, MKL_Complex8 *x,  MKL_Complex8 *y);
void mkl_cspblas_cbsrsymv(char *uplo, MKL_INT *m, MKL_INT *lb, MKL_Complex8 *a, MKL_INT *ia,  MKL_INT *ja, MKL_Complex8 *x,  MKL_Complex8 *y);
void mkl_cbsrtrsv(char *uplo, char *transa, char *diag, MKL_INT *m, MKL_INT *lb, MKL_Complex8 *a, MKL_INT *ia,  MKL_INT *ja, MKL_Complex8 *x,  MKL_Complex8 *y);
void mkl_cspblas_cbsrtrsv(char *uplo, char *transa, char *diag, MKL_INT *m, MKL_INT *lb, MKL_Complex8 *a, MKL_INT *ia,  MKL_INT *ja, MKL_Complex8 *x,  MKL_Complex8 *y);
/* Sparse BLAS Level3 lower case */

void mkl_ccsrmm(char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex8 *alpha, char *matdescra, MKL_Complex8  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, MKL_Complex8 *b, MKL_INT *ldb, MKL_Complex8 *beta, MKL_Complex8 *c, MKL_INT *ldc);
void mkl_ccsrsm(char *transa, MKL_INT *m, MKL_INT *n, MKL_Complex8 *alpha, char *matdescra, MKL_Complex8  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, MKL_Complex8 *b, MKL_INT *ldb,  MKL_Complex8 *c, MKL_INT *ldc);

void mkl_ccscmm(char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex8 *alpha, char *matdescra, MKL_Complex8  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, MKL_Complex8 *b, MKL_INT *ldb, MKL_Complex8 *beta, MKL_Complex8 *c, MKL_INT *ldc);
void mkl_ccscsm(char *transa, MKL_INT *m, MKL_INT *n, MKL_Complex8 *alpha, char *matdescra, MKL_Complex8  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, MKL_Complex8 *b, MKL_INT *ldb,  MKL_Complex8 *c, MKL_INT *ldc);

void mkl_ccoomm(char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex8 *alpha, char *matdescra, MKL_Complex8  *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, MKL_Complex8 *b, MKL_INT *ldb, MKL_Complex8 *beta, MKL_Complex8 *c, MKL_INT *ldc);
void mkl_ccoosm(char *transa, MKL_INT *m, MKL_INT *n, MKL_Complex8 *alpha, char *matdescra, MKL_Complex8  *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, MKL_Complex8 *b, MKL_INT *ldb,  MKL_Complex8 *c, MKL_INT *ldc);

void mkl_cdiamm (char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex8 *alpha, char *matdescra, MKL_Complex8  *val, MKL_INT *lval, MKL_INT *idiag,  MKL_INT *ndiag, MKL_Complex8 *b, MKL_INT *ldb, MKL_Complex8 *beta, MKL_Complex8 *c, MKL_INT *ldc);
void mkl_cdiasm (char *transa, MKL_INT *m, MKL_INT *n, MKL_Complex8 *alpha, char *matdescra, MKL_Complex8  *val, MKL_INT *lval, MKL_INT *idiag,  MKL_INT *ndiag, MKL_Complex8 *b, MKL_INT *ldb, MKL_Complex8 *c, MKL_INT *ldc);

void mkl_cskysm (char *transa, MKL_INT *m, MKL_INT *n, MKL_Complex8 *alpha, char *matdescra, MKL_Complex8  *val, MKL_INT *pntr,  MKL_Complex8 *b, MKL_INT *ldb, MKL_Complex8 *c, MKL_INT *ldc);
void mkl_cskymm (char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex8 *alpha, char *matdescra, MKL_Complex8  *val, MKL_INT *pntr, MKL_Complex8 *b, MKL_INT *ldb, MKL_Complex8 *beta, MKL_Complex8 *c, MKL_INT *ldc);

void mkl_cbsrmm(char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_INT *lb, MKL_Complex8 *alpha, char *matdescra, MKL_Complex8  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, MKL_Complex8 *b, MKL_INT *ldb, MKL_Complex8 *beta, MKL_Complex8 *c, MKL_INT *ldc);
void mkl_cbsrsm(char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *lb, MKL_Complex8 *alpha, char *matdescra, MKL_Complex8  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, MKL_Complex8 *b, MKL_INT *ldb,  MKL_Complex8 *c, MKL_INT *ldc);

/* Upper case declaration */
/* Sparse BLAS Level2 upper case */
void MKL_CCSRMV (char *transa, MKL_INT *m, MKL_INT *k, MKL_Complex8 *alpha, char *matdescra, MKL_Complex8  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, MKL_Complex8 *x, MKL_Complex8 *beta, MKL_Complex8 *y);
void MKL_CCSRSV(char *transa, MKL_INT *m, MKL_Complex8 *alpha, char *matdescra, MKL_Complex8  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, MKL_Complex8 *x, MKL_Complex8 *y);
void MKL_CCSRGEMV(char *transa, MKL_INT *m, MKL_Complex8 *a, MKL_INT *ia,  MKL_INT *ja, MKL_Complex8 *x,  MKL_Complex8 *y);
void MKL_CSPBLAS_CCSRGEMV(char *transa, MKL_INT *m, MKL_Complex8 *a, MKL_INT *ia,  MKL_INT *ja, MKL_Complex8 *x,  MKL_Complex8 *y);
void MKL_CCSRSYMV(char *uplo, MKL_INT *m, MKL_Complex8 *a, MKL_INT *ia,  MKL_INT *ja, MKL_Complex8 *x,  MKL_Complex8 *y);
void MKL_CSPBLAS_CCSRSYMV(char *uplo, MKL_INT *m, MKL_Complex8 *a, MKL_INT *ia,  MKL_INT *ja, MKL_Complex8 *x,  MKL_Complex8 *y);
void MKL_CCSRTRSV(char *uplo, char *transa, char *diag, MKL_INT *m, MKL_Complex8 *a, MKL_INT *ia,  MKL_INT *ja, MKL_Complex8 *x,  MKL_Complex8 *y);
void MKL_CSPBLAS_CCSRTRSV(char *uplo, char *transa, char *diag, MKL_INT *m, MKL_Complex8 *a, MKL_INT *ia,  MKL_INT *ja, MKL_Complex8 *x,  MKL_Complex8 *y);

void MKL_CCSCMV(char *transa, MKL_INT *m, MKL_INT *k, MKL_Complex8 *alpha, char *matdescra, MKL_Complex8  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, MKL_Complex8 *x, MKL_Complex8 *beta, MKL_Complex8 *y);
void MKL_CCSCSV(char *transa, MKL_INT *m, MKL_Complex8 *alpha, char *matdescra, MKL_Complex8  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, MKL_Complex8 *x, MKL_Complex8 *y);

void MKL_CCOOMV(char *transa, MKL_INT *m, MKL_INT *k, MKL_Complex8 *alpha, char *matdescra, MKL_Complex8  *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, MKL_Complex8 *x, MKL_Complex8 *beta, MKL_Complex8 *y);
void MKL_CCOOSV(char *transa, MKL_INT *m, MKL_Complex8 *alpha, char *matdescra, MKL_Complex8  *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, MKL_Complex8 *x, MKL_Complex8 *y);
void MKL_CCOOGEMV(char *transa, MKL_INT *m, MKL_Complex8 *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, MKL_Complex8 *x,  MKL_Complex8 *y);
void MKL_CSPBLAS_CCOOGEMV(char *transa, MKL_INT *m, MKL_Complex8 *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, MKL_Complex8 *x,  MKL_Complex8 *y);
void MKL_CCOOSYMV(char *uplo, MKL_INT *m, MKL_Complex8 *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, MKL_Complex8 *x,  MKL_Complex8 *y);
void MKL_CSPBLAS_CCOOSYMV(char *uplo, MKL_INT *m, MKL_Complex8 *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, MKL_Complex8 *x,  MKL_Complex8 *y);
void MKL_CCOOTRSV(char *uplo, char *transa, char *diag, MKL_INT *m, MKL_Complex8 *val, MKL_INT *rowind, MKL_INT *colind, MKL_INT *nnz, MKL_Complex8 *x,  MKL_Complex8 *y);
void MKL_CSPBLAS_CCOOTRSV(char *uplo, char *transa, char *diag, MKL_INT *m, MKL_Complex8 *val, MKL_INT *rowind, MKL_INT *colind, MKL_INT *nnz, MKL_Complex8 *x,  MKL_Complex8 *y);

void MKL_CDIAMV (char *transa, MKL_INT *m, MKL_INT *k, MKL_Complex8 *alpha, char *matdescra, MKL_Complex8  *val, MKL_INT *lval, MKL_INT *idiag,  MKL_INT *ndiag, MKL_Complex8 *x, MKL_Complex8 *beta, MKL_Complex8 *y);
void MKL_CDIASV (char *transa, MKL_INT *m, MKL_Complex8 *alpha, char *matdescra, MKL_Complex8  *val, MKL_INT *lval, MKL_INT *idiag,  MKL_INT *ndiag, MKL_Complex8 *x, MKL_Complex8 *y);
void MKL_CDIAGEMV(char *transa, MKL_INT *m, MKL_Complex8 *val, MKL_INT *lval,  MKL_INT *idiag, MKL_INT *ndiag, MKL_Complex8 *x,  MKL_Complex8 *y);
void MKL_CDIASYMV(char *uplo, MKL_INT *m, MKL_Complex8 *val, MKL_INT *lval,  MKL_INT *idiag, MKL_INT *ndiag, MKL_Complex8 *x,  MKL_Complex8 *y);
void MKL_CDIATRSV(char *uplo, char *transa, char *diag, MKL_INT *m, MKL_Complex8 *val, MKL_INT *lval,  MKL_INT  *idiag, MKL_INT *ndiag, MKL_Complex8 *x,  MKL_Complex8 *y);

void MKL_CSKYMV (char *transa, MKL_INT *m, MKL_INT *k, MKL_Complex8 *alpha, char *matdescra, MKL_Complex8  *val, MKL_INT *pntr, MKL_Complex8 *x, MKL_Complex8 *beta, MKL_Complex8 *y);
void MKL_CSKYSV(char *transa, MKL_INT *m, MKL_Complex8 *alpha, char *matdescra, MKL_Complex8  *val, MKL_INT *pntr,  MKL_Complex8 *x, MKL_Complex8 *y);

void MKL_CBSRMV (char *transa, MKL_INT *m, MKL_INT *k, MKL_INT *lb, MKL_Complex8 *alpha, char *matdescra, MKL_Complex8  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, MKL_Complex8 *x, MKL_Complex8 *beta, MKL_Complex8 *y);
void MKL_CBSRSV(char *transa, MKL_INT *m, MKL_INT *lb, MKL_Complex8 *alpha, char *matdescra, MKL_Complex8  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, MKL_Complex8 *x, MKL_Complex8 *y);
void MKL_CBSRGEMV(char *transa, MKL_INT *m, MKL_INT *lb, MKL_Complex8 *a, MKL_INT *ia,  MKL_INT *ja, MKL_Complex8 *x,  MKL_Complex8 *y);
void MKL_CSPBLAS_CBSRGEMV(char *transa, MKL_INT *m, MKL_INT *lb, MKL_Complex8 *a, MKL_INT *ia,  MKL_INT *ja, MKL_Complex8 *x,  MKL_Complex8 *y);
void MKL_CBSRSYMV(char *uplo, MKL_INT *m, MKL_INT *lb, MKL_Complex8 *a, MKL_INT *ia,  MKL_INT *ja, MKL_Complex8 *x,  MKL_Complex8 *y);
void MKL_CSPBLAS_CBSRSYMV(char *uplo, MKL_INT *m, MKL_INT *lb, MKL_Complex8 *a, MKL_INT *ia,  MKL_INT *ja, MKL_Complex8 *x,  MKL_Complex8 *y);
void MKL_CBSRTRSV(char *uplo, char *transa, char *diag, MKL_INT *m, MKL_INT *lb, MKL_Complex8 *a, MKL_INT *ia,  MKL_INT *ja, MKL_Complex8 *x,  MKL_Complex8 *y);
void MKL_CSPBLAS_CBSRTRSV(char *uplo, char *transa, char *diag, MKL_INT *m, MKL_INT *lb, MKL_Complex8 *a, MKL_INT *ia,  MKL_INT *ja, MKL_Complex8 *x,  MKL_Complex8 *y);

/* Sparse BLAS Level3 upper case */

void MKL_CCSRMM(char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex8 *alpha, char *matdescra, MKL_Complex8  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, MKL_Complex8 *b, MKL_INT *ldb, MKL_Complex8 *beta, MKL_Complex8 *c, MKL_INT *ldc);
void MKL_CCSRSM(char *transa, MKL_INT *m, MKL_INT *n, MKL_Complex8 *alpha, char *matdescra, MKL_Complex8  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, MKL_Complex8 *b, MKL_INT *ldb,  MKL_Complex8 *c, MKL_INT *ldc);

void MKL_CCSCMM(char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex8 *alpha, char *matdescra, MKL_Complex8  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, MKL_Complex8 *b, MKL_INT *ldb, MKL_Complex8 *beta, MKL_Complex8 *c, MKL_INT *ldc);
void MKL_CCSCSM(char *transa, MKL_INT *m, MKL_INT *n, MKL_Complex8 *alpha, char *matdescra, MKL_Complex8  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, MKL_Complex8 *b, MKL_INT *ldb,  MKL_Complex8 *c, MKL_INT *ldc);

void MKL_CCOOMM(char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex8 *alpha, char *matdescra, MKL_Complex8  *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, MKL_Complex8 *b, MKL_INT *ldb, MKL_Complex8 *beta, MKL_Complex8 *c, MKL_INT *ldc);
void MKL_CCOOSM(char *transa, MKL_INT *m, MKL_INT *n, MKL_Complex8 *alpha, char *matdescra, MKL_Complex8  *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, MKL_Complex8 *b, MKL_INT *ldb,  MKL_Complex8 *c, MKL_INT *ldc);

void MKL_CDIAMM (char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex8 *alpha, char *matdescra, MKL_Complex8  *val, MKL_INT *lval, MKL_INT *idiag,  MKL_INT *ndiag, MKL_Complex8 *b, MKL_INT *ldb, MKL_Complex8 *beta, MKL_Complex8 *c, MKL_INT *ldc);
void MKL_CDIASM (char *transa, MKL_INT *m, MKL_INT *n, MKL_Complex8 *alpha, char *matdescra, MKL_Complex8  *val, MKL_INT *lval, MKL_INT *idiag,  MKL_INT *ndiag, MKL_Complex8 *b, MKL_INT *ldb, MKL_Complex8 *c, MKL_INT *ldc);

void MKL_CSKYSM (char *transa, MKL_INT *m, MKL_INT *n, MKL_Complex8 *alpha, char *matdescra, MKL_Complex8  *val, MKL_INT *pntr,  MKL_Complex8 *b, MKL_INT *ldb, MKL_Complex8 *c, MKL_INT *ldc);
void MKL_CSKYMM (char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex8 *alpha, char *matdescra, MKL_Complex8  *val, MKL_INT *pntr, MKL_Complex8 *b, MKL_INT *ldb, MKL_Complex8 *beta, MKL_Complex8 *c, MKL_INT *ldc);

void MKL_CBSRMM(char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_INT *lb, MKL_Complex8 *alpha, char *matdescra, MKL_Complex8  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, MKL_Complex8 *b, MKL_INT *ldb, MKL_Complex8 *beta, MKL_Complex8 *c, MKL_INT *ldc);
void MKL_CBSRSM(char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *lb, MKL_Complex8 *alpha, char *matdescra, MKL_Complex8  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, MKL_Complex8 *b, MKL_INT *ldb,  MKL_Complex8 *c, MKL_INT *ldc);

/*Float*/
/* Sparse BLAS Level2 lower case */
void mkl_zcsrmv(char *transa, MKL_INT *m, MKL_INT *k, MKL_Complex16 *alpha, char *matdescra, MKL_Complex16  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, MKL_Complex16 *x, MKL_Complex16 *beta, MKL_Complex16 *y);
void mkl_zcsrsv(char *transa, MKL_INT *m, MKL_Complex16 *alpha, char *matdescra, MKL_Complex16  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, MKL_Complex16 *x, MKL_Complex16 *y);
void mkl_zcsrgemv(char *transa, MKL_INT *m, MKL_Complex16 *a, MKL_INT *ia,  MKL_INT *ja, MKL_Complex16 *x,  MKL_Complex16 *y);
void mkl_cspblas_zcsrgemv(char *transa, MKL_INT *m, MKL_Complex16 *a, MKL_INT *ia,  MKL_INT *ja, MKL_Complex16 *x,  MKL_Complex16 *y);
void mkl_zcsrsymv(char *uplo, MKL_INT *m, MKL_Complex16 *a, MKL_INT *ia,  MKL_INT *ja, MKL_Complex16 *x,  MKL_Complex16 *y);
void mkl_cspblas_zcsrsymv(char *uplo, MKL_INT *m, MKL_Complex16 *a, MKL_INT *ia,  MKL_INT *ja, MKL_Complex16 *x,  MKL_Complex16 *y);
void mkl_zcsrtrsv(char *uplo, char *transa, char *diag, MKL_INT *m, MKL_Complex16 *a, MKL_INT *ia,  MKL_INT *ja, MKL_Complex16 *x,  MKL_Complex16 *y);
void mkl_cspblas_zcsrtrsv(char *uplo, char *transa, char *diag, MKL_INT *m, MKL_Complex16 *a, MKL_INT *ia,  MKL_INT *ja, MKL_Complex16 *x,  MKL_Complex16 *y);

void mkl_zcscmv(char *transa, MKL_INT *m, MKL_INT *k, MKL_Complex16 *alpha, char *matdescra, MKL_Complex16  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, MKL_Complex16 *x, MKL_Complex16 *beta, MKL_Complex16 *y);
void mkl_zcscsv(char *transa, MKL_INT *m, MKL_Complex16 *alpha, char *matdescra, MKL_Complex16  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, MKL_Complex16 *x, MKL_Complex16 *y);

void mkl_zcoomv(char *transa, MKL_INT *m, MKL_INT *k, MKL_Complex16 *alpha, char *matdescra, MKL_Complex16  *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, MKL_Complex16 *x, MKL_Complex16 *beta, MKL_Complex16 *y);
void mkl_zcoosv(char *transa, MKL_INT *m, MKL_Complex16 *alpha, char *matdescra, MKL_Complex16  *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, MKL_Complex16 *x, MKL_Complex16 *y);
void mkl_zcoogemv(char *transa, MKL_INT *m, MKL_Complex16 *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, MKL_Complex16 *x,  MKL_Complex16 *y);
void mkl_cspblas_zcoogemv(char *transa, MKL_INT *m, MKL_Complex16 *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, MKL_Complex16 *x,  MKL_Complex16 *y);
void mkl_zcoosymv(char *uplo, MKL_INT *m, MKL_Complex16 *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, MKL_Complex16 *x,  MKL_Complex16 *y);
void mkl_cspblas_zcoosymv(char *uplo, MKL_INT *m, MKL_Complex16 *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, MKL_Complex16 *x,  MKL_Complex16 *y);
void mkl_zcootrsv(char *uplo, char *transa, char *diag, MKL_INT *m, MKL_Complex16 *val, MKL_INT *rowind, MKL_INT *colind, MKL_INT *nnz, MKL_Complex16 *x,  MKL_Complex16 *y);
void mkl_cspblas_zcootrsv(char *uplo, char *transa, char *diag, MKL_INT *m, MKL_Complex16 *val, MKL_INT *rowind, MKL_INT *colind, MKL_INT *nnz, MKL_Complex16 *x,  MKL_Complex16 *y);

void mkl_zdiamv (char *transa, MKL_INT *m, MKL_INT *k, MKL_Complex16 *alpha, char *matdescra, MKL_Complex16  *val, MKL_INT *lval, MKL_INT *idiag,  MKL_INT *ndiag, MKL_Complex16 *x, MKL_Complex16 *beta, MKL_Complex16 *y);
void mkl_zdiasv (char *transa, MKL_INT *m, MKL_Complex16 *alpha, char *matdescra, MKL_Complex16  *val, MKL_INT *lval, MKL_INT *idiag,  MKL_INT *ndiag, MKL_Complex16 *x, MKL_Complex16 *y);
void mkl_zdiagemv(char *transa, MKL_INT *m, MKL_Complex16 *val, MKL_INT *lval,  MKL_INT *idiag, MKL_INT *ndiag, MKL_Complex16 *x,  MKL_Complex16 *y);
void mkl_zdiasymv(char *uplo, MKL_INT *m, MKL_Complex16 *val, MKL_INT *lval,  MKL_INT *idiag, MKL_INT *ndiag, MKL_Complex16 *x,  MKL_Complex16 *y);
void mkl_zdiatrsv(char *uplo, char *transa, char *diag, MKL_INT *m, MKL_Complex16 *val, MKL_INT *lval,  MKL_INT  *idiag, MKL_INT *ndiag, MKL_Complex16 *x,  MKL_Complex16 *y);

void mkl_zskymv (char *transa, MKL_INT *m, MKL_INT *k, MKL_Complex16 *alpha, char *matdescra, MKL_Complex16  *val, MKL_INT *pntr, MKL_Complex16 *x, MKL_Complex16 *beta, MKL_Complex16 *y);
void mkl_zskysv(char *transa, MKL_INT *m, MKL_Complex16 *alpha, char *matdescra, MKL_Complex16  *val, MKL_INT *pntr,  MKL_Complex16 *x, MKL_Complex16 *y);

void mkl_zbsrmv (char *transa, MKL_INT *m, MKL_INT *k, MKL_INT *lb, MKL_Complex16 *alpha, char *matdescra, MKL_Complex16  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, MKL_Complex16 *x, MKL_Complex16 *beta, MKL_Complex16 *y);
void mkl_zbsrsv(char *transa, MKL_INT *m, MKL_INT *lb, MKL_Complex16 *alpha, char *matdescra, MKL_Complex16  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, MKL_Complex16 *x, MKL_Complex16 *y);
void mkl_zbsrgemv(char *transa, MKL_INT *m, MKL_INT *lb, MKL_Complex16 *a, MKL_INT *ia,  MKL_INT *ja, MKL_Complex16 *x,  MKL_Complex16 *y);
void mkl_cspblas_zbsrgemv(char *transa, MKL_INT *m, MKL_INT *lb, MKL_Complex16 *a, MKL_INT *ia,  MKL_INT *ja, MKL_Complex16 *x,  MKL_Complex16 *y);
void mkl_zbsrsymv(char *uplo, MKL_INT *m, MKL_INT *lb, MKL_Complex16 *a, MKL_INT *ia,  MKL_INT *ja, MKL_Complex16 *x,  MKL_Complex16 *y);
void mkl_cspblas_zbsrsymv(char *uplo, MKL_INT *m, MKL_INT *lb, MKL_Complex16 *a, MKL_INT *ia,  MKL_INT *ja, MKL_Complex16 *x,  MKL_Complex16 *y);
void mkl_zbsrtrsv(char *uplo, char *transa, char *diag, MKL_INT *m, MKL_INT *lb, MKL_Complex16 *a, MKL_INT *ia,  MKL_INT *ja, MKL_Complex16 *x,  MKL_Complex16 *y);
void mkl_cspblas_zbsrtrsv(char *uplo, char *transa, char *diag, MKL_INT *m, MKL_INT *lb, MKL_Complex16 *a, MKL_INT *ia,  MKL_INT *ja, MKL_Complex16 *x,  MKL_Complex16 *y);
/* Sparse BLAS Level3 lower case */

void mkl_zcsrmm(char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex16 *alpha, char *matdescra, MKL_Complex16  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, MKL_Complex16 *b, MKL_INT *ldb, MKL_Complex16 *beta, MKL_Complex16 *c, MKL_INT *ldc);
void mkl_zcsrsm(char *transa, MKL_INT *m, MKL_INT *n, MKL_Complex16 *alpha, char *matdescra, MKL_Complex16  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, MKL_Complex16 *b, MKL_INT *ldb,  MKL_Complex16 *c, MKL_INT *ldc);

void mkl_zcscmm(char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex16 *alpha, char *matdescra, MKL_Complex16  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, MKL_Complex16 *b, MKL_INT *ldb, MKL_Complex16 *beta, MKL_Complex16 *c, MKL_INT *ldc);
void mkl_zcscsm(char *transa, MKL_INT *m, MKL_INT *n, MKL_Complex16 *alpha, char *matdescra, MKL_Complex16  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, MKL_Complex16 *b, MKL_INT *ldb,  MKL_Complex16 *c, MKL_INT *ldc);

void mkl_zcoomm(char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex16 *alpha, char *matdescra, MKL_Complex16  *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, MKL_Complex16 *b, MKL_INT *ldb, MKL_Complex16 *beta, MKL_Complex16 *c, MKL_INT *ldc);
void mkl_zcoosm(char *transa, MKL_INT *m, MKL_INT *n, MKL_Complex16 *alpha, char *matdescra, MKL_Complex16  *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, MKL_Complex16 *b, MKL_INT *ldb,  MKL_Complex16 *c, MKL_INT *ldc);

void mkl_zdiamm (char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex16 *alpha, char *matdescra, MKL_Complex16  *val, MKL_INT *lval, MKL_INT *idiag,  MKL_INT *ndiag, MKL_Complex16 *b, MKL_INT *ldb, MKL_Complex16 *beta, MKL_Complex16 *c, MKL_INT *ldc);
void mkl_zdiasm (char *transa, MKL_INT *m, MKL_INT *n, MKL_Complex16 *alpha, char *matdescra, MKL_Complex16  *val, MKL_INT *lval, MKL_INT *idiag,  MKL_INT *ndiag, MKL_Complex16 *b, MKL_INT *ldb, MKL_Complex16 *c, MKL_INT *ldc);

void mkl_zskysm (char *transa, MKL_INT *m, MKL_INT *n, MKL_Complex16 *alpha, char *matdescra, MKL_Complex16  *val, MKL_INT *pntr,  MKL_Complex16 *b, MKL_INT *ldb, MKL_Complex16 *c, MKL_INT *ldc);
void mkl_zskymm (char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex16 *alpha, char *matdescra, MKL_Complex16  *val, MKL_INT *pntr, MKL_Complex16 *b, MKL_INT *ldb, MKL_Complex16 *beta, MKL_Complex16 *c, MKL_INT *ldc);

void mkl_zbsrmm(char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_INT *lb, MKL_Complex16 *alpha, char *matdescra, MKL_Complex16  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, MKL_Complex16 *b, MKL_INT *ldb, MKL_Complex16 *beta, MKL_Complex16 *c, MKL_INT *ldc);
void mkl_zbsrsm(char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *lb, MKL_Complex16 *alpha, char *matdescra, MKL_Complex16  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, MKL_Complex16 *b, MKL_INT *ldb,  MKL_Complex16 *c, MKL_INT *ldc);

/* Upper case declaration */
/* Sparse BLAS Level2 upper case */
void MKL_ZCSRMV (char *transa, MKL_INT *m, MKL_INT *k, MKL_Complex16 *alpha, char *matdescra, MKL_Complex16  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, MKL_Complex16 *x, MKL_Complex16 *beta, MKL_Complex16 *y);
void MKL_ZCSRSV(char *transa, MKL_INT *m, MKL_Complex16 *alpha, char *matdescra, MKL_Complex16  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, MKL_Complex16 *x, MKL_Complex16 *y);
void MKL_ZCSRGEMV(char *transa, MKL_INT *m, MKL_Complex16 *a, MKL_INT *ia,  MKL_INT *ja, MKL_Complex16 *x,  MKL_Complex16 *y);
void MKL_CSPBLAS_ZCSRGEMV(char *transa, MKL_INT *m, MKL_Complex16 *a, MKL_INT *ia,  MKL_INT *ja, MKL_Complex16 *x,  MKL_Complex16 *y);
void MKL_ZCSRSYMV(char *uplo, MKL_INT *m, MKL_Complex16 *a, MKL_INT *ia,  MKL_INT *ja, MKL_Complex16 *x,  MKL_Complex16 *y);
void MKL_CSPBLAS_ZCSRSYMV(char *uplo, MKL_INT *m, MKL_Complex16 *a, MKL_INT *ia,  MKL_INT *ja, MKL_Complex16 *x,  MKL_Complex16 *y);
void MKL_ZCSRTRSV(char *uplo, char *transa, char *diag, MKL_INT *m, MKL_Complex16 *a, MKL_INT *ia,  MKL_INT *ja, MKL_Complex16 *x,  MKL_Complex16 *y);
void MKL_CSPBLAS_ZCSRTRSV(char *uplo, char *transa, char *diag, MKL_INT *m, MKL_Complex16 *a, MKL_INT *ia,  MKL_INT *ja, MKL_Complex16 *x,  MKL_Complex16 *y);

void MKL_ZCSCMV(char *transa, MKL_INT *m, MKL_INT *k, MKL_Complex16 *alpha, char *matdescra, MKL_Complex16  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, MKL_Complex16 *x, MKL_Complex16 *beta, MKL_Complex16 *y);
void MKL_ZCSCSV(char *transa, MKL_INT *m, MKL_Complex16 *alpha, char *matdescra, MKL_Complex16  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, MKL_Complex16 *x, MKL_Complex16 *y);

void MKL_ZCOOMV(char *transa, MKL_INT *m, MKL_INT *k, MKL_Complex16 *alpha, char *matdescra, MKL_Complex16  *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, MKL_Complex16 *x, MKL_Complex16 *beta, MKL_Complex16 *y);
void MKL_ZCOOSV(char *transa, MKL_INT *m, MKL_Complex16 *alpha, char *matdescra, MKL_Complex16  *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, MKL_Complex16 *x, MKL_Complex16 *y);
void MKL_ZCOOGEMV(char *transa, MKL_INT *m, MKL_Complex16 *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, MKL_Complex16 *x,  MKL_Complex16 *y);
void MKL_CSPBLAS_ZCOOGEMV(char *transa, MKL_INT *m, MKL_Complex16 *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, MKL_Complex16 *x,  MKL_Complex16 *y);
void MKL_ZCOOSYMV(char *uplo, MKL_INT *m, MKL_Complex16 *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, MKL_Complex16 *x,  MKL_Complex16 *y);
void MKL_CSPBLAS_ZCOOSYMV(char *uplo, MKL_INT *m, MKL_Complex16 *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, MKL_Complex16 *x,  MKL_Complex16 *y);
void MKL_ZCOOTRSV(char *uplo, char *transa, char *diag, MKL_INT *m, MKL_Complex16 *val, MKL_INT *rowind, MKL_INT *colind, MKL_INT *nnz, MKL_Complex16 *x,  MKL_Complex16 *y);
void MKL_CSPBLAS_ZCOOTRSV(char *uplo, char *transa, char *diag, MKL_INT *m, MKL_Complex16 *val, MKL_INT *rowind, MKL_INT *colind, MKL_INT *nnz, MKL_Complex16 *x,  MKL_Complex16 *y);

void MKL_ZDIAMV (char *transa, MKL_INT *m, MKL_INT *k, MKL_Complex16 *alpha, char *matdescra, MKL_Complex16  *val, MKL_INT *lval, MKL_INT *idiag,  MKL_INT *ndiag, MKL_Complex16 *x, MKL_Complex16 *beta, MKL_Complex16 *y);
void MKL_ZDIASV (char *transa, MKL_INT *m, MKL_Complex16 *alpha, char *matdescra, MKL_Complex16  *val, MKL_INT *lval, MKL_INT *idiag,  MKL_INT *ndiag, MKL_Complex16 *x, MKL_Complex16 *y);
void MKL_ZDIAGEMV(char *transa, MKL_INT *m, MKL_Complex16 *val, MKL_INT *lval,  MKL_INT *idiag, MKL_INT *ndiag, MKL_Complex16 *x,  MKL_Complex16 *y);
void MKL_ZDIASYMV(char *uplo, MKL_INT *m, MKL_Complex16 *val, MKL_INT *lval,  MKL_INT *idiag, MKL_INT *ndiag, MKL_Complex16 *x,  MKL_Complex16 *y);
void MKL_ZDIATRSV(char *uplo, char *transa, char *diag, MKL_INT *m, MKL_Complex16 *val, MKL_INT *lval,  MKL_INT  *idiag, MKL_INT *ndiag, MKL_Complex16 *x,  MKL_Complex16 *y);

void MKL_ZSKYMV (char *transa, MKL_INT *m, MKL_INT *k, MKL_Complex16 *alpha, char *matdescra, MKL_Complex16  *val, MKL_INT *pntr, MKL_Complex16 *x, MKL_Complex16 *beta, MKL_Complex16 *y);
void MKL_ZSKYSV(char *transa, MKL_INT *m, MKL_Complex16 *alpha, char *matdescra, MKL_Complex16  *val, MKL_INT *pntr,  MKL_Complex16 *x, MKL_Complex16 *y);

void MKL_ZBSRMV (char *transa, MKL_INT *m, MKL_INT *k, MKL_INT *lb, MKL_Complex16 *alpha, char *matdescra, MKL_Complex16  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, MKL_Complex16 *x, MKL_Complex16 *beta, MKL_Complex16 *y);
void MKL_ZBSRSV(char *transa, MKL_INT *m, MKL_INT *lb, MKL_Complex16 *alpha, char *matdescra, MKL_Complex16  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, MKL_Complex16 *x, MKL_Complex16 *y);
void MKL_ZBSRGEMV(char *transa, MKL_INT *m, MKL_INT *lb, MKL_Complex16 *a, MKL_INT *ia,  MKL_INT *ja, MKL_Complex16 *x,  MKL_Complex16 *y);
void MKL_CSPBLAS_ZBSRGEMV(char *transa, MKL_INT *m, MKL_INT *lb, MKL_Complex16 *a, MKL_INT *ia,  MKL_INT *ja, MKL_Complex16 *x,  MKL_Complex16 *y);
void MKL_ZBSRSYMV(char *uplo, MKL_INT *m, MKL_INT *lb, MKL_Complex16 *a, MKL_INT *ia,  MKL_INT *ja, MKL_Complex16 *x,  MKL_Complex16 *y);
void MKL_CSPBLAS_ZBSRSYMV(char *uplo, MKL_INT *m, MKL_INT *lb, MKL_Complex16 *a, MKL_INT *ia,  MKL_INT *ja, MKL_Complex16 *x,  MKL_Complex16 *y);
void MKL_ZBSRTRSV(char *uplo, char *transa, char *diag, MKL_INT *m, MKL_INT *lb, MKL_Complex16 *a, MKL_INT *ia,  MKL_INT *ja, MKL_Complex16 *x,  MKL_Complex16 *y);
void MKL_CSPBLAS_ZBSRTRSV(char *uplo, char *transa, char *diag, MKL_INT *m, MKL_INT *lb, MKL_Complex16 *a, MKL_INT *ia,  MKL_INT *ja, MKL_Complex16 *x,  MKL_Complex16 *y);

/* Sparse BLAS Level3 upper case */

void MKL_ZCSRMM(char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex16 *alpha, char *matdescra, MKL_Complex16  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, MKL_Complex16 *b, MKL_INT *ldb, MKL_Complex16 *beta, MKL_Complex16 *c, MKL_INT *ldc);
void MKL_ZCSRSM(char *transa, MKL_INT *m, MKL_INT *n, MKL_Complex16 *alpha, char *matdescra, MKL_Complex16  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, MKL_Complex16 *b, MKL_INT *ldb,  MKL_Complex16 *c, MKL_INT *ldc);

void MKL_ZCSCMM(char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex16 *alpha, char *matdescra, MKL_Complex16  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, MKL_Complex16 *b, MKL_INT *ldb, MKL_Complex16 *beta, MKL_Complex16 *c, MKL_INT *ldc);
void MKL_ZCSCSM(char *transa, MKL_INT *m, MKL_INT *n, MKL_Complex16 *alpha, char *matdescra, MKL_Complex16  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, MKL_Complex16 *b, MKL_INT *ldb,  MKL_Complex16 *c, MKL_INT *ldc);

void MKL_ZCOOMM(char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex16 *alpha, char *matdescra, MKL_Complex16  *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, MKL_Complex16 *b, MKL_INT *ldb, MKL_Complex16 *beta, MKL_Complex16 *c, MKL_INT *ldc);
void MKL_ZCOOSM(char *transa, MKL_INT *m, MKL_INT *n, MKL_Complex16 *alpha, char *matdescra, MKL_Complex16  *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, MKL_Complex16 *b, MKL_INT *ldb,  MKL_Complex16 *c, MKL_INT *ldc);

void MKL_ZDIAMM (char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex16 *alpha, char *matdescra, MKL_Complex16  *val, MKL_INT *lval, MKL_INT *idiag,  MKL_INT *ndiag, MKL_Complex16 *b, MKL_INT *ldb, MKL_Complex16 *beta, MKL_Complex16 *c, MKL_INT *ldc);
void MKL_ZDIASM (char *transa, MKL_INT *m, MKL_INT *n, MKL_Complex16 *alpha, char *matdescra, MKL_Complex16  *val, MKL_INT *lval, MKL_INT *idiag,  MKL_INT *ndiag, MKL_Complex16 *b, MKL_INT *ldb, MKL_Complex16 *c, MKL_INT *ldc);

void MKL_ZSKYSM (char *transa, MKL_INT *m, MKL_INT *n, MKL_Complex16 *alpha, char *matdescra, MKL_Complex16  *val, MKL_INT *pntr,  MKL_Complex16 *b, MKL_INT *ldb, MKL_Complex16 *c, MKL_INT *ldc);
void MKL_ZSKYMM (char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex16 *alpha, char *matdescra, MKL_Complex16  *val, MKL_INT *pntr, MKL_Complex16 *b, MKL_INT *ldb, MKL_Complex16 *beta, MKL_Complex16 *c, MKL_INT *ldc);

void MKL_ZBSRMM(char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_INT *lb, MKL_Complex16 *alpha, char *matdescra, MKL_Complex16  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, MKL_Complex16 *b, MKL_INT *ldb, MKL_Complex16 *beta, MKL_Complex16 *c, MKL_INT *ldc);
void MKL_ZBSRSM(char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *lb, MKL_Complex16 *alpha, char *matdescra, MKL_Complex16  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, MKL_Complex16 *b, MKL_INT *ldb,  MKL_Complex16 *c, MKL_INT *ldc);

/* Converters lower case*/

void mkl_dcsrbsr(MKL_INT * job,MKL_INT * m,MKL_INT * mblk,MKL_INT * ldAbsr,double *Acsr,MKL_INT * AJ,MKL_INT * AI,double *Absr, MKL_INT * AJB, MKL_INT * AIB, MKL_INT * info);
void mkl_dcsrcoo(MKL_INT * job,MKL_INT * n,double *Acsr,MKL_INT * AJR,MKL_INT * AIR,MKL_INT * nnz,double *Acoo, MKL_INT * ir, MKL_INT * jc, MKL_INT * info);
void mkl_ddnscsr(MKL_INT *job,MKL_INT *m,MKL_INT *n,double *Adns,MKL_INT *lda,double *Acsr,MKL_INT *AJ,MKL_INT *AI,MKL_INT *info);
void mkl_dcsrcsc(MKL_INT * job,MKL_INT * n,double *Acsr,MKL_INT * AJ0,MKL_INT * AI0,double *Acsc,MKL_INT * AJ1,MKL_INT * AI1,MKL_INT * info);
void mkl_dcsrdia(MKL_INT * job,MKL_INT * n,double *Acsr,MKL_INT * AJ0,MKL_INT * AI0,double *Adia,MKL_INT * ndiag,MKL_INT * distance,MKL_INT * idiag,double *Acsr_rem,MKL_INT * AJ0_rem,MKL_INT * AI0_rem,MKL_INT * info);
void mkl_dcsrsky(MKL_INT * job,MKL_INT * n,double *Acsr,MKL_INT * AJ0,MKL_INT * AI0, double *Asky,MKL_INT * pointers,MKL_INT * info);

/* Converters upper case*/

void MKL_DCSRBSR(MKL_INT * job,MKL_INT * m,MKL_INT * mblk,MKL_INT * ldAbsr,double *Acsr,MKL_INT * AJ,MKL_INT * AI,double *Absr, MKL_INT * AJB, MKL_INT * AIB, MKL_INT * info);
void MKL_DCSRCOO(MKL_INT * job,MKL_INT * n,double *Acsr,MKL_INT * AJR,MKL_INT * AIR,MKL_INT * nnz,double *Acoo, MKL_INT * ir, MKL_INT * jc, MKL_INT * info);
void MKL_DDNSCSR(MKL_INT *job,MKL_INT *m,MKL_INT *n,double *Adns,MKL_INT *lda,double *Acsr,MKL_INT *AJ,MKL_INT *AI,MKL_INT *info);
void MKL_DCSRCSC(MKL_INT * job,MKL_INT * n,double *Acsr,MKL_INT * AJ0,MKL_INT * AI0,double *Acsc,MKL_INT * AJ1,MKL_INT * AI1,MKL_INT * info);
void MKL_DCSRDIA(MKL_INT * job,MKL_INT * n,double *Acsr,MKL_INT * AJ0,MKL_INT * AI0,double *Adia,MKL_INT * ndiag,MKL_INT * distance,MKL_INT * idiag,double *Acsr_rem,MKL_INT * AJ0_rem,MKL_INT * AI0_rem,MKL_INT * info);
void MKL_DCSRSKY(MKL_INT * job,MKL_INT * n,double *Acsr,MKL_INT * AJ0,MKL_INT * AI0, double *Asky,MKL_INT * pointers,MKL_INT * info);

/* Sparse BLAS Level2 (CSR-CSR) lower case */
void mkl_dcsrmultcsr(char *transa, MKL_INT *job, MKL_INT *sort, MKL_INT *n, MKL_INT *k, MKL_INT *m, double *a, MKL_INT *ja, MKL_INT *ia, double *b, MKL_INT *jb, MKL_INT *ib, double *c, MKL_INT *jc, MKL_INT *ic, MKL_INT *nnzmax, MKL_INT *ierr);
void mkl_dcsrmultd(char *transa,  MKL_INT *n, MKL_INT *k, MKL_INT *m, double *a, MKL_INT *ja, MKL_INT *ia, double *b, MKL_INT *jb, MKL_INT *ib, double *c, MKL_INT *ldc);
void mkl_dcsradd(char *transa, MKL_INT *job, MKL_INT *sort, MKL_INT *n, MKL_INT *k, double *a, MKL_INT *ja, MKL_INT *ia, double *beta, double *b, MKL_INT *jb, MKL_INT *ib, double *c, MKL_INT *jc, MKL_INT *ic, MKL_INT *nnzmax, MKL_INT *ierr);

void mkl_scsrmultcsr(char *transa, MKL_INT *job, MKL_INT *sort, MKL_INT *n, MKL_INT *k, MKL_INT *m, float *a, MKL_INT *ja, MKL_INT *ia, float *b, MKL_INT *jb, MKL_INT *ib, float *c, MKL_INT *jc, MKL_INT *ic, MKL_INT *nnzmax, MKL_INT *ierr);
void mkl_scsrmultd(char *transa,  MKL_INT *n, MKL_INT *k, MKL_INT *m, float *a, MKL_INT *ja, MKL_INT *ia, float *b, MKL_INT *jb, MKL_INT *ib, float *c, MKL_INT *ldc);
void mkl_scsradd(char *transa, MKL_INT *job, MKL_INT *sort, MKL_INT *n, MKL_INT *k, float *a, MKL_INT *ja, MKL_INT *ia, float *beta, float *b, MKL_INT *jb, MKL_INT *ib, float *c, MKL_INT *jc, MKL_INT *ic, MKL_INT *nnzmax, MKL_INT *ierr);

void mkl_ccsrmultcsr(char *transa, MKL_INT *job, MKL_INT *sort, MKL_INT *n, MKL_INT *k, MKL_INT *m, MKL_Complex8 *a, MKL_INT *ja, MKL_INT *ia, MKL_Complex8 *b, MKL_INT *jb, MKL_INT *ib, MKL_Complex8 *c, MKL_INT *jc, MKL_INT *ic, MKL_INT *nnzmax, MKL_INT *ierr);
void mkl_ccsrmultd(char *transa,  MKL_INT *n, MKL_INT *k, MKL_INT *m, MKL_Complex8 *a, MKL_INT *ja, MKL_INT *ia, MKL_Complex8 *b, MKL_INT *jb, MKL_INT *ib, MKL_Complex8 *c, MKL_INT *ldc);
void mkl_ccsradd(char *transa, MKL_INT *job, MKL_INT *sort, MKL_INT *n, MKL_INT *k, MKL_Complex8 *a, MKL_INT *ja, MKL_INT *ia, MKL_Complex8 *beta, MKL_Complex8 *b, MKL_INT *jb, MKL_INT *ib, MKL_Complex8 *c, MKL_INT *jc, MKL_INT *ic, MKL_INT *nnzmax, MKL_INT *ierr);

void mkl_zcsrmultcsr(char *transa, MKL_INT *job, MKL_INT *sort, MKL_INT *n, MKL_INT *k, MKL_INT *m, MKL_Complex16 *a, MKL_INT *ja, MKL_INT *ia, MKL_Complex16 *b, MKL_INT *jb, MKL_INT *ib, MKL_Complex16 *c, MKL_INT *jc, MKL_INT *ic, MKL_INT *nnzmax, MKL_INT *ierr);
void mkl_zcsrmultd(char *transa,  MKL_INT *n, MKL_INT *k, MKL_INT *m, MKL_Complex16 *a, MKL_INT *ja, MKL_INT *ia, MKL_Complex16 *b, MKL_INT *jb, MKL_INT *ib, MKL_Complex16 *c, MKL_INT *ldc);
void mkl_zcsradd(char *transa, MKL_INT *job, MKL_INT *sort, MKL_INT *n, MKL_INT *k, MKL_Complex16 *a, MKL_INT *ja, MKL_INT *ia, MKL_Complex16 *beta, MKL_Complex16 *b, MKL_INT *jb, MKL_INT *ib, MKL_Complex16 *c, MKL_INT *jc, MKL_INT *ic, MKL_INT *nnzmax, MKL_INT *ierr);


/* Sparse BLAS Level2 (CSR-CSR) upper case */
void MKL_DCSRMULTCSR(char *transa, MKL_INT *job, MKL_INT *sort, MKL_INT *n, MKL_INT *k, MKL_INT *m, double *a, MKL_INT *ja, MKL_INT *ia, double *b, MKL_INT *jb, MKL_INT *ib, double *c, MKL_INT *jc, MKL_INT *ic, MKL_INT *nnzmax, MKL_INT *ierr);
void MKL_DCSRMULTD(char *transa,  MKL_INT *n, MKL_INT *k, MKL_INT *m, double *a, MKL_INT *ja, MKL_INT *ia, double *b, MKL_INT *jb, MKL_INT *ib, double *c, MKL_INT *ldc);
void MKL_DCSRADD(char *transa, MKL_INT *job, MKL_INT *sort, MKL_INT *n, MKL_INT *k, double *a, MKL_INT *ja, MKL_INT *ia, double *beta, double *b, MKL_INT *jb, MKL_INT *ib, double *c, MKL_INT *jc, MKL_INT *ic, MKL_INT *nnzmax, MKL_INT *ierr);

void MKL_SCSRMULTCSR(char *transa, MKL_INT *job, MKL_INT *sort, MKL_INT *n, MKL_INT *k, MKL_INT *m, float *a, MKL_INT *ja, MKL_INT *ia, float *b, MKL_INT *jb, MKL_INT *ib, float *c, MKL_INT *jc, MKL_INT *ic, MKL_INT *nnzmax, MKL_INT *ierr);
void MKL_SCSRMULTD(char *transa,  MKL_INT *n, MKL_INT *k, MKL_INT *m, float *a, MKL_INT *ja, MKL_INT *ia, float *b, MKL_INT *jb, MKL_INT *ib, float *c, MKL_INT *ldc);
void MKL_SCSRADD(char *transa, MKL_INT *job, MKL_INT *sort, MKL_INT *n, MKL_INT *k, float *a, MKL_INT *ja, MKL_INT *ia, float *beta, float *b, MKL_INT *jb, MKL_INT *ib, float *c, MKL_INT *jc, MKL_INT *ic, MKL_INT *nnzmax, MKL_INT *ierr);

void MKL_CCSRMULTCSR(char *transa, MKL_INT *job, MKL_INT *sort, MKL_INT *n, MKL_INT *k, MKL_INT *m, MKL_Complex8 *a, MKL_INT *ja, MKL_INT *ia, MKL_Complex8 *b, MKL_INT *jb, MKL_INT *ib, MKL_Complex8 *c, MKL_INT *jc, MKL_INT *ic, MKL_INT *nnzmax, MKL_INT *ierr);
void MKL_CCSRMULTD(char *transa,  MKL_INT *n, MKL_INT *k, MKL_INT *m, MKL_Complex8 *a, MKL_INT *ja, MKL_INT *ia, MKL_Complex8 *b, MKL_INT *jb, MKL_INT *ib, MKL_Complex8 *c, MKL_INT *ldc);
void MKL_CCSRADD(char *transa, MKL_INT *job, MKL_INT *sort, MKL_INT *n, MKL_INT *k, MKL_Complex8 *a, MKL_INT *ja, MKL_INT *ia, MKL_Complex8 *beta, MKL_Complex8 *b, MKL_INT *jb, MKL_INT *ib, MKL_Complex8 *c, MKL_INT *jc, MKL_INT *ic, MKL_INT *nnzmax, MKL_INT *ierr);

void MKL_ZCSRMULTCSR(char *transa, MKL_INT *job, MKL_INT *sort, MKL_INT *n, MKL_INT *k, MKL_INT *m, MKL_Complex16 *a, MKL_INT *ja, MKL_INT *ia, MKL_Complex16 *b, MKL_INT *jb, MKL_INT *ib, MKL_Complex16 *c, MKL_INT *jc, MKL_INT *ic, MKL_INT *nnzmax, MKL_INT *ierr);
void MKL_ZCSRMULTD(char *transa,  MKL_INT *n, MKL_INT *k, MKL_INT *m, MKL_Complex16 *a, MKL_INT *ja, MKL_INT *ia, MKL_Complex16 *b, MKL_INT *jb, MKL_INT *ib, MKL_Complex16 *c, MKL_INT *ldc);
void MKL_ZCSRADD(char *transa, MKL_INT *job, MKL_INT *sort, MKL_INT *n, MKL_INT *k, MKL_Complex16 *a, MKL_INT *ja, MKL_INT *ia, MKL_Complex16 *beta, MKL_Complex16 *b, MKL_INT *jb, MKL_INT *ib, MKL_Complex16 *c, MKL_INT *jc, MKL_INT *ic, MKL_INT *nnzmax, MKL_INT *ierr);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* _MKL_SPBLAS_H_ */
