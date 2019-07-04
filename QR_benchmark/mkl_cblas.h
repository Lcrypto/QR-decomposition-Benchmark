/*******************************************************************************
!                             INTEL CONFIDENTIAL
!   Copyright(C) 1999-2010 Intel Corporation. All Rights Reserved.
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
!      Intel(R) Math Kernel Library (MKL) CBLAS interface
!******************************************************************************/

#ifndef __MKL_CBLAS_H__
#define __MKL_CBLAS_H__
#include <stddef.h>

#include "mkl_types.h"

#ifdef __cplusplus
extern "C" {            /* Assume C declarations for C++ */
#endif /* __cplusplus */


/*
 * Enumerated and derived types
 */
#define CBLAS_INDEX size_t  /* this may vary between platforms */

typedef enum {CblasRowMajor=101, CblasColMajor=102} CBLAS_ORDER;
typedef enum {CblasNoTrans=111, CblasTrans=112, CblasConjTrans=113} CBLAS_TRANSPOSE;
typedef enum {CblasUpper=121, CblasLower=122} CBLAS_UPLO;
typedef enum {CblasNonUnit=131, CblasUnit=132} CBLAS_DIAG;
typedef enum {CblasLeft=141, CblasRight=142} CBLAS_SIDE;

/*
 * ===========================================================================
 * Prototypes for level 1 BLAS functions (complex are recast as routines)
 * ===========================================================================
 */


double cblas_dcabs1(const void  *z);
float  cblas_scabs1(const void  *c);

float  cblas_sdot(const MKL_INT N, const float  *X, const MKL_INT incX,
                  const float  *Y, const MKL_INT incY);
float  cblas_sdoti(const MKL_INT N, const float *X, const MKL_INT *indx,
                   const float *Y);
double cblas_ddot(const MKL_INT N, const double *X, const MKL_INT incX,
                  const double *Y, const MKL_INT incY);
double cblas_ddoti(const MKL_INT N, const double *X, const MKL_INT *indx,
                   const double *Y);


double cblas_dsdot(const MKL_INT N, const float  *X, const MKL_INT incX,
                  const float  *Y, const MKL_INT incY);

float  cblas_sdsdot(const MKL_INT N, const float sb, const float  *X,
		    const MKL_INT incX, const float  *Y, const MKL_INT incY);

/*
 * Functions having prefixes Z and C only
 */
void   cblas_cdotu_sub(const MKL_INT N, const void *X, const MKL_INT incX,
                       const void *Y, const MKL_INT incY, void *dotu);
void   cblas_cdotui_sub(const MKL_INT N, const void *X, const MKL_INT *indx,
                        const void *Y, void *dotui);
void   cblas_cdotc_sub(const MKL_INT N, const void *X, const MKL_INT incX,
                       const void *Y, const MKL_INT incY, void *dotc);
void   cblas_cdotci_sub(const MKL_INT N, const void *X, const MKL_INT *indx,
                        const void *Y, void *dotui);

void   cblas_zdotu_sub(const MKL_INT N, const void *X, const MKL_INT incX,
                       const void *Y, const MKL_INT incY, void *dotu);
void   cblas_zdotui_sub(const MKL_INT N, const void *X, const MKL_INT *indx,
                        const void *Y, void *dotui);
void   cblas_zdotc_sub(const MKL_INT N, const void *X, const MKL_INT incX,
                       const void *Y, const MKL_INT incY, void *dotc);
void   cblas_zdotci_sub(const MKL_INT N, const void *X, const MKL_INT *indx,
                        const void *Y, void *dotui);

/*
 * Functions having prefixes S D SC DZ
 */
float  cblas_snrm2(const MKL_INT N, const float *X, const MKL_INT incX);
float  cblas_sasum(const MKL_INT N, const float *X, const MKL_INT incX);

double cblas_dnrm2(const MKL_INT N, const double *X, const MKL_INT incX);
double cblas_dasum(const MKL_INT N, const double *X, const MKL_INT incX);

float  cblas_scnrm2(const MKL_INT N, const void *X, const MKL_INT incX);
float  cblas_scasum(const MKL_INT N, const void *X, const MKL_INT incX);

double cblas_dznrm2(const MKL_INT N, const void *X, const MKL_INT incX);
double cblas_dzasum(const MKL_INT N, const void *X, const MKL_INT incX);


/*
 * Functions having standard 4 prefixes (S D C Z)
 */
CBLAS_INDEX cblas_isamax(const MKL_INT N, const float  *X, const MKL_INT incX);
CBLAS_INDEX cblas_idamax(const MKL_INT N, const double *X, const MKL_INT incX);
CBLAS_INDEX cblas_icamax(const MKL_INT N, const void   *X, const MKL_INT incX);
CBLAS_INDEX cblas_izamax(const MKL_INT N, const void   *X, const MKL_INT incX);
CBLAS_INDEX cblas_isamin(const MKL_INT N, const float  *X, const MKL_INT incX);
CBLAS_INDEX cblas_idamin(const MKL_INT N, const double *X, const MKL_INT incX);
CBLAS_INDEX cblas_icamin(const MKL_INT N, const void   *X, const MKL_INT incX);
CBLAS_INDEX cblas_izamin(const MKL_INT N, const void   *X, const MKL_INT incX);

/*
 * ===========================================================================
 * Prototypes for level 1 BLAS routines
 * ===========================================================================
 */

/*
 * Routines with standard 4 prefixes (s, d, c, z)
 */
void cblas_sswap(const MKL_INT N, float *X, const MKL_INT incX,
                 float *Y, const MKL_INT incY);
void cblas_scopy(const MKL_INT N, const float *X, const MKL_INT incX,
                 float *Y, const MKL_INT incY);
void cblas_saxpy(const MKL_INT N, const float alpha, const float *X,
                 const MKL_INT incX, float *Y, const MKL_INT incY);
void cblas_saxpyi(const MKL_INT N, const float alpha, const float *X,
                 const MKL_INT *indx, float *Y);
void cblas_sgthr(const MKL_INT N, const float *Y, float *X,
                                 const MKL_INT *indx);
void cblas_sgthrz(const MKL_INT N, float *Y, float *X,
                                 const MKL_INT *indx);
void cblas_ssctr(const MKL_INT N, const float *X, const MKL_INT *indx,
                                 float *Y);
void cblas_srotg(float *a, float *b, float *c, float *s);

void cblas_dswap(const MKL_INT N, double *X, const MKL_INT incX,
                 double *Y, const MKL_INT incY);
void cblas_dcopy(const MKL_INT N, const double *X, const MKL_INT incX,
                 double *Y, const MKL_INT incY);
void cblas_daxpy(const MKL_INT N, const double alpha, const double *X,
                 const MKL_INT incX, double *Y, const MKL_INT incY);
void cblas_daxpyi(const MKL_INT N, const double alpha, const double *X,
                 const MKL_INT *indx, double *Y);
void cblas_dgthr(const MKL_INT N, const double *Y, double *X,
                                 const MKL_INT *indx);
void cblas_dgthrz(const MKL_INT N, double *Y, double *X,
                                 const MKL_INT *indx);
void cblas_dsctr(const MKL_INT N, const double *X, const MKL_INT *indx,
                                 double *Y);
void cblas_drotg(double *a, double *b, double *c, double *s);

void cblas_cswap(const MKL_INT N, void *X, const MKL_INT incX,
                 void *Y, const MKL_INT incY);
void cblas_ccopy(const MKL_INT N, const void *X, const MKL_INT incX,
                 void *Y, const MKL_INT incY);
void cblas_caxpy(const MKL_INT N, const void *alpha, const void *X,
                 const MKL_INT incX, void *Y, const MKL_INT incY);
void cblas_caxpyi(const MKL_INT N, const void *alpha, const void *X,
                 const MKL_INT *indx, void *Y);
void cblas_cgthr(const MKL_INT N, const void *Y, void *X,
                                 const MKL_INT *indx);
void cblas_cgthrz(const MKL_INT N, void *Y, void *X,
                                 const MKL_INT *indx);
void cblas_csctr(const MKL_INT N, const void *X, const MKL_INT *indx,
                                 void *Y);
void cblas_crotg(void *a, const void *b, float *c, void *s);

void cblas_zswap(const MKL_INT N, void *X, const MKL_INT incX,
                 void *Y, const MKL_INT incY);
void cblas_zcopy(const MKL_INT N, const void *X, const MKL_INT incX,
                 void *Y, const MKL_INT incY);
void cblas_zaxpy(const MKL_INT N, const void *alpha, const void *X,
                 const MKL_INT incX, void *Y, const MKL_INT incY);
void cblas_zaxpyi(const MKL_INT N, const void *alpha, const void *X,
                 const MKL_INT *indx, void *Y);
void cblas_zgthr(const MKL_INT N, const void *Y, void *X,
                                 const MKL_INT *indx);
void cblas_zgthrz(const MKL_INT N, void *Y, void *X,
                                 const MKL_INT *indx);
void cblas_zsctr(const MKL_INT N, const void *X, const MKL_INT *indx,
                                 void *Y);
void cblas_zrotg(void *a, const void *b, double *c, void *s);

/*
 * Routines with S and D prefix only
 */
void cblas_srotmg(float *d1, float *d2, float *b1, const float b2, float *P);
void cblas_srot(const MKL_INT N, float *X, const MKL_INT incX,
                float *Y, const MKL_INT incY, const float c, const float s);
void cblas_sroti(const MKL_INT N, float *X, const MKL_INT *indx,
                float *Y, const float c, const float s);
void cblas_srotm(const MKL_INT N, float *X, const MKL_INT incX,
                float *Y, const MKL_INT incY, const float *P);

void cblas_drotmg(double *d1, double *d2, double *b1, const double b2, double *P);
void cblas_drot(const MKL_INT N, double *X, const MKL_INT incX,
                double *Y, const MKL_INT incY, const double c, const double  s);
void cblas_drotm(const MKL_INT N, double *X, const MKL_INT incX,
                double *Y, const MKL_INT incY, const double *P);
void cblas_droti(const MKL_INT N, double *X, const MKL_INT *indx,
                double *Y, const double c, const double s);

/*
 * Routines with CS and ZD prefix only
 */
void cblas_csrot(const MKL_INT N, void *X, const MKL_INT incX,
                 void *Y, const MKL_INT incY, const float c, const float s);
void cblas_zdrot(const MKL_INT N, void *X, const MKL_INT incX,
                 void *Y, const MKL_INT incY, const double c, const double  s);

/*
 * Routines with S D C Z CS and ZD prefixes
 */
void cblas_sscal(const MKL_INT N, const float alpha, float *X, const MKL_INT incX);
void cblas_dscal(const MKL_INT N, const double alpha, double *X, const MKL_INT incX);
void cblas_cscal(const MKL_INT N, const void *alpha, void *X, const MKL_INT incX);
void cblas_zscal(const MKL_INT N, const void *alpha, void *X, const MKL_INT incX);
void cblas_csscal(const MKL_INT N, const float alpha, void *X, const MKL_INT incX);
void cblas_zdscal(const MKL_INT N, const double alpha, void *X, const MKL_INT incX);

/*
 * ===========================================================================
 * Prototypes for level 2 BLAS
 * ===========================================================================
 */

/*
 * Routines with standard 4 prefixes (S, D, C, Z)
 */
void cblas_sgemv(const  CBLAS_ORDER order,
                 const  CBLAS_TRANSPOSE TransA, const MKL_INT M, const MKL_INT N,
                 const float alpha, const float *A, const MKL_INT lda,
                 const float *X, const MKL_INT incX, const float beta,
                 float *Y, const MKL_INT incY);
void cblas_sgbmv(const  CBLAS_ORDER order,
                 const  CBLAS_TRANSPOSE TransA, const MKL_INT M, const MKL_INT N,
                 const MKL_INT KL, const MKL_INT KU, const float alpha,
                 const float *A, const MKL_INT lda, const float *X,
                 const MKL_INT incX, const float beta, float *Y, const MKL_INT incY);
void cblas_strmv(const  CBLAS_ORDER order, const  CBLAS_UPLO Uplo,
                 const  CBLAS_TRANSPOSE TransA, const  CBLAS_DIAG Diag,
                 const MKL_INT N, const float *A, const MKL_INT lda,
                 float *X, const MKL_INT incX);
void cblas_stbmv(const  CBLAS_ORDER order, const  CBLAS_UPLO Uplo,
                 const  CBLAS_TRANSPOSE TransA, const  CBLAS_DIAG Diag,
                 const MKL_INT N, const MKL_INT K, const float *A, const MKL_INT lda,
                 float *X, const MKL_INT incX);
void cblas_stpmv(const  CBLAS_ORDER order, const  CBLAS_UPLO Uplo,
                 const  CBLAS_TRANSPOSE TransA, const  CBLAS_DIAG Diag,
                 const MKL_INT N, const float *Ap, float *X, const MKL_INT incX);
void cblas_strsv(const  CBLAS_ORDER order, const  CBLAS_UPLO Uplo,
                 const  CBLAS_TRANSPOSE TransA, const  CBLAS_DIAG Diag,
                 const MKL_INT N, const float *A, const MKL_INT lda, float *X,
                 const MKL_INT incX);
void cblas_stbsv(const  CBLAS_ORDER order, const  CBLAS_UPLO Uplo,
                 const  CBLAS_TRANSPOSE TransA, const  CBLAS_DIAG Diag,
                 const MKL_INT N, const MKL_INT K, const float *A, const MKL_INT lda,
                 float *X, const MKL_INT incX);
void cblas_stpsv(const  CBLAS_ORDER order, const  CBLAS_UPLO Uplo,
                 const  CBLAS_TRANSPOSE TransA, const  CBLAS_DIAG Diag,
                 const MKL_INT N, const float *Ap, float *X, const MKL_INT incX);

void cblas_dgemv(const  CBLAS_ORDER order,
                 const  CBLAS_TRANSPOSE TransA, const MKL_INT M, const MKL_INT N,
                 const double alpha, const double *A, const MKL_INT lda,
                 const double *X, const MKL_INT incX, const double beta,
                 double *Y, const MKL_INT incY);
void cblas_dgbmv(const  CBLAS_ORDER order,
                 const  CBLAS_TRANSPOSE TransA, const MKL_INT M, const MKL_INT N,
                 const MKL_INT KL, const MKL_INT KU, const double alpha,
                 const double *A, const MKL_INT lda, const double *X,
                 const MKL_INT incX, const double beta, double *Y, const MKL_INT incY);
void cblas_dtrmv(const  CBLAS_ORDER order, const  CBLAS_UPLO Uplo,
                 const  CBLAS_TRANSPOSE TransA, const  CBLAS_DIAG Diag,
                 const MKL_INT N, const double *A, const MKL_INT lda,
                 double *X, const MKL_INT incX);
void cblas_dtbmv(const  CBLAS_ORDER order, const  CBLAS_UPLO Uplo,
                 const  CBLAS_TRANSPOSE TransA, const  CBLAS_DIAG Diag,
                 const MKL_INT N, const MKL_INT K, const double *A, const MKL_INT lda,
                 double *X, const MKL_INT incX);
void cblas_dtpmv(const  CBLAS_ORDER order, const  CBLAS_UPLO Uplo,
                 const  CBLAS_TRANSPOSE TransA, const  CBLAS_DIAG Diag,
                 const MKL_INT N, const double *Ap, double *X, const MKL_INT incX);
void cblas_dtrsv(const  CBLAS_ORDER order, const  CBLAS_UPLO Uplo,
                 const  CBLAS_TRANSPOSE TransA, const  CBLAS_DIAG Diag,
                 const MKL_INT N, const double *A, const MKL_INT lda, double *X,
                 const MKL_INT incX);
void cblas_dtbsv(const  CBLAS_ORDER order, const  CBLAS_UPLO Uplo,
                 const  CBLAS_TRANSPOSE TransA, const  CBLAS_DIAG Diag,
                 const MKL_INT N, const MKL_INT K, const double *A, const MKL_INT lda,
                 double *X, const MKL_INT incX);
void cblas_dtpsv(const  CBLAS_ORDER order, const  CBLAS_UPLO Uplo,
                 const  CBLAS_TRANSPOSE TransA, const  CBLAS_DIAG Diag,
                 const MKL_INT N, const double *Ap, double *X, const MKL_INT incX);

void cblas_cgemv(const  CBLAS_ORDER order,
                 const  CBLAS_TRANSPOSE TransA, const MKL_INT M, const MKL_INT N,
                 const void *alpha, const void *A, const MKL_INT lda,
                 const void *X, const MKL_INT incX, const void *beta,
                 void *Y, const MKL_INT incY);
void cblas_cgbmv(const  CBLAS_ORDER order,
                 const  CBLAS_TRANSPOSE TransA, const MKL_INT M, const MKL_INT N,
                 const MKL_INT KL, const MKL_INT KU, const void *alpha,
                 const void *A, const MKL_INT lda, const void *X,
                 const MKL_INT incX, const void *beta, void *Y, const MKL_INT incY);
void cblas_ctrmv(const  CBLAS_ORDER order, const  CBLAS_UPLO Uplo,
                 const  CBLAS_TRANSPOSE TransA, const  CBLAS_DIAG Diag,
                 const MKL_INT N, const void *A, const MKL_INT lda,
                 void *X, const MKL_INT incX);
void cblas_ctbmv(const  CBLAS_ORDER order, const  CBLAS_UPLO Uplo,
                 const  CBLAS_TRANSPOSE TransA, const  CBLAS_DIAG Diag,
                 const MKL_INT N, const MKL_INT K, const void *A, const MKL_INT lda,
                 void *X, const MKL_INT incX);
void cblas_ctpmv(const  CBLAS_ORDER order, const  CBLAS_UPLO Uplo,
                 const  CBLAS_TRANSPOSE TransA, const  CBLAS_DIAG Diag,
                 const MKL_INT N, const void *Ap, void *X, const MKL_INT incX);
void cblas_ctrsv(const  CBLAS_ORDER order, const  CBLAS_UPLO Uplo,
                 const  CBLAS_TRANSPOSE TransA, const  CBLAS_DIAG Diag,
                 const MKL_INT N, const void *A, const MKL_INT lda, void *X,
                 const MKL_INT incX);
void cblas_ctbsv(const  CBLAS_ORDER order, const  CBLAS_UPLO Uplo,
                 const  CBLAS_TRANSPOSE TransA, const  CBLAS_DIAG Diag,
                 const MKL_INT N, const MKL_INT K, const void *A, const MKL_INT lda,
                 void *X, const MKL_INT incX);
void cblas_ctpsv(const  CBLAS_ORDER order, const  CBLAS_UPLO Uplo,
                 const  CBLAS_TRANSPOSE TransA, const  CBLAS_DIAG Diag,
                 const MKL_INT N, const void *Ap, void *X, const MKL_INT incX);

void cblas_zgemv(const  CBLAS_ORDER order,
                 const  CBLAS_TRANSPOSE TransA, const MKL_INT M, const MKL_INT N,
                 const void *alpha, const void *A, const MKL_INT lda,
                 const void *X, const MKL_INT incX, const void *beta,
                 void *Y, const MKL_INT incY);
void cblas_zgbmv(const  CBLAS_ORDER order,
                 const  CBLAS_TRANSPOSE TransA, const MKL_INT M, const MKL_INT N,
                 const MKL_INT KL, const MKL_INT KU, const void *alpha,
                 const void *A, const MKL_INT lda, const void *X,
                 const MKL_INT incX, const void *beta, void *Y, const MKL_INT incY);
void cblas_ztrmv(const  CBLAS_ORDER order, const  CBLAS_UPLO Uplo,
                 const  CBLAS_TRANSPOSE TransA, const  CBLAS_DIAG Diag,
                 const MKL_INT N, const void *A, const MKL_INT lda,
                 void *X, const MKL_INT incX);
void cblas_ztbmv(const  CBLAS_ORDER order, const  CBLAS_UPLO Uplo,
                 const  CBLAS_TRANSPOSE TransA, const  CBLAS_DIAG Diag,
                 const MKL_INT N, const MKL_INT K, const void *A, const MKL_INT lda,
                 void *X, const MKL_INT incX);
void cblas_ztpmv(const  CBLAS_ORDER order, const  CBLAS_UPLO Uplo,
                 const  CBLAS_TRANSPOSE TransA, const  CBLAS_DIAG Diag,
                 const MKL_INT N, const void *Ap, void *X, const MKL_INT incX);
void cblas_ztrsv(const  CBLAS_ORDER order, const  CBLAS_UPLO Uplo,
                 const  CBLAS_TRANSPOSE TransA, const  CBLAS_DIAG Diag,
                 const MKL_INT N, const void *A, const MKL_INT lda, void *X,
                 const MKL_INT incX);
void cblas_ztbsv(const  CBLAS_ORDER order, const  CBLAS_UPLO Uplo,
                 const  CBLAS_TRANSPOSE TransA, const  CBLAS_DIAG Diag,
                 const MKL_INT N, const MKL_INT K, const void *A, const MKL_INT lda,
                 void *X, const MKL_INT incX);
void cblas_ztpsv(const  CBLAS_ORDER order, const  CBLAS_UPLO Uplo,
                 const  CBLAS_TRANSPOSE TransA, const  CBLAS_DIAG Diag,
                 const MKL_INT N, const void *Ap, void *X, const MKL_INT incX);


/*
 * Routines with S and D prefixes only
 */
void cblas_ssymv(const  CBLAS_ORDER order, const  CBLAS_UPLO Uplo,
                 const MKL_INT N, const float alpha, const float *A,
                 const MKL_INT lda, const float *X, const MKL_INT incX,
                 const float beta, float *Y, const MKL_INT incY);
void cblas_ssbmv(const  CBLAS_ORDER order, const  CBLAS_UPLO Uplo,
                 const MKL_INT N, const MKL_INT K, const float alpha, const float *A,
                 const MKL_INT lda, const float *X, const MKL_INT incX,
                 const float beta, float *Y, const MKL_INT incY);
void cblas_sspmv(const  CBLAS_ORDER order, const  CBLAS_UPLO Uplo,
                 const MKL_INT N, const float alpha, const float *Ap,
                 const float *X, const MKL_INT incX,
                 const float beta, float *Y, const MKL_INT incY);
void cblas_sger(const  CBLAS_ORDER order, const MKL_INT M, const MKL_INT N,
                const float alpha, const float *X, const MKL_INT incX,
                const float *Y, const MKL_INT incY, float *A, const MKL_INT lda);
void cblas_ssyr(const  CBLAS_ORDER order, const  CBLAS_UPLO Uplo,
                const MKL_INT N, const float alpha, const float *X,
                const MKL_INT incX, float *A, const MKL_INT lda);
void cblas_sspr(const  CBLAS_ORDER order, const  CBLAS_UPLO Uplo,
                const MKL_INT N, const float alpha, const float *X,
                const MKL_INT incX, float *Ap);
void cblas_ssyr2(const  CBLAS_ORDER order, const  CBLAS_UPLO Uplo,
                const MKL_INT N, const float alpha, const float *X,
                const MKL_INT incX, const float *Y, const MKL_INT incY, float *A,
                const MKL_INT lda);
void cblas_sspr2(const  CBLAS_ORDER order, const  CBLAS_UPLO Uplo,
                const MKL_INT N, const float alpha, const float *X,
                const MKL_INT incX, const float *Y, const MKL_INT incY, float *A);

void cblas_dsymv(const  CBLAS_ORDER order, const  CBLAS_UPLO Uplo,
                 const MKL_INT N, const double alpha, const double *A,
                 const MKL_INT lda, const double *X, const MKL_INT incX,
                 const double beta, double *Y, const MKL_INT incY);
void cblas_dsbmv(const  CBLAS_ORDER order, const  CBLAS_UPLO Uplo,
                 const MKL_INT N, const MKL_INT K, const double alpha, const double *A,
                 const MKL_INT lda, const double *X, const MKL_INT incX,
                 const double beta, double *Y, const MKL_INT incY);
void cblas_dspmv(const  CBLAS_ORDER order, const  CBLAS_UPLO Uplo,
                 const MKL_INT N, const double alpha, const double *Ap,
                 const double *X, const MKL_INT incX,
                 const double beta, double *Y, const MKL_INT incY);
void cblas_dger(const  CBLAS_ORDER order, const MKL_INT M, const MKL_INT N,
                const double alpha, const double *X, const MKL_INT incX,
                const double *Y, const MKL_INT incY, double *A, const MKL_INT lda);
void cblas_dsyr(const  CBLAS_ORDER order, const  CBLAS_UPLO Uplo,
                const MKL_INT N, const double alpha, const double *X,
                const MKL_INT incX, double *A, const MKL_INT lda);
void cblas_dspr(const  CBLAS_ORDER order, const  CBLAS_UPLO Uplo,
                const MKL_INT N, const double alpha, const double *X,
                const MKL_INT incX, double *Ap);
void cblas_dsyr2(const  CBLAS_ORDER order, const  CBLAS_UPLO Uplo,
                const MKL_INT N, const double alpha, const double *X,
                const MKL_INT incX, const double *Y, const MKL_INT incY, double *A,
                const MKL_INT lda);
void cblas_dspr2(const  CBLAS_ORDER order, const  CBLAS_UPLO Uplo,
                const MKL_INT N, const double alpha, const double *X,
                const MKL_INT incX, const double *Y, const MKL_INT incY, double *A);

/*
 * Routines with C and Z prefixes only
 */
void cblas_chemv(const  CBLAS_ORDER order, const  CBLAS_UPLO Uplo,
                 const MKL_INT N, const void *alpha, const void *A,
                 const MKL_INT lda, const void *X, const MKL_INT incX,
                 const void *beta, void *Y, const MKL_INT incY);
void cblas_chbmv(const  CBLAS_ORDER order, const  CBLAS_UPLO Uplo,
                 const MKL_INT N, const MKL_INT K, const void *alpha, const void *A,
                 const MKL_INT lda, const void *X, const MKL_INT incX,
                 const void *beta, void *Y, const MKL_INT incY);
void cblas_chpmv(const  CBLAS_ORDER order, const  CBLAS_UPLO Uplo,
                 const MKL_INT N, const void *alpha, const void *Ap,
                 const void *X, const MKL_INT incX,
                 const void *beta, void *Y, const MKL_INT incY);
void cblas_cgeru(const  CBLAS_ORDER order, const MKL_INT M, const MKL_INT N,
                 const void *alpha, const void *X, const MKL_INT incX,
                 const void *Y, const MKL_INT incY, void *A, const MKL_INT lda);
void cblas_cgerc(const  CBLAS_ORDER order, const MKL_INT M, const MKL_INT N,
                 const void *alpha, const void *X, const MKL_INT incX,
                 const void *Y, const MKL_INT incY, void *A, const MKL_INT lda);
void cblas_cher(const  CBLAS_ORDER order, const  CBLAS_UPLO Uplo,
                const MKL_INT N, const float alpha, const void *X, const MKL_INT incX,
                void *A, const MKL_INT lda);
void cblas_chpr(const  CBLAS_ORDER order, const  CBLAS_UPLO Uplo,
                const MKL_INT N, const float alpha, const void *X,
                const MKL_INT incX, void *A);
void cblas_cher2(const  CBLAS_ORDER order, const  CBLAS_UPLO Uplo, const MKL_INT N,
                const void *alpha, const void *X, const MKL_INT incX,
                const void *Y, const MKL_INT incY, void *A, const MKL_INT lda);
void cblas_chpr2(const  CBLAS_ORDER order, const  CBLAS_UPLO Uplo, const MKL_INT N,
                const void *alpha, const void *X, const MKL_INT incX,
                const void *Y, const MKL_INT incY, void *Ap);

void cblas_zhemv(const  CBLAS_ORDER order, const  CBLAS_UPLO Uplo,
                 const MKL_INT N, const void *alpha, const void *A,
                 const MKL_INT lda, const void *X, const MKL_INT incX,
                 const void *beta, void *Y, const MKL_INT incY);
void cblas_zhbmv(const  CBLAS_ORDER order, const  CBLAS_UPLO Uplo,
                 const MKL_INT N, const MKL_INT K, const void *alpha, const void *A,
                 const MKL_INT lda, const void *X, const MKL_INT incX,
                 const void *beta, void *Y, const MKL_INT incY);
void cblas_zhpmv(const  CBLAS_ORDER order, const  CBLAS_UPLO Uplo,
                 const MKL_INT N, const void *alpha, const void *Ap,
                 const void *X, const MKL_INT incX,
                 const void *beta, void *Y, const MKL_INT incY);
void cblas_zgeru(const  CBLAS_ORDER order, const MKL_INT M, const MKL_INT N,
                 const void *alpha, const void *X, const MKL_INT incX,
                 const void *Y, const MKL_INT incY, void *A, const MKL_INT lda);
void cblas_zgerc(const  CBLAS_ORDER order, const MKL_INT M, const MKL_INT N,
                 const void *alpha, const void *X, const MKL_INT incX,
                 const void *Y, const MKL_INT incY, void *A, const MKL_INT lda);
void cblas_zher(const  CBLAS_ORDER order, const  CBLAS_UPLO Uplo,
                const MKL_INT N, const double alpha, const void *X, const MKL_INT incX,
                void *A, const MKL_INT lda);
void cblas_zhpr(const  CBLAS_ORDER order, const  CBLAS_UPLO Uplo,
                const MKL_INT N, const double alpha, const void *X,
                const MKL_INT incX, void *A);
void cblas_zher2(const  CBLAS_ORDER order, const  CBLAS_UPLO Uplo, const MKL_INT N,
                const void *alpha, const void *X, const MKL_INT incX,
                const void *Y, const MKL_INT incY, void *A, const MKL_INT lda);
void cblas_zhpr2(const  CBLAS_ORDER order, const  CBLAS_UPLO Uplo, const MKL_INT N,
                const void *alpha, const void *X, const MKL_INT incX,
                const void *Y, const MKL_INT incY, void *Ap);

/*
 * ===========================================================================
 * Prototypes for level 3 BLAS
 * ===========================================================================
 */

/*
 * Routines with standard 4 prefixes (S, D, C, Z)
 */
void cblas_sgemm(const  CBLAS_ORDER Order, const  CBLAS_TRANSPOSE TransA,
                 const  CBLAS_TRANSPOSE TransB, const MKL_INT M, const MKL_INT N,
                 const MKL_INT K, const float alpha, const float *A,
                 const MKL_INT lda, const float *B, const MKL_INT ldb,
                 const float beta, float *C, const MKL_INT ldc);
void cblas_ssymm(const  CBLAS_ORDER Order, const  CBLAS_SIDE Side,
                 const  CBLAS_UPLO Uplo, const MKL_INT M, const MKL_INT N,
                 const float alpha, const float *A, const MKL_INT lda,
                 const float *B, const MKL_INT ldb, const float beta,
                 float *C, const MKL_INT ldc);
void cblas_ssyrk(const  CBLAS_ORDER Order, const  CBLAS_UPLO Uplo,
                 const  CBLAS_TRANSPOSE Trans, const MKL_INT N, const MKL_INT K,
                 const float alpha, const float *A, const MKL_INT lda,
                 const float beta, float *C, const MKL_INT ldc);
void cblas_ssyr2k(const  CBLAS_ORDER Order, const  CBLAS_UPLO Uplo,
                  const  CBLAS_TRANSPOSE Trans, const MKL_INT N, const MKL_INT K,
                  const float alpha, const float *A, const MKL_INT lda,
                  const float *B, const MKL_INT ldb, const float beta,
                  float *C, const MKL_INT ldc);
void cblas_strmm(const  CBLAS_ORDER Order, const  CBLAS_SIDE Side,
                 const  CBLAS_UPLO Uplo, const  CBLAS_TRANSPOSE TransA,
                 const  CBLAS_DIAG Diag, const MKL_INT M, const MKL_INT N,
                 const float alpha, const float *A, const MKL_INT lda,
                 float *B, const MKL_INT ldb);
void cblas_strsm(const  CBLAS_ORDER Order, const  CBLAS_SIDE Side,
                 const  CBLAS_UPLO Uplo, const  CBLAS_TRANSPOSE TransA,
                 const  CBLAS_DIAG Diag, const MKL_INT M, const MKL_INT N,
                 const float alpha, const float *A, const MKL_INT lda,
                 float *B, const MKL_INT ldb);

void cblas_dgemm(const  CBLAS_ORDER Order, const  CBLAS_TRANSPOSE TransA,
                 const  CBLAS_TRANSPOSE TransB, const MKL_INT M, const MKL_INT N,
                 const MKL_INT K, const double alpha, const double *A,
                 const MKL_INT lda, const double *B, const MKL_INT ldb,
                 const double beta, double *C, const MKL_INT ldc);
void cblas_dsymm(const  CBLAS_ORDER Order, const  CBLAS_SIDE Side,
                 const  CBLAS_UPLO Uplo, const MKL_INT M, const MKL_INT N,
                 const double alpha, const double *A, const MKL_INT lda,
                 const double *B, const MKL_INT ldb, const double beta,
                 double *C, const MKL_INT ldc);
void cblas_dsyrk(const  CBLAS_ORDER Order, const  CBLAS_UPLO Uplo,
                 const  CBLAS_TRANSPOSE Trans, const MKL_INT N, const MKL_INT K,
                 const double alpha, const double *A, const MKL_INT lda,
                 const double beta, double *C, const MKL_INT ldc);
void cblas_dsyr2k(const  CBLAS_ORDER Order, const  CBLAS_UPLO Uplo,
                  const  CBLAS_TRANSPOSE Trans, const MKL_INT N, const MKL_INT K,
                  const double alpha, const double *A, const MKL_INT lda,
                  const double *B, const MKL_INT ldb, const double beta,
                  double *C, const MKL_INT ldc);
void cblas_dtrmm(const  CBLAS_ORDER Order, const  CBLAS_SIDE Side,
                 const  CBLAS_UPLO Uplo, const  CBLAS_TRANSPOSE TransA,
                 const  CBLAS_DIAG Diag, const MKL_INT M, const MKL_INT N,
                 const double alpha, const double *A, const MKL_INT lda,
                 double *B, const MKL_INT ldb);
void cblas_dtrsm(const  CBLAS_ORDER Order, const  CBLAS_SIDE Side,
                 const  CBLAS_UPLO Uplo, const  CBLAS_TRANSPOSE TransA,
                 const  CBLAS_DIAG Diag, const MKL_INT M, const MKL_INT N,
                 const double alpha, const double *A, const MKL_INT lda,
                 double *B, const MKL_INT ldb);

void cblas_cgemm(const  CBLAS_ORDER Order, const  CBLAS_TRANSPOSE TransA,
                 const  CBLAS_TRANSPOSE TransB, const MKL_INT M, const MKL_INT N,
                 const MKL_INT K, const void *alpha, const void *A,
                 const MKL_INT lda, const void *B, const MKL_INT ldb,
                 const void *beta, void *C, const MKL_INT ldc);
void cblas_cgemm3m(const  CBLAS_ORDER Order, const  CBLAS_TRANSPOSE TransA,
                 const  CBLAS_TRANSPOSE TransB, const MKL_INT M, const MKL_INT N,
                 const MKL_INT K, const void *alpha, const void *A,
                 const MKL_INT lda, const void *B, const MKL_INT ldb,
                 const void *beta, void *C, const MKL_INT ldc);
void cblas_csymm(const  CBLAS_ORDER Order, const  CBLAS_SIDE Side,
                 const  CBLAS_UPLO Uplo, const MKL_INT M, const MKL_INT N,
                 const void *alpha, const void *A, const MKL_INT lda,
                 const void *B, const MKL_INT ldb, const void *beta,
                 void *C, const MKL_INT ldc);
void cblas_csyrk(const  CBLAS_ORDER Order, const  CBLAS_UPLO Uplo,
                 const  CBLAS_TRANSPOSE Trans, const MKL_INT N, const MKL_INT K,
                 const void *alpha, const void *A, const MKL_INT lda,
                 const void *beta, void *C, const MKL_INT ldc);
void cblas_csyr2k(const  CBLAS_ORDER Order, const  CBLAS_UPLO Uplo,
                  const  CBLAS_TRANSPOSE Trans, const MKL_INT N, const MKL_INT K,
                  const void *alpha, const void *A, const MKL_INT lda,
                  const void *B, const MKL_INT ldb, const void *beta,
                  void *C, const MKL_INT ldc);
void cblas_ctrmm(const  CBLAS_ORDER Order, const  CBLAS_SIDE Side,
                 const  CBLAS_UPLO Uplo, const  CBLAS_TRANSPOSE TransA,
                 const  CBLAS_DIAG Diag, const MKL_INT M, const MKL_INT N,
                 const void *alpha, const void *A, const MKL_INT lda,
                 void *B, const MKL_INT ldb);
void cblas_ctrsm(const  CBLAS_ORDER Order, const  CBLAS_SIDE Side,
                 const  CBLAS_UPLO Uplo, const  CBLAS_TRANSPOSE TransA,
                 const  CBLAS_DIAG Diag, const MKL_INT M, const MKL_INT N,
                 const void *alpha, const void *A, const MKL_INT lda,
                 void *B, const MKL_INT ldb);

void cblas_zgemm(const  CBLAS_ORDER Order, const  CBLAS_TRANSPOSE TransA,
                 const  CBLAS_TRANSPOSE TransB, const MKL_INT M, const MKL_INT N,
                 const MKL_INT K, const void *alpha, const void *A,
                 const MKL_INT lda, const void *B, const MKL_INT ldb,
                 const void *beta, void *C, const MKL_INT ldc);
void cblas_zgemm3m(const  CBLAS_ORDER Order, const  CBLAS_TRANSPOSE TransA,
                 const  CBLAS_TRANSPOSE TransB, const MKL_INT M, const MKL_INT N,
                 const MKL_INT K, const void *alpha, const void *A,
                 const MKL_INT lda, const void *B, const MKL_INT ldb,
                 const void *beta, void *C, const MKL_INT ldc);
void cblas_zsymm(const  CBLAS_ORDER Order, const  CBLAS_SIDE Side,
                 const  CBLAS_UPLO Uplo, const MKL_INT M, const MKL_INT N,
                 const void *alpha, const void *A, const MKL_INT lda,
                 const void *B, const MKL_INT ldb, const void *beta,
                 void *C, const MKL_INT ldc);
void cblas_zsyrk(const  CBLAS_ORDER Order, const  CBLAS_UPLO Uplo,
                 const  CBLAS_TRANSPOSE Trans, const MKL_INT N, const MKL_INT K,
                 const void *alpha, const void *A, const MKL_INT lda,
                 const void *beta, void *C, const MKL_INT ldc);
void cblas_zsyr2k(const  CBLAS_ORDER Order, const  CBLAS_UPLO Uplo,
                  const  CBLAS_TRANSPOSE Trans, const MKL_INT N, const MKL_INT K,
                  const void *alpha, const void *A, const MKL_INT lda,
                  const void *B, const MKL_INT ldb, const void *beta,
                  void *C, const MKL_INT ldc);
void cblas_ztrmm(const  CBLAS_ORDER Order, const  CBLAS_SIDE Side,
                 const  CBLAS_UPLO Uplo, const  CBLAS_TRANSPOSE TransA,
                 const  CBLAS_DIAG Diag, const MKL_INT M, const MKL_INT N,
                 const void *alpha, const void *A, const MKL_INT lda,
                 void *B, const MKL_INT ldb);
void cblas_ztrsm(const  CBLAS_ORDER Order, const  CBLAS_SIDE Side,
                 const  CBLAS_UPLO Uplo, const  CBLAS_TRANSPOSE TransA,
                 const  CBLAS_DIAG Diag, const MKL_INT M, const MKL_INT N,
                 const void *alpha, const void *A, const MKL_INT lda,
                 void *B, const MKL_INT ldb);

/*
 * Routines with prefixes C and Z only
 */
void cblas_chemm(const  CBLAS_ORDER Order, const  CBLAS_SIDE Side,
                 const  CBLAS_UPLO Uplo, const MKL_INT M, const MKL_INT N,
                 const void *alpha, const void *A, const MKL_INT lda,
                 const void *B, const MKL_INT ldb, const void *beta,
                 void *C, const MKL_INT ldc);
void cblas_cherk(const  CBLAS_ORDER Order, const  CBLAS_UPLO Uplo,
                 const  CBLAS_TRANSPOSE Trans, const MKL_INT N, const MKL_INT K,
                 const float alpha, const void *A, const MKL_INT lda,
                 const float beta, void *C, const MKL_INT ldc);
void cblas_cher2k(const  CBLAS_ORDER Order, const  CBLAS_UPLO Uplo,
                  const  CBLAS_TRANSPOSE Trans, const MKL_INT N, const MKL_INT K,
                  const void *alpha, const void *A, const MKL_INT lda,
                  const void *B, const MKL_INT ldb, const float beta,
                  void *C, const MKL_INT ldc);

void cblas_zhemm(const  CBLAS_ORDER Order, const  CBLAS_SIDE Side,
                 const  CBLAS_UPLO Uplo, const MKL_INT M, const MKL_INT N,
                 const void *alpha, const void *A, const MKL_INT lda,
                 const void *B, const MKL_INT ldb, const void *beta,
                 void *C, const MKL_INT ldc);
void cblas_zherk(const  CBLAS_ORDER Order, const  CBLAS_UPLO Uplo,
                 const  CBLAS_TRANSPOSE Trans, const MKL_INT N, const MKL_INT K,
                 const double alpha, const void *A, const MKL_INT lda,
                 const double beta, void *C, const MKL_INT ldc);
void cblas_zher2k(const  CBLAS_ORDER Order, const  CBLAS_UPLO Uplo,
                  const  CBLAS_TRANSPOSE Trans, const MKL_INT N, const MKL_INT K,
                  const void *alpha, const void *A, const MKL_INT lda,
                  const void *B, const MKL_INT ldb, const double beta,
                  void *C, const MKL_INT ldc);

#ifdef __cplusplus
}
#endif    /* __cplusplus */

#endif    /* __MKL_CBLAS_H__ */
