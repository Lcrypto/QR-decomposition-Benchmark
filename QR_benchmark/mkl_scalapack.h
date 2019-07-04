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
!      Intel(R) Math Kernel Library (MKL) interface for SCALAPACK routines
!******************************************************************************/

#ifndef _MKL_SCALAPACK_H_
#define _MKL_SCALAPACK_H_

#include "mkl_types.h"

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/* <NAME> declarations */

void	PSGETRF(MKL_INT *m, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, MKL_INT *info);
void	PDGETRF(MKL_INT *m, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, MKL_INT *info);
void	PCGETRF(MKL_INT *m, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, MKL_INT *info);
void	PZGETRF(MKL_INT *m, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, MKL_INT *info);

void	PSGBTRF(MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, float *a, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, float *af, MKL_INT *laf, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDGBTRF(MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, double *a, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, double *af, MKL_INT *laf, double *work, MKL_INT *lwork, MKL_INT *info);
void	PCGBTRF(MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, MKL_Complex8 *a, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, MKL_Complex8 *af, MKL_INT *laf, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZGBTRF(MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, MKL_Complex16 *a, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, MKL_Complex16 *af, MKL_INT *laf, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSDBTRF(MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, float *a, MKL_INT *ja, MKL_INT *desca, float *af, MKL_INT *laf, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDDBTRF(MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, double *a, MKL_INT *ja, MKL_INT *desca, double *af, MKL_INT *laf, double *work, MKL_INT *lwork, MKL_INT *info);
void	PCDBTRF(MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, MKL_Complex8 *a, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *af, MKL_INT *laf, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZDBTRF(MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, MKL_Complex16 *a, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *af, MKL_INT *laf, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSPOTRF(char *uplo, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *info);
void	PDPOTRF(char *uplo, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *info);
void	PCPOTRF(char *uplo, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *info);
void	PZPOTRF(char *uplo, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *info);

void	PSPBTRF(char *uplo, MKL_INT *n, MKL_INT *bw, float *a, MKL_INT *ja, MKL_INT *desca, float *af, MKL_INT *laf, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDPBTRF(char *uplo, MKL_INT *n, MKL_INT *bw, double *a, MKL_INT *ja, MKL_INT *desca, double *af, MKL_INT *laf, double *work, MKL_INT *lwork, MKL_INT *info);
void	PCPBTRF(char *uplo, MKL_INT *n, MKL_INT *bw, MKL_Complex8 *a, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *af, MKL_INT *laf, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZPBTRF(char *uplo, MKL_INT *n, MKL_INT *bw, MKL_Complex16 *a, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *af, MKL_INT *laf, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSPTTRF(MKL_INT *n, float *d, float *e, MKL_INT *ja, MKL_INT *desca, float *af, MKL_INT *laf, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDPTTRF(MKL_INT *n, double *d, double *e, MKL_INT *ja, MKL_INT *desca, double *af, MKL_INT *laf, double *work, MKL_INT *lwork, MKL_INT *info);
void	PCPTTRF(MKL_INT *n, float *d, MKL_Complex8 *e, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *af, MKL_INT *laf, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZPTTRF(MKL_INT *n, double *d, MKL_Complex16 *e, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *af, MKL_INT *laf, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSDTTRF(MKL_INT *n, float *dl, float *d, float *du, MKL_INT *ja, MKL_INT *desca, float *af, MKL_INT *laf, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDDTTRF(MKL_INT *n, double *dl, double *d, double *du, MKL_INT *ja, MKL_INT *desca, double *af, MKL_INT *laf, double *work, MKL_INT *lwork, MKL_INT *info);
void	PCDTTRF(MKL_INT *n, MKL_Complex8 *dl, MKL_Complex8 *d, MKL_Complex8 *du, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *af, MKL_INT *laf, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZDTTRF(MKL_INT *n, MKL_Complex16 *dl, MKL_Complex16 *d, MKL_Complex16 *du, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *af, MKL_INT *laf, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSGETRS(char *trans, MKL_INT *n, MKL_INT *nrhs, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, float *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_INT *info);
void	PDGETRS(char *trans, MKL_INT *n, MKL_INT *nrhs, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, double *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_INT *info);
void	PCGETRS(char *trans, MKL_INT *n, MKL_INT *nrhs, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_INT *info);
void	PZGETRS(char *trans, MKL_INT *n, MKL_INT *nrhs, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_INT *info);

void	PSGBTRS(char *trans, MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, MKL_INT *nrhs, float *a, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, float *b, MKL_INT *ib, MKL_INT *descb, float *af, MKL_INT *laf, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDGBTRS(char *trans, MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, MKL_INT *nrhs, double *a, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, double *b, MKL_INT *ib, MKL_INT *descb, double *af, MKL_INT *laf, double *work, MKL_INT *lwork, MKL_INT *info);
void	PCGBTRS(char *trans, MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, MKL_INT *nrhs, MKL_Complex8 *a, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex8 *af, MKL_INT *laf, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZGBTRS(char *trans, MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, MKL_INT *nrhs, MKL_Complex16 *a, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex16 *af, MKL_INT *laf, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSPOTRS(char *uplo, MKL_INT *n, MKL_INT *nrhs, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_INT *info);
void	PDPOTRS(char *uplo, MKL_INT *n, MKL_INT *nrhs, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_INT *info);
void	PCPOTRS(char *uplo, MKL_INT *n, MKL_INT *nrhs, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_INT *info);
void	PZPOTRS(char *uplo, MKL_INT *n, MKL_INT *nrhs, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_INT *info);

void	PSPBTRS(char *uplo, MKL_INT *n, MKL_INT *bw, MKL_INT *nrhs, float *a, MKL_INT *ja, MKL_INT *desca, float *b, MKL_INT *ib, MKL_INT *descb, float *af, MKL_INT *laf, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDPBTRS(char *uplo, MKL_INT *n, MKL_INT *bw, MKL_INT *nrhs, double *a, MKL_INT *ja, MKL_INT *desca, double *b, MKL_INT *ib, MKL_INT *descb, double *af, MKL_INT *laf, double *work, MKL_INT *lwork, MKL_INT *info);
void	PCPBTRS(char *uplo, MKL_INT *n, MKL_INT *bw, MKL_INT *nrhs, MKL_Complex8 *a, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex8 *af, MKL_INT *laf, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZPBTRS(char *uplo, MKL_INT *n, MKL_INT *bw, MKL_INT *nrhs, MKL_Complex16 *a, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex16 *af, MKL_INT *laf, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSPTTRS(MKL_INT *n, MKL_INT *nrhs, float *d, float *e, MKL_INT *ja, MKL_INT *desca, float *b, MKL_INT *ib, MKL_INT *descb, float *af, MKL_INT *laf, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDPTTRS(MKL_INT *n, MKL_INT *nrhs, double *d, double *e, MKL_INT *ja, MKL_INT *desca, double *b, MKL_INT *ib, MKL_INT *descb, double *af, MKL_INT *laf, double *work, MKL_INT *lwork, MKL_INT *info);
void	PCPTTRS(char *uplo, MKL_INT *n, MKL_INT *nrhs, float *d, MKL_Complex8 *e, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex8 *af, MKL_INT *laf, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZPTTRS(char *uplo, MKL_INT *n, MKL_INT *nrhs, double *d, MKL_Complex16 *e, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex16 *af, MKL_INT *laf, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSDTTRS(char *trans, MKL_INT *n, MKL_INT *nrhs, float *dl, float *d, float *du, MKL_INT *ja, MKL_INT *desca, float *b, MKL_INT *ib, MKL_INT *descb, float *af, MKL_INT *laf, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDDTTRS(char *trans, MKL_INT *n, MKL_INT *nrhs, double *dl, double *d, double *du, MKL_INT *ja, MKL_INT *desca, double *b, MKL_INT *ib, MKL_INT *descb, double *af, MKL_INT *laf, double *work, MKL_INT *lwork, MKL_INT *info);
void	PCDTTRS(char *trans, MKL_INT *n, MKL_INT *nrhs, MKL_Complex8 *dl, MKL_Complex8 *d, MKL_Complex8 *du, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex8 *af, MKL_INT *laf, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZDTTRS(char *trans, MKL_INT *n, MKL_INT *nrhs, MKL_Complex16 *dl, MKL_Complex16 *d, MKL_Complex16 *du, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex16 *af, MKL_INT *laf, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSDBTRS(char *trans, MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, MKL_INT *nrhs, float *a, MKL_INT *ja, MKL_INT *desca, float *b, MKL_INT *ib, MKL_INT *descb, float *af, MKL_INT *laf, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDDBTRS(char *trans, MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, MKL_INT *nrhs, double *a, MKL_INT *ja, MKL_INT *desca, double *b, MKL_INT *ib, MKL_INT *descb, double *af, MKL_INT *laf, double *work, MKL_INT *lwork, MKL_INT *info);
void	PCDBTRS(char *trans, MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, MKL_INT *nrhs, MKL_Complex8 *a, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex8 *af, MKL_INT *laf, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZDBTRS(char *trans, MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, MKL_INT *nrhs, MKL_Complex16 *a, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex16 *af, MKL_INT *laf, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSTRTRS(char *uplo, char *trans, char *diag, MKL_INT *n, MKL_INT *nrhs, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_INT *info);
void	PDTRTRS(char *uplo, char *trans, char *diag, MKL_INT *n, MKL_INT *nrhs, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_INT *info);
void	PCTRTRS(char *uplo, char *trans, char *diag, MKL_INT *n, MKL_INT *nrhs, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_INT *info);
void	PZTRTRS(char *uplo, char *trans, char *diag, MKL_INT *n, MKL_INT *nrhs, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_INT *info);

void	PSGECON(char *norm, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *anorm, float *rcond, float *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *info);
void	PDGECON(char *norm, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *anorm, double *rcond, double *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *info);
void	PCGECON(char *norm, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *anorm, float *rcond, MKL_Complex8 *work, MKL_INT *lwork, float *rwork, MKL_INT *lrwork, MKL_INT *info);
void	PZGECON(char *norm, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *anorm, double *rcond, MKL_Complex16 *work, MKL_INT *lwork, double *rwork, MKL_INT *lrwork, MKL_INT *info);

void	PSPOCON(char *uplo, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *anorm, float *rcond, float *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *info);
void	PDPOCON(char *uplo, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *anorm, double *rcond, double *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *info);
void	PCPOCON(char *uplo, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *anorm, float *rcond, MKL_Complex8 *work, MKL_INT *lwork, float *rwork, MKL_INT *lrwork, MKL_INT *info);
void	PZPOCON(char *uplo, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *anorm, double *rcond, MKL_Complex16 *work, MKL_INT *lwork, double *rwork, MKL_INT *lrwork, MKL_INT *info);

void	PSTRCON(char *norm, char *uplo, char *diag, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *rcond, float *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *info);
void	PDTRCON(char *norm, char *uplo, char *diag, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *rcond, double *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *info);
void	PCTRCON(char *norm, char *uplo, char *diag, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *rcond, MKL_Complex8 *work, MKL_INT *lwork, float *rwork, MKL_INT *lrwork, MKL_INT *info);
void	PZTRCON(char *norm, char *uplo, char *diag, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *rcond, MKL_Complex16 *work, MKL_INT *lwork, double *rwork, MKL_INT *lrwork, MKL_INT *info);

void	PSGERFS(char *trans, MKL_INT *n, MKL_INT *nrhs, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *af, MKL_INT *iaf, MKL_INT *jaf, MKL_INT *descaf, MKL_INT *ipiv, float *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, float *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, float *ferr, float *berr, float *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *info);
void	PDGERFS(char *trans, MKL_INT *n, MKL_INT *nrhs, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *af, MKL_INT *iaf, MKL_INT *jaf, MKL_INT *descaf, MKL_INT *ipiv, double *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, double *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, double *ferr, double *berr, double *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *info);
void	PCGERFS(char *trans, MKL_INT *n, MKL_INT *nrhs, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *af, MKL_INT *iaf, MKL_INT *jaf, MKL_INT *descaf, MKL_INT *ipiv, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_Complex8 *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, float *ferr, float *berr, MKL_Complex8 *work, MKL_INT *lwork, float *rwork, MKL_INT *lrwork, MKL_INT *info);
void	PZGERFS(char *trans, MKL_INT *n, MKL_INT *nrhs, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *af, MKL_INT *iaf, MKL_INT *jaf, MKL_INT *descaf, MKL_INT *ipiv, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_Complex16 *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, double *ferr, double *berr, MKL_Complex16 *work, MKL_INT *lwork, double *rwork, MKL_INT *lrwork, MKL_INT *info);

void	PSPORFS(char *uplo, MKL_INT *n, MKL_INT *nrhs, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *af, MKL_INT *iaf, MKL_INT *jaf, MKL_INT *descaf, float *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, float *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, float *ferr, float *berr, float *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *info);
void	PDPORFS(char *uplo, MKL_INT *n, MKL_INT *nrhs, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *af, MKL_INT *iaf, MKL_INT *jaf, MKL_INT *descaf, double *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, double *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, double *ferr, double *berr, double *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *info);
void	PCPORFS(char *uplo, MKL_INT *n, MKL_INT *nrhs, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *af, MKL_INT *iaf, MKL_INT *jaf, MKL_INT *descaf, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_Complex8 *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, float *ferr, float *berr, MKL_Complex8 *work, MKL_INT *lwork, float *rwork, MKL_INT *lrwork, MKL_INT *info);
void	PZPORFS(char *uplo, MKL_INT *n, MKL_INT *nrhs, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *af, MKL_INT *iaf, MKL_INT *jaf, MKL_INT *descaf, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_Complex16 *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, double *ferr, double *berr, MKL_Complex16 *work, MKL_INT *lwork, double *rwork, MKL_INT *lrwork, MKL_INT *info);

void	PSTRRFS(char *uplo, char *trans, char *diag, MKL_INT *n, MKL_INT *nrhs, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, float *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, float *ferr, float *berr, float *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *info);
void	PDTRRFS(char *uplo, char *trans, char *diag, MKL_INT *n, MKL_INT *nrhs, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, double *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, double *ferr, double *berr, double *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *info);
void	PCTRRFS(char *uplo, char *trans, char *diag, MKL_INT *n, MKL_INT *nrhs, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_Complex8 *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, float *ferr, float *berr, MKL_Complex8 *work, MKL_INT *lwork, float *rwork, MKL_INT *lrwork, MKL_INT *info);
void	PZTRRFS(char *uplo, char *trans, char *diag, MKL_INT *n, MKL_INT *nrhs, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_Complex16 *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, double *ferr, double *berr, MKL_Complex16 *work, MKL_INT *lwork, double *rwork, MKL_INT *lrwork, MKL_INT *info);

void	PSGETRI(MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, float *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *info);
void	PDGETRI(MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, double *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *info);
void	PCGETRI(MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *info);
void	PZGETRI(MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *info);

void	PSPOTRI(char *uplo, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *info);
void	PDPOTRI(char *uplo, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *info);
void	PCPOTRI(char *uplo, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *info);
void	PZPOTRI(char *uplo, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *info);

void	PSTRTRI(char *uplo, char *diag, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *info);
void	PDTRTRI(char *uplo, char *diag, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *info);
void	PCTRTRI(char *uplo, char *diag, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *info);
void	PZTRTRI(char *uplo, char *diag, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *info);

void	PSGEEQU(MKL_INT *m, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *r, float *c, float *rowcnd, float *colcnd, float *amax, MKL_INT *info);
void	PDGEEQU(MKL_INT *m, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *r, double *c, double *rowcnd, double *colcnd, double *amax, MKL_INT *info);
void	PCGEEQU(MKL_INT *m, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *r, float *c, float *rowcnd, float *colcnd, float *amax, MKL_INT *info);
void	PZGEEQU(MKL_INT *m, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *r, double *c, double *rowcnd, double *colcnd, double *amax, MKL_INT *info);

void	PSPOEQU(MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *sr, float *sc, float *scond, float *amax, MKL_INT *info);
void	PDPOEQU(MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *sr, double *sc, double *scond, double *amax, MKL_INT *info);
void	PCPOEQU(MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *sr, float *sc, float *scond, float *amax, MKL_INT *info);
void	PZPOEQU(MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *sr, double *sc, double *scond, double *amax, MKL_INT *info);

void	PSGEQRF(MKL_INT *m, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDGEQRF(MKL_INT *m, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *work, MKL_INT *lwork, MKL_INT *info);
void	PCGEQRF(MKL_INT *m, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZGEQRF(MKL_INT *m, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSGEQPF(MKL_INT *m, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, float *tau, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDGEQPF(MKL_INT *m, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, double *tau, double *work, MKL_INT *lwork, MKL_INT *info);
void	PCGEQPF(MKL_INT *m, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, MKL_Complex8 *tau, MKL_Complex8 *work, MKL_INT *lwork, float *rwork, MKL_INT *lrwork, MKL_INT *info);
void	PZGEQPF(MKL_INT *m, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, MKL_Complex16 *tau, MKL_Complex16 *work, MKL_INT *lwork, double *rwork, MKL_INT *lrwork, MKL_INT *info);

void	PSORGQR(MKL_INT *m, MKL_INT *n, MKL_INT *k, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDORGQR(MKL_INT *m, MKL_INT *n, MKL_INT *k, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *work, MKL_INT *lwork, MKL_INT *info);

void	PCUNGQR(MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZUNGQR(MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSORMQR(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDORMQR(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, double *work, MKL_INT *lwork, MKL_INT *info);

void	PCUNMQR(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZUNMQR(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSGELQF(MKL_INT *m, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDGELQF(MKL_INT *m, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *work, MKL_INT *lwork, MKL_INT *info);
void	PCGELQF(MKL_INT *m, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZGELQF(MKL_INT *m, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSORGLQ(MKL_INT *m, MKL_INT *n, MKL_INT *k, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDORGLQ(MKL_INT *m, MKL_INT *n, MKL_INT *k, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *work, MKL_INT *lwork, MKL_INT *info);

void	PCUNGLQ(MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZUNGLQ(MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSORMLQ(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDORMLQ(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, double *work, MKL_INT *lwork, MKL_INT *info);

void	PCUNMLQ(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZUNMLQ(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSGEQLF(MKL_INT *m, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDGEQLF(MKL_INT *m, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *work, MKL_INT *lwork, MKL_INT *info);
void	PCGEQLF(MKL_INT *m, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZGEQLF(MKL_INT *m, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSORGQL(MKL_INT *m, MKL_INT *n, MKL_INT *k, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDORGQL(MKL_INT *m, MKL_INT *n, MKL_INT *k, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *work, MKL_INT *lwork, MKL_INT *info);

void	PCUNGQL(MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZUNGQL(MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSORMQL(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDORMQL(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, double *work, MKL_INT *lwork, MKL_INT *info);

void	PCUNMQL(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZUNMQL(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSGERQF(MKL_INT *m, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDGERQF(MKL_INT *m, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *work, MKL_INT *lwork, MKL_INT *info);
void	PCGERQF(MKL_INT *m, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZGERQF(MKL_INT *m, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSORGRQ(MKL_INT *m, MKL_INT *n, MKL_INT *k, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDORGRQ(MKL_INT *m, MKL_INT *n, MKL_INT *k, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *work, MKL_INT *lwork, MKL_INT *info);

void	PCUNGRQ(MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZUNGRQ(MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSORMRQ(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDORMRQ(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, double *work, MKL_INT *lwork, MKL_INT *info);

void	PCUNMRQ(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZUNMRQ(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSTZRZF(MKL_INT *m, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDTZRZF(MKL_INT *m, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *work, MKL_INT *lwork, MKL_INT *info);
void	PCTZRZF(MKL_INT *m, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZTZRZF(MKL_INT *m, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSORMRZ(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_INT *l, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDORMRZ(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_INT *l, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, double *work, MKL_INT *lwork, MKL_INT *info);

void	PCUNMRZ(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_INT *l, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZUNMRZ(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_INT *l, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSGGQRF(MKL_INT *n, MKL_INT *m, MKL_INT *p, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *taua, float *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, float *taub, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDGGQRF(MKL_INT *n, MKL_INT *m, MKL_INT *p, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *taua, double *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, double *taub, double *work, MKL_INT *lwork, MKL_INT *info);
void	PCGGQRF(MKL_INT *n, MKL_INT *m, MKL_INT *p, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *taua, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_Complex8 *taub, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZGGQRF(MKL_INT *n, MKL_INT *m, MKL_INT *p, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *taua, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_Complex16 *taub, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSGGRQF(MKL_INT *m, MKL_INT *p, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *taua, float *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, float *taub, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDGGRQF(MKL_INT *m, MKL_INT *p, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *taua, double *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, double *taub, double *work, MKL_INT *lwork, MKL_INT *info);
void	PCGGRQF(MKL_INT *m, MKL_INT *p, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *taua, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_Complex8 *taub, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZGGRQF(MKL_INT *m, MKL_INT *p, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *taua, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_Complex16 *taub, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSSYTRD(char *uplo, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *d, float *e, float *tau, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDSYTRD(char *uplo, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *d, double *e, double *tau, double *work, MKL_INT *lwork, MKL_INT *info);

void	PSORMTR(char *side, char *uplo, char *trans, MKL_INT *m, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDORMTR(char *side, char *uplo, char *trans, MKL_INT *m, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, double *work, MKL_INT *lwork, MKL_INT *info);

void	PCHETRD(char *uplo, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *d, float *e, MKL_Complex8 *tau, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZHETRD(char *uplo, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *d, double *e, MKL_Complex16 *tau, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PCUNMTR(char *side, char *uplo, char *trans, MKL_INT *m, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZUNMTR(char *side, char *uplo, char *trans, MKL_INT *m, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSSTEBZ(MKL_INT *ictxt, char *range, char *order, MKL_INT *n, float *vl, float *vu, MKL_INT *il, MKL_INT *iu, float *abstol, float *d, float *e, MKL_INT *m, MKL_INT *nsplit, float *w, MKL_INT *iblock, MKL_INT *isplit, float *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *info);
void	PDSTEBZ(MKL_INT *ictxt, char *range, char *order, MKL_INT *n, double *vl, double *vu, MKL_INT *il, MKL_INT *iu, double *abstol, double *d, double *e, MKL_INT *m, MKL_INT *nsplit, double *w, MKL_INT *iblock, MKL_INT *isplit, double *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *info);

void	PSSTEIN(MKL_INT *n, float *d, float *e, MKL_INT *m, float *w, MKL_INT *iblock, MKL_INT *isplit, float *orfac, float *z, MKL_INT *iz, MKL_INT *jz, MKL_INT *descz, float *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *ifail, MKL_INT *iclustr, float *gap, MKL_INT *info);
void	PDSTEIN(MKL_INT *n, double *d, double *e, MKL_INT *m, double *w, MKL_INT *iblock, MKL_INT *isplit, double *orfac, double *z, MKL_INT *iz, MKL_INT *jz, MKL_INT *descz, double *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *ifail, MKL_INT *iclustr, double *gap, MKL_INT *info);
void	PCSTEIN(MKL_INT *n, float *d, float *e, MKL_INT *m, float *w, MKL_INT *iblock, MKL_INT *isplit, float *orfac, MKL_Complex8 *z, MKL_INT *iz, MKL_INT *jz, MKL_INT *descz, float *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *ifail, MKL_INT *iclustr, float *gap, MKL_INT *info);
void	PZSTEIN(MKL_INT *n, double *d, double *e, MKL_INT *m, double *w, MKL_INT *iblock, MKL_INT *isplit, double *orfac, MKL_Complex16 *z, MKL_INT *iz, MKL_INT *jz, MKL_INT *descz, double *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *ifail, MKL_INT *iclustr, double *gap, MKL_INT *info);

void	PSGEHRD(MKL_INT *n, MKL_INT *ilo, MKL_INT *ihi, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDGEHRD(MKL_INT *n, MKL_INT *ilo, MKL_INT *ihi, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *work, MKL_INT *lwork, MKL_INT *info);
void	PCGEHRD(MKL_INT *n, MKL_INT *ilo, MKL_INT *ihi, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZGEHRD(MKL_INT *n, MKL_INT *ilo, MKL_INT *ihi, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSORMHR(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *ilo, MKL_INT *ihi, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDORMHR(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *ilo, MKL_INT *ihi, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, double *work, MKL_INT *lwork, MKL_INT *info);

void	PCUNMHR(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *ilo, MKL_INT *ihi, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZUNMHR(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *ilo, MKL_INT *ihi, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSLAHQR(MKL_INT *wantt, MKL_INT *wantz, MKL_INT *n, MKL_INT *ilo, MKL_INT *ihi, float *a, MKL_INT *desca, float *wr, float *wi, MKL_INT *iloz, MKL_INT *ihiz, float *z, MKL_INT *descz, float *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *ilwork, MKL_INT *info);
void	PDLAHQR(MKL_INT *wantt, MKL_INT *wantz, MKL_INT *n, MKL_INT *ilo, MKL_INT *ihi, double *a, MKL_INT *desca, double *wr, double *wi, MKL_INT *iloz, MKL_INT *ihiz, double *z, MKL_INT *descz, double *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *ilwork, MKL_INT *info);

void	PSGEBRD(MKL_INT *m, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *d, float *e, float *tauq, float *taup, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDGEBRD(MKL_INT *m, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *d, double *e, double *tauq, double *taup, double *work, MKL_INT *lwork, MKL_INT *info);
void	PCGEBRD(MKL_INT *m, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *d, float *e, MKL_Complex8 *tauq, MKL_Complex8 *taup, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZGEBRD(MKL_INT *m, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *d, double *e, MKL_Complex16 *tauq, MKL_Complex16 *taup, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSORMBR(char *vect, char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDORMBR(char *vect, char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, double *work, MKL_INT *lwork, MKL_INT *info);

void	PCUNMBR(char *vect, char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZUNMBR(char *vect, char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSSYGST(MKL_INT *ibtype, char *uplo, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, float *scale, MKL_INT *info);
void	PDSYGST(MKL_INT *ibtype, char *uplo, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, double *scale, MKL_INT *info);

void	PCHEGST(MKL_INT *ibtype, char *uplo, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, float *scale, MKL_INT *info);
void	PZHEGST(MKL_INT *ibtype, char *uplo, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, double *scale, MKL_INT *info);

void	PSGESV(MKL_INT *n, MKL_INT *nrhs, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, float *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_INT *info);
void	PDGESV(MKL_INT *n, MKL_INT *nrhs, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, double *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_INT *info);
void	PCGESV(MKL_INT *n, MKL_INT *nrhs, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_INT *info);
void	PZGESV(MKL_INT *n, MKL_INT *nrhs, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_INT *info);

void	PSGESVX(char *fact, char *trans, MKL_INT *n, MKL_INT *nrhs, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *af, MKL_INT *iaf, MKL_INT *jaf, MKL_INT *descaf, MKL_INT *ipiv, char *equed, float *r, float *c, float *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, float *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, float *rcond, float *ferr, float *berr, float *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *info);
void	PDGESVX(char *fact, char *trans, MKL_INT *n, MKL_INT *nrhs, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *af, MKL_INT *iaf, MKL_INT *jaf, MKL_INT *descaf, MKL_INT *ipiv, char *equed, double *r, double *c, double *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, double *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, double *rcond, double *ferr, double *berr, double *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *info);
void	PCGESVX(char *fact, char *trans, MKL_INT *n, MKL_INT *nrhs, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *af, MKL_INT *iaf, MKL_INT *jaf, MKL_INT *descaf, MKL_INT *ipiv, char *equed, float *r, float *c, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_Complex8 *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, float *rcond, float *ferr, float *berr, MKL_Complex8 *work, MKL_INT *lwork, float *rwork, MKL_INT *lrwork, MKL_INT *info);
void	PZGESVX(char *fact, char *trans, MKL_INT *n, MKL_INT *nrhs, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *af, MKL_INT *iaf, MKL_INT *jaf, MKL_INT *descaf, MKL_INT *ipiv, char *equed, double *r, double *c, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_Complex16 *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, double *rcond, double *ferr, double *berr, MKL_Complex16 *work, MKL_INT *lwork, double *rwork, MKL_INT *lrwork, MKL_INT *info);

void	PSGBSV(MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, MKL_INT *nrhs, float *a, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, float *b, MKL_INT *ib, MKL_INT *descb, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDGBSV(MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, MKL_INT *nrhs, double *a, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, double *b, MKL_INT *ib, MKL_INT *descb, double *work, MKL_INT *lwork, MKL_INT *info);
void	PCGBSV(MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, MKL_INT *nrhs, MKL_Complex8 *a, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZGBSV(MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, MKL_INT *nrhs, MKL_Complex16 *a, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSDBSV(MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, MKL_INT *nrhs, float *a, MKL_INT *ja, MKL_INT *desca, float *b, MKL_INT *ib, MKL_INT *descb, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDDBSV(MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, MKL_INT *nrhs, double *a, MKL_INT *ja, MKL_INT *desca, double *b, MKL_INT *ib, MKL_INT *descb, double *work, MKL_INT *lwork, MKL_INT *info);
void	PCDBSV(MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, MKL_INT *nrhs, MKL_Complex8 *a, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZDBSV(MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, MKL_INT *nrhs, MKL_Complex16 *a, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSDTSV(MKL_INT *n, MKL_INT *nrhs, float *dl, float *d, float *du, MKL_INT *ja, MKL_INT *desca, float *b, MKL_INT *ib, MKL_INT *descb, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDDTSV(MKL_INT *n, MKL_INT *nrhs, double *dl, double *d, double *du, MKL_INT *ja, MKL_INT *desca, double *b, MKL_INT *ib, MKL_INT *descb, double *work, MKL_INT *lwork, MKL_INT *info);
void	PCDTSV(MKL_INT *n, MKL_INT *nrhs, MKL_Complex8 *dl, MKL_Complex8 *d, MKL_Complex8 *du, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZDTSV(MKL_INT *n, MKL_INT *nrhs, MKL_Complex16 *dl, MKL_Complex16 *d, MKL_Complex16 *du, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSPOSV(char *uplo, MKL_INT *n, MKL_INT *nrhs, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_INT *info);
void	PDPOSV(char *uplo, MKL_INT *n, MKL_INT *nrhs, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_INT *info);
void	PCPOSV(char *uplo, MKL_INT *n, MKL_INT *nrhs, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_INT *info);
void	PZPOSV(char *uplo, MKL_INT *n, MKL_INT *nrhs, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_INT *info);

void	PSPOSVX(char *fact, char *uplo, MKL_INT *n, MKL_INT *nrhs, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *af, MKL_INT *iaf, MKL_INT *jaf, MKL_INT *descaf, char *equed, float *sr, float *sc, float *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, float *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, float *rcond, float *ferr, float *berr, float *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *info);
void	PDPOSVX(char *fact, char *uplo, MKL_INT *n, MKL_INT *nrhs, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *af, MKL_INT *iaf, MKL_INT *jaf, MKL_INT *descaf, char *equed, double *sr, double *sc, double *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, double *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, double *rcond, double *ferr, double *berr, double *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *info);
void	PCPOSVX(char *fact, char *uplo, MKL_INT *n, MKL_INT *nrhs, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *af, MKL_INT *iaf, MKL_INT *jaf, MKL_INT *descaf, char *equed, float *sr, float *sc, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_Complex8 *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, float *rcond, float *ferr, float *berr, MKL_Complex8 *work, MKL_INT *lwork, float *rwork, MKL_INT *lrwork, MKL_INT *info);
void	PZPOSVX(char *fact, char *uplo, MKL_INT *n, MKL_INT *nrhs, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *af, MKL_INT *iaf, MKL_INT *jaf, MKL_INT *descaf, char *equed, double *sr, double *sc, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_Complex16 *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, double *rcond, double *ferr, double *berr, MKL_Complex16 *work, MKL_INT *lwork, double *rwork, MKL_INT *lrwork, MKL_INT *info);

void	PSPBSV(char *uplo, MKL_INT *n, MKL_INT *bw, MKL_INT *nrhs, float *a, MKL_INT *ja, MKL_INT *desca, float *b, MKL_INT *ib, MKL_INT *descb, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDPBSV(char *uplo, MKL_INT *n, MKL_INT *bw, MKL_INT *nrhs, double *a, MKL_INT *ja, MKL_INT *desca, double *b, MKL_INT *ib, MKL_INT *descb, double *work, MKL_INT *lwork, MKL_INT *info);
void	PCPBSV(char *uplo, MKL_INT *n, MKL_INT *bw, MKL_INT *nrhs, MKL_Complex8 *a, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZPBSV(char *uplo, MKL_INT *n, MKL_INT *bw, MKL_INT *nrhs, MKL_Complex16 *a, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSPTSV(MKL_INT *n, MKL_INT *nrhs, float *d, float *e, MKL_INT *ja, MKL_INT *desca, float *b, MKL_INT *ib, MKL_INT *descb, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDPTSV(MKL_INT *n, MKL_INT *nrhs, double *d, double *e, MKL_INT *ja, MKL_INT *desca, double *b, MKL_INT *ib, MKL_INT *descb, double *work, MKL_INT *lwork, MKL_INT *info);
void	PCPTSV(char *uplo, MKL_INT *n, MKL_INT *nrhs, float *d, MKL_Complex8 *e, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZPTSV(char *uplo, MKL_INT *n, MKL_INT *nrhs, double *d, MKL_Complex16 *e, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSGELS(char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *nrhs, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDGELS(char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *nrhs, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, double *work, MKL_INT *lwork, MKL_INT *info);
void	PCGELS(char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *nrhs, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZGELS(char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *nrhs, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSSYEV(char *jobz, char *uplo, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *w, float *z, MKL_INT *iz, MKL_INT *jz, MKL_INT *descz, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDSYEV(char *jobz, char *uplo, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *w, double *z, MKL_INT *iz, MKL_INT *jz, MKL_INT *descz, double *work, MKL_INT *lwork, MKL_INT *info);

void	PSSYEVX(char *jobz, char *range, char *uplo, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *vl, float *vu, MKL_INT *il, MKL_INT *iu, float *abstol, MKL_INT *m, MKL_INT *nz, float *w, float *orfac, float *z, MKL_INT *iz, MKL_INT *jz, MKL_INT *descz, float *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *ifail, MKL_INT *iclustr, float *gap, MKL_INT *info);
void	PDSYEVX(char *jobz, char *range, char *uplo, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *vl, double *vu, MKL_INT *il, MKL_INT *iu, double *abstol, MKL_INT *m, MKL_INT *nz, double *w, double *orfac, double *z, MKL_INT *iz, MKL_INT *jz, MKL_INT *descz, double *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *ifail, MKL_INT *iclustr, double *gap, MKL_INT *info);

void	PCHEEVX(char *jobz, char *range, char *uplo, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *vl, float *vu, MKL_INT *il, MKL_INT *iu, float *abstol, MKL_INT *m, MKL_INT *nz, float *w, float *orfac, MKL_Complex8 *z, MKL_INT *iz, MKL_INT *jz, MKL_INT *descz, MKL_Complex8 *work, MKL_INT *lwork, float *rwork, MKL_INT *lrwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *ifail, MKL_INT *iclustr, float *gap, MKL_INT *info);
void	PZHEEVX(char *jobz, char *range, char *uplo, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *vl, double *vu, MKL_INT *il, MKL_INT *iu, double *abstol, MKL_INT *m, MKL_INT *nz, double *w, double *orfac, MKL_Complex16 *z, MKL_INT *iz, MKL_INT *jz, MKL_INT *descz, MKL_Complex16 *work, MKL_INT *lwork, double *rwork, MKL_INT *lrwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *ifail, MKL_INT *iclustr, double *gap, MKL_INT *info);

void	PSGESVD(char *jobu, char *jobvt, MKL_INT *m, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *s, float *u, MKL_INT *iu, MKL_INT *ju, MKL_INT *descu, float *vt, MKL_INT *ivt, MKL_INT *jvt, MKL_INT *descvt, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDGESVD(char *jobu, char *jobvt, MKL_INT *m, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *s, double *u, MKL_INT *iu, MKL_INT *ju, MKL_INT *descu, double *vt, MKL_INT *ivt, MKL_INT *jvt, MKL_INT *descvt, double *work, MKL_INT *lwork, MKL_INT *info);
void	PCGESVD(char *jobu, char *jobvt, MKL_INT *m, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *s, MKL_Complex8 *u, MKL_INT *iu, MKL_INT *ju, MKL_INT *descu, MKL_Complex8 *vt, MKL_INT *ivt, MKL_INT *jvt, MKL_INT *descvt, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZGESVD(char *jobu, char *jobvt, MKL_INT *m, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *s, MKL_Complex16 *u, MKL_INT *iu, MKL_INT *ju, MKL_INT *descu, MKL_Complex16 *vt, MKL_INT *ivt, MKL_INT *jvt, MKL_INT *descvt, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSSYGVX(MKL_INT *ibtype, char *jobz, char *range, char *uplo, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, float *vl, float *vu, MKL_INT *il, MKL_INT *iu, float *abstol, MKL_INT *m, MKL_INT *nz, float *w, float *orfac, float *z, MKL_INT *iz, MKL_INT *jz, MKL_INT *descz, float *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *ifail, MKL_INT *iclustr, float *gap, MKL_INT *info);
void	PDSYGVX(MKL_INT *ibtype, char *jobz, char *range, char *uplo, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, double *vl, double *vu, MKL_INT *il, MKL_INT *iu, double *abstol, MKL_INT *m, MKL_INT *nz, double *w, double *orfac, double *z, MKL_INT *iz, MKL_INT *jz, MKL_INT *descz, double *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *ifail, MKL_INT *iclustr, double *gap, MKL_INT *info);

void	PCHEGVX(MKL_INT *ibtype, char *jobz, char *range, char *uplo, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, float *vl, float *vu, MKL_INT *il, MKL_INT *iu, float *abstol, MKL_INT *m, MKL_INT *nz, float *w, float *orfac, MKL_Complex8 *z, MKL_INT *iz, MKL_INT *jz, MKL_INT *descz, MKL_Complex8 *work, MKL_INT *lwork, float *rwork, MKL_INT *lrwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *ifail, MKL_INT *iclustr, float *gap, MKL_INT *info);
void	PZHEGVX(MKL_INT *ibtype, char *jobz, char *range, char *uplo, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, double *vl, double *vu, MKL_INT *il, MKL_INT *iu, double *abstol, MKL_INT *m, MKL_INT *nz, double *w, double *orfac, MKL_Complex16 *z, MKL_INT *iz, MKL_INT *jz, MKL_INT *descz, MKL_Complex16 *work, MKL_INT *lwork, double *rwork, MKL_INT *lrwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *ifail, MKL_INT *iclustr, double *gap, MKL_INT *info);

void	PCLACGV(MKL_INT *n, MKL_Complex8 *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, MKL_INT *incx);
void	PZLACGV(MKL_INT *n, MKL_Complex16 *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, MKL_INT *incx);

void	PCMAX1(MKL_INT *n, MKL_Complex8 *amax, MKL_INT *indx, MKL_Complex8 *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, MKL_INT *incx);
void	PZMAX1(MKL_INT *n, MKL_Complex16 *amax, MKL_INT *indx, MKL_Complex16 *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, MKL_INT *incx);

void	CCOMBAMAX1(MKL_Complex8 *v1, MKL_Complex8 *v2);
void	ZCOMBAMAX1(MKL_Complex16 *v1, MKL_Complex16 *v2);

void	PSCSUM1(MKL_INT *n, float *asum, MKL_Complex8 *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, MKL_INT *incx);
void	PDZSUM1(MKL_INT *n, double *asum, MKL_Complex16 *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, MKL_INT *incx);

void	PSDBTRSV(char *uplo, char *trans, MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, MKL_INT *nrhs, float *a, MKL_INT *ja, MKL_INT *desca, float *b, MKL_INT *ib, MKL_INT *descb, float *af, MKL_INT *laf, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDDBTRSV(char *uplo, char *trans, MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, MKL_INT *nrhs, double *a, MKL_INT *ja, MKL_INT *desca, double *b, MKL_INT *ib, MKL_INT *descb, double *af, MKL_INT *laf, double *work, MKL_INT *lwork, MKL_INT *info);
void	PCDBTRSV(char *uplo, char *trans, MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, MKL_INT *nrhs, MKL_Complex8 *a, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex8 *af, MKL_INT *laf, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZDBTRSV(char *uplo, char *trans, MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, MKL_INT *nrhs, MKL_Complex16 *a, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex16 *af, MKL_INT *laf, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSDTTRSV(char *uplo, char *trans, MKL_INT *n, MKL_INT *nrhs, float *dl, float *d, float *du, MKL_INT *ja, MKL_INT *desca, float *b, MKL_INT *ib, MKL_INT *descb, float *af, MKL_INT *laf, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDDTTRSV(char *uplo, char *trans, MKL_INT *n, MKL_INT *nrhs, double *dl, double *d, double *du, MKL_INT *ja, MKL_INT *desca, double *b, MKL_INT *ib, MKL_INT *descb, double *af, MKL_INT *laf, double *work, MKL_INT *lwork, MKL_INT *info);
void	PCDTTRSV(char *uplo, char *trans, MKL_INT *n, MKL_INT *nrhs, MKL_Complex8 *dl, MKL_Complex8 *d, MKL_Complex8 *du, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex8 *af, MKL_INT *laf, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZDTTRSV(char *uplo, char *trans, MKL_INT *n, MKL_INT *nrhs, MKL_Complex16 *dl, MKL_Complex16 *d, MKL_Complex16 *du, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex16 *af, MKL_INT *laf, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSGEBD2(MKL_INT *m, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *d, float *e, float *tauq, float *taup, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDGEBD2(MKL_INT *m, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *d, double *e, double *tauq, double *taup, double *work, MKL_INT *lwork, MKL_INT *info);
void	PCGEBD2(MKL_INT *m, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *d, float *e, MKL_Complex8 *tauq, MKL_Complex8 *taup, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZGEBD2(MKL_INT *m, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *d, double *e, MKL_Complex16 *tauq, MKL_Complex16 *taup, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSGEHD2(MKL_INT *n, MKL_INT *ilo, MKL_INT *ihi, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDGEHD2(MKL_INT *n, MKL_INT *ilo, MKL_INT *ihi, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *work, MKL_INT *lwork, MKL_INT *info);
void	PCGEHD2(MKL_INT *n, MKL_INT *ilo, MKL_INT *ihi, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZGEHD2(MKL_INT *n, MKL_INT *ilo, MKL_INT *ihi, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSGELQ2(MKL_INT *m, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDGELQ2(MKL_INT *m, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *work, MKL_INT *lwork, MKL_INT *info);
void	PCGELQ2(MKL_INT *m, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZGELQ2(MKL_INT *m, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSGEQL2(MKL_INT *m, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDGEQL2(MKL_INT *m, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *work, MKL_INT *lwork, MKL_INT *info);
void	PCGEQL2(MKL_INT *m, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZGEQL2(MKL_INT *m, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSGEQR2(MKL_INT *m, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDGEQR2(MKL_INT *m, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *work, MKL_INT *lwork, MKL_INT *info);
void	PCGEQR2(MKL_INT *m, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZGEQR2(MKL_INT *m, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSGERQ2(MKL_INT *m, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDGERQ2(MKL_INT *m, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *work, MKL_INT *lwork, MKL_INT *info);
void	PCGERQ2(MKL_INT *m, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZGERQ2(MKL_INT *m, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSGETF2(MKL_INT *m, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, MKL_INT *info);
void	PDGETF2(MKL_INT *m, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, MKL_INT *info);
void	PCGETF2(MKL_INT *m, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, MKL_INT *info);
void	PZGETF2(MKL_INT *m, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, MKL_INT *info);

void	PSLABRD(MKL_INT *m, MKL_INT *n, MKL_INT *nb, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *d, float *e, float *tauq, float *taup, float *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, float *y, MKL_INT *iy, MKL_INT *jy, MKL_INT *descy, float *work);
void	PDLABRD(MKL_INT *m, MKL_INT *n, MKL_INT *nb, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *d, double *e, double *tauq, double *taup, double *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, double *y, MKL_INT *iy, MKL_INT *jy, MKL_INT *descy, double *work);
void	PCLABRD(MKL_INT *m, MKL_INT *n, MKL_INT *nb, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *d, float *e, MKL_Complex8 *tauq, MKL_Complex8 *taup, MKL_Complex8 *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, MKL_Complex8 *y, MKL_INT *iy, MKL_INT *jy, MKL_INT *descy, MKL_Complex8 *work);
void	PZLABRD(MKL_INT *m, MKL_INT *n, MKL_INT *nb, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *d, double *e, MKL_Complex16 *tauq, MKL_Complex16 *taup, MKL_Complex16 *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, MKL_Complex16 *y, MKL_INT *iy, MKL_INT *jy, MKL_INT *descy, MKL_Complex16 *work);

void	PSLACON(MKL_INT *n, float *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, float *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, MKL_INT *isgn, float *est, MKL_INT *kase);
void	PDLACON(MKL_INT *n, double *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, double *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, MKL_INT *isgn, double *est, MKL_INT *kase);
void	PCLACON(MKL_INT *n, MKL_Complex8 *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, MKL_Complex8 *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, float *est, MKL_INT *kase);
void	PZLACON(MKL_INT *n, MKL_Complex16 *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, MKL_Complex16 *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, double *est, MKL_INT *kase);

void	PSLACONSB(float *a, MKL_INT *desca, MKL_INT *i, MKL_INT *l, MKL_INT *m, float *h44, float *h33, float *h43h34, float *buf, MKL_INT *lwork);
void	PDLACONSB(double *a, MKL_INT *desca, MKL_INT *i, MKL_INT *l, MKL_INT *m, double *h44, double *h33, double *h43h34, double *buf, MKL_INT *lwork);

void	PSLACP2(char *uplo, MKL_INT *m, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb);
void	PDLACP2(char *uplo, MKL_INT *m, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb);
void	PCLACP2(char *uplo, MKL_INT *m, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb);
void	PZLACP2(char *uplo, MKL_INT *m, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb);

void	PSLACP3(MKL_INT *m, MKL_INT *i, float *a, MKL_INT *desca, float *b, MKL_INT *ldb, MKL_INT *ii, MKL_INT *jj, MKL_INT *rev);
void	PDLACP3(MKL_INT *m, MKL_INT *i, double *a, MKL_INT *desca, double *b, MKL_INT *ldb, MKL_INT *ii, MKL_INT *jj, MKL_INT *rev);

void	PSLACPY(char *uplo, MKL_INT *m, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb);
void	PDLACPY(char *uplo, MKL_INT *m, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb);
void	PCLACPY(char *uplo, MKL_INT *m, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb);
void	PZLACPY(char *uplo, MKL_INT *m, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb);

void	PSLAEVSWP(MKL_INT *n, float *zin, MKL_INT *ldzi, float *z, MKL_INT *iz, MKL_INT *jz, MKL_INT *descz, MKL_INT *nvs, MKL_INT *key, float *work, MKL_INT *lwork);
void	PDLAEVSWP(MKL_INT *n, double *zin, MKL_INT *ldzi, double *z, MKL_INT *iz, MKL_INT *jz, MKL_INT *descz, MKL_INT *nvs, MKL_INT *key, double *work, MKL_INT *lwork);
void	PCLAEVSWP(MKL_INT *n, float *zin, MKL_INT *ldzi, MKL_Complex8 *z, MKL_INT *iz, MKL_INT *jz, MKL_INT *descz, MKL_INT *nvs, MKL_INT *key, float *rwork, MKL_INT *lrwork);
void	PZLAEVSWP(MKL_INT *n, double *zin, MKL_INT *ldzi, MKL_Complex16 *z, MKL_INT *iz, MKL_INT *jz, MKL_INT *descz, MKL_INT *nvs, MKL_INT *key, double *rwork, MKL_INT *lrwork);

void	PSLAHRD(MKL_INT *n, MKL_INT *k, MKL_INT *nb, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *t, float *y, MKL_INT *iy, MKL_INT *jy, MKL_INT *descy, float *work);
void	PDLAHRD(MKL_INT *n, MKL_INT *k, MKL_INT *nb, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *t, double *y, MKL_INT *iy, MKL_INT *jy, MKL_INT *descy, double *work);
void	PCLAHRD(MKL_INT *n, MKL_INT *k, MKL_INT *nb, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *t, MKL_Complex8 *y, MKL_INT *iy, MKL_INT *jy, MKL_INT *descy, MKL_Complex8 *work);
void	PZLAHRD(MKL_INT *n, MKL_INT *k, MKL_INT *nb, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *t, MKL_Complex16 *y, MKL_INT *iy, MKL_INT *jy, MKL_INT *descy, MKL_Complex16 *work);

void	PSLAIECT(float *sigma, MKL_INT *n, float *d, MKL_INT *count);
void	PDLAIECTB(float *sigma, MKL_INT *n, float *d, MKL_INT *count);
void	PDLAIECTL(float *sigma, MKL_INT *n, float *d, MKL_INT *count);

float	PSLANGE(char *norm, MKL_INT *m, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *work);
double	PDLANGE(char *norm, MKL_INT *m, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *work);
float	PCLANGE(char *norm, MKL_INT *m, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *work);
double	PZLANGE(char *norm, MKL_INT *m, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *work);

float	PSLANHS(char *norm, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *work);
double	PDLANHS(char *norm, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *work);
float	PCLANHS(char *norm, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *work);
double	PZLANHS(char *norm, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *work);

float	PSLANSY(char *norm, char *uplo, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *work);
double	PDLANSY(char *norm, char *uplo, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *work);
float	PCLANSY(char *norm, char *uplo, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *work);
double	PZLANSY(char *norm, char *uplo, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *work);
float	PCLANHE(char *norm, char *uplo, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *work);
double	PZLANHE(char *norm, char *uplo, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *work);

float	PSLANTR(char *norm, char *uplo, char *diag, MKL_INT *m, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *work);
double	PDLANTR(char *norm, char *uplo, char *diag, MKL_INT *m, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *work);
float	PCLANTR(char *norm, char *uplo, char *diag, MKL_INT *m, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *work);
double	PZLANTR(char *norm, char *uplo, char *diag, MKL_INT *m, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *work);

void	PSLAPIV(char *direc, char *rowcol, char *pivroc, MKL_INT *m, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, MKL_INT *ip, MKL_INT *jp, MKL_INT *descip, MKL_INT *iwork);
void	PDLAPIV(char *direc, char *rowcol, char *pivroc, MKL_INT *m, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, MKL_INT *ip, MKL_INT *jp, MKL_INT *descip, MKL_INT *iwork);
void	PCLAPIV(char *direc, char *rowcol, char *pivroc, MKL_INT *m, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, MKL_INT *ip, MKL_INT *jp, MKL_INT *descip, MKL_INT *iwork);
void	PZLAPIV(char *direc, char *rowcol, char *pivroc, MKL_INT *m, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, MKL_INT *ip, MKL_INT *jp, MKL_INT *descip, MKL_INT *iwork);

void	PSLAQGE(MKL_INT *m, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *r, float *c, float *rowcnd, float *colcnd, float *amax, char *equed);
void	PDLAQGE(MKL_INT *m, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *r, double *c, double *rowcnd, double *colcnd, double *amax, char *equed);
void	PCLAQGE(MKL_INT *m, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *r, float *c, float *rowcnd, float *colcnd, float *amax, char *equed);
void	PZLAQGE(MKL_INT *m, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *r, double *c, double *rowcnd, double *colcnd, double *amax, char *equed);

void	PSLAQSY(char *uplo, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *sr, float *sc, float *scond, float *amax, char *equed);
void	PDLAQSY(char *uplo, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *sr, double *sc, double *scond, double *amax, char *equed);
void	PCLAQSY(char *uplo, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *sr, float *sc, float *scond, float *amax, char *equed);
void	PZLAQSY(char *uplo, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *sr, double *sc, double *scond, double *amax, char *equed);

void	PSLARED1D(MKL_INT *n, MKL_INT *ia, MKL_INT *ja, MKL_INT *desc, float *bycol, float *byall, float *work, MKL_INT *lwork);
void	PDLARED1D(MKL_INT *n, MKL_INT *ia, MKL_INT *ja, MKL_INT *desc, double *bycol, double *byall, double *work, MKL_INT *lwork);

void	PSLARED2D(MKL_INT *n, MKL_INT *ia, MKL_INT *ja, MKL_INT *desc, float *byrow, float *byall, float *work, MKL_INT *lwork);
void	PDLARED2D(MKL_INT *n, MKL_INT *ia, MKL_INT *ja, MKL_INT *desc, double *byrow, double *byall, double *work, MKL_INT *lwork);

void	PSLARF(char *side, MKL_INT *m, MKL_INT *n, float *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, MKL_INT *incv, float *tau, float *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, float *work);
void	PDLARF(char *side, MKL_INT *m, MKL_INT *n, double *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, MKL_INT *incv, double *tau, double *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, double *work);
void	PCLARF(char *side, MKL_INT *m, MKL_INT *n, MKL_Complex8 *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, MKL_INT *incv, MKL_Complex8 *tau, MKL_Complex8 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex8 *work);
void	PZLARF(char *side, MKL_INT *m, MKL_INT *n, MKL_Complex16 *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, MKL_INT *incv, MKL_Complex16 *tau, MKL_Complex16 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex16 *work);

void	PSLARFB(char *side, char *trans, char *direct, char *storev, MKL_INT *m, MKL_INT *n, MKL_INT *k, float *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, float *t, float *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, float *work);
void	PDLARFB(char *side, char *trans, char *direct, char *storev, MKL_INT *m, MKL_INT *n, MKL_INT *k, double *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, double *t, double *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, double *work);
void	PCLARFB(char *side, char *trans, char *direct, char *storev, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex8 *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, MKL_Complex8 *t, MKL_Complex8 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex8 *work);
void	PZLARFB(char *side, char *trans, char *direct, char *storev, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex16 *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, MKL_Complex16 *t, MKL_Complex16 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex16 *work);

void	PCLARFC(char *side, MKL_INT *m, MKL_INT *n, MKL_Complex8 *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, MKL_INT *incv, MKL_Complex8 *tau, MKL_Complex8 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex8 *work);
void	PZLARFC(char *side, MKL_INT *m, MKL_INT *n, MKL_Complex16 *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, MKL_INT *incv, MKL_Complex16 *tau, MKL_Complex16 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex16 *work);

void	PSLARFG(MKL_INT *n, float *alpha, MKL_INT *iax, MKL_INT *jax, float *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, MKL_INT *incx, float *tau);
void	PDLARFG(MKL_INT *n, double *alpha, MKL_INT *iax, MKL_INT *jax, double *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, MKL_INT *incx, double *tau);
void	PCLARFG(MKL_INT *n, MKL_Complex8 *alpha, MKL_INT *iax, MKL_INT *jax, MKL_Complex8 *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, MKL_INT *incx, MKL_Complex8 *tau);
void	PZLARFG(MKL_INT *n, MKL_Complex16 *alpha, MKL_INT *iax, MKL_INT *jax, MKL_Complex16 *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, MKL_INT *incx, MKL_Complex16 *tau);

void	PSLARFT(char *direct, char *storev, MKL_INT *n, MKL_INT *k, float *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, float *tau, float *t, float *work);
void	PDLARFT(char *direct, char *storev, MKL_INT *n, MKL_INT *k, double *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, double *tau, double *t, double *work);
void	PCLARFT(char *direct, char *storev, MKL_INT *n, MKL_INT *k, MKL_Complex8 *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, MKL_Complex8 *tau, MKL_Complex8 *t, MKL_Complex8 *work);
void	PZLARFT(char *direct, char *storev, MKL_INT *n, MKL_INT *k, MKL_Complex16 *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, MKL_Complex16 *tau, MKL_Complex16 *t, MKL_Complex16 *work);

void	PSLARZ(char *side, MKL_INT *m, MKL_INT *n, MKL_INT *l, float *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, MKL_INT *incv, float *tau, float *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, float *work);
void	PDLARZ(char *side, MKL_INT *m, MKL_INT *n, MKL_INT *l, double *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, MKL_INT *incv, double *tau, double *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, double *work);
void	PCLARZ(char *side, MKL_INT *m, MKL_INT *n, MKL_INT *l, MKL_Complex8 *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, MKL_INT *incv, MKL_Complex8 *tau, MKL_Complex8 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex8 *work);
void	PZLARZ(char *side, MKL_INT *m, MKL_INT *n, MKL_INT *l, MKL_Complex16 *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, MKL_INT *incv, MKL_Complex16 *tau, MKL_Complex16 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex16 *work);

void	PSLARZB(char *side, char *trans, char *direct, char *storev, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_INT *l, float *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, float *t, float *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, float *work);
void	PDLARZB(char *side, char *trans, char *direct, char *storev, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_INT *l, double *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, double *t, double *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, double *work);
void	PCLARZB(char *side, char *trans, char *direct, char *storev, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_INT *l, MKL_Complex8 *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, MKL_Complex8 *t, MKL_Complex8 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex8 *work);
void	PZLARZB(char *side, char *trans, char *direct, char *storev, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_INT *l, MKL_Complex16 *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, MKL_Complex16 *t, MKL_Complex16 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex16 *work);

void	PCLARZC(char *side, MKL_INT *m, MKL_INT *n, MKL_INT *l, MKL_Complex8 *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, MKL_INT *incv, MKL_Complex8 *tau, MKL_Complex8 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex8 *work);
void	PZLARZC(char *side, MKL_INT *m, MKL_INT *n, MKL_INT *l, MKL_Complex16 *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, MKL_INT *incv, MKL_Complex16 *tau, MKL_Complex16 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex16 *work);

void	PSLARZT(char *direct, char *storev, MKL_INT *n, MKL_INT *k, float *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, float *tau, float *t, float *work);
void	PDLARZT(char *direct, char *storev, MKL_INT *n, MKL_INT *k, double *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, double *tau, double *t, double *work);
void	PCLARZT(char *direct, char *storev, MKL_INT *n, MKL_INT *k, MKL_Complex8 *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, MKL_Complex8 *tau, MKL_Complex8 *t, MKL_Complex8 *work);
void	PZLARZT(char *direct, char *storev, MKL_INT *n, MKL_INT *k, MKL_Complex16 *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, MKL_Complex16 *tau, MKL_Complex16 *t, MKL_Complex16 *work);

void	PSLASCL(char *type, float *cfrom, float *cto, MKL_INT *m, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *info);
void	PDLASCL(char *type, double *cfrom, double *cto, MKL_INT *m, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *info);
void	PCLASCL(char *type, float *cfrom, float *cto, MKL_INT *m, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *info);
void	PZLASCL(char *type, double *cfrom, double *cto, MKL_INT *m, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *info);

void	PSLASET(char *uplo, MKL_INT *m, MKL_INT *n, float *alpha, float *beta, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca);
void	PDLASET(char *uplo, MKL_INT *m, MKL_INT *n, double *alpha, double *beta, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca);
void	PCLASET(char *uplo, MKL_INT *m, MKL_INT *n, MKL_Complex8 *alpha, MKL_Complex8 *beta, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca);
void	PZLASET(char *uplo, MKL_INT *m, MKL_INT *n, MKL_Complex16 *alpha, MKL_Complex16 *beta, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca);

void	PSLASMSUB(float *a, MKL_INT *desca, MKL_INT *i, MKL_INT *l, MKL_INT *k, float *smlnum, float *buf, MKL_INT *lwork);
void	PDLASMSUB(double *a, MKL_INT *desca, MKL_INT *i, MKL_INT *l, MKL_INT *k, double *smlnum, double *buf, MKL_INT *lwork);

void	PSLASSQ(MKL_INT *n, float *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, MKL_INT *incx, float *scale, float *sumsq);
void	PDLASSQ(MKL_INT *n, double *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, MKL_INT *incx, double *scale, double *sumsq);
void	PCLASSQ(MKL_INT *n, MKL_Complex8 *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, MKL_INT *incx, float *scale, float *sumsq);
void	PZLASSQ(MKL_INT *n, MKL_Complex16 *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, MKL_INT *incx, double *scale, double *sumsq);

void	PSLASWP(char *direc, char *rowcol, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *k1, MKL_INT *k2, MKL_INT *ipiv);
void	PDLASWP(char *direc, char *rowcol, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *k1, MKL_INT *k2, MKL_INT *ipiv);
void	PCLASWP(char *direc, char *rowcol, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *k1, MKL_INT *k2, MKL_INT *ipiv);
void	PZLASWP(char *direc, char *rowcol, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *k1, MKL_INT *k2, MKL_INT *ipiv);

float	PSLATRA(MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca);
double	PDLATRA(MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca);
void	PCLATRA(MKL_Complex8 *, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca);
void	PZLATRA(MKL_Complex16 *, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca);

void	PSLATRD(char *uplo, MKL_INT *n, MKL_INT *nb, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *d, float *e, float *tau, float *w, MKL_INT *iw, MKL_INT *jw, MKL_INT *descw, float *work);
void	PDLATRD(char *uplo, MKL_INT *n, MKL_INT *nb, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *d, double *e, double *tau, double *w, MKL_INT *iw, MKL_INT *jw, MKL_INT *descw, double *work);
void	PCLATRD(char *uplo, MKL_INT *n, MKL_INT *nb, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *d, float *e, MKL_Complex8 *tau, MKL_Complex8 *w, MKL_INT *iw, MKL_INT *jw, MKL_INT *descw, MKL_Complex8 *work);
void	PZLATRD(char *uplo, MKL_INT *n, MKL_INT *nb, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *d, double *e, MKL_Complex16 *tau, MKL_Complex16 *w, MKL_INT *iw, MKL_INT *jw, MKL_INT *descw, MKL_Complex16 *work);

void	PSLATRS(char *uplo, char *trans, char *diag, char *normin, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, float *scale, float *cnorm, float *work);
void	PDLATRS(char *uplo, char *trans, char *diag, char *normin, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, double *scale, double *cnorm, double *work);
void	PCLATRS(char *uplo, char *trans, char *diag, char *normin, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, float *scale, float *cnorm, MKL_Complex8 *work);
void	PZLATRS(char *uplo, char *trans, char *diag, char *normin, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, double *scale, double *cnorm, MKL_Complex16 *work);

void	PSLATRZ(MKL_INT *m, MKL_INT *n, MKL_INT *l, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *work);
void	PDLATRZ(MKL_INT *m, MKL_INT *n, MKL_INT *l, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *work);
void	PCLATRZ(MKL_INT *m, MKL_INT *n, MKL_INT *l, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *work);
void	PZLATRZ(MKL_INT *m, MKL_INT *n, MKL_INT *l, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *work);

void	PSLAUU2(char *uplo, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca);
void	PDLAUU2(char *uplo, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca);
void	PCLAUU2(char *uplo, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca);
void	PZLAUU2(char *uplo, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca);

void	PSLAUUM(char *uplo, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca);
void	PDLAUUM(char *uplo, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca);
void	PCLAUUM(char *uplo, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca);
void	PZLAUUM(char *uplo, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca);

void	PSLAWIL(MKL_INT *ii, MKL_INT *jj, MKL_INT *m, float *a, MKL_INT *desca, float *h44, float *h33, float *h43h34, float *v);
void	PDLAWIL(MKL_INT *ii, MKL_INT *jj, MKL_INT *m, double *a, MKL_INT *desca, double *h44, double *h33, double *h43h34, double *v);

void	PSORG2L(MKL_INT *m, MKL_INT *n, MKL_INT *k, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDORG2L(MKL_INT *m, MKL_INT *n, MKL_INT *k, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *work, MKL_INT *lwork, MKL_INT *info);
void	PCUNG2L(MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZUNG2L(MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSORG2R(MKL_INT *m, MKL_INT *n, MKL_INT *k, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDORG2R(MKL_INT *m, MKL_INT *n, MKL_INT *k, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *work, MKL_INT *lwork, MKL_INT *info);
void	PCUNG2R(MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZUNG2R(MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSORGL2(MKL_INT *m, MKL_INT *n, MKL_INT *k, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDORGL2(MKL_INT *m, MKL_INT *n, MKL_INT *k, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *work, MKL_INT *lwork, MKL_INT *info);
void	PCUNGL2(MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZUNGL2(MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSORGR2(MKL_INT *m, MKL_INT *n, MKL_INT *k, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDORGR2(MKL_INT *m, MKL_INT *n, MKL_INT *k, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *work, MKL_INT *lwork, MKL_INT *info);
void	PCUNGR2(MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZUNGR2(MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSORM2L(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDORM2L(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, double *work, MKL_INT *lwork, MKL_INT *info);
void	PCUNM2L(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZUNM2L(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSORM2R(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDORM2R(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, double *work, MKL_INT *lwork, MKL_INT *info);
void	PCUNM2R(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZUNM2R(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSORML2(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDORML2(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, double *work, MKL_INT *lwork, MKL_INT *info);
void	PCUNML2(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZUNML2(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSORMR2(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDORMR2(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, double *work, MKL_INT *lwork, MKL_INT *info);
void	PCUNMR2(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZUNMR2(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSPBTRSV(char *uplo, char *trans, MKL_INT *n, MKL_INT *bw, MKL_INT *nrhs, float *a, MKL_INT *ja, MKL_INT *desca, float *b, MKL_INT *ib, MKL_INT *descb, float *af, MKL_INT *laf, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDPBTRSV(char *uplo, char *trans, MKL_INT *n, MKL_INT *bw, MKL_INT *nrhs, double *a, MKL_INT *ja, MKL_INT *desca, double *b, MKL_INT *ib, MKL_INT *descb, double *af, MKL_INT *laf, double *work, MKL_INT *lwork, MKL_INT *info);
void	PCPBTRSV(char *uplo, char *trans, MKL_INT *n, MKL_INT *bw, MKL_INT *nrhs, MKL_Complex8 *a, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex8 *af, MKL_INT *laf, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZPBTRSV(char *uplo, char *trans, MKL_INT *n, MKL_INT *bw, MKL_INT *nrhs, MKL_Complex16 *a, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex16 *af, MKL_INT *laf, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSPTTRSV(char *uplo, MKL_INT *n, MKL_INT *nrhs, float *d, float *e, MKL_INT *ja, MKL_INT *desca, float *b, MKL_INT *ib, MKL_INT *descb, float *af, MKL_INT *laf, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDPTTRSV(char *uplo, MKL_INT *n, MKL_INT *nrhs, double *d, double *e, MKL_INT *ja, MKL_INT *desca, double *b, MKL_INT *ib, MKL_INT *descb, double *af, MKL_INT *laf, double *work, MKL_INT *lwork, MKL_INT *info);
void	PCPTTRSV(char *uplo, char *trans, MKL_INT *n, MKL_INT *nrhs, float *d, MKL_Complex8 *e, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex8 *af, MKL_INT *laf, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZPTTRSV(char *uplo, char *trans, MKL_INT *n, MKL_INT *nrhs, double *d, MKL_Complex16 *e, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex16 *af, MKL_INT *laf, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSPOTF2(char *uplo, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *info);
void	PDPOTF2(char *uplo, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *info);
void	PCPOTF2(char *uplo, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *info);
void	PZPOTF2(char *uplo, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *info);

void	PSRSCL(MKL_INT *n, float *sa, float *sx, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, MKL_INT *incx);
void	PDRSCL(MKL_INT *n, double *sa, double *sx, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, MKL_INT *incx);
void	PCSRSCL(MKL_INT *n, float *sa, MKL_Complex8 *sx, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, MKL_INT *incx);
void	PZDRSCL(MKL_INT *n, double *sa, MKL_Complex16 *sx, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, MKL_INT *incx);

void	PSSYGS2(MKL_INT *ibtype, char *uplo, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_INT *info);
void	PDSYGS2(MKL_INT *ibtype, char *uplo, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_INT *info);
void	PCHEGS2(MKL_INT *ibtype, char *uplo, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_INT *info);
void	PZHEGS2(MKL_INT *ibtype, char *uplo, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_INT *info);

void	PSSYTD2(char *uplo, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *d, float *e, float *tau, float *work, MKL_INT *lwork, MKL_INT *info);
void	PDSYTD2(char *uplo, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *d, double *e, double *tau, double *work, MKL_INT *lwork, MKL_INT *info);
void	PCHETD2(char *uplo, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *d, float *e, MKL_Complex8 *tau, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	PZHETD2(char *uplo, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *d, double *e, MKL_Complex16 *tau, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	PSTRTI2(char *uplo, char *diag, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *info);
void	PDTRTI2(char *uplo, char *diag, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *info);
void	PCTRTI2(char *uplo, char *diag, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *info);
void	PZTRTI2(char *uplo, char *diag, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *info);

void	SLAMSH(float *s, MKL_INT *lds, MKL_INT *nbulge, MKL_INT *jblk, float *h, MKL_INT *ldh, MKL_INT *n, float *ulp);
void	DLAMSH(double *s, MKL_INT *lds, MKL_INT *nbulge, MKL_INT *jblk, double *h, MKL_INT *ldh, MKL_INT *n, double *ulp);

void	SLAREF(char *type, float *a, MKL_INT *lda, MKL_INT *wantz, float *z, MKL_INT *ldz, MKL_INT *block, MKL_INT *irow1, MKL_INT *icol1, MKL_INT *istart, MKL_INT *istop, MKL_INT *itmp1, MKL_INT *itmp2, MKL_INT *liloz, MKL_INT *lihiz, float *vecs, float *v2, float *v3, float *t1, float *t2, float *t3);
void	DLAREF(char *type, double *a, MKL_INT *lda, MKL_INT *wantz, double *z, MKL_INT *ldz, MKL_INT *block, MKL_INT *irow1, MKL_INT *icol1, MKL_INT *istart, MKL_INT *istop, MKL_INT *itmp1, MKL_INT *itmp2, MKL_INT *liloz, MKL_INT *lihiz, double *vecs, double *v2, double *v3, double *t1, double *t2, double *t3);

void	SLASORTE(float *s, MKL_INT *lds, MKL_INT *j, float *out, MKL_INT *info);
void	DLASORTE(double *s, MKL_INT *lds, MKL_INT *j, double *out, MKL_INT *info);

void	SLASRT2(char *id, MKL_INT *n, float *d, MKL_INT *key, MKL_INT *info);
void	DLASRT2(char *id, MKL_INT *n, double *d, MKL_INT *key, MKL_INT *info);

void	SSTEIN2(MKL_INT *n, float *d, float *e, MKL_INT *m, float *w, MKL_INT *iblock, MKL_INT *isplit, float *orfac, float *z, MKL_INT *ldz, float *work, MKL_INT *iwork, MKL_INT *ifail, MKL_INT *info);
void	DSTEIN2(MKL_INT *n, double *d, double *e, MKL_INT *m, double *w, MKL_INT *iblock, MKL_INT *isplit, double *orfac, double *z, MKL_INT *ldz, double *work, MKL_INT *iwork, MKL_INT *ifail, MKL_INT *info);

void	SDBTF2(MKL_INT *m, MKL_INT *n, MKL_INT *kl, MKL_INT *ku, float *ab, MKL_INT *ldab, MKL_INT *info);
void	DDBTF2(MKL_INT *m, MKL_INT *n, MKL_INT *kl, MKL_INT *ku, double *ab, MKL_INT *ldab, MKL_INT *info);
void	CDBTF2(MKL_INT *m, MKL_INT *n, MKL_INT *kl, MKL_INT *ku, MKL_Complex8 *ab, MKL_INT *ldab, MKL_INT *info);
void	ZDBTF2(MKL_INT *m, MKL_INT *n, MKL_INT *kl, MKL_INT *ku, MKL_Complex16 *ab, MKL_INT *ldab, MKL_INT *info);

void	SDBTRF(MKL_INT *m, MKL_INT *n, MKL_INT *kl, MKL_INT *ku, float *ab, MKL_INT *ldab, MKL_INT *info);
void	DDBTRF(MKL_INT *m, MKL_INT *n, MKL_INT *kl, MKL_INT *ku, double *ab, MKL_INT *ldab, MKL_INT *info);
void	CDBTRF(MKL_INT *m, MKL_INT *n, MKL_INT *kl, MKL_INT *ku, MKL_Complex8 *ab, MKL_INT *ldab, MKL_INT *info);
void	ZDBTRF(MKL_INT *m, MKL_INT *n, MKL_INT *kl, MKL_INT *ku, MKL_Complex16 *ab, MKL_INT *ldab, MKL_INT *info);

void	SDTTRF(MKL_INT *n, float *dl, float *d, float *du, MKL_INT *info);
void	DDTTRF(MKL_INT *n, double *dl, double *d, double *du, MKL_INT *info);
void	CDTTRF(MKL_INT *n, MKL_Complex8 *dl, MKL_Complex8 *d, MKL_Complex8 *du, MKL_INT *info);
void	ZDTTRF(MKL_INT *n, MKL_Complex16 *dl, MKL_Complex16 *d, MKL_Complex16 *du, MKL_INT *info);

void	SDTTRSV(char *uplo, char *trans, MKL_INT *n, MKL_INT *nrhs, float *dl, float *d, float *du, float *b, MKL_INT *ldb, MKL_INT *info);
void	DDTTRSV(char *uplo, char *trans, MKL_INT *n, MKL_INT *nrhs, double *dl, double *d, double *du, double *b, MKL_INT *ldb, MKL_INT *info);
void	CDTTRSV(char *uplo, char *trans, MKL_INT *n, MKL_INT *nrhs, MKL_Complex8 *dl, MKL_Complex8 *d, MKL_Complex8 *du, MKL_Complex8 *b, MKL_INT *ldb, MKL_INT *info);
void	ZDTTRSV(char *uplo, char *trans, MKL_INT *n, MKL_INT *nrhs, MKL_Complex16 *dl, MKL_Complex16 *d, MKL_Complex16 *du, MKL_Complex16 *b, MKL_INT *ldb, MKL_INT *info);

void	SPTTRSV(char *trans, MKL_INT *n, MKL_INT *nrhs, float *d, float *e, float *b, MKL_INT *ldb, MKL_INT *info);
void	DPTTRSV(char *trans, MKL_INT *n, MKL_INT *nrhs, double *d, double *e, double *b, MKL_INT *ldb, MKL_INT *info);
void	CPTTRSV(char *uplo, char *trans, MKL_INT *n, MKL_INT *nrhs, float *d, MKL_Complex8 *e, MKL_Complex8 *b, MKL_INT *ldb, MKL_INT *info);
void	ZPTTRSV(char *uplo, char *trans, MKL_INT *n, MKL_INT *nrhs, double *d, MKL_Complex16 *e, MKL_Complex16 *b, MKL_INT *ldb, MKL_INT *info);

void	SSTEQR2(char *compz, MKL_INT *n, float *d, float *e, float *z, MKL_INT *ldz, MKL_INT *nr, float *work, MKL_INT *info);
void	DSTEQR2(char *compz, MKL_INT *n, double *d, double *e, double *z, MKL_INT *ldz, MKL_INT *nr, double *work, MKL_INT *info);

void	PSLABAD(MKL_INT *ictxt, float *small, float *large);
void	PDLABAD(MKL_INT *ictxt, double *small, double *large);

void	PSTRAN(MKL_INT *m, MKL_INT *n, float *alpha, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *beta, float *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc);
void	PDTRAN(MKL_INT *m, MKL_INT *n, double *alpha, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *beta, double *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc);

void	PSLACHKIEEE(MKL_INT *isieee, float *rmax, float *rmin);
void	PDLACHKIEEE(MKL_INT *isieee, float *rmax, float *rmin);

float	PSLAMCH(MKL_INT *ictxt, char *cmach);
double	PDLAMCH(MKL_INT *ictxt, char *cmach);

void	PSLASNBT(MKL_INT *ieflag);
void	PDLASNBT(MKL_INT *ieflag);

void	PXERBLA(MKL_INT *ictxt, char *srname, MKL_INT *info);


/* _<NAME> declarations */

void	_PSGETRF(MKL_INT *m, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, MKL_INT *info);
void	_PDGETRF(MKL_INT *m, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, MKL_INT *info);
void	_PCGETRF(MKL_INT *m, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, MKL_INT *info);
void	_PZGETRF(MKL_INT *m, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, MKL_INT *info);

void	_PSGBTRF(MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, float *a, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, float *af, MKL_INT *laf, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDGBTRF(MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, double *a, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, double *af, MKL_INT *laf, double *work, MKL_INT *lwork, MKL_INT *info);
void	_PCGBTRF(MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, MKL_Complex8 *a, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, MKL_Complex8 *af, MKL_INT *laf, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZGBTRF(MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, MKL_Complex16 *a, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, MKL_Complex16 *af, MKL_INT *laf, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSDBTRF(MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, float *a, MKL_INT *ja, MKL_INT *desca, float *af, MKL_INT *laf, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDDBTRF(MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, double *a, MKL_INT *ja, MKL_INT *desca, double *af, MKL_INT *laf, double *work, MKL_INT *lwork, MKL_INT *info);
void	_PCDBTRF(MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, MKL_Complex8 *a, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *af, MKL_INT *laf, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZDBTRF(MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, MKL_Complex16 *a, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *af, MKL_INT *laf, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSPOTRF(char *uplo, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *info);
void	_PDPOTRF(char *uplo, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *info);
void	_PCPOTRF(char *uplo, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *info);
void	_PZPOTRF(char *uplo, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *info);

void	_PSPBTRF(char *uplo, MKL_INT *n, MKL_INT *bw, float *a, MKL_INT *ja, MKL_INT *desca, float *af, MKL_INT *laf, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDPBTRF(char *uplo, MKL_INT *n, MKL_INT *bw, double *a, MKL_INT *ja, MKL_INT *desca, double *af, MKL_INT *laf, double *work, MKL_INT *lwork, MKL_INT *info);
void	_PCPBTRF(char *uplo, MKL_INT *n, MKL_INT *bw, MKL_Complex8 *a, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *af, MKL_INT *laf, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZPBTRF(char *uplo, MKL_INT *n, MKL_INT *bw, MKL_Complex16 *a, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *af, MKL_INT *laf, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSPTTRF(MKL_INT *n, float *d, float *e, MKL_INT *ja, MKL_INT *desca, float *af, MKL_INT *laf, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDPTTRF(MKL_INT *n, double *d, double *e, MKL_INT *ja, MKL_INT *desca, double *af, MKL_INT *laf, double *work, MKL_INT *lwork, MKL_INT *info);
void	_PCPTTRF(MKL_INT *n, float *d, MKL_Complex8 *e, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *af, MKL_INT *laf, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZPTTRF(MKL_INT *n, double *d, MKL_Complex16 *e, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *af, MKL_INT *laf, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSDTTRF(MKL_INT *n, float *dl, float *d, float *du, MKL_INT *ja, MKL_INT *desca, float *af, MKL_INT *laf, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDDTTRF(MKL_INT *n, double *dl, double *d, double *du, MKL_INT *ja, MKL_INT *desca, double *af, MKL_INT *laf, double *work, MKL_INT *lwork, MKL_INT *info);
void	_PCDTTRF(MKL_INT *n, MKL_Complex8 *dl, MKL_Complex8 *d, MKL_Complex8 *du, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *af, MKL_INT *laf, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZDTTRF(MKL_INT *n, MKL_Complex16 *dl, MKL_Complex16 *d, MKL_Complex16 *du, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *af, MKL_INT *laf, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSGETRS(char *trans, MKL_INT *n, MKL_INT *nrhs, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, float *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_INT *info);
void	_PDGETRS(char *trans, MKL_INT *n, MKL_INT *nrhs, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, double *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_INT *info);
void	_PCGETRS(char *trans, MKL_INT *n, MKL_INT *nrhs, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_INT *info);
void	_PZGETRS(char *trans, MKL_INT *n, MKL_INT *nrhs, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_INT *info);

void	_PSGBTRS(char *trans, MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, MKL_INT *nrhs, float *a, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, float *b, MKL_INT *ib, MKL_INT *descb, float *af, MKL_INT *laf, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDGBTRS(char *trans, MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, MKL_INT *nrhs, double *a, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, double *b, MKL_INT *ib, MKL_INT *descb, double *af, MKL_INT *laf, double *work, MKL_INT *lwork, MKL_INT *info);
void	_PCGBTRS(char *trans, MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, MKL_INT *nrhs, MKL_Complex8 *a, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex8 *af, MKL_INT *laf, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZGBTRS(char *trans, MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, MKL_INT *nrhs, MKL_Complex16 *a, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex16 *af, MKL_INT *laf, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSPOTRS(char *uplo, MKL_INT *n, MKL_INT *nrhs, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_INT *info);
void	_PDPOTRS(char *uplo, MKL_INT *n, MKL_INT *nrhs, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_INT *info);
void	_PCPOTRS(char *uplo, MKL_INT *n, MKL_INT *nrhs, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_INT *info);
void	_PZPOTRS(char *uplo, MKL_INT *n, MKL_INT *nrhs, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_INT *info);

void	_PSPBTRS(char *uplo, MKL_INT *n, MKL_INT *bw, MKL_INT *nrhs, float *a, MKL_INT *ja, MKL_INT *desca, float *b, MKL_INT *ib, MKL_INT *descb, float *af, MKL_INT *laf, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDPBTRS(char *uplo, MKL_INT *n, MKL_INT *bw, MKL_INT *nrhs, double *a, MKL_INT *ja, MKL_INT *desca, double *b, MKL_INT *ib, MKL_INT *descb, double *af, MKL_INT *laf, double *work, MKL_INT *lwork, MKL_INT *info);
void	_PCPBTRS(char *uplo, MKL_INT *n, MKL_INT *bw, MKL_INT *nrhs, MKL_Complex8 *a, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex8 *af, MKL_INT *laf, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZPBTRS(char *uplo, MKL_INT *n, MKL_INT *bw, MKL_INT *nrhs, MKL_Complex16 *a, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex16 *af, MKL_INT *laf, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSPTTRS(MKL_INT *n, MKL_INT *nrhs, float *d, float *e, MKL_INT *ja, MKL_INT *desca, float *b, MKL_INT *ib, MKL_INT *descb, float *af, MKL_INT *laf, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDPTTRS(MKL_INT *n, MKL_INT *nrhs, double *d, double *e, MKL_INT *ja, MKL_INT *desca, double *b, MKL_INT *ib, MKL_INT *descb, double *af, MKL_INT *laf, double *work, MKL_INT *lwork, MKL_INT *info);
void	_PCPTTRS(char *uplo, MKL_INT *n, MKL_INT *nrhs, float *d, MKL_Complex8 *e, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex8 *af, MKL_INT *laf, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZPTTRS(char *uplo, MKL_INT *n, MKL_INT *nrhs, double *d, MKL_Complex16 *e, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex16 *af, MKL_INT *laf, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSDTTRS(char *trans, MKL_INT *n, MKL_INT *nrhs, float *dl, float *d, float *du, MKL_INT *ja, MKL_INT *desca, float *b, MKL_INT *ib, MKL_INT *descb, float *af, MKL_INT *laf, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDDTTRS(char *trans, MKL_INT *n, MKL_INT *nrhs, double *dl, double *d, double *du, MKL_INT *ja, MKL_INT *desca, double *b, MKL_INT *ib, MKL_INT *descb, double *af, MKL_INT *laf, double *work, MKL_INT *lwork, MKL_INT *info);
void	_PCDTTRS(char *trans, MKL_INT *n, MKL_INT *nrhs, MKL_Complex8 *dl, MKL_Complex8 *d, MKL_Complex8 *du, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex8 *af, MKL_INT *laf, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZDTTRS(char *trans, MKL_INT *n, MKL_INT *nrhs, MKL_Complex16 *dl, MKL_Complex16 *d, MKL_Complex16 *du, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex16 *af, MKL_INT *laf, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSDBTRS(char *trans, MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, MKL_INT *nrhs, float *a, MKL_INT *ja, MKL_INT *desca, float *b, MKL_INT *ib, MKL_INT *descb, float *af, MKL_INT *laf, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDDBTRS(char *trans, MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, MKL_INT *nrhs, double *a, MKL_INT *ja, MKL_INT *desca, double *b, MKL_INT *ib, MKL_INT *descb, double *af, MKL_INT *laf, double *work, MKL_INT *lwork, MKL_INT *info);
void	_PCDBTRS(char *trans, MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, MKL_INT *nrhs, MKL_Complex8 *a, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex8 *af, MKL_INT *laf, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZDBTRS(char *trans, MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, MKL_INT *nrhs, MKL_Complex16 *a, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex16 *af, MKL_INT *laf, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSTRTRS(char *uplo, char *trans, char *diag, MKL_INT *n, MKL_INT *nrhs, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_INT *info);
void	_PDTRTRS(char *uplo, char *trans, char *diag, MKL_INT *n, MKL_INT *nrhs, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_INT *info);
void	_PCTRTRS(char *uplo, char *trans, char *diag, MKL_INT *n, MKL_INT *nrhs, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_INT *info);
void	_PZTRTRS(char *uplo, char *trans, char *diag, MKL_INT *n, MKL_INT *nrhs, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_INT *info);

void	_PSGECON(char *norm, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *anorm, float *rcond, float *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *info);
void	_PDGECON(char *norm, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *anorm, double *rcond, double *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *info);
void	_PCGECON(char *norm, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *anorm, float *rcond, MKL_Complex8 *work, MKL_INT *lwork, float *rwork, MKL_INT *lrwork, MKL_INT *info);
void	_PZGECON(char *norm, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *anorm, double *rcond, MKL_Complex16 *work, MKL_INT *lwork, double *rwork, MKL_INT *lrwork, MKL_INT *info);

void	_PSPOCON(char *uplo, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *anorm, float *rcond, float *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *info);
void	_PDPOCON(char *uplo, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *anorm, double *rcond, double *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *info);
void	_PCPOCON(char *uplo, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *anorm, float *rcond, MKL_Complex8 *work, MKL_INT *lwork, float *rwork, MKL_INT *lrwork, MKL_INT *info);
void	_PZPOCON(char *uplo, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *anorm, double *rcond, MKL_Complex16 *work, MKL_INT *lwork, double *rwork, MKL_INT *lrwork, MKL_INT *info);

void	_PSTRCON(char *norm, char *uplo, char *diag, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *rcond, float *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *info);
void	_PDTRCON(char *norm, char *uplo, char *diag, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *rcond, double *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *info);
void	_PCTRCON(char *norm, char *uplo, char *diag, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *rcond, MKL_Complex8 *work, MKL_INT *lwork, float *rwork, MKL_INT *lrwork, MKL_INT *info);
void	_PZTRCON(char *norm, char *uplo, char *diag, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *rcond, MKL_Complex16 *work, MKL_INT *lwork, double *rwork, MKL_INT *lrwork, MKL_INT *info);

void	_PSGERFS(char *trans, MKL_INT *n, MKL_INT *nrhs, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *af, MKL_INT *iaf, MKL_INT *jaf, MKL_INT *descaf, MKL_INT *ipiv, float *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, float *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, float *ferr, float *berr, float *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *info);
void	_PDGERFS(char *trans, MKL_INT *n, MKL_INT *nrhs, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *af, MKL_INT *iaf, MKL_INT *jaf, MKL_INT *descaf, MKL_INT *ipiv, double *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, double *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, double *ferr, double *berr, double *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *info);
void	_PCGERFS(char *trans, MKL_INT *n, MKL_INT *nrhs, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *af, MKL_INT *iaf, MKL_INT *jaf, MKL_INT *descaf, MKL_INT *ipiv, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_Complex8 *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, float *ferr, float *berr, MKL_Complex8 *work, MKL_INT *lwork, float *rwork, MKL_INT *lrwork, MKL_INT *info);
void	_PZGERFS(char *trans, MKL_INT *n, MKL_INT *nrhs, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *af, MKL_INT *iaf, MKL_INT *jaf, MKL_INT *descaf, MKL_INT *ipiv, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_Complex16 *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, double *ferr, double *berr, MKL_Complex16 *work, MKL_INT *lwork, double *rwork, MKL_INT *lrwork, MKL_INT *info);

void	_PSPORFS(char *uplo, MKL_INT *n, MKL_INT *nrhs, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *af, MKL_INT *iaf, MKL_INT *jaf, MKL_INT *descaf, float *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, float *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, float *ferr, float *berr, float *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *info);
void	_PDPORFS(char *uplo, MKL_INT *n, MKL_INT *nrhs, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *af, MKL_INT *iaf, MKL_INT *jaf, MKL_INT *descaf, double *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, double *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, double *ferr, double *berr, double *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *info);
void	_PCPORFS(char *uplo, MKL_INT *n, MKL_INT *nrhs, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *af, MKL_INT *iaf, MKL_INT *jaf, MKL_INT *descaf, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_Complex8 *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, float *ferr, float *berr, MKL_Complex8 *work, MKL_INT *lwork, float *rwork, MKL_INT *lrwork, MKL_INT *info);
void	_PZPORFS(char *uplo, MKL_INT *n, MKL_INT *nrhs, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *af, MKL_INT *iaf, MKL_INT *jaf, MKL_INT *descaf, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_Complex16 *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, double *ferr, double *berr, MKL_Complex16 *work, MKL_INT *lwork, double *rwork, MKL_INT *lrwork, MKL_INT *info);

void	_PSTRRFS(char *uplo, char *trans, char *diag, MKL_INT *n, MKL_INT *nrhs, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, float *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, float *ferr, float *berr, float *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *info);
void	_PDTRRFS(char *uplo, char *trans, char *diag, MKL_INT *n, MKL_INT *nrhs, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, double *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, double *ferr, double *berr, double *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *info);
void	_PCTRRFS(char *uplo, char *trans, char *diag, MKL_INT *n, MKL_INT *nrhs, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_Complex8 *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, float *ferr, float *berr, MKL_Complex8 *work, MKL_INT *lwork, float *rwork, MKL_INT *lrwork, MKL_INT *info);
void	_PZTRRFS(char *uplo, char *trans, char *diag, MKL_INT *n, MKL_INT *nrhs, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_Complex16 *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, double *ferr, double *berr, MKL_Complex16 *work, MKL_INT *lwork, double *rwork, MKL_INT *lrwork, MKL_INT *info);

void	_PSGETRI(MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, float *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *info);
void	_PDGETRI(MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, double *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *info);
void	_PCGETRI(MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *info);
void	_PZGETRI(MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *info);

void	_PSPOTRI(char *uplo, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *info);
void	_PDPOTRI(char *uplo, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *info);
void	_PCPOTRI(char *uplo, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *info);
void	_PZPOTRI(char *uplo, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *info);

void	_PSTRTRI(char *uplo, char *diag, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *info);
void	_PDTRTRI(char *uplo, char *diag, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *info);
void	_PCTRTRI(char *uplo, char *diag, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *info);
void	_PZTRTRI(char *uplo, char *diag, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *info);

void	_PSGEEQU(MKL_INT *m, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *r, float *c, float *rowcnd, float *colcnd, float *amax, MKL_INT *info);
void	_PDGEEQU(MKL_INT *m, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *r, double *c, double *rowcnd, double *colcnd, double *amax, MKL_INT *info);
void	_PCGEEQU(MKL_INT *m, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *r, float *c, float *rowcnd, float *colcnd, float *amax, MKL_INT *info);
void	_PZGEEQU(MKL_INT *m, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *r, double *c, double *rowcnd, double *colcnd, double *amax, MKL_INT *info);

void	_PSPOEQU(MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *sr, float *sc, float *scond, float *amax, MKL_INT *info);
void	_PDPOEQU(MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *sr, double *sc, double *scond, double *amax, MKL_INT *info);
void	_PCPOEQU(MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *sr, float *sc, float *scond, float *amax, MKL_INT *info);
void	_PZPOEQU(MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *sr, double *sc, double *scond, double *amax, MKL_INT *info);

void	_PSGEQRF(MKL_INT *m, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDGEQRF(MKL_INT *m, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *work, MKL_INT *lwork, MKL_INT *info);
void	_PCGEQRF(MKL_INT *m, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZGEQRF(MKL_INT *m, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSGEQPF(MKL_INT *m, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, float *tau, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDGEQPF(MKL_INT *m, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, double *tau, double *work, MKL_INT *lwork, MKL_INT *info);
void	_PCGEQPF(MKL_INT *m, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, MKL_Complex8 *tau, MKL_Complex8 *work, MKL_INT *lwork, float *rwork, MKL_INT *lrwork, MKL_INT *info);
void	_PZGEQPF(MKL_INT *m, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, MKL_Complex16 *tau, MKL_Complex16 *work, MKL_INT *lwork, double *rwork, MKL_INT *lrwork, MKL_INT *info);

void	_PSORGQR(MKL_INT *m, MKL_INT *n, MKL_INT *k, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDORGQR(MKL_INT *m, MKL_INT *n, MKL_INT *k, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *work, MKL_INT *lwork, MKL_INT *info);

void	_PCUNGQR(MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZUNGQR(MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSORMQR(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDORMQR(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, double *work, MKL_INT *lwork, MKL_INT *info);

void	_PCUNMQR(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZUNMQR(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSGELQF(MKL_INT *m, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDGELQF(MKL_INT *m, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *work, MKL_INT *lwork, MKL_INT *info);
void	_PCGELQF(MKL_INT *m, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZGELQF(MKL_INT *m, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSORGLQ(MKL_INT *m, MKL_INT *n, MKL_INT *k, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDORGLQ(MKL_INT *m, MKL_INT *n, MKL_INT *k, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *work, MKL_INT *lwork, MKL_INT *info);

void	_PCUNGLQ(MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZUNGLQ(MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSORMLQ(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDORMLQ(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, double *work, MKL_INT *lwork, MKL_INT *info);

void	_PCUNMLQ(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZUNMLQ(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSGEQLF(MKL_INT *m, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDGEQLF(MKL_INT *m, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *work, MKL_INT *lwork, MKL_INT *info);
void	_PCGEQLF(MKL_INT *m, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZGEQLF(MKL_INT *m, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSORGQL(MKL_INT *m, MKL_INT *n, MKL_INT *k, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDORGQL(MKL_INT *m, MKL_INT *n, MKL_INT *k, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *work, MKL_INT *lwork, MKL_INT *info);

void	_PCUNGQL(MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZUNGQL(MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSORMQL(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDORMQL(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, double *work, MKL_INT *lwork, MKL_INT *info);

void	_PCUNMQL(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZUNMQL(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSGERQF(MKL_INT *m, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDGERQF(MKL_INT *m, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *work, MKL_INT *lwork, MKL_INT *info);
void	_PCGERQF(MKL_INT *m, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZGERQF(MKL_INT *m, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSORGRQ(MKL_INT *m, MKL_INT *n, MKL_INT *k, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDORGRQ(MKL_INT *m, MKL_INT *n, MKL_INT *k, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *work, MKL_INT *lwork, MKL_INT *info);

void	_PCUNGRQ(MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZUNGRQ(MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSORMRQ(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDORMRQ(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, double *work, MKL_INT *lwork, MKL_INT *info);

void	_PCUNMRQ(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZUNMRQ(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSTZRZF(MKL_INT *m, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDTZRZF(MKL_INT *m, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *work, MKL_INT *lwork, MKL_INT *info);
void	_PCTZRZF(MKL_INT *m, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZTZRZF(MKL_INT *m, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSORMRZ(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_INT *l, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDORMRZ(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_INT *l, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, double *work, MKL_INT *lwork, MKL_INT *info);

void	_PCUNMRZ(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_INT *l, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZUNMRZ(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_INT *l, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSGGQRF(MKL_INT *n, MKL_INT *m, MKL_INT *p, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *taua, float *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, float *taub, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDGGQRF(MKL_INT *n, MKL_INT *m, MKL_INT *p, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *taua, double *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, double *taub, double *work, MKL_INT *lwork, MKL_INT *info);
void	_PCGGQRF(MKL_INT *n, MKL_INT *m, MKL_INT *p, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *taua, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_Complex8 *taub, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZGGQRF(MKL_INT *n, MKL_INT *m, MKL_INT *p, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *taua, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_Complex16 *taub, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSGGRQF(MKL_INT *m, MKL_INT *p, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *taua, float *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, float *taub, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDGGRQF(MKL_INT *m, MKL_INT *p, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *taua, double *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, double *taub, double *work, MKL_INT *lwork, MKL_INT *info);
void	_PCGGRQF(MKL_INT *m, MKL_INT *p, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *taua, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_Complex8 *taub, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZGGRQF(MKL_INT *m, MKL_INT *p, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *taua, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_Complex16 *taub, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSSYTRD(char *uplo, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *d, float *e, float *tau, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDSYTRD(char *uplo, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *d, double *e, double *tau, double *work, MKL_INT *lwork, MKL_INT *info);

void	_PSORMTR(char *side, char *uplo, char *trans, MKL_INT *m, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDORMTR(char *side, char *uplo, char *trans, MKL_INT *m, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, double *work, MKL_INT *lwork, MKL_INT *info);

void	_PCHETRD(char *uplo, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *d, float *e, MKL_Complex8 *tau, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZHETRD(char *uplo, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *d, double *e, MKL_Complex16 *tau, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PCUNMTR(char *side, char *uplo, char *trans, MKL_INT *m, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZUNMTR(char *side, char *uplo, char *trans, MKL_INT *m, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSSTEBZ(MKL_INT *ictxt, char *range, char *order, MKL_INT *n, float *vl, float *vu, MKL_INT *il, MKL_INT *iu, float *abstol, float *d, float *e, MKL_INT *m, MKL_INT *nsplit, float *w, MKL_INT *iblock, MKL_INT *isplit, float *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *info);
void	_PDSTEBZ(MKL_INT *ictxt, char *range, char *order, MKL_INT *n, double *vl, double *vu, MKL_INT *il, MKL_INT *iu, double *abstol, double *d, double *e, MKL_INT *m, MKL_INT *nsplit, double *w, MKL_INT *iblock, MKL_INT *isplit, double *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *info);

void	_PSSTEIN(MKL_INT *n, float *d, float *e, MKL_INT *m, float *w, MKL_INT *iblock, MKL_INT *isplit, float *orfac, float *z, MKL_INT *iz, MKL_INT *jz, MKL_INT *descz, float *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *ifail, MKL_INT *iclustr, float *gap, MKL_INT *info);
void	_PDSTEIN(MKL_INT *n, double *d, double *e, MKL_INT *m, double *w, MKL_INT *iblock, MKL_INT *isplit, double *orfac, double *z, MKL_INT *iz, MKL_INT *jz, MKL_INT *descz, double *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *ifail, MKL_INT *iclustr, double *gap, MKL_INT *info);
void	_PCSTEIN(MKL_INT *n, float *d, float *e, MKL_INT *m, float *w, MKL_INT *iblock, MKL_INT *isplit, float *orfac, MKL_Complex8 *z, MKL_INT *iz, MKL_INT *jz, MKL_INT *descz, float *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *ifail, MKL_INT *iclustr, float *gap, MKL_INT *info);
void	_PZSTEIN(MKL_INT *n, double *d, double *e, MKL_INT *m, double *w, MKL_INT *iblock, MKL_INT *isplit, double *orfac, MKL_Complex16 *z, MKL_INT *iz, MKL_INT *jz, MKL_INT *descz, double *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *ifail, MKL_INT *iclustr, double *gap, MKL_INT *info);

void	_PSGEHRD(MKL_INT *n, MKL_INT *ilo, MKL_INT *ihi, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDGEHRD(MKL_INT *n, MKL_INT *ilo, MKL_INT *ihi, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *work, MKL_INT *lwork, MKL_INT *info);
void	_PCGEHRD(MKL_INT *n, MKL_INT *ilo, MKL_INT *ihi, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZGEHRD(MKL_INT *n, MKL_INT *ilo, MKL_INT *ihi, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSORMHR(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *ilo, MKL_INT *ihi, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDORMHR(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *ilo, MKL_INT *ihi, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, double *work, MKL_INT *lwork, MKL_INT *info);

void	_PCUNMHR(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *ilo, MKL_INT *ihi, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZUNMHR(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *ilo, MKL_INT *ihi, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSLAHQR(MKL_INT *wantt, MKL_INT *wantz, MKL_INT *n, MKL_INT *ilo, MKL_INT *ihi, float *a, MKL_INT *desca, float *wr, float *wi, MKL_INT *iloz, MKL_INT *ihiz, float *z, MKL_INT *descz, float *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *ilwork, MKL_INT *info);
void	_PDLAHQR(MKL_INT *wantt, MKL_INT *wantz, MKL_INT *n, MKL_INT *ilo, MKL_INT *ihi, double *a, MKL_INT *desca, double *wr, double *wi, MKL_INT *iloz, MKL_INT *ihiz, double *z, MKL_INT *descz, double *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *ilwork, MKL_INT *info);

void	_PSGEBRD(MKL_INT *m, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *d, float *e, float *tauq, float *taup, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDGEBRD(MKL_INT *m, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *d, double *e, double *tauq, double *taup, double *work, MKL_INT *lwork, MKL_INT *info);
void	_PCGEBRD(MKL_INT *m, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *d, float *e, MKL_Complex8 *tauq, MKL_Complex8 *taup, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZGEBRD(MKL_INT *m, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *d, double *e, MKL_Complex16 *tauq, MKL_Complex16 *taup, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSORMBR(char *vect, char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDORMBR(char *vect, char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, double *work, MKL_INT *lwork, MKL_INT *info);

void	_PCUNMBR(char *vect, char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZUNMBR(char *vect, char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSSYGST(MKL_INT *ibtype, char *uplo, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, float *scale, MKL_INT *info);
void	_PDSYGST(MKL_INT *ibtype, char *uplo, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, double *scale, MKL_INT *info);

void	_PCHEGST(MKL_INT *ibtype, char *uplo, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, float *scale, MKL_INT *info);
void	_PZHEGST(MKL_INT *ibtype, char *uplo, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, double *scale, MKL_INT *info);

void	_PSGESV(MKL_INT *n, MKL_INT *nrhs, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, float *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_INT *info);
void	_PDGESV(MKL_INT *n, MKL_INT *nrhs, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, double *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_INT *info);
void	_PCGESV(MKL_INT *n, MKL_INT *nrhs, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_INT *info);
void	_PZGESV(MKL_INT *n, MKL_INT *nrhs, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_INT *info);

void	_PSGESVX(char *fact, char *trans, MKL_INT *n, MKL_INT *nrhs, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *af, MKL_INT *iaf, MKL_INT *jaf, MKL_INT *descaf, MKL_INT *ipiv, char *equed, float *r, float *c, float *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, float *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, float *rcond, float *ferr, float *berr, float *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *info);
void	_PDGESVX(char *fact, char *trans, MKL_INT *n, MKL_INT *nrhs, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *af, MKL_INT *iaf, MKL_INT *jaf, MKL_INT *descaf, MKL_INT *ipiv, char *equed, double *r, double *c, double *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, double *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, double *rcond, double *ferr, double *berr, double *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *info);
void	_PCGESVX(char *fact, char *trans, MKL_INT *n, MKL_INT *nrhs, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *af, MKL_INT *iaf, MKL_INT *jaf, MKL_INT *descaf, MKL_INT *ipiv, char *equed, float *r, float *c, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_Complex8 *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, float *rcond, float *ferr, float *berr, MKL_Complex8 *work, MKL_INT *lwork, float *rwork, MKL_INT *lrwork, MKL_INT *info);
void	_PZGESVX(char *fact, char *trans, MKL_INT *n, MKL_INT *nrhs, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *af, MKL_INT *iaf, MKL_INT *jaf, MKL_INT *descaf, MKL_INT *ipiv, char *equed, double *r, double *c, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_Complex16 *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, double *rcond, double *ferr, double *berr, MKL_Complex16 *work, MKL_INT *lwork, double *rwork, MKL_INT *lrwork, MKL_INT *info);

void	_PSGBSV(MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, MKL_INT *nrhs, float *a, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, float *b, MKL_INT *ib, MKL_INT *descb, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDGBSV(MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, MKL_INT *nrhs, double *a, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, double *b, MKL_INT *ib, MKL_INT *descb, double *work, MKL_INT *lwork, MKL_INT *info);
void	_PCGBSV(MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, MKL_INT *nrhs, MKL_Complex8 *a, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZGBSV(MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, MKL_INT *nrhs, MKL_Complex16 *a, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSDBSV(MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, MKL_INT *nrhs, float *a, MKL_INT *ja, MKL_INT *desca, float *b, MKL_INT *ib, MKL_INT *descb, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDDBSV(MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, MKL_INT *nrhs, double *a, MKL_INT *ja, MKL_INT *desca, double *b, MKL_INT *ib, MKL_INT *descb, double *work, MKL_INT *lwork, MKL_INT *info);
void	_PCDBSV(MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, MKL_INT *nrhs, MKL_Complex8 *a, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZDBSV(MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, MKL_INT *nrhs, MKL_Complex16 *a, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSDTSV(MKL_INT *n, MKL_INT *nrhs, float *dl, float *d, float *du, MKL_INT *ja, MKL_INT *desca, float *b, MKL_INT *ib, MKL_INT *descb, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDDTSV(MKL_INT *n, MKL_INT *nrhs, double *dl, double *d, double *du, MKL_INT *ja, MKL_INT *desca, double *b, MKL_INT *ib, MKL_INT *descb, double *work, MKL_INT *lwork, MKL_INT *info);
void	_PCDTSV(MKL_INT *n, MKL_INT *nrhs, MKL_Complex8 *dl, MKL_Complex8 *d, MKL_Complex8 *du, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZDTSV(MKL_INT *n, MKL_INT *nrhs, MKL_Complex16 *dl, MKL_Complex16 *d, MKL_Complex16 *du, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSPOSV(char *uplo, MKL_INT *n, MKL_INT *nrhs, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_INT *info);
void	_PDPOSV(char *uplo, MKL_INT *n, MKL_INT *nrhs, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_INT *info);
void	_PCPOSV(char *uplo, MKL_INT *n, MKL_INT *nrhs, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_INT *info);
void	_PZPOSV(char *uplo, MKL_INT *n, MKL_INT *nrhs, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_INT *info);

void	_PSPOSVX(char *fact, char *uplo, MKL_INT *n, MKL_INT *nrhs, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *af, MKL_INT *iaf, MKL_INT *jaf, MKL_INT *descaf, char *equed, float *sr, float *sc, float *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, float *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, float *rcond, float *ferr, float *berr, float *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *info);
void	_PDPOSVX(char *fact, char *uplo, MKL_INT *n, MKL_INT *nrhs, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *af, MKL_INT *iaf, MKL_INT *jaf, MKL_INT *descaf, char *equed, double *sr, double *sc, double *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, double *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, double *rcond, double *ferr, double *berr, double *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *info);
void	_PCPOSVX(char *fact, char *uplo, MKL_INT *n, MKL_INT *nrhs, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *af, MKL_INT *iaf, MKL_INT *jaf, MKL_INT *descaf, char *equed, float *sr, float *sc, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_Complex8 *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, float *rcond, float *ferr, float *berr, MKL_Complex8 *work, MKL_INT *lwork, float *rwork, MKL_INT *lrwork, MKL_INT *info);
void	_PZPOSVX(char *fact, char *uplo, MKL_INT *n, MKL_INT *nrhs, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *af, MKL_INT *iaf, MKL_INT *jaf, MKL_INT *descaf, char *equed, double *sr, double *sc, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_Complex16 *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, double *rcond, double *ferr, double *berr, MKL_Complex16 *work, MKL_INT *lwork, double *rwork, MKL_INT *lrwork, MKL_INT *info);

void	_PSPBSV(char *uplo, MKL_INT *n, MKL_INT *bw, MKL_INT *nrhs, float *a, MKL_INT *ja, MKL_INT *desca, float *b, MKL_INT *ib, MKL_INT *descb, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDPBSV(char *uplo, MKL_INT *n, MKL_INT *bw, MKL_INT *nrhs, double *a, MKL_INT *ja, MKL_INT *desca, double *b, MKL_INT *ib, MKL_INT *descb, double *work, MKL_INT *lwork, MKL_INT *info);
void	_PCPBSV(char *uplo, MKL_INT *n, MKL_INT *bw, MKL_INT *nrhs, MKL_Complex8 *a, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZPBSV(char *uplo, MKL_INT *n, MKL_INT *bw, MKL_INT *nrhs, MKL_Complex16 *a, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSPTSV(MKL_INT *n, MKL_INT *nrhs, float *d, float *e, MKL_INT *ja, MKL_INT *desca, float *b, MKL_INT *ib, MKL_INT *descb, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDPTSV(MKL_INT *n, MKL_INT *nrhs, double *d, double *e, MKL_INT *ja, MKL_INT *desca, double *b, MKL_INT *ib, MKL_INT *descb, double *work, MKL_INT *lwork, MKL_INT *info);
void	_PCPTSV(char *uplo, MKL_INT *n, MKL_INT *nrhs, float *d, MKL_Complex8 *e, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZPTSV(char *uplo, MKL_INT *n, MKL_INT *nrhs, double *d, MKL_Complex16 *e, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSGELS(char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *nrhs, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDGELS(char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *nrhs, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, double *work, MKL_INT *lwork, MKL_INT *info);
void	_PCGELS(char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *nrhs, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZGELS(char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *nrhs, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSSYEV(char *jobz, char *uplo, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *w, float *z, MKL_INT *iz, MKL_INT *jz, MKL_INT *descz, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDSYEV(char *jobz, char *uplo, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *w, double *z, MKL_INT *iz, MKL_INT *jz, MKL_INT *descz, double *work, MKL_INT *lwork, MKL_INT *info);

void	_PSSYEVX(char *jobz, char *range, char *uplo, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *vl, float *vu, MKL_INT *il, MKL_INT *iu, float *abstol, MKL_INT *m, MKL_INT *nz, float *w, float *orfac, float *z, MKL_INT *iz, MKL_INT *jz, MKL_INT *descz, float *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *ifail, MKL_INT *iclustr, float *gap, MKL_INT *info);
void	_PDSYEVX(char *jobz, char *range, char *uplo, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *vl, double *vu, MKL_INT *il, MKL_INT *iu, double *abstol, MKL_INT *m, MKL_INT *nz, double *w, double *orfac, double *z, MKL_INT *iz, MKL_INT *jz, MKL_INT *descz, double *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *ifail, MKL_INT *iclustr, double *gap, MKL_INT *info);

void	_PCHEEVX(char *jobz, char *range, char *uplo, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *vl, float *vu, MKL_INT *il, MKL_INT *iu, float *abstol, MKL_INT *m, MKL_INT *nz, float *w, float *orfac, MKL_Complex8 *z, MKL_INT *iz, MKL_INT *jz, MKL_INT *descz, MKL_Complex8 *work, MKL_INT *lwork, float *rwork, MKL_INT *lrwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *ifail, MKL_INT *iclustr, float *gap, MKL_INT *info);
void	_PZHEEVX(char *jobz, char *range, char *uplo, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *vl, double *vu, MKL_INT *il, MKL_INT *iu, double *abstol, MKL_INT *m, MKL_INT *nz, double *w, double *orfac, MKL_Complex16 *z, MKL_INT *iz, MKL_INT *jz, MKL_INT *descz, MKL_Complex16 *work, MKL_INT *lwork, double *rwork, MKL_INT *lrwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *ifail, MKL_INT *iclustr, double *gap, MKL_INT *info);

void	_PSGESVD(char *jobu, char *jobvt, MKL_INT *m, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *s, float *u, MKL_INT *iu, MKL_INT *ju, MKL_INT *descu, float *vt, MKL_INT *ivt, MKL_INT *jvt, MKL_INT *descvt, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDGESVD(char *jobu, char *jobvt, MKL_INT *m, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *s, double *u, MKL_INT *iu, MKL_INT *ju, MKL_INT *descu, double *vt, MKL_INT *ivt, MKL_INT *jvt, MKL_INT *descvt, double *work, MKL_INT *lwork, MKL_INT *info);
void	_PCGESVD(char *jobu, char *jobvt, MKL_INT *m, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *s, MKL_Complex8 *u, MKL_INT *iu, MKL_INT *ju, MKL_INT *descu, MKL_Complex8 *vt, MKL_INT *ivt, MKL_INT *jvt, MKL_INT *descvt, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZGESVD(char *jobu, char *jobvt, MKL_INT *m, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *s, MKL_Complex16 *u, MKL_INT *iu, MKL_INT *ju, MKL_INT *descu, MKL_Complex16 *vt, MKL_INT *ivt, MKL_INT *jvt, MKL_INT *descvt, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSSYGVX(MKL_INT *ibtype, char *jobz, char *range, char *uplo, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, float *vl, float *vu, MKL_INT *il, MKL_INT *iu, float *abstol, MKL_INT *m, MKL_INT *nz, float *w, float *orfac, float *z, MKL_INT *iz, MKL_INT *jz, MKL_INT *descz, float *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *ifail, MKL_INT *iclustr, float *gap, MKL_INT *info);
void	_PDSYGVX(MKL_INT *ibtype, char *jobz, char *range, char *uplo, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, double *vl, double *vu, MKL_INT *il, MKL_INT *iu, double *abstol, MKL_INT *m, MKL_INT *nz, double *w, double *orfac, double *z, MKL_INT *iz, MKL_INT *jz, MKL_INT *descz, double *work, MKL_INT *lwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *ifail, MKL_INT *iclustr, double *gap, MKL_INT *info);

void	_PCHEGVX(MKL_INT *ibtype, char *jobz, char *range, char *uplo, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, float *vl, float *vu, MKL_INT *il, MKL_INT *iu, float *abstol, MKL_INT *m, MKL_INT *nz, float *w, float *orfac, MKL_Complex8 *z, MKL_INT *iz, MKL_INT *jz, MKL_INT *descz, MKL_Complex8 *work, MKL_INT *lwork, float *rwork, MKL_INT *lrwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *ifail, MKL_INT *iclustr, float *gap, MKL_INT *info);
void	_PZHEGVX(MKL_INT *ibtype, char *jobz, char *range, char *uplo, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, double *vl, double *vu, MKL_INT *il, MKL_INT *iu, double *abstol, MKL_INT *m, MKL_INT *nz, double *w, double *orfac, MKL_Complex16 *z, MKL_INT *iz, MKL_INT *jz, MKL_INT *descz, MKL_Complex16 *work, MKL_INT *lwork, double *rwork, MKL_INT *lrwork, MKL_INT *iwork, MKL_INT *liwork, MKL_INT *ifail, MKL_INT *iclustr, double *gap, MKL_INT *info);

void	_PCLACGV(MKL_INT *n, MKL_Complex8 *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, MKL_INT *incx);
void	_PZLACGV(MKL_INT *n, MKL_Complex16 *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, MKL_INT *incx);

void	_PCMAX1(MKL_INT *n, MKL_Complex8 *amax, MKL_INT *indx, MKL_Complex8 *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, MKL_INT *incx);
void	_PZMAX1(MKL_INT *n, MKL_Complex16 *amax, MKL_INT *indx, MKL_Complex16 *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, MKL_INT *incx);

void	_CCOMBAMAX1(MKL_Complex8 *v1, MKL_Complex8 *v2);
void	_ZCOMBAMAX1(MKL_Complex16 *v1, MKL_Complex16 *v2);

void	_PSCSUM1(MKL_INT *n, float *asum, MKL_Complex8 *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, MKL_INT *incx);
void	_PDZSUM1(MKL_INT *n, double *asum, MKL_Complex16 *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, MKL_INT *incx);

void	_PSDBTRSV(char *uplo, char *trans, MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, MKL_INT *nrhs, float *a, MKL_INT *ja, MKL_INT *desca, float *b, MKL_INT *ib, MKL_INT *descb, float *af, MKL_INT *laf, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDDBTRSV(char *uplo, char *trans, MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, MKL_INT *nrhs, double *a, MKL_INT *ja, MKL_INT *desca, double *b, MKL_INT *ib, MKL_INT *descb, double *af, MKL_INT *laf, double *work, MKL_INT *lwork, MKL_INT *info);
void	_PCDBTRSV(char *uplo, char *trans, MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, MKL_INT *nrhs, MKL_Complex8 *a, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex8 *af, MKL_INT *laf, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZDBTRSV(char *uplo, char *trans, MKL_INT *n, MKL_INT *bwl, MKL_INT *bwu, MKL_INT *nrhs, MKL_Complex16 *a, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex16 *af, MKL_INT *laf, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSDTTRSV(char *uplo, char *trans, MKL_INT *n, MKL_INT *nrhs, float *dl, float *d, float *du, MKL_INT *ja, MKL_INT *desca, float *b, MKL_INT *ib, MKL_INT *descb, float *af, MKL_INT *laf, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDDTTRSV(char *uplo, char *trans, MKL_INT *n, MKL_INT *nrhs, double *dl, double *d, double *du, MKL_INT *ja, MKL_INT *desca, double *b, MKL_INT *ib, MKL_INT *descb, double *af, MKL_INT *laf, double *work, MKL_INT *lwork, MKL_INT *info);
void	_PCDTTRSV(char *uplo, char *trans, MKL_INT *n, MKL_INT *nrhs, MKL_Complex8 *dl, MKL_Complex8 *d, MKL_Complex8 *du, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex8 *af, MKL_INT *laf, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZDTTRSV(char *uplo, char *trans, MKL_INT *n, MKL_INT *nrhs, MKL_Complex16 *dl, MKL_Complex16 *d, MKL_Complex16 *du, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex16 *af, MKL_INT *laf, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSGEBD2(MKL_INT *m, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *d, float *e, float *tauq, float *taup, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDGEBD2(MKL_INT *m, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *d, double *e, double *tauq, double *taup, double *work, MKL_INT *lwork, MKL_INT *info);
void	_PCGEBD2(MKL_INT *m, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *d, float *e, MKL_Complex8 *tauq, MKL_Complex8 *taup, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZGEBD2(MKL_INT *m, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *d, double *e, MKL_Complex16 *tauq, MKL_Complex16 *taup, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSGEHD2(MKL_INT *n, MKL_INT *ilo, MKL_INT *ihi, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDGEHD2(MKL_INT *n, MKL_INT *ilo, MKL_INT *ihi, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *work, MKL_INT *lwork, MKL_INT *info);
void	_PCGEHD2(MKL_INT *n, MKL_INT *ilo, MKL_INT *ihi, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZGEHD2(MKL_INT *n, MKL_INT *ilo, MKL_INT *ihi, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSGELQ2(MKL_INT *m, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDGELQ2(MKL_INT *m, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *work, MKL_INT *lwork, MKL_INT *info);
void	_PCGELQ2(MKL_INT *m, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZGELQ2(MKL_INT *m, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSGEQL2(MKL_INT *m, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDGEQL2(MKL_INT *m, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *work, MKL_INT *lwork, MKL_INT *info);
void	_PCGEQL2(MKL_INT *m, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZGEQL2(MKL_INT *m, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSGEQR2(MKL_INT *m, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDGEQR2(MKL_INT *m, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *work, MKL_INT *lwork, MKL_INT *info);
void	_PCGEQR2(MKL_INT *m, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZGEQR2(MKL_INT *m, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSGERQ2(MKL_INT *m, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDGERQ2(MKL_INT *m, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *work, MKL_INT *lwork, MKL_INT *info);
void	_PCGERQ2(MKL_INT *m, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZGERQ2(MKL_INT *m, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSGETF2(MKL_INT *m, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, MKL_INT *info);
void	_PDGETF2(MKL_INT *m, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, MKL_INT *info);
void	_PCGETF2(MKL_INT *m, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, MKL_INT *info);
void	_PZGETF2(MKL_INT *m, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, MKL_INT *info);

void	_PSLABRD(MKL_INT *m, MKL_INT *n, MKL_INT *nb, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *d, float *e, float *tauq, float *taup, float *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, float *y, MKL_INT *iy, MKL_INT *jy, MKL_INT *descy, float *work);
void	_PDLABRD(MKL_INT *m, MKL_INT *n, MKL_INT *nb, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *d, double *e, double *tauq, double *taup, double *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, double *y, MKL_INT *iy, MKL_INT *jy, MKL_INT *descy, double *work);
void	_PCLABRD(MKL_INT *m, MKL_INT *n, MKL_INT *nb, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *d, float *e, MKL_Complex8 *tauq, MKL_Complex8 *taup, MKL_Complex8 *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, MKL_Complex8 *y, MKL_INT *iy, MKL_INT *jy, MKL_INT *descy, MKL_Complex8 *work);
void	_PZLABRD(MKL_INT *m, MKL_INT *n, MKL_INT *nb, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *d, double *e, MKL_Complex16 *tauq, MKL_Complex16 *taup, MKL_Complex16 *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, MKL_Complex16 *y, MKL_INT *iy, MKL_INT *jy, MKL_INT *descy, MKL_Complex16 *work);

void	_PSLACON(MKL_INT *n, float *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, float *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, MKL_INT *isgn, float *est, MKL_INT *kase);
void	_PDLACON(MKL_INT *n, double *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, double *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, MKL_INT *isgn, double *est, MKL_INT *kase);
void	_PCLACON(MKL_INT *n, MKL_Complex8 *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, MKL_Complex8 *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, float *est, MKL_INT *kase);
void	_PZLACON(MKL_INT *n, MKL_Complex16 *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, MKL_Complex16 *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, double *est, MKL_INT *kase);

void	_PSLACONSB(float *a, MKL_INT *desca, MKL_INT *i, MKL_INT *l, MKL_INT *m, float *h44, float *h33, float *h43h34, float *buf, MKL_INT *lwork);
void	_PDLACONSB(double *a, MKL_INT *desca, MKL_INT *i, MKL_INT *l, MKL_INT *m, double *h44, double *h33, double *h43h34, double *buf, MKL_INT *lwork);

void	_PSLACP2(char *uplo, MKL_INT *m, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb);
void	_PDLACP2(char *uplo, MKL_INT *m, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb);
void	_PCLACP2(char *uplo, MKL_INT *m, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb);
void	_PZLACP2(char *uplo, MKL_INT *m, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb);

void	_PSLACP3(MKL_INT *m, MKL_INT *i, float *a, MKL_INT *desca, float *b, MKL_INT *ldb, MKL_INT *ii, MKL_INT *jj, MKL_INT *rev);
void	_PDLACP3(MKL_INT *m, MKL_INT *i, double *a, MKL_INT *desca, double *b, MKL_INT *ldb, MKL_INT *ii, MKL_INT *jj, MKL_INT *rev);

void	_PSLACPY(char *uplo, MKL_INT *m, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb);
void	_PDLACPY(char *uplo, MKL_INT *m, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb);
void	_PCLACPY(char *uplo, MKL_INT *m, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb);
void	_PZLACPY(char *uplo, MKL_INT *m, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb);

void	_PSLAEVSWP(MKL_INT *n, float *zin, MKL_INT *ldzi, float *z, MKL_INT *iz, MKL_INT *jz, MKL_INT *descz, MKL_INT *nvs, MKL_INT *key, float *work, MKL_INT *lwork);
void	_PDLAEVSWP(MKL_INT *n, double *zin, MKL_INT *ldzi, double *z, MKL_INT *iz, MKL_INT *jz, MKL_INT *descz, MKL_INT *nvs, MKL_INT *key, double *work, MKL_INT *lwork);
void	_PCLAEVSWP(MKL_INT *n, float *zin, MKL_INT *ldzi, MKL_Complex8 *z, MKL_INT *iz, MKL_INT *jz, MKL_INT *descz, MKL_INT *nvs, MKL_INT *key, float *rwork, MKL_INT *lrwork);
void	_PZLAEVSWP(MKL_INT *n, double *zin, MKL_INT *ldzi, MKL_Complex16 *z, MKL_INT *iz, MKL_INT *jz, MKL_INT *descz, MKL_INT *nvs, MKL_INT *key, double *rwork, MKL_INT *lrwork);

void	_PSLAHRD(MKL_INT *n, MKL_INT *k, MKL_INT *nb, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *t, float *y, MKL_INT *iy, MKL_INT *jy, MKL_INT *descy, float *work);
void	_PDLAHRD(MKL_INT *n, MKL_INT *k, MKL_INT *nb, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *t, double *y, MKL_INT *iy, MKL_INT *jy, MKL_INT *descy, double *work);
void	_PCLAHRD(MKL_INT *n, MKL_INT *k, MKL_INT *nb, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *t, MKL_Complex8 *y, MKL_INT *iy, MKL_INT *jy, MKL_INT *descy, MKL_Complex8 *work);
void	_PZLAHRD(MKL_INT *n, MKL_INT *k, MKL_INT *nb, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *t, MKL_Complex16 *y, MKL_INT *iy, MKL_INT *jy, MKL_INT *descy, MKL_Complex16 *work);

void	_PSLAIECT(float *sigma, MKL_INT *n, float *d, MKL_INT *count);
void	_PDLAIECTB(float *sigma, MKL_INT *n, float *d, MKL_INT *count);
void	_PDLAIECTL(float *sigma, MKL_INT *n, float *d, MKL_INT *count);

float	_PSLANGE(char *norm, MKL_INT *m, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *work);
double	_PDLANGE(char *norm, MKL_INT *m, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *work);
float	_PCLANGE(char *norm, MKL_INT *m, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *work);
double	_PZLANGE(char *norm, MKL_INT *m, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *work);

float	_PSLANHS(char *norm, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *work);
double	_PDLANHS(char *norm, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *work);
float	_PCLANHS(char *norm, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *work);
double	_PZLANHS(char *norm, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *work);

float	_PSLANSY(char *norm, char *uplo, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *work);
double	_PDLANSY(char *norm, char *uplo, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *work);
float	_PCLANSY(char *norm, char *uplo, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *work);
double	_PZLANSY(char *norm, char *uplo, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *work);
float	_PCLANHE(char *norm, char *uplo, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *work);
double	_PZLANHE(char *norm, char *uplo, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *work);

float	_PSLANTR(char *norm, char *uplo, char *diag, MKL_INT *m, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *work);
double	_PDLANTR(char *norm, char *uplo, char *diag, MKL_INT *m, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *work);
float	_PCLANTR(char *norm, char *uplo, char *diag, MKL_INT *m, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *work);
double	_PZLANTR(char *norm, char *uplo, char *diag, MKL_INT *m, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *work);

void	_PSLAPIV(char *direc, char *rowcol, char *pivroc, MKL_INT *m, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, MKL_INT *ip, MKL_INT *jp, MKL_INT *descip, MKL_INT *iwork);
void	_PDLAPIV(char *direc, char *rowcol, char *pivroc, MKL_INT *m, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, MKL_INT *ip, MKL_INT *jp, MKL_INT *descip, MKL_INT *iwork);
void	_PCLAPIV(char *direc, char *rowcol, char *pivroc, MKL_INT *m, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, MKL_INT *ip, MKL_INT *jp, MKL_INT *descip, MKL_INT *iwork);
void	_PZLAPIV(char *direc, char *rowcol, char *pivroc, MKL_INT *m, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *ipiv, MKL_INT *ip, MKL_INT *jp, MKL_INT *descip, MKL_INT *iwork);

void	_PSLAQGE(MKL_INT *m, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *r, float *c, float *rowcnd, float *colcnd, float *amax, char *equed);
void	_PDLAQGE(MKL_INT *m, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *r, double *c, double *rowcnd, double *colcnd, double *amax, char *equed);
void	_PCLAQGE(MKL_INT *m, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *r, float *c, float *rowcnd, float *colcnd, float *amax, char *equed);
void	_PZLAQGE(MKL_INT *m, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *r, double *c, double *rowcnd, double *colcnd, double *amax, char *equed);

void	_PSLAQSY(char *uplo, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *sr, float *sc, float *scond, float *amax, char *equed);
void	_PDLAQSY(char *uplo, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *sr, double *sc, double *scond, double *amax, char *equed);
void	_PCLAQSY(char *uplo, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *sr, float *sc, float *scond, float *amax, char *equed);
void	_PZLAQSY(char *uplo, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *sr, double *sc, double *scond, double *amax, char *equed);

void	_PSLARED1D(MKL_INT *n, MKL_INT *ia, MKL_INT *ja, MKL_INT *desc, float *bycol, float *byall, float *work, MKL_INT *lwork);
void	_PDLARED1D(MKL_INT *n, MKL_INT *ia, MKL_INT *ja, MKL_INT *desc, double *bycol, double *byall, double *work, MKL_INT *lwork);

void	_PSLARED2D(MKL_INT *n, MKL_INT *ia, MKL_INT *ja, MKL_INT *desc, float *byrow, float *byall, float *work, MKL_INT *lwork);
void	_PDLARED2D(MKL_INT *n, MKL_INT *ia, MKL_INT *ja, MKL_INT *desc, double *byrow, double *byall, double *work, MKL_INT *lwork);

void	_PSLARF(char *side, MKL_INT *m, MKL_INT *n, float *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, MKL_INT *incv, float *tau, float *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, float *work);
void	_PDLARF(char *side, MKL_INT *m, MKL_INT *n, double *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, MKL_INT *incv, double *tau, double *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, double *work);
void	_PCLARF(char *side, MKL_INT *m, MKL_INT *n, MKL_Complex8 *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, MKL_INT *incv, MKL_Complex8 *tau, MKL_Complex8 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex8 *work);
void	_PZLARF(char *side, MKL_INT *m, MKL_INT *n, MKL_Complex16 *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, MKL_INT *incv, MKL_Complex16 *tau, MKL_Complex16 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex16 *work);

void	_PSLARFB(char *side, char *trans, char *direct, char *storev, MKL_INT *m, MKL_INT *n, MKL_INT *k, float *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, float *t, float *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, float *work);
void	_PDLARFB(char *side, char *trans, char *direct, char *storev, MKL_INT *m, MKL_INT *n, MKL_INT *k, double *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, double *t, double *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, double *work);
void	_PCLARFB(char *side, char *trans, char *direct, char *storev, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex8 *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, MKL_Complex8 *t, MKL_Complex8 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex8 *work);
void	_PZLARFB(char *side, char *trans, char *direct, char *storev, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex16 *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, MKL_Complex16 *t, MKL_Complex16 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex16 *work);

void	_PCLARFC(char *side, MKL_INT *m, MKL_INT *n, MKL_Complex8 *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, MKL_INT *incv, MKL_Complex8 *tau, MKL_Complex8 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex8 *work);
void	_PZLARFC(char *side, MKL_INT *m, MKL_INT *n, MKL_Complex16 *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, MKL_INT *incv, MKL_Complex16 *tau, MKL_Complex16 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex16 *work);

void	_PSLARFG(MKL_INT *n, float *alpha, MKL_INT *iax, MKL_INT *jax, float *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, MKL_INT *incx, float *tau);
void	_PDLARFG(MKL_INT *n, double *alpha, MKL_INT *iax, MKL_INT *jax, double *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, MKL_INT *incx, double *tau);
void	_PCLARFG(MKL_INT *n, MKL_Complex8 *alpha, MKL_INT *iax, MKL_INT *jax, MKL_Complex8 *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, MKL_INT *incx, MKL_Complex8 *tau);
void	_PZLARFG(MKL_INT *n, MKL_Complex16 *alpha, MKL_INT *iax, MKL_INT *jax, MKL_Complex16 *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, MKL_INT *incx, MKL_Complex16 *tau);

void	_PSLARFT(char *direct, char *storev, MKL_INT *n, MKL_INT *k, float *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, float *tau, float *t, float *work);
void	_PDLARFT(char *direct, char *storev, MKL_INT *n, MKL_INT *k, double *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, double *tau, double *t, double *work);
void	_PCLARFT(char *direct, char *storev, MKL_INT *n, MKL_INT *k, MKL_Complex8 *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, MKL_Complex8 *tau, MKL_Complex8 *t, MKL_Complex8 *work);
void	_PZLARFT(char *direct, char *storev, MKL_INT *n, MKL_INT *k, MKL_Complex16 *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, MKL_Complex16 *tau, MKL_Complex16 *t, MKL_Complex16 *work);

void	_PSLARZ(char *side, MKL_INT *m, MKL_INT *n, MKL_INT *l, float *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, MKL_INT *incv, float *tau, float *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, float *work);
void	_PDLARZ(char *side, MKL_INT *m, MKL_INT *n, MKL_INT *l, double *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, MKL_INT *incv, double *tau, double *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, double *work);
void	_PCLARZ(char *side, MKL_INT *m, MKL_INT *n, MKL_INT *l, MKL_Complex8 *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, MKL_INT *incv, MKL_Complex8 *tau, MKL_Complex8 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex8 *work);
void	_PZLARZ(char *side, MKL_INT *m, MKL_INT *n, MKL_INT *l, MKL_Complex16 *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, MKL_INT *incv, MKL_Complex16 *tau, MKL_Complex16 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex16 *work);

void	_PSLARZB(char *side, char *trans, char *direct, char *storev, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_INT *l, float *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, float *t, float *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, float *work);
void	_PDLARZB(char *side, char *trans, char *direct, char *storev, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_INT *l, double *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, double *t, double *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, double *work);
void	_PCLARZB(char *side, char *trans, char *direct, char *storev, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_INT *l, MKL_Complex8 *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, MKL_Complex8 *t, MKL_Complex8 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex8 *work);
void	_PZLARZB(char *side, char *trans, char *direct, char *storev, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_INT *l, MKL_Complex16 *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, MKL_Complex16 *t, MKL_Complex16 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex16 *work);

void	_PCLARZC(char *side, MKL_INT *m, MKL_INT *n, MKL_INT *l, MKL_Complex8 *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, MKL_INT *incv, MKL_Complex8 *tau, MKL_Complex8 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex8 *work);
void	_PZLARZC(char *side, MKL_INT *m, MKL_INT *n, MKL_INT *l, MKL_Complex16 *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, MKL_INT *incv, MKL_Complex16 *tau, MKL_Complex16 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex16 *work);

void	_PSLARZT(char *direct, char *storev, MKL_INT *n, MKL_INT *k, float *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, float *tau, float *t, float *work);
void	_PDLARZT(char *direct, char *storev, MKL_INT *n, MKL_INT *k, double *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, double *tau, double *t, double *work);
void	_PCLARZT(char *direct, char *storev, MKL_INT *n, MKL_INT *k, MKL_Complex8 *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, MKL_Complex8 *tau, MKL_Complex8 *t, MKL_Complex8 *work);
void	_PZLARZT(char *direct, char *storev, MKL_INT *n, MKL_INT *k, MKL_Complex16 *v, MKL_INT *iv, MKL_INT *jv, MKL_INT *descv, MKL_Complex16 *tau, MKL_Complex16 *t, MKL_Complex16 *work);

void	_PSLASCL(char *type, float *cfrom, float *cto, MKL_INT *m, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *info);
void	_PDLASCL(char *type, double *cfrom, double *cto, MKL_INT *m, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *info);
void	_PCLASCL(char *type, float *cfrom, float *cto, MKL_INT *m, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *info);
void	_PZLASCL(char *type, double *cfrom, double *cto, MKL_INT *m, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *info);

void	_PSLASET(char *uplo, MKL_INT *m, MKL_INT *n, float *alpha, float *beta, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca);
void	_PDLASET(char *uplo, MKL_INT *m, MKL_INT *n, double *alpha, double *beta, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca);
void	_PCLASET(char *uplo, MKL_INT *m, MKL_INT *n, MKL_Complex8 *alpha, MKL_Complex8 *beta, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca);
void	_PZLASET(char *uplo, MKL_INT *m, MKL_INT *n, MKL_Complex16 *alpha, MKL_Complex16 *beta, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca);

void	_PSLASMSUB(float *a, MKL_INT *desca, MKL_INT *i, MKL_INT *l, MKL_INT *k, float *smlnum, float *buf, MKL_INT *lwork);
void	_PDLASMSUB(double *a, MKL_INT *desca, MKL_INT *i, MKL_INT *l, MKL_INT *k, double *smlnum, double *buf, MKL_INT *lwork);

void	_PSLASSQ(MKL_INT *n, float *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, MKL_INT *incx, float *scale, float *sumsq);
void	_PDLASSQ(MKL_INT *n, double *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, MKL_INT *incx, double *scale, double *sumsq);
void	_PCLASSQ(MKL_INT *n, MKL_Complex8 *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, MKL_INT *incx, float *scale, float *sumsq);
void	_PZLASSQ(MKL_INT *n, MKL_Complex16 *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, MKL_INT *incx, double *scale, double *sumsq);

void	_PSLASWP(char *direc, char *rowcol, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *k1, MKL_INT *k2, MKL_INT *ipiv);
void	_PDLASWP(char *direc, char *rowcol, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *k1, MKL_INT *k2, MKL_INT *ipiv);
void	_PCLASWP(char *direc, char *rowcol, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *k1, MKL_INT *k2, MKL_INT *ipiv);
void	_PZLASWP(char *direc, char *rowcol, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *k1, MKL_INT *k2, MKL_INT *ipiv);

float	_PSLATRA(MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca);
double	_PDLATRA(MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca);
void	_PCLATRA(MKL_Complex8 *, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca);
void	_PZLATRA(MKL_Complex16 *, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca);

void	_PSLATRD(char *uplo, MKL_INT *n, MKL_INT *nb, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *d, float *e, float *tau, float *w, MKL_INT *iw, MKL_INT *jw, MKL_INT *descw, float *work);
void	_PDLATRD(char *uplo, MKL_INT *n, MKL_INT *nb, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *d, double *e, double *tau, double *w, MKL_INT *iw, MKL_INT *jw, MKL_INT *descw, double *work);
void	_PCLATRD(char *uplo, MKL_INT *n, MKL_INT *nb, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *d, float *e, MKL_Complex8 *tau, MKL_Complex8 *w, MKL_INT *iw, MKL_INT *jw, MKL_INT *descw, MKL_Complex8 *work);
void	_PZLATRD(char *uplo, MKL_INT *n, MKL_INT *nb, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *d, double *e, MKL_Complex16 *tau, MKL_Complex16 *w, MKL_INT *iw, MKL_INT *jw, MKL_INT *descw, MKL_Complex16 *work);

void	_PSLATRS(char *uplo, char *trans, char *diag, char *normin, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, float *scale, float *cnorm, float *work);
void	_PDLATRS(char *uplo, char *trans, char *diag, char *normin, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, double *scale, double *cnorm, double *work);
void	_PCLATRS(char *uplo, char *trans, char *diag, char *normin, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, float *scale, float *cnorm, MKL_Complex8 *work);
void	_PZLATRS(char *uplo, char *trans, char *diag, char *normin, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *x, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, double *scale, double *cnorm, MKL_Complex16 *work);

void	_PSLATRZ(MKL_INT *m, MKL_INT *n, MKL_INT *l, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *work);
void	_PDLATRZ(MKL_INT *m, MKL_INT *n, MKL_INT *l, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *work);
void	_PCLATRZ(MKL_INT *m, MKL_INT *n, MKL_INT *l, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *work);
void	_PZLATRZ(MKL_INT *m, MKL_INT *n, MKL_INT *l, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *work);

void	_PSLAUU2(char *uplo, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca);
void	_PDLAUU2(char *uplo, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca);
void	_PCLAUU2(char *uplo, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca);
void	_PZLAUU2(char *uplo, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca);

void	_PSLAUUM(char *uplo, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca);
void	_PDLAUUM(char *uplo, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca);
void	_PCLAUUM(char *uplo, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca);
void	_PZLAUUM(char *uplo, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca);

void	_PSLAWIL(MKL_INT *ii, MKL_INT *jj, MKL_INT *m, float *a, MKL_INT *desca, float *h44, float *h33, float *h43h34, float *v);
void	_PDLAWIL(MKL_INT *ii, MKL_INT *jj, MKL_INT *m, double *a, MKL_INT *desca, double *h44, double *h33, double *h43h34, double *v);

void	_PSORG2L(MKL_INT *m, MKL_INT *n, MKL_INT *k, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDORG2L(MKL_INT *m, MKL_INT *n, MKL_INT *k, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *work, MKL_INT *lwork, MKL_INT *info);
void	_PCUNG2L(MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZUNG2L(MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSORG2R(MKL_INT *m, MKL_INT *n, MKL_INT *k, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDORG2R(MKL_INT *m, MKL_INT *n, MKL_INT *k, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *work, MKL_INT *lwork, MKL_INT *info);
void	_PCUNG2R(MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZUNG2R(MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSORGL2(MKL_INT *m, MKL_INT *n, MKL_INT *k, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDORGL2(MKL_INT *m, MKL_INT *n, MKL_INT *k, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *work, MKL_INT *lwork, MKL_INT *info);
void	_PCUNGL2(MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZUNGL2(MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSORGR2(MKL_INT *m, MKL_INT *n, MKL_INT *k, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDORGR2(MKL_INT *m, MKL_INT *n, MKL_INT *k, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *work, MKL_INT *lwork, MKL_INT *info);
void	_PCUNGR2(MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZUNGR2(MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSORM2L(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDORM2L(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, double *work, MKL_INT *lwork, MKL_INT *info);
void	_PCUNM2L(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZUNM2L(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSORM2R(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDORM2R(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, double *work, MKL_INT *lwork, MKL_INT *info);
void	_PCUNM2R(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZUNM2R(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSORML2(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDORML2(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, double *work, MKL_INT *lwork, MKL_INT *info);
void	_PCUNML2(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZUNML2(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSORMR2(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *tau, float *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDORMR2(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *tau, double *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, double *work, MKL_INT *lwork, MKL_INT *info);
void	_PCUNMR2(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *tau, MKL_Complex8 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZUNMR2(char *side, char *trans, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *tau, MKL_Complex16 *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSPBTRSV(char *uplo, char *trans, MKL_INT *n, MKL_INT *bw, MKL_INT *nrhs, float *a, MKL_INT *ja, MKL_INT *desca, float *b, MKL_INT *ib, MKL_INT *descb, float *af, MKL_INT *laf, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDPBTRSV(char *uplo, char *trans, MKL_INT *n, MKL_INT *bw, MKL_INT *nrhs, double *a, MKL_INT *ja, MKL_INT *desca, double *b, MKL_INT *ib, MKL_INT *descb, double *af, MKL_INT *laf, double *work, MKL_INT *lwork, MKL_INT *info);
void	_PCPBTRSV(char *uplo, char *trans, MKL_INT *n, MKL_INT *bw, MKL_INT *nrhs, MKL_Complex8 *a, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex8 *af, MKL_INT *laf, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZPBTRSV(char *uplo, char *trans, MKL_INT *n, MKL_INT *bw, MKL_INT *nrhs, MKL_Complex16 *a, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex16 *af, MKL_INT *laf, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSPTTRSV(char *uplo, MKL_INT *n, MKL_INT *nrhs, float *d, float *e, MKL_INT *ja, MKL_INT *desca, float *b, MKL_INT *ib, MKL_INT *descb, float *af, MKL_INT *laf, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDPTTRSV(char *uplo, MKL_INT *n, MKL_INT *nrhs, double *d, double *e, MKL_INT *ja, MKL_INT *desca, double *b, MKL_INT *ib, MKL_INT *descb, double *af, MKL_INT *laf, double *work, MKL_INT *lwork, MKL_INT *info);
void	_PCPTTRSV(char *uplo, char *trans, MKL_INT *n, MKL_INT *nrhs, float *d, MKL_Complex8 *e, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex8 *af, MKL_INT *laf, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZPTTRSV(char *uplo, char *trans, MKL_INT *n, MKL_INT *nrhs, double *d, MKL_Complex16 *e, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *descb, MKL_Complex16 *af, MKL_INT *laf, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSPOTF2(char *uplo, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *info);
void	_PDPOTF2(char *uplo, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *info);
void	_PCPOTF2(char *uplo, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *info);
void	_PZPOTF2(char *uplo, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *info);

void	_PSRSCL(MKL_INT *n, float *sa, float *sx, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, MKL_INT *incx);
void	_PDRSCL(MKL_INT *n, double *sa, double *sx, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, MKL_INT *incx);
void	_PCSRSCL(MKL_INT *n, float *sa, MKL_Complex8 *sx, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, MKL_INT *incx);
void	_PZDRSCL(MKL_INT *n, double *sa, MKL_Complex16 *sx, MKL_INT *ix, MKL_INT *jx, MKL_INT *descx, MKL_INT *incx);

void	_PSSYGS2(MKL_INT *ibtype, char *uplo, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_INT *info);
void	_PDSYGS2(MKL_INT *ibtype, char *uplo, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_INT *info);
void	_PCHEGS2(MKL_INT *ibtype, char *uplo, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex8 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_INT *info);
void	_PZHEGS2(MKL_INT *ibtype, char *uplo, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_Complex16 *b, MKL_INT *ib, MKL_INT *jb, MKL_INT *descb, MKL_INT *info);

void	_PSSYTD2(char *uplo, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *d, float *e, float *tau, float *work, MKL_INT *lwork, MKL_INT *info);
void	_PDSYTD2(char *uplo, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *d, double *e, double *tau, double *work, MKL_INT *lwork, MKL_INT *info);
void	_PCHETD2(char *uplo, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *d, float *e, MKL_Complex8 *tau, MKL_Complex8 *work, MKL_INT *lwork, MKL_INT *info);
void	_PZHETD2(char *uplo, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *d, double *e, MKL_Complex16 *tau, MKL_Complex16 *work, MKL_INT *lwork, MKL_INT *info);

void	_PSTRTI2(char *uplo, char *diag, MKL_INT *n, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *info);
void	_PDTRTI2(char *uplo, char *diag, MKL_INT *n, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *info);
void	_PCTRTI2(char *uplo, char *diag, MKL_INT *n, MKL_Complex8 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *info);
void	_PZTRTI2(char *uplo, char *diag, MKL_INT *n, MKL_Complex16 *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, MKL_INT *info);

void	_SLAMSH(float *s, MKL_INT *lds, MKL_INT *nbulge, MKL_INT *jblk, float *h, MKL_INT *ldh, MKL_INT *n, float *ulp);
void	_DLAMSH(double *s, MKL_INT *lds, MKL_INT *nbulge, MKL_INT *jblk, double *h, MKL_INT *ldh, MKL_INT *n, double *ulp);

void	_SLAREF(char *type, float *a, MKL_INT *lda, MKL_INT *wantz, float *z, MKL_INT *ldz, MKL_INT *block, MKL_INT *irow1, MKL_INT *icol1, MKL_INT *istart, MKL_INT *istop, MKL_INT *itmp1, MKL_INT *itmp2, MKL_INT *liloz, MKL_INT *lihiz, float *vecs, float *v2, float *v3, float *t1, float *t2, float *t3);
void	_DLAREF(char *type, double *a, MKL_INT *lda, MKL_INT *wantz, double *z, MKL_INT *ldz, MKL_INT *block, MKL_INT *irow1, MKL_INT *icol1, MKL_INT *istart, MKL_INT *istop, MKL_INT *itmp1, MKL_INT *itmp2, MKL_INT *liloz, MKL_INT *lihiz, double *vecs, double *v2, double *v3, double *t1, double *t2, double *t3);

void	_SLASORTE(float *s, MKL_INT *lds, MKL_INT *j, float *out, MKL_INT *info);
void	_DLASORTE(double *s, MKL_INT *lds, MKL_INT *j, double *out, MKL_INT *info);

void	_SLASRT2(char *id, MKL_INT *n, float *d, MKL_INT *key, MKL_INT *info);
void	_DLASRT2(char *id, MKL_INT *n, double *d, MKL_INT *key, MKL_INT *info);

void	_SSTEIN2(MKL_INT *n, float *d, float *e, MKL_INT *m, float *w, MKL_INT *iblock, MKL_INT *isplit, float *orfac, float *z, MKL_INT *ldz, float *work, MKL_INT *iwork, MKL_INT *ifail, MKL_INT *info);
void	_DSTEIN2(MKL_INT *n, double *d, double *e, MKL_INT *m, double *w, MKL_INT *iblock, MKL_INT *isplit, double *orfac, double *z, MKL_INT *ldz, double *work, MKL_INT *iwork, MKL_INT *ifail, MKL_INT *info);

void	_SDBTF2(MKL_INT *m, MKL_INT *n, MKL_INT *kl, MKL_INT *ku, float *ab, MKL_INT *ldab, MKL_INT *info);
void	_DDBTF2(MKL_INT *m, MKL_INT *n, MKL_INT *kl, MKL_INT *ku, double *ab, MKL_INT *ldab, MKL_INT *info);
void	_CDBTF2(MKL_INT *m, MKL_INT *n, MKL_INT *kl, MKL_INT *ku, MKL_Complex8 *ab, MKL_INT *ldab, MKL_INT *info);
void	_ZDBTF2(MKL_INT *m, MKL_INT *n, MKL_INT *kl, MKL_INT *ku, MKL_Complex16 *ab, MKL_INT *ldab, MKL_INT *info);

void	_SDBTRF(MKL_INT *m, MKL_INT *n, MKL_INT *kl, MKL_INT *ku, float *ab, MKL_INT *ldab, MKL_INT *info);
void	_DDBTRF(MKL_INT *m, MKL_INT *n, MKL_INT *kl, MKL_INT *ku, double *ab, MKL_INT *ldab, MKL_INT *info);
void	_CDBTRF(MKL_INT *m, MKL_INT *n, MKL_INT *kl, MKL_INT *ku, MKL_Complex8 *ab, MKL_INT *ldab, MKL_INT *info);
void	_ZDBTRF(MKL_INT *m, MKL_INT *n, MKL_INT *kl, MKL_INT *ku, MKL_Complex16 *ab, MKL_INT *ldab, MKL_INT *info);

void	_SDTTRF(MKL_INT *n, float *dl, float *d, float *du, MKL_INT *info);
void	_DDTTRF(MKL_INT *n, double *dl, double *d, double *du, MKL_INT *info);
void	_CDTTRF(MKL_INT *n, MKL_Complex8 *dl, MKL_Complex8 *d, MKL_Complex8 *du, MKL_INT *info);
void	_ZDTTRF(MKL_INT *n, MKL_Complex16 *dl, MKL_Complex16 *d, MKL_Complex16 *du, MKL_INT *info);

void	_SDTTRSV(char *uplo, char *trans, MKL_INT *n, MKL_INT *nrhs, float *dl, float *d, float *du, float *b, MKL_INT *ldb, MKL_INT *info);
void	_DDTTRSV(char *uplo, char *trans, MKL_INT *n, MKL_INT *nrhs, double *dl, double *d, double *du, double *b, MKL_INT *ldb, MKL_INT *info);
void	_CDTTRSV(char *uplo, char *trans, MKL_INT *n, MKL_INT *nrhs, MKL_Complex8 *dl, MKL_Complex8 *d, MKL_Complex8 *du, MKL_Complex8 *b, MKL_INT *ldb, MKL_INT *info);
void	_ZDTTRSV(char *uplo, char *trans, MKL_INT *n, MKL_INT *nrhs, MKL_Complex16 *dl, MKL_Complex16 *d, MKL_Complex16 *du, MKL_Complex16 *b, MKL_INT *ldb, MKL_INT *info);

void	_SPTTRSV(char *trans, MKL_INT *n, MKL_INT *nrhs, float *d, float *e, float *b, MKL_INT *ldb, MKL_INT *info);
void	_DPTTRSV(char *trans, MKL_INT *n, MKL_INT *nrhs, double *d, double *e, double *b, MKL_INT *ldb, MKL_INT *info);
void	_CPTTRSV(char *uplo, char *trans, MKL_INT *n, MKL_INT *nrhs, float *d, MKL_Complex8 *e, MKL_Complex8 *b, MKL_INT *ldb, MKL_INT *info);
void	_ZPTTRSV(char *uplo, char *trans, MKL_INT *n, MKL_INT *nrhs, double *d, MKL_Complex16 *e, MKL_Complex16 *b, MKL_INT *ldb, MKL_INT *info);

void	_SSTEQR2(char *compz, MKL_INT *n, float *d, float *e, float *z, MKL_INT *ldz, MKL_INT *nr, float *work, MKL_INT *info);
void	_DSTEQR2(char *compz, MKL_INT *n, double *d, double *e, double *z, MKL_INT *ldz, MKL_INT *nr, double *work, MKL_INT *info);

void	_PSLABAD(MKL_INT *ictxt, float *small, float *large);
void	_PDLABAD(MKL_INT *ictxt, double *small, double *large);

void	_PSTRAN(MKL_INT *m, MKL_INT *n, float *alpha, float *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, float *beta, float *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc);
void	_PDTRAN(MKL_INT *m, MKL_INT *n, double *alpha, double *a, MKL_INT *ia, MKL_INT *ja, MKL_INT *desca, double *beta, double *c, MKL_INT *ic, MKL_INT *jc, MKL_INT *descc);

void	_PSLACHKIEEE(MKL_INT *isieee, float *rmax, float *rmin);
void	_PDLACHKIEEE(MKL_INT *isieee, float *rmax, float *rmin);

float	_PSLAMCH(MKL_INT *ictxt, char *cmach);
double	_PDLAMCH(MKL_INT *ictxt, char *cmach);

void	_PSLASNBT(MKL_INT *ieflag);
void	_PDLASNBT(MKL_INT *ieflag);

void	_PXERBLA(MKL_INT *ictxt, char *srname, MKL_INT *info);


#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* _MKL_SCALAPACK_H_ */
