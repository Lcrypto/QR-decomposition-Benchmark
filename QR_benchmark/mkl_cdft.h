/*******************************************************************************
!                             INTEL CONFIDENTIAL
!   Copyright(C) 2002-2010 Intel Corporation. All Rights Reserved.
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
!      Intel(R) Math Kernel Library (MKL) interface for Cluster DFT routines
!******************************************************************************/

/* Avoid multiple definition */
#ifndef _MKL_CDFT_H_
#define _MKL_CDFT_H_

/* Include header-files */
#include "mkl_dfti.h"
#include "mpi.h"

/* Keep C++ compilers from getting confused */
#ifdef __cplusplus
extern "C" {
#endif

/* Codes of errors */
#define CDFT_MPI_ERROR		1000
#define CDFT_SPREAD_ERROR	1001

/* Codes of parameters for DftiGetValueDM / DftiSetValueDM */
enum CDFT_CONFIG_PARAM {
	CDFT_LOCAL_SIZE			=1000,
	CDFT_LOCAL_X_START		=1001,
	CDFT_LOCAL_NX			=1002,
	CDFT_MPI_COMM			=1003,
	CDFT_WORKSPACE			=1004,
	CDFT_LOCAL_OUT_X_START	=1005,
	CDFT_LOCAL_OUT_NX		=1006
};

/* Definition of handle to descriptor */
typedef struct _DFTI_DESCRIPTOR_DM* DFTI_DESCRIPTOR_DM_HANDLE;

/* Prototypes of routines */
extern MKL_LONG DftiCreateDescriptorDM(MPI_Comm,DFTI_DESCRIPTOR_DM_HANDLE*,enum DFTI_CONFIG_VALUE,enum DFTI_CONFIG_VALUE,MKL_LONG,...);
extern MKL_LONG DftiGetValueDM(DFTI_DESCRIPTOR_DM_HANDLE,int,...);
extern MKL_LONG DftiSetValueDM(DFTI_DESCRIPTOR_DM_HANDLE,int,...);
extern MKL_LONG DftiCommitDescriptorDM(DFTI_DESCRIPTOR_DM_HANDLE);
extern MKL_LONG DftiComputeForwardDM(DFTI_DESCRIPTOR_DM_HANDLE,void*,...);
extern MKL_LONG DftiComputeBackwardDM(DFTI_DESCRIPTOR_DM_HANDLE,void*,...);
extern MKL_LONG DftiFreeDescriptorDM(DFTI_DESCRIPTOR_DM_HANDLE*);

/* Keep C++ compilers from getting confused (extern "C" {) */
#ifdef __cplusplus
}
#endif

/* Avoid multiple definition (#ifndef _MKL_CDFT_H_) */
#endif
