/******************************************************************************
 *                            INTEL CONFIDENTIAL
 * Copyright(C) 2005-2010 Intel Corporation. All Rights Reserved.
 * The source code contained  or  described herein and all documents related to
 * the source code ("Material") are owned by Intel Corporation or its suppliers
 * or licensors.  Title to the  Material remains with  Intel Corporation or its
 * suppliers and licensors. The Material contains trade secrets and proprietary
 * and  confidential  information of  Intel or its suppliers and licensors. The
 * Material  is  protected  by  worldwide  copyright  and trade secret laws and
 * treaty  provisions. No part of the Material may be used, copied, reproduced,
 * modified, published, uploaded, posted, transmitted, distributed or disclosed
 * in any way without Intel's prior express written permission.
 * No license  under any  patent, copyright, trade secret or other intellectual
 * property right is granted to or conferred upon you by disclosure or delivery
 * of the Materials,  either expressly, by implication, inducement, estoppel or
 * otherwise.  Any  license  under  such  intellectual property  rights must be
 * express and approved by Intel in writing.
 *
 ******************************************************************************
 *
 * Definitions for FFTW3 wrappers to MKL.
 *
 ******************************************************************************
 */

#ifndef FFTW3_MKL_H
#define FFTW3_MKL_H

#include <stdlib.h>
#include "fftw3.h"
#include "mkl_dfti.h"
#include "mkl_trig_transforms.h"
#include "mkl_service.h"

typedef struct fftw_mkl_plan_s *fftw_mkl_plan;
typedef struct fftw3_mkl_s      fftw3_mkl_s;

/* Plan holder for the wrappers */
struct fftw_mkl_plan_s
{
    void (*execute) (fftw_mkl_plan p);
    DFTI_DESCRIPTOR_HANDLE desc;
    void *io[4];
    MKL_INT *ipar;
    double *dpar;
    float *spar;
};

/* Global helper structure */
struct fftw3_mkl_s
{
    int verbose;
    int nthreads;
    double timelimit;
    int number_of_user_threads;
    fftw_mkl_plan (*new_plan) (void);
    void (*delete_plan) (fftw_mkl_plan p);
    int default_alignment;
};

extern fftw3_mkl_s fftw3_mkl;

#define MKL_MAXRANK 7
#define MKL_ONE     1
#define MKL_RODFT00 413

#define BAD(status) ((status) && !DftiErrorClass((status),DFTI_NO_ERROR))

#ifndef UNUSED
#define UNUSED(p) (void)p
#endif

#endif /* FFTW3_MKL_H */
