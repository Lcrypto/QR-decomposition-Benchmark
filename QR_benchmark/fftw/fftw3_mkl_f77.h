/******************************************************************************
 *                            INTEL CONFIDENTIAL
 * Copyright(C) 2006-2010 Intel Corporation. All Rights Reserved.
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
 * Fortran 77 declarations for FFTW3 wrappers to MKL.
 *
 ******************************************************************************
 */

#ifndef FFTW3_MKL_F77_H
#define FFTW3_MKL_F77_H

/* We rely on C interface of FFTW3 wrappers to MKL */
#include "fftw3_mkl.h"

/* Fortran type names */
#define INTEGER    MKL_INT
#define INTEGER4   int
#define INTEGER8   MKL_INT64
#define COMPLEX8   fftwf_complex
#define COMPLEX16  fftw_complex
#define REAL4      float
#define REAL8      double

/* Dummy type names for empty lfftw_* wrappers */
#define COMPLEX32  fftwl_complex
#define REAL16     long double

/* Plan shall be stored in INTEGER*8 variable */
#define PLAN       INTEGER8

#if defined(_FNAME_UPPERCASE)

#define dfftw_cleanup                  DFFTW_CLEANUP
#define dfftw_cleanup_threads          DFFTW_CLEANUP_THREADS
#define dfftw_destroy_plan             DFFTW_DESTROY_PLAN
#define dfftw_execute                  DFFTW_EXECUTE
#define dfftw_execute_dft              DFFTW_EXECUTE_DFT
#define dfftw_execute_dft_c2r          DFFTW_EXECUTE_DFT_C2R
#define dfftw_execute_dft_r2c          DFFTW_EXECUTE_DFT_R2C
#define dfftw_execute_r2r              DFFTW_EXECUTE_R2R
#define dfftw_execute_split_dft        DFFTW_EXECUTE_SPLIT_DFT
#define dfftw_execute_split_dft_c2r    DFFTW_EXECUTE_SPLIT_DFT_C2R
#define dfftw_execute_split_dft_r2c    DFFTW_EXECUTE_SPLIT_DFT_R2C
#define dfftw_export_wisdom            DFFTW_EXPORT_WISDOM
#define dfftw_flops                    DFFTW_FLOPS
#define dfftw_forget_wisdom            DFFTW_FORGET_WISDOM
#define dfftw_import_system_wisdom     DFFTW_IMPORT_SYSTEM_WISDOM
#define dfftw_import_wisdom            DFFTW_IMPORT_WISDOM
#define dfftw_init_threads             DFFTW_INIT_THREADS
#define dfftw_plan_dft                 DFFTW_PLAN_DFT
#define dfftw_plan_dft_1d              DFFTW_PLAN_DFT_1D
#define dfftw_plan_dft_2d              DFFTW_PLAN_DFT_2D
#define dfftw_plan_dft_3d              DFFTW_PLAN_DFT_3D
#define dfftw_plan_dft_c2r             DFFTW_PLAN_DFT_C2R
#define dfftw_plan_dft_c2r_1d          DFFTW_PLAN_DFT_C2R_1D
#define dfftw_plan_dft_c2r_2d          DFFTW_PLAN_DFT_C2R_2D
#define dfftw_plan_dft_c2r_3d          DFFTW_PLAN_DFT_C2R_3D
#define dfftw_plan_dft_r2c             DFFTW_PLAN_DFT_R2C
#define dfftw_plan_dft_r2c_1d          DFFTW_PLAN_DFT_R2C_1D
#define dfftw_plan_dft_r2c_2d          DFFTW_PLAN_DFT_R2C_2D
#define dfftw_plan_dft_r2c_3d          DFFTW_PLAN_DFT_R2C_3D
#define dfftw_plan_guru_dft            DFFTW_PLAN_GURU_DFT
#define dfftw_plan_guru_dft_c2r        DFFTW_PLAN_GURU_DFT_C2R
#define dfftw_plan_guru_dft_r2c        DFFTW_PLAN_GURU_DFT_R2C
#define dfftw_plan_guru_r2r            DFFTW_PLAN_GURU_R2R
#define dfftw_plan_guru_split_dft      DFFTW_PLAN_GURU_SPLIT_DFT
#define dfftw_plan_guru_split_dft_c2r  DFFTW_PLAN_GURU_SPLIT_DFT_C2R
#define dfftw_plan_guru_split_dft_r2c  DFFTW_PLAN_GURU_SPLIT_DFT_R2C
#define dfftw_plan_many_dft            DFFTW_PLAN_MANY_DFT
#define dfftw_plan_many_dft_c2r        DFFTW_PLAN_MANY_DFT_C2R
#define dfftw_plan_many_dft_r2c        DFFTW_PLAN_MANY_DFT_R2C
#define dfftw_plan_many_r2r            DFFTW_PLAN_MANY_R2R
#define dfftw_plan_r2r                 DFFTW_PLAN_R2R
#define dfftw_plan_r2r_1d              DFFTW_PLAN_R2R_1D
#define dfftw_plan_r2r_2d              DFFTW_PLAN_R2R_2D
#define dfftw_plan_r2r_3d              DFFTW_PLAN_R2R_3D
#define dfftw_plan_with_nthreads       DFFTW_PLAN_WITH_NTHREADS
#define dfftw_print_plan               DFFTW_PRINT_PLAN

#define sfftw_cleanup                  SFFTW_CLEANUP
#define sfftw_cleanup_threads          SFFTW_CLEANUP_THREADS
#define sfftw_destroy_plan             SFFTW_DESTROY_PLAN
#define sfftw_execute                  SFFTW_EXECUTE
#define sfftw_execute_dft              SFFTW_EXECUTE_DFT
#define sfftw_execute_dft_c2r          SFFTW_EXECUTE_DFT_C2R
#define sfftw_execute_dft_r2c          SFFTW_EXECUTE_DFT_R2C
#define sfftw_execute_r2r              SFFTW_EXECUTE_R2R
#define sfftw_execute_split_dft        SFFTW_EXECUTE_SPLIT_DFT
#define sfftw_execute_split_dft_c2r    SFFTW_EXECUTE_SPLIT_DFT_C2R
#define sfftw_execute_split_dft_r2c    SFFTW_EXECUTE_SPLIT_DFT_R2C
#define sfftw_export_wisdom            SFFTW_EXPORT_WISDOM
#define sfftw_flops                    SFFTW_FLOPS
#define sfftw_forget_wisdom            SFFTW_FORGET_WISDOM
#define sfftw_import_system_wisdom     SFFTW_IMPORT_SYSTEM_WISDOM
#define sfftw_import_wisdom            SFFTW_IMPORT_WISDOM
#define sfftw_init_threads             SFFTW_INIT_THREADS
#define sfftw_plan_dft                 SFFTW_PLAN_DFT
#define sfftw_plan_dft_1d              SFFTW_PLAN_DFT_1D
#define sfftw_plan_dft_2d              SFFTW_PLAN_DFT_2D
#define sfftw_plan_dft_3d              SFFTW_PLAN_DFT_3D
#define sfftw_plan_dft_c2r             SFFTW_PLAN_DFT_C2R
#define sfftw_plan_dft_c2r_1d          SFFTW_PLAN_DFT_C2R_1D
#define sfftw_plan_dft_c2r_2d          SFFTW_PLAN_DFT_C2R_2D
#define sfftw_plan_dft_c2r_3d          SFFTW_PLAN_DFT_C2R_3D
#define sfftw_plan_dft_r2c             SFFTW_PLAN_DFT_R2C
#define sfftw_plan_dft_r2c_1d          SFFTW_PLAN_DFT_R2C_1D
#define sfftw_plan_dft_r2c_2d          SFFTW_PLAN_DFT_R2C_2D
#define sfftw_plan_dft_r2c_3d          SFFTW_PLAN_DFT_R2C_3D
#define sfftw_plan_guru_dft            SFFTW_PLAN_GURU_DFT
#define sfftw_plan_guru_dft_c2r        SFFTW_PLAN_GURU_DFT_C2R
#define sfftw_plan_guru_dft_r2c        SFFTW_PLAN_GURU_DFT_R2C
#define sfftw_plan_guru_r2r            SFFTW_PLAN_GURU_R2R
#define sfftw_plan_guru_split_dft      SFFTW_PLAN_GURU_SPLIT_DFT
#define sfftw_plan_guru_split_dft_c2r  SFFTW_PLAN_GURU_SPLIT_DFT_C2R
#define sfftw_plan_guru_split_dft_r2c  SFFTW_PLAN_GURU_SPLIT_DFT_R2C
#define sfftw_plan_many_dft            SFFTW_PLAN_MANY_DFT
#define sfftw_plan_many_dft_c2r        SFFTW_PLAN_MANY_DFT_C2R
#define sfftw_plan_many_dft_r2c        SFFTW_PLAN_MANY_DFT_R2C
#define sfftw_plan_many_r2r            SFFTW_PLAN_MANY_R2R
#define sfftw_plan_r2r                 SFFTW_PLAN_R2R
#define sfftw_plan_r2r_1d              SFFTW_PLAN_R2R_1D
#define sfftw_plan_r2r_2d              SFFTW_PLAN_R2R_2D
#define sfftw_plan_r2r_3d              SFFTW_PLAN_R2R_3D
#define sfftw_plan_with_nthreads       SFFTW_PLAN_WITH_NTHREADS
#define sfftw_print_plan               SFFTW_PRINT_PLAN

#define lfftw_cleanup                  LFFTW_CLEANUP
#define lfftw_cleanup_threads          LFFTW_CLEANUP_THREADS
#define lfftw_destroy_plan             LFFTW_DESTROY_PLAN
#define lfftw_execute                  LFFTW_EXECUTE
#define lfftw_execute_dft              LFFTW_EXECUTE_DFT
#define lfftw_execute_dft_c2r          LFFTW_EXECUTE_DFT_C2R
#define lfftw_execute_dft_r2c          LFFTW_EXECUTE_DFT_R2C
#define lfftw_execute_r2r              LFFTW_EXECUTE_R2R
#define lfftw_execute_split_dft        LFFTW_EXECUTE_SPLIT_DFT
#define lfftw_execute_split_dft_c2r    LFFTW_EXECUTE_SPLIT_DFT_C2R
#define lfftw_execute_split_dft_r2c    LFFTW_EXECUTE_SPLIT_DFT_R2C
#define lfftw_export_wisdom            LFFTW_EXPORT_WISDOM
#define lfftw_flops                    LFFTW_FLOPS
#define lfftw_forget_wisdom            LFFTW_FORGET_WISDOM
#define lfftw_import_system_wisdom     LFFTW_IMPORT_SYSTEM_WISDOM
#define lfftw_import_wisdom            LFFTW_IMPORT_WISDOM
#define lfftw_init_threads             LFFTW_INIT_THREADS
#define lfftw_plan_dft                 LFFTW_PLAN_DFT
#define lfftw_plan_dft_1d              LFFTW_PLAN_DFT_1D
#define lfftw_plan_dft_2d              LFFTW_PLAN_DFT_2D
#define lfftw_plan_dft_3d              LFFTW_PLAN_DFT_3D
#define lfftw_plan_dft_c2r             LFFTW_PLAN_DFT_C2R
#define lfftw_plan_dft_c2r_1d          LFFTW_PLAN_DFT_C2R_1D
#define lfftw_plan_dft_c2r_2d          LFFTW_PLAN_DFT_C2R_2D
#define lfftw_plan_dft_c2r_3d          LFFTW_PLAN_DFT_C2R_3D
#define lfftw_plan_dft_r2c             LFFTW_PLAN_DFT_R2C
#define lfftw_plan_dft_r2c_1d          LFFTW_PLAN_DFT_R2C_1D
#define lfftw_plan_dft_r2c_2d          LFFTW_PLAN_DFT_R2C_2D
#define lfftw_plan_dft_r2c_3d          LFFTW_PLAN_DFT_R2C_3D
#define lfftw_plan_guru_dft            LFFTW_PLAN_GURU_DFT
#define lfftw_plan_guru_dft_c2r        LFFTW_PLAN_GURU_DFT_C2R
#define lfftw_plan_guru_dft_r2c        LFFTW_PLAN_GURU_DFT_R2C
#define lfftw_plan_guru_r2r            LFFTW_PLAN_GURU_R2R
#define lfftw_plan_guru_split_dft      LFFTW_PLAN_GURU_SPLIT_DFT
#define lfftw_plan_guru_split_dft_c2r  LFFTW_PLAN_GURU_SPLIT_DFT_C2R
#define lfftw_plan_guru_split_dft_r2c  LFFTW_PLAN_GURU_SPLIT_DFT_R2C
#define lfftw_plan_many_dft            LFFTW_PLAN_MANY_DFT
#define lfftw_plan_many_dft_c2r        LFFTW_PLAN_MANY_DFT_C2R
#define lfftw_plan_many_dft_r2c        LFFTW_PLAN_MANY_DFT_R2C
#define lfftw_plan_many_r2r            LFFTW_PLAN_MANY_R2R
#define lfftw_plan_r2r                 LFFTW_PLAN_R2R
#define lfftw_plan_r2r_1d              LFFTW_PLAN_R2R_1D
#define lfftw_plan_r2r_2d              LFFTW_PLAN_R2R_2D
#define lfftw_plan_r2r_3d              LFFTW_PLAN_R2R_3D
#define lfftw_plan_with_nthreads       LFFTW_PLAN_WITH_NTHREADS
#define lfftw_print_plan               LFFTW_PRINT_PLAN

#else /* i.e. lowercase */

#if defined(_FNAME_SECOND_UNDERSCORE)
#define N(n) n##__
#elif defined(_FNAME_NOUNDERSCORE)
#define N(n) n
#else
#define N(n) n##_
#endif

#define dfftw_cleanup                  N(dfftw_cleanup)
#define dfftw_cleanup_threads          N(dfftw_cleanup_threads)
#define dfftw_destroy_plan             N(dfftw_destroy_plan)
#define dfftw_execute                  N(dfftw_execute)
#define dfftw_execute_dft              N(dfftw_execute_dft)
#define dfftw_execute_dft_c2r          N(dfftw_execute_dft_c2r)
#define dfftw_execute_dft_r2c          N(dfftw_execute_dft_r2c)
#define dfftw_execute_r2r              N(dfftw_execute_r2r)
#define dfftw_execute_split_dft        N(dfftw_execute_split_dft)
#define dfftw_execute_split_dft_c2r    N(dfftw_execute_split_dft_c2r)
#define dfftw_execute_split_dft_r2c    N(dfftw_execute_split_dft_r2c)
#define dfftw_export_wisdom            N(dfftw_export_wisdom)
#define dfftw_flops                    N(dfftw_flops)
#define dfftw_forget_wisdom            N(dfftw_forget_wisdom)
#define dfftw_import_system_wisdom     N(dfftw_import_system_wisdom)
#define dfftw_import_wisdom            N(dfftw_import_wisdom)
#define dfftw_init_threads             N(dfftw_init_threads)
#define dfftw_plan_dft                 N(dfftw_plan_dft)
#define dfftw_plan_dft_1d              N(dfftw_plan_dft_1d)
#define dfftw_plan_dft_2d              N(dfftw_plan_dft_2d)
#define dfftw_plan_dft_3d              N(dfftw_plan_dft_3d)
#define dfftw_plan_dft_c2r             N(dfftw_plan_dft_c2r)
#define dfftw_plan_dft_c2r_1d          N(dfftw_plan_dft_c2r_1d)
#define dfftw_plan_dft_c2r_2d          N(dfftw_plan_dft_c2r_2d)
#define dfftw_plan_dft_c2r_3d          N(dfftw_plan_dft_c2r_3d)
#define dfftw_plan_dft_r2c             N(dfftw_plan_dft_r2c)
#define dfftw_plan_dft_r2c_1d          N(dfftw_plan_dft_r2c_1d)
#define dfftw_plan_dft_r2c_2d          N(dfftw_plan_dft_r2c_2d)
#define dfftw_plan_dft_r2c_3d          N(dfftw_plan_dft_r2c_3d)
#define dfftw_plan_guru_dft            N(dfftw_plan_guru_dft)
#define dfftw_plan_guru_dft_c2r        N(dfftw_plan_guru_dft_c2r)
#define dfftw_plan_guru_dft_r2c        N(dfftw_plan_guru_dft_r2c)
#define dfftw_plan_guru_r2r            N(dfftw_plan_guru_r2r)
#define dfftw_plan_guru_split_dft      N(dfftw_plan_guru_split_dft)
#define dfftw_plan_guru_split_dft_c2r  N(dfftw_plan_guru_split_dft_c2r)
#define dfftw_plan_guru_split_dft_r2c  N(dfftw_plan_guru_split_dft_r2c)
#define dfftw_plan_many_dft            N(dfftw_plan_many_dft)
#define dfftw_plan_many_dft_c2r        N(dfftw_plan_many_dft_c2r)
#define dfftw_plan_many_dft_r2c        N(dfftw_plan_many_dft_r2c)
#define dfftw_plan_many_r2r            N(dfftw_plan_many_r2r)
#define dfftw_plan_r2r                 N(dfftw_plan_r2r)
#define dfftw_plan_r2r_1d              N(dfftw_plan_r2r_1d)
#define dfftw_plan_r2r_2d              N(dfftw_plan_r2r_2d)
#define dfftw_plan_r2r_3d              N(dfftw_plan_r2r_3d)
#define dfftw_plan_with_nthreads       N(dfftw_plan_with_nthreads)
#define dfftw_print_plan               N(dfftw_print_plan)

#define sfftw_cleanup                  N(sfftw_cleanup)
#define sfftw_cleanup_threads          N(sfftw_cleanup_threads)
#define sfftw_destroy_plan             N(sfftw_destroy_plan)
#define sfftw_execute                  N(sfftw_execute)
#define sfftw_execute_dft              N(sfftw_execute_dft)
#define sfftw_execute_dft_c2r          N(sfftw_execute_dft_c2r)
#define sfftw_execute_dft_r2c          N(sfftw_execute_dft_r2c)
#define sfftw_execute_r2r              N(sfftw_execute_r2r)
#define sfftw_execute_split_dft        N(sfftw_execute_split_dft)
#define sfftw_execute_split_dft_c2r    N(sfftw_execute_split_dft_c2r)
#define sfftw_execute_split_dft_r2c    N(sfftw_execute_split_dft_r2c)
#define sfftw_export_wisdom            N(sfftw_export_wisdom)
#define sfftw_flops                    N(sfftw_flops)
#define sfftw_forget_wisdom            N(sfftw_forget_wisdom)
#define sfftw_import_system_wisdom     N(sfftw_import_system_wisdom)
#define sfftw_import_wisdom            N(sfftw_import_wisdom)
#define sfftw_init_threads             N(sfftw_init_threads)
#define sfftw_plan_dft                 N(sfftw_plan_dft)
#define sfftw_plan_dft_1d              N(sfftw_plan_dft_1d)
#define sfftw_plan_dft_2d              N(sfftw_plan_dft_2d)
#define sfftw_plan_dft_3d              N(sfftw_plan_dft_3d)
#define sfftw_plan_dft_c2r             N(sfftw_plan_dft_c2r)
#define sfftw_plan_dft_c2r_1d          N(sfftw_plan_dft_c2r_1d)
#define sfftw_plan_dft_c2r_2d          N(sfftw_plan_dft_c2r_2d)
#define sfftw_plan_dft_c2r_3d          N(sfftw_plan_dft_c2r_3d)
#define sfftw_plan_dft_r2c             N(sfftw_plan_dft_r2c)
#define sfftw_plan_dft_r2c_1d          N(sfftw_plan_dft_r2c_1d)
#define sfftw_plan_dft_r2c_2d          N(sfftw_plan_dft_r2c_2d)
#define sfftw_plan_dft_r2c_3d          N(sfftw_plan_dft_r2c_3d)
#define sfftw_plan_guru_dft            N(sfftw_plan_guru_dft)
#define sfftw_plan_guru_dft_c2r        N(sfftw_plan_guru_dft_c2r)
#define sfftw_plan_guru_dft_r2c        N(sfftw_plan_guru_dft_r2c)
#define sfftw_plan_guru_r2r            N(sfftw_plan_guru_r2r)
#define sfftw_plan_guru_split_dft      N(sfftw_plan_guru_split_dft)
#define sfftw_plan_guru_split_dft_c2r  N(sfftw_plan_guru_split_dft_c2r)
#define sfftw_plan_guru_split_dft_r2c  N(sfftw_plan_guru_split_dft_r2c)
#define sfftw_plan_many_dft            N(sfftw_plan_many_dft)
#define sfftw_plan_many_dft_c2r        N(sfftw_plan_many_dft_c2r)
#define sfftw_plan_many_dft_r2c        N(sfftw_plan_many_dft_r2c)
#define sfftw_plan_many_r2r            N(sfftw_plan_many_r2r)
#define sfftw_plan_r2r                 N(sfftw_plan_r2r)
#define sfftw_plan_r2r_1d              N(sfftw_plan_r2r_1d)
#define sfftw_plan_r2r_2d              N(sfftw_plan_r2r_2d)
#define sfftw_plan_r2r_3d              N(sfftw_plan_r2r_3d)
#define sfftw_plan_with_nthreads       N(sfftw_plan_with_nthreads)
#define sfftw_print_plan               N(sfftw_print_plan)

#define lfftw_cleanup                  N(lfftw_cleanup)
#define lfftw_cleanup_threads          N(lfftw_cleanup_threads)
#define lfftw_destroy_plan             N(lfftw_destroy_plan)
#define lfftw_execute                  N(lfftw_execute)
#define lfftw_execute_dft              N(lfftw_execute_dft)
#define lfftw_execute_dft_c2r          N(lfftw_execute_dft_c2r)
#define lfftw_execute_dft_r2c          N(lfftw_execute_dft_r2c)
#define lfftw_execute_r2r              N(lfftw_execute_r2r)
#define lfftw_execute_split_dft        N(lfftw_execute_split_dft)
#define lfftw_execute_split_dft_c2r    N(lfftw_execute_split_dft_c2r)
#define lfftw_execute_split_dft_r2c    N(lfftw_execute_split_dft_r2c)
#define lfftw_export_wisdom            N(lfftw_export_wisdom)
#define lfftw_flops                    N(lfftw_flops)
#define lfftw_forget_wisdom            N(lfftw_forget_wisdom)
#define lfftw_import_system_wisdom     N(lfftw_import_system_wisdom)
#define lfftw_import_wisdom            N(lfftw_import_wisdom)
#define lfftw_init_threads             N(lfftw_init_threads)
#define lfftw_plan_dft                 N(lfftw_plan_dft)
#define lfftw_plan_dft_1d              N(lfftw_plan_dft_1d)
#define lfftw_plan_dft_2d              N(lfftw_plan_dft_2d)
#define lfftw_plan_dft_3d              N(lfftw_plan_dft_3d)
#define lfftw_plan_dft_c2r             N(lfftw_plan_dft_c2r)
#define lfftw_plan_dft_c2r_1d          N(lfftw_plan_dft_c2r_1d)
#define lfftw_plan_dft_c2r_2d          N(lfftw_plan_dft_c2r_2d)
#define lfftw_plan_dft_c2r_3d          N(lfftw_plan_dft_c2r_3d)
#define lfftw_plan_dft_r2c             N(lfftw_plan_dft_r2c)
#define lfftw_plan_dft_r2c_1d          N(lfftw_plan_dft_r2c_1d)
#define lfftw_plan_dft_r2c_2d          N(lfftw_plan_dft_r2c_2d)
#define lfftw_plan_dft_r2c_3d          N(lfftw_plan_dft_r2c_3d)
#define lfftw_plan_guru_dft            N(lfftw_plan_guru_dft)
#define lfftw_plan_guru_dft_c2r        N(lfftw_plan_guru_dft_c2r)
#define lfftw_plan_guru_dft_r2c        N(lfftw_plan_guru_dft_r2c)
#define lfftw_plan_guru_r2r            N(lfftw_plan_guru_r2r)
#define lfftw_plan_guru_split_dft      N(lfftw_plan_guru_split_dft)
#define lfftw_plan_guru_split_dft_c2r  N(lfftw_plan_guru_split_dft_c2r)
#define lfftw_plan_guru_split_dft_r2c  N(lfftw_plan_guru_split_dft_r2c)
#define lfftw_plan_many_dft            N(lfftw_plan_many_dft)
#define lfftw_plan_many_dft_c2r        N(lfftw_plan_many_dft_c2r)
#define lfftw_plan_many_dft_r2c        N(lfftw_plan_many_dft_r2c)
#define lfftw_plan_many_r2r            N(lfftw_plan_many_r2r)
#define lfftw_plan_r2r                 N(lfftw_plan_r2r)
#define lfftw_plan_r2r_1d              N(lfftw_plan_r2r_1d)
#define lfftw_plan_r2r_2d              N(lfftw_plan_r2r_2d)
#define lfftw_plan_r2r_3d              N(lfftw_plan_r2r_3d)
#define lfftw_plan_with_nthreads       N(lfftw_plan_with_nthreads)
#define lfftw_print_plan               N(lfftw_print_plan)

#endif

void dfftw_cleanup(void);
void dfftw_cleanup_threads(void);
void dfftw_destroy_plan(PLAN*);
void dfftw_execute(PLAN*);
void dfftw_execute_dft(PLAN*,COMPLEX16*,COMPLEX16*);
void dfftw_execute_dft_c2r(PLAN*,COMPLEX16*,REAL8*);
void dfftw_execute_dft_r2c(PLAN*,REAL8*,COMPLEX16*);
void dfftw_execute_r2r(PLAN*,REAL8*,REAL8*);
void dfftw_execute_split_dft(PLAN*,REAL8*,REAL8*,REAL8*,REAL8*);
void dfftw_execute_split_dft_c2r(PLAN*,REAL8*,REAL8*,REAL8*);
void dfftw_execute_split_dft_r2c(PLAN*,REAL8*,REAL8*,REAL8*);
void dfftw_export_wisdom(void*,void*);
void dfftw_flops(PLAN*,double*,double*,double*);
void dfftw_forget_wisdom(void);
void dfftw_import_system_wisdom(INTEGER*);
void dfftw_import_wisdom(INTEGER*,void*,void*);
void dfftw_init_threads(INTEGER*);
void dfftw_plan_dft(PLAN*,INTEGER*,INTEGER*,COMPLEX16*,COMPLEX16*,INTEGER*,INTEGER*);
void dfftw_plan_dft_1d(PLAN*,INTEGER*,COMPLEX16*,COMPLEX16*,INTEGER*,INTEGER*);
void dfftw_plan_dft_2d(PLAN*,INTEGER*,INTEGER*,COMPLEX16*,COMPLEX16*,INTEGER*,INTEGER*);
void dfftw_plan_dft_3d(PLAN*,INTEGER*,INTEGER*,INTEGER*,COMPLEX16*,COMPLEX16*,INTEGER*,INTEGER*);
void dfftw_plan_dft_c2r(PLAN*,INTEGER*,INTEGER*,COMPLEX16*,REAL8*,INTEGER*);
void dfftw_plan_dft_c2r_1d(PLAN*,INTEGER*,COMPLEX16*,REAL8*,INTEGER*);
void dfftw_plan_dft_c2r_2d(PLAN*,INTEGER*,INTEGER*,COMPLEX16*,REAL8*,INTEGER*);
void dfftw_plan_dft_c2r_3d(PLAN*,INTEGER*,INTEGER*,INTEGER*,COMPLEX16*,REAL8*,INTEGER*);
void dfftw_plan_dft_r2c(PLAN*,INTEGER*,INTEGER*,REAL8*,COMPLEX16*,INTEGER*);
void dfftw_plan_dft_r2c_1d(PLAN*,INTEGER*,REAL8*,COMPLEX16*,INTEGER*);
void dfftw_plan_dft_r2c_2d(PLAN*,INTEGER*,INTEGER*,REAL8*,COMPLEX16*,INTEGER*);
void dfftw_plan_dft_r2c_3d(PLAN*,INTEGER*,INTEGER*,INTEGER*,REAL8*,COMPLEX16*,INTEGER*);
void dfftw_plan_guru_dft(PLAN*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,COMPLEX16*,COMPLEX16*,INTEGER*,INTEGER*);
void dfftw_plan_guru_dft_c2r(PLAN*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,COMPLEX16*,REAL8*,INTEGER*);
void dfftw_plan_guru_dft_r2c(PLAN*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,REAL8*,COMPLEX16*,INTEGER*);
void dfftw_plan_guru_r2r(PLAN*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,REAL8*,REAL8*,INTEGER*,INTEGER*);
void dfftw_plan_guru_split_dft(PLAN*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,REAL8*,REAL8*,REAL8*,REAL8*,INTEGER*);
void dfftw_plan_guru_split_dft_c2r(PLAN*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,REAL8*,REAL8*,REAL8*,INTEGER*);
void dfftw_plan_guru_split_dft_r2c(PLAN*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,REAL8*,REAL8*,REAL8*,INTEGER*);
void dfftw_plan_many_dft(PLAN*,INTEGER*,INTEGER*,INTEGER*,COMPLEX16*,INTEGER*,INTEGER*,INTEGER*,COMPLEX16*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*);
void dfftw_plan_many_dft_c2r(PLAN*,INTEGER*,INTEGER*,INTEGER*,COMPLEX16*,INTEGER*,INTEGER*,INTEGER*,REAL8*,INTEGER*,INTEGER*,INTEGER*,INTEGER*);
void dfftw_plan_many_dft_r2c(PLAN*,INTEGER*,INTEGER*,INTEGER*,REAL8*,INTEGER*,INTEGER*,INTEGER*,COMPLEX16*,INTEGER*,INTEGER*,INTEGER*,INTEGER*);
void dfftw_plan_many_r2r(PLAN*,INTEGER*,INTEGER*,INTEGER*,REAL8*,INTEGER*,INTEGER*,INTEGER*,REAL8*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*);
void dfftw_plan_r2r(PLAN*,INTEGER*,INTEGER*,REAL8*,REAL8*,INTEGER*,INTEGER*);
void dfftw_plan_r2r_1d(PLAN*,INTEGER*,REAL8*,REAL8*,INTEGER*,INTEGER*);
void dfftw_plan_r2r_2d(PLAN*,INTEGER*,INTEGER*,REAL8*,REAL8*,INTEGER*,INTEGER*,INTEGER*);
void dfftw_plan_r2r_3d(PLAN*,INTEGER*,INTEGER*,INTEGER*,REAL8*,REAL8*,INTEGER*,INTEGER*,INTEGER*,INTEGER*);
void dfftw_plan_with_nthreads(INTEGER*);
void dfftw_print_plan(PLAN*);

void sfftw_cleanup(void);
void sfftw_cleanup_threads(void);
void sfftw_destroy_plan(PLAN*);
void sfftw_execute(PLAN*);
void sfftw_execute_dft(PLAN*,COMPLEX8*,COMPLEX8*);
void sfftw_execute_dft_c2r(PLAN*,COMPLEX8*,REAL4*);
void sfftw_execute_dft_r2c(PLAN*,REAL4*,COMPLEX8*);
void sfftw_execute_r2r(PLAN*,REAL4*,REAL4*);
void sfftw_execute_split_dft(PLAN*,REAL4*,REAL4*,REAL4*,REAL4*);
void sfftw_execute_split_dft_c2r(PLAN*,REAL4*,REAL4*,REAL4*);
void sfftw_execute_split_dft_r2c(PLAN*,REAL4*,REAL4*,REAL4*);
void sfftw_export_wisdom(void*,void*);
void sfftw_flops(PLAN*,double*,double*,double*);
void sfftw_forget_wisdom(void);
void sfftw_import_system_wisdom(INTEGER*);
void sfftw_import_wisdom(INTEGER*,void*,void*);
void sfftw_init_threads(INTEGER*);
void sfftw_plan_dft(PLAN*,INTEGER*,INTEGER*,COMPLEX8*,COMPLEX8*,INTEGER*,INTEGER*);
void sfftw_plan_dft_1d(PLAN*,INTEGER*,COMPLEX8*,COMPLEX8*,INTEGER*,INTEGER*);
void sfftw_plan_dft_2d(PLAN*,INTEGER*,INTEGER*,COMPLEX8*,COMPLEX8*,INTEGER*,INTEGER*);
void sfftw_plan_dft_3d(PLAN*,INTEGER*,INTEGER*,INTEGER*,COMPLEX8*,COMPLEX8*,INTEGER*,INTEGER*);
void sfftw_plan_dft_c2r(PLAN*,INTEGER*,INTEGER*,COMPLEX8*,REAL4*,INTEGER*);
void sfftw_plan_dft_c2r_1d(PLAN*,INTEGER*,COMPLEX8*,REAL4*,INTEGER*);
void sfftw_plan_dft_c2r_2d(PLAN*,INTEGER*,INTEGER*,COMPLEX8*,REAL4*,INTEGER*);
void sfftw_plan_dft_c2r_3d(PLAN*,INTEGER*,INTEGER*,INTEGER*,COMPLEX8*,REAL4*,INTEGER*);
void sfftw_plan_dft_r2c(PLAN*,INTEGER*,INTEGER*,REAL4*,COMPLEX8*,INTEGER*);
void sfftw_plan_dft_r2c_1d(PLAN*,INTEGER*,REAL4*,COMPLEX8*,INTEGER*);
void sfftw_plan_dft_r2c_2d(PLAN*,INTEGER*,INTEGER*,REAL4*,COMPLEX8*,INTEGER*);
void sfftw_plan_dft_r2c_3d(PLAN*,INTEGER*,INTEGER*,INTEGER*,REAL4*,COMPLEX8*,INTEGER*);
void sfftw_plan_guru_dft(PLAN*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,COMPLEX8*,COMPLEX8*,INTEGER*,INTEGER*);
void sfftw_plan_guru_dft_c2r(PLAN*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,COMPLEX8*,REAL4*,INTEGER*);
void sfftw_plan_guru_dft_r2c(PLAN*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,REAL4*,COMPLEX8*,INTEGER*);
void sfftw_plan_guru_r2r(PLAN*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,REAL4*,REAL4*,INTEGER*,INTEGER*);
void sfftw_plan_guru_split_dft(PLAN*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,REAL4*,REAL4*,REAL4*,REAL4*,INTEGER*);
void sfftw_plan_guru_split_dft_c2r(PLAN*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,REAL4*,REAL4*,REAL4*,INTEGER*);
void sfftw_plan_guru_split_dft_r2c(PLAN*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,REAL4*,REAL4*,REAL4*,INTEGER*);
void sfftw_plan_many_dft(PLAN*,INTEGER*,INTEGER*,INTEGER*,COMPLEX8*,INTEGER*,INTEGER*,INTEGER*,COMPLEX8*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*);
void sfftw_plan_many_dft_c2r(PLAN*,INTEGER*,INTEGER*,INTEGER*,COMPLEX8*,INTEGER*,INTEGER*,INTEGER*,REAL4*,INTEGER*,INTEGER*,INTEGER*,INTEGER*);
void sfftw_plan_many_dft_r2c(PLAN*,INTEGER*,INTEGER*,INTEGER*,REAL4*,INTEGER*,INTEGER*,INTEGER*,COMPLEX8*,INTEGER*,INTEGER*,INTEGER*,INTEGER*);
void sfftw_plan_many_r2r(PLAN*,INTEGER*,INTEGER*,INTEGER*,REAL4*,INTEGER*,INTEGER*,INTEGER*,REAL4*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*);
void sfftw_plan_r2r(PLAN*,INTEGER*,INTEGER*,REAL4*,REAL4*,INTEGER*,INTEGER*);
void sfftw_plan_r2r_1d(PLAN*,INTEGER*,REAL4*,REAL4*,INTEGER*,INTEGER*);
void sfftw_plan_r2r_2d(PLAN*,INTEGER*,INTEGER*,REAL4*,REAL4*,INTEGER*,INTEGER*,INTEGER*);
void sfftw_plan_r2r_3d(PLAN*,INTEGER*,INTEGER*,INTEGER*,REAL4*,REAL4*,INTEGER*,INTEGER*,INTEGER*,INTEGER*);
void sfftw_plan_with_nthreads(INTEGER*);
void sfftw_print_plan(PLAN*);

void lfftw_cleanup(void);
void lfftw_cleanup_threads(void);
void lfftw_destroy_plan(PLAN*);
void lfftw_execute(PLAN*);
void lfftw_execute_dft(PLAN*,COMPLEX32*,COMPLEX32*);
void lfftw_execute_dft_c2r(PLAN*,COMPLEX32*,REAL16*);
void lfftw_execute_dft_r2c(PLAN*,REAL16*,COMPLEX32*);
void lfftw_execute_r2r(PLAN*,REAL16*,REAL16*);
void lfftw_execute_split_dft(PLAN*,REAL16*,REAL16*,REAL16*,REAL16*);
void lfftw_execute_split_dft_c2r(PLAN*,REAL16*,REAL16*,REAL16*);
void lfftw_execute_split_dft_r2c(PLAN*,REAL16*,REAL16*,REAL16*);
void lfftw_export_wisdom(void*,void*);
void lfftw_flops(PLAN*,double*,double*,double*);
void lfftw_forget_wisdom(void);
void lfftw_import_system_wisdom(INTEGER*);
void lfftw_import_wisdom(INTEGER*,void*,void*);
void lfftw_init_threads(INTEGER*);
void lfftw_plan_dft(PLAN*,INTEGER*,INTEGER*,COMPLEX32*,COMPLEX32*,INTEGER*,INTEGER*);
void lfftw_plan_dft_1d(PLAN*,INTEGER*,COMPLEX32*,COMPLEX32*,INTEGER*,INTEGER*);
void lfftw_plan_dft_2d(PLAN*,INTEGER*,INTEGER*,COMPLEX32*,COMPLEX32*,INTEGER*,INTEGER*);
void lfftw_plan_dft_3d(PLAN*,INTEGER*,INTEGER*,INTEGER*,COMPLEX32*,COMPLEX32*,INTEGER*,INTEGER*);
void lfftw_plan_dft_c2r(PLAN*,INTEGER*,INTEGER*,COMPLEX32*,REAL16*,INTEGER*);
void lfftw_plan_dft_c2r_1d(PLAN*,INTEGER*,COMPLEX32*,REAL16*,INTEGER*);
void lfftw_plan_dft_c2r_2d(PLAN*,INTEGER*,INTEGER*,COMPLEX32*,REAL16*,INTEGER*);
void lfftw_plan_dft_c2r_3d(PLAN*,INTEGER*,INTEGER*,INTEGER*,COMPLEX32*,REAL16*,INTEGER*);
void lfftw_plan_dft_r2c(PLAN*,INTEGER*,INTEGER*,REAL16*,COMPLEX32*,INTEGER*);
void lfftw_plan_dft_r2c_1d(PLAN*,INTEGER*,REAL16*,COMPLEX32*,INTEGER*);
void lfftw_plan_dft_r2c_2d(PLAN*,INTEGER*,INTEGER*,REAL16*,COMPLEX32*,INTEGER*);
void lfftw_plan_dft_r2c_3d(PLAN*,INTEGER*,INTEGER*,INTEGER*,REAL16*,COMPLEX32*,INTEGER*);
void lfftw_plan_guru_dft(PLAN*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,COMPLEX32*,COMPLEX32*,INTEGER*,INTEGER*);
void lfftw_plan_guru_dft_c2r(PLAN*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,COMPLEX32*,REAL16*,INTEGER*);
void lfftw_plan_guru_dft_r2c(PLAN*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,REAL16*,COMPLEX32*,INTEGER*);
void lfftw_plan_guru_r2r(PLAN*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,REAL16*,REAL16*,INTEGER*,INTEGER*);
void lfftw_plan_guru_split_dft(PLAN*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,REAL16*,REAL16*,REAL16*,REAL16*,INTEGER*);
void lfftw_plan_guru_split_dft_c2r(PLAN*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,REAL16*,REAL16*,REAL16*,INTEGER*);
void lfftw_plan_guru_split_dft_r2c(PLAN*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,REAL16*,REAL16*,REAL16*,INTEGER*);
void lfftw_plan_many_dft(PLAN*,INTEGER*,INTEGER*,INTEGER*,COMPLEX32*,INTEGER*,INTEGER*,INTEGER*,COMPLEX32*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*);
void lfftw_plan_many_dft_c2r(PLAN*,INTEGER*,INTEGER*,INTEGER*,COMPLEX32*,INTEGER*,INTEGER*,INTEGER*,REAL16*,INTEGER*,INTEGER*,INTEGER*,INTEGER*);
void lfftw_plan_many_dft_r2c(PLAN*,INTEGER*,INTEGER*,INTEGER*,REAL16*,INTEGER*,INTEGER*,INTEGER*,COMPLEX32*,INTEGER*,INTEGER*,INTEGER*,INTEGER*);
void lfftw_plan_many_r2r(PLAN*,INTEGER*,INTEGER*,INTEGER*,REAL16*,INTEGER*,INTEGER*,INTEGER*,REAL16*,INTEGER*,INTEGER*,INTEGER*,INTEGER*,INTEGER*);
void lfftw_plan_r2r(PLAN*,INTEGER*,INTEGER*,REAL16*,REAL16*,INTEGER*,INTEGER*);
void lfftw_plan_r2r_1d(PLAN*,INTEGER*,REAL16*,REAL16*,INTEGER*,INTEGER*);
void lfftw_plan_r2r_2d(PLAN*,INTEGER*,INTEGER*,REAL16*,REAL16*,INTEGER*,INTEGER*,INTEGER*);
void lfftw_plan_r2r_3d(PLAN*,INTEGER*,INTEGER*,INTEGER*,REAL16*,REAL16*,INTEGER*,INTEGER*,INTEGER*,INTEGER*);
void lfftw_plan_with_nthreads(INTEGER*);
void lfftw_print_plan(PLAN*);

#endif /* FFTW3_MKL_F77_H */
