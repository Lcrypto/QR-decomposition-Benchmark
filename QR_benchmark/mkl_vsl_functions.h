/* file: mkl_vsl_functions.h */
/*
//                             INTEL CONFIDENTIAL
//  Copyright(C) 2006-2010 Intel Corporation. All Rights Reserved.
//  The source code contained  or  described herein and all documents related to
//  the source code ("Material") are owned by Intel Corporation or its suppliers
//  or licensors.  Title to the  Material remains with  Intel Corporation or its
//  suppliers and licensors. The Material contains trade secrets and proprietary
//  and  confidential  information of  Intel or its suppliers and licensors. The
//  Material  is  protected  by  worldwide  copyright  and trade secret laws and
//  treaty  provisions. No part of the Material may be used, copied, reproduced,
//  modified, published, uploaded, posted, transmitted, distributed or disclosed
//  in any way without Intel's prior express written permission.
//  No license  under any  patent, copyright, trade secret or other intellectual
//  property right is granted to or conferred upon you by disclosure or delivery
//  of the Materials,  either expressly, by implication, inducement, estoppel or
//  otherwise.  Any  license  under  such  intellectual property  rights must be
//  express and approved by Intel in writing.
*/
/*
//++
//  User-level VSL function declarations
//--
*/

#ifndef __MKL_VSL_FUNCTIONS_H__
#define __MKL_VSL_FUNCTIONS_H__

#include "mkl_vsl_types.h"

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/*
//++
//  EXTERNAL API MACROS.
//  Used to construct VSL function declaration. Change them if you are going to
//  provide different API for VSL functions.
//--
*/
#if defined(MKL_VSL_STDCALL)
  #define _Vsl_Api(rtype,name,arg)   extern rtype __stdcall name    arg;
  #define _vsl_api(rtype,name,arg)   extern rtype __stdcall name    arg;
  #define _VSL_API(rtype,name,arg)   extern rtype __stdcall name    arg;
#else
  #define _Vsl_Api(rtype,name,arg)   extern rtype __cdecl   name    arg;
  #define _vsl_api(rtype,name,arg)   extern rtype __cdecl   name    arg;
  #define _VSL_API(rtype,name,arg)   extern rtype __cdecl   name    arg;
#endif

/*
//++
//  VSL CONTINUOUS DISTRIBUTION GENERATOR FUNCTION DECLARATIONS.
//--
*/
/* Cauchy distribution */
_Vsl_Api(int,vdRngCauchy,(const MKL_INT  , VSLStreamStatePtr  , const MKL_INT  , double [], const double  , const double  ))
_VSL_API(int,VDRNGCAUCHY,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, double [], const double *, const double *))
_vsl_api(int,vdrngcauchy,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, double [], const double *, const double *))
_Vsl_Api(int,vsRngCauchy,(const MKL_INT  , VSLStreamStatePtr  , const MKL_INT  , float [],  const float  ,  const float   ))
_VSL_API(int,VSRNGCAUCHY,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, float [],  const float *,  const float * ))
_vsl_api(int,vsrngcauchy,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, float [],  const float *,  const float * ))

/* Uniform distribution */
_Vsl_Api(int,vdRngUniform,(const MKL_INT  , VSLStreamStatePtr  , const MKL_INT  , double [], const double  , const double  ))
_VSL_API(int,VDRNGUNIFORM,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, double [], const double *, const double *))
_vsl_api(int,vdrnguniform,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, double [], const double *, const double *))
_Vsl_Api(int,vsRngUniform,(const MKL_INT  , VSLStreamStatePtr  , const MKL_INT  , float [],  const float  ,  const float   ))
_VSL_API(int,VSRNGUNIFORM,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, float [],  const float *,  const float * ))
_vsl_api(int,vsrnguniform,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, float [],  const float *,  const float * ))

/* Gaussian distribution */
_Vsl_Api(int,vdRngGaussian,(const MKL_INT  , VSLStreamStatePtr  , const MKL_INT  , double [], const double  , const double  ))
_VSL_API(int,VDRNGGAUSSIAN,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, double [], const double *, const double *))
_vsl_api(int,vdrnggaussian,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, double [], const double *, const double *))
_Vsl_Api(int,vsRngGaussian,(const MKL_INT  , VSLStreamStatePtr  , const MKL_INT  , float [],  const float  ,  const float   ))
_VSL_API(int,VSRNGGAUSSIAN,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, float [],  const float *,  const float * ))
_vsl_api(int,vsrnggaussian,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, float [],  const float *,  const float * ))

/* GaussianMV distribution */
_Vsl_Api(int,vdRngGaussianMV,(const MKL_INT  , VSLStreamStatePtr  , const MKL_INT  , double [], const MKL_INT  ,  const MKL_INT  , const double *, const double *))
_VSL_API(int,VDRNGGAUSSIANMV,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, double [], const MKL_INT *,  const MKL_INT *, const double *, const double *))
_vsl_api(int,vdrnggaussianmv,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, double [], const MKL_INT *,  const MKL_INT *, const double *, const double *))
_Vsl_Api(int,vsRngGaussianMV,(const MKL_INT  , VSLStreamStatePtr  , const MKL_INT  , float [],  const MKL_INT  ,  const MKL_INT  , const float *,  const float * ))
_VSL_API(int,VSRNGGAUSSIANMV,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, float [],  const MKL_INT *,  const MKL_INT *, const float *,  const float * ))
_vsl_api(int,vsrnggaussianmv,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, float [],  const MKL_INT *,  const MKL_INT *, const float *,  const float * ))

/* Exponential distribution */
_Vsl_Api(int,vdRngExponential,(const MKL_INT  , VSLStreamStatePtr  , const MKL_INT  ,  double [], const double  , const double  ))
_VSL_API(int,VDRNGEXPONENTIAL,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *,  double [], const double *, const double *))
_vsl_api(int,vdrngexponential,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *,  double [], const double *, const double *))
_Vsl_Api(int,vsRngExponential,(const MKL_INT  , VSLStreamStatePtr  , const MKL_INT  ,  float [],  const float  ,  const float   ))
_VSL_API(int,VSRNGEXPONENTIAL,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *,  float [],  const float *,  const float * ))
_vsl_api(int,vsrngexponential,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *,  float [],  const float *,  const float * ))

/* Laplace distribution */
_Vsl_Api(int,vdRngLaplace,(const MKL_INT  , VSLStreamStatePtr  , const MKL_INT  , double [], const double  , const double  ))
_VSL_API(int,VDRNGLAPLACE,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, double [], const double *, const double *))
_vsl_api(int,vdrnglaplace,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, double [], const double *, const double *))
_Vsl_Api(int,vsRngLaplace,(const MKL_INT  , VSLStreamStatePtr  , const MKL_INT  , float [],  const float  ,  const float   ))
_VSL_API(int,VSRNGLAPLACE,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, float [],  const float *,  const float * ))
_vsl_api(int,vsrnglaplace,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, float [],  const float *,  const float * ))

/* Weibull distribution */
_Vsl_Api(int,vdRngWeibull,(const MKL_INT  , VSLStreamStatePtr  , const MKL_INT  , double [], const double  , const double  , const double  ))
_VSL_API(int,VDRNGWEIBULL,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, double [], const double *, const double *, const double *))
_vsl_api(int,vdrngweibull,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, double [], const double *, const double *, const double *))
_Vsl_Api(int,vsRngWeibull,(const MKL_INT  , VSLStreamStatePtr  , const MKL_INT  , float [],  const float  ,  const float  ,  const float   ))
_VSL_API(int,VSRNGWEIBULL,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, float [],  const float *,  const float *,  const float * ))
_vsl_api(int,vsrngweibull,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, float [],  const float *,  const float *,  const float * ))

/* Rayleigh distribution */
_Vsl_Api(int,vdRngRayleigh,(const MKL_INT  , VSLStreamStatePtr  , const MKL_INT  ,  double [], const double  , const double  ))
_VSL_API(int,VDRNGRAYLEIGH,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *,  double [], const double *, const double *))
_vsl_api(int,vdrngrayleigh,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *,  double [], const double *, const double *))
_Vsl_Api(int,vsRngRayleigh,(const MKL_INT  , VSLStreamStatePtr  , const MKL_INT  ,  float [],  const float  ,  const float   ))
_VSL_API(int,VSRNGRAYLEIGH,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *,  float [],  const float *,  const float * ))
_vsl_api(int,vsrngrayleigh,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *,  float [],  const float *,  const float * ))

/* Lognormal distribution */
_Vsl_Api(int,vdRngLognormal,(const MKL_INT  , VSLStreamStatePtr  , const MKL_INT  , double [], const double  , const double  , const double  , const double  ))
_VSL_API(int,VDRNGLOGNORMAL,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, double [], const double *, const double *, const double *, const double *))
_vsl_api(int,vdrnglognormal,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, double [], const double *, const double *, const double *, const double *))
_Vsl_Api(int,vsRngLognormal,(const MKL_INT  , VSLStreamStatePtr  , const MKL_INT  , float [],  const float  ,  const float  ,  const float  ,  const float   ))
_VSL_API(int,VSRNGLOGNORMAL,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, float [],  const float *,  const float *,  const float *,  const float * ))
_vsl_api(int,vsrnglognormal,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, float [],  const float *,  const float *,  const float *,  const float * ))

/* Gumbel distribution */
_Vsl_Api(int,vdRngGumbel,(const MKL_INT  , VSLStreamStatePtr  , const MKL_INT  , double [], const double  , const double  ))
_VSL_API(int,VDRNGGUMBEL,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, double [], const double *, const double *))
_vsl_api(int,vdrnggumbel,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, double [], const double *, const double *))
_Vsl_Api(int,vsRngGumbel,(const MKL_INT  , VSLStreamStatePtr  , const MKL_INT  , float [],  const float  ,  const float   ))
_VSL_API(int,VSRNGGUMBEL,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, float [],  const float *,  const float * ))
_vsl_api(int,vsrnggumbel,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, float [],  const float *,  const float * ))

/* Gamma distribution */
_Vsl_Api(int,vdRngGamma,(const MKL_INT  , VSLStreamStatePtr  , const MKL_INT  , double [], const double  , const double  , const double  ))
_VSL_API(int,VDRNGGAMMA,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, double [], const double *, const double *, const double *))
_vsl_api(int,vdrnggamma,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, double [], const double *, const double *, const double *))
_Vsl_Api(int,vsRngGamma,(const MKL_INT  , VSLStreamStatePtr  , const MKL_INT  , float [],  const float  ,  const float  ,  const float   ))
_VSL_API(int,VSRNGGAMMA,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, float [],  const float *,  const float *,  const float * ))
_vsl_api(int,vsrnggamma,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, float [],  const float *,  const float *,  const float * ))

/* Beta distribution */
_Vsl_Api(int,vdRngBeta,(const MKL_INT  , VSLStreamStatePtr  , const MKL_INT  , double [], const double  , const double  , const double  , const double  ))
_VSL_API(int,VDRNGBETA,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, double [], const double *, const double *, const double *, const double *))
_vsl_api(int,vdrngbeta,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, double [], const double *, const double *, const double *, const double *))
_Vsl_Api(int,vsRngBeta,(const MKL_INT  , VSLStreamStatePtr  , const MKL_INT  , float [],  const float  ,  const float  ,  const float  ,  const float   ))
_VSL_API(int,VSRNGBETA,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, float [],  const float *,  const float *,  const float *,  const float * ))
_vsl_api(int,vsrngbeta,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, float [],  const float *,  const float *,  const float *,  const float * ))

/*
//++
//  VSL DISCRETE DISTRIBUTION GENERATOR FUNCTION DECLARATIONS.
//--
*/
/* Bernoulli distribution */
_Vsl_Api(int,viRngBernoulli,(const MKL_INT  , VSLStreamStatePtr  , const MKL_INT  , int [], const double  ))
_VSL_API(int,VIRNGBERNOULLI,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, int [], const double *))
_vsl_api(int,virngbernoulli,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, int [], const double *))

/* Uniform distribution */
_Vsl_Api(int,viRngUniform,(const MKL_INT  , VSLStreamStatePtr  , const MKL_INT  , int [], const int  , const int  ))
_VSL_API(int,VIRNGUNIFORM,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, int [], const int *, const int *))
_vsl_api(int,virnguniform,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, int [], const int *, const int *))

/* UniformBits distribution */
_Vsl_Api(int,viRngUniformBits,(const MKL_INT  , VSLStreamStatePtr  , const MKL_INT  , unsigned int []))
_VSL_API(int,VIRNGUNIFORMBITS,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, unsigned int []))
_vsl_api(int,virnguniformbits,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, unsigned int []))

/* Geometric distribution */
_Vsl_Api(int,viRngGeometric,(const MKL_INT  , VSLStreamStatePtr  , const MKL_INT  , int [], const double  ))
_VSL_API(int,VIRNGGEOMETRIC,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, int [], const double *))
_vsl_api(int,virnggeometric,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, int [], const double *))

/* Binomial distribution */
_Vsl_Api(int,viRngBinomial,(const MKL_INT  , VSLStreamStatePtr  , const MKL_INT  , int [], const int  , const double  ))
_VSL_API(int,VIRNGBINOMIAL,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, int [], const int *, const double *))
_vsl_api(int,virngbinomial,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, int [], const int *, const double *))

/* Hypergeometric distribution */
_Vsl_Api(int,viRngHypergeometric,(const MKL_INT  , VSLStreamStatePtr  , const MKL_INT  , int [], const int  , const int  , const int  ))
_VSL_API(int,VIRNGHYPERGEOMETRIC,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, int [], const int *, const int *, const int *))
_vsl_api(int,virnghypergeometric,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, int [], const int *, const int *, const int *))

/* Negbinomial distribution */
_Vsl_Api(int,viRngNegbinomial,(const MKL_INT  , VSLStreamStatePtr  , const MKL_INT  , int [], const double  , const double  ))
_Vsl_Api(int,viRngNegBinomial,(const MKL_INT  , VSLStreamStatePtr  , const MKL_INT  , int [], const double  , const double  ))
_VSL_API(int,VIRNGNEGBINOMIAL,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, int [], const double *, const double *))
_vsl_api(int,virngnegbinomial,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, int [], const double *, const double *))

/* Poisson distribution */
_Vsl_Api(int,viRngPoisson,(const MKL_INT  , VSLStreamStatePtr  , const MKL_INT  , int [], const double  ))
_VSL_API(int,VIRNGPOISSON,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, int [], const double *))
_vsl_api(int,virngpoisson,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, int [], const double *))

/* PoissonV distribution */
_Vsl_Api(int,viRngPoissonV,(const MKL_INT  , VSLStreamStatePtr  , const MKL_INT  , int [], const double []))
_VSL_API(int,VIRNGPOISSONV,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, int [], const double []))
_vsl_api(int,virngpoissonv,(const MKL_INT *, VSLStreamStatePtr *, const MKL_INT *, int [], const double []))


/*
//++
//  VSL SERVICE FUNCTION DECLARATIONS.
//--
*/
/* NewStream - stream creation/initialization */
_Vsl_Api(int,vslNewStream,(VSLStreamStatePtr* , const MKL_INT  , const unsigned MKL_INT  ))
_vsl_api(int,vslnewstream,(VSLStreamStatePtr* , const MKL_INT *, const unsigned MKL_INT *))
_VSL_API(int,VSLNEWSTREAM,(VSLStreamStatePtr* , const MKL_INT *, const unsigned MKL_INT *))

/* NewStreamEx - advanced stream creation/initialization */
_Vsl_Api(int,vslNewStreamEx,(VSLStreamStatePtr* , const MKL_INT  , const MKL_INT  , const unsigned int[]))
_vsl_api(int,vslnewstreamex,(VSLStreamStatePtr* , const MKL_INT *, const MKL_INT *, const unsigned int[]))
_VSL_API(int,VSLNEWSTREAMEX,(VSLStreamStatePtr* , const MKL_INT *, const MKL_INT *, const unsigned int[]))

_Vsl_Api(int,vsliNewAbstractStream,(VSLStreamStatePtr* , const MKL_INT  , const unsigned int[], const iUpdateFuncPtr))
_vsl_api(int,vslinewabstractstream,(VSLStreamStatePtr* , const MKL_INT *, const unsigned int[], const iUpdateFuncPtr))
_VSL_API(int,VSLINEWABSTRACTSTREAM,(VSLStreamStatePtr* , const MKL_INT *, const unsigned int[], const iUpdateFuncPtr))

_Vsl_Api(int,vsldNewAbstractStream,(VSLStreamStatePtr* , const MKL_INT  , const double[], const double  , const double  , const dUpdateFuncPtr))
_vsl_api(int,vsldnewabstractstream,(VSLStreamStatePtr* , const MKL_INT *, const double[], const double *, const double *, const dUpdateFuncPtr))
_VSL_API(int,VSLDNEWABSTRACTSTREAM,(VSLStreamStatePtr* , const MKL_INT *, const double[], const double *, const double *, const dUpdateFuncPtr))

_Vsl_Api(int,vslsNewAbstractStream,(VSLStreamStatePtr* , const MKL_INT  , const float[], const float  , const float  , const sUpdateFuncPtr))
_vsl_api(int,vslsnewabstractstream,(VSLStreamStatePtr* , const MKL_INT *, const float[], const float *, const float *, const sUpdateFuncPtr))
_VSL_API(int,VSLSNEWABSTRACTSTREAM,(VSLStreamStatePtr* , const MKL_INT *, const float[], const float *, const float *, const sUpdateFuncPtr))

/* DeleteStream - delete stream */
_Vsl_Api(int,vslDeleteStream,(VSLStreamStatePtr*))
_vsl_api(int,vsldeletestream,(VSLStreamStatePtr*))
_VSL_API(int,VSLDELETESTREAM,(VSLStreamStatePtr*))

/* CopyStream - copy all stream information */
_Vsl_Api(int,vslCopyStream,(VSLStreamStatePtr*, const VSLStreamStatePtr))
_vsl_api(int,vslcopystream,(VSLStreamStatePtr*, const VSLStreamStatePtr))
_VSL_API(int,VSLCOPYSTREAM,(VSLStreamStatePtr*, const VSLStreamStatePtr))

/* CopyStreamState - copy stream state only */
_Vsl_Api(int,vslCopyStreamState,(VSLStreamStatePtr  , const VSLStreamStatePtr  ))
_vsl_api(int,vslcopystreamstate,(VSLStreamStatePtr *, const VSLStreamStatePtr *))
_VSL_API(int,VSLCOPYSTREAMSTATE,(VSLStreamStatePtr *, const VSLStreamStatePtr *))

/* LeapfrogStream - leapfrog method */
_Vsl_Api(int,vslLeapfrogStream,(VSLStreamStatePtr  , const MKL_INT  , const MKL_INT  ))
_vsl_api(int,vslleapfrogstream,(VSLStreamStatePtr *, const MKL_INT *, const MKL_INT *))
_VSL_API(int,VSLLEAPFROGSTREAM,(VSLStreamStatePtr *, const MKL_INT *, const MKL_INT *))

/* SkipAheadStream - skip-ahead method */
#if defined(_MSC_VER)
_Vsl_Api(int,vslSkipAheadStream,(VSLStreamStatePtr  , const __int64  ))
_vsl_api(int,vslskipaheadstream,(VSLStreamStatePtr *, const __int64 *))
_VSL_API(int,VSLSKIPAHEADSTREAM,(VSLStreamStatePtr *, const __int64 *))
#else
_Vsl_Api(int,vslSkipAheadStream,(VSLStreamStatePtr  , const long long int  ))
_vsl_api(int,vslskipaheadstream,(VSLStreamStatePtr *, const long long int *))
_VSL_API(int,VSLSKIPAHEADSTREAM,(VSLStreamStatePtr *, const long long int *))
#endif

/* GetStreamStateBrng - get BRNG associated with given stream */
_Vsl_Api(int,vslGetStreamStateBrng,(const VSLStreamStatePtr  ))
_vsl_api(int,vslgetstreamstatebrng,(const VSLStreamStatePtr *))
_VSL_API(int,VSLGETSTREAMSTATEBRNG,(const VSLStreamStatePtr *))

/* GetNumRegBrngs - get number of registered BRNGs */
_Vsl_Api(int,vslGetNumRegBrngs,(void))
_vsl_api(int,vslgetnumregbrngs,(void))
_VSL_API(int,VSLGETNUMREGBRNGS,(void))

/* RegisterBrng - register new BRNG */
_Vsl_Api(int,vslRegisterBrng,(const VSLBRngProperties* ))
_vsl_api(int,vslregisterbrng,(const VSLBRngProperties* ))
_VSL_API(int,VSLREGISTERBRNG,(const VSLBRngProperties* ))

/* GetBrngProperties - get BRNG properties */
_Vsl_Api(int,vslGetBrngProperties,(const int  , VSLBRngProperties* ))
_vsl_api(int,vslgetbrngproperties,(const int *, VSLBRngProperties* ))
_VSL_API(int,VSLGETBRNGPROPERTIES,(const int *, VSLBRngProperties* ))


_Vsl_Api(int,vslSaveStreamF,(const VSLStreamStatePtr  , const char*             ))
_vsl_api(int,vslsavestreamf,(const VSLStreamStatePtr *, const char* , const int ))
_VSL_API(int,VSLSAVESTREAMF,(const VSLStreamStatePtr *, const char* , const int ))

_Vsl_Api(int,vslLoadStreamF,(VSLStreamStatePtr *, const char*             ))
_vsl_api(int,vslloadstreamf,(VSLStreamStatePtr *, const char* , const int ))
_VSL_API(int,VSLLOADSTREAMF,(VSLStreamStatePtr *, const char* , const int ))


/*
//++
//  VSL CONVOLUTION AND CORRELATION FUNCTION DECLARATIONS.
//--
*/

_Vsl_Api(int,vsldConvNewTask,(VSLConvTaskPtr* , const MKL_INT  , const MKL_INT  , const MKL_INT [], const MKL_INT [], const MKL_INT []))
_vsl_api(int,vsldconvnewtask,(VSLConvTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT [], const MKL_INT [], const MKL_INT []))
_VSL_API(int,VSLDCONVNEWTASK,(VSLConvTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT [], const MKL_INT [], const MKL_INT []))

_Vsl_Api(int,vslsConvNewTask,(VSLConvTaskPtr* , const MKL_INT  , const MKL_INT  , const MKL_INT [], const MKL_INT [], const MKL_INT []))
_vsl_api(int,vslsconvnewtask,(VSLConvTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT [], const MKL_INT [], const MKL_INT []))
_VSL_API(int,VSLSCONVNEWTASK,(VSLConvTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT [], const MKL_INT [], const MKL_INT []))

_Vsl_Api(int,vslzConvNewTask,(VSLConvTaskPtr* , const MKL_INT  , const MKL_INT  , const MKL_INT [], const MKL_INT [], const MKL_INT []))
_vsl_api(int,vslzconvnewtask,(VSLConvTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT [], const MKL_INT [], const MKL_INT []))
_VSL_API(int,VSLZCONVNEWTASK,(VSLConvTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT [], const MKL_INT [], const MKL_INT []))

_Vsl_Api(int,vslcConvNewTask,(VSLConvTaskPtr* , const MKL_INT  , const MKL_INT  , const MKL_INT [], const MKL_INT [], const MKL_INT []))
_vsl_api(int,vslcconvnewtask,(VSLConvTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT [], const MKL_INT [], const MKL_INT []))
_VSL_API(int,VSLCCONVNEWTASK,(VSLConvTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT [], const MKL_INT [], const MKL_INT []))

_Vsl_Api(int,vsldCorrNewTask,(VSLCorrTaskPtr* , const MKL_INT  , const MKL_INT  , const MKL_INT [], const MKL_INT [], const MKL_INT []))
_vsl_api(int,vsldcorrnewtask,(VSLCorrTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT [], const MKL_INT [], const MKL_INT []))
_VSL_API(int,VSLDCORRNEWTASK,(VSLCorrTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT [], const MKL_INT [], const MKL_INT []))

_Vsl_Api(int,vslsCorrNewTask,(VSLCorrTaskPtr* , const MKL_INT  , const MKL_INT  , const MKL_INT [], const MKL_INT [], const MKL_INT []))
_vsl_api(int,vslscorrnewtask,(VSLCorrTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT [], const MKL_INT [], const MKL_INT []))
_VSL_API(int,VSLSCORRNEWTASK,(VSLCorrTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT [], const MKL_INT [], const MKL_INT []))

_Vsl_Api(int,vslzCorrNewTask,(VSLCorrTaskPtr* , const MKL_INT  , const MKL_INT  , const MKL_INT [], const MKL_INT [], const MKL_INT []))
_vsl_api(int,vslzcorrnewtask,(VSLCorrTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT [], const MKL_INT [], const MKL_INT []))
_VSL_API(int,VSLZCORRNEWTASK,(VSLCorrTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT [], const MKL_INT [], const MKL_INT []))

_Vsl_Api(int,vslcCorrNewTask,(VSLCorrTaskPtr* , const MKL_INT  , const MKL_INT  , const MKL_INT [], const MKL_INT [], const MKL_INT []))
_vsl_api(int,vslccorrnewtask,(VSLCorrTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT [], const MKL_INT [], const MKL_INT []))
_VSL_API(int,VSLCCORRNEWTASK,(VSLCorrTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT [], const MKL_INT [], const MKL_INT []))


_Vsl_Api(int,vsldConvNewTask1D,(VSLConvTaskPtr* , const MKL_INT  , const MKL_INT  , const MKL_INT ,  const MKL_INT  ))
_vsl_api(int,vsldconvnewtask1d,(VSLConvTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT* , const MKL_INT* ))
_VSL_API(int,VSLDCONVNEWTASK1D,(VSLConvTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT* , const MKL_INT* ))

_Vsl_Api(int,vslsConvNewTask1D,(VSLConvTaskPtr* , const MKL_INT  , const MKL_INT  , const MKL_INT ,  const MKL_INT  ))
_vsl_api(int,vslsconvnewtask1d,(VSLConvTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT* , const MKL_INT* ))
_VSL_API(int,VSLSCONVNEWTASK1D,(VSLConvTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT* , const MKL_INT* ))

_Vsl_Api(int,vslzConvNewTask1D,(VSLConvTaskPtr* , const MKL_INT  , const MKL_INT  , const MKL_INT ,  const MKL_INT  ))
_vsl_api(int,vslzconvnewtask1d,(VSLConvTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT* , const MKL_INT* ))
_VSL_API(int,VSLZCONVNEWTASK1D,(VSLConvTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT* , const MKL_INT* ))

_Vsl_Api(int,vslcConvNewTask1D,(VSLConvTaskPtr* , const MKL_INT  , const MKL_INT  , const MKL_INT ,  const MKL_INT  ))
_vsl_api(int,vslcconvnewtask1d,(VSLConvTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT* , const MKL_INT* ))
_VSL_API(int,VSLCCONVNEWTASK1D,(VSLConvTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT* , const MKL_INT* ))

_Vsl_Api(int,vsldCorrNewTask1D,(VSLCorrTaskPtr* , const MKL_INT  , const MKL_INT  , const MKL_INT ,  const MKL_INT  ))
_vsl_api(int,vsldcorrnewtask1d,(VSLCorrTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT* , const MKL_INT* ))
_VSL_API(int,VSLDCORRNEWTASK1D,(VSLCorrTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT* , const MKL_INT* ))

_Vsl_Api(int,vslsCorrNewTask1D,(VSLCorrTaskPtr* , const MKL_INT  , const MKL_INT  , const MKL_INT ,  const MKL_INT  ))
_vsl_api(int,vslscorrnewtask1d,(VSLCorrTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT* , const MKL_INT* ))
_VSL_API(int,VSLSCORRNEWTASK1D,(VSLCorrTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT* , const MKL_INT* ))

_Vsl_Api(int,vslzCorrNewTask1D,(VSLCorrTaskPtr* , const MKL_INT  , const MKL_INT  , const MKL_INT ,  const MKL_INT  ))
_vsl_api(int,vslzcorrnewtask1d,(VSLCorrTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT* , const MKL_INT* ))
_VSL_API(int,VSLZCORRNEWTASK1D,(VSLCorrTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT* , const MKL_INT* ))

_Vsl_Api(int,vslcCorrNewTask1D,(VSLCorrTaskPtr* , const MKL_INT  , const MKL_INT  , const MKL_INT ,  const MKL_INT  ))
_vsl_api(int,vslccorrnewtask1d,(VSLCorrTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT* , const MKL_INT* ))
_VSL_API(int,VSLCCORRNEWTASK1D,(VSLCorrTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT* , const MKL_INT* ))


_Vsl_Api(int,vsldConvNewTaskX,(VSLConvTaskPtr* , const MKL_INT  , const MKL_INT  , const MKL_INT [], const MKL_INT [], const MKL_INT [], const double [], const MKL_INT []))
_vsl_api(int,vsldconvnewtaskx,(VSLConvTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT [], const MKL_INT [], const MKL_INT [], const double [], const MKL_INT []))
_VSL_API(int,VSLDCONVNEWTASKX,(VSLConvTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT [], const MKL_INT [], const MKL_INT [], const double [], const MKL_INT []))

_Vsl_Api(int,vslsConvNewTaskX,(VSLConvTaskPtr* , const MKL_INT  , const MKL_INT  , const MKL_INT [], const MKL_INT [], const MKL_INT [], const float [],  const MKL_INT []))
_vsl_api(int,vslsconvnewtaskx,(VSLConvTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT [], const MKL_INT [], const MKL_INT [], const float [],  const MKL_INT []))
_VSL_API(int,VSLSCONVNEWTASKX,(VSLConvTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT [], const MKL_INT [], const MKL_INT [], const float [],  const MKL_INT []))

_Vsl_Api(int,vslzConvNewTaskX,(VSLConvTaskPtr* , const MKL_INT  , const MKL_INT  , const MKL_INT [], const MKL_INT [], const MKL_INT [], const MKL_Complex16 [], const MKL_INT []))
_vsl_api(int,vslzconvnewtaskx,(VSLConvTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT [], const MKL_INT [], const MKL_INT [], const MKL_Complex16 [], const MKL_INT []))
_VSL_API(int,VSLZCONVNEWTASKX,(VSLConvTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT [], const MKL_INT [], const MKL_INT [], const MKL_Complex16 [], const MKL_INT []))

_Vsl_Api(int,vslcConvNewTaskX,(VSLConvTaskPtr* , const MKL_INT  , const MKL_INT  , const MKL_INT [], const MKL_INT [], const MKL_INT [], const MKL_Complex8 [],  const MKL_INT []))
_vsl_api(int,vslcconvnewtaskx,(VSLConvTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT [], const MKL_INT [], const MKL_INT [], const MKL_Complex8 [],  const MKL_INT []))
_VSL_API(int,VSLCCONVNEWTASKX,(VSLConvTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT [], const MKL_INT [], const MKL_INT [], const MKL_Complex8 [],  const MKL_INT []))

_Vsl_Api(int,vsldCorrNewTaskX,(VSLCorrTaskPtr* , const MKL_INT  , const MKL_INT  , const MKL_INT [], const MKL_INT [], const MKL_INT [], const double [], const MKL_INT []))
_vsl_api(int,vsldcorrnewtaskx,(VSLCorrTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT [], const MKL_INT [], const MKL_INT [], const double [], const MKL_INT []))
_VSL_API(int,VSLDCORRNEWTASKX,(VSLCorrTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT [], const MKL_INT [], const MKL_INT [], const double [], const MKL_INT []))

_Vsl_Api(int,vslsCorrNewTaskX,(VSLCorrTaskPtr* , const MKL_INT  , const MKL_INT  , const MKL_INT [], const MKL_INT [], const MKL_INT [], const float [],  const MKL_INT []))
_vsl_api(int,vslscorrnewtaskx,(VSLCorrTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT [], const MKL_INT [], const MKL_INT [], const float [],  const MKL_INT []))
_VSL_API(int,VSLSCORRNEWTASKX,(VSLCorrTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT [], const MKL_INT [], const MKL_INT [], const float [],  const MKL_INT []))

_Vsl_Api(int,vslzCorrNewTaskX,(VSLCorrTaskPtr* , const MKL_INT  , const MKL_INT  , const MKL_INT [], const MKL_INT [], const MKL_INT [], const MKL_Complex16 [], const MKL_INT []))
_vsl_api(int,vslzcorrnewtaskx,(VSLCorrTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT [], const MKL_INT [], const MKL_INT [], const MKL_Complex16 [], const MKL_INT []))
_VSL_API(int,VSLZCORRNEWTASKX,(VSLCorrTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT [], const MKL_INT [], const MKL_INT [], const MKL_Complex16 [], const MKL_INT []))

_Vsl_Api(int,vslcCorrNewTaskX,(VSLCorrTaskPtr* , const MKL_INT  , const MKL_INT  , const MKL_INT [], const MKL_INT [], const MKL_INT [], const MKL_Complex8 [],  const MKL_INT []))
_vsl_api(int,vslccorrnewtaskx,(VSLCorrTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT [], const MKL_INT [], const MKL_INT [], const MKL_Complex8 [],  const MKL_INT []))
_VSL_API(int,VSLCCORRNEWTASKX,(VSLCorrTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT [], const MKL_INT [], const MKL_INT [], const MKL_Complex8 [],  const MKL_INT []))


_Vsl_Api(int,vsldConvNewTaskX1D,(VSLConvTaskPtr* , const MKL_INT  , const MKL_INT  , const MKL_INT  , const MKL_INT  , const double [], const MKL_INT  ))
_vsl_api(int,vsldconvnewtaskx1d,(VSLConvTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT* , const MKL_INT* , const double [], const MKL_INT* ))
_VSL_API(int,VSLDCONVNEWTASKX1D,(VSLConvTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT* , const MKL_INT* , const double [], const MKL_INT* ))

_Vsl_Api(int,vslsConvNewTaskX1D,(VSLConvTaskPtr* , const MKL_INT  , const MKL_INT  , const MKL_INT  , const MKL_INT  , const float [],  const MKL_INT  ))
_vsl_api(int,vslsconvnewtaskx1d,(VSLConvTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT* , const MKL_INT* , const float [],  const MKL_INT* ))
_VSL_API(int,VSLSCONVNEWTASKX1D,(VSLConvTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT* , const MKL_INT* , const float [],  const MKL_INT* ))

_Vsl_Api(int,vslzConvNewTaskX1D,(VSLConvTaskPtr* , const MKL_INT  , const MKL_INT  , const MKL_INT  , const MKL_INT  , const MKL_Complex16 [], const MKL_INT  ))
_vsl_api(int,vslzconvnewtaskx1d,(VSLConvTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT* , const MKL_INT* , const MKL_Complex16 [], const MKL_INT* ))
_VSL_API(int,VSLZCONVNEWTASKX1D,(VSLConvTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT* , const MKL_INT* , const MKL_Complex16 [], const MKL_INT* ))

_Vsl_Api(int,vslcConvNewTaskX1D,(VSLConvTaskPtr* , const MKL_INT  , const MKL_INT  , const MKL_INT  , const MKL_INT  , const MKL_Complex8 [],  const MKL_INT  ))
_vsl_api(int,vslcconvnewtaskx1d,(VSLConvTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT* , const MKL_INT* , const MKL_Complex8 [],  const MKL_INT* ))
_VSL_API(int,VSLCCONVNEWTASKX1D,(VSLConvTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT* , const MKL_INT* , const MKL_Complex8 [],  const MKL_INT* ))

_Vsl_Api(int,vsldCorrNewTaskX1D,(VSLCorrTaskPtr* , const MKL_INT  , const MKL_INT  , const MKL_INT  , const MKL_INT  , const double [], const MKL_INT  ))
_vsl_api(int,vsldcorrnewtaskx1d,(VSLCorrTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT* , const MKL_INT* , const double [], const MKL_INT* ))
_VSL_API(int,VSLDCORRNEWTASKX1D,(VSLCorrTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT* , const MKL_INT* , const double [], const MKL_INT* ))

_Vsl_Api(int,vslsCorrNewTaskX1D,(VSLCorrTaskPtr* , const MKL_INT  , const MKL_INT  , const MKL_INT  , const MKL_INT  , const float [],  const MKL_INT  ))
_vsl_api(int,vslscorrnewtaskx1d,(VSLCorrTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT* , const MKL_INT* , const float [],  const MKL_INT* ))
_VSL_API(int,VSLSCORRNEWTASKX1D,(VSLCorrTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT* , const MKL_INT* , const float [],  const MKL_INT* ))

_Vsl_Api(int,vslzCorrNewTaskX1D,(VSLCorrTaskPtr* , const MKL_INT  , const MKL_INT  , const MKL_INT  , const MKL_INT  , const MKL_Complex16 [], const MKL_INT  ))
_vsl_api(int,vslzcorrnewtaskx1d,(VSLCorrTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT* , const MKL_INT* , const MKL_Complex16 [], const MKL_INT* ))
_VSL_API(int,VSLZCORRNEWTASKX1D,(VSLCorrTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT* , const MKL_INT* , const MKL_Complex16 [], const MKL_INT* ))

_Vsl_Api(int,vslcCorrNewTaskX1D,(VSLCorrTaskPtr* , const MKL_INT  , const MKL_INT  , const MKL_INT  , const MKL_INT  , const MKL_Complex8 [],  const MKL_INT  ))
_vsl_api(int,vslccorrnewtaskx1d,(VSLCorrTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT* , const MKL_INT* , const MKL_Complex8 [],  const MKL_INT* ))
_VSL_API(int,VSLCCORRNEWTASKX1D,(VSLCorrTaskPtr* , const MKL_INT* , const MKL_INT* , const MKL_INT* , const MKL_INT* , const MKL_Complex8 [],  const MKL_INT* ))


_Vsl_Api(int,vslConvDeleteTask,(VSLConvTaskPtr* ))
_vsl_api(int,vslconvdeletetask,(VSLConvTaskPtr* ))
_VSL_API(int,VSLCONVDeleteTask,(VSLConvTaskPtr* ))

_Vsl_Api(int,vslCorrDeleteTask,(VSLCorrTaskPtr* ))
_vsl_api(int,vslcorrdeletetask,(VSLCorrTaskPtr* ))
_VSL_API(int,VSLCORRDeleteTask,(VSLCorrTaskPtr* ))


_Vsl_Api(int,vslConvCopyTask,(VSLConvTaskPtr* , const VSLConvTaskPtr  ))
_vsl_api(int,vslconvcopytask,(VSLConvTaskPtr* , const VSLConvTaskPtr* ))
_VSL_API(int,VSLCONVCopyTask,(VSLConvTaskPtr* , const VSLConvTaskPtr* ))

_Vsl_Api(int,vslCorrCopyTask,(VSLCorrTaskPtr* , const VSLCorrTaskPtr  ))
_vsl_api(int,vslcorrcopytask,(VSLCorrTaskPtr* , const VSLCorrTaskPtr* ))
_VSL_API(int,VSLCORRCopyTask,(VSLCorrTaskPtr* , const VSLCorrTaskPtr* ))


_Vsl_Api(int,vslConvSetMode,(VSLConvTaskPtr  , const MKL_INT  ))
_vsl_api(int,vslconvsetmode,(VSLConvTaskPtr* , const MKL_INT* ))
_VSL_API(int,VSLCONVSETMOME,(VSLConvTaskPtr* , const MKL_INT* ))

_Vsl_Api(int,vslCorrSetMode,(VSLCorrTaskPtr  , const MKL_INT  ))
_vsl_api(int,vslcorrsetmode,(VSLCorrTaskPtr* , const MKL_INT* ))
_VSL_API(int,VSLCORRSETMODE,(VSLCorrTaskPtr* , const MKL_INT* ))


_Vsl_Api(int,vslConvSetInternalPrecision,(VSLConvTaskPtr  , const MKL_INT  ))
_vsl_api(int,vslconvsetinternalprecision,(VSLConvTaskPtr* , const MKL_INT* ))
_VSL_API(int,VSLCONVSETINTERNALPRECISION,(VSLConvTaskPtr* , const MKL_INT* ))

_Vsl_Api(int,vslCorrSetInternalPrecision,(VSLCorrTaskPtr  , const MKL_INT  ))
_vsl_api(int,vslcorrsetinternalprecision,(VSLCorrTaskPtr* , const MKL_INT* ))
_VSL_API(int,VSLCORRSETINTERNALPRECISION,(VSLCorrTaskPtr* , const MKL_INT* ))


_Vsl_Api(int,vslConvSetStart,(VSLConvTaskPtr  , const MKL_INT []))
_vsl_api(int,vslconvsetstart,(VSLConvTaskPtr* , const MKL_INT []))
_VSL_API(int,VSLCONVSETSTART,(VSLConvTaskPtr* , const MKL_INT []))

_Vsl_Api(int,vslCorrSetStart,(VSLCorrTaskPtr  , const MKL_INT []))
_vsl_api(int,vslcorrsetstart,(VSLCorrTaskPtr* , const MKL_INT []))
_VSL_API(int,VSLCORRSETSTART,(VSLCorrTaskPtr* , const MKL_INT []))


_Vsl_Api(int,vslConvSetDecimation,(VSLConvTaskPtr  , const MKL_INT []))
_vsl_api(int,vslconvsetdecimation,(VSLConvTaskPtr* , const MKL_INT []))
_VSL_API(int,VSLCONVSETDECIMATION,(VSLConvTaskPtr* , const MKL_INT []))

_Vsl_Api(int,vslCorrSetDecimation,(VSLCorrTaskPtr  , const MKL_INT []))
_vsl_api(int,vslcorrsetdecimation,(VSLCorrTaskPtr* , const MKL_INT []))
_VSL_API(int,VSLCORRSETDECIMATION,(VSLCorrTaskPtr* , const MKL_INT []))


_Vsl_Api(int,vsldConvExec,(VSLConvTaskPtr  , const double [], const MKL_INT [], const double [], const MKL_INT [], double [], const MKL_INT []))
_vsl_api(int,vsldconvexec,(VSLConvTaskPtr* , const double [], const MKL_INT [], const double [], const MKL_INT [], double [], const MKL_INT []))
_VSL_API(int,VSLDCONVEXEC,(VSLConvTaskPtr* , const double [], const MKL_INT [], const double [], const MKL_INT [], double [], const MKL_INT []))

_Vsl_Api(int,vslsConvExec,(VSLConvTaskPtr  , const float [],  const MKL_INT [], const float [],  const MKL_INT [], float [],  const MKL_INT []))
_vsl_api(int,vslsconvexec,(VSLConvTaskPtr* , const float [],  const MKL_INT [], const float [],  const MKL_INT [], float [],  const MKL_INT []))
_VSL_API(int,VSLSCONVEXEC,(VSLConvTaskPtr* , const float [],  const MKL_INT [], const float [],  const MKL_INT [], float [],  const MKL_INT []))

_Vsl_Api(int,vslzConvExec,(VSLConvTaskPtr  , const MKL_Complex16 [], const MKL_INT [], const MKL_Complex16 [], const MKL_INT [], MKL_Complex16 [], const MKL_INT []))
_vsl_api(int,vslzconvexec,(VSLConvTaskPtr* , const MKL_Complex16 [], const MKL_INT [], const MKL_Complex16 [], const MKL_INT [], MKL_Complex16 [], const MKL_INT []))
_VSL_API(int,VSLZCONVEXEC,(VSLConvTaskPtr* , const MKL_Complex16 [], const MKL_INT [], const MKL_Complex16 [], const MKL_INT [], MKL_Complex16 [], const MKL_INT []))

_Vsl_Api(int,vslcConvExec,(VSLConvTaskPtr  , const MKL_Complex8 [],  const MKL_INT [], const MKL_Complex8 [],  const MKL_INT [], MKL_Complex8 [],  const MKL_INT []))
_vsl_api(int,vslcconvexec,(VSLConvTaskPtr* , const MKL_Complex8 [],  const MKL_INT [], const MKL_Complex8 [],  const MKL_INT [], MKL_Complex8 [],  const MKL_INT []))
_VSL_API(int,VSLCCONVEXEC,(VSLConvTaskPtr* , const MKL_Complex8 [],  const MKL_INT [], const MKL_Complex8 [],  const MKL_INT [], MKL_Complex8 [],  const MKL_INT []))

_Vsl_Api(int,vsldCorrExec,(VSLCorrTaskPtr  , const double [], const MKL_INT [], const double [], const MKL_INT [], double [], const MKL_INT []))
_vsl_api(int,vsldcorrexec,(VSLCorrTaskPtr* , const double [], const MKL_INT [], const double [], const MKL_INT [], double [], const MKL_INT []))
_VSL_API(int,VSLDCORREXEC,(VSLCorrTaskPtr* , const double [], const MKL_INT [], const double [], const MKL_INT [], double [], const MKL_INT []))

_Vsl_Api(int,vslsCorrExec,(VSLCorrTaskPtr  , const float [],  const MKL_INT [], const float [],  const MKL_INT [], float [],  const MKL_INT []))
_vsl_api(int,vslscorrexec,(VSLCorrTaskPtr* , const float [],  const MKL_INT [], const float [],  const MKL_INT [], float [],  const MKL_INT []))
_VSL_API(int,VSLSCORREXEC,(VSLCorrTaskPtr* , const float [],  const MKL_INT [], const float [],  const MKL_INT [], float [],  const MKL_INT []))

_Vsl_Api(int,vslzCorrExec,(VSLCorrTaskPtr  , const MKL_Complex16 [], const MKL_INT [], const MKL_Complex16 [], const MKL_INT [], MKL_Complex16 [], const MKL_INT []))
_vsl_api(int,vslzcorrexec,(VSLCorrTaskPtr* , const MKL_Complex16 [], const MKL_INT [], const MKL_Complex16 [], const MKL_INT [], MKL_Complex16 [], const MKL_INT []))
_VSL_API(int,VSLZCORREXEC,(VSLCorrTaskPtr* , const MKL_Complex16 [], const MKL_INT [], const MKL_Complex16 [], const MKL_INT [], MKL_Complex16 [], const MKL_INT []))

_Vsl_Api(int,vslcCorrExec,(VSLCorrTaskPtr  , const MKL_Complex8 [],  const MKL_INT [], const MKL_Complex8 [],  const MKL_INT [], MKL_Complex8 [],  const MKL_INT []))
_vsl_api(int,vslccorrexec,(VSLCorrTaskPtr* , const MKL_Complex8 [],  const MKL_INT [], const MKL_Complex8 [],  const MKL_INT [], MKL_Complex8 [],  const MKL_INT []))
_VSL_API(int,VSLCCORREXEC,(VSLCorrTaskPtr* , const MKL_Complex8 [],  const MKL_INT [], const MKL_Complex8 [],  const MKL_INT [], MKL_Complex8 [],  const MKL_INT []))


_Vsl_Api(int,vsldConvExec1D,(VSLConvTaskPtr  , const double [], const MKL_INT  , const double [], const MKL_INT  , double [], const MKL_INT  ))
_vsl_api(int,vsldconvexec1d,(VSLConvTaskPtr* , const double [], const MKL_INT* , const double [], const MKL_INT* , double [], const MKL_INT* ))
_VSL_API(int,VSLDCONVEXEC1D,(VSLConvTaskPtr* , const double [], const MKL_INT* , const double [], const MKL_INT* , double [], const MKL_INT* ))

_Vsl_Api(int,vslsConvExec1D,(VSLConvTaskPtr  , const float [],  const MKL_INT  , const float [],  const MKL_INT  , float [],  const MKL_INT  ))
_vsl_api(int,vslsconvexec1d,(VSLConvTaskPtr* , const float [],  const MKL_INT* , const float [],  const MKL_INT* , float [],  const MKL_INT* ))
_VSL_API(int,VSLSCONVEXEC1D,(VSLConvTaskPtr* , const float [],  const MKL_INT* , const float [],  const MKL_INT* , float [],  const MKL_INT* ))

_Vsl_Api(int,vslzConvExec1D,(VSLConvTaskPtr  , const MKL_Complex16 [], const MKL_INT  , const MKL_Complex16 [], const MKL_INT  , MKL_Complex16 [], const MKL_INT  ))
_vsl_api(int,vslzconvexec1d,(VSLConvTaskPtr* , const MKL_Complex16 [], const MKL_INT* , const MKL_Complex16 [], const MKL_INT* , MKL_Complex16 [], const MKL_INT* ))
_VSL_API(int,VSLZCONVEXEC1D,(VSLConvTaskPtr* , const MKL_Complex16 [], const MKL_INT* , const MKL_Complex16 [], const MKL_INT* , MKL_Complex16 [], const MKL_INT* ))

_Vsl_Api(int,vslcConvExec1D,(VSLConvTaskPtr  , const MKL_Complex8 [],  const MKL_INT  , const MKL_Complex8 [],  const MKL_INT  , MKL_Complex8 [],  const MKL_INT  ))
_vsl_api(int,vslcconvexec1d,(VSLConvTaskPtr* , const MKL_Complex8 [],  const MKL_INT* , const MKL_Complex8 [],  const MKL_INT* , MKL_Complex8 [],  const MKL_INT* ))
_VSL_API(int,VSLCCONVEXEC1D,(VSLConvTaskPtr* , const MKL_Complex8 [],  const MKL_INT* , const MKL_Complex8 [],  const MKL_INT* , MKL_Complex8 [],  const MKL_INT* ))

_Vsl_Api(int,vsldCorrExec1D,(VSLCorrTaskPtr  , const double [], const MKL_INT  , const double [], const MKL_INT  , double [], const MKL_INT  ))
_vsl_api(int,vsldcorrexec1d,(VSLCorrTaskPtr* , const double [], const MKL_INT* , const double [], const MKL_INT* , double [], const MKL_INT* ))
_VSL_API(int,VSLDCORREXEC1D,(VSLCorrTaskPtr* , const double [], const MKL_INT* , const double [], const MKL_INT* , double [], const MKL_INT* ))

_Vsl_Api(int,vslsCorrExec1D,(VSLCorrTaskPtr  , const float [],  const MKL_INT  , const float [],  const MKL_INT  , float [],  const MKL_INT  ))
_vsl_api(int,vslscorrexec1d,(VSLCorrTaskPtr* , const float [],  const MKL_INT* , const float [],  const MKL_INT* , float [],  const MKL_INT* ))
_VSL_API(int,VSLSCORREXEC1D,(VSLCorrTaskPtr* , const float [],  const MKL_INT* , const float [],  const MKL_INT* , float [],  const MKL_INT* ))

_Vsl_Api(int,vslzCorrExec1D,(VSLCorrTaskPtr  , const MKL_Complex16 [], const MKL_INT  , const MKL_Complex16 [], const MKL_INT  , MKL_Complex16 [], const MKL_INT  ))
_vsl_api(int,vslzcorrexec1d,(VSLCorrTaskPtr* , const MKL_Complex16 [], const MKL_INT* , const MKL_Complex16 [], const MKL_INT* , MKL_Complex16 [], const MKL_INT* ))
_VSL_API(int,VSLZCORREXEC1D,(VSLCorrTaskPtr* , const MKL_Complex16 [], const MKL_INT* , const MKL_Complex16 [], const MKL_INT* , MKL_Complex16 [], const MKL_INT* ))

_Vsl_Api(int,vslcCorrExec1D,(VSLCorrTaskPtr  , const MKL_Complex8 [],  const MKL_INT  , const MKL_Complex8 [],  const MKL_INT  , MKL_Complex8 [],  const MKL_INT  ))
_vsl_api(int,vslccorrexec1d,(VSLCorrTaskPtr* , const MKL_Complex8 [],  const MKL_INT* , const MKL_Complex8 [],  const MKL_INT* , MKL_Complex8 [],  const MKL_INT* ))
_VSL_API(int,VSLCCORREXEC1D,(VSLCorrTaskPtr* , const MKL_Complex8 [],  const MKL_INT* , const MKL_Complex8 [],  const MKL_INT* , MKL_Complex8 [],  const MKL_INT* ))


_Vsl_Api(int,vsldConvExecX,(VSLConvTaskPtr  , const double [], const MKL_INT [], double [], const MKL_INT []))
_vsl_api(int,vsldconvexecx,(VSLConvTaskPtr* , const double [], const MKL_INT [], double [], const MKL_INT []))
_VSL_API(int,VSLDCONVEXECX,(VSLConvTaskPtr* , const double [], const MKL_INT [], double [], const MKL_INT []))

_Vsl_Api(int,vslsConvExecX,(VSLConvTaskPtr  , const float [],  const MKL_INT [], float [],  const MKL_INT []))
_vsl_api(int,vslsconvexecx,(VSLConvTaskPtr* , const float [],  const MKL_INT [], float [],  const MKL_INT []))
_VSL_API(int,VSLSCONVEXECX,(VSLConvTaskPtr* , const float [],  const MKL_INT [], float [],  const MKL_INT []))

_Vsl_Api(int,vslzConvExecX,(VSLConvTaskPtr  , const MKL_Complex16 [], const MKL_INT [], MKL_Complex16 [], const MKL_INT []))
_vsl_api(int,vslzconvexecx,(VSLConvTaskPtr* , const MKL_Complex16 [], const MKL_INT [], MKL_Complex16 [], const MKL_INT []))
_VSL_API(int,VSLZCONVEXECX,(VSLConvTaskPtr* , const MKL_Complex16 [], const MKL_INT [], MKL_Complex16 [], const MKL_INT []))

_Vsl_Api(int,vslcConvExecX,(VSLConvTaskPtr  , const MKL_Complex8 [],  const MKL_INT [], MKL_Complex8 [],  const MKL_INT []))
_vsl_api(int,vslcconvexecx,(VSLConvTaskPtr* , const MKL_Complex8 [],  const MKL_INT [], MKL_Complex8 [],  const MKL_INT []))
_VSL_API(int,VSLCCONVEXECX,(VSLConvTaskPtr* , const MKL_Complex8 [],  const MKL_INT [], MKL_Complex8 [],  const MKL_INT []))

_Vsl_Api(int,vsldCorrExecX,(VSLCorrTaskPtr  , const double [], const MKL_INT [], double [], const MKL_INT []))
_vsl_api(int,vsldcorrexecx,(VSLCorrTaskPtr* , const double [], const MKL_INT [], double [], const MKL_INT []))
_VSL_API(int,VSLDCORREXECX,(VSLCorrTaskPtr* , const double [], const MKL_INT [], double [], const MKL_INT []))

_Vsl_Api(int,vslsCorrExecX,(VSLCorrTaskPtr  , const float [],  const MKL_INT [], float [],  const MKL_INT []))
_vsl_api(int,vslscorrexecx,(VSLCorrTaskPtr* , const float [],  const MKL_INT [], float [],  const MKL_INT []))
_VSL_API(int,VSLSCORREXECX,(VSLCorrTaskPtr* , const float [],  const MKL_INT [], float [],  const MKL_INT []))

_Vsl_Api(int,vslzCorrExecX,(VSLCorrTaskPtr  , const MKL_Complex16 [], const MKL_INT [], MKL_Complex16 [], const MKL_INT []))
_vsl_api(int,vslzcorrexecx,(VSLCorrTaskPtr* , const MKL_Complex16 [], const MKL_INT [], MKL_Complex16 [], const MKL_INT []))
_VSL_API(int,VSLZCORREXECX,(VSLCorrTaskPtr* , const MKL_Complex16 [], const MKL_INT [], MKL_Complex16 [], const MKL_INT []))

_Vsl_Api(int,vslcCorrExecX,(VSLCorrTaskPtr  , const MKL_Complex8 [],  const MKL_INT [], MKL_Complex8 [],  const MKL_INT []))
_vsl_api(int,vslccorrexecx,(VSLCorrTaskPtr* , const MKL_Complex8 [],  const MKL_INT [], MKL_Complex8 [],  const MKL_INT []))
_VSL_API(int,VSLCCORREXECX,(VSLCorrTaskPtr* , const MKL_Complex8 [],  const MKL_INT [], MKL_Complex8 [],  const MKL_INT []))


_Vsl_Api(int,vsldConvExecX1D,(VSLConvTaskPtr  , const double [], const MKL_INT  , double [], const MKL_INT  ))
_vsl_api(int,vsldconvexecx1d,(VSLConvTaskPtr* , const double [], const MKL_INT* , double [], const MKL_INT* ))
_VSL_API(int,VSLDCONVEXECX1D,(VSLConvTaskPtr* , const double [], const MKL_INT* , double [], const MKL_INT* ))

_Vsl_Api(int,vslsConvExecX1D,(VSLConvTaskPtr  , const float [],  const MKL_INT  , float [],  const MKL_INT  ))
_vsl_api(int,vslsconvexecx1d,(VSLConvTaskPtr* , const float [],  const MKL_INT* , float [],  const MKL_INT* ))
_VSL_API(int,VSLSCONVEXECX1D,(VSLConvTaskPtr* , const float [],  const MKL_INT* , float [],  const MKL_INT* ))

_Vsl_Api(int,vslzConvExecX1D,(VSLConvTaskPtr  , const MKL_Complex16 [], const MKL_INT  , MKL_Complex16 [], const MKL_INT  ))
_vsl_api(int,vslzconvexecx1d,(VSLConvTaskPtr* , const MKL_Complex16 [], const MKL_INT* , MKL_Complex16 [], const MKL_INT* ))
_VSL_API(int,VSLZCONVEXECX1D,(VSLConvTaskPtr* , const MKL_Complex16 [], const MKL_INT* , MKL_Complex16 [], const MKL_INT* ))

_Vsl_Api(int,vslcConvExecX1D,(VSLConvTaskPtr  , const MKL_Complex8 [],  const MKL_INT  , MKL_Complex8 [],  const MKL_INT  ))
_vsl_api(int,vslcconvexecx1d,(VSLConvTaskPtr* , const MKL_Complex8 [],  const MKL_INT* , MKL_Complex8 [],  const MKL_INT* ))
_VSL_API(int,VSLCCONVEXECX1D,(VSLConvTaskPtr* , const MKL_Complex8 [],  const MKL_INT* , MKL_Complex8 [],  const MKL_INT* ))

_Vsl_Api(int,vsldCorrExecX1D,(VSLCorrTaskPtr  , const double [], const MKL_INT  , double [], const MKL_INT  ))
_vsl_api(int,vsldcorrexecx1d,(VSLCorrTaskPtr* , const double [], const MKL_INT* , double [], const MKL_INT* ))
_VSL_API(int,VSLDCORREXECX1D,(VSLCorrTaskPtr* , const double [], const MKL_INT* , double [], const MKL_INT* ))

_Vsl_Api(int,vslsCorrExecX1D,(VSLCorrTaskPtr  , const float [],  const MKL_INT  , float [],  const MKL_INT  ))
_vsl_api(int,vslscorrexecx1d,(VSLCorrTaskPtr* , const float [],  const MKL_INT* , float [],  const MKL_INT* ))
_VSL_API(int,VSLSCORREXECX1D,(VSLCorrTaskPtr* , const float [],  const MKL_INT* , float [],  const MKL_INT* ))

_Vsl_Api(int,vslzCorrExecX1D,(VSLCorrTaskPtr  , const MKL_Complex16 [], const MKL_INT  , MKL_Complex16 [], const MKL_INT  ))
_vsl_api(int,vslzcorrexecx1d,(VSLCorrTaskPtr* , const MKL_Complex16 [], const MKL_INT* , MKL_Complex16 [], const MKL_INT* ))
_VSL_API(int,VSLZCORREXECX1D,(VSLCorrTaskPtr* , const MKL_Complex16 [], const MKL_INT* , MKL_Complex16 [], const MKL_INT* ))

_Vsl_Api(int,vslcCorrExecX1D,(VSLCorrTaskPtr  , const MKL_Complex8 [],  const MKL_INT  , MKL_Complex8 [],  const MKL_INT  ))
_vsl_api(int,vslccorrexecx1d,(VSLCorrTaskPtr* , const MKL_Complex8 [],  const MKL_INT* , MKL_Complex8 [],  const MKL_INT* ))
_VSL_API(int,VSLCCORREXECX1D,(VSLCorrTaskPtr* , const MKL_Complex8 [],  const MKL_INT* , MKL_Complex8 [],  const MKL_INT* ))


#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __MKL_VSL_FUNCTIONS_H__ */
