! file: mkl_vsl.f77
!
!                             INTEL CONFIDENTIAL
!  Copyright(C) 2006-2010 Intel Corporation. All Rights Reserved.
!  The source code contained  or  described herein and all documents related to
!  the source code ("Material") are owned by Intel Corporation or its suppliers
!  or licensors.  Title to the  Material remains with  Intel Corporation or its
!  suppliers and licensors. The Material contains trade secrets and proprietary
!  and  confidential  information of  Intel or its suppliers and licensors. The
!  Material  is  protected  by  worldwide  copyright  and trade secret laws and
!  treaty  provisions. No part of the Material may be used, copied, reproduced,
!  modified, published, uploaded, posted, transmitted, distributed or disclosed
!  in any way without Intel's prior express written permission.
!  No license  under any  patent, copyright, trade secret or other intellectual
!  property right is granted to or conferred upon you by disclosure or delivery
!  of the Materials,  either expressly, by implication, inducement, estoppel or
!  otherwise.  Any  license  under  such  intellectual property  rights must be
!  express and approved by Intel in writing.
!++
!  Fortran 77 VSL interface.
!--

!++
!  Definitions for VSL functions return values (errors, warnings)
!--

!  "No error" status
      INTEGER*4 VSL_STATUS_OK
      INTEGER*4 VSL_ERROR_OK
      PARAMETER (VSL_STATUS_OK = 0)
      PARAMETER (VSL_ERROR_OK  = 0)

!  Common errors (-1..-999)
      INTEGER*4 VSL_ERROR_FEATURE_NOT_IMPLEMENTED
      INTEGER*4 VSL_ERROR_UNKNOWN
      INTEGER*4 VSL_ERROR_BADARGS
      INTEGER*4 VSL_ERROR_MEM_FAILURE
      INTEGER*4 VSL_ERROR_NULL_PTR
      INTEGER*4 VSL_ERROR_CPU_NOT_SUPPORTED
      PARAMETER (VSL_ERROR_FEATURE_NOT_IMPLEMENTED = -1)
      PARAMETER (VSL_ERROR_UNKNOWN                 = -2)
      PARAMETER (VSL_ERROR_BADARGS                 = -3)
      PARAMETER (VSL_ERROR_MEM_FAILURE             = -4)
      PARAMETER (VSL_ERROR_NULL_PTR                = -5)
      PARAMETER (VSL_ERROR_CPU_NOT_SUPPORTED       = -6)

!  RNG errors (-1000..-1999)
!  brng errors
      INTEGER*4 VSL_ERROR_INVALID_BRNG_INDEX
      INTEGER*4 VSL_ERROR_LEAPFROG_UNSUPPORTED
      INTEGER*4 VSL_ERROR_SKIPAHEAD_UNSUPPORTED
      INTEGER*4 VSL_ERROR_BRNGS_INCOMPATIBLE
      INTEGER*4 VSL_ERROR_BAD_STREAM
      INTEGER*4 VSL_ERROR_BRNG_TABLE_FULL
      INTEGER*4 VSL_ERROR_BAD_STREAM_STATE_SIZE
      INTEGER*4 VSL_ERROR_BAD_WORD_SIZE
      INTEGER*4 VSL_ERROR_BAD_NSEEDS
      INTEGER*4 VSL_ERROR_BAD_NBITS
      INTEGER*4 VSL_QRNG_PERIOD_ELAPSED
      INTEGER*4 VSL_ERROR_LEAPFROG_NSTREAMS_TOO_BIG
      PARAMETER (VSL_ERROR_INVALID_BRNG_INDEX        = -1000)
      PARAMETER (VSL_ERROR_LEAPFROG_UNSUPPORTED      = -1002)
      PARAMETER (VSL_ERROR_SKIPAHEAD_UNSUPPORTED     = -1003)
      PARAMETER (VSL_ERROR_BRNGS_INCOMPATIBLE        = -1005)
      PARAMETER (VSL_ERROR_BAD_STREAM                = -1006)
      PARAMETER (VSL_ERROR_BRNG_TABLE_FULL           = -1007)
      PARAMETER (VSL_ERROR_BAD_STREAM_STATE_SIZE     = -1008)
      PARAMETER (VSL_ERROR_BAD_WORD_SIZE             = -1009)
      PARAMETER (VSL_ERROR_BAD_NSEEDS                = -1010)
      PARAMETER (VSL_ERROR_BAD_NBITS                 = -1011)
      PARAMETER (VSL_QRNG_PERIOD_ELAPSED             = -1012)
      PARAMETER (VSL_ERROR_LEAPFROG_NSTREAMS_TOO_BIG = -1013)

! abstract stream related errors
      INTEGER*4 VSL_ERROR_BAD_UPDATE
      INTEGER*4 VSL_ERROR_NO_NUMBERS
      INTEGER*4 VSL_ERROR_INVALID_ABSTRACT_STREAM
      PARAMETER (VSL_ERROR_BAD_UPDATE              = -1120)
      PARAMETER (VSL_ERROR_NO_NUMBERS              = -1121)
      PARAMETER (VSL_ERROR_INVALID_ABSTRACT_STREAM = -1122)

! read/write stream to file errors
      INTEGER*4 VSL_ERROR_FILE_CLOSE
      INTEGER*4 VSL_ERROR_FILE_OPEN
      INTEGER*4 VSL_ERROR_FILE_WRITE
      INTEGER*4 VSL_ERROR_FILE_READ

      INTEGER*4 VSL_ERROR_BAD_FILE_FORMAT
      INTEGER*4 VSL_ERROR_UNSUPPORTED_FILE_VER
      PARAMETER (VSL_ERROR_FILE_CLOSE           = -1100)
      PARAMETER (VSL_ERROR_FILE_OPEN            = -1101)
      PARAMETER (VSL_ERROR_FILE_WRITE           = -1102)
      PARAMETER (VSL_ERROR_FILE_READ            = -1103)

      PARAMETER (VSL_ERROR_BAD_FILE_FORMAT      = -1110)
      PARAMETER (VSL_ERROR_UNSUPPORTED_FILE_VER = -1111)

! Convolution/correlation errors
      INTEGER*4 VSL_CC_ERROR_NOT_IMPLEMENTED
      INTEGER*4 VSL_CC_ERROR_ALLOCATION_FAILURE
      INTEGER*4 VSL_CC_ERROR_BAD_DESCRIPTOR
      INTEGER*4 VSL_CC_ERROR_SERVICE_FAILURE
      INTEGER*4 VSL_CC_ERROR_EDIT_FAILURE
      INTEGER*4 VSL_CC_ERROR_EDIT_PROHIBITED
      INTEGER*4 VSL_CC_ERROR_COMMIT_FAILURE
      INTEGER*4 VSL_CC_ERROR_COPY_FAILURE
      INTEGER*4 VSL_CC_ERROR_DELETE_FAILURE
      INTEGER*4 VSL_CC_ERROR_BAD_ARGUMENT
      INTEGER*4 VSL_CC_ERROR_DIMS
      INTEGER*4 VSL_CC_ERROR_START
      INTEGER*4 VSL_CC_ERROR_DECIMATION
      INTEGER*4 VSL_CC_ERROR_XSHAPE
      INTEGER*4 VSL_CC_ERROR_YSHAPE
      INTEGER*4 VSL_CC_ERROR_ZSHAPE
      INTEGER*4 VSL_CC_ERROR_XSTRIDE
      INTEGER*4 VSL_CC_ERROR_YSTRIDE
      INTEGER*4 VSL_CC_ERROR_ZSTRIDE
      INTEGER*4 VSL_CC_ERROR_X
      INTEGER*4 VSL_CC_ERROR_Y
      INTEGER*4 VSL_CC_ERROR_Z
      INTEGER*4 VSL_CC_ERROR_JOB
      INTEGER*4 VSL_CC_ERROR_KIND
      INTEGER*4 VSL_CC_ERROR_MODE
      INTEGER*4 VSL_CC_ERROR_TYPE
      INTEGER*4 VSL_CC_ERROR_PRECISION
      INTEGER*4 VSL_CC_ERROR_EXTERNAL_PRECISION
      INTEGER*4 VSL_CC_ERROR_INTERNAL_PRECISION
      INTEGER*4 VSL_CC_ERROR_METHOD
      INTEGER*4 VSL_CC_ERROR_OTHER
      PARAMETER (VSL_CC_ERROR_NOT_IMPLEMENTED    = -2000)
      PARAMETER (VSL_CC_ERROR_ALLOCATION_FAILURE = -2001)
      PARAMETER (VSL_CC_ERROR_BAD_DESCRIPTOR     = -2200)
      PARAMETER (VSL_CC_ERROR_SERVICE_FAILURE    = -2210)
      PARAMETER (VSL_CC_ERROR_EDIT_FAILURE       = -2211)
      PARAMETER (VSL_CC_ERROR_EDIT_PROHIBITED    = -2212)
      PARAMETER (VSL_CC_ERROR_COMMIT_FAILURE     = -2220)
      PARAMETER (VSL_CC_ERROR_COPY_FAILURE       = -2230)
      PARAMETER (VSL_CC_ERROR_DELETE_FAILURE     = -2240)
      PARAMETER (VSL_CC_ERROR_BAD_ARGUMENT       = -2300)
      PARAMETER (VSL_CC_ERROR_DIMS               = -2301)
      PARAMETER (VSL_CC_ERROR_START              = -2302)
      PARAMETER (VSL_CC_ERROR_DECIMATION         = -2303)
      PARAMETER (VSL_CC_ERROR_XSHAPE             = -2311)
      PARAMETER (VSL_CC_ERROR_YSHAPE             = -2312)
      PARAMETER (VSL_CC_ERROR_ZSHAPE             = -2313)
      PARAMETER (VSL_CC_ERROR_XSTRIDE            = -2321)
      PARAMETER (VSL_CC_ERROR_YSTRIDE            = -2322)
      PARAMETER (VSL_CC_ERROR_ZSTRIDE            = -2323)
      PARAMETER (VSL_CC_ERROR_X                  = -2331)
      PARAMETER (VSL_CC_ERROR_Y                  = -2332)
      PARAMETER (VSL_CC_ERROR_Z                  = -2333)
      PARAMETER (VSL_CC_ERROR_JOB                = -2100)
      PARAMETER (VSL_CC_ERROR_KIND               = -2110)
      PARAMETER (VSL_CC_ERROR_MODE               = -2120)
      PARAMETER (VSL_CC_ERROR_TYPE               = -2130)
      PARAMETER (VSL_CC_ERROR_PRECISION          = -2400)
      PARAMETER (VSL_CC_ERROR_EXTERNAL_PRECISION = -2141)
      PARAMETER (VSL_CC_ERROR_INTERNAL_PRECISION = -2142)
      PARAMETER (VSL_CC_ERROR_METHOD             = -2400)
      PARAMETER (VSL_CC_ERROR_OTHER              = -2800)

!++
!  CONV/CORR RELATED MACRO DEFINITIONS
!--
      INTEGER*4 VSL_CONV_MODE_AUTO
      INTEGER*4 VSL_CORR_MODE_AUTO
      INTEGER*4 VSL_CONV_MODE_DIRECT
      INTEGER*4 VSL_CORR_MODE_DIRECT
      INTEGER*4 VSL_CONV_MODE_FFT
      INTEGER*4 VSL_CORR_MODE_FFT
      INTEGER*4 VSL_CONV_PRECISION_SINGLE
      INTEGER*4 VSL_CORR_PRECISION_SINGLE
      INTEGER*4 VSL_CONV_PRECISION_DOUBLE
      INTEGER*4 VSL_CORR_PRECISION_DOUBLE
      PARAMETER (VSL_CONV_MODE_AUTO        = 0)
      PARAMETER (VSL_CORR_MODE_AUTO        = 0)
      PARAMETER (VSL_CONV_MODE_DIRECT      = 1)
      PARAMETER (VSL_CORR_MODE_DIRECT      = 1)
      PARAMETER (VSL_CONV_MODE_FFT         = 2)
      PARAMETER (VSL_CORR_MODE_FFT         = 2)
      PARAMETER (VSL_CONV_PRECISION_SINGLE = 1)
      PARAMETER (VSL_CORR_PRECISION_SINGLE = 1)
      PARAMETER (VSL_CONV_PRECISION_DOUBLE = 2)
      PARAMETER (VSL_CORR_PRECISION_DOUBLE = 2)


!++
!  BASIC RANDOM NUMBER GENERATOR (BRNG) RELATED MACRO DEFINITIONS
!--


!  MAX NUMBER OF BRNGS CAN BE REGISTERED IN VSL
!  No more than VSL_MAX_REG_BRNGS basic generators can be registered in VSL
!  (including predefined basic generators).
!
!  Change this number to increase/decrease number of BRNGs can be registered.
      INTEGER*4 VSL_MAX_REG_BRNGS
      PARAMETER (VSL_MAX_REG_BRNGS = 512)

!  PREDEFINED BRNG NAMES
      INTEGER*4 VSL_BRNG_SHIFT
      INTEGER*4 VSL_BRNG_INC

      INTEGER*4 VSL_BRNG_MCG31
      INTEGER*4 VSL_BRNG_R250
      INTEGER*4 VSL_BRNG_MRG32K3A
      INTEGER*4 VSL_BRNG_MCG59
      INTEGER*4 VSL_BRNG_WH
      INTEGER*4 VSL_BRNG_SOBOL
      INTEGER*4 VSL_BRNG_NIEDERR
      INTEGER*4 VSL_BRNG_MT19937
      INTEGER*4 VSL_BRNG_MT2203
      INTEGER*4 VSL_BRNG_IABSTRACT
      INTEGER*4 VSL_BRNG_DABSTRACT
      INTEGER*4 VSL_BRNG_SABSTRACT

      PARAMETER (VSL_BRNG_INC  = Z"00100000")

      PARAMETER (VSL_BRNG_MCG31    =VSL_BRNG_INC)
      PARAMETER (VSL_BRNG_R250     =VSL_BRNG_MCG31    +VSL_BRNG_INC)
      PARAMETER (VSL_BRNG_MRG32K3A =VSL_BRNG_R250     +VSL_BRNG_INC)
      PARAMETER (VSL_BRNG_MCG59    =VSL_BRNG_MRG32K3A +VSL_BRNG_INC)
      PARAMETER (VSL_BRNG_WH       =VSL_BRNG_MCG59    +VSL_BRNG_INC)
      PARAMETER (VSL_BRNG_SOBOL    =VSL_BRNG_WH       +VSL_BRNG_INC)
      PARAMETER (VSL_BRNG_NIEDERR  =VSL_BRNG_SOBOL    +VSL_BRNG_INC)
      PARAMETER (VSL_BRNG_MT19937  =VSL_BRNG_NIEDERR  +VSL_BRNG_INC)
      PARAMETER (VSL_BRNG_MT2203   =VSL_BRNG_MT19937  +VSL_BRNG_INC)
      PARAMETER (VSL_BRNG_IABSTRACT=VSL_BRNG_MT2203   +VSL_BRNG_INC)
      PARAMETER (VSL_BRNG_DABSTRACT=VSL_BRNG_IABSTRACT+VSL_BRNG_INC)
      PARAMETER (VSL_BRNG_SABSTRACT=VSL_BRNG_DABSTRACT+VSL_BRNG_INC)

!  LEAPFROG METHOD FOR GRAY-CODE BASED QUASI-RANDOM NUMBER BASIC GENERATORS
!  VSL_BRNG_SOBOL and VSL_BRNG_NIEDERR are Gray-code based quasi-random number
!  basic generators. In contrast to pseudorandom number basic generators,
!  quasi-random ones take the dimension as initialization parameter.
!
!  Suppose that quasi-random number generator (QRNG) dimension is S. QRNG
!  sequence is a sequence of S-dimensional vectors:
!
!     x0=(x0[0],x0[1],...,x0[S-1]),x1=(x1[0],x1[1],...,x1[S-1]),...
!
!  VSL treats the output of any basic generator as 1-dimensional, however:
!
!     x0[0],x0[1],...,x0[S-1],x1[0],x1[1],...,x1[S-1],...
!
!  Because of nature of VSL_BRNG_SOBOL and VSL_BRNG_NIEDERR QRNGs,
!  the only S-stride Leapfrog method is supported for them. In other words,
!  user can generate subsequences, which consist of fixed elements of
!  vectors x0,x1,... For example, if 0 element is fixed, the following
!  subsequence is generated:
!
!     x0[1],x1[1],x2[1],...
!
!  To use the s-stride Leapfrog method with given QRNG, user should call
!  vslLeapfrogStream function with parameter k equal to element to be fixed
!  (0<=k<S) and parameter nstreams equal to VSL_QRNG_LEAPFROG_COMPONENTS.
      INTEGER*4 VSL_QRNG_LEAPFROG_COMPONENTS
      PARAMETER (VSL_QRNG_LEAPFROG_COMPONENTS = Z"7FFFFFFF")


!  USER-DEFINED PARAMETERS FOR QUASI-RANDOM NUMBER BASIC GENERATORS
!  VSL_BRNG_SOBOL and VSL_BRNG_NIEDERR are Gray-code based quasi-random
!  number basic generators. Default parameters of the generators
!  support generation of quasi-random number vectors of dimensions
!  S<=40 for SOBOL and S<=318 for NIEDERRITER. The library provides
!  opportunity to register user-defined initial values for the
!  generators and generate quasi-random vectors of desirable dimension.
!  There is also opportunity to register user-defined parameters for
!  default dimensions and obtain another sequence of quasi-random vectors.
!  Service function vslNewStreamEx is used to pass the parameters to
!  the library. Data are packed into array params, parameter of the routine.
!  First element of the array is used for dimension S, second element
!  contains indicator, VSL_USER_QRNG_INITIAL_VALUES, of user-defined
!  parameters for quasi-random number generators.
!  Macros VSL_USER_PRIMITIVE_POLYMS and VSL_USER_INIT_DIRECTION_NUMBERS
!  are used to describe which data are passed to SOBOL QRNG and
!  VSL_USER_IRRED_POLYMS - which data are passed to NIEDERRITER QRNG.
!  For example, to demonstrate that both primitive polynomials and initial
!  direction numbers are passed in SOBOL one should set third element of the
!  array params to  VSL_USER_PRIMITIVE_POLYMS | VSL_USER_DIRECTION_NUMBERS.
!  Macro VSL_QRNG_OVERRIDE_1ST_DIM_INIT is used to override default
!  initialization for the first dimension. Macro VSL_USER_DIRECTION_NUMBERS
!  is used when direction numbers calculated on the user side are passed
!  into the generators. More detailed description of interface for
!  registration of user-defined QRNG initial parameters can be found
!  in VslNotes.pdf.
      INTEGER*4 VSL_USER_QRNG_INITIAL_VALUES
      INTEGER*4 VSL_USER_PRIMITIVE_POLYMS
      INTEGER*4 VSL_USER_IRRED_POLYMS
      INTEGER*4 VSL_USER_DIRECTION_NUMBERS
      INTEGER*4 VSL_QRNG_OVERRIDE_1ST_DIM_INIT

      PARAMETER (VSL_USER_QRNG_INITIAL_VALUES    = 1)
      PARAMETER (VSL_USER_PRIMITIVE_POLYMS       = 1)
      PARAMETER (VSL_USER_INIT_DIRECTION_NUMBERS = 2)
      PARAMETER (VSL_USER_IRRED_POLYMS           = 1)
      PARAMETER (VSL_USER_DIRECTION_NUMBERS      = 4)
      PARAMETER (VSL_QRNG_OVERRIDE_1ST_DIM_INIT  = 8)

!  INITIALIZATION METHODS FOR USER-DESIGNED BASIC RANDOM NUMBER GENERATORS.
!  Each BRNG must support at least VSL_INIT_METHOD_STANDARD initialization
!  method. In addition, VSL_INIT_METHOD_LEAPFROG and VSL_INIT_METHOD_SKIPAHEAD
!  initialization methods can be supported.
!
!  If VSL_INIT_METHOD_LEAPFROG is not supported then initialization routine
!  must return VSL_ERROR_LEAPFROG_UNSUPPORTED error code.
!
!  If VSL_INIT_METHOD_SKIPAHEAD is not supported then initialization routine
!  must return VSL_ERROR_SKIPAHEAD_UNSUPPORTED error code.
!
!  If there is no error during initialization, the initialization routine must
!  return VSL_ERROR_OK code.
      INTEGER*4 VSL_INIT_METHOD_STANDARD
      INTEGER*4 VSL_INIT_METHOD_LEAPFROG
      INTEGER*4 VSL_INIT_METHOD_SKIPAHEAD
      PARAMETER (VSL_INIT_METHOD_STANDARD  = 0)
      PARAMETER (VSL_INIT_METHOD_LEAPFROG  = 1)
      PARAMETER (VSL_INIT_METHOD_SKIPAHEAD = 2)

!++
!  ACCURACY FLAG FOR DISTRIBUTION GENERATORS
!  This flag defines mode of random number generation.
!  If accuracy mode is set distribution generators will produce
!  numbers lying exactly within definitional domain for all values
!  of distribution parameters. In this case slight performance
!  degradation is expected. By default accuracy mode is switched off
!  admitting random numbers to be out of the definitional domain for
!  specific values of distribution parameters.
!  This macro is used to form names for accuracy versions of
!  distribution number generators
!--
      INTEGER*4 VSL_METHOD_ACCURACY_FLAG
      PARAMETER (VSL_METHOD_ACCURACY_FLAG = Z"40000000")

!++
!  TRANSFORMATION METHOD NAMES FOR DISTRIBUTION RANDOM NUMBER GENERATORS
!  VSL interface allows more than one generation method in a distribution
!  transformation subroutine. Following macro definitions are used to
!  specify generation method for given distribution generator.
!
!  Method name macro is constructed as
!
!     VSL_METHOD_<Precision><Distribution>_<Method>
!
!  where
!
!     <Precision> - S (single precision) or D (double precision)
!     <Distribution> - probability distribution
!     <Method> - method name
!
!  VSL_METHOD_<Precision><Distribution>_<Method> should be used with
!  vsl<precision>Rng<Distribution> function only, where
!
!     <precision> - s (single) or d (double)
!     <Distribution> - probability distribution
!--

! Uniform
!
! <Method>   <Short Description>
! STD        standard method. Currently there is only one method for this
!            distribution generator
      INTEGER VSL_METHOD_SUNIFORM_STD
      INTEGER VSL_METHOD_DUNIFORM_STD
      INTEGER VSL_METHOD_IUNIFORM_STD
      PARAMETER (VSL_METHOD_SUNIFORM_STD = 0)
      PARAMETER (VSL_METHOD_DUNIFORM_STD = 0)
      PARAMETER (VSL_METHOD_IUNIFORM_STD = 0)

      INTEGER VSL_METHOD_SUNIFORM_STD_ACCURATE
      INTEGER VSL_METHOD_DUNIFORM_STD_ACCURATE
      PARAMETER (VSL_METHOD_SUNIFORM_STD_ACCURATE = Z"40000000")
      PARAMETER (VSL_METHOD_DUNIFORM_STD_ACCURATE = Z"40000000")


! Uniform Bits
!
! <Method>   <Short Description>
! STD        standard method. Currently there is only one method for this
!            distribution generator
      INTEGER VSL_METHOD_IUNIFORMBITS_STD
      PARAMETER (VSL_METHOD_IUNIFORMBITS_STD = 0)

! Gaussian
!
! <Method>   <Short Description>
! BOXMULLER  generates normally distributed random number x thru the pair of
!            uniformly distributed numbers u1 and u2 according to the formula:
!
!               x=sqrt(-ln(u1))*sin(2*Pi*u2)
!
! BOXMULLER2 generates pair of normally distributed random numbers x1 and x2
!            thru the pair of uniformly dustributed numbers u1 and u2
!            according to the formula
!
!               x1=sqrt(-ln(u1))*sin(2*Pi*u2)
!               x2=sqrt(-ln(u1))*cos(2*Pi*u2)
!
!            NOTE: implementation correctly works with odd vector lengths
!
! ICDF       inverse cumulative distribution function method
      INTEGER VSL_METHOD_SGAUSSIAN_BOXMULLER
      INTEGER VSL_METHOD_SGAUSSIAN_BOXMULLER2
      INTEGER VSL_METHOD_SGAUSSIAN_ICDF
      INTEGER VSL_METHOD_DGAUSSIAN_BOXMULLER
      INTEGER VSL_METHOD_DGAUSSIAN_BOXMULLER2
      INTEGER VSL_METHOD_DGAUSSIAN_ICDF
      PARAMETER (VSL_METHOD_SGAUSSIAN_BOXMULLER  = 0)
      PARAMETER (VSL_METHOD_SGAUSSIAN_BOXMULLER2 = 1)
      PARAMETER (VSL_METHOD_SGAUSSIAN_ICDF       = 2)
      PARAMETER (VSL_METHOD_DGAUSSIAN_BOXMULLER  = 0)
      PARAMETER (VSL_METHOD_DGAUSSIAN_BOXMULLER2 = 1)
      PARAMETER (VSL_METHOD_DGAUSSIAN_ICDF       = 2)

! GaussianMV - multivariate (correlated) normal
! Multivariate (correlated) normal random number generator is based on
! uncorrelated Gaussian random number generator (see vslsRngGaussian and
! vsldRngGaussian functions):
!
! <Method>   <Short Description>
! BOXMULLER  generates normally distributed random number x thru the pair of
!            uniformly distributed numbers u1 and u2 according to the formula:
!
!               x=sqrt(-ln(u1))*sin(2*Pi*u2)
!
! BOXMULLER2 generates pair of normally distributed random numbers x1 and x2
!            thru the pair of uniformly dustributed numbers u1 and u2
!            according to the formula
!
!               x1=sqrt(-ln(u1))*sin(2*Pi*u2)
!               x2=sqrt(-ln(u1))*cos(2*Pi*u2)
!
!            NOTE: implementation correctly works with odd vector lengths
!
! ICDF       inverse cumulative distribution function method
      INTEGER VSL_METHOD_SGAUSSIANMV_BOXMULLER
      INTEGER VSL_METHOD_SGAUSSIANMV_BOXMULLER2
      INTEGER VSL_METHOD_SGAUSSIANMV_ICDF
      INTEGER VSL_METHOD_DGAUSSIANMV_BOXMULLER
      INTEGER VSL_METHOD_DGAUSSIANMV_BOXMULLER2
      INTEGER VSL_METHOD_DGAUSSIANMV_ICDF
      PARAMETER (VSL_METHOD_SGAUSSIANMV_BOXMULLER  = 0)
      PARAMETER (VSL_METHOD_SGAUSSIANMV_BOXMULLER2 = 1)
      PARAMETER (VSL_METHOD_SGAUSSIANMV_ICDF       = 2)
      PARAMETER (VSL_METHOD_DGAUSSIANMV_BOXMULLER  = 0)
      PARAMETER (VSL_METHOD_DGAUSSIANMV_BOXMULLER2 = 1)
      PARAMETER (VSL_METHOD_DGAUSSIANMV_ICDF       = 2)

! Exponential
!
! <Method>   <Short Description>
! ICDF       inverse cumulative distribution function method
      INTEGER VSL_METHOD_SEXPONENTIAL_ICDF
      INTEGER VSL_METHOD_DEXPONENTIAL_ICDF
      PARAMETER (VSL_METHOD_SEXPONENTIAL_ICDF = 0)
      PARAMETER (VSL_METHOD_DEXPONENTIAL_ICDF = 0)

      INTEGER VSL_METHOD_SEXPONENTIAL_ICDF_ACCURATE
      INTEGER VSL_METHOD_DEXPONENTIAL_ICDF_ACCURATE
      PARAMETER (VSL_METHOD_SEXPONENTIAL_ICDF_ACCURATE = Z"40000000")
      PARAMETER (VSL_METHOD_DEXPONENTIAL_ICDF_ACCURATE = Z"40000000")

! Laplace
!
! <Method>   <Short Description>
! ICDF       inverse cumulative distribution function method
!
! ICDF - inverse cumulative distribution function method:
!
!           x=+/-ln(u) with probability 1/2,
!
!        where
!
!           x - random number with Laplace distribution,
!           u - uniformly distributed random number
      INTEGER VSL_METHOD_SLAPLACE_ICDF
      INTEGER VSL_METHOD_DLAPLACE_ICDF
      PARAMETER (VSL_METHOD_SLAPLACE_ICDF = 0)
      PARAMETER (VSL_METHOD_DLAPLACE_ICDF = 0)

! Weibull
!
! <Method>   <Short Description>
! ICDF       inverse cumulative distribution function method
      INTEGER VSL_METHOD_SWEIBULL_ICDF
      INTEGER VSL_METHOD_DWEIBULL_ICDF
      PARAMETER (VSL_METHOD_SWEIBULL_ICDF = 0)
      PARAMETER (VSL_METHOD_DWEIBULL_ICDF = 0)

      INTEGER VSL_METHOD_SWEIBULL_ICDF_ACCURATE
      INTEGER VSL_METHOD_DWEIBULL_ICDF_ACCURATE
      PARAMETER (VSL_METHOD_SWEIBULL_ICDF_ACCURATE = Z"40000000")
      PARAMETER (VSL_METHOD_DWEIBULL_ICDF_ACCURATE = Z"40000000")



! Cauchy
!
! <Method>   <Short Description>
! ICDF       inverse cumulative distribution function method
      INTEGER VSL_METHOD_SCAUCHY_ICDF
      INTEGER VSL_METHOD_DCAUCHY_ICDF
      PARAMETER (VSL_METHOD_SCAUCHY_ICDF = 0)
      PARAMETER (VSL_METHOD_DCAUCHY_ICDF = 0)

! Rayleigh
!
! <Method>   <Short Description>
! ICDF       inverse cumulative distribution function method
      INTEGER VSL_METHOD_SRAYLEIGH_ICDF
      INTEGER VSL_METHOD_DRAYLEIGH_ICDF
      PARAMETER (VSL_METHOD_SRAYLEIGH_ICDF = 0)
      PARAMETER (VSL_METHOD_DRAYLEIGH_ICDF = 0)

      INTEGER VSL_METHOD_SRAYLEIGH_ICDF_ACCURATE
      INTEGER VSL_METHOD_DRAYLEIGH_ICDF_ACCURATE
      PARAMETER (VSL_METHOD_SRAYLEIGH_ICDF_ACCURATE = Z"40000000")
      PARAMETER (VSL_METHOD_DRAYLEIGH_ICDF_ACCURATE = Z"40000000")


! Lognormal
!
! <Method>   <Short Description>
! BOXMULLER2      Box-Muller 2 algorithm based method
      INTEGER VSL_METHOD_SLOGNORMAL_BOXMULLER2
      INTEGER VSL_METHOD_DLOGNORMAL_BOXMULLER2
      INTEGER VSL_METHOD_SLOGNORMAL_ICDF
      INTEGER VSL_METHOD_DLOGNORMAL_ICDF
      PARAMETER (VSL_METHOD_SLOGNORMAL_BOXMULLER2 = 0)
      PARAMETER (VSL_METHOD_DLOGNORMAL_BOXMULLER2 = 0)
      PARAMETER (VSL_METHOD_SLOGNORMAL_ICDF = 0)
      PARAMETER (VSL_METHOD_DLOGNORMAL_ICDF = 0)

      INTEGER VSL_METHOD_SLOGNORMAL_BOXMULLER2_ACCURATE
      INTEGER VSL_METHOD_DLOGNORMAL_BOXMULLER2_ACCURATE
      INTEGER VSL_METHOD_SLOGNORMAL_ICDF_ACCURATE
      INTEGER VSL_METHOD_DLOGNORMAL_ICDF_ACCURATE
      PARAMETER (VSL_METHOD_SLOGNORMAL_BOXMULLER2_ACCURATE=Z"40000000")
      PARAMETER (VSL_METHOD_DLOGNORMAL_BOXMULLER2_ACCURATE=Z"40000000")
      PARAMETER (VSL_METHOD_SLOGNORMAL_ICDF_ACCURATE = Z"40000000")
      PARAMETER (VSL_METHOD_DLOGNORMAL_ICDF_ACCURATE = Z"40000000")


! Gumbel
!
! <Method>   <Short Description>
! ICDF       inverse cumulative distribution function method
      INTEGER VSL_METHOD_SGUMBEL_ICDF
      INTEGER VSL_METHOD_DGUMBEL_ICDF
      PARAMETER (VSL_METHOD_SGUMBEL_ICDF = 0)
      PARAMETER (VSL_METHOD_DGUMBEL_ICDF = 0)

! Gamma
!
! <Method>     <Short Description>
! GNORM        nonlinear transformation of gaussian numbers
! alpha>1,     based on acceptance/rejection method with
!              squeezes
!
! alpha>=0.6,  rejection from the Weibull distribution
! alpha<1
!
! alpha<0.6,   transformation of exponential power distribution
!              (EPD), EPD random numbers are generated using
!              by means of acceptance/rejection technique
      INTEGER VSL_METHOD_SGAMMA_GNORM
      INTEGER VSL_METHOD_DGAMMA_GNORM
      PARAMETER (VSL_METHOD_SGAMMA_GNORM = 0)
      PARAMETER (VSL_METHOD_DGAMMA_GNORM = 0)

      INTEGER VSL_METHOD_SGAMMA_GNORM_ACCURATE
      INTEGER VSL_METHOD_DGAMMA_GNORM_ACCURATE
      PARAMETER (VSL_METHOD_SGAMMA_GNORM_ACCURATE = Z"40000000")
      PARAMETER (VSL_METHOD_DGAMMA_GNORM_ACCURATE = Z"40000000")


! Beta
!
! <Method>     <Short Description>
! CJA - stands for first letters of Cheng, Johnk, and Atkinson
! Cheng      - generation of beta random numbers of the second kind
! min(p,q)>1   based on acceptance/rejection technique and its
!              transformation to beta random numbers of the first kind;
!
! Johnk,     - if q + K*p^2+C<=0, K=0.852..., C=-0.956...
! Atkinson,    algorithm of Johnk: beta distributed random number
! max(p,q)<1   is generated as u1^(1/p) / (u1^(1/p)+u2^(1/q)),
!              if u1^(1/p)+u2^(1/q)<=1;
!              otherwise switching algorithm of Atkinson:
!              interval (0,1) is divided into two domains (0,t) and (t,1),
!              on each interval acceptance/rejection technique with
!              convenient majorizing function is used;
!
! Atkinson   - switching algorithm of Atkinson is used
! min(p,q)<1   (with another point t, see short description above);
! max(p,q)>1
!
! ICDF       - inverse cumulative distribution function method according
!              to formulas x=1-u^(1/q) for p = 1, and x = u^(1/p) for q=1,
!              where x is beta distributed random number,
!              u - uniformly distributed random number.
!              for p=q=1 beta distribution reduces to uniform distribution.

      INTEGER VSL_METHOD_SBETA_CJA
      INTEGER VSL_METHOD_DBETA_CJA
      PARAMETER (VSL_METHOD_SBETA_CJA = 0)
      PARAMETER (VSL_METHOD_DBETA_CJA = 0)

      INTEGER VSL_METHOD_SBETA_CJA_ACCURATE
      INTEGER VSL_METHOD_DBETA_CJA_ACCURATE
      PARAMETER (VSL_METHOD_SBETA_CJA_ACCURATE = Z"40000000")
      PARAMETER (VSL_METHOD_DBETA_CJA_ACCURATE = Z"40000000")



! Bernoulli
!
! <Method>   <Short Description>
! ICDF       inverse cumulative distribution function method
      INTEGER VSL_METHOD_IBERNOULLI_ICDF
      PARAMETER (VSL_METHOD_IBERNOULLI_ICDF = 0)

! Geometric
!
! <Method>   <Short Description>
! ICDF       inverse cumulative distribution function method
      INTEGER VSL_METHOD_IGEOMETRIC_ICDF
      PARAMETER (VSL_METHOD_IGEOMETRIC_ICDF = 0)

! Binomial
!
! <Method>   <Short Description>
! BTPE       for ntrial*min(p,1-p)>30 acceptance/rejection method with
!            decomposition onto 4 regions:
!
!               * 2 parallelograms;
!               * triangle;
!               * left exponential tail;
!               * right exponential tail.
!
!            othewise table lookup method is used
      INTEGER VSL_METHOD_IBINOMIAL_BTPE
      PARAMETER (VSL_METHOD_IBINOMIAL_BTPE = 0)

! Hypergeometric
!
! <Method>   <Short Description>
! H2PE       if mode of distribution is large, acceptance/rejection method is
!            used with decomposition onto 3 regions:
!
!               * rectangular;
!               * left exponential tail;
!               * right exponential tail.
!
!            othewise table lookup method is used
      INTEGER VSL_METHOD_IHYPERGEOMETRIC_H2PE
      PARAMETER (VSL_METHOD_IHYPERGEOMETRIC_H2PE = 0)

! Poisson
!
! <Method>   <Short Description>
! PTPE       if lambda>=27, acceptance/rejection method is used with
!            decomposition onto 4 regions:
!
!               * 2 parallelograms;
!               * triangle;
!               * left exponential tail;
!               * right exponential tail.
!
!            othewise table lookup method is used
!
! POISNORM   for lambda>=1 method is based on Poisson inverse CDF
!            approximation by Gaussian inverse CDF; for lambda<1
!            table lookup method is used.
      INTEGER VSL_METHOD_IPOISSON_PTPE
      INTEGER VSL_METHOD_IPOISSON_POISNORM
      PARAMETER (VSL_METHOD_IPOISSON_PTPE     = 0)
      PARAMETER (VSL_METHOD_IPOISSON_POISNORM = 1)

! Poisson
!
! <Method>   <Short Description>
! POISNORM   for lambda>=1 method is based on Poisson inverse CDF
!            approximation by Gaussian inverse CDF; for lambda<1
!            ICDF method is used.
      INTEGER VSL_METHOD_IPOISSONV_POISNORM
      PARAMETER (VSL_METHOD_IPOISSONV_POISNORM = 0)

! Negbinomial
!
! <Method>   <Short Description>
! NBAR       if (a-1)*(1-p)/p>=100, acceptance/rejection method is used with
!            decomposition onto 5 regions:
!
!               * rectangular;
!               * 2 trapezoid;
!               * left exponential tail;
!               * right exponential tail.
!
!            othewise table lookup method is used.
      INTEGER VSL_METHOD_INEGBINOMIAL_NBAR
      PARAMETER (VSL_METHOD_INEGBINOMIAL_NBAR = 0)

!++
!  MATRIX STORAGE SCHEMES
!--

! Some multivariate random number generators, e.g. GaussianMV, operate
! with matrix parameters. To optimize matrix parameters usage VSL offers
! following matrix storage schemes. (See VSL documentation for more details).
!
! FULL     - whole matrix is stored
! PACKED   - lower/higher triangular matrix is packed in 1-dimensional array
! DIAGONAL - diagonal elements are packed in 1-dimensional array
      INTEGER*4 VSL_MATRIX_STORAGE_FULL
      INTEGER*4 VSL_MATRIX_STORAGE_PACKED
      INTEGER*4 VSL_MATRIX_STORAGE_DIAGONAL
      PARAMETER (VSL_MATRIX_STORAGE_FULL     = 0)
      PARAMETER (VSL_MATRIX_STORAGE_PACKED   = 1)
      PARAMETER (VSL_MATRIX_STORAGE_DIAGONAL = 2)


!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!==============================================================================
!------------------------------------------------------------------------------

      INTEGER vsldconvnewtask
      EXTERNAL vsldconvnewtask

      INTEGER vslsconvnewtask
      EXTERNAL vslsconvnewtask

      INTEGER vslzconvnewtask
      EXTERNAL vslzconvnewtask

      INTEGER vslcconvnewtask
      EXTERNAL vslcconvnewtask

      INTEGER vsldcorrnewtask
      EXTERNAL vsldcorrnewtask

      INTEGER vslscorrnewtask
      EXTERNAL vslscorrnewtask

      INTEGER vslzcorrnewtask
      EXTERNAL vslzcorrnewtask

      INTEGER vslccorrnewtask
      EXTERNAL vslccorrnewtask

      INTEGER vsldconvnewtask1d
      EXTERNAL vsldconvnewtask1d

      INTEGER vslsconvnewtask1d
      EXTERNAL vslsconvnewtask1d

      INTEGER vslzconvnewtask1d
      EXTERNAL vslzconvnewtask1d

      INTEGER vslcconvnewtask1d
      EXTERNAL vslcconvnewtask1d

      INTEGER vsldcorrnewtask1d
      EXTERNAL vsldcorrnewtask1d

      INTEGER vslscorrnewtask1d
      EXTERNAL vslscorrnewtask1d

      INTEGER vslzcorrnewtask1d
      EXTERNAL vslzcorrnewtask1d

      INTEGER vslccorrnewtask1d
      EXTERNAL vslccorrnewtask1d

      INTEGER vsldconvnewtaskx
      EXTERNAL vsldconvnewtaskx

      INTEGER vslsconvnewtaskx
      EXTERNAL vslsconvnewtaskx

      INTEGER vslzconvnewtaskx
      EXTERNAL vslzconvnewtaskx

      INTEGER vslcconvnewtaskx
      EXTERNAL vslcconvnewtaskx

      INTEGER vsldcorrnewtaskx
      EXTERNAL vsldcorrnewtaskx

      INTEGER vslscorrnewtaskx
      EXTERNAL vslscorrnewtaskx

      INTEGER vslzcorrnewtaskx
      EXTERNAL vslzcorrnewtaskx

      INTEGER vslccorrnewtaskx
      EXTERNAL vslccorrnewtaskx

      INTEGER vsldconvnewtaskx1d
      EXTERNAL vsldconvnewtaskx1d

      INTEGER vslsconvnewtaskx1d
      EXTERNAL vslsconvnewtaskx1d

      INTEGER vslzconvnewtaskx1d
      EXTERNAL vslzconvnewtaskx1d

      INTEGER vslcconvnewtaskx1d
      EXTERNAL vslcconvnewtaskx1d

      INTEGER vsldcorrnewtaskx1d
      EXTERNAL vsldcorrnewtaskx1d

      INTEGER vslscorrnewtaskx1d
      EXTERNAL vslscorrnewtaskx1d

      INTEGER vslzcorrnewtaskx1d
      EXTERNAL vslzcorrnewtaskx1d

      INTEGER vslccorrnewtaskx1d
      EXTERNAL vslccorrnewtaskx1d

      INTEGER vslconvdeletetask
      EXTERNAL vslconvdeletetask

      INTEGER vslcorrdeletetask
      EXTERNAL vslcorrdeletetask

      INTEGER vslconvcopytask
      EXTERNAL vslconvcopytask

      INTEGER vslcorrcopytask
      EXTERNAL vslcorrcopytask

      INTEGER vslConvSetMode
      EXTERNAL vslConvSetMode

      INTEGER vslCorrSetMode
      EXTERNAL vslCorrSetMode

      INTEGER vslConvSetInternalPrecision
      EXTERNAL vslConvSetInternalPrecision

      INTEGER vslCorrSetInternalPrecision
      EXTERNAL vslCorrSetInternalPrecision

      INTEGER vslConvSetStart
      EXTERNAL vslConvSetStart

      INTEGER vslCorrSetStart
      EXTERNAL vslCorrSetStart

      INTEGER vslConvSetDecimation
      EXTERNAL vslConvSetDecimation

      INTEGER vslCorrSetDecimation
      EXTERNAL vslCorrSetDecimation


      INTEGER vsldconvexec
      EXTERNAL vsldconvexec

      INTEGER vslsconvexec
      EXTERNAL vslsconvexec

      INTEGER vslzconvexec
      EXTERNAL vslzconvexec

      INTEGER vslcconvexec
      EXTERNAL vslcconvexec

      INTEGER vsldcorrexec
      EXTERNAL vsldcorrexec

      INTEGER vslscorrexec
      EXTERNAL vslscorrexec

      INTEGER vslzcorrexec
      EXTERNAL vslzcorrexec

      INTEGER vslccorrexec
      EXTERNAL vslccorrexec

      INTEGER vsldconvexec1d
      EXTERNAL vsldconvexec1d

      INTEGER vslsconvexec1d
      EXTERNAL vslsconvexec1d

      INTEGER vslzconvexec1d
      EXTERNAL vslzconvexec1d

      INTEGER vslcconvexec1d
      EXTERNAL vslcconvexec1d

      INTEGER vsldcorrexec1d
      EXTERNAL vsldcorrexec1d

      INTEGER vslscorrexec1d
      EXTERNAL vslscorrexec1d

      INTEGER vslzcorrexec1d
      EXTERNAL vslzcorrexec1d

      INTEGER vslccorrexec1d
      EXTERNAL vslccorrexec1d

      INTEGER vsldconvexecx
      EXTERNAL vsldconvexecx

      INTEGER vslsconvexecx
      EXTERNAL vslsconvexecx

      INTEGER vslzconvexecx
      EXTERNAL vslzconvexecx

      INTEGER vslcconvexecx
      EXTERNAL vslcconvexecx

      INTEGER vsldcorrexecx
      EXTERNAL vsldcorrexecx

      INTEGER vslscorrexecx
      EXTERNAL vslscorrexecx

      INTEGER vslzcorrexecx
      EXTERNAL vslzcorrexecx

      INTEGER vslccorrexecx
      EXTERNAL vslccorrexecx

      INTEGER vsldconvexecx1d
      EXTERNAL vsldconvexecx1d

      INTEGER vslsconvexecx1d
      EXTERNAL vslsconvexecx1d

      INTEGER vslzconvexecx1d
      EXTERNAL vslzconvexecx1d

      INTEGER vslcconvexecx1d
      EXTERNAL vslcconvexecx1d

      INTEGER vsldcorrexecx1d
      EXTERNAL vsldcorrexecx1d

      INTEGER vslscorrexecx1d
      EXTERNAL vslscorrexecx1d

      INTEGER vslzcorrexecx1d
      EXTERNAL vslzcorrexecx1d

      INTEGER vslccorrexecx1d
      EXTERNAL vslccorrexecx1d

!++
!  VSL CONTINUOUS DISTRIBUTION GENERATOR INTERFACES.
!--

!  Uniform distribution

      INTEGER vsrnguniform
      INTEGER vdrnguniform
      EXTERNAL vsrnguniform
      EXTERNAL vdrnguniform

!  Gaussian distribution

      INTEGER vsrnggaussian
      INTEGER vdrnggaussian
      EXTERNAL vsrnggaussian
      EXTERNAL vdrnggaussian

!  GaussianMV distribution

      INTEGER vsrnggaussianmv
      INTEGER vdrnggaussianmv
      EXTERNAL vsrnggaussianmv
      EXTERNAL vdrnggaussianmv

!  Exponential distribution

      INTEGER vsrngexponential
      INTEGER vdrngexponential
      EXTERNAL vsrngexponential
      EXTERNAL vdrngexponential

!  Laplace distribution

      INTEGER vsrnglaplace
      INTEGER vdrnglaplace
      EXTERNAL vsrnglaplace
      EXTERNAL vdrnglaplace

!  Weibull distribution

      INTEGER vsrngweibull
      INTEGER vdrngweibull
      EXTERNAL vsrngweibull
      EXTERNAL vdrngweibull

!  Cauchy distribution

      INTEGER vsrngcauchy
      INTEGER vdrngcauchy
      EXTERNAL vsrngcauchy
      EXTERNAL vdrngcauchy

!  Rayleigh distribution

      INTEGER vsrngrayleigh
      INTEGER vdrngrayleigh
      EXTERNAL vsrngrayleigh
      EXTERNAL vdrngrayleigh

!  Lognormal distribution

      INTEGER vsrnglognormal
      INTEGER vdrnglognormal
      EXTERNAL vsrnglognormal
      EXTERNAL vdrnglognormal

!  Gumbel distribution

      INTEGER vsrnggumbel
      INTEGER vdrnggumbel
      EXTERNAL vsrnggumbel
      EXTERNAL vdrnggumbel

!  Gamma distribution

      INTEGER vsrnggamma
      INTEGER vdrnggamma
      EXTERNAL vsrnggamma
      EXTERNAL vdrnggamma

!  Beta distribution

      INTEGER vsrngbeta
      INTEGER vdrngbeta
      EXTERNAL vsrngbeta
      EXTERNAL vdrngbeta

!++
!  VSL DISCRETE DISTRIBUTION GENERATOR INTERFACES.
!--

!  Uniform distribution

      INTEGER virnguniform
      EXTERNAL virnguniform

!  UniformBits distribution

      INTEGER virnguniformbits
      EXTERNAL virnguniformbits

!  Bernoulli distribution

      INTEGER virngbernoulli
      EXTERNAL virngbernoulli

!  Geometric distribution

      INTEGER virnggeometric
      EXTERNAL virnggeometric

!  Binomial distribution

      INTEGER virngbinomial
      EXTERNAL virngbinomial

!  Hypergeometric distribution

      INTEGER virnghypergeometric
      EXTERNAL virnghypergeometric

!  Poisson distribution

      INTEGER virngpoisson
      EXTERNAL virngpoisson

!  PoissonV distribution

      INTEGER virngpoissonv
      EXTERNAL virngpoissonv

!  Negbinomial distribution

      INTEGER virngnegbinomial
      EXTERNAL virngnegbinomial

!++
!  VSL SERVICE INTERFACES.
!--

! NewStream - stream creation/initialization

      INTEGER vslnewstream
      EXTERNAL vslnewstream

! NewStreamEx - advanced stream creation/initialization

      INTEGER vslnewstreamex
      EXTERNAL vslnewstreamex

!    INEWABSTRACTSTREAM

      INTEGER vslinewabstractstream
      EXTERNAL vslinewabstractstream

!    DNEWABSTRACTSTREAM

      INTEGER vsldnewabstractstream
      EXTERNAL vsldnewabstractstream

!    SNEWABSTRACTSTREAM

      INTEGER vslsnewabstractstream
      EXTERNAL vslsnewabstractstream

! DeleteStream - delete stream

      INTEGER vsldeletestream
      EXTERNAL vsldeletestream

! CopyStream - copy all stream information

      INTEGER vslcopystream
      EXTERNAL vslcopystream

! CopyStreamState - copy stream state only

      INTEGER vslcopystreamstate
      EXTERNAL vslcopystreamstate

! LeapfrogStream - leapfrog method

      INTEGER vslleapfrogstream
      EXTERNAL vslleapfrogstream

! SkipAheadStream - skip-ahead method

      INTEGER vslskipaheadstream
      EXTERNAL vslskipaheadstream

! GetBrngProperties - get BRNG properties

      INTEGER vslgetbrngproperties
      EXTERNAL vslgetbrngproperties

! GetNumRegBrngs - get number of registered BRNGs

      INTEGER vslgetnumregbrngs
      EXTERNAL vslgetnumregbrngs

! SaveStreamF - save stream to file

      INTEGER vslsavestreamf
      EXTERNAL vslsavestreamf

! LoadStreamF - save stream to file

      INTEGER vslloadstreamf
      EXTERNAL vslloadstreamf
