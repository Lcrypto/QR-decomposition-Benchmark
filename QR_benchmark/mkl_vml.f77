! file: mkl_vml.f77
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
!  Fortran 77 VML interface.
!--

!++
!  PARAMETER DEFINITIONS
!  Parameter definitions for VML mode and VML error status.
!
!  VML mode controls VML function accuracy, floating-point settings (rounding
!  mode and precision) and VML error handling options. Default VML mode is
!  VML_HA | VML_ERRMODE_DEFAULT, i.e. VML high accuracy functions are
!  called, and current floating-point precision and the rounding mode is used.
!
!  Error status macros are used for error classification.
!
!  NOTE: A | B means bitwise OR operation with A and B
!--

!  VML FUNCTION ACCURACY CONTROL
!  VML_HA - when VML_HA is set, high accuracy VML functions are called
!  VML_LA - when VML_LA is set, low accuracy VML functions are called
!  VML_EP - when VML_EP is set, enhanced performance VML functions are called
!
!  NOTE: VML_HA, VML_LA and VML_EP must not be used in combination
      INTEGER*4 VML_LA
      INTEGER*4 VML_HA
      INTEGER*4 VML_EP
      PARAMETER (VML_LA = Z"00000001")
      PARAMETER (VML_HA = Z"00000002")
      PARAMETER (VML_EP = Z"00000003")

!  SETTING OPTIMAL FLOATING-POINT PRECISION AND ROUNDING MODE
!  Definitions below are to set optimal floating-point control word
!  (precision and rounding mode).
!
!  For their correct work, VML functions change floating-point precision and
!  rounding mode (if necessary). Since control word changing is typically
!  expensive operation, it is recommended to set precision and rounding mode
!  to optimal values before VML function calls.
!
!  VML_FLOAT_CONSISTENT  - use this value if the calls are typically to single
!                          precision VML functions
!  VML_DOUBLE_CONSISTENT - use this value if the calls are typically to double
!                          precision VML functions
!  VML_RESTORE           - restore original floating-point precision and
!                          rounding mode
!  VML_DEFAULT_PRECISION - use default (current) floating-point precision and
!                          rounding mode
!  NOTE: VML_FLOAT_CONSISTENT, VML_DOUBLE_CONSISTENT, VML_RESTORE and
!        VML_DEFAULT_PRECISION must not be used in combination
      INTEGER*4 VML_DEFAULT_PRECISION
      INTEGER*4 VML_FLOAT_CONSISTENT
      INTEGER*4 VML_DOUBLE_CONSISTENT
      INTEGER*4 VML_RESTORE
      PARAMETER (VML_DEFAULT_PRECISION = Z"00000000")
      PARAMETER (VML_FLOAT_CONSISTENT  = Z"00000010")
      PARAMETER (VML_DOUBLE_CONSISTENT = Z"00000020")
      PARAMETER (VML_RESTORE           = Z"00000030")

!  VML ERROR HANDLING CONTROL
!  Macros below are used to control VML error handler.
!
!  VML_ERRMODE_IGNORE   - ignore errors
!  VML_ERRMODE_ERRNO    - errno variable is set on error
!  VML_ERRMODE_STDERR   - error description text is written to stderr on error
!  VML_ERRMODE_EXCEPT   - exception is raised on error
!  VML_ERRMODE_CALLBACK - user's error handler function is called on error
!  VML_ERRMODE_DEFAULT  - errno variable is set, exceptions are raised and
!                         user's error handler is called on error
!  NOTE: VML_ERRMODE_IGNORE must not be used in combination with
!        VML_ERRMODE_ERRNO, VML_ERRMODE_STDERR, VML_ERRMODE_EXCEPT,
!        VML_ERRMODE_CALLBACK and VML_ERRMODE_DEFAULT.
      INTEGER*4 VML_ERRMODE_IGNORE
      INTEGER*4 VML_ERRMODE_ERRNO
      INTEGER*4 VML_ERRMODE_STDERR
      INTEGER*4 VML_ERRMODE_EXCEPT
      INTEGER*4 VML_ERRMODE_CALLBACK
      INTEGER*4 VML_ERRMODE_DEFAULT
      PARAMETER (VML_ERRMODE_IGNORE   = Z"00000100")
      PARAMETER (VML_ERRMODE_ERRNO    = Z"00000200")
      PARAMETER (VML_ERRMODE_STDERR   = Z"00000400")
      PARAMETER (VML_ERRMODE_EXCEPT   = Z"00000800")
      PARAMETER (VML_ERRMODE_CALLBACK = Z"00001000")
      PARAMETER (VML_ERRMODE_DEFAULT  = Z"00001A00")

!  ACCURACY, FLOATING-POINT CONTROL AND ERROR HANDLING MASKS
!  Accuracy, floating-point and error handling control are packed in
!  the VML mode variable. Macros below are useful to extract accuracy and/or
!  floating-point control and/or error handling control settings.
!
!  VML_ACCURACY_MASK           - extract accuracy bits
!  VML_FPUMODE_MASK            - extract floating-point control bits
!  VML_ERRMODE_MASK            - extract error handling control bits
!                                (including error callback bits)
!  VML_ERRMODE_STDHANDLER_MASK - extract error handling control bits
!                                (not including error callback bits)
!  VML_ERRMODE_CALLBACK_MASK   - extract error callback bits
      INTEGER*4 VML_ACCURACY_MASK
      INTEGER*4 VML_FPUMODE_MASK
      INTEGER*4 VML_ERRMODE_MASK
      INTEGER*4 VML_ERRMODE_STDHANDLER_MASK
      INTEGER*4 VML_ERRMODE_CALLBACK_MASK
      PARAMETER (VML_ACCURACY_MASK = Z"0000000f")
      PARAMETER (VML_FPUMODE_MASK  = Z"000000f0")
      PARAMETER (VML_ERRMODE_MASK  = Z"0000ff00")
      PARAMETER (VML_ERRMODE_STDHANDLER_MASK = Z"00000f00")
      PARAMETER (VML_ERRMODE_CALLBACK_MASK = Z"0000f000")

!  ERROR STATUS PARAMETER DEFINITIONS
!  VML_STATUS_OK        - no errors
!  VML_STATUS_BADSIZE   - array dimension is not positive
!  VML_STATUS_BADMEM    - invalid pointer passed
!  VML_STATUS_ERRDOM    - at least one of arguments is out of function domain
!  VML_STATUS_SING      - at least one of arguments caused singularity
!  VML_STATUS_OVERFLOW  - at least one of arguments caused overflow
!  VML_STATUS_UNDERFLOW - at least one of arguments caused underflow
      INTEGER*4 VML_STATUS_OK
      INTEGER*4 VML_STATUS_BADSIZE
      INTEGER*4 VML_STATUS_BADMEM
      INTEGER*4 VML_STATUS_ERRDOM
      INTEGER*4 VML_STATUS_SING
      INTEGER*4 VML_STATUS_OVERFLOW
      INTEGER*4 VML_STATUS_UNDERFLOW
      PARAMETER (VML_STATUS_OK        = 0)
      PARAMETER (VML_STATUS_BADSIZE   = -1)
      PARAMETER (VML_STATUS_BADMEM    = -2)
      PARAMETER (VML_STATUS_ERRDOM    = 1)
      PARAMETER (VML_STATUS_SING      = 2)
      PARAMETER (VML_STATUS_OVERFLOW  = 3)
      PARAMETER (VML_STATUS_UNDERFLOW = 4)

!++
!  VML ELEMENTARY FUNCTION INTERFACES.
!--

!  Absolute value: r[i] = |a[i]|

      EXTERNAL vsabs
      EXTERNAL vdabs

!  Complex absolute value: r[i] = |a[i]|

      EXTERNAL vcabs
      EXTERNAL vzabs

!  Argument of complex value: r[i] = carg(a[i])

      EXTERNAL vcarg
      EXTERNAL vzarg

!  Addition: r[i] = a[i] + b[i]

      EXTERNAL vsadd
      EXTERNAL vdadd

!  Complex addition: r[i] = a[i] + b[i]

      EXTERNAL vcadd
      EXTERNAL vzadd

!  Subtraction: r[i] = a[i] - b[i]

      EXTERNAL vssub
      EXTERNAL vdsub

!  Complex subtraction: r[i] = a[i] - b[i]

      EXTERNAL vcsub
      EXTERNAL vzsub

!  Reciprocal: r[i] = 1.0 / a[i]

      EXTERNAL vsinv
      EXTERNAL vdinv

!  Square Root: r[i] = a[i]^0.5

      EXTERNAL vssqrt
      EXTERNAL vdsqrt

!  Complex Square Root: r[i] = a[i]^0.5

      EXTERNAL vcsqrt
      EXTERNAL vzsqrt

!  Reciprocal Square Root: r[i] = 1/a[i]^0.5

      EXTERNAL vsinvsqrt
      EXTERNAL vdinvsqrt

!  Cube Root: r[i] = a[i]^(1/3)

      EXTERNAL vscbrt
      EXTERNAL vdcbrt

!  Reciprocal Cube Root: r[i] = 1/a[i]^(1/3)

      EXTERNAL vsinvcbrt
      EXTERNAL vdinvcbrt

!  Squaring: r[i] = a[i]^2

      EXTERNAL vssqr
      EXTERNAL vdsqr

!  Exponential Function: r[i] = e^a[i]

      EXTERNAL vsexp
      EXTERNAL vdexp

!  Complex Exponential Function: r[i] = e^a[i]

      EXTERNAL vcexp
      EXTERNAL vzexp

!  : r[i] = e^(a[i]-1)

      EXTERNAL vsexpm1
      EXTERNAL vdexpm1

!  Logarithm (base e): r[i] = ln(a[i])

      EXTERNAL vsln
      EXTERNAL vdln

!  Complex Logarithm (base e): r[i] = ln(a[i])

      EXTERNAL vcln
      EXTERNAL vzln

!  Logarithm (base 10): r[i] = lg(a[i])

      EXTERNAL vslog10
      EXTERNAL vdlog10

!  : r[i] = log(1+a[i])

      EXTERNAL vslog1p
      EXTERNAL vdlog1p

!  Complex Logarithm (base 10): r[i] = lg(a[i])

      EXTERNAL vclog10
      EXTERNAL vzlog10

!  Sine: r[i] = sin(a[i])

      EXTERNAL vssin
      EXTERNAL vdsin

!  Complex Sine: r[i] = sin(a[i])

      EXTERNAL vcsin
      EXTERNAL vzsin

!  Cosine: r[i] = cos(a[i])

      EXTERNAL vscos
      EXTERNAL vdcos

!  Complex Cosine: r[i] = cos(a[i])

      EXTERNAL vccos
      EXTERNAL vzcos

!  Tangent: r[i] = tan(a[i])

      EXTERNAL vstan
      EXTERNAL vdtan

!  Complex Tangent: r[i] = tan(a[i])

      EXTERNAL vctan
      EXTERNAL vztan

!  Hyperbolic Sine: r[i] = sh(a[i])

      EXTERNAL vssinh
      EXTERNAL vdsinh

!  Complex Hyperbolic Sine: r[i] = sh(a[i])

      EXTERNAL vcsinh
      EXTERNAL vzsinh

!  Hyperbolic Cosine: r[i] = ch(a[i])

      EXTERNAL vscosh
      EXTERNAL vdcosh

!  Complex Hyperbolic Cosine: r[i] = ch(a[i])

      EXTERNAL vccosh
      EXTERNAL vzcosh

!  Hyperbolic Tangent: r[i] = th(a[i])

      EXTERNAL vstanh
      EXTERNAL vdtanh

!  Complex Hyperbolic Tangent: r[i] = th(a[i])

      EXTERNAL vctanh
      EXTERNAL vztanh

!  Arc Cosine: r[i] = arccos(a[i])

      EXTERNAL vsacos
      EXTERNAL vdacos

!  Complex Arc Cosine: r[i] = arccos(a[i])

      EXTERNAL vcacos
      EXTERNAL vzacos

!  Arc Sine: r[i] = arcsin(a[i])

      EXTERNAL vsasin
      EXTERNAL vdasin

!  Complex Arc Sine: r[i] = arcsin(a[i])

      EXTERNAL vcasin
      EXTERNAL vzasin

!  Arc Tangent: r[i] = arctan(a[i])

      EXTERNAL vsatan
      EXTERNAL vdatan

!  Complex Arc Tangent: r[i] = arctan(a[i])

      EXTERNAL vcatan
      EXTERNAL vzatan

!  Hyperbolic Arc Cosine: r[i] = arcch(a[i])

      EXTERNAL vsacosh
      EXTERNAL vdacosh

!  Complex Hyperbolic Arc Cosine: r[i] = arcch(a[i])

      EXTERNAL vcacosh
      EXTERNAL vzacosh

!  Hyperbolic Arc Sine: r[i] = arcsh(a[i])

      EXTERNAL vsasinh
      EXTERNAL vdasinh

!  Complex Hyperbolic Arc Sine: r[i] = arcsh(a[i])

      EXTERNAL vcasinh
      EXTERNAL vzasinh

!  Hyperbolic Arc Tangent: r[i] = arcth(a[i])

      EXTERNAL vsatanh
      EXTERNAL vdatanh

!  Complex Hyperbolic Arc Tangent: r[i] = arcth(a[i])

      EXTERNAL vcatanh
      EXTERNAL vzatanh

!  Error Function: r[i] = erf(a[i])

      EXTERNAL vserf
      EXTERNAL vderf

!

      EXTERNAL vserfinv
      EXTERNAL vderfinv

!

      EXTERNAL vshypot
      EXTERNAL vdhypot

!  Complementary Error Function: r[i] = 1 - erf(a[i])

      EXTERNAL vserfc
      EXTERNAL vderfc

!

      EXTERNAL vserfcinv
      EXTERNAL vderfcinv

!

      EXTERNAL vscdfnorm
      EXTERNAL vdcdfnorm

!

      EXTERNAL vscdfnorminv
      EXTERNAL vdcdfnorminv

!  Arc Tangent of a/b: r[i] = arctan(a[i]/b[i])

      EXTERNAL vsatan2
      EXTERNAL vdatan2

!  Division: r[i] = a[i] * b[i]

      EXTERNAL vsmul
      EXTERNAL vdmul

!  Complex division: r[i] = a[i] * b[i]

      EXTERNAL vcmul
      EXTERNAL vzmul

!  Division: r[i] = a[i] / b[i]

      EXTERNAL vsdiv
      EXTERNAL vddiv

!  Complex division: r[i] = a[i] / b[i]

      EXTERNAL vcdiv
      EXTERNAL vzdiv

!  Power Function: r[i] = a[i]^b[i]

      EXTERNAL vspow
      EXTERNAL vdpow

!  Complex Power Function: r[i] = a[i]^b[i]

      EXTERNAL vcpow
      EXTERNAL vzpow

!  Power Function: r[i] = a[i]^(3/2)

      EXTERNAL vspow3o2
      EXTERNAL vdpow3o2

!  Power Function: r[i] = a[i]^(2/3)

      EXTERNAL vspow2o3
      EXTERNAL vdpow2o3

!  Power Function with Fixed Degree: r[i] = a[i]^b

      EXTERNAL vspowx
      EXTERNAL vdpowx

!  Complex Power Function with Fixed Degree: r[i] = a[i]^b

      EXTERNAL vcpowx
      EXTERNAL vzpowx

!  Sine & Cosine: r1[i] = sin(a[i]), r2[i]=cos(a[i])

      EXTERNAL vssincos
      EXTERNAL vdsincos

!

      EXTERNAL vsceil
      EXTERNAL vdceil

!

      EXTERNAL vsfloor
      EXTERNAL vdfloor

!

      EXTERNAL vsmodf
      EXTERNAL vdmodf

!

      EXTERNAL vsnearbyint
      EXTERNAL vdnearbyint

!

      EXTERNAL vsrint
      EXTERNAL vdrint

!

      EXTERNAL vsround
      EXTERNAL vdround

!

      EXTERNAL vstrunc
      EXTERNAL vdtrunc

!  : r[i] =

      EXTERNAL vcconj
      EXTERNAL vzconj

!  : r[i] =

      EXTERNAL vcmulbyconj
      EXTERNAL vzmulbyconj

!  : r[i] =

      EXTERNAL vccis
      EXTERNAL vzcis

!++
!  VML PACK FUNCTION INTERFACES.
!--

!  Positive Increment Indexing

      EXTERNAL vspacki
      EXTERNAL vdpacki

!  Index Vector Indexing

      EXTERNAL vspackv
      EXTERNAL vdpackv

!  Mask Vector Indexing

      EXTERNAL vspackm
      EXTERNAL vdpackm

!++
!  VML UNPACK FUNCTION DECLARATIONS.
!--

!  Positive Increment Indexing

      EXTERNAL vsunpacki
      EXTERNAL vdunpacki

!  Index Vector Indexing

      EXTERNAL vsunpackv
      EXTERNAL vdunpackv

!  Mask Vector Indexing

      EXTERNAL vsunpackm
      EXTERNAL vdunpackm

!++
!  VML ERROR HANDLING FUNCTION DECLARATIONS.
!--

!  Set VML Error Status

      INTEGER*4 vmlseterrstatus
      EXTERNAL vmlseterrstatus

!  Get VML Error Status

      INTEGER*4 vmlgeterrstatus
      EXTERNAL vmlgeterrstatus

!  Clear VML Error Status

      INTEGER*4 vmlclearerrstatus
      EXTERNAL vmlclearerrstatus


!++
!  VML MODE FUNCTION DECLARATIONS.
!--

!  Set VML Mode

      INTEGER*4 vmlsetmode
      EXTERNAL vmlsetmode

!  Get VML Mode

      INTEGER*4 vmlgetmode
      EXTERNAL vmlgetmode
