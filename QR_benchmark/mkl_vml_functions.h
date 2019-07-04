/* file: mkl_vml_functions.h */
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
//  User-level VML function declarations
//--
*/

#ifndef __MKL_VML_FUNCTIONS_H__
#define __MKL_VML_FUNCTIONS_H__

#include "mkl_vml_types.h"

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/*
//++
//  EXTERNAL API MACROS.
//  Used to construct VML function declaration. Change them if you are going to
//  provide different API for VML functions.
//--
*/
#if defined(MKL_VML_STDCALL)
  #define _Vml_Api(rtype,name,arg) extern rtype __stdcall name    arg;
  #define _vml_api(rtype,name,arg) extern rtype __stdcall name    arg;
  #define _VML_API(rtype,name,arg) extern rtype __stdcall name    arg;
#else /* MKL_VML_CDECL */
  #define _Vml_Api(rtype,name,arg) extern rtype __cdecl   name    arg;
  #define _vml_api(rtype,name,arg) extern rtype __cdecl   name    arg;
  #define _VML_API(rtype,name,arg) extern rtype __cdecl   name    arg;
#endif

/*
//++
//  VML ELEMENTARY FUNCTION DECLARATIONS.
//--
*/
/* Absolute value: r[i] = |a[i]| */
_VML_API(void,VSABS,(const MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDABS,(const MKL_INT *n, const double a[], double r[]))
_vml_api(void,vsabs,(const MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdabs,(const MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsAbs,(const MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdAbs,(const MKL_INT n,  const double a[], double r[]))

/* Complex absolute value: r[i] = |a[i]| */
_VML_API(void,VCABS,(const MKL_INT *n, const MKL_Complex8  a[], float  r[]))
_VML_API(void,VZABS,(const MKL_INT *n, const MKL_Complex16 a[], double r[]))
_vml_api(void,vcabs,(const MKL_INT *n, const MKL_Complex8  a[], float  r[]))
_vml_api(void,vzabs,(const MKL_INT *n, const MKL_Complex16 a[], double r[]))
_Vml_Api(void,vcAbs,(const MKL_INT n,  const MKL_Complex8  a[], float  r[]))
_Vml_Api(void,vzAbs,(const MKL_INT n,  const MKL_Complex16 a[], double r[]))

/* Argument of complex value: r[i] = carg(a[i]) */
_VML_API(void,VCARG,(const MKL_INT *n, const MKL_Complex8  a[], float  r[]))
_VML_API(void,VZARG,(const MKL_INT *n, const MKL_Complex16 a[], double r[]))
_vml_api(void,vcarg,(const MKL_INT *n, const MKL_Complex8  a[], float  r[]))
_vml_api(void,vzarg,(const MKL_INT *n, const MKL_Complex16 a[], double r[]))
_Vml_Api(void,vcArg,(const MKL_INT n,  const MKL_Complex8  a[], float  r[]))
_Vml_Api(void,vzArg,(const MKL_INT n,  const MKL_Complex16 a[], double r[]))

/* Addition: r[i] = a[i] + b[i] */
_VML_API(void,VSADD,(const MKL_INT *n, const float  a[], const float  b[], float  r[]))
_VML_API(void,VDADD,(const MKL_INT *n, const double a[], const double b[], double r[]))
_vml_api(void,vsadd,(const MKL_INT *n, const float  a[], const float  b[], float  r[]))
_vml_api(void,vdadd,(const MKL_INT *n, const double a[], const double b[], double r[]))
_Vml_Api(void,vsAdd,(const MKL_INT n,  const float  a[], const float  b[], float  r[]))
_Vml_Api(void,vdAdd,(const MKL_INT n,  const double a[], const double b[], double r[]))

/* Complex addition: r[i] = a[i] + b[i] */
_VML_API(void,VCADD,(const MKL_INT *n, const MKL_Complex8  a[], const MKL_Complex8  b[], MKL_Complex8  r[]))
_VML_API(void,VZADD,(const MKL_INT *n, const MKL_Complex16 a[], const MKL_Complex16 b[], MKL_Complex16 r[]))
_vml_api(void,vcadd,(const MKL_INT *n, const MKL_Complex8  a[], const MKL_Complex8  b[], MKL_Complex8  r[]))
_vml_api(void,vzadd,(const MKL_INT *n, const MKL_Complex16 a[], const MKL_Complex16 b[], MKL_Complex16 r[]))
_Vml_Api(void,vcAdd,(const MKL_INT n,  const MKL_Complex8  a[], const MKL_Complex8  b[], MKL_Complex8  r[]))
_Vml_Api(void,vzAdd,(const MKL_INT n,  const MKL_Complex16 a[], const MKL_Complex16 b[], MKL_Complex16 r[]))

/* Subtraction: r[i] = a[i] - b[i] */
_VML_API(void,VSSUB,(const MKL_INT *n, const float  a[], const float  b[], float  r[]))
_VML_API(void,VDSUB,(const MKL_INT *n, const double a[], const double b[], double r[]))
_vml_api(void,vssub,(const MKL_INT *n, const float  a[], const float  b[], float  r[]))
_vml_api(void,vdsub,(const MKL_INT *n, const double a[], const double b[], double r[]))
_Vml_Api(void,vsSub,(const MKL_INT n,  const float  a[], const float  b[], float  r[]))
_Vml_Api(void,vdSub,(const MKL_INT n,  const double a[], const double b[], double r[]))

/* Complex subtraction: r[i] = a[i] - b[i] */
_VML_API(void,VCSUB,(const MKL_INT *n, const MKL_Complex8  a[], const MKL_Complex8  b[], MKL_Complex8  r[]))
_VML_API(void,VZSUB,(const MKL_INT *n, const MKL_Complex16 a[], const MKL_Complex16 b[], MKL_Complex16 r[]))
_vml_api(void,vcsub,(const MKL_INT *n, const MKL_Complex8  a[], const MKL_Complex8  b[], MKL_Complex8  r[]))
_vml_api(void,vzsub,(const MKL_INT *n, const MKL_Complex16 a[], const MKL_Complex16 b[], MKL_Complex16 r[]))
_Vml_Api(void,vcSub,(const MKL_INT n,  const MKL_Complex8  a[], const MKL_Complex8  b[], MKL_Complex8  r[]))
_Vml_Api(void,vzSub,(const MKL_INT n,  const MKL_Complex16 a[], const MKL_Complex16 b[], MKL_Complex16 r[]))

/* Reciprocal: r[i] = 1.0 / a[i] */
_VML_API(void,VSINV,(const MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDINV,(const MKL_INT *n, const double a[], double r[]))
_vml_api(void,vsinv,(const MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdinv,(const MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsInv,(const MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdInv,(const MKL_INT n,  const double a[], double r[]))

/* Square Root: r[i] = a[i]^0.5 */
_VML_API(void,VSSQRT,(const MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDSQRT,(const MKL_INT *n, const double a[], double r[]))
_vml_api(void,vssqrt,(const MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdsqrt,(const MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsSqrt,(const MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdSqrt,(const MKL_INT n,  const double a[], double r[]))

/* Complex Square Root: r[i] = a[i]^0.5 */
_VML_API(void,VCSQRT,(const MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_VML_API(void,VZSQRT,(const MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_vml_api(void,vcsqrt,(const MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_vml_api(void,vzsqrt,(const MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_Vml_Api(void,vcSqrt,(const MKL_INT n,  const MKL_Complex8  a[], MKL_Complex8  r[]))
_Vml_Api(void,vzSqrt,(const MKL_INT n,  const MKL_Complex16 a[], MKL_Complex16 r[]))

/* Reciprocal Square Root: r[i] = 1/a[i]^0.5 */
_VML_API(void,VSINVSQRT,(const MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDINVSQRT,(const MKL_INT *n, const double a[], double r[]))
_vml_api(void,vsinvsqrt,(const MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdinvsqrt,(const MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsInvSqrt,(const MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdInvSqrt,(const MKL_INT n,  const double a[], double r[]))

/* Cube Root: r[i] = a[i]^(1/3) */
_VML_API(void,VSCBRT,(const MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDCBRT,(const MKL_INT *n, const double a[], double r[]))
_vml_api(void,vscbrt,(const MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdcbrt,(const MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsCbrt,(const MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdCbrt,(const MKL_INT n,  const double a[], double r[]))

/* Reciprocal Cube Root: r[i] = 1/a[i]^(1/3) */
_VML_API(void,VSINVCBRT,(const MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDINVCBRT,(const MKL_INT *n, const double a[], double r[]))
_vml_api(void,vsinvcbrt,(const MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdinvcbrt,(const MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsInvCbrt,(const MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdInvCbrt,(const MKL_INT n,  const double a[], double r[]))

/* Squaring: r[i] = a[i]^2 */
_VML_API(void,VSSQR,(const MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDSQR,(const MKL_INT *n, const double a[], double r[]))
_vml_api(void,vssqr,(const MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdsqr,(const MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsSqr,(const MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdSqr,(const MKL_INT n,  const double a[], double r[]))

/* Exponential Function: r[i] = e^a[i] */
_VML_API(void,VSEXP,(const MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDEXP,(const MKL_INT *n, const double a[], double r[]))
_vml_api(void,vsexp,(const MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdexp,(const MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsExp,(const MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdExp,(const MKL_INT n,  const double a[], double r[]))

/* : r[i] = e^(a[i]-1) */
_VML_API(void,VSEXPM1,(const MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDEXPM1,(const MKL_INT *n, const double a[], double r[]))
_vml_api(void,vsexpm1,(const MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdexpm1,(const MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsExpm1,(const MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdExpm1,(const MKL_INT n,  const double a[], double r[]))

/* Complex Exponential Function: r[i] = e^a[i] */
_VML_API(void,VCEXP,(const MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_VML_API(void,VZEXP,(const MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_vml_api(void,vcexp,(const MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_vml_api(void,vzexp,(const MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_Vml_Api(void,vcExp,(const MKL_INT n,  const MKL_Complex8  a[], MKL_Complex8  r[]))
_Vml_Api(void,vzExp,(const MKL_INT n,  const MKL_Complex16 a[], MKL_Complex16 r[]))

/* Logarithm (base e): r[i] = ln(a[i]) */
_VML_API(void,VSLN,(const MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDLN,(const MKL_INT *n, const double a[], double r[]))
_vml_api(void,vsln,(const MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdln,(const MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsLn,(const MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdLn,(const MKL_INT n,  const double a[], double r[]))

/* Complex Logarithm (base e): r[i] = ln(a[i]) */
_VML_API(void,VCLN,(const MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_VML_API(void,VZLN,(const MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_vml_api(void,vcln,(const MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_vml_api(void,vzln,(const MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_Vml_Api(void,vcLn,(const MKL_INT n,  const MKL_Complex8  a[], MKL_Complex8  r[]))
_Vml_Api(void,vzLn,(const MKL_INT n,  const MKL_Complex16 a[], MKL_Complex16 r[]))

/* Logarithm (base 10): r[i] = lg(a[i]) */
_VML_API(void,VSLOG10,(const MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDLOG10,(const MKL_INT *n, const double a[], double r[]))
_vml_api(void,vslog10,(const MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdlog10,(const MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsLog10,(const MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdLog10,(const MKL_INT n,  const double a[], double r[]))

/* Complex Logarithm (base 10): r[i] = lg(a[i]) */
_VML_API(void,VCLOG10,(const MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_VML_API(void,VZLOG10,(const MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_vml_api(void,vclog10,(const MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_vml_api(void,vzlog10,(const MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_Vml_Api(void,vcLog10,(const MKL_INT n,  const MKL_Complex8  a[], MKL_Complex8  r[]))
_Vml_Api(void,vzLog10,(const MKL_INT n,  const MKL_Complex16 a[], MKL_Complex16 r[]))

/* : r[i] = log(1+a[i]) */
_VML_API(void,VSLOG1P,(const MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDLOG1P,(const MKL_INT *n, const double a[], double r[]))
_vml_api(void,vslog1p,(const MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdlog1p,(const MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsLog1p,(const MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdLog1p,(const MKL_INT n,  const double a[], double r[]))

/* Cosine: r[i] = cos(a[i]) */
_VML_API(void,VSCOS,(const MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDCOS,(const MKL_INT *n, const double a[], double r[]))
_vml_api(void,vscos,(const MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdcos,(const MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsCos,(const MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdCos,(const MKL_INT n,  const double a[], double r[]))

/* Complex Cosine: r[i] = ccos(a[i]) */
_VML_API(void,VCCOS,(const MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_VML_API(void,VZCOS,(const MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_vml_api(void,vccos,(const MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_vml_api(void,vzcos,(const MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_Vml_Api(void,vcCos,(const MKL_INT n,  const MKL_Complex8  a[], MKL_Complex8  r[]))
_Vml_Api(void,vzCos,(const MKL_INT n,  const MKL_Complex16 a[], MKL_Complex16 r[]))

/* Sine: r[i] = sin(a[i]) */
_VML_API(void,VSSIN,(const MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDSIN,(const MKL_INT *n, const double a[], double r[]))
_vml_api(void,vssin,(const MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdsin,(const MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsSin,(const MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdSin,(const MKL_INT n,  const double a[], double r[]))

/* Complex Sine: r[i] = sin(a[i]) */
_VML_API(void,VCSIN,(const MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_VML_API(void,VZSIN,(const MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_vml_api(void,vcsin,(const MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_vml_api(void,vzsin,(const MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_Vml_Api(void,vcSin,(const MKL_INT n,  const MKL_Complex8  a[], MKL_Complex8  r[]))
_Vml_Api(void,vzSin,(const MKL_INT n,  const MKL_Complex16 a[], MKL_Complex16 r[]))

/* Tangent: r[i] = tan(a[i]) */
_VML_API(void,VSTAN,(const MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDTAN,(const MKL_INT *n, const double a[], double r[]))
_vml_api(void,vstan,(const MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdtan,(const MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsTan,(const MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdTan,(const MKL_INT n,  const double a[], double r[]))

/* Complex Tangent: r[i] = tan(a[i]) */
_VML_API(void,VCTAN,(const MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_VML_API(void,VZTAN,(const MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_vml_api(void,vctan,(const MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_vml_api(void,vztan,(const MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_Vml_Api(void,vcTan,(const MKL_INT n,  const MKL_Complex8  a[], MKL_Complex8  r[]))
_Vml_Api(void,vzTan,(const MKL_INT n,  const MKL_Complex16 a[], MKL_Complex16 r[]))

/* Hyperbolic Cosine: r[i] = ch(a[i]) */
_VML_API(void,VSCOSH,(const MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDCOSH,(const MKL_INT *n, const double a[], double r[]))
_vml_api(void,vscosh,(const MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdcosh,(const MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsCosh,(const MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdCosh,(const MKL_INT n,  const double a[], double r[]))

/* Complex Hyperbolic Cosine: r[i] = ch(a[i]) */
_VML_API(void,VCCOSH,(const MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_VML_API(void,VZCOSH,(const MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_vml_api(void,vccosh,(const MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_vml_api(void,vzcosh,(const MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_Vml_Api(void,vcCosh,(const MKL_INT n,  const MKL_Complex8  a[], MKL_Complex8  r[]))
_Vml_Api(void,vzCosh,(const MKL_INT n,  const MKL_Complex16 a[], MKL_Complex16 r[]))

/* Hyperbolic Sine: r[i] = sh(a[i]) */
_VML_API(void,VSSINH,(const MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDSINH,(const MKL_INT *n, const double a[], double r[]))
_vml_api(void,vssinh,(const MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdsinh,(const MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsSinh,(const MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdSinh,(const MKL_INT n,  const double a[], double r[]))

/* Complex Hyperbolic Sine: r[i] = sh(a[i]) */
_VML_API(void,VCSINH,(const MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_VML_API(void,VZSINH,(const MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_vml_api(void,vcsinh,(const MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_vml_api(void,vzsinh,(const MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_Vml_Api(void,vcSinh,(const MKL_INT n,  const MKL_Complex8  a[], MKL_Complex8  r[]))
_Vml_Api(void,vzSinh,(const MKL_INT n,  const MKL_Complex16 a[], MKL_Complex16 r[]))

/* Hyperbolic Tangent: r[i] = th(a[i]) */
_VML_API(void,VSTANH,(const MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDTANH,(const MKL_INT *n, const double a[], double r[]))
_vml_api(void,vstanh,(const MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdtanh,(const MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsTanh,(const MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdTanh,(const MKL_INT n,  const double a[], double r[]))

/* Complex Hyperbolic Tangent: r[i] = th(a[i]) */
_VML_API(void,VCTANH,(const MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_VML_API(void,VZTANH,(const MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_vml_api(void,vctanh,(const MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_vml_api(void,vztanh,(const MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_Vml_Api(void,vcTanh,(const MKL_INT n,  const MKL_Complex8  a[], MKL_Complex8  r[]))
_Vml_Api(void,vzTanh,(const MKL_INT n,  const MKL_Complex16 a[], MKL_Complex16 r[]))

/* Arc Cosine: r[i] = arccos(a[i]) */
_VML_API(void,VSACOS,(const MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDACOS,(const MKL_INT *n, const double a[], double r[]))
_vml_api(void,vsacos,(const MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdacos,(const MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsAcos,(const MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdAcos,(const MKL_INT n,  const double a[], double r[]))

/* Complex Arc Cosine: r[i] = arccos(a[i]) */
_VML_API(void,VCACOS,(const MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_VML_API(void,VZACOS,(const MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_vml_api(void,vcacos,(const MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_vml_api(void,vzacos,(const MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_Vml_Api(void,vcAcos,(const MKL_INT n,  const MKL_Complex8  a[], MKL_Complex8  r[]))
_Vml_Api(void,vzAcos,(const MKL_INT n,  const MKL_Complex16 a[], MKL_Complex16 r[]))

/* Arc Sine: r[i] = arcsin(a[i]) */
_VML_API(void,VSASIN,(const MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDASIN,(const MKL_INT *n, const double a[], double r[]))
_vml_api(void,vsasin,(const MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdasin,(const MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsAsin,(const MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdAsin,(const MKL_INT n,  const double a[], double r[]))

/* Complex Arc Sine: r[i] = arcsin(a[i]) */
_VML_API(void,VCASIN,(const MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_VML_API(void,VZASIN,(const MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_vml_api(void,vcasin,(const MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_vml_api(void,vzasin,(const MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_Vml_Api(void,vcAsin,(const MKL_INT n,  const MKL_Complex8  a[], MKL_Complex8  r[]))
_Vml_Api(void,vzAsin,(const MKL_INT n,  const MKL_Complex16 a[], MKL_Complex16 r[]))

/* Arc Tangent: r[i] = arctan(a[i]) */
_VML_API(void,VSATAN,(const MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDATAN,(const MKL_INT *n, const double a[], double r[]))
_vml_api(void,vsatan,(const MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdatan,(const MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsAtan,(const MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdAtan,(const MKL_INT n,  const double a[], double r[]))

/* Complex Arc Tangent: r[i] = arctan(a[i]) */
_VML_API(void,VCATAN,(const MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_VML_API(void,VZATAN,(const MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_vml_api(void,vcatan,(const MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_vml_api(void,vzatan,(const MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_Vml_Api(void,vcAtan,(const MKL_INT n,  const MKL_Complex8  a[], MKL_Complex8  r[]))
_Vml_Api(void,vzAtan,(const MKL_INT n,  const MKL_Complex16 a[], MKL_Complex16 r[]))

/* Hyperbolic Arc Cosine: r[i] = arcch(a[i]) */
_VML_API(void,VSACOSH,(const MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDACOSH,(const MKL_INT *n, const double a[], double r[]))
_vml_api(void,vsacosh,(const MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdacosh,(const MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsAcosh,(const MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdAcosh,(const MKL_INT n,  const double a[], double r[]))

/* Complex Hyperbolic Arc Cosine: r[i] = arcch(a[i]) */
_VML_API(void,VCACOSH,(const MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_VML_API(void,VZACOSH,(const MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_vml_api(void,vcacosh,(const MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_vml_api(void,vzacosh,(const MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_Vml_Api(void,vcAcosh,(const MKL_INT n,  const MKL_Complex8  a[], MKL_Complex8  r[]))
_Vml_Api(void,vzAcosh,(const MKL_INT n,  const MKL_Complex16 a[], MKL_Complex16 r[]))

/* Hyperbolic Arc Sine: r[i] = arcsh(a[i]) */
_VML_API(void,VSASINH,(const MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDASINH,(const MKL_INT *n, const double a[], double r[]))
_vml_api(void,vsasinh,(const MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdasinh,(const MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsAsinh,(const MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdAsinh,(const MKL_INT n,  const double a[], double r[]))

/* Complex Hyperbolic Arc Sine: r[i] = arcsh(a[i]) */
_VML_API(void,VCASINH,(const MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_VML_API(void,VZASINH,(const MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_vml_api(void,vcasinh,(const MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_vml_api(void,vzasinh,(const MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_Vml_Api(void,vcAsinh,(const MKL_INT n,  const MKL_Complex8  a[], MKL_Complex8  r[]))
_Vml_Api(void,vzAsinh,(const MKL_INT n,  const MKL_Complex16 a[], MKL_Complex16 r[]))

/* Hyperbolic Arc Tangent: r[i] = arcth(a[i]) */
_VML_API(void,VSATANH,(const MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDATANH,(const MKL_INT *n, const double a[], double r[]))
_vml_api(void,vsatanh,(const MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdatanh,(const MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsAtanh,(const MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdAtanh,(const MKL_INT n,  const double a[], double r[]))

/* Complex Hyperbolic Arc Tangent: r[i] = arcth(a[i]) */
_VML_API(void,VCATANH,(const MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_VML_API(void,VZATANH,(const MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_vml_api(void,vcatanh,(const MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_vml_api(void,vzatanh,(const MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_Vml_Api(void,vcAtanh,(const MKL_INT n,  const MKL_Complex8  a[], MKL_Complex8  r[]))
_Vml_Api(void,vzAtanh,(const MKL_INT n,  const MKL_Complex16 a[], MKL_Complex16 r[]))

/*  Error Function: r[i] = erf(a[i]) */
_VML_API(void,VSERF,(const MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDERF,(const MKL_INT *n, const double a[], double r[]))
_vml_api(void,vserf,(const MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vderf,(const MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsErf,(const MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdErf,(const MKL_INT n,  const double a[], double r[]))

/*  Inverse Error Function: r[i] = erf(a[i]) */
_VML_API(void,VSERFINV,(const MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDERFINV,(const MKL_INT *n, const double a[], double r[]))
_vml_api(void,vserfinv,(const MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vderfinv,(const MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsErfInv,(const MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdErfInv,(const MKL_INT n,  const double a[], double r[]))

/*   */
_VML_API(void,VSHYPOT,(const MKL_INT *n, const float  a[], const float  b[], float  r[]))
_VML_API(void,VDHYPOT,(const MKL_INT *n, const double a[], const double b[], double r[]))
_vml_api(void,vshypot,(const MKL_INT *n, const float  a[], const float  b[], float  r[]))
_vml_api(void,vdhypot,(const MKL_INT *n, const double a[], const double b[], double r[]))
_Vml_Api(void,vsHypot,(const MKL_INT n,  const float  a[], const float  b[], float  r[]))
_Vml_Api(void,vdHypot,(const MKL_INT n,  const double a[], const double b[], double r[]))

/*  Complementary Error Function: r[i] = 1 - erf(a[i]) */
_VML_API(void,VSERFC,(const MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDERFC,(const MKL_INT *n, const double a[], double r[]))
_vml_api(void,vserfc,(const MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vderfc,(const MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsErfc,(const MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdErfc,(const MKL_INT n,  const double a[], double r[]))

/*   */
_VML_API(void,VSERFCINV,(const MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDERFCINV,(const MKL_INT *n, const double a[], double r[]))
_vml_api(void,vserfcinv,(const MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vderfcinv,(const MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsErfcInv,(const MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdErfcInv,(const MKL_INT n,  const double a[], double r[]))

/*   */
_VML_API(void,VSCDFNORM,(const MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDCDFNORM,(const MKL_INT *n, const double a[], double r[]))
_vml_api(void,vscdfnorm,(const MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdcdfnorm,(const MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsCdfNorm,(const MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdCdfNorm,(const MKL_INT n,  const double a[], double r[]))

/*   */
_VML_API(void,VSCDFNORMINV,(const MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDCDFNORMINV,(const MKL_INT *n, const double a[], double r[]))
_vml_api(void,vscdfnorminv,(const MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdcdfnorminv,(const MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsCdfNormInv,(const MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdCdfNormInv,(const MKL_INT n,  const double a[], double r[]))

/* Arc Tangent of a/b: r[i] = arctan(a[i]/b[i]) */
_VML_API(void,VSATAN2,(const MKL_INT *n, const float  a[], const float  b[], float  r[]))
_VML_API(void,VDATAN2,(const MKL_INT *n, const double a[], const double b[], double r[]))
_vml_api(void,vsatan2,(const MKL_INT *n, const float  a[], const float  b[], float  r[]))
_vml_api(void,vdatan2,(const MKL_INT *n, const double a[], const double b[], double r[]))
_Vml_Api(void,vsAtan2,(const MKL_INT n,  const float  a[], const float  b[], float  r[]))
_Vml_Api(void,vdAtan2,(const MKL_INT n,  const double a[], const double b[], double r[]))

/* Multiplicaton: r[i] = a[i] * b[i] */
_VML_API(void,VSMUL,(const MKL_INT *n, const float  a[], const float  b[], float  r[]))
_VML_API(void,VDMUL,(const MKL_INT *n, const double a[], const double b[], double r[]))
_vml_api(void,vsmul,(const MKL_INT *n, const float  a[], const float  b[], float  r[]))
_vml_api(void,vdmul,(const MKL_INT *n, const double a[], const double b[], double r[]))
_Vml_Api(void,vsMul,(const MKL_INT n,  const float  a[], const float  b[], float  r[]))
_Vml_Api(void,vdMul,(const MKL_INT n,  const double a[], const double b[], double r[]))

/* Complex multiplication: r[i] = a[i] * b[i] */
_VML_API(void,VCMUL,(const MKL_INT *n, const MKL_Complex8  a[], const MKL_Complex8  b[], MKL_Complex8  r[]))
_VML_API(void,VZMUL,(const MKL_INT *n, const MKL_Complex16 a[], const MKL_Complex16 b[], MKL_Complex16 r[]))
_vml_api(void,vcmul,(const MKL_INT *n, const MKL_Complex8  a[], const MKL_Complex8  b[], MKL_Complex8  r[]))
_vml_api(void,vzmul,(const MKL_INT *n, const MKL_Complex16 a[], const MKL_Complex16 b[], MKL_Complex16 r[]))
_Vml_Api(void,vcMul,(const MKL_INT n,  const MKL_Complex8  a[], const MKL_Complex8  b[], MKL_Complex8  r[]))
_Vml_Api(void,vzMul,(const MKL_INT n,  const MKL_Complex16 a[], const MKL_Complex16 b[], MKL_Complex16 r[]))

/* Division: r[i] = a[i] / b[i] */
_VML_API(void,VSDIV,(const MKL_INT *n, const float  a[], const float  b[], float  r[]))
_VML_API(void,VDDIV,(const MKL_INT *n, const double a[], const double b[], double r[]))
_vml_api(void,vsdiv,(const MKL_INT *n, const float  a[], const float  b[], float  r[]))
_vml_api(void,vddiv,(const MKL_INT *n, const double a[], const double b[], double r[]))
_Vml_Api(void,vsDiv,(const MKL_INT n,  const float  a[], const float  b[], float  r[]))
_Vml_Api(void,vdDiv,(const MKL_INT n,  const double a[], const double b[], double r[]))

/* Complex division: r[i] = a[i] / b[i] */
_VML_API(void,VCDIV,(const MKL_INT *n, const MKL_Complex8  a[], const MKL_Complex8  b[], MKL_Complex8  r[]))
_VML_API(void,VZDIV,(const MKL_INT *n, const MKL_Complex16 a[], const MKL_Complex16 b[], MKL_Complex16 r[]))
_vml_api(void,vcdiv,(const MKL_INT *n, const MKL_Complex8  a[], const MKL_Complex8  b[], MKL_Complex8  r[]))
_vml_api(void,vzdiv,(const MKL_INT *n, const MKL_Complex16 a[], const MKL_Complex16 b[], MKL_Complex16 r[]))
_Vml_Api(void,vcDiv,(const MKL_INT n,  const MKL_Complex8  a[], const MKL_Complex8  b[], MKL_Complex8  r[]))
_Vml_Api(void,vzDiv,(const MKL_INT n,  const MKL_Complex16 a[], const MKL_Complex16 b[], MKL_Complex16 r[]))

/* Power Function: r[i] = a[i]^b[i] */
_VML_API(void,VSPOW,(const MKL_INT *n, const float  a[], const float  b[], float  r[]))
_VML_API(void,VDPOW,(const MKL_INT *n, const double a[], const double b[], double r[]))
_vml_api(void,vspow,(const MKL_INT *n, const float  a[], const float  b[], float  r[]))
_vml_api(void,vdpow,(const MKL_INT *n, const double a[], const double b[], double r[]))
_Vml_Api(void,vsPow,(const MKL_INT n,  const float  a[], const float  b[], float  r[]))
_Vml_Api(void,vdPow,(const MKL_INT n,  const double a[], const double b[], double r[]))

/* Complex Power Function: r[i] = a[i]^b[i] */
_VML_API(void,VCPOW,(const MKL_INT *n, const MKL_Complex8  a[], const MKL_Complex8  b[], MKL_Complex8  r[]))
_VML_API(void,VZPOW,(const MKL_INT *n, const MKL_Complex16 a[], const MKL_Complex16 b[], MKL_Complex16 r[]))
_vml_api(void,vcpow,(const MKL_INT *n, const MKL_Complex8  a[], const MKL_Complex8  b[], MKL_Complex8  r[]))
_vml_api(void,vzpow,(const MKL_INT *n, const MKL_Complex16 a[], const MKL_Complex16 b[], MKL_Complex16 r[]))
_Vml_Api(void,vcPow,(const MKL_INT n,  const MKL_Complex8  a[], const MKL_Complex8  b[], MKL_Complex8  r[]))
_Vml_Api(void,vzPow,(const MKL_INT n,  const MKL_Complex16 a[], const MKL_Complex16 b[], MKL_Complex16 r[]))

/* Power Function: r[i] = a[i]^(3/2) */
_VML_API(void,VSPOW3O2,(const MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDPOW3O2,(const MKL_INT *n, const double a[], double r[]))
_vml_api(void,vspow3o2,(const MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdpow3o2,(const MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsPow3o2,(const MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdPow3o2,(const MKL_INT n,  const double a[], double r[]))

/* Power Function: r[i] = a[i]^(2/3) */
_VML_API(void,VSPOW2O3,(const MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDPOW2O3,(const MKL_INT *n, const double a[], double r[]))
_vml_api(void,vspow2o3,(const MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdpow2o3,(const MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsPow2o3,(const MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdPow2o3,(const MKL_INT n,  const double a[], double r[]))

/* Power Function with Fixed Degree: r[i] = a[i]^b */
_VML_API(void,VSPOWX,(const MKL_INT *n, const float  a[], const float  *b, float  r[]))
_VML_API(void,VDPOWX,(const MKL_INT *n, const double a[], const double *b, double r[]))
_vml_api(void,vspowx,(const MKL_INT *n, const float  a[], const float  *b, float  r[]))
_vml_api(void,vdpowx,(const MKL_INT *n, const double a[], const double *b, double r[]))
_Vml_Api(void,vsPowx,(const MKL_INT n,  const float  a[], const float   b, float  r[]))
_Vml_Api(void,vdPowx,(const MKL_INT n,  const double a[], const double  b, double r[]))

/* Complex Power Function with Fixed Degree: r[i] = a[i]^b */
_VML_API(void,VCPOWX,(const MKL_INT *n, const MKL_Complex8  a[], const MKL_Complex8  *b, MKL_Complex8  r[]))
_VML_API(void,VZPOWX,(const MKL_INT *n, const MKL_Complex16 a[], const MKL_Complex16 *b, MKL_Complex16 r[]))
_vml_api(void,vcpowx,(const MKL_INT *n, const MKL_Complex8  a[], const MKL_Complex8  *b, MKL_Complex8  r[]))
_vml_api(void,vzpowx,(const MKL_INT *n, const MKL_Complex16 a[], const MKL_Complex16 *b, MKL_Complex16 r[]))
_Vml_Api(void,vcPowx,(const MKL_INT n,  const MKL_Complex8  a[], const MKL_Complex8   b, MKL_Complex8  r[]))
_Vml_Api(void,vzPowx,(const MKL_INT n,  const MKL_Complex16 a[], const MKL_Complex16  b, MKL_Complex16 r[]))

/* Sine & Cosine: r1[i] = sin(a[i]), r2[i]=cos(a[i]) */
_VML_API(void,VSSINCOS,(const MKL_INT *n, const float  a[], float  r1[], float  r2[]))
_VML_API(void,VDSINCOS,(const MKL_INT *n, const double a[], double r1[], double r2[]))
_vml_api(void,vssincos,(const MKL_INT *n, const float  a[], float  r1[], float  r2[]))
_vml_api(void,vdsincos,(const MKL_INT *n, const double a[], double r1[], double r2[]))
_Vml_Api(void,vsSinCos,(const MKL_INT n,  const float  a[], float  r1[], float  r2[]))
_Vml_Api(void,vdSinCos,(const MKL_INT n,  const double a[], double r1[], double r2[]))

/*  */
_VML_API(void,VSCEIL,(const MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDCEIL,(const MKL_INT *n, const double a[], double r[]))
_vml_api(void,vsceil,(const MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdceil,(const MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsCeil,(const MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdCeil,(const MKL_INT n,  const double a[], double r[]))

/*  */
_VML_API(void,VSFLOOR,(const MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDFLOOR,(const MKL_INT *n, const double a[], double r[]))
_vml_api(void,vsfloor,(const MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdfloor,(const MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsFloor,(const MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdFloor,(const MKL_INT n,  const double a[], double r[]))

/*  */
_VML_API(void,VSMODF,(const MKL_INT *n, const float  a[], float  r1[], float  r2[]))
_VML_API(void,VDMODF,(const MKL_INT *n, const double a[], double r1[], double r2[]))
_vml_api(void,vsmodf,(const MKL_INT *n, const float  a[], float  r1[], float  r2[]))
_vml_api(void,vdmodf,(const MKL_INT *n, const double a[], double r1[], double r2[]))
_Vml_Api(void,vsModf,(const MKL_INT n,  const float  a[], float  r1[], float  r2[]))
_Vml_Api(void,vdModf,(const MKL_INT n,  const double a[], double r1[], double r2[]))

/*  */
_VML_API(void,VSNEARBYINT,(const MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDNEARBYINT,(const MKL_INT *n, const double a[], double r[]))
_vml_api(void,vsnearbyint,(const MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdnearbyint,(const MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsNearbyInt,(const MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdNearbyInt,(const MKL_INT n,  const double a[], double r[]))

/*  */
_VML_API(void,VSRINT,(const MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDRINT,(const MKL_INT *n, const double a[], double r[]))
_vml_api(void,vsrint,(const MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdrint,(const MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsRint,(const MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdRint,(const MKL_INT n,  const double a[], double r[]))

/*  */
_VML_API(void,VSROUND,(const MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDROUND,(const MKL_INT *n, const double a[], double r[]))
_vml_api(void,vsround,(const MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdround,(const MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsRound,(const MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdRound,(const MKL_INT n,  const double a[], double r[]))

/*  */
_VML_API(void,VSTRUNC,(const MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDTRUNC,(const MKL_INT *n, const double a[], double r[]))
_vml_api(void,vstrunc,(const MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdtrunc,(const MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsTrunc,(const MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdTrunc,(const MKL_INT n,  const double a[], double r[]))

/* : r[i] =  */
_VML_API(void,VCCONJ,(const MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_VML_API(void,VZCONJ,(const MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_vml_api(void,vcconj,(const MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_vml_api(void,vzconj,(const MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_Vml_Api(void,vcConj,(const MKL_INT n,  const MKL_Complex8  a[], MKL_Complex8  r[]))
_Vml_Api(void,vzConj,(const MKL_INT n,  const MKL_Complex16 a[], MKL_Complex16 r[]))

/* : r[i] =  */
_VML_API(void,VCMULBYCONJ,(const MKL_INT *n, const MKL_Complex8  a[], const MKL_Complex8  b[], MKL_Complex8  r[]))
_VML_API(void,VZMULBYCONJ,(const MKL_INT *n, const MKL_Complex16 a[], const MKL_Complex16 b[], MKL_Complex16 r[]))
_vml_api(void,vcmulbyconj,(const MKL_INT *n, const MKL_Complex8  a[], const MKL_Complex8  b[], MKL_Complex8  r[]))
_vml_api(void,vzmulbyconj,(const MKL_INT *n, const MKL_Complex16 a[], const MKL_Complex16 b[], MKL_Complex16 r[]))
_Vml_Api(void,vcMulByConj,(const MKL_INT n,  const MKL_Complex8  a[], const MKL_Complex8  b[], MKL_Complex8  r[]))
_Vml_Api(void,vzMulByConj,(const MKL_INT n,  const MKL_Complex16 a[], const MKL_Complex16 b[], MKL_Complex16 r[]))

/* : r[i] =  */
_VML_API(void,VCCIS,(const MKL_INT *n, const float  a[], MKL_Complex8  r[]))
_VML_API(void,VZCIS,(const MKL_INT *n, const double a[], MKL_Complex16 r[]))
_vml_api(void,vccis,(const MKL_INT *n, const float  a[], MKL_Complex8  r[]))
_vml_api(void,vzcis,(const MKL_INT *n, const double a[], MKL_Complex16 r[]))
_Vml_Api(void,vcCIS,(const MKL_INT n,  const float  a[], MKL_Complex8  r[]))
_Vml_Api(void,vzCIS,(const MKL_INT n,  const double a[], MKL_Complex16 r[]))


/*
//++
//  VML PACK FUNCTION DECLARATIONS.
//--
*/
/* Positive Increment Indexing */
_VML_API(void,VSPACKI,(const MKL_INT *n, const float  a[], const MKL_INT * incra, float  y[]))
_VML_API(void,VDPACKI,(const MKL_INT *n, const double a[], const MKL_INT * incra, double y[]))
_vml_api(void,vspacki,(const MKL_INT *n, const float  a[], const MKL_INT * incra, float  y[]))
_vml_api(void,vdpacki,(const MKL_INT *n, const double a[], const MKL_INT * incra, double y[]))
_Vml_Api(void,vsPackI,(const MKL_INT n,  const float  a[], const MKL_INT   incra, float  y[]))
_Vml_Api(void,vdPackI,(const MKL_INT n,  const double a[], const MKL_INT   incra, double y[]))

/* Index Vector Indexing */
_VML_API(void,VSPACKV,(const MKL_INT *n, const float  a[], const MKL_INT ia[], float  y[]))
_VML_API(void,VDPACKV,(const MKL_INT *n, const double a[], const MKL_INT ia[], double y[]))
_vml_api(void,vspackv,(const MKL_INT *n, const float  a[], const MKL_INT ia[], float  y[]))
_vml_api(void,vdpackv,(const MKL_INT *n, const double a[], const MKL_INT ia[], double y[]))
_Vml_Api(void,vsPackV,(const MKL_INT n,  const float  a[], const MKL_INT ia[], float  y[]))
_Vml_Api(void,vdPackV,(const MKL_INT n,  const double a[], const MKL_INT ia[], double y[]))

/* Mask Vector Indexing */
_VML_API(void,VSPACKM,(const MKL_INT *n, const float  a[], const MKL_INT ma[], float  y[]))
_VML_API(void,VDPACKM,(const MKL_INT *n, const double a[], const MKL_INT ma[], double y[]))
_vml_api(void,vspackm,(const MKL_INT *n, const float  a[], const MKL_INT ma[], float  y[]))
_vml_api(void,vdpackm,(const MKL_INT *n, const double a[], const MKL_INT ma[], double y[]))
_Vml_Api(void,vsPackM,(const MKL_INT n,  const float  a[], const MKL_INT ma[], float  y[]))
_Vml_Api(void,vdPackM,(const MKL_INT n,  const double a[], const MKL_INT ma[], double y[]))

/*
//++
//  VML UNPACK FUNCTION DECLARATIONS.
//--
*/
/* Positive Increment Indexing */
_VML_API(void,VSUNPACKI,(const MKL_INT *n, const float  a[], float  y[], const MKL_INT * incry))
_VML_API(void,VDUNPACKI,(const MKL_INT *n, const double a[], double y[], const MKL_INT * incry))
_vml_api(void,vsunpacki,(const MKL_INT *n, const float  a[], float  y[], const MKL_INT * incry))
_vml_api(void,vdunpacki,(const MKL_INT *n, const double a[], double y[], const MKL_INT * incry))
_Vml_Api(void,vsUnpackI,(const MKL_INT n,  const float  a[], float  y[], const MKL_INT incry  ))
_Vml_Api(void,vdUnpackI,(const MKL_INT n,  const double a[], double y[], const MKL_INT incry  ))

/* Index Vector Indexing */
_VML_API(void,VSUNPACKV,(const MKL_INT *n, const float  a[], float  y[], const MKL_INT iy[]   ))
_VML_API(void,VDUNPACKV,(const MKL_INT *n, const double a[], double y[], const MKL_INT iy[]   ))
_vml_api(void,vsunpackv,(const MKL_INT *n, const float  a[], float  y[], const MKL_INT iy[]   ))
_vml_api(void,vdunpackv,(const MKL_INT *n, const double a[], double y[], const MKL_INT iy[]   ))
_Vml_Api(void,vsUnpackV,(const MKL_INT n,  const float  a[], float  y[], const MKL_INT iy[]   ))
_Vml_Api(void,vdUnpackV,(const MKL_INT n,  const double a[], double y[], const MKL_INT iy[]   ))

/* Mask Vector Indexing */
_VML_API(void,VSUNPACKM,(const MKL_INT *n, const float  a[], float  y[], const MKL_INT my[]   ))
_VML_API(void,VDUNPACKM,(const MKL_INT *n, const double a[], double y[], const MKL_INT my[]   ))
_vml_api(void,vsunpackm,(const MKL_INT *n, const float  a[], float  y[], const MKL_INT my[]   ))
_vml_api(void,vdunpackm,(const MKL_INT *n, const double a[], double y[], const MKL_INT my[]   ))
_Vml_Api(void,vsUnpackM,(const MKL_INT n,  const float  a[], float  y[], const MKL_INT my[]   ))
_Vml_Api(void,vdUnpackM,(const MKL_INT n,  const double a[], double y[], const MKL_INT my[]   ))


/*
//++
//  VML ERROR HANDLING FUNCTION DECLARATIONS.
//--
*/
/* Set VML Error Status */
_VML_API(int,VMLSETERRSTATUS,(const MKL_INT * status))
_vml_api(int,vmlseterrstatus,(const MKL_INT * status))
_Vml_Api(int,vmlSetErrStatus,(const MKL_INT   status))

/* Get VML Error Status */
_VML_API(int,VMLGETERRSTATUS,(void))
_vml_api(int,vmlgeterrstatus,(void))
_Vml_Api(int,vmlGetErrStatus,(void))

/* Clear VML Error Status */
_VML_API(int,VMLCLEARERRSTATUS,(void))
_vml_api(int,vmlclearerrstatus,(void))
_Vml_Api(int,vmlClearErrStatus,(void))

/* Set VML Error Callback Function */
_VML_API(VMLErrorCallBack,VMLSETERRORCALLBACK,(const VMLErrorCallBack func))
_vml_api(VMLErrorCallBack,vmlseterrorcallback,(const VMLErrorCallBack func))
_Vml_Api(VMLErrorCallBack,vmlSetErrorCallBack,(const VMLErrorCallBack func))

/* Get VML Error Callback Function */
_VML_API(VMLErrorCallBack,VMLGETERRORCALLBACK,(void))
_vml_api(VMLErrorCallBack,vmlgeterrorcallback,(void))
_Vml_Api(VMLErrorCallBack,vmlGetErrorCallBack,(void))

/* Reset VML Error Callback Function */
_VML_API(VMLErrorCallBack,VMLCLEARERRORCALLBACK,(void))
_vml_api(VMLErrorCallBack,vmlclearerrorcallback,(void))
_Vml_Api(VMLErrorCallBack,vmlClearErrorCallBack,(void))


/*
//++
//  VML MODE FUNCTION DECLARATIONS.
//--
*/
/* Set VML Mode */
_VML_API(unsigned int,VMLSETMODE,(const unsigned MKL_INT *newmode))
_vml_api(unsigned int,vmlsetmode,(const unsigned MKL_INT *newmode))
_Vml_Api(unsigned int,vmlSetMode,(const unsigned MKL_INT  newmode))

/* Get VML Mode */
_VML_API(unsigned int,VMLGETMODE,(void))
_vml_api(unsigned int,vmlgetmode,(void))
_Vml_Api(unsigned int,vmlGetMode,(void))

_VML_API(void,MKLFREETLS,(const unsigned MKL_INT *fdwReason))
_vml_api(void,mklfreetls,(const unsigned MKL_INT *fdwReason))
_Vml_Api(void,MKLFreeTls,(const unsigned MKL_INT  fdwReason))

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __MKL_VML_FUNCTIONS_H__ */
