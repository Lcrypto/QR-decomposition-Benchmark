/* file: mkl_vml_types.h */
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
//  User-level type definitions.
//--
*/

#ifndef __MKL_VML_TYPES_H__
#define __MKL_VML_TYPES_H__

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#include "mkl_types.h"

/*
//++
//  TYPEDEFS
//--
*/

/*
//  ERROR CALLBACK CONTEXT.
//  Error callback context structure is used in a user's error callback
//  function with the following interface:
//
//      int USER_CALLBACK_FUNC_NAME( DefVmlErrorContext par )
//
//  Error callback context fields:
//  iCode        - error status
//  iIndex       - index of bad argument
//  dbA1         - 1-st argument value, at which error occured
//  dbA2         - 2-nd argument value, at which error occured
//                 (2-argument functions only)
//  dbR1         - 1-st resulting value
//  dbR2         - 2-nd resulting value (2-result functions only)
//  cFuncName    - function name, for which error occured
//  iFuncNameLen - length of function name
*/
typedef struct _DefVmlErrorContext
{
    int     iCode;
    int     iIndex;
    double  dbA1;
    double  dbA2;
    double  dbR1;
    double  dbR2;
    char    cFuncName[64];
    int     iFuncNameLen;
} DefVmlErrorContext;

/*
// User error callback handler function type
*/
typedef int (*VMLErrorCallBack) (DefVmlErrorContext* pdefVmlErrorContext);


#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __MKL_VML_TYPES_H__ */
