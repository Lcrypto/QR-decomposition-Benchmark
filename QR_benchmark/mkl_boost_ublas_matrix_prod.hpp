//******************************************************************************
//                            INTEL CONFIDENTIAL
//  Copyright(C) 2008-2010 Intel Corporation. All Rights Reserved.
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
//
//******************************************************************************
// Content:
//     Intel(R) Math Kernel Library (MKL) overloaded Boost/uBLAS prod()
//******************************************************************************

#ifndef _MKL_BOOST_UBLAS_MATRIX_PROD_
#define _MKL_BOOST_UBLAS_MATRIX_PROD_

#ifdef NDEBUG

#include <boost/version.hpp>
#if defined (BOOST_VERSION) && \
	  ((BOOST_VERSION == 103401) \
	|| (BOOST_VERSION == 103500) \
	|| (BOOST_VERSION == 103600) \
	|| (BOOST_VERSION == 103700) \
	|| (BOOST_VERSION == 103800) \
	|| (BOOST_VERSION == 103900) \
	|| (BOOST_VERSION == 104000) \
	|| (BOOST_VERSION == 104100))

#include <boost/numeric/ublas/matrix.hpp>

#include "mkl_boost_ublas_gemm.hpp"

namespace boost { namespace numeric { namespace ublas {

    template<class T, class F, class A>
    MKL_BOOST_UBLAS_INLINE
    matrix<T,F,A>    // prod( m1, m2 )
    prod(const matrix<T,F,A> &m1,
         const matrix<T,F,A> &m2)
    {
        matrix<T,F,A> temporary(m1.size1(),m2.size2());
        mkl::gemm(CblasNoTrans, CblasNoTrans, m1, m2, temporary);
        return temporary;
    }
    template<class T, class F, class A>
    MKL_BOOST_UBLAS_INLINE
    matrix<T,F,A>    // prod( trans(m1), m2 )
    prod(const matrix_unary2<matrix<T,F,A>,scalar_identity<T> > &m1,
         const matrix<T,F,A> &m2)
    {
        matrix<T,F,A> temporary(m1.size1(),m2.size2());
        mkl::gemm(CblasTrans, CblasNoTrans, m1, m2, temporary);
        return temporary;
    }
    template<class T, class F, class A>
    MKL_BOOST_UBLAS_INLINE
    matrix<T,F,A>    // prod( trans(conj(m1)), m2 )
    prod(const matrix_unary2<matrix_unary1<matrix<T,F,A>,scalar_conj<T> > const ,scalar_identity<T> > &m1,
         const matrix<T,F,A> &m2)
    {
        matrix<T,F,A> temporary(m1.size1(),m2.size2());
        mkl::gemm(CblasConjTrans, CblasNoTrans, m1, m2, temporary);
        return temporary;
    }
    template<class T, class F, class A>
    MKL_BOOST_UBLAS_INLINE
    matrix<T,F,A>    // prod( conj(trans(m1)), m2 )
    prod(const matrix_unary1<matrix_unary2<matrix<T,F,A>,scalar_identity<T> >,scalar_conj<T> > &m1,
         const matrix<T,F,A> &m2)
    {
        matrix<T,F,A> temporary(m1.size1(),m2.size2());
        mkl::gemm(CblasConjTrans, CblasNoTrans, m1, m2, temporary);
        return temporary;
    }
    template<class T, class F, class A>
    MKL_BOOST_UBLAS_INLINE
    matrix<T,F,A>    // prod( m1, trans(m2) )
    prod(const matrix<T,F,A> &m1,
         const matrix_unary2<matrix<T,F,A>,scalar_identity<T> > &m2)
    {
        matrix<T,F,A> temporary(m1.size1(),m2.size2());
        mkl::gemm(CblasNoTrans, CblasTrans, m1, m2, temporary);
        return temporary;
    }
    template<class T, class F, class A>
    MKL_BOOST_UBLAS_INLINE
    matrix<T,F,A>    // prod( trans(m1), trans(m2) )
    prod(const matrix_unary2<matrix<T,F,A>,scalar_identity<T> > &m1,
         const matrix_unary2<matrix<T,F,A>,scalar_identity<T> > &m2)
    {
        matrix<T,F,A> temporary(m1.size1(),m2.size2());
        mkl::gemm(CblasTrans, CblasTrans, m1, m2, temporary);
        return temporary;
    }
    template<class T, class F, class A>
    MKL_BOOST_UBLAS_INLINE
    matrix<T,F,A>    // prod( trans(conj(m1)), trans(m2) )
    prod(const matrix_unary2<matrix_unary1<matrix<T,F,A>,scalar_conj<T> > const ,scalar_identity<T> > &m1,
         const matrix_unary2<matrix<T,F,A>,scalar_identity<T> > &m2)
    {
        matrix<T,F,A> temporary(m1.size1(),m2.size2());
        mkl::gemm(CblasConjTrans, CblasTrans, m1, m2, temporary);
        return temporary;
    }
    template<class T, class F, class A>
    MKL_BOOST_UBLAS_INLINE
    matrix<T,F,A>    // prod( conj(trans(m1)), trans(m2) )
    prod(const matrix_unary1<matrix_unary2<matrix<T,F,A>,scalar_identity<T> >,scalar_conj<T> > &m1,
         const matrix_unary2<matrix<T,F,A>,scalar_identity<T> > &m2)
    {
        matrix<T,F,A> temporary(m1.size1(),m2.size2());
        mkl::gemm(CblasConjTrans, CblasTrans, m1, m2, temporary);
        return temporary;
    }
    template<class T, class F, class A>
    MKL_BOOST_UBLAS_INLINE
    matrix<T,F,A>    // prod( m1, trans(conj(m2)) )
    prod(const matrix<T,F,A> &m1,
         const matrix_unary2<matrix_unary1<matrix<T,F,A>,scalar_conj<T> > const ,scalar_identity<T> > &m2)
    {
        matrix<T,F,A> temporary(m1.size1(),m2.size2());
        mkl::gemm(CblasNoTrans, CblasConjTrans, m1, m2, temporary);
        return temporary;
    }
    template<class T, class F, class A>
    MKL_BOOST_UBLAS_INLINE
    matrix<T,F,A>    // prod( trans(m1), trans(conj(m2)) )
    prod(const matrix_unary2<matrix<T,F,A>,scalar_identity<T> > &m1,
         const matrix_unary2<matrix_unary1<matrix<T,F,A>,scalar_conj<T> > const ,scalar_identity<T> > &m2)
    {
        matrix<T,F,A> temporary(m1.size1(),m2.size2());
        mkl::gemm(CblasTrans, CblasConjTrans, m1, m2, temporary);
        return temporary;
    }
    template<class T, class F, class A>
    MKL_BOOST_UBLAS_INLINE
    matrix<T,F,A>    // prod( trans(conj(m1)), trans(conj(m2)) )
    prod(const matrix_unary2<matrix_unary1<matrix<T,F,A>,scalar_conj<T> > const ,scalar_identity<T> > &m1,
         const matrix_unary2<matrix_unary1<matrix<T,F,A>,scalar_conj<T> > const ,scalar_identity<T> > &m2)
    {
        matrix<T,F,A> temporary(m1.size1(),m2.size2());
        mkl::gemm(CblasConjTrans, CblasConjTrans, m1, m2, temporary);
        return temporary;
    }
    template<class T, class F, class A>
    MKL_BOOST_UBLAS_INLINE
    matrix<T,F,A>    // prod( conj(trans(m1)), trans(conj(m2)) )
    prod(const matrix_unary1<matrix_unary2<matrix<T,F,A>,scalar_identity<T> >,scalar_conj<T> > &m1,
         const matrix_unary2<matrix_unary1<matrix<T,F,A>,scalar_conj<T> > const ,scalar_identity<T> > &m2)
    {
        matrix<T,F,A> temporary(m1.size1(),m2.size2());
        mkl::gemm(CblasConjTrans, CblasConjTrans, m1, m2, temporary);
        return temporary;
    }
    template<class T, class F, class A>
    MKL_BOOST_UBLAS_INLINE
    matrix<T,F,A>    // prod( m1, conj(trans(m2)) )
    prod(const matrix<T,F,A> &m1,
         const matrix_unary1<matrix_unary2<matrix<T,F,A>,scalar_identity<T> >,scalar_conj<T> > &m2)
    {
        matrix<T,F,A> temporary(m1.size1(),m2.size2());
        mkl::gemm(CblasNoTrans, CblasConjTrans, m1, m2, temporary);
        return temporary;
    }
    template<class T, class F, class A>
    MKL_BOOST_UBLAS_INLINE
    matrix<T,F,A>    // prod( trans(m1), conj(trans(m2)) )
    prod(const matrix_unary2<matrix<T,F,A>,scalar_identity<T> > &m1,
         const matrix_unary1<matrix_unary2<matrix<T,F,A>,scalar_identity<T> >,scalar_conj<T> > &m2)
    {
        matrix<T,F,A> temporary(m1.size1(),m2.size2());
        mkl::gemm(CblasTrans, CblasConjTrans, m1, m2, temporary);
        return temporary;
    }
    template<class T, class F, class A>
    MKL_BOOST_UBLAS_INLINE
    matrix<T,F,A>    // prod( trans(conj(m1)), conj(trans(m2)) )
    prod(const matrix_unary2<matrix_unary1<matrix<T,F,A>,scalar_conj<T> > const ,scalar_identity<T> > &m1,
         const matrix_unary1<matrix_unary2<matrix<T,F,A>,scalar_identity<T> >,scalar_conj<T> > &m2)
    {
        matrix<T,F,A> temporary(m1.size1(),m2.size2());
        mkl::gemm(CblasConjTrans, CblasConjTrans, m1, m2, temporary);
        return temporary;
    }
    template<class T, class F, class A>
    MKL_BOOST_UBLAS_INLINE
    matrix<T,F,A>    // prod( conj(trans(m1)), conj(trans(m2)) )
    prod(const matrix_unary1<matrix_unary2<matrix<T,F,A>,scalar_identity<T> >,scalar_conj<T> > &m1,
         const matrix_unary1<matrix_unary2<matrix<T,F,A>,scalar_identity<T> >,scalar_conj<T> > &m2)
    {
        matrix<T,F,A> temporary(m1.size1(),m2.size2());
        mkl::gemm(CblasConjTrans, CblasConjTrans, m1, m2, temporary);
        return temporary;
    }

}}}
#endif  // BOOST_VERSION
#endif  // NDEBUG
#endif  // _MKL_BOOST_UBLAS_MATRIX_PROD_
