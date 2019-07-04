!*******************************************************************************
!                              INTEL CONFIDENTIAL
!   Copyright(C) 2005-2010 Intel Corporation. All Rights Reserved.
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
!      F95 interface for BLAS routines
!*******************************************************************************
! This file was generated automatically!
!*******************************************************************************

MODULE F95_PRECISION
    INTEGER, PARAMETER :: SP = KIND(1.0E0)
    INTEGER, PARAMETER :: DP = KIND(1.0D0)
END MODULE F95_PRECISION

MODULE BLAS95

INTERFACE ASUM
    PURE FUNCTION SASUM_F95(X)
        ! Fortran77 call:
        ! SASUM(N,X,INCX)
        USE F95_PRECISION, ONLY: WP => SP
        REAL(WP) :: SASUM_F95
        REAL(WP), INTENT(IN) :: X(:)
    END FUNCTION SASUM_F95
    PURE FUNCTION SCASUM_F95(X)
        ! Fortran77 call:
        ! SCASUM(N,X,INCX)
        USE F95_PRECISION, ONLY: WP => SP
        REAL(WP) :: SCASUM_F95
        COMPLEX(WP), INTENT(IN) :: X(:)
    END FUNCTION SCASUM_F95
    PURE FUNCTION DASUM_F95(X)
        ! Fortran77 call:
        ! DASUM(N,X,INCX)
        USE F95_PRECISION, ONLY: WP => DP
        REAL(WP) :: DASUM_F95
        REAL(WP), INTENT(IN) :: X(:)
    END FUNCTION DASUM_F95
    PURE FUNCTION DZASUM_F95(X)
        ! Fortran77 call:
        ! DZASUM(N,X,INCX)
        USE F95_PRECISION, ONLY: WP => DP
        REAL(WP) :: DZASUM_F95
        COMPLEX(WP), INTENT(IN) :: X(:)
    END FUNCTION DZASUM_F95
END INTERFACE ASUM

INTERFACE AXPY
        ! Default A=1
    PURE SUBROUTINE SAXPY_F95(X,Y,A)
        ! Fortran77 call:
        ! SAXPY(N,A,X,INCX,Y,INCY)
        USE F95_PRECISION, ONLY: WP => SP
        REAL(WP), INTENT(IN), OPTIONAL :: A
        REAL(WP), INTENT(IN) :: X(:)
        REAL(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE SAXPY_F95
    PURE SUBROUTINE DAXPY_F95(X,Y,A)
        ! Fortran77 call:
        ! DAXPY(N,A,X,INCX,Y,INCY)
        USE F95_PRECISION, ONLY: WP => DP
        REAL(WP), INTENT(IN), OPTIONAL :: A
        REAL(WP), INTENT(IN) :: X(:)
        REAL(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE DAXPY_F95
    PURE SUBROUTINE CAXPY_F95(X,Y,A)
        ! Fortran77 call:
        ! CAXPY(N,A,X,INCX,Y,INCY)
        USE F95_PRECISION, ONLY: WP => SP
        COMPLEX(WP), INTENT(IN), OPTIONAL :: A
        COMPLEX(WP), INTENT(IN) :: X(:)
        COMPLEX(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE CAXPY_F95
    PURE SUBROUTINE ZAXPY_F95(X,Y,A)
        ! Fortran77 call:
        ! ZAXPY(N,A,X,INCX,Y,INCY)
        USE F95_PRECISION, ONLY: WP => DP
        COMPLEX(WP), INTENT(IN), OPTIONAL :: A
        COMPLEX(WP), INTENT(IN) :: X(:)
        COMPLEX(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE ZAXPY_F95
END INTERFACE AXPY

INTERFACE COPY
    PURE SUBROUTINE SCOPY_F95(X,Y)
        ! Fortran77 call:
        ! SCOPY(N,X,INCX,Y,INCY)
        USE F95_PRECISION, ONLY: WP => SP
        REAL(WP), INTENT(IN) :: X(:)
        REAL(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE SCOPY_F95
    PURE SUBROUTINE DCOPY_F95(X,Y)
        ! Fortran77 call:
        ! DCOPY(N,X,INCX,Y,INCY)
        USE F95_PRECISION, ONLY: WP => DP
        REAL(WP), INTENT(IN) :: X(:)
        REAL(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE DCOPY_F95
    PURE SUBROUTINE CCOPY_F95(X,Y)
        ! Fortran77 call:
        ! CCOPY(N,X,INCX,Y,INCY)
        USE F95_PRECISION, ONLY: WP => SP
        COMPLEX(WP), INTENT(IN) :: X(:)
        COMPLEX(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE CCOPY_F95
    PURE SUBROUTINE ZCOPY_F95(X,Y)
        ! Fortran77 call:
        ! ZCOPY(N,X,INCX,Y,INCY)
        USE F95_PRECISION, ONLY: WP => DP
        COMPLEX(WP), INTENT(IN) :: X(:)
        COMPLEX(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE ZCOPY_F95
END INTERFACE COPY

INTERFACE DOT
    PURE FUNCTION SDOT_F95(X,Y)
        ! Fortran77 call:
        ! SDOT(N,X,INCX,Y,INCY)
        USE F95_PRECISION, ONLY: WP => SP
        REAL(WP) :: SDOT_F95
        REAL(WP), INTENT(IN) :: X(:)
        REAL(WP), INTENT(IN) :: Y(:)
    END FUNCTION SDOT_F95
    PURE FUNCTION DDOT_F95(X,Y)
        ! Fortran77 call:
        ! DDOT(N,X,INCX,Y,INCY)
        USE F95_PRECISION, ONLY: WP => DP
        REAL(WP) :: DDOT_F95
        REAL(WP), INTENT(IN) :: X(:)
        REAL(WP), INTENT(IN) :: Y(:)
    END FUNCTION DDOT_F95
END INTERFACE DOT

INTERFACE SDOT
    PURE FUNCTION SDSDOT_F95(SX,SY,SB)
        ! Fortran77 call:
        ! SDSDOT(N,SB,SX,INCX,SY,INCY)
        USE F95_PRECISION, ONLY: WP => SP
        REAL(WP) :: SDSDOT_F95
        REAL(WP), INTENT(IN) :: SB
        REAL(WP), INTENT(IN) :: SX(:)
        REAL(WP), INTENT(IN) :: SY(:)
    END FUNCTION SDSDOT_F95
    PURE FUNCTION DSDOT_F95(SX,SY)
        ! Fortran77 call:
        ! DSDOT(N,SX,INCX,SY,INCY)
        USE F95_PRECISION, ONLY: WP => DP, SP
        REAL(WP) :: DSDOT_F95
        REAL(SP), INTENT(IN) :: SX(:)
        REAL(SP), INTENT(IN) :: SY(:)
    END FUNCTION DSDOT_F95
END INTERFACE SDOT

INTERFACE DOTC
    PURE FUNCTION CDOTC_F95(X,Y)
        ! Fortran77 call:
        ! CDOTC(N,X,INCX,Y,INCY)
        USE F95_PRECISION, ONLY: WP => SP
        COMPLEX(WP) :: CDOTC_F95
        COMPLEX(WP), INTENT(IN) :: X(:)
        COMPLEX(WP), INTENT(IN) :: Y(:)
    END FUNCTION CDOTC_F95
    PURE FUNCTION ZDOTC_F95(X,Y)
        ! Fortran77 call:
        ! ZDOTC(N,X,INCX,Y,INCY)
        USE F95_PRECISION, ONLY: WP => DP
        COMPLEX(WP) :: ZDOTC_F95
        COMPLEX(WP), INTENT(IN) :: X(:)
        COMPLEX(WP), INTENT(IN) :: Y(:)
    END FUNCTION ZDOTC_F95
END INTERFACE DOTC

INTERFACE DOTU
    PURE FUNCTION CDOTU_F95(X,Y)
        ! Fortran77 call:
        ! CDOTU(N,X,INCX,Y,INCY)
        USE F95_PRECISION, ONLY: WP => SP
        COMPLEX(WP) :: CDOTU_F95
        COMPLEX(WP), INTENT(IN) :: X(:)
        COMPLEX(WP), INTENT(IN) :: Y(:)
    END FUNCTION CDOTU_F95
    PURE FUNCTION ZDOTU_F95(X,Y)
        ! Fortran77 call:
        ! ZDOTU(N,X,INCX,Y,INCY)
        USE F95_PRECISION, ONLY: WP => DP
        COMPLEX(WP) :: ZDOTU_F95
        COMPLEX(WP), INTENT(IN) :: X(:)
        COMPLEX(WP), INTENT(IN) :: Y(:)
    END FUNCTION ZDOTU_F95
END INTERFACE DOTU

INTERFACE NRM2
    PURE FUNCTION SNRM2_F95(X)
        ! Fortran77 call:
        ! SNRM2(N,X,INCX)
        USE F95_PRECISION, ONLY: WP => SP
        REAL(WP) :: SNRM2_F95
        REAL(WP), INTENT(IN) :: X(:)
    END FUNCTION SNRM2_F95
    PURE FUNCTION DNRM2_F95(X)
        ! Fortran77 call:
        ! DNRM2(N,X,INCX)
        USE F95_PRECISION, ONLY: WP => DP
        REAL(WP) :: DNRM2_F95
        REAL(WP), INTENT(IN) :: X(:)
    END FUNCTION DNRM2_F95
    PURE FUNCTION SCNRM2_F95(X)
        ! Fortran77 call:
        ! SCNRM2(N,X,INCX)
        USE F95_PRECISION, ONLY: WP => SP
        REAL(WP) :: SCNRM2_F95
        COMPLEX(WP), INTENT(IN) :: X(:)
    END FUNCTION SCNRM2_F95
    PURE FUNCTION DZNRM2_F95(X)
        ! Fortran77 call:
        ! DZNRM2(N,X,INCX)
        USE F95_PRECISION, ONLY: WP => DP
        REAL(WP) :: DZNRM2_F95
        COMPLEX(WP), INTENT(IN) :: X(:)
    END FUNCTION DZNRM2_F95
END INTERFACE NRM2

INTERFACE ROT
    PURE SUBROUTINE SROT_F95(X,Y,C,S)
        ! Fortran77 call:
        ! SROT(N,X,INCX,Y,INCY,C,S)
        USE F95_PRECISION, ONLY: WP => SP
        REAL(WP), INTENT(IN) :: C
        REAL(WP), INTENT(IN) :: S
        REAL(WP), INTENT(INOUT) :: X(:)
        REAL(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE SROT_F95
    PURE SUBROUTINE DROT_F95(X,Y,C,S)
        ! Fortran77 call:
        ! DROT(N,X,INCX,Y,INCY,C,S)
        USE F95_PRECISION, ONLY: WP => DP
        REAL(WP), INTENT(IN) :: C
        REAL(WP), INTENT(IN) :: S
        REAL(WP), INTENT(INOUT) :: X(:)
        REAL(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE DROT_F95
    PURE SUBROUTINE CSROT_F95(X,Y,C,S)
        ! Fortran77 call:
        ! CSROT(N,X,INCX,Y,INCY,C,S)
        USE F95_PRECISION, ONLY: WP => SP
        REAL(WP), INTENT(IN) :: C
        REAL(WP), INTENT(IN) :: S
        COMPLEX(WP), INTENT(INOUT) :: X(:)
        COMPLEX(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE CSROT_F95
    PURE SUBROUTINE ZDROT_F95(X,Y,C,S)
        ! Fortran77 call:
        ! ZDROT(N,X,INCX,Y,INCY,C,S)
        USE F95_PRECISION, ONLY: WP => DP
        REAL(WP), INTENT(IN) :: C
        REAL(WP), INTENT(IN) :: S
        COMPLEX(WP), INTENT(INOUT) :: X(:)
        COMPLEX(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE ZDROT_F95
END INTERFACE ROT

INTERFACE ROTG
    PURE SUBROUTINE SROTG(A,B,C,S)
        ! Fortran77 call:
        ! SROTG(A,B,C,S)
        USE F95_PRECISION, ONLY: WP => SP
        REAL(WP), INTENT(INOUT) :: A
        REAL(WP), INTENT(INOUT) :: B
        REAL(WP), INTENT(OUT) :: C
        REAL(WP), INTENT(OUT) :: S
    END SUBROUTINE SROTG
    PURE SUBROUTINE DROTG(A,B,C,S)
        ! Fortran77 call:
        ! DROTG(A,B,C,S)
        USE F95_PRECISION, ONLY: WP => DP
        REAL(WP), INTENT(INOUT) :: A
        REAL(WP), INTENT(INOUT) :: B
        REAL(WP), INTENT(OUT) :: C
        REAL(WP), INTENT(OUT) :: S
    END SUBROUTINE DROTG
    PURE SUBROUTINE CROTG(A,B,C,S)
        ! Fortran77 call:
        ! CROTG(A,B,C,S)
        USE F95_PRECISION, ONLY: WP => SP
        COMPLEX(WP), INTENT(INOUT) :: A
        COMPLEX(WP), INTENT(INOUT) :: B
        REAL(WP), INTENT(OUT) :: C
        COMPLEX(WP), INTENT(OUT) :: S
    END SUBROUTINE CROTG
    PURE SUBROUTINE ZROTG(A,B,C,S)
        ! Fortran77 call:
        ! ZROTG(A,B,C,S)
        USE F95_PRECISION, ONLY: WP => DP
        COMPLEX(WP), INTENT(INOUT) :: A
        COMPLEX(WP), INTENT(INOUT) :: B
        REAL(WP), INTENT(OUT) :: C
        COMPLEX(WP), INTENT(OUT) :: S
    END SUBROUTINE ZROTG
END INTERFACE ROTG

INTERFACE ROTM
    PURE SUBROUTINE SROTM_F95(X,Y,PARAM)
        ! Fortran77 call:
        ! SROTM(N,X,INCX,Y,INCY,PARAM)
        USE F95_PRECISION, ONLY: WP => SP
        REAL(WP), INTENT(INOUT) :: X(:)
        REAL(WP), INTENT(INOUT) :: Y(:)
        REAL(WP), INTENT(IN) :: PARAM(5)
    END SUBROUTINE SROTM_F95
    PURE SUBROUTINE DROTM_F95(X,Y,PARAM)
        ! Fortran77 call:
        ! DROTM(N,X,INCX,Y,INCY,PARAM)
        USE F95_PRECISION, ONLY: WP => DP
        REAL(WP), INTENT(INOUT) :: X(:)
        REAL(WP), INTENT(INOUT) :: Y(:)
        REAL(WP), INTENT(IN) :: PARAM(5)
    END SUBROUTINE DROTM_F95
END INTERFACE ROTM

INTERFACE ROTMG
    PURE SUBROUTINE SROTMG_F95(D1,D2,X1,Y1,PARAM)
        ! Fortran77 call:
        ! SROTMG(D1,D2,X1,Y1,PARAM)
        USE F95_PRECISION, ONLY: WP => SP
        REAL(WP), INTENT(INOUT) :: D1
        REAL(WP), INTENT(INOUT) :: D2
        REAL(WP), INTENT(INOUT) :: X1
        REAL(WP), INTENT(IN) :: Y1
        REAL(WP), INTENT(OUT) :: PARAM(5)
    END SUBROUTINE SROTMG_F95
    PURE SUBROUTINE DROTMG_F95(D1,D2,X1,Y1,PARAM)
        ! Fortran77 call:
        ! DROTMG(D1,D2,X1,Y1,PARAM)
        USE F95_PRECISION, ONLY: WP => DP
        REAL(WP), INTENT(INOUT) :: D1
        REAL(WP), INTENT(INOUT) :: D2
        REAL(WP), INTENT(INOUT) :: X1
        REAL(WP), INTENT(IN) :: Y1
        REAL(WP), INTENT(OUT) :: PARAM(5)
    END SUBROUTINE DROTMG_F95
END INTERFACE ROTMG

INTERFACE SCAL
    PURE SUBROUTINE SSCAL_F95(X,A)
        ! Fortran77 call:
        ! SSCAL(N,A,X,INCX)
        USE F95_PRECISION, ONLY: WP => SP
        REAL(WP), INTENT(IN) :: A
        REAL(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE SSCAL_F95
    PURE SUBROUTINE DSCAL_F95(X,A)
        ! Fortran77 call:
        ! DSCAL(N,A,X,INCX)
        USE F95_PRECISION, ONLY: WP => DP
        REAL(WP), INTENT(IN) :: A
        REAL(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE DSCAL_F95
    PURE SUBROUTINE CSCAL_F95(X,A)
        ! Fortran77 call:
        ! CSCAL(N,A,X,INCX)
        USE F95_PRECISION, ONLY: WP => SP
        COMPLEX(WP), INTENT(IN) :: A
        COMPLEX(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE CSCAL_F95
    PURE SUBROUTINE ZSCAL_F95(X,A)
        ! Fortran77 call:
        ! ZSCAL(N,A,X,INCX)
        USE F95_PRECISION, ONLY: WP => DP
        COMPLEX(WP), INTENT(IN) :: A
        COMPLEX(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE ZSCAL_F95
    PURE SUBROUTINE CSSCAL_F95(X,A)
        ! Fortran77 call:
        ! CSSCAL(N,A,X,INCX)
        USE F95_PRECISION, ONLY: WP => SP
        REAL(WP), INTENT(IN) :: A
        COMPLEX(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE CSSCAL_F95
    PURE SUBROUTINE ZDSCAL_F95(X,A)
        ! Fortran77 call:
        ! ZDSCAL(N,A,X,INCX)
        USE F95_PRECISION, ONLY: WP => DP
        REAL(WP), INTENT(IN) :: A
        COMPLEX(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE ZDSCAL_F95
END INTERFACE SCAL

INTERFACE SWAP
    PURE SUBROUTINE SSWAP_F95(X,Y)
        ! Fortran77 call:
        ! SSWAP(N,X,INCX,Y,INCY)
        USE F95_PRECISION, ONLY: WP => SP
        REAL(WP), INTENT(INOUT) :: X(:)
        REAL(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE SSWAP_F95
    PURE SUBROUTINE DSWAP_F95(X,Y)
        ! Fortran77 call:
        ! DSWAP(N,X,INCX,Y,INCY)
        USE F95_PRECISION, ONLY: WP => DP
        REAL(WP), INTENT(INOUT) :: X(:)
        REAL(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE DSWAP_F95
    PURE SUBROUTINE CSWAP_F95(X,Y)
        ! Fortran77 call:
        ! CSWAP(N,X,INCX,Y,INCY)
        USE F95_PRECISION, ONLY: WP => SP
        COMPLEX(WP), INTENT(INOUT) :: X(:)
        COMPLEX(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE CSWAP_F95
    PURE SUBROUTINE ZSWAP_F95(X,Y)
        ! Fortran77 call:
        ! ZSWAP(N,X,INCX,Y,INCY)
        USE F95_PRECISION, ONLY: WP => DP
        COMPLEX(WP), INTENT(INOUT) :: X(:)
        COMPLEX(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE ZSWAP_F95
END INTERFACE SWAP

INTERFACE IAMAX
    PURE FUNCTION ISAMAX_F95(X)
        ! Fortran77 call:
        ! ISAMAX(N,X,INCX)
        USE F95_PRECISION, ONLY: WP => SP
        INTEGER :: ISAMAX_F95
        REAL(WP), INTENT(IN) :: X(:)
    END FUNCTION ISAMAX_F95
    PURE FUNCTION IDAMAX_F95(X)
        ! Fortran77 call:
        ! IDAMAX(N,X,INCX)
        USE F95_PRECISION, ONLY: WP => DP
        INTEGER :: IDAMAX_F95
        REAL(WP), INTENT(IN) :: X(:)
    END FUNCTION IDAMAX_F95
    PURE FUNCTION ICAMAX_F95(X)
        ! Fortran77 call:
        ! ICAMAX(N,X,INCX)
        USE F95_PRECISION, ONLY: WP => SP
        INTEGER :: ICAMAX_F95
        COMPLEX(WP), INTENT(IN) :: X(:)
    END FUNCTION ICAMAX_F95
    PURE FUNCTION IZAMAX_F95(X)
        ! Fortran77 call:
        ! IZAMAX(N,X,INCX)
        USE F95_PRECISION, ONLY: WP => DP
        INTEGER :: IZAMAX_F95
        COMPLEX(WP), INTENT(IN) :: X(:)
    END FUNCTION IZAMAX_F95
END INTERFACE IAMAX

INTERFACE IAMIN
    PURE FUNCTION ISAMIN_F95(X)
        ! Fortran77 call:
        ! ISAMIN(N,X,INCX)
        USE F95_PRECISION, ONLY: WP => SP
        INTEGER :: ISAMIN_F95
        REAL(WP), INTENT(IN) :: X(:)
    END FUNCTION ISAMIN_F95
    PURE FUNCTION IDAMIN_F95(X)
        ! Fortran77 call:
        ! IDAMIN(N,X,INCX)
        USE F95_PRECISION, ONLY: WP => DP
        INTEGER :: IDAMIN_F95
        REAL(WP), INTENT(IN) :: X(:)
    END FUNCTION IDAMIN_F95
    PURE FUNCTION ICAMIN_F95(X)
        ! Fortran77 call:
        ! ICAMIN(N,X,INCX)
        USE F95_PRECISION, ONLY: WP => SP
        INTEGER :: ICAMIN_F95
        COMPLEX(WP), INTENT(IN) :: X(:)
    END FUNCTION ICAMIN_F95
    PURE FUNCTION IZAMIN_F95(X)
        ! Fortran77 call:
        ! IZAMIN(N,X,INCX)
        USE F95_PRECISION, ONLY: WP => DP
        INTEGER :: IZAMIN_F95
        COMPLEX(WP), INTENT(IN) :: X(:)
    END FUNCTION IZAMIN_F95
END INTERFACE IAMIN

INTERFACE CABS1
    PURE FUNCTION SCABS1(C)
        ! Fortran77 call:
        ! SCABS1(C)
        USE F95_PRECISION, ONLY: WP => SP
        REAL(WP) :: SCABS1
        COMPLEX(WP), INTENT(IN) :: C
    END FUNCTION SCABS1
    PURE FUNCTION DCABS1(Z)
        ! Fortran77 call:
        ! DCABS1(Z)
        USE F95_PRECISION, ONLY: WP => DP
        REAL(WP) :: DCABS1
        COMPLEX(WP), INTENT(IN) :: Z
    END FUNCTION DCABS1
END INTERFACE CABS1

INTERFACE GBMV
        ! TRANS='N','C','T'; default: 'N'
        ! Default ALPHA=1
        ! Default BETA=0
    PURE SUBROUTINE SGBMV_F95(A,X,Y,KL,M,ALPHA,BETA,TRANS)
        ! Fortran77 call:
        ! SGBMV(TRANS,M,N,KL,KU,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
        USE F95_PRECISION, ONLY: WP => SP
        INTEGER, INTENT(IN), OPTIONAL :: KL
        INTEGER, INTENT(IN), OPTIONAL :: M
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(IN), OPTIONAL :: BETA
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(IN) :: X(:)
        REAL(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE SGBMV_F95
    PURE SUBROUTINE DGBMV_F95(A,X,Y,KL,M,ALPHA,BETA,TRANS)
        ! Fortran77 call:
        ! DGBMV(TRANS,M,N,KL,KU,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
        USE F95_PRECISION, ONLY: WP => DP
        INTEGER, INTENT(IN), OPTIONAL :: KL
        INTEGER, INTENT(IN), OPTIONAL :: M
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(IN), OPTIONAL :: BETA
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(IN) :: X(:)
        REAL(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE DGBMV_F95
    PURE SUBROUTINE CGBMV_F95(A,X,Y,KL,M,ALPHA,BETA,TRANS)
        ! Fortran77 call:
        ! CGBMV(TRANS,M,N,KL,KU,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
        USE F95_PRECISION, ONLY: WP => SP
        INTEGER, INTENT(IN), OPTIONAL :: KL
        INTEGER, INTENT(IN), OPTIONAL :: M
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(IN), OPTIONAL :: BETA
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: X(:)
        COMPLEX(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE CGBMV_F95
    PURE SUBROUTINE ZGBMV_F95(A,X,Y,KL,M,ALPHA,BETA,TRANS)
        ! Fortran77 call:
        ! ZGBMV(TRANS,M,N,KL,KU,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
        USE F95_PRECISION, ONLY: WP => DP
        INTEGER, INTENT(IN), OPTIONAL :: KL
        INTEGER, INTENT(IN), OPTIONAL :: M
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(IN), OPTIONAL :: BETA
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: X(:)
        COMPLEX(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE ZGBMV_F95
END INTERFACE GBMV

INTERFACE GEMV
        ! TRANS='N','C','T'; default: 'N'
        ! Default ALPHA=1
        ! Default BETA=0
    PURE SUBROUTINE SGEMV_F95(A,X,Y,ALPHA,BETA,TRANS)
        ! Fortran77 call:
        ! SGEMV(TRANS,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
        USE F95_PRECISION, ONLY: WP => SP
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(IN), OPTIONAL :: BETA
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(IN) :: X(:)
        REAL(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE SGEMV_F95
    PURE SUBROUTINE DGEMV_F95(A,X,Y,ALPHA,BETA,TRANS)
        ! Fortran77 call:
        ! DGEMV(TRANS,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
        USE F95_PRECISION, ONLY: WP => DP
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(IN), OPTIONAL :: BETA
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(IN) :: X(:)
        REAL(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE DGEMV_F95
    PURE SUBROUTINE CGEMV_F95(A,X,Y,ALPHA,BETA,TRANS)
        ! Fortran77 call:
        ! CGEMV(TRANS,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
        USE F95_PRECISION, ONLY: WP => SP
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(IN), OPTIONAL :: BETA
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: X(:)
        COMPLEX(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE CGEMV_F95
    PURE SUBROUTINE ZGEMV_F95(A,X,Y,ALPHA,BETA,TRANS)
        ! Fortran77 call:
        ! ZGEMV(TRANS,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
        USE F95_PRECISION, ONLY: WP => DP
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(IN), OPTIONAL :: BETA
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: X(:)
        COMPLEX(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE ZGEMV_F95
END INTERFACE GEMV

INTERFACE GER
        ! Default ALPHA=1
    PURE SUBROUTINE SGER_F95(A,X,Y,ALPHA)
        ! Fortran77 call:
        ! SGER(M,N,ALPHA,X,INCX,Y,INCY,A,LDA)
        USE F95_PRECISION, ONLY: WP => SP
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(INOUT) :: A(:,:)
        REAL(WP), INTENT(IN) :: X(:)
        REAL(WP), INTENT(IN) :: Y(:)
    END SUBROUTINE SGER_F95
    PURE SUBROUTINE DGER_F95(A,X,Y,ALPHA)
        ! Fortran77 call:
        ! DGER(M,N,ALPHA,X,INCX,Y,INCY,A,LDA)
        USE F95_PRECISION, ONLY: WP => DP
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(INOUT) :: A(:,:)
        REAL(WP), INTENT(IN) :: X(:)
        REAL(WP), INTENT(IN) :: Y(:)
    END SUBROUTINE DGER_F95
END INTERFACE GER

INTERFACE GERC
        ! Default ALPHA=1
    PURE SUBROUTINE CGERC_F95(A,X,Y,ALPHA)
        ! Fortran77 call:
        ! CGERC(M,N,ALPHA,X,INCX,Y,INCY,A,LDA)
        USE F95_PRECISION, ONLY: WP => SP
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(INOUT) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: X(:)
        COMPLEX(WP), INTENT(IN) :: Y(:)
    END SUBROUTINE CGERC_F95
    PURE SUBROUTINE ZGERC_F95(A,X,Y,ALPHA)
        ! Fortran77 call:
        ! ZGERC(M,N,ALPHA,X,INCX,Y,INCY,A,LDA)
        USE F95_PRECISION, ONLY: WP => DP
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(INOUT) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: X(:)
        COMPLEX(WP), INTENT(IN) :: Y(:)
    END SUBROUTINE ZGERC_F95
END INTERFACE GERC

INTERFACE GERU
        ! Default ALPHA=1
    PURE SUBROUTINE CGERU_F95(A,X,Y,ALPHA)
        ! Fortran77 call:
        ! CGERU(M,N,ALPHA,X,INCX,Y,INCY,A,LDA)
        USE F95_PRECISION, ONLY: WP => SP
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(INOUT) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: X(:)
        COMPLEX(WP), INTENT(IN) :: Y(:)
    END SUBROUTINE CGERU_F95
    PURE SUBROUTINE ZGERU_F95(A,X,Y,ALPHA)
        ! Fortran77 call:
        ! ZGERU(M,N,ALPHA,X,INCX,Y,INCY,A,LDA)
        USE F95_PRECISION, ONLY: WP => DP
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(INOUT) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: X(:)
        COMPLEX(WP), INTENT(IN) :: Y(:)
    END SUBROUTINE ZGERU_F95
END INTERFACE GERU

INTERFACE HBMV
        ! UPLO='U','L'; default: 'U'
        ! Default ALPHA=1
        ! Default BETA=0
    PURE SUBROUTINE CHBMV_F95(A,X,Y,UPLO,ALPHA,BETA)
        ! Fortran77 call:
        ! CHBMV(UPLO,N,K,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
        USE F95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(IN), OPTIONAL :: BETA
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: X(:)
        COMPLEX(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE CHBMV_F95
    PURE SUBROUTINE ZHBMV_F95(A,X,Y,UPLO,ALPHA,BETA)
        ! Fortran77 call:
        ! ZHBMV(UPLO,N,K,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
        USE F95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(IN), OPTIONAL :: BETA
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: X(:)
        COMPLEX(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE ZHBMV_F95
END INTERFACE HBMV

INTERFACE HEMV
        ! UPLO='U','L'; default: 'U'
        ! Default ALPHA=1
        ! Default BETA=0
    PURE SUBROUTINE CHEMV_F95(A,X,Y,UPLO,ALPHA,BETA)
        ! Fortran77 call:
        ! CHEMV(UPLO,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
        USE F95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(IN), OPTIONAL :: BETA
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: X(:)
        COMPLEX(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE CHEMV_F95
    PURE SUBROUTINE ZHEMV_F95(A,X,Y,UPLO,ALPHA,BETA)
        ! Fortran77 call:
        ! ZHEMV(UPLO,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
        USE F95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(IN), OPTIONAL :: BETA
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: X(:)
        COMPLEX(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE ZHEMV_F95
END INTERFACE HEMV

INTERFACE HER
        ! UPLO='U','L'; default: 'U'
        ! Default ALPHA=1
    PURE SUBROUTINE CHER_F95(A,X,UPLO,ALPHA)
        ! Fortran77 call:
        ! CHER(UPLO,N,ALPHA,X,INCX,A,LDA)
        USE F95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(INOUT) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: X(:)
    END SUBROUTINE CHER_F95
    PURE SUBROUTINE ZHER_F95(A,X,UPLO,ALPHA)
        ! Fortran77 call:
        ! ZHER(UPLO,N,ALPHA,X,INCX,A,LDA)
        USE F95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(INOUT) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: X(:)
    END SUBROUTINE ZHER_F95
END INTERFACE HER

INTERFACE HER2
        ! UPLO='U','L'; default: 'U'
        ! Default ALPHA=1
    PURE SUBROUTINE CHER2_F95(A,X,Y,UPLO,ALPHA)
        ! Fortran77 call:
        ! CHER2(UPLO,N,ALPHA,X,INCX,Y,INCY,A,LDA)
        USE F95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(INOUT) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: X(:)
        COMPLEX(WP), INTENT(IN) :: Y(:)
    END SUBROUTINE CHER2_F95
    PURE SUBROUTINE ZHER2_F95(A,X,Y,UPLO,ALPHA)
        ! Fortran77 call:
        ! ZHER2(UPLO,N,ALPHA,X,INCX,Y,INCY,A,LDA)
        USE F95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(INOUT) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: X(:)
        COMPLEX(WP), INTENT(IN) :: Y(:)
    END SUBROUTINE ZHER2_F95
END INTERFACE HER2

INTERFACE HPMV
        ! UPLO='U','L'; default: 'U'
        ! Default ALPHA=1
        ! Default BETA=0
    PURE SUBROUTINE CHPMV_F95(AP,X,Y,UPLO,ALPHA,BETA)
        ! Fortran77 call:
        ! CHPMV(UPLO,N,ALPHA,AP,X,INCX,BETA,Y,INCY)
        USE F95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(IN), OPTIONAL :: BETA
        COMPLEX(WP), INTENT(IN) :: AP(:)
        COMPLEX(WP), INTENT(IN) :: X(:)
        COMPLEX(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE CHPMV_F95
    PURE SUBROUTINE ZHPMV_F95(AP,X,Y,UPLO,ALPHA,BETA)
        ! Fortran77 call:
        ! ZHPMV(UPLO,N,ALPHA,AP,X,INCX,BETA,Y,INCY)
        USE F95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(IN), OPTIONAL :: BETA
        COMPLEX(WP), INTENT(IN) :: AP(:)
        COMPLEX(WP), INTENT(IN) :: X(:)
        COMPLEX(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE ZHPMV_F95
END INTERFACE HPMV

INTERFACE HPR
        ! UPLO='U','L'; default: 'U'
        ! Default ALPHA=1
    PURE SUBROUTINE CHPR_F95(AP,X,UPLO,ALPHA)
        ! Fortran77 call:
        ! CHPR(UPLO,N,ALPHA,X,INCX,AP)
        USE F95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(INOUT) :: AP(:)
        COMPLEX(WP), INTENT(IN) :: X(:)
    END SUBROUTINE CHPR_F95
    PURE SUBROUTINE ZHPR_F95(AP,X,UPLO,ALPHA)
        ! Fortran77 call:
        ! ZHPR(UPLO,N,ALPHA,X,INCX,AP)
        USE F95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(INOUT) :: AP(:)
        COMPLEX(WP), INTENT(IN) :: X(:)
    END SUBROUTINE ZHPR_F95
END INTERFACE HPR

INTERFACE HPR2
        ! UPLO='U','L'; default: 'U'
        ! Default ALPHA=1
    PURE SUBROUTINE CHPR2_F95(AP,X,Y,UPLO,ALPHA)
        ! Fortran77 call:
        ! CHPR2(UPLO,N,ALPHA,X,INCX,Y,INCY,AP)
        USE F95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(INOUT) :: AP(:)
        COMPLEX(WP), INTENT(IN) :: X(:)
        COMPLEX(WP), INTENT(IN) :: Y(:)
    END SUBROUTINE CHPR2_F95
    PURE SUBROUTINE ZHPR2_F95(AP,X,Y,UPLO,ALPHA)
        ! Fortran77 call:
        ! ZHPR2(UPLO,N,ALPHA,X,INCX,Y,INCY,AP)
        USE F95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(INOUT) :: AP(:)
        COMPLEX(WP), INTENT(IN) :: X(:)
        COMPLEX(WP), INTENT(IN) :: Y(:)
    END SUBROUTINE ZHPR2_F95
END INTERFACE HPR2

INTERFACE SBMV
        ! UPLO='U','L'; default: 'U'
        ! Default ALPHA=1
        ! Default BETA=0
    PURE SUBROUTINE SSBMV_F95(A,X,Y,UPLO,ALPHA,BETA)
        ! Fortran77 call:
        ! SSBMV(UPLO,N,K,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
        USE F95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(IN), OPTIONAL :: BETA
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(IN) :: X(:)
        REAL(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE SSBMV_F95
    PURE SUBROUTINE DSBMV_F95(A,X,Y,UPLO,ALPHA,BETA)
        ! Fortran77 call:
        ! DSBMV(UPLO,N,K,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
        USE F95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(IN), OPTIONAL :: BETA
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(IN) :: X(:)
        REAL(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE DSBMV_F95
END INTERFACE SBMV

INTERFACE SPMV
        ! UPLO='U','L'; default: 'U'
        ! Default ALPHA=1
        ! Default BETA=0
    PURE SUBROUTINE SSPMV_F95(AP,X,Y,UPLO,ALPHA,BETA)
        ! Fortran77 call:
        ! SSPMV(UPLO,N,ALPHA,AP,X,INCX,BETA,Y,INCY)
        USE F95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(IN), OPTIONAL :: BETA
        REAL(WP), INTENT(IN) :: AP(:)
        REAL(WP), INTENT(IN) :: X(:)
        REAL(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE SSPMV_F95
    PURE SUBROUTINE DSPMV_F95(AP,X,Y,UPLO,ALPHA,BETA)
        ! Fortran77 call:
        ! DSPMV(UPLO,N,ALPHA,AP,X,INCX,BETA,Y,INCY)
        USE F95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(IN), OPTIONAL :: BETA
        REAL(WP), INTENT(IN) :: AP(:)
        REAL(WP), INTENT(IN) :: X(:)
        REAL(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE DSPMV_F95
END INTERFACE SPMV

INTERFACE SPR
        ! UPLO='U','L'; default: 'U'
        ! Default ALPHA=1
    PURE SUBROUTINE SSPR_F95(AP,X,UPLO,ALPHA)
        ! Fortran77 call:
        ! SSPR(UPLO,N,ALPHA,X,INCX,AP)
        USE F95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(INOUT) :: AP(:)
        REAL(WP), INTENT(IN) :: X(:)
    END SUBROUTINE SSPR_F95
    PURE SUBROUTINE DSPR_F95(AP,X,UPLO,ALPHA)
        ! Fortran77 call:
        ! DSPR(UPLO,N,ALPHA,X,INCX,AP)
        USE F95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(INOUT) :: AP(:)
        REAL(WP), INTENT(IN) :: X(:)
    END SUBROUTINE DSPR_F95
END INTERFACE SPR

INTERFACE SPR2
        ! UPLO='U','L'; default: 'U'
        ! Default ALPHA=1
    PURE SUBROUTINE SSPR2_F95(AP,X,Y,UPLO,ALPHA)
        ! Fortran77 call:
        ! SSPR2(UPLO,N,ALPHA,X,INCX,Y,INCY,AP)
        USE F95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(INOUT) :: AP(:)
        REAL(WP), INTENT(IN) :: X(:)
        REAL(WP), INTENT(IN) :: Y(:)
    END SUBROUTINE SSPR2_F95
    PURE SUBROUTINE DSPR2_F95(AP,X,Y,UPLO,ALPHA)
        ! Fortran77 call:
        ! DSPR2(UPLO,N,ALPHA,X,INCX,Y,INCY,AP)
        USE F95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(INOUT) :: AP(:)
        REAL(WP), INTENT(IN) :: X(:)
        REAL(WP), INTENT(IN) :: Y(:)
    END SUBROUTINE DSPR2_F95
END INTERFACE SPR2

INTERFACE SYMV
        ! UPLO='U','L'; default: 'U'
        ! Default ALPHA=1
        ! Default BETA=0
    PURE SUBROUTINE SSYMV_F95(A,X,Y,UPLO,ALPHA,BETA)
        ! Fortran77 call:
        ! SSYMV(UPLO,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
        USE F95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(IN), OPTIONAL :: BETA
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(IN) :: X(:)
        REAL(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE SSYMV_F95
    PURE SUBROUTINE DSYMV_F95(A,X,Y,UPLO,ALPHA,BETA)
        ! Fortran77 call:
        ! DSYMV(UPLO,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
        USE F95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(IN), OPTIONAL :: BETA
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(IN) :: X(:)
        REAL(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE DSYMV_F95
END INTERFACE SYMV

INTERFACE SYR
        ! UPLO='U','L'; default: 'U'
        ! Default ALPHA=1
    PURE SUBROUTINE SSYR_F95(A,X,UPLO,ALPHA)
        ! Fortran77 call:
        ! SSYR(UPLO,N,ALPHA,X,INCX,A,LDA)
        USE F95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(INOUT) :: A(:,:)
        REAL(WP), INTENT(IN) :: X(:)
    END SUBROUTINE SSYR_F95
    PURE SUBROUTINE DSYR_F95(A,X,UPLO,ALPHA)
        ! Fortran77 call:
        ! DSYR(UPLO,N,ALPHA,X,INCX,A,LDA)
        USE F95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(INOUT) :: A(:,:)
        REAL(WP), INTENT(IN) :: X(:)
    END SUBROUTINE DSYR_F95
END INTERFACE SYR

INTERFACE SYR2
        ! UPLO='U','L'; default: 'U'
        ! Default ALPHA=1
    PURE SUBROUTINE SSYR2_F95(A,X,Y,UPLO,ALPHA)
        ! Fortran77 call:
        ! SSYR2(UPLO,N,ALPHA,X,INCX,Y,INCY,A,LDA)
        USE F95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(INOUT) :: A(:,:)
        REAL(WP), INTENT(IN) :: X(:)
        REAL(WP), INTENT(IN) :: Y(:)
    END SUBROUTINE SSYR2_F95
    PURE SUBROUTINE DSYR2_F95(A,X,Y,UPLO,ALPHA)
        ! Fortran77 call:
        ! DSYR2(UPLO,N,ALPHA,X,INCX,Y,INCY,A,LDA)
        USE F95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(INOUT) :: A(:,:)
        REAL(WP), INTENT(IN) :: X(:)
        REAL(WP), INTENT(IN) :: Y(:)
    END SUBROUTINE DSYR2_F95
END INTERFACE SYR2

INTERFACE TBMV
        ! UPLO='U','L'; default: 'U'
        ! TRANS='N','C','T'; default: 'N'
        ! DIAG='N','U'; default: 'N'
    PURE SUBROUTINE STBMV_F95(A,X,UPLO,TRANS,DIAG)
        ! Fortran77 call:
        ! STBMV(UPLO,TRANS,DIAG,N,K,A,LDA,X,INCX)
        USE F95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE STBMV_F95
    PURE SUBROUTINE DTBMV_F95(A,X,UPLO,TRANS,DIAG)
        ! Fortran77 call:
        ! DTBMV(UPLO,TRANS,DIAG,N,K,A,LDA,X,INCX)
        USE F95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE DTBMV_F95
    PURE SUBROUTINE CTBMV_F95(A,X,UPLO,TRANS,DIAG)
        ! Fortran77 call:
        ! CTBMV(UPLO,TRANS,DIAG,N,K,A,LDA,X,INCX)
        USE F95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE CTBMV_F95
    PURE SUBROUTINE ZTBMV_F95(A,X,UPLO,TRANS,DIAG)
        ! Fortran77 call:
        ! ZTBMV(UPLO,TRANS,DIAG,N,K,A,LDA,X,INCX)
        USE F95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE ZTBMV_F95
END INTERFACE TBMV

INTERFACE TBSV
        ! UPLO='U','L'; default: 'U'
        ! TRANS='N','C','T'; default: 'N'
        ! DIAG='N','U'; default: 'N'
    PURE SUBROUTINE STBSV_F95(A,X,UPLO,TRANS,DIAG)
        ! Fortran77 call:
        ! STBSV(UPLO,TRANS,DIAG,N,K,A,LDA,X,INCX)
        USE F95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE STBSV_F95
    PURE SUBROUTINE DTBSV_F95(A,X,UPLO,TRANS,DIAG)
        ! Fortran77 call:
        ! DTBSV(UPLO,TRANS,DIAG,N,K,A,LDA,X,INCX)
        USE F95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE DTBSV_F95
    PURE SUBROUTINE CTBSV_F95(A,X,UPLO,TRANS,DIAG)
        ! Fortran77 call:
        ! CTBSV(UPLO,TRANS,DIAG,N,K,A,LDA,X,INCX)
        USE F95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE CTBSV_F95
    PURE SUBROUTINE ZTBSV_F95(A,X,UPLO,TRANS,DIAG)
        ! Fortran77 call:
        ! ZTBSV(UPLO,TRANS,DIAG,N,K,A,LDA,X,INCX)
        USE F95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE ZTBSV_F95
END INTERFACE TBSV

INTERFACE TPMV
        ! UPLO='U','L'; default: 'U'
        ! TRANS='N','C','T'; default: 'N'
        ! DIAG='N','U'; default: 'N'
    PURE SUBROUTINE STPMV_F95(AP,X,UPLO,TRANS,DIAG)
        ! Fortran77 call:
        ! STPMV(UPLO,TRANS,DIAG,N,AP,X,INCX)
        USE F95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        REAL(WP), INTENT(IN) :: AP(:)
        REAL(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE STPMV_F95
    PURE SUBROUTINE DTPMV_F95(AP,X,UPLO,TRANS,DIAG)
        ! Fortran77 call:
        ! DTPMV(UPLO,TRANS,DIAG,N,AP,X,INCX)
        USE F95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        REAL(WP), INTENT(IN) :: AP(:)
        REAL(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE DTPMV_F95
    PURE SUBROUTINE CTPMV_F95(AP,X,UPLO,TRANS,DIAG)
        ! Fortran77 call:
        ! CTPMV(UPLO,TRANS,DIAG,N,AP,X,INCX)
        USE F95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        COMPLEX(WP), INTENT(IN) :: AP(:)
        COMPLEX(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE CTPMV_F95
    PURE SUBROUTINE ZTPMV_F95(AP,X,UPLO,TRANS,DIAG)
        ! Fortran77 call:
        ! ZTPMV(UPLO,TRANS,DIAG,N,AP,X,INCX)
        USE F95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        COMPLEX(WP), INTENT(IN) :: AP(:)
        COMPLEX(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE ZTPMV_F95
END INTERFACE TPMV

INTERFACE TPSV
        ! UPLO='U','L'; default: 'U'
        ! TRANS='N','C','T'; default: 'N'
        ! DIAG='N','U'; default: 'N'
    PURE SUBROUTINE STPSV_F95(AP,X,UPLO,TRANS,DIAG)
        ! Fortran77 call:
        ! STPSV(UPLO,TRANS,DIAG,N,AP,X,INCX)
        USE F95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        REAL(WP), INTENT(IN) :: AP(:)
        REAL(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE STPSV_F95
    PURE SUBROUTINE DTPSV_F95(AP,X,UPLO,TRANS,DIAG)
        ! Fortran77 call:
        ! DTPSV(UPLO,TRANS,DIAG,N,AP,X,INCX)
        USE F95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        REAL(WP), INTENT(IN) :: AP(:)
        REAL(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE DTPSV_F95
    PURE SUBROUTINE CTPSV_F95(AP,X,UPLO,TRANS,DIAG)
        ! Fortran77 call:
        ! CTPSV(UPLO,TRANS,DIAG,N,AP,X,INCX)
        USE F95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        COMPLEX(WP), INTENT(IN) :: AP(:)
        COMPLEX(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE CTPSV_F95
    PURE SUBROUTINE ZTPSV_F95(AP,X,UPLO,TRANS,DIAG)
        ! Fortran77 call:
        ! ZTPSV(UPLO,TRANS,DIAG,N,AP,X,INCX)
        USE F95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        COMPLEX(WP), INTENT(IN) :: AP(:)
        COMPLEX(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE ZTPSV_F95
END INTERFACE TPSV

INTERFACE TRMV
        ! UPLO='U','L'; default: 'U'
        ! TRANS='N','C','T'; default: 'N'
        ! DIAG='N','U'; default: 'N'
    PURE SUBROUTINE STRMV_F95(A,X,UPLO,TRANS,DIAG)
        ! Fortran77 call:
        ! STRMV(UPLO,TRANS,DIAG,N,A,LDA,X,INCX)
        USE F95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE STRMV_F95
    PURE SUBROUTINE DTRMV_F95(A,X,UPLO,TRANS,DIAG)
        ! Fortran77 call:
        ! DTRMV(UPLO,TRANS,DIAG,N,A,LDA,X,INCX)
        USE F95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE DTRMV_F95
    PURE SUBROUTINE CTRMV_F95(A,X,UPLO,TRANS,DIAG)
        ! Fortran77 call:
        ! CTRMV(UPLO,TRANS,DIAG,N,A,LDA,X,INCX)
        USE F95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE CTRMV_F95
    PURE SUBROUTINE ZTRMV_F95(A,X,UPLO,TRANS,DIAG)
        ! Fortran77 call:
        ! ZTRMV(UPLO,TRANS,DIAG,N,A,LDA,X,INCX)
        USE F95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE ZTRMV_F95
END INTERFACE TRMV

INTERFACE TRSV
        ! UPLO='U','L'; default: 'U'
        ! TRANS='N','C','T'; default: 'N'
        ! DIAG='N','U'; default: 'N'
    PURE SUBROUTINE STRSV_F95(A,X,UPLO,TRANS,DIAG)
        ! Fortran77 call:
        ! STRSV(UPLO,TRANS,DIAG,N,A,LDA,X,INCX)
        USE F95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE STRSV_F95
    PURE SUBROUTINE DTRSV_F95(A,X,UPLO,TRANS,DIAG)
        ! Fortran77 call:
        ! DTRSV(UPLO,TRANS,DIAG,N,A,LDA,X,INCX)
        USE F95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE DTRSV_F95
    PURE SUBROUTINE CTRSV_F95(A,X,UPLO,TRANS,DIAG)
        ! Fortran77 call:
        ! CTRSV(UPLO,TRANS,DIAG,N,A,LDA,X,INCX)
        USE F95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE CTRSV_F95
    PURE SUBROUTINE ZTRSV_F95(A,X,UPLO,TRANS,DIAG)
        ! Fortran77 call:
        ! ZTRSV(UPLO,TRANS,DIAG,N,A,LDA,X,INCX)
        USE F95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE ZTRSV_F95
END INTERFACE TRSV

INTERFACE GEMM
        ! TRANSA='N','C','T'; default: 'N'
        ! TRANSB='N','C','T'; default: 'N'
        ! Default ALPHA=1
        ! Default BETA=0
    PURE SUBROUTINE SGEMM_F95(A,B,C,TRANSA,TRANSB,ALPHA,BETA)
        ! Fortran77 call:
        ! SGEMM(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
        USE F95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANSA
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANSB
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(IN), OPTIONAL :: BETA
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(IN) :: B(:,:)
        REAL(WP), INTENT(INOUT) :: C(:,:)
    END SUBROUTINE SGEMM_F95
    PURE SUBROUTINE DGEMM_F95(A,B,C,TRANSA,TRANSB,ALPHA,BETA)
        ! Fortran77 call:
        ! DGEMM(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
        USE F95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANSA
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANSB
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(IN), OPTIONAL :: BETA
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(IN) :: B(:,:)
        REAL(WP), INTENT(INOUT) :: C(:,:)
    END SUBROUTINE DGEMM_F95
    PURE SUBROUTINE CGEMM_F95(A,B,C,TRANSA,TRANSB,ALPHA,BETA)
        ! Fortran77 call:
        ! CGEMM(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
        USE F95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANSA
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANSB
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(IN), OPTIONAL :: BETA
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: B(:,:)
        COMPLEX(WP), INTENT(INOUT) :: C(:,:)
    END SUBROUTINE CGEMM_F95
    PURE SUBROUTINE ZGEMM_F95(A,B,C,TRANSA,TRANSB,ALPHA,BETA)
        ! Fortran77 call:
        ! ZGEMM(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
        USE F95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANSA
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANSB
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(IN), OPTIONAL :: BETA
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: B(:,:)
        COMPLEX(WP), INTENT(INOUT) :: C(:,:)
    END SUBROUTINE ZGEMM_F95
    PURE SUBROUTINE SCGEMM_F95(A,B,C,TRANSA,TRANSB,ALPHA,BETA)
        ! Fortran77 call:
        ! SCGEMM(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
        USE F95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANSA
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANSB
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(IN), OPTIONAL :: BETA
        REAL(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: B(:,:)
        COMPLEX(WP), INTENT(INOUT) :: C(:,:)
    END SUBROUTINE SCGEMM_F95
    PURE SUBROUTINE DZGEMM_F95(A,B,C,TRANSA,TRANSB,ALPHA,BETA)
        ! Fortran77 call:
        ! DZGEMM(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
        USE F95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANSA
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANSB
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(IN), OPTIONAL :: BETA
        REAL(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: B(:,:)
        COMPLEX(WP), INTENT(INOUT) :: C(:,:)
    END SUBROUTINE DZGEMM_F95
END INTERFACE GEMM

INTERFACE HEMM
        ! SIDE='L','R'; default: 'L'
        ! UPLO='U','L'; default: 'U'
        ! Default ALPHA=1
        ! Default BETA=0
    PURE SUBROUTINE CHEMM_F95(A,B,C,SIDE,UPLO,ALPHA,BETA)
        ! Fortran77 call:
        ! CHEMM(SIDE,UPLO,M,N,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
        USE F95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: SIDE
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(IN), OPTIONAL :: BETA
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: B(:,:)
        COMPLEX(WP), INTENT(INOUT) :: C(:,:)
    END SUBROUTINE CHEMM_F95
    PURE SUBROUTINE ZHEMM_F95(A,B,C,SIDE,UPLO,ALPHA,BETA)
        ! Fortran77 call:
        ! ZHEMM(SIDE,UPLO,M,N,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
        USE F95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: SIDE
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(IN), OPTIONAL :: BETA
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: B(:,:)
        COMPLEX(WP), INTENT(INOUT) :: C(:,:)
    END SUBROUTINE ZHEMM_F95
END INTERFACE HEMM

INTERFACE HERK
        ! UPLO='U','L'; default: 'U'
        ! TRANS='N','C'; default: 'N'
        ! Default ALPHA=1
        ! Default BETA=0
    PURE SUBROUTINE CHERK_F95(A,C,UPLO,TRANS,ALPHA,BETA)
        ! Fortran77 call:
        ! CHERK(UPLO,TRANS,N,K,ALPHA,A,LDA,BETA,C,LDC)
        USE F95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(IN), OPTIONAL :: BETA
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(INOUT) :: C(:,:)
    END SUBROUTINE CHERK_F95
    PURE SUBROUTINE ZHERK_F95(A,C,UPLO,TRANS,ALPHA,BETA)
        ! Fortran77 call:
        ! ZHERK(UPLO,TRANS,N,K,ALPHA,A,LDA,BETA,C,LDC)
        USE F95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(IN), OPTIONAL :: BETA
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(INOUT) :: C(:,:)
    END SUBROUTINE ZHERK_F95
END INTERFACE HERK

INTERFACE HER2K
        ! UPLO='U','L'; default: 'U'
        ! TRANS='N','C'; default: 'N'
        ! Default ALPHA=1
        ! Default BETA=0
    PURE SUBROUTINE CHER2K_F95(A,B,C,UPLO,TRANS,ALPHA,BETA)
        ! Fortran77 call:
        ! CHER2K(UPLO,TRANS,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
        USE F95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(IN), OPTIONAL :: BETA
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: B(:,:)
        COMPLEX(WP), INTENT(INOUT) :: C(:,:)
    END SUBROUTINE CHER2K_F95
    PURE SUBROUTINE ZHER2K_F95(A,B,C,UPLO,TRANS,ALPHA,BETA)
        ! Fortran77 call:
        ! ZHER2K(UPLO,TRANS,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
        USE F95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(IN), OPTIONAL :: BETA
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: B(:,:)
        COMPLEX(WP), INTENT(INOUT) :: C(:,:)
    END SUBROUTINE ZHER2K_F95
END INTERFACE HER2K

INTERFACE SYMM
        ! SIDE='L','R'; default: 'L'
        ! UPLO='U','L'; default: 'U'
        ! Default ALPHA=1
        ! Default BETA=0
    PURE SUBROUTINE SSYMM_F95(A,B,C,SIDE,UPLO,ALPHA,BETA)
        ! Fortran77 call:
        ! SSYMM(SIDE,UPLO,M,N,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
        USE F95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: SIDE
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(IN), OPTIONAL :: BETA
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(IN) :: B(:,:)
        REAL(WP), INTENT(INOUT) :: C(:,:)
    END SUBROUTINE SSYMM_F95
    PURE SUBROUTINE DSYMM_F95(A,B,C,SIDE,UPLO,ALPHA,BETA)
        ! Fortran77 call:
        ! DSYMM(SIDE,UPLO,M,N,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
        USE F95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: SIDE
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(IN), OPTIONAL :: BETA
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(IN) :: B(:,:)
        REAL(WP), INTENT(INOUT) :: C(:,:)
    END SUBROUTINE DSYMM_F95
    PURE SUBROUTINE CSYMM_F95(A,B,C,SIDE,UPLO,ALPHA,BETA)
        ! Fortran77 call:
        ! CSYMM(SIDE,UPLO,M,N,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
        USE F95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: SIDE
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(IN), OPTIONAL :: BETA
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: B(:,:)
        COMPLEX(WP), INTENT(INOUT) :: C(:,:)
    END SUBROUTINE CSYMM_F95
    PURE SUBROUTINE ZSYMM_F95(A,B,C,SIDE,UPLO,ALPHA,BETA)
        ! Fortran77 call:
        ! ZSYMM(SIDE,UPLO,M,N,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
        USE F95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: SIDE
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(IN), OPTIONAL :: BETA
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: B(:,:)
        COMPLEX(WP), INTENT(INOUT) :: C(:,:)
    END SUBROUTINE ZSYMM_F95
END INTERFACE SYMM

INTERFACE SYRK
        ! UPLO='U','L'; default: 'U'
        ! TRANS='N','C','T'; default: 'N'
        ! Default ALPHA=1
        ! Default BETA=0
    PURE SUBROUTINE SSYRK_F95(A,C,UPLO,TRANS,ALPHA,BETA)
        ! Fortran77 call:
        ! SSYRK(UPLO,TRANS,N,K,ALPHA,A,LDA,BETA,C,LDC)
        USE F95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(IN), OPTIONAL :: BETA
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(INOUT) :: C(:,:)
    END SUBROUTINE SSYRK_F95
    PURE SUBROUTINE DSYRK_F95(A,C,UPLO,TRANS,ALPHA,BETA)
        ! Fortran77 call:
        ! DSYRK(UPLO,TRANS,N,K,ALPHA,A,LDA,BETA,C,LDC)
        USE F95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(IN), OPTIONAL :: BETA
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(INOUT) :: C(:,:)
    END SUBROUTINE DSYRK_F95
    PURE SUBROUTINE CSYRK_F95(A,C,UPLO,TRANS,ALPHA,BETA)
        ! Fortran77 call:
        ! CSYRK(UPLO,TRANS,N,K,ALPHA,A,LDA,BETA,C,LDC)
        USE F95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(IN), OPTIONAL :: BETA
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(INOUT) :: C(:,:)
    END SUBROUTINE CSYRK_F95
    PURE SUBROUTINE ZSYRK_F95(A,C,UPLO,TRANS,ALPHA,BETA)
        ! Fortran77 call:
        ! ZSYRK(UPLO,TRANS,N,K,ALPHA,A,LDA,BETA,C,LDC)
        USE F95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(IN), OPTIONAL :: BETA
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(INOUT) :: C(:,:)
    END SUBROUTINE ZSYRK_F95
END INTERFACE SYRK

INTERFACE SYR2K
        ! UPLO='U','L'; default: 'U'
        ! TRANS='N','C','T'; default: 'N'
        ! Default ALPHA=1
        ! Default BETA=0
    PURE SUBROUTINE SSYR2K_F95(A,B,C,UPLO,TRANS,ALPHA,BETA)
        ! Fortran77 call:
        ! SSYR2K(UPLO,TRANS,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
        USE F95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(IN), OPTIONAL :: BETA
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(IN) :: B(:,:)
        REAL(WP), INTENT(INOUT) :: C(:,:)
    END SUBROUTINE SSYR2K_F95
    PURE SUBROUTINE DSYR2K_F95(A,B,C,UPLO,TRANS,ALPHA,BETA)
        ! Fortran77 call:
        ! DSYR2K(UPLO,TRANS,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
        USE F95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(IN), OPTIONAL :: BETA
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(IN) :: B(:,:)
        REAL(WP), INTENT(INOUT) :: C(:,:)
    END SUBROUTINE DSYR2K_F95
    PURE SUBROUTINE CSYR2K_F95(A,B,C,UPLO,TRANS,ALPHA,BETA)
        ! Fortran77 call:
        ! CSYR2K(UPLO,TRANS,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
        USE F95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(IN), OPTIONAL :: BETA
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: B(:,:)
        COMPLEX(WP), INTENT(INOUT) :: C(:,:)
    END SUBROUTINE CSYR2K_F95
    PURE SUBROUTINE ZSYR2K_F95(A,B,C,UPLO,TRANS,ALPHA,BETA)
        ! Fortran77 call:
        ! ZSYR2K(UPLO,TRANS,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
        USE F95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(IN), OPTIONAL :: BETA
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: B(:,:)
        COMPLEX(WP), INTENT(INOUT) :: C(:,:)
    END SUBROUTINE ZSYR2K_F95
END INTERFACE SYR2K

INTERFACE TRMM
        ! SIDE='L','R'; default: 'L'
        ! UPLO='U','L'; default: 'U'
        ! TRANSA='N','C','T'; default: 'N'
        ! DIAG='N','U'; default: 'N'
        ! Default ALPHA=1
    PURE SUBROUTINE STRMM_F95(A,B,SIDE,UPLO,TRANSA,DIAG,ALPHA)
        ! Fortran77 call:
        ! STRMM(SIDE,UPLO,TRANSA,DIAG,M,N,ALPHA,A,LDA,B,LDB)
        USE F95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: SIDE
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANSA
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(INOUT) :: B(:,:)
    END SUBROUTINE STRMM_F95
    PURE SUBROUTINE DTRMM_F95(A,B,SIDE,UPLO,TRANSA,DIAG,ALPHA)
        ! Fortran77 call:
        ! DTRMM(SIDE,UPLO,TRANSA,DIAG,M,N,ALPHA,A,LDA,B,LDB)
        USE F95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: SIDE
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANSA
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(INOUT) :: B(:,:)
    END SUBROUTINE DTRMM_F95
    PURE SUBROUTINE CTRMM_F95(A,B,SIDE,UPLO,TRANSA,DIAG,ALPHA)
        ! Fortran77 call:
        ! CTRMM(SIDE,UPLO,TRANSA,DIAG,M,N,ALPHA,A,LDA,B,LDB)
        USE F95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: SIDE
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANSA
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(INOUT) :: B(:,:)
    END SUBROUTINE CTRMM_F95
    PURE SUBROUTINE ZTRMM_F95(A,B,SIDE,UPLO,TRANSA,DIAG,ALPHA)
        ! Fortran77 call:
        ! ZTRMM(SIDE,UPLO,TRANSA,DIAG,M,N,ALPHA,A,LDA,B,LDB)
        USE F95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: SIDE
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANSA
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(INOUT) :: B(:,:)
    END SUBROUTINE ZTRMM_F95
END INTERFACE TRMM

INTERFACE TRSM
        ! SIDE='L','R'; default: 'L'
        ! UPLO='U','L'; default: 'U'
        ! TRANSA='N','C','T'; default: 'N'
        ! DIAG='N','U'; default: 'N'
        ! Default ALPHA=1
    PURE SUBROUTINE STRSM_F95(A,B,SIDE,UPLO,TRANSA,DIAG,ALPHA)
        ! Fortran77 call:
        ! STRSM(SIDE,UPLO,TRANSA,DIAG,M,N,ALPHA,A,LDA,B,LDB)
        USE F95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: SIDE
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANSA
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(INOUT) :: B(:,:)
    END SUBROUTINE STRSM_F95
    PURE SUBROUTINE DTRSM_F95(A,B,SIDE,UPLO,TRANSA,DIAG,ALPHA)
        ! Fortran77 call:
        ! DTRSM(SIDE,UPLO,TRANSA,DIAG,M,N,ALPHA,A,LDA,B,LDB)
        USE F95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: SIDE
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANSA
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(INOUT) :: B(:,:)
    END SUBROUTINE DTRSM_F95
    PURE SUBROUTINE CTRSM_F95(A,B,SIDE,UPLO,TRANSA,DIAG,ALPHA)
        ! Fortran77 call:
        ! CTRSM(SIDE,UPLO,TRANSA,DIAG,M,N,ALPHA,A,LDA,B,LDB)
        USE F95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: SIDE
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANSA
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(INOUT) :: B(:,:)
    END SUBROUTINE CTRSM_F95
    PURE SUBROUTINE ZTRSM_F95(A,B,SIDE,UPLO,TRANSA,DIAG,ALPHA)
        ! Fortran77 call:
        ! ZTRSM(SIDE,UPLO,TRANSA,DIAG,M,N,ALPHA,A,LDA,B,LDB)
        USE F95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: SIDE
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANSA
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(INOUT) :: B(:,:)
    END SUBROUTINE ZTRSM_F95
END INTERFACE TRSM

INTERFACE AXPYI
        ! Default A=1
    PURE SUBROUTINE SAXPYI_F95(X,INDX,Y,A)
        ! Fortran77 call:
        ! SAXPYI(NZ,A,X,INDX,Y)
        USE F95_PRECISION, ONLY: WP => SP
        REAL(WP), INTENT(IN), OPTIONAL :: A
        REAL(WP), INTENT(IN) :: X(:)
        INTEGER, INTENT(IN) :: INDX(:)
        REAL(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE SAXPYI_F95
    PURE SUBROUTINE DAXPYI_F95(X,INDX,Y,A)
        ! Fortran77 call:
        ! DAXPYI(NZ,A,X,INDX,Y)
        USE F95_PRECISION, ONLY: WP => DP
        REAL(WP), INTENT(IN), OPTIONAL :: A
        REAL(WP), INTENT(IN) :: X(:)
        INTEGER, INTENT(IN) :: INDX(:)
        REAL(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE DAXPYI_F95
    PURE SUBROUTINE CAXPYI_F95(X,INDX,Y,A)
        ! Fortran77 call:
        ! CAXPYI(NZ,A,X,INDX,Y)
        USE F95_PRECISION, ONLY: WP => SP
        COMPLEX(WP), INTENT(IN), OPTIONAL :: A
        COMPLEX(WP), INTENT(IN) :: X(:)
        INTEGER, INTENT(IN) :: INDX(:)
        COMPLEX(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE CAXPYI_F95
    PURE SUBROUTINE ZAXPYI_F95(X,INDX,Y,A)
        ! Fortran77 call:
        ! ZAXPYI(NZ,A,X,INDX,Y)
        USE F95_PRECISION, ONLY: WP => DP
        COMPLEX(WP), INTENT(IN), OPTIONAL :: A
        COMPLEX(WP), INTENT(IN) :: X(:)
        INTEGER, INTENT(IN) :: INDX(:)
        COMPLEX(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE ZAXPYI_F95
END INTERFACE AXPYI

INTERFACE DOTI
    PURE FUNCTION SDOTI_F95(X,INDX,Y)
        ! Fortran77 call:
        ! SDOTI(NZ,X,INDX,Y)
        USE F95_PRECISION, ONLY: WP => SP
        REAL(WP) :: SDOTI_F95
        REAL(WP), INTENT(IN) :: X(:)
        INTEGER, INTENT(IN) :: INDX(:)
        REAL(WP), INTENT(IN) :: Y(:)
    END FUNCTION SDOTI_F95
    PURE FUNCTION DDOTI_F95(X,INDX,Y)
        ! Fortran77 call:
        ! DDOTI(NZ,X,INDX,Y)
        USE F95_PRECISION, ONLY: WP => DP
        REAL(WP) :: DDOTI_F95
        REAL(WP), INTENT(IN) :: X(:)
        INTEGER, INTENT(IN) :: INDX(:)
        REAL(WP), INTENT(IN) :: Y(:)
    END FUNCTION DDOTI_F95
END INTERFACE DOTI

INTERFACE DOTCI
    PURE FUNCTION CDOTCI_F95(X,INDX,Y)
        ! Fortran77 call:
        ! CDOTCI(NZ,X,INDX,Y)
        USE F95_PRECISION, ONLY: WP => SP
        COMPLEX(WP) :: CDOTCI_F95
        COMPLEX(WP), INTENT(IN) :: X(:)
        INTEGER, INTENT(IN) :: INDX(:)
        COMPLEX(WP), INTENT(IN) :: Y(:)
    END FUNCTION CDOTCI_F95
    PURE FUNCTION ZDOTCI_F95(X,INDX,Y)
        ! Fortran77 call:
        ! ZDOTCI(NZ,X,INDX,Y)
        USE F95_PRECISION, ONLY: WP => DP
        COMPLEX(WP) :: ZDOTCI_F95
        COMPLEX(WP), INTENT(IN) :: X(:)
        INTEGER, INTENT(IN) :: INDX(:)
        COMPLEX(WP), INTENT(IN) :: Y(:)
    END FUNCTION ZDOTCI_F95
END INTERFACE DOTCI

INTERFACE DOTUI
    PURE FUNCTION CDOTUI_F95(X,INDX,Y)
        ! Fortran77 call:
        ! CDOTUI(NZ,X,INDX,Y)
        USE F95_PRECISION, ONLY: WP => SP
        COMPLEX(WP) :: CDOTUI_F95
        COMPLEX(WP), INTENT(IN) :: X(:)
        INTEGER, INTENT(IN) :: INDX(:)
        COMPLEX(WP), INTENT(IN) :: Y(:)
    END FUNCTION CDOTUI_F95
    PURE FUNCTION ZDOTUI_F95(X,INDX,Y)
        ! Fortran77 call:
        ! ZDOTUI(NZ,X,INDX,Y)
        USE F95_PRECISION, ONLY: WP => DP
        COMPLEX(WP) :: ZDOTUI_F95
        COMPLEX(WP), INTENT(IN) :: X(:)
        INTEGER, INTENT(IN) :: INDX(:)
        COMPLEX(WP), INTENT(IN) :: Y(:)
    END FUNCTION ZDOTUI_F95
END INTERFACE DOTUI

INTERFACE GTHR
    PURE SUBROUTINE SGTHR_F95(X,INDX,Y)
        ! Fortran77 call:
        ! SGTHR(NZ,Y,X,INDX)
        USE F95_PRECISION, ONLY: WP => SP
        REAL(WP), INTENT(OUT) :: X(:)
        INTEGER, INTENT(IN) :: INDX(:)
        REAL(WP), INTENT(IN) :: Y(:)
    END SUBROUTINE SGTHR_F95
    PURE SUBROUTINE DGTHR_F95(X,INDX,Y)
        ! Fortran77 call:
        ! DGTHR(NZ,Y,X,INDX)
        USE F95_PRECISION, ONLY: WP => DP
        REAL(WP), INTENT(OUT) :: X(:)
        INTEGER, INTENT(IN) :: INDX(:)
        REAL(WP), INTENT(IN) :: Y(:)
    END SUBROUTINE DGTHR_F95
    PURE SUBROUTINE CGTHR_F95(X,INDX,Y)
        ! Fortran77 call:
        ! CGTHR(NZ,Y,X,INDX)
        USE F95_PRECISION, ONLY: WP => SP
        COMPLEX(WP), INTENT(OUT) :: X(:)
        INTEGER, INTENT(IN) :: INDX(:)
        COMPLEX(WP), INTENT(IN) :: Y(:)
    END SUBROUTINE CGTHR_F95
    PURE SUBROUTINE ZGTHR_F95(X,INDX,Y)
        ! Fortran77 call:
        ! ZGTHR(NZ,Y,X,INDX)
        USE F95_PRECISION, ONLY: WP => DP
        COMPLEX(WP), INTENT(OUT) :: X(:)
        INTEGER, INTENT(IN) :: INDX(:)
        COMPLEX(WP), INTENT(IN) :: Y(:)
    END SUBROUTINE ZGTHR_F95
END INTERFACE GTHR

INTERFACE GTHRZ
    PURE SUBROUTINE SGTHRZ_F95(X,INDX,Y)
        ! Fortran77 call:
        ! SGTHRZ(NZ,Y,X,INDX)
        USE F95_PRECISION, ONLY: WP => SP
        REAL(WP), INTENT(OUT) :: X(:)
        INTEGER, INTENT(IN) :: INDX(:)
        REAL(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE SGTHRZ_F95
    PURE SUBROUTINE DGTHRZ_F95(X,INDX,Y)
        ! Fortran77 call:
        ! DGTHRZ(NZ,Y,X,INDX)
        USE F95_PRECISION, ONLY: WP => DP
        REAL(WP), INTENT(OUT) :: X(:)
        INTEGER, INTENT(IN) :: INDX(:)
        REAL(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE DGTHRZ_F95
    PURE SUBROUTINE CGTHRZ_F95(X,INDX,Y)
        ! Fortran77 call:
        ! CGTHRZ(NZ,Y,X,INDX)
        USE F95_PRECISION, ONLY: WP => SP
        COMPLEX(WP), INTENT(OUT) :: X(:)
        INTEGER, INTENT(IN) :: INDX(:)
        COMPLEX(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE CGTHRZ_F95
    PURE SUBROUTINE ZGTHRZ_F95(X,INDX,Y)
        ! Fortran77 call:
        ! ZGTHRZ(NZ,Y,X,INDX)
        USE F95_PRECISION, ONLY: WP => DP
        COMPLEX(WP), INTENT(OUT) :: X(:)
        INTEGER, INTENT(IN) :: INDX(:)
        COMPLEX(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE ZGTHRZ_F95
END INTERFACE GTHRZ

INTERFACE ROTI
        ! Default C=1
        ! Default S=1
    PURE SUBROUTINE SROTI_F95(X,INDX,Y,C,S)
        ! Fortran77 call:
        ! SROTI(NZ,X,INDX,Y,C,S)
        USE F95_PRECISION, ONLY: WP => SP
        REAL(WP), INTENT(IN) :: C
        REAL(WP), INTENT(IN) :: S
        REAL(WP), INTENT(INOUT) :: X(:)
        INTEGER, INTENT(IN) :: INDX(:)
        REAL(WP), INTENT(IN) :: Y(:)
    END SUBROUTINE SROTI_F95
    PURE SUBROUTINE DROTI_F95(X,INDX,Y,C,S)
        ! Fortran77 call:
        ! DROTI(NZ,X,INDX,Y,C,S)
        USE F95_PRECISION, ONLY: WP => DP
        REAL(WP), INTENT(IN) :: C
        REAL(WP), INTENT(IN) :: S
        REAL(WP), INTENT(INOUT) :: X(:)
        INTEGER, INTENT(IN) :: INDX(:)
        REAL(WP), INTENT(IN) :: Y(:)
    END SUBROUTINE DROTI_F95
END INTERFACE ROTI

INTERFACE SCTR
    PURE SUBROUTINE SSCTR_F95(X,INDX,Y)
        ! Fortran77 call:
        ! SSCTR(NZ,X,INDX,Y)
        USE F95_PRECISION, ONLY: WP => SP
        REAL(WP), INTENT(IN) :: X(:)
        INTEGER, INTENT(IN) :: INDX(:)
        REAL(WP), INTENT(OUT) :: Y(:)
    END SUBROUTINE SSCTR_F95
    PURE SUBROUTINE DSCTR_F95(X,INDX,Y)
        ! Fortran77 call:
        ! DSCTR(NZ,X,INDX,Y)
        USE F95_PRECISION, ONLY: WP => DP
        REAL(WP), INTENT(IN) :: X(:)
        INTEGER, INTENT(IN) :: INDX(:)
        REAL(WP), INTENT(OUT) :: Y(:)
    END SUBROUTINE DSCTR_F95
    PURE SUBROUTINE CSCTR_F95(X,INDX,Y)
        ! Fortran77 call:
        ! CSCTR(NZ,X,INDX,Y)
        USE F95_PRECISION, ONLY: WP => SP
        COMPLEX(WP), INTENT(IN) :: X(:)
        INTEGER, INTENT(IN) :: INDX(:)
        COMPLEX(WP), INTENT(OUT) :: Y(:)
    END SUBROUTINE CSCTR_F95
    PURE SUBROUTINE ZSCTR_F95(X,INDX,Y)
        ! Fortran77 call:
        ! ZSCTR(NZ,X,INDX,Y)
        USE F95_PRECISION, ONLY: WP => DP
        COMPLEX(WP), INTENT(IN) :: X(:)
        INTEGER, INTENT(IN) :: INDX(:)
        COMPLEX(WP), INTENT(OUT) :: Y(:)
    END SUBROUTINE ZSCTR_F95
END INTERFACE SCTR

INTERFACE GEMM3M
        ! TRANSA='N','C','T'; default: 'N'
        ! TRANSB='N','C','T'; default: 'N'
        ! Default ALPHA=1
        ! Default BETA=0
    PURE SUBROUTINE CGEMM3M_F95(A,B,C,TRANSA,TRANSB,ALPHA,BETA)
        ! Fortran77 call:
        ! CGEMM3M(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
        USE F95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANSA
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANSB
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(IN), OPTIONAL :: BETA
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: B(:,:)
        COMPLEX(WP), INTENT(INOUT) :: C(:,:)
    END SUBROUTINE CGEMM3M_F95
    PURE SUBROUTINE ZGEMM3M_F95(A,B,C,TRANSA,TRANSB,ALPHA,BETA)
        ! Fortran77 call:
        ! ZGEMM3M(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
        USE F95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANSA
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANSB
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(IN), OPTIONAL :: BETA
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: B(:,:)
        COMPLEX(WP), INTENT(INOUT) :: C(:,:)
    END SUBROUTINE ZGEMM3M_F95
END INTERFACE GEMM3M

END MODULE BLAS95

MODULE MKL95_PRECISION
    INTEGER, PARAMETER :: SP = KIND(1.0E0)
    INTEGER, PARAMETER :: DP = KIND(1.0D0)
END MODULE MKL95_PRECISION

MODULE MKL95_BLAS
INTERFACE ASUM
    PURE FUNCTION SASUM_MKL95(X)
        ! MKL Fortran77 call:
        ! SASUM(N,X,INCX)
        USE MKL95_PRECISION, ONLY: WP => SP
        REAL(WP) :: SASUM_MKL95
        REAL(WP), INTENT(IN) :: X(:)
    END FUNCTION SASUM_MKL95
    PURE FUNCTION SCASUM_MKL95(X)
        ! MKL Fortran77 call:
        ! SCASUM(N,X,INCX)
        USE MKL95_PRECISION, ONLY: WP => SP
        REAL(WP) :: SCASUM_MKL95
        COMPLEX(WP), INTENT(IN) :: X(:)
    END FUNCTION SCASUM_MKL95
    PURE FUNCTION DASUM_MKL95(X)
        ! MKL Fortran77 call:
        ! DASUM(N,X,INCX)
        USE MKL95_PRECISION, ONLY: WP => DP
        REAL(WP) :: DASUM_MKL95
        REAL(WP), INTENT(IN) :: X(:)
    END FUNCTION DASUM_MKL95
    PURE FUNCTION DZASUM_MKL95(X)
        ! MKL Fortran77 call:
        ! DZASUM(N,X,INCX)
        USE MKL95_PRECISION, ONLY: WP => DP
        REAL(WP) :: DZASUM_MKL95
        COMPLEX(WP), INTENT(IN) :: X(:)
    END FUNCTION DZASUM_MKL95
END INTERFACE ASUM
INTERFACE AXPY
    PURE SUBROUTINE SAXPY_MKL95(X,Y,A)
        ! MKL Fortran77 call:
        ! SAXPY(N,A,X,INCX,Y,INCY)
        USE MKL95_PRECISION, ONLY: WP => SP
        REAL(WP), INTENT(IN), OPTIONAL :: A
        REAL(WP), INTENT(IN) :: X(:)
        REAL(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE SAXPY_MKL95
    PURE SUBROUTINE DAXPY_MKL95(X,Y,A)
        ! MKL Fortran77 call:
        ! DAXPY(N,A,X,INCX,Y,INCY)
        USE MKL95_PRECISION, ONLY: WP => DP
        REAL(WP), INTENT(IN), OPTIONAL :: A
        REAL(WP), INTENT(IN) :: X(:)
        REAL(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE DAXPY_MKL95
    PURE SUBROUTINE CAXPY_MKL95(X,Y,A)
        ! MKL Fortran77 call:
        ! CAXPY(N,A,X,INCX,Y,INCY)
        USE MKL95_PRECISION, ONLY: WP => SP
        COMPLEX(WP), INTENT(IN), OPTIONAL :: A
        COMPLEX(WP), INTENT(IN) :: X(:)
        COMPLEX(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE CAXPY_MKL95
    PURE SUBROUTINE ZAXPY_MKL95(X,Y,A)
        ! MKL Fortran77 call:
        ! ZAXPY(N,A,X,INCX,Y,INCY)
        USE MKL95_PRECISION, ONLY: WP => DP
        COMPLEX(WP), INTENT(IN), OPTIONAL :: A
        COMPLEX(WP), INTENT(IN) :: X(:)
        COMPLEX(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE ZAXPY_MKL95
END INTERFACE AXPY
INTERFACE COPY
    PURE SUBROUTINE SCOPY_MKL95(X,Y)
        ! MKL Fortran77 call:
        ! SCOPY(N,X,INCX,Y,INCY)
        USE MKL95_PRECISION, ONLY: WP => SP
        REAL(WP), INTENT(IN) :: X(:)
        REAL(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE SCOPY_MKL95
    PURE SUBROUTINE DCOPY_MKL95(X,Y)
        ! MKL Fortran77 call:
        ! DCOPY(N,X,INCX,Y,INCY)
        USE MKL95_PRECISION, ONLY: WP => DP
        REAL(WP), INTENT(IN) :: X(:)
        REAL(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE DCOPY_MKL95
    PURE SUBROUTINE CCOPY_MKL95(X,Y)
        ! MKL Fortran77 call:
        ! CCOPY(N,X,INCX,Y,INCY)
        USE MKL95_PRECISION, ONLY: WP => SP
        COMPLEX(WP), INTENT(IN) :: X(:)
        COMPLEX(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE CCOPY_MKL95
    PURE SUBROUTINE ZCOPY_MKL95(X,Y)
        ! MKL Fortran77 call:
        ! ZCOPY(N,X,INCX,Y,INCY)
        USE MKL95_PRECISION, ONLY: WP => DP
        COMPLEX(WP), INTENT(IN) :: X(:)
        COMPLEX(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE ZCOPY_MKL95
END INTERFACE COPY
INTERFACE DOT
    PURE FUNCTION SDOT_MKL95(X,Y)
        ! MKL Fortran77 call:
        ! SDOT(N,X,INCX,Y,INCY)
        USE MKL95_PRECISION, ONLY: WP => SP
        REAL(WP) :: SDOT_MKL95
        REAL(WP), INTENT(IN) :: X(:)
        REAL(WP), INTENT(IN) :: Y(:)
    END FUNCTION SDOT_MKL95
    PURE FUNCTION DDOT_MKL95(X,Y)
        ! MKL Fortran77 call:
        ! DDOT(N,X,INCX,Y,INCY)
        USE MKL95_PRECISION, ONLY: WP => DP
        REAL(WP) :: DDOT_MKL95
        REAL(WP), INTENT(IN) :: X(:)
        REAL(WP), INTENT(IN) :: Y(:)
    END FUNCTION DDOT_MKL95
END INTERFACE DOT
INTERFACE SDOT
    PURE FUNCTION SDSDOT_MKL95(SX,SY,SB)
        ! MKL Fortran77 call:
        ! SDSDOT(N,SB,SX,INCX,SY,INCY)
        USE MKL95_PRECISION, ONLY: WP => SP
        REAL(WP) :: SDSDOT_MKL95
        REAL(WP), INTENT(IN) :: SB
        REAL(WP), INTENT(IN) :: SX(:)
        REAL(WP), INTENT(IN) :: SY(:)
    END FUNCTION SDSDOT_MKL95
    PURE FUNCTION DSDOT_MKL95(SX,SY)
        ! MKL Fortran77 call:
        ! DSDOT(N,SX,INCX,SY,INCY)
        USE MKL95_PRECISION, ONLY: WP => DP, SP
        REAL(WP) :: DSDOT_MKL95
        REAL(SP), INTENT(IN) :: SX(:)
        REAL(SP), INTENT(IN) :: SY(:)
    END FUNCTION DSDOT_MKL95
END INTERFACE SDOT
INTERFACE DOTC
    PURE FUNCTION CDOTC_MKL95(X,Y)
        ! MKL Fortran77 call:
        ! CDOTC(N,X,INCX,Y,INCY)
        USE MKL95_PRECISION, ONLY: WP => SP
        COMPLEX(WP) :: CDOTC_MKL95
        COMPLEX(WP), INTENT(IN) :: X(:)
        COMPLEX(WP), INTENT(IN) :: Y(:)
    END FUNCTION CDOTC_MKL95
    PURE FUNCTION ZDOTC_MKL95(X,Y)
        ! MKL Fortran77 call:
        ! ZDOTC(N,X,INCX,Y,INCY)
        USE MKL95_PRECISION, ONLY: WP => DP
        COMPLEX(WP) :: ZDOTC_MKL95
        COMPLEX(WP), INTENT(IN) :: X(:)
        COMPLEX(WP), INTENT(IN) :: Y(:)
    END FUNCTION ZDOTC_MKL95
END INTERFACE DOTC
INTERFACE DOTU
    PURE FUNCTION CDOTU_MKL95(X,Y)
        ! MKL Fortran77 call:
        ! CDOTU(N,X,INCX,Y,INCY)
        USE MKL95_PRECISION, ONLY: WP => SP
        COMPLEX(WP) :: CDOTU_MKL95
        COMPLEX(WP), INTENT(IN) :: X(:)
        COMPLEX(WP), INTENT(IN) :: Y(:)
    END FUNCTION CDOTU_MKL95
    PURE FUNCTION ZDOTU_MKL95(X,Y)
        ! MKL Fortran77 call:
        ! ZDOTU(N,X,INCX,Y,INCY)
        USE MKL95_PRECISION, ONLY: WP => DP
        COMPLEX(WP) :: ZDOTU_MKL95
        COMPLEX(WP), INTENT(IN) :: X(:)
        COMPLEX(WP), INTENT(IN) :: Y(:)
    END FUNCTION ZDOTU_MKL95
END INTERFACE DOTU
INTERFACE NRM2
    PURE FUNCTION SNRM2_MKL95(X)
        ! MKL Fortran77 call:
        ! SNRM2(N,X,INCX)
        USE MKL95_PRECISION, ONLY: WP => SP
        REAL(WP) :: SNRM2_MKL95
        REAL(WP), INTENT(IN) :: X(:)
    END FUNCTION SNRM2_MKL95
    PURE FUNCTION DNRM2_MKL95(X)
        ! MKL Fortran77 call:
        ! DNRM2(N,X,INCX)
        USE MKL95_PRECISION, ONLY: WP => DP
        REAL(WP) :: DNRM2_MKL95
        REAL(WP), INTENT(IN) :: X(:)
    END FUNCTION DNRM2_MKL95
    PURE FUNCTION SCNRM2_MKL95(X)
        ! MKL Fortran77 call:
        ! SCNRM2(N,X,INCX)
        USE MKL95_PRECISION, ONLY: WP => SP
        REAL(WP) :: SCNRM2_MKL95
        COMPLEX(WP), INTENT(IN) :: X(:)
    END FUNCTION SCNRM2_MKL95
    PURE FUNCTION DZNRM2_MKL95(X)
        ! MKL Fortran77 call:
        ! DZNRM2(N,X,INCX)
        USE MKL95_PRECISION, ONLY: WP => DP
        REAL(WP) :: DZNRM2_MKL95
        COMPLEX(WP), INTENT(IN) :: X(:)
    END FUNCTION DZNRM2_MKL95
END INTERFACE NRM2
INTERFACE ROT
    PURE SUBROUTINE SROT_MKL95(X,Y,C,S)
        ! MKL Fortran77 call:
        ! SROT(N,X,INCX,Y,INCY,C,S)
        USE MKL95_PRECISION, ONLY: WP => SP
        REAL(WP), INTENT(IN) :: C
        REAL(WP), INTENT(IN) :: S
        REAL(WP), INTENT(INOUT) :: X(:)
        REAL(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE SROT_MKL95
    PURE SUBROUTINE DROT_MKL95(X,Y,C,S)
        ! MKL Fortran77 call:
        ! DROT(N,X,INCX,Y,INCY,C,S)
        USE MKL95_PRECISION, ONLY: WP => DP
        REAL(WP), INTENT(IN) :: C
        REAL(WP), INTENT(IN) :: S
        REAL(WP), INTENT(INOUT) :: X(:)
        REAL(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE DROT_MKL95
    PURE SUBROUTINE CSROT_MKL95(X,Y,C,S)
        ! MKL Fortran77 call:
        ! CSROT(N,X,INCX,Y,INCY,C,S)
        USE MKL95_PRECISION, ONLY: WP => SP
        REAL(WP), INTENT(IN) :: C
        REAL(WP), INTENT(IN) :: S
        COMPLEX(WP), INTENT(INOUT) :: X(:)
        COMPLEX(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE CSROT_MKL95
    PURE SUBROUTINE ZDROT_MKL95(X,Y,C,S)
        ! MKL Fortran77 call:
        ! ZDROT(N,X,INCX,Y,INCY,C,S)
        USE MKL95_PRECISION, ONLY: WP => DP
        REAL(WP), INTENT(IN) :: C
        REAL(WP), INTENT(IN) :: S
        COMPLEX(WP), INTENT(INOUT) :: X(:)
        COMPLEX(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE ZDROT_MKL95
END INTERFACE ROT
INTERFACE ROTG
    PURE SUBROUTINE SROTG(A,B,C,S)
        ! MKL Fortran77 call:
        ! SROTG(A,B,C,S)
        USE MKL95_PRECISION, ONLY: WP => SP
        REAL(WP), INTENT(INOUT) :: A
        REAL(WP), INTENT(INOUT) :: B
        REAL(WP), INTENT(OUT) :: C
        REAL(WP), INTENT(OUT) :: S
    END SUBROUTINE SROTG
    PURE SUBROUTINE DROTG(A,B,C,S)
        ! MKL Fortran77 call:
        ! DROTG(A,B,C,S)
        USE MKL95_PRECISION, ONLY: WP => DP
        REAL(WP), INTENT(INOUT) :: A
        REAL(WP), INTENT(INOUT) :: B
        REAL(WP), INTENT(OUT) :: C
        REAL(WP), INTENT(OUT) :: S
    END SUBROUTINE DROTG
    PURE SUBROUTINE CROTG(A,B,C,S)
        ! MKL Fortran77 call:
        ! CROTG(A,B,C,S)
        USE MKL95_PRECISION, ONLY: WP => SP
        COMPLEX(WP), INTENT(INOUT) :: A
        COMPLEX(WP), INTENT(INOUT) :: B
        REAL(WP), INTENT(OUT) :: C
        COMPLEX(WP), INTENT(OUT) :: S
    END SUBROUTINE CROTG
    PURE SUBROUTINE ZROTG(A,B,C,S)
        ! MKL Fortran77 call:
        ! ZROTG(A,B,C,S)
        USE MKL95_PRECISION, ONLY: WP => DP
        COMPLEX(WP), INTENT(INOUT) :: A
        COMPLEX(WP), INTENT(INOUT) :: B
        REAL(WP), INTENT(OUT) :: C
        COMPLEX(WP), INTENT(OUT) :: S
    END SUBROUTINE ZROTG
END INTERFACE ROTG
INTERFACE ROTM
    PURE SUBROUTINE SROTM_MKL95(X,Y,PARAM)
        ! MKL Fortran77 call:
        ! SROTM(N,X,INCX,Y,INCY,PARAM)
        USE MKL95_PRECISION, ONLY: WP => SP
        REAL(WP), INTENT(INOUT) :: X(:)
        REAL(WP), INTENT(INOUT) :: Y(:)
        REAL(WP), INTENT(IN) :: PARAM(5)
    END SUBROUTINE SROTM_MKL95
    PURE SUBROUTINE DROTM_MKL95(X,Y,PARAM)
        ! MKL Fortran77 call:
        ! DROTM(N,X,INCX,Y,INCY,PARAM)
        USE MKL95_PRECISION, ONLY: WP => DP
        REAL(WP), INTENT(INOUT) :: X(:)
        REAL(WP), INTENT(INOUT) :: Y(:)
        REAL(WP), INTENT(IN) :: PARAM(5)
    END SUBROUTINE DROTM_MKL95
END INTERFACE ROTM
INTERFACE ROTMG
    PURE SUBROUTINE SROTMG_MKL95(D1,D2,X1,Y1,PARAM)
        ! MKL Fortran77 call:
        ! SROTMG(D1,D2,X1,Y1,PARAM)
        USE MKL95_PRECISION, ONLY: WP => SP
        REAL(WP), INTENT(INOUT) :: D1
        REAL(WP), INTENT(INOUT) :: D2
        REAL(WP), INTENT(INOUT) :: X1
        REAL(WP), INTENT(IN) :: Y1
        REAL(WP), INTENT(OUT) :: PARAM(5)
    END SUBROUTINE SROTMG_MKL95
    PURE SUBROUTINE DROTMG_MKL95(D1,D2,X1,Y1,PARAM)
        ! MKL Fortran77 call:
        ! DROTMG(D1,D2,X1,Y1,PARAM)
        USE MKL95_PRECISION, ONLY: WP => DP
        REAL(WP), INTENT(INOUT) :: D1
        REAL(WP), INTENT(INOUT) :: D2
        REAL(WP), INTENT(INOUT) :: X1
        REAL(WP), INTENT(IN) :: Y1
        REAL(WP), INTENT(OUT) :: PARAM(5)
    END SUBROUTINE DROTMG_MKL95
END INTERFACE ROTMG
INTERFACE SCAL
    PURE SUBROUTINE SSCAL_MKL95(X,A)
        ! MKL Fortran77 call:
        ! SSCAL(N,A,X,INCX)
        USE MKL95_PRECISION, ONLY: WP => SP
        REAL(WP), INTENT(IN) :: A
        REAL(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE SSCAL_MKL95
    PURE SUBROUTINE DSCAL_MKL95(X,A)
        ! MKL Fortran77 call:
        ! DSCAL(N,A,X,INCX)
        USE MKL95_PRECISION, ONLY: WP => DP
        REAL(WP), INTENT(IN) :: A
        REAL(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE DSCAL_MKL95
    PURE SUBROUTINE CSCAL_MKL95(X,A)
        ! MKL Fortran77 call:
        ! CSCAL(N,A,X,INCX)
        USE MKL95_PRECISION, ONLY: WP => SP
        COMPLEX(WP), INTENT(IN) :: A
        COMPLEX(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE CSCAL_MKL95
    PURE SUBROUTINE ZSCAL_MKL95(X,A)
        ! MKL Fortran77 call:
        ! ZSCAL(N,A,X,INCX)
        USE MKL95_PRECISION, ONLY: WP => DP
        COMPLEX(WP), INTENT(IN) :: A
        COMPLEX(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE ZSCAL_MKL95
    PURE SUBROUTINE CSSCAL_MKL95(X,A)
        ! MKL Fortran77 call:
        ! CSSCAL(N,A,X,INCX)
        USE MKL95_PRECISION, ONLY: WP => SP
        REAL(WP), INTENT(IN) :: A
        COMPLEX(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE CSSCAL_MKL95
    PURE SUBROUTINE ZDSCAL_MKL95(X,A)
        ! MKL Fortran77 call:
        ! ZDSCAL(N,A,X,INCX)
        USE MKL95_PRECISION, ONLY: WP => DP
        REAL(WP), INTENT(IN) :: A
        COMPLEX(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE ZDSCAL_MKL95
END INTERFACE SCAL
INTERFACE SWAP
    PURE SUBROUTINE SSWAP_MKL95(X,Y)
        ! MKL Fortran77 call:
        ! SSWAP(N,X,INCX,Y,INCY)
        USE MKL95_PRECISION, ONLY: WP => SP
        REAL(WP), INTENT(INOUT) :: X(:)
        REAL(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE SSWAP_MKL95
    PURE SUBROUTINE DSWAP_MKL95(X,Y)
        ! MKL Fortran77 call:
        ! DSWAP(N,X,INCX,Y,INCY)
        USE MKL95_PRECISION, ONLY: WP => DP
        REAL(WP), INTENT(INOUT) :: X(:)
        REAL(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE DSWAP_MKL95
    PURE SUBROUTINE CSWAP_MKL95(X,Y)
        ! MKL Fortran77 call:
        ! CSWAP(N,X,INCX,Y,INCY)
        USE MKL95_PRECISION, ONLY: WP => SP
        COMPLEX(WP), INTENT(INOUT) :: X(:)
        COMPLEX(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE CSWAP_MKL95
    PURE SUBROUTINE ZSWAP_MKL95(X,Y)
        ! MKL Fortran77 call:
        ! ZSWAP(N,X,INCX,Y,INCY)
        USE MKL95_PRECISION, ONLY: WP => DP
        COMPLEX(WP), INTENT(INOUT) :: X(:)
        COMPLEX(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE ZSWAP_MKL95
END INTERFACE SWAP
INTERFACE IAMAX
    PURE FUNCTION ISAMAX_MKL95(X)
        ! MKL Fortran77 call:
        ! ISAMAX(N,X,INCX)
        USE MKL95_PRECISION, ONLY: WP => SP
        INTEGER :: ISAMAX_MKL95
        REAL(WP), INTENT(IN) :: X(:)
    END FUNCTION ISAMAX_MKL95
    PURE FUNCTION IDAMAX_MKL95(X)
        ! MKL Fortran77 call:
        ! IDAMAX(N,X,INCX)
        USE MKL95_PRECISION, ONLY: WP => DP
        INTEGER :: IDAMAX_MKL95
        REAL(WP), INTENT(IN) :: X(:)
    END FUNCTION IDAMAX_MKL95
    PURE FUNCTION ICAMAX_MKL95(X)
        ! MKL Fortran77 call:
        ! ICAMAX(N,X,INCX)
        USE MKL95_PRECISION, ONLY: WP => SP
        INTEGER :: ICAMAX_MKL95
        COMPLEX(WP), INTENT(IN) :: X(:)
    END FUNCTION ICAMAX_MKL95
    PURE FUNCTION IZAMAX_MKL95(X)
        ! MKL Fortran77 call:
        ! IZAMAX(N,X,INCX)
        USE MKL95_PRECISION, ONLY: WP => DP
        INTEGER :: IZAMAX_MKL95
        COMPLEX(WP), INTENT(IN) :: X(:)
    END FUNCTION IZAMAX_MKL95
END INTERFACE IAMAX
INTERFACE IAMIN
    PURE FUNCTION ISAMIN_MKL95(X)
        ! MKL Fortran77 call:
        ! ISAMIN(N,X,INCX)
        USE MKL95_PRECISION, ONLY: WP => SP
        INTEGER :: ISAMIN_MKL95
        REAL(WP), INTENT(IN) :: X(:)
    END FUNCTION ISAMIN_MKL95
    PURE FUNCTION IDAMIN_MKL95(X)
        ! MKL Fortran77 call:
        ! IDAMIN(N,X,INCX)
        USE MKL95_PRECISION, ONLY: WP => DP
        INTEGER :: IDAMIN_MKL95
        REAL(WP), INTENT(IN) :: X(:)
    END FUNCTION IDAMIN_MKL95
    PURE FUNCTION ICAMIN_MKL95(X)
        ! MKL Fortran77 call:
        ! ICAMIN(N,X,INCX)
        USE MKL95_PRECISION, ONLY: WP => SP
        INTEGER :: ICAMIN_MKL95
        COMPLEX(WP), INTENT(IN) :: X(:)
    END FUNCTION ICAMIN_MKL95
    PURE FUNCTION IZAMIN_MKL95(X)
        ! MKL Fortran77 call:
        ! IZAMIN(N,X,INCX)
        USE MKL95_PRECISION, ONLY: WP => DP
        INTEGER :: IZAMIN_MKL95
        COMPLEX(WP), INTENT(IN) :: X(:)
    END FUNCTION IZAMIN_MKL95
END INTERFACE IAMIN
INTERFACE DCABS1
    PURE FUNCTION DCABS1(Z)
        ! MKL Fortran77 call:
        ! DCABS1(Z)
        USE MKL95_PRECISION, ONLY: WP => DP
        REAL(WP) :: DCABS1
        COMPLEX(WP), INTENT(IN) :: Z
    END FUNCTION DCABS1
END INTERFACE DCABS1
INTERFACE GBMV
    PURE SUBROUTINE SGBMV_MKL95(A,X,Y,KL,M,ALPHA,BETA,TRANS)
        ! MKL Fortran77 call:
        ! SGBMV(TRANS,M,N,KL,KU,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
        USE MKL95_PRECISION, ONLY: WP => SP
        INTEGER, INTENT(IN), OPTIONAL :: KL
        INTEGER, INTENT(IN), OPTIONAL :: M
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(IN), OPTIONAL :: BETA
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(IN) :: X(:)
        REAL(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE SGBMV_MKL95
    PURE SUBROUTINE DGBMV_MKL95(A,X,Y,KL,M,ALPHA,BETA,TRANS)
        ! MKL Fortran77 call:
        ! DGBMV(TRANS,M,N,KL,KU,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
        USE MKL95_PRECISION, ONLY: WP => DP
        INTEGER, INTENT(IN), OPTIONAL :: KL
        INTEGER, INTENT(IN), OPTIONAL :: M
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(IN), OPTIONAL :: BETA
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(IN) :: X(:)
        REAL(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE DGBMV_MKL95
    PURE SUBROUTINE CGBMV_MKL95(A,X,Y,KL,M,ALPHA,BETA,TRANS)
        ! MKL Fortran77 call:
        ! CGBMV(TRANS,M,N,KL,KU,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
        USE MKL95_PRECISION, ONLY: WP => SP
        INTEGER, INTENT(IN), OPTIONAL :: KL
        INTEGER, INTENT(IN), OPTIONAL :: M
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(IN), OPTIONAL :: BETA
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: X(:)
        COMPLEX(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE CGBMV_MKL95
    PURE SUBROUTINE ZGBMV_MKL95(A,X,Y,KL,M,ALPHA,BETA,TRANS)
        ! MKL Fortran77 call:
        ! ZGBMV(TRANS,M,N,KL,KU,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
        USE MKL95_PRECISION, ONLY: WP => DP
        INTEGER, INTENT(IN), OPTIONAL :: KL
        INTEGER, INTENT(IN), OPTIONAL :: M
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(IN), OPTIONAL :: BETA
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: X(:)
        COMPLEX(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE ZGBMV_MKL95
END INTERFACE GBMV
INTERFACE GEMV
    PURE SUBROUTINE SGEMV_MKL95(A,X,Y,ALPHA,BETA,TRANS)
        ! MKL Fortran77 call:
        ! SGEMV(TRANS,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
        USE MKL95_PRECISION, ONLY: WP => SP
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(IN), OPTIONAL :: BETA
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(IN) :: X(:)
        REAL(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE SGEMV_MKL95
    PURE SUBROUTINE DGEMV_MKL95(A,X,Y,ALPHA,BETA,TRANS)
        ! MKL Fortran77 call:
        ! DGEMV(TRANS,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
        USE MKL95_PRECISION, ONLY: WP => DP
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(IN), OPTIONAL :: BETA
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(IN) :: X(:)
        REAL(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE DGEMV_MKL95
    PURE SUBROUTINE CGEMV_MKL95(A,X,Y,ALPHA,BETA,TRANS)
        ! MKL Fortran77 call:
        ! CGEMV(TRANS,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
        USE MKL95_PRECISION, ONLY: WP => SP
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(IN), OPTIONAL :: BETA
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: X(:)
        COMPLEX(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE CGEMV_MKL95
    PURE SUBROUTINE ZGEMV_MKL95(A,X,Y,ALPHA,BETA,TRANS)
        ! MKL Fortran77 call:
        ! ZGEMV(TRANS,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
        USE MKL95_PRECISION, ONLY: WP => DP
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(IN), OPTIONAL :: BETA
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: X(:)
        COMPLEX(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE ZGEMV_MKL95
END INTERFACE GEMV
INTERFACE GER
    PURE SUBROUTINE SGER_MKL95(A,X,Y,ALPHA)
        ! MKL Fortran77 call:
        ! SGER(M,N,ALPHA,X,INCX,Y,INCY,A,LDA)
        USE MKL95_PRECISION, ONLY: WP => SP
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(INOUT) :: A(:,:)
        REAL(WP), INTENT(IN) :: X(:)
        REAL(WP), INTENT(IN) :: Y(:)
    END SUBROUTINE SGER_MKL95
    PURE SUBROUTINE DGER_MKL95(A,X,Y,ALPHA)
        ! MKL Fortran77 call:
        ! DGER(M,N,ALPHA,X,INCX,Y,INCY,A,LDA)
        USE MKL95_PRECISION, ONLY: WP => DP
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(INOUT) :: A(:,:)
        REAL(WP), INTENT(IN) :: X(:)
        REAL(WP), INTENT(IN) :: Y(:)
    END SUBROUTINE DGER_MKL95
END INTERFACE GER
INTERFACE GERC
    PURE SUBROUTINE CGERC_MKL95(A,X,Y,ALPHA)
        ! MKL Fortran77 call:
        ! CGERC(M,N,ALPHA,X,INCX,Y,INCY,A,LDA)
        USE MKL95_PRECISION, ONLY: WP => SP
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(INOUT) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: X(:)
        COMPLEX(WP), INTENT(IN) :: Y(:)
    END SUBROUTINE CGERC_MKL95
    PURE SUBROUTINE ZGERC_MKL95(A,X,Y,ALPHA)
        ! MKL Fortran77 call:
        ! ZGERC(M,N,ALPHA,X,INCX,Y,INCY,A,LDA)
        USE MKL95_PRECISION, ONLY: WP => DP
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(INOUT) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: X(:)
        COMPLEX(WP), INTENT(IN) :: Y(:)
    END SUBROUTINE ZGERC_MKL95
END INTERFACE GERC
INTERFACE GERU
    PURE SUBROUTINE CGERU_MKL95(A,X,Y,ALPHA)
        ! MKL Fortran77 call:
        ! CGERU(M,N,ALPHA,X,INCX,Y,INCY,A,LDA)
        USE MKL95_PRECISION, ONLY: WP => SP
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(INOUT) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: X(:)
        COMPLEX(WP), INTENT(IN) :: Y(:)
    END SUBROUTINE CGERU_MKL95
    PURE SUBROUTINE ZGERU_MKL95(A,X,Y,ALPHA)
        ! MKL Fortran77 call:
        ! ZGERU(M,N,ALPHA,X,INCX,Y,INCY,A,LDA)
        USE MKL95_PRECISION, ONLY: WP => DP
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(INOUT) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: X(:)
        COMPLEX(WP), INTENT(IN) :: Y(:)
    END SUBROUTINE ZGERU_MKL95
END INTERFACE GERU
INTERFACE HBMV
    PURE SUBROUTINE CHBMV_MKL95(A,X,Y,UPLO,ALPHA,BETA)
        ! MKL Fortran77 call:
        ! CHBMV(UPLO,N,K,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
        USE MKL95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(IN), OPTIONAL :: BETA
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: X(:)
        COMPLEX(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE CHBMV_MKL95
    PURE SUBROUTINE ZHBMV_MKL95(A,X,Y,UPLO,ALPHA,BETA)
        ! MKL Fortran77 call:
        ! ZHBMV(UPLO,N,K,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
        USE MKL95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(IN), OPTIONAL :: BETA
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: X(:)
        COMPLEX(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE ZHBMV_MKL95
END INTERFACE HBMV
INTERFACE HEMV
    PURE SUBROUTINE CHEMV_MKL95(A,X,Y,UPLO,ALPHA,BETA)
        ! MKL Fortran77 call:
        ! CHEMV(UPLO,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
        USE MKL95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(IN), OPTIONAL :: BETA
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: X(:)
        COMPLEX(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE CHEMV_MKL95
    PURE SUBROUTINE ZHEMV_MKL95(A,X,Y,UPLO,ALPHA,BETA)
        ! MKL Fortran77 call:
        ! ZHEMV(UPLO,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
        USE MKL95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(IN), OPTIONAL :: BETA
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: X(:)
        COMPLEX(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE ZHEMV_MKL95
END INTERFACE HEMV
INTERFACE HER
    PURE SUBROUTINE CHER_MKL95(A,X,UPLO,ALPHA)
        ! MKL Fortran77 call:
        ! CHER(UPLO,N,ALPHA,X,INCX,A,LDA)
        USE MKL95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(INOUT) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: X(:)
    END SUBROUTINE CHER_MKL95
    PURE SUBROUTINE ZHER_MKL95(A,X,UPLO,ALPHA)
        ! MKL Fortran77 call:
        ! ZHER(UPLO,N,ALPHA,X,INCX,A,LDA)
        USE MKL95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(INOUT) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: X(:)
    END SUBROUTINE ZHER_MKL95
END INTERFACE HER
INTERFACE HER2
    PURE SUBROUTINE CHER2_MKL95(A,X,Y,UPLO,ALPHA)
        ! MKL Fortran77 call:
        ! CHER2(UPLO,N,ALPHA,X,INCX,Y,INCY,A,LDA)
        USE MKL95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(INOUT) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: X(:)
        COMPLEX(WP), INTENT(IN) :: Y(:)
    END SUBROUTINE CHER2_MKL95
    PURE SUBROUTINE ZHER2_MKL95(A,X,Y,UPLO,ALPHA)
        ! MKL Fortran77 call:
        ! ZHER2(UPLO,N,ALPHA,X,INCX,Y,INCY,A,LDA)
        USE MKL95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(INOUT) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: X(:)
        COMPLEX(WP), INTENT(IN) :: Y(:)
    END SUBROUTINE ZHER2_MKL95
END INTERFACE HER2
INTERFACE HPMV
    PURE SUBROUTINE CHPMV_MKL95(AP,X,Y,UPLO,ALPHA,BETA)
        ! MKL Fortran77 call:
        ! CHPMV(UPLO,N,ALPHA,AP,X,INCX,BETA,Y,INCY)
        USE MKL95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(IN), OPTIONAL :: BETA
        COMPLEX(WP), INTENT(IN) :: AP(:)
        COMPLEX(WP), INTENT(IN) :: X(:)
        COMPLEX(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE CHPMV_MKL95
    PURE SUBROUTINE ZHPMV_MKL95(AP,X,Y,UPLO,ALPHA,BETA)
        ! MKL Fortran77 call:
        ! ZHPMV(UPLO,N,ALPHA,AP,X,INCX,BETA,Y,INCY)
        USE MKL95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(IN), OPTIONAL :: BETA
        COMPLEX(WP), INTENT(IN) :: AP(:)
        COMPLEX(WP), INTENT(IN) :: X(:)
        COMPLEX(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE ZHPMV_MKL95
END INTERFACE HPMV
INTERFACE HPR
    PURE SUBROUTINE CHPR_MKL95(AP,X,UPLO,ALPHA)
        ! MKL Fortran77 call:
        ! CHPR(UPLO,N,ALPHA,X,INCX,AP)
        USE MKL95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(INOUT) :: AP(:)
        COMPLEX(WP), INTENT(IN) :: X(:)
    END SUBROUTINE CHPR_MKL95
    PURE SUBROUTINE ZHPR_MKL95(AP,X,UPLO,ALPHA)
        ! MKL Fortran77 call:
        ! ZHPR(UPLO,N,ALPHA,X,INCX,AP)
        USE MKL95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(INOUT) :: AP(:)
        COMPLEX(WP), INTENT(IN) :: X(:)
    END SUBROUTINE ZHPR_MKL95
END INTERFACE HPR
INTERFACE HPR2
    PURE SUBROUTINE CHPR2_MKL95(AP,X,Y,UPLO,ALPHA)
        ! MKL Fortran77 call:
        ! CHPR2(UPLO,N,ALPHA,X,INCX,Y,INCY,AP)
        USE MKL95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(INOUT) :: AP(:)
        COMPLEX(WP), INTENT(IN) :: X(:)
        COMPLEX(WP), INTENT(IN) :: Y(:)
    END SUBROUTINE CHPR2_MKL95
    PURE SUBROUTINE ZHPR2_MKL95(AP,X,Y,UPLO,ALPHA)
        ! MKL Fortran77 call:
        ! ZHPR2(UPLO,N,ALPHA,X,INCX,Y,INCY,AP)
        USE MKL95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(INOUT) :: AP(:)
        COMPLEX(WP), INTENT(IN) :: X(:)
        COMPLEX(WP), INTENT(IN) :: Y(:)
    END SUBROUTINE ZHPR2_MKL95
END INTERFACE HPR2
INTERFACE SBMV
    PURE SUBROUTINE SSBMV_MKL95(A,X,Y,UPLO,ALPHA,BETA)
        ! MKL Fortran77 call:
        ! SSBMV(UPLO,N,K,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
        USE MKL95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(IN), OPTIONAL :: BETA
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(IN) :: X(:)
        REAL(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE SSBMV_MKL95
    PURE SUBROUTINE DSBMV_MKL95(A,X,Y,UPLO,ALPHA,BETA)
        ! MKL Fortran77 call:
        ! DSBMV(UPLO,N,K,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
        USE MKL95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(IN), OPTIONAL :: BETA
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(IN) :: X(:)
        REAL(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE DSBMV_MKL95
END INTERFACE SBMV
INTERFACE SPMV
    PURE SUBROUTINE SSPMV_MKL95(AP,X,Y,UPLO,ALPHA,BETA)
        ! MKL Fortran77 call:
        ! SSPMV(UPLO,N,ALPHA,AP,X,INCX,BETA,Y,INCY)
        USE MKL95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(IN), OPTIONAL :: BETA
        REAL(WP), INTENT(IN) :: AP(:)
        REAL(WP), INTENT(IN) :: X(:)
        REAL(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE SSPMV_MKL95
    PURE SUBROUTINE DSPMV_MKL95(AP,X,Y,UPLO,ALPHA,BETA)
        ! MKL Fortran77 call:
        ! DSPMV(UPLO,N,ALPHA,AP,X,INCX,BETA,Y,INCY)
        USE MKL95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(IN), OPTIONAL :: BETA
        REAL(WP), INTENT(IN) :: AP(:)
        REAL(WP), INTENT(IN) :: X(:)
        REAL(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE DSPMV_MKL95
END INTERFACE SPMV
INTERFACE SPR
    PURE SUBROUTINE SSPR_MKL95(AP,X,UPLO,ALPHA)
        ! MKL Fortran77 call:
        ! SSPR(UPLO,N,ALPHA,X,INCX,AP)
        USE MKL95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(INOUT) :: AP(:)
        REAL(WP), INTENT(IN) :: X(:)
    END SUBROUTINE SSPR_MKL95
    PURE SUBROUTINE DSPR_MKL95(AP,X,UPLO,ALPHA)
        ! MKL Fortran77 call:
        ! DSPR(UPLO,N,ALPHA,X,INCX,AP)
        USE MKL95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(INOUT) :: AP(:)
        REAL(WP), INTENT(IN) :: X(:)
    END SUBROUTINE DSPR_MKL95
END INTERFACE SPR
INTERFACE SPR2
    PURE SUBROUTINE SSPR2_MKL95(AP,X,Y,UPLO,ALPHA)
        ! MKL Fortran77 call:
        ! SSPR2(UPLO,N,ALPHA,X,INCX,Y,INCY,AP)
        USE MKL95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(INOUT) :: AP(:)
        REAL(WP), INTENT(IN) :: X(:)
        REAL(WP), INTENT(IN) :: Y(:)
    END SUBROUTINE SSPR2_MKL95
    PURE SUBROUTINE DSPR2_MKL95(AP,X,Y,UPLO,ALPHA)
        ! MKL Fortran77 call:
        ! DSPR2(UPLO,N,ALPHA,X,INCX,Y,INCY,AP)
        USE MKL95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(INOUT) :: AP(:)
        REAL(WP), INTENT(IN) :: X(:)
        REAL(WP), INTENT(IN) :: Y(:)
    END SUBROUTINE DSPR2_MKL95
END INTERFACE SPR2
INTERFACE SYMV
    PURE SUBROUTINE SSYMV_MKL95(A,X,Y,UPLO,ALPHA,BETA)
        ! MKL Fortran77 call:
        ! SSYMV(UPLO,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
        USE MKL95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(IN), OPTIONAL :: BETA
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(IN) :: X(:)
        REAL(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE SSYMV_MKL95
    PURE SUBROUTINE DSYMV_MKL95(A,X,Y,UPLO,ALPHA,BETA)
        ! MKL Fortran77 call:
        ! DSYMV(UPLO,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
        USE MKL95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(IN), OPTIONAL :: BETA
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(IN) :: X(:)
        REAL(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE DSYMV_MKL95
END INTERFACE SYMV
INTERFACE SYR
    PURE SUBROUTINE SSYR_MKL95(A,X,UPLO,ALPHA)
        ! MKL Fortran77 call:
        ! SSYR(UPLO,N,ALPHA,X,INCX,A,LDA)
        USE MKL95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(INOUT) :: A(:,:)
        REAL(WP), INTENT(IN) :: X(:)
    END SUBROUTINE SSYR_MKL95
    PURE SUBROUTINE DSYR_MKL95(A,X,UPLO,ALPHA)
        ! MKL Fortran77 call:
        ! DSYR(UPLO,N,ALPHA,X,INCX,A,LDA)
        USE MKL95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(INOUT) :: A(:,:)
        REAL(WP), INTENT(IN) :: X(:)
    END SUBROUTINE DSYR_MKL95
END INTERFACE SYR
INTERFACE SYR2
    PURE SUBROUTINE SSYR2_MKL95(A,X,Y,UPLO,ALPHA)
        ! MKL Fortran77 call:
        ! SSYR2(UPLO,N,ALPHA,X,INCX,Y,INCY,A,LDA)
        USE MKL95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(INOUT) :: A(:,:)
        REAL(WP), INTENT(IN) :: X(:)
        REAL(WP), INTENT(IN) :: Y(:)
    END SUBROUTINE SSYR2_MKL95
    PURE SUBROUTINE DSYR2_MKL95(A,X,Y,UPLO,ALPHA)
        ! MKL Fortran77 call:
        ! DSYR2(UPLO,N,ALPHA,X,INCX,Y,INCY,A,LDA)
        USE MKL95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(INOUT) :: A(:,:)
        REAL(WP), INTENT(IN) :: X(:)
        REAL(WP), INTENT(IN) :: Y(:)
    END SUBROUTINE DSYR2_MKL95
END INTERFACE SYR2
INTERFACE TBMV
    PURE SUBROUTINE STBMV_MKL95(A,X,UPLO,TRANS,DIAG)
        ! MKL Fortran77 call:
        ! STBMV(UPLO,TRANS,DIAG,N,K,A,LDA,X,INCX)
        USE MKL95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE STBMV_MKL95
    PURE SUBROUTINE DTBMV_MKL95(A,X,UPLO,TRANS,DIAG)
        ! MKL Fortran77 call:
        ! DTBMV(UPLO,TRANS,DIAG,N,K,A,LDA,X,INCX)
        USE MKL95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE DTBMV_MKL95
    PURE SUBROUTINE CTBMV_MKL95(A,X,UPLO,TRANS,DIAG)
        ! MKL Fortran77 call:
        ! CTBMV(UPLO,TRANS,DIAG,N,K,A,LDA,X,INCX)
        USE MKL95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE CTBMV_MKL95
    PURE SUBROUTINE ZTBMV_MKL95(A,X,UPLO,TRANS,DIAG)
        ! MKL Fortran77 call:
        ! ZTBMV(UPLO,TRANS,DIAG,N,K,A,LDA,X,INCX)
        USE MKL95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE ZTBMV_MKL95
END INTERFACE TBMV
INTERFACE TBSV
    PURE SUBROUTINE STBSV_MKL95(A,X,UPLO,TRANS,DIAG)
        ! MKL Fortran77 call:
        ! STBSV(UPLO,TRANS,DIAG,N,K,A,LDA,X,INCX)
        USE MKL95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE STBSV_MKL95
    PURE SUBROUTINE DTBSV_MKL95(A,X,UPLO,TRANS,DIAG)
        ! MKL Fortran77 call:
        ! DTBSV(UPLO,TRANS,DIAG,N,K,A,LDA,X,INCX)
        USE MKL95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE DTBSV_MKL95
    PURE SUBROUTINE CTBSV_MKL95(A,X,UPLO,TRANS,DIAG)
        ! MKL Fortran77 call:
        ! CTBSV(UPLO,TRANS,DIAG,N,K,A,LDA,X,INCX)
        USE MKL95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE CTBSV_MKL95
    PURE SUBROUTINE ZTBSV_MKL95(A,X,UPLO,TRANS,DIAG)
        ! MKL Fortran77 call:
        ! ZTBSV(UPLO,TRANS,DIAG,N,K,A,LDA,X,INCX)
        USE MKL95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE ZTBSV_MKL95
END INTERFACE TBSV
INTERFACE TPMV
    PURE SUBROUTINE STPMV_MKL95(AP,X,UPLO,TRANS,DIAG)
        ! MKL Fortran77 call:
        ! STPMV(UPLO,TRANS,DIAG,N,AP,X,INCX)
        USE MKL95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        REAL(WP), INTENT(IN) :: AP(:)
        REAL(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE STPMV_MKL95
    PURE SUBROUTINE DTPMV_MKL95(AP,X,UPLO,TRANS,DIAG)
        ! MKL Fortran77 call:
        ! DTPMV(UPLO,TRANS,DIAG,N,AP,X,INCX)
        USE MKL95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        REAL(WP), INTENT(IN) :: AP(:)
        REAL(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE DTPMV_MKL95
    PURE SUBROUTINE CTPMV_MKL95(AP,X,UPLO,TRANS,DIAG)
        ! MKL Fortran77 call:
        ! CTPMV(UPLO,TRANS,DIAG,N,AP,X,INCX)
        USE MKL95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        COMPLEX(WP), INTENT(IN) :: AP(:)
        COMPLEX(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE CTPMV_MKL95
    PURE SUBROUTINE ZTPMV_MKL95(AP,X,UPLO,TRANS,DIAG)
        ! MKL Fortran77 call:
        ! ZTPMV(UPLO,TRANS,DIAG,N,AP,X,INCX)
        USE MKL95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        COMPLEX(WP), INTENT(IN) :: AP(:)
        COMPLEX(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE ZTPMV_MKL95
END INTERFACE TPMV
INTERFACE TPSV
    PURE SUBROUTINE STPSV_MKL95(AP,X,UPLO,TRANS,DIAG)
        ! MKL Fortran77 call:
        ! STPSV(UPLO,TRANS,DIAG,N,AP,X,INCX)
        USE MKL95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        REAL(WP), INTENT(IN) :: AP(:)
        REAL(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE STPSV_MKL95
    PURE SUBROUTINE DTPSV_MKL95(AP,X,UPLO,TRANS,DIAG)
        ! MKL Fortran77 call:
        ! DTPSV(UPLO,TRANS,DIAG,N,AP,X,INCX)
        USE MKL95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        REAL(WP), INTENT(IN) :: AP(:)
        REAL(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE DTPSV_MKL95
    PURE SUBROUTINE CTPSV_MKL95(AP,X,UPLO,TRANS,DIAG)
        ! MKL Fortran77 call:
        ! CTPSV(UPLO,TRANS,DIAG,N,AP,X,INCX)
        USE MKL95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        COMPLEX(WP), INTENT(IN) :: AP(:)
        COMPLEX(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE CTPSV_MKL95
    PURE SUBROUTINE ZTPSV_MKL95(AP,X,UPLO,TRANS,DIAG)
        ! MKL Fortran77 call:
        ! ZTPSV(UPLO,TRANS,DIAG,N,AP,X,INCX)
        USE MKL95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        COMPLEX(WP), INTENT(IN) :: AP(:)
        COMPLEX(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE ZTPSV_MKL95
END INTERFACE TPSV
INTERFACE TRMV
    PURE SUBROUTINE STRMV_MKL95(A,X,UPLO,TRANS,DIAG)
        ! MKL Fortran77 call:
        ! STRMV(UPLO,TRANS,DIAG,N,A,LDA,X,INCX)
        USE MKL95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE STRMV_MKL95
    PURE SUBROUTINE DTRMV_MKL95(A,X,UPLO,TRANS,DIAG)
        ! MKL Fortran77 call:
        ! DTRMV(UPLO,TRANS,DIAG,N,A,LDA,X,INCX)
        USE MKL95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE DTRMV_MKL95
    PURE SUBROUTINE CTRMV_MKL95(A,X,UPLO,TRANS,DIAG)
        ! MKL Fortran77 call:
        ! CTRMV(UPLO,TRANS,DIAG,N,A,LDA,X,INCX)
        USE MKL95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE CTRMV_MKL95
    PURE SUBROUTINE ZTRMV_MKL95(A,X,UPLO,TRANS,DIAG)
        ! MKL Fortran77 call:
        ! ZTRMV(UPLO,TRANS,DIAG,N,A,LDA,X,INCX)
        USE MKL95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE ZTRMV_MKL95
END INTERFACE TRMV
INTERFACE TRSV
    PURE SUBROUTINE STRSV_MKL95(A,X,UPLO,TRANS,DIAG)
        ! MKL Fortran77 call:
        ! STRSV(UPLO,TRANS,DIAG,N,A,LDA,X,INCX)
        USE MKL95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE STRSV_MKL95
    PURE SUBROUTINE DTRSV_MKL95(A,X,UPLO,TRANS,DIAG)
        ! MKL Fortran77 call:
        ! DTRSV(UPLO,TRANS,DIAG,N,A,LDA,X,INCX)
        USE MKL95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE DTRSV_MKL95
    PURE SUBROUTINE CTRSV_MKL95(A,X,UPLO,TRANS,DIAG)
        ! MKL Fortran77 call:
        ! CTRSV(UPLO,TRANS,DIAG,N,A,LDA,X,INCX)
        USE MKL95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE CTRSV_MKL95
    PURE SUBROUTINE ZTRSV_MKL95(A,X,UPLO,TRANS,DIAG)
        ! MKL Fortran77 call:
        ! ZTRSV(UPLO,TRANS,DIAG,N,A,LDA,X,INCX)
        USE MKL95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(INOUT) :: X(:)
    END SUBROUTINE ZTRSV_MKL95
END INTERFACE TRSV
INTERFACE GEMM
    PURE SUBROUTINE SGEMM_MKL95(A,B,C,TRANSA,TRANSB,ALPHA,BETA)
        ! MKL Fortran77 call:
        ! SGEMM(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
        USE MKL95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANSA
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANSB
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(IN), OPTIONAL :: BETA
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(IN) :: B(:,:)
        REAL(WP), INTENT(INOUT) :: C(:,:)
    END SUBROUTINE SGEMM_MKL95
    PURE SUBROUTINE DGEMM_MKL95(A,B,C,TRANSA,TRANSB,ALPHA,BETA)
        ! MKL Fortran77 call:
        ! DGEMM(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
        USE MKL95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANSA
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANSB
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(IN), OPTIONAL :: BETA
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(IN) :: B(:,:)
        REAL(WP), INTENT(INOUT) :: C(:,:)
    END SUBROUTINE DGEMM_MKL95
    PURE SUBROUTINE CGEMM_MKL95(A,B,C,TRANSA,TRANSB,ALPHA,BETA)
        ! MKL Fortran77 call:
        ! CGEMM(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
        USE MKL95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANSA
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANSB
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(IN), OPTIONAL :: BETA
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: B(:,:)
        COMPLEX(WP), INTENT(INOUT) :: C(:,:)
    END SUBROUTINE CGEMM_MKL95
    PURE SUBROUTINE ZGEMM_MKL95(A,B,C,TRANSA,TRANSB,ALPHA,BETA)
        ! MKL Fortran77 call:
        ! ZGEMM(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
        USE MKL95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANSA
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANSB
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(IN), OPTIONAL :: BETA
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: B(:,:)
        COMPLEX(WP), INTENT(INOUT) :: C(:,:)
    END SUBROUTINE ZGEMM_MKL95
END INTERFACE GEMM
INTERFACE HEMM
    PURE SUBROUTINE CHEMM_MKL95(A,B,C,SIDE,UPLO,ALPHA,BETA)
        ! MKL Fortran77 call:
        ! CHEMM(SIDE,UPLO,M,N,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
        USE MKL95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: SIDE
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(IN), OPTIONAL :: BETA
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: B(:,:)
        COMPLEX(WP), INTENT(INOUT) :: C(:,:)
    END SUBROUTINE CHEMM_MKL95
    PURE SUBROUTINE ZHEMM_MKL95(A,B,C,SIDE,UPLO,ALPHA,BETA)
        ! MKL Fortran77 call:
        ! ZHEMM(SIDE,UPLO,M,N,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
        USE MKL95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: SIDE
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(IN), OPTIONAL :: BETA
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: B(:,:)
        COMPLEX(WP), INTENT(INOUT) :: C(:,:)
    END SUBROUTINE ZHEMM_MKL95
END INTERFACE HEMM
INTERFACE HERK
    PURE SUBROUTINE CHERK_MKL95(A,C,UPLO,TRANS,ALPHA,BETA)
        ! MKL Fortran77 call:
        ! CHERK(UPLO,TRANS,N,K,ALPHA,A,LDA,BETA,C,LDC)
        USE MKL95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(IN), OPTIONAL :: BETA
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(INOUT) :: C(:,:)
    END SUBROUTINE CHERK_MKL95
    PURE SUBROUTINE ZHERK_MKL95(A,C,UPLO,TRANS,ALPHA,BETA)
        ! MKL Fortran77 call:
        ! ZHERK(UPLO,TRANS,N,K,ALPHA,A,LDA,BETA,C,LDC)
        USE MKL95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(IN), OPTIONAL :: BETA
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(INOUT) :: C(:,:)
    END SUBROUTINE ZHERK_MKL95
END INTERFACE HERK
INTERFACE HER2K
    PURE SUBROUTINE CHER2K_MKL95(A,B,C,UPLO,TRANS,ALPHA,BETA)
        ! MKL Fortran77 call:
        ! CHER2K(UPLO,TRANS,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
        USE MKL95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(IN), OPTIONAL :: BETA
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: B(:,:)
        COMPLEX(WP), INTENT(INOUT) :: C(:,:)
    END SUBROUTINE CHER2K_MKL95
    PURE SUBROUTINE ZHER2K_MKL95(A,B,C,UPLO,TRANS,ALPHA,BETA)
        ! MKL Fortran77 call:
        ! ZHER2K(UPLO,TRANS,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
        USE MKL95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(IN), OPTIONAL :: BETA
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: B(:,:)
        COMPLEX(WP), INTENT(INOUT) :: C(:,:)
    END SUBROUTINE ZHER2K_MKL95
END INTERFACE HER2K
INTERFACE SYMM
    PURE SUBROUTINE SSYMM_MKL95(A,B,C,SIDE,UPLO,ALPHA,BETA)
        ! MKL Fortran77 call:
        ! SSYMM(SIDE,UPLO,M,N,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
        USE MKL95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: SIDE
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(IN), OPTIONAL :: BETA
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(IN) :: B(:,:)
        REAL(WP), INTENT(INOUT) :: C(:,:)
    END SUBROUTINE SSYMM_MKL95
    PURE SUBROUTINE DSYMM_MKL95(A,B,C,SIDE,UPLO,ALPHA,BETA)
        ! MKL Fortran77 call:
        ! DSYMM(SIDE,UPLO,M,N,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
        USE MKL95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: SIDE
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(IN), OPTIONAL :: BETA
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(IN) :: B(:,:)
        REAL(WP), INTENT(INOUT) :: C(:,:)
    END SUBROUTINE DSYMM_MKL95
    PURE SUBROUTINE CSYMM_MKL95(A,B,C,SIDE,UPLO,ALPHA,BETA)
        ! MKL Fortran77 call:
        ! CSYMM(SIDE,UPLO,M,N,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
        USE MKL95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: SIDE
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(IN), OPTIONAL :: BETA
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: B(:,:)
        COMPLEX(WP), INTENT(INOUT) :: C(:,:)
    END SUBROUTINE CSYMM_MKL95
    PURE SUBROUTINE ZSYMM_MKL95(A,B,C,SIDE,UPLO,ALPHA,BETA)
        ! MKL Fortran77 call:
        ! ZSYMM(SIDE,UPLO,M,N,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
        USE MKL95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: SIDE
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(IN), OPTIONAL :: BETA
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: B(:,:)
        COMPLEX(WP), INTENT(INOUT) :: C(:,:)
    END SUBROUTINE ZSYMM_MKL95
END INTERFACE SYMM
INTERFACE SYRK
    PURE SUBROUTINE SSYRK_MKL95(A,C,UPLO,TRANS,ALPHA,BETA)
        ! MKL Fortran77 call:
        ! SSYRK(UPLO,TRANS,N,K,ALPHA,A,LDA,BETA,C,LDC)
        USE MKL95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(IN), OPTIONAL :: BETA
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(INOUT) :: C(:,:)
    END SUBROUTINE SSYRK_MKL95
    PURE SUBROUTINE DSYRK_MKL95(A,C,UPLO,TRANS,ALPHA,BETA)
        ! MKL Fortran77 call:
        ! DSYRK(UPLO,TRANS,N,K,ALPHA,A,LDA,BETA,C,LDC)
        USE MKL95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(IN), OPTIONAL :: BETA
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(INOUT) :: C(:,:)
    END SUBROUTINE DSYRK_MKL95
    PURE SUBROUTINE CSYRK_MKL95(A,C,UPLO,TRANS,ALPHA,BETA)
        ! MKL Fortran77 call:
        ! CSYRK(UPLO,TRANS,N,K,ALPHA,A,LDA,BETA,C,LDC)
        USE MKL95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(IN), OPTIONAL :: BETA
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(INOUT) :: C(:,:)
    END SUBROUTINE CSYRK_MKL95
    PURE SUBROUTINE ZSYRK_MKL95(A,C,UPLO,TRANS,ALPHA,BETA)
        ! MKL Fortran77 call:
        ! ZSYRK(UPLO,TRANS,N,K,ALPHA,A,LDA,BETA,C,LDC)
        USE MKL95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(IN), OPTIONAL :: BETA
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(INOUT) :: C(:,:)
    END SUBROUTINE ZSYRK_MKL95
END INTERFACE SYRK
INTERFACE SYR2K
    PURE SUBROUTINE SSYR2K_MKL95(A,B,C,UPLO,TRANS,ALPHA,BETA)
        ! MKL Fortran77 call:
        ! SSYR2K(UPLO,TRANS,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
        USE MKL95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(IN), OPTIONAL :: BETA
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(IN) :: B(:,:)
        REAL(WP), INTENT(INOUT) :: C(:,:)
    END SUBROUTINE SSYR2K_MKL95
    PURE SUBROUTINE DSYR2K_MKL95(A,B,C,UPLO,TRANS,ALPHA,BETA)
        ! MKL Fortran77 call:
        ! DSYR2K(UPLO,TRANS,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
        USE MKL95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(IN), OPTIONAL :: BETA
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(IN) :: B(:,:)
        REAL(WP), INTENT(INOUT) :: C(:,:)
    END SUBROUTINE DSYR2K_MKL95
    PURE SUBROUTINE CSYR2K_MKL95(A,B,C,UPLO,TRANS,ALPHA,BETA)
        ! MKL Fortran77 call:
        ! CSYR2K(UPLO,TRANS,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
        USE MKL95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(IN), OPTIONAL :: BETA
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: B(:,:)
        COMPLEX(WP), INTENT(INOUT) :: C(:,:)
    END SUBROUTINE CSYR2K_MKL95
    PURE SUBROUTINE ZSYR2K_MKL95(A,B,C,UPLO,TRANS,ALPHA,BETA)
        ! MKL Fortran77 call:
        ! ZSYR2K(UPLO,TRANS,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
        USE MKL95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(IN), OPTIONAL :: BETA
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(IN) :: B(:,:)
        COMPLEX(WP), INTENT(INOUT) :: C(:,:)
    END SUBROUTINE ZSYR2K_MKL95
END INTERFACE SYR2K
INTERFACE TRMM
    PURE SUBROUTINE STRMM_MKL95(A,B,SIDE,UPLO,TRANSA,DIAG,ALPHA)
        ! MKL Fortran77 call:
        ! STRMM(SIDE,UPLO,TRANSA,DIAG,M,N,ALPHA,A,LDA,B,LDB)
        USE MKL95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: SIDE
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANSA
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(INOUT) :: B(:,:)
    END SUBROUTINE STRMM_MKL95
    PURE SUBROUTINE DTRMM_MKL95(A,B,SIDE,UPLO,TRANSA,DIAG,ALPHA)
        ! MKL Fortran77 call:
        ! DTRMM(SIDE,UPLO,TRANSA,DIAG,M,N,ALPHA,A,LDA,B,LDB)
        USE MKL95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: SIDE
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANSA
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(INOUT) :: B(:,:)
    END SUBROUTINE DTRMM_MKL95
    PURE SUBROUTINE CTRMM_MKL95(A,B,SIDE,UPLO,TRANSA,DIAG,ALPHA)
        ! MKL Fortran77 call:
        ! CTRMM(SIDE,UPLO,TRANSA,DIAG,M,N,ALPHA,A,LDA,B,LDB)
        USE MKL95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: SIDE
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANSA
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(INOUT) :: B(:,:)
    END SUBROUTINE CTRMM_MKL95
    PURE SUBROUTINE ZTRMM_MKL95(A,B,SIDE,UPLO,TRANSA,DIAG,ALPHA)
        ! MKL Fortran77 call:
        ! ZTRMM(SIDE,UPLO,TRANSA,DIAG,M,N,ALPHA,A,LDA,B,LDB)
        USE MKL95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: SIDE
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANSA
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(INOUT) :: B(:,:)
    END SUBROUTINE ZTRMM_MKL95
END INTERFACE TRMM
INTERFACE TRSM
    PURE SUBROUTINE STRSM_MKL95(A,B,SIDE,UPLO,TRANSA,DIAG,ALPHA)
        ! MKL Fortran77 call:
        ! STRSM(SIDE,UPLO,TRANSA,DIAG,M,N,ALPHA,A,LDA,B,LDB)
        USE MKL95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: SIDE
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANSA
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(INOUT) :: B(:,:)
    END SUBROUTINE STRSM_MKL95
    PURE SUBROUTINE DTRSM_MKL95(A,B,SIDE,UPLO,TRANSA,DIAG,ALPHA)
        ! MKL Fortran77 call:
        ! DTRSM(SIDE,UPLO,TRANSA,DIAG,M,N,ALPHA,A,LDA,B,LDB)
        USE MKL95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: SIDE
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANSA
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
        REAL(WP), INTENT(IN) :: A(:,:)
        REAL(WP), INTENT(INOUT) :: B(:,:)
    END SUBROUTINE DTRSM_MKL95
    PURE SUBROUTINE CTRSM_MKL95(A,B,SIDE,UPLO,TRANSA,DIAG,ALPHA)
        ! MKL Fortran77 call:
        ! CTRSM(SIDE,UPLO,TRANSA,DIAG,M,N,ALPHA,A,LDA,B,LDB)
        USE MKL95_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: SIDE
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANSA
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(INOUT) :: B(:,:)
    END SUBROUTINE CTRSM_MKL95
    PURE SUBROUTINE ZTRSM_MKL95(A,B,SIDE,UPLO,TRANSA,DIAG,ALPHA)
        ! MKL Fortran77 call:
        ! ZTRSM(SIDE,UPLO,TRANSA,DIAG,M,N,ALPHA,A,LDA,B,LDB)
        USE MKL95_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: SIDE
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANSA
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
        COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
        COMPLEX(WP), INTENT(IN) :: A(:,:)
        COMPLEX(WP), INTENT(INOUT) :: B(:,:)
    END SUBROUTINE ZTRSM_MKL95
END INTERFACE TRSM
INTERFACE AXPYI
    PURE SUBROUTINE SAXPYI_MKL95(X,INDX,Y,A)
        ! MKL Fortran77 call:
        ! SAXPYI(NZ,A,X,INDX,Y)
        USE MKL95_PRECISION, ONLY: WP => SP
        REAL(WP), INTENT(IN), OPTIONAL :: A
        REAL(WP), INTENT(IN) :: X(:)
        INTEGER, INTENT(IN) :: INDX(:)
        REAL(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE SAXPYI_MKL95
    PURE SUBROUTINE DAXPYI_MKL95(X,INDX,Y,A)
        ! MKL Fortran77 call:
        ! DAXPYI(NZ,A,X,INDX,Y)
        USE MKL95_PRECISION, ONLY: WP => DP
        REAL(WP), INTENT(IN), OPTIONAL :: A
        REAL(WP), INTENT(IN) :: X(:)
        INTEGER, INTENT(IN) :: INDX(:)
        REAL(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE DAXPYI_MKL95
    PURE SUBROUTINE CAXPYI_MKL95(X,INDX,Y,A)
        ! MKL Fortran77 call:
        ! CAXPYI(NZ,A,X,INDX,Y)
        USE MKL95_PRECISION, ONLY: WP => SP
        COMPLEX(WP), INTENT(IN), OPTIONAL :: A
        COMPLEX(WP), INTENT(IN) :: X(:)
        INTEGER, INTENT(IN) :: INDX(:)
        COMPLEX(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE CAXPYI_MKL95
    PURE SUBROUTINE ZAXPYI_MKL95(X,INDX,Y,A)
        ! MKL Fortran77 call:
        ! ZAXPYI(NZ,A,X,INDX,Y)
        USE MKL95_PRECISION, ONLY: WP => DP
        COMPLEX(WP), INTENT(IN), OPTIONAL :: A
        COMPLEX(WP), INTENT(IN) :: X(:)
        INTEGER, INTENT(IN) :: INDX(:)
        COMPLEX(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE ZAXPYI_MKL95
END INTERFACE AXPYI
INTERFACE DOTI
    PURE FUNCTION SDOTI_MKL95(X,INDX,Y)
        ! MKL Fortran77 call:
        ! SDOTI(NZ,X,INDX,Y)
        USE MKL95_PRECISION, ONLY: WP => SP
        REAL(WP) :: SDOTI_MKL95
        REAL(WP), INTENT(IN) :: X(:)
        INTEGER, INTENT(IN) :: INDX(:)
        REAL(WP), INTENT(IN) :: Y(:)
    END FUNCTION SDOTI_MKL95
    PURE FUNCTION DDOTI_MKL95(X,INDX,Y)
        ! MKL Fortran77 call:
        ! DDOTI(NZ,X,INDX,Y)
        USE MKL95_PRECISION, ONLY: WP => DP
        REAL(WP) :: DDOTI_MKL95
        REAL(WP), INTENT(IN) :: X(:)
        INTEGER, INTENT(IN) :: INDX(:)
        REAL(WP), INTENT(IN) :: Y(:)
    END FUNCTION DDOTI_MKL95
END INTERFACE DOTI
INTERFACE DOTCI
    PURE FUNCTION CDOTCI_MKL95(X,INDX,Y)
        ! MKL Fortran77 call:
        ! CDOTCI(NZ,X,INDX,Y)
        USE MKL95_PRECISION, ONLY: WP => SP
        COMPLEX(WP) :: CDOTCI_MKL95
        COMPLEX(WP), INTENT(IN) :: X(:)
        INTEGER, INTENT(IN) :: INDX(:)
        COMPLEX(WP), INTENT(IN) :: Y(:)
    END FUNCTION CDOTCI_MKL95
    PURE FUNCTION ZDOTCI_MKL95(X,INDX,Y)
        ! MKL Fortran77 call:
        ! ZDOTCI(NZ,X,INDX,Y)
        USE MKL95_PRECISION, ONLY: WP => DP
        COMPLEX(WP) :: ZDOTCI_MKL95
        COMPLEX(WP), INTENT(IN) :: X(:)
        INTEGER, INTENT(IN) :: INDX(:)
        COMPLEX(WP), INTENT(IN) :: Y(:)
    END FUNCTION ZDOTCI_MKL95
END INTERFACE DOTCI
INTERFACE DOTUI
    PURE FUNCTION CDOTUI_MKL95(X,INDX,Y)
        ! MKL Fortran77 call:
        ! CDOTUI(NZ,X,INDX,Y)
        USE MKL95_PRECISION, ONLY: WP => SP
        COMPLEX(WP) :: CDOTUI_MKL95
        COMPLEX(WP), INTENT(IN) :: X(:)
        INTEGER, INTENT(IN) :: INDX(:)
        COMPLEX(WP), INTENT(IN) :: Y(:)
    END FUNCTION CDOTUI_MKL95
    PURE FUNCTION ZDOTUI_MKL95(X,INDX,Y)
        ! MKL Fortran77 call:
        ! ZDOTUI(NZ,X,INDX,Y)
        USE MKL95_PRECISION, ONLY: WP => DP
        COMPLEX(WP) :: ZDOTUI_MKL95
        COMPLEX(WP), INTENT(IN) :: X(:)
        INTEGER, INTENT(IN) :: INDX(:)
        COMPLEX(WP), INTENT(IN) :: Y(:)
    END FUNCTION ZDOTUI_MKL95
END INTERFACE DOTUI
INTERFACE GTHR
    PURE SUBROUTINE SGTHR_MKL95(X,INDX,Y)
        ! MKL Fortran77 call:
        ! SGTHR(NZ,Y,X,INDX)
        USE MKL95_PRECISION, ONLY: WP => SP
        REAL(WP), INTENT(OUT) :: X(:)
        INTEGER, INTENT(IN) :: INDX(:)
        REAL(WP), INTENT(IN) :: Y(:)
    END SUBROUTINE SGTHR_MKL95
    PURE SUBROUTINE DGTHR_MKL95(X,INDX,Y)
        ! MKL Fortran77 call:
        ! DGTHR(NZ,Y,X,INDX)
        USE MKL95_PRECISION, ONLY: WP => DP
        REAL(WP), INTENT(OUT) :: X(:)
        INTEGER, INTENT(IN) :: INDX(:)
        REAL(WP), INTENT(IN) :: Y(:)
    END SUBROUTINE DGTHR_MKL95
    PURE SUBROUTINE CGTHR_MKL95(X,INDX,Y)
        ! MKL Fortran77 call:
        ! CGTHR(NZ,Y,X,INDX)
        USE MKL95_PRECISION, ONLY: WP => SP
        COMPLEX(WP), INTENT(OUT) :: X(:)
        INTEGER, INTENT(IN) :: INDX(:)
        COMPLEX(WP), INTENT(IN) :: Y(:)
    END SUBROUTINE CGTHR_MKL95
    PURE SUBROUTINE ZGTHR_MKL95(X,INDX,Y)
        ! MKL Fortran77 call:
        ! ZGTHR(NZ,Y,X,INDX)
        USE MKL95_PRECISION, ONLY: WP => DP
        COMPLEX(WP), INTENT(OUT) :: X(:)
        INTEGER, INTENT(IN) :: INDX(:)
        COMPLEX(WP), INTENT(IN) :: Y(:)
    END SUBROUTINE ZGTHR_MKL95
END INTERFACE GTHR
INTERFACE GTHRZ
    PURE SUBROUTINE SGTHRZ_MKL95(X,INDX,Y)
        ! MKL Fortran77 call:
        ! SGTHRZ(NZ,Y,X,INDX)
        USE MKL95_PRECISION, ONLY: WP => SP
        REAL(WP), INTENT(OUT) :: X(:)
        INTEGER, INTENT(IN) :: INDX(:)
        REAL(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE SGTHRZ_MKL95
    PURE SUBROUTINE DGTHRZ_MKL95(X,INDX,Y)
        ! MKL Fortran77 call:
        ! DGTHRZ(NZ,Y,X,INDX)
        USE MKL95_PRECISION, ONLY: WP => DP
        REAL(WP), INTENT(OUT) :: X(:)
        INTEGER, INTENT(IN) :: INDX(:)
        REAL(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE DGTHRZ_MKL95
    PURE SUBROUTINE CGTHRZ_MKL95(X,INDX,Y)
        ! MKL Fortran77 call:
        ! CGTHRZ(NZ,Y,X,INDX)
        USE MKL95_PRECISION, ONLY: WP => SP
        COMPLEX(WP), INTENT(OUT) :: X(:)
        INTEGER, INTENT(IN) :: INDX(:)
        COMPLEX(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE CGTHRZ_MKL95
    PURE SUBROUTINE ZGTHRZ_MKL95(X,INDX,Y)
        ! MKL Fortran77 call:
        ! ZGTHRZ(NZ,Y,X,INDX)
        USE MKL95_PRECISION, ONLY: WP => DP
        COMPLEX(WP), INTENT(OUT) :: X(:)
        INTEGER, INTENT(IN) :: INDX(:)
        COMPLEX(WP), INTENT(INOUT) :: Y(:)
    END SUBROUTINE ZGTHRZ_MKL95
END INTERFACE GTHRZ
INTERFACE ROTI
    PURE SUBROUTINE SROTI_MKL95(X,INDX,Y,C,S)
        ! MKL Fortran77 call:
        ! SROTI(NZ,X,INDX,Y,C,S)
        USE MKL95_PRECISION, ONLY: WP => SP
        REAL(WP), INTENT(IN) :: C
        REAL(WP), INTENT(IN) :: S
        REAL(WP), INTENT(INOUT) :: X(:)
        INTEGER, INTENT(IN) :: INDX(:)
        REAL(WP), INTENT(IN) :: Y(:)
    END SUBROUTINE SROTI_MKL95
    PURE SUBROUTINE DROTI_MKL95(X,INDX,Y,C,S)
        ! MKL Fortran77 call:
        ! DROTI(NZ,X,INDX,Y,C,S)
        USE MKL95_PRECISION, ONLY: WP => DP
        REAL(WP), INTENT(IN) :: C
        REAL(WP), INTENT(IN) :: S
        REAL(WP), INTENT(INOUT) :: X(:)
        INTEGER, INTENT(IN) :: INDX(:)
        REAL(WP), INTENT(IN) :: Y(:)
    END SUBROUTINE DROTI_MKL95
END INTERFACE ROTI
INTERFACE SCTR
    PURE SUBROUTINE SSCTR_MKL95(X,INDX,Y)
        ! MKL Fortran77 call:
        ! SSCTR(NZ,X,INDX,Y)
        USE MKL95_PRECISION, ONLY: WP => SP
        REAL(WP), INTENT(IN) :: X(:)
        INTEGER, INTENT(IN) :: INDX(:)
        REAL(WP), INTENT(OUT) :: Y(:)
    END SUBROUTINE SSCTR_MKL95
    PURE SUBROUTINE DSCTR_MKL95(X,INDX,Y)
        ! MKL Fortran77 call:
        ! DSCTR(NZ,X,INDX,Y)
        USE MKL95_PRECISION, ONLY: WP => DP
        REAL(WP), INTENT(IN) :: X(:)
        INTEGER, INTENT(IN) :: INDX(:)
        REAL(WP), INTENT(OUT) :: Y(:)
    END SUBROUTINE DSCTR_MKL95
    PURE SUBROUTINE CSCTR_MKL95(X,INDX,Y)
        ! MKL Fortran77 call:
        ! CSCTR(NZ,X,INDX,Y)
        USE MKL95_PRECISION, ONLY: WP => SP
        COMPLEX(WP), INTENT(IN) :: X(:)
        INTEGER, INTENT(IN) :: INDX(:)
        COMPLEX(WP), INTENT(OUT) :: Y(:)
    END SUBROUTINE CSCTR_MKL95
    PURE SUBROUTINE ZSCTR_MKL95(X,INDX,Y)
        ! MKL Fortran77 call:
        ! ZSCTR(NZ,X,INDX,Y)
        USE MKL95_PRECISION, ONLY: WP => DP
        COMPLEX(WP), INTENT(IN) :: X(:)
        INTEGER, INTENT(IN) :: INDX(:)
        COMPLEX(WP), INTENT(OUT) :: Y(:)
    END SUBROUTINE ZSCTR_MKL95
END INTERFACE SCTR
END MODULE MKL95_BLAS
