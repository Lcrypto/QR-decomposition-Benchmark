!*******************************************************************************
!                              INTEL CONFIDENTIAL
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
!*******************************************************************************

! Include to build module MKL_DFTI
INCLUDE 'mkl_dfti.f90'

! Definition of module MKL_CDFT_DM_TYPE. It is used just to define type DFTI_DESCRIPTOR_DM
MODULE MKL_CDFT_DM_TYPE

! Definition of descriptor.
! Structure of this type is not used in Fortran code. The pointer to this type is used only
TYPE DFTI_DESCRIPTOR_DM
    PRIVATE
    INTEGER(4) DESCRIPTOR
END TYPE

END MODULE

! Definition of module MKL_CDFT. It is used to define constants and interfaces of routines
MODULE MKL_CDFT

! Module MKL_CDFT includes definitions from module MKL_DFTI and MKL_CDFT_DM_TYPE
USE MKL_DFTI
USE MKL_CDFT_DM_TYPE

IMPLICIT NONE

! Codes of parameters for DftiGetValueDM / DftiSetValueDM
INTEGER, PARAMETER :: CDFT_LOCAL_SIZE        =1000
INTEGER, PARAMETER :: CDFT_LOCAL_X_START     =1001
INTEGER, PARAMETER :: CDFT_LOCAL_NX          =1002
INTEGER, PARAMETER :: CDFT_MPI_COMM          =1003
INTEGER, PARAMETER :: CDFT_WORKSPACE         =1004
INTEGER, PARAMETER :: CDFT_LOCAL_OUT_X_START =1005
INTEGER, PARAMETER :: CDFT_LOCAL_OUT_NX      =1006

! Codes of errors
INTEGER, PARAMETER :: CDFT_MPI_ERROR     =1000
INTEGER, PARAMETER :: CDFT_SPREAD_ERROR  =1001

! Interfaces of routines
INTERFACE DftiCreateDescriptorDM
    MODULE PROCEDURE DftiCreateDescriptorDM1
    MODULE PROCEDURE DftiCreateDescriptorDMn
    MODULE PROCEDURE DftiCreateDescriptorDM1_s
    MODULE PROCEDURE DftiCreateDescriptorDM1_d
    MODULE PROCEDURE DftiCreateDescriptorDMn_s
    MODULE PROCEDURE DftiCreateDescriptorDMn_d
END INTERFACE
PRIVATE DftiCreateDescriptorDM1
PRIVATE DftiCreateDescriptorDMn
PRIVATE DftiCreateDescriptorDM1_s
PRIVATE DftiCreateDescriptorDM1_d
PRIVATE DftiCreateDescriptorDMn_s
PRIVATE DftiCreateDescriptorDMn_d

INTERFACE DftiGetValueDM
    MODULE PROCEDURE DftiGetValueDMs
    MODULE PROCEDURE DftiGetValueDMd
    MODULE PROCEDURE DftiGetValueDMi
END INTERFACE
PRIVATE DftiGetValueDMs
PRIVATE DftiGetValueDMd
PRIVATE DftiGetValueDMi

INTERFACE DftiSetValueDM
    MODULE PROCEDURE DftiSetValueDMs
    MODULE PROCEDURE DftiSetValueDMd
    MODULE PROCEDURE DftiSetValueDMi
    MODULE PROCEDURE DftiSetValueDMpc
    MODULE PROCEDURE DftiSetValueDMpz
    MODULE PROCEDURE DftiSetValueDMps
    MODULE PROCEDURE DftiSetValueDMpd
    MODULE PROCEDURE DftiSetValueDMpi
END INTERFACE
PRIVATE DftiSetValueDMs
PRIVATE DftiSetValueDMd
PRIVATE DftiSetValueDMi
PRIVATE DftiSetValueDMpc
PRIVATE DftiSetValueDMpz
PRIVATE DftiSetValueDMps
PRIVATE DftiSetValueDMpd
PRIVATE DftiSetValueDMpi

INTERFACE DftiCommitDescriptorDM
    INTEGER FUNCTION DftiCommitDescriptorDM_internal(H)
        USE MKL_CDFT_DM_TYPE
        TYPE(DFTI_DESCRIPTOR_DM), POINTER :: H
    END FUNCTION
END INTERFACE
PRIVATE DftiCommitDescriptorDM_internal

INTERFACE DftiComputeForwardDM
    MODULE PROCEDURE DftiComputeForwardDMoc
    MODULE PROCEDURE DftiComputeForwardDMoz
    MODULE PROCEDURE DftiComputeForwardDMic
    MODULE PROCEDURE DftiComputeForwardDMiz
END INTERFACE
PRIVATE DftiComputeForwardDMoc
PRIVATE DftiComputeForwardDMoz
PRIVATE DftiComputeForwardDMic
PRIVATE DftiComputeForwardDMiz

INTERFACE DftiComputeBackwardDM
    MODULE PROCEDURE DftiComputeBackwardDMoc
    MODULE PROCEDURE DftiComputeBackwardDMoz
    MODULE PROCEDURE DftiComputeBackwardDMic
    MODULE PROCEDURE DftiComputeBackwardDMiz
END INTERFACE
PRIVATE DftiComputeBackwardDMoc
PRIVATE DftiComputeBackwardDMoz
PRIVATE DftiComputeBackwardDMic
PRIVATE DftiComputeBackwardDMiz

INTERFACE DftiFreeDescriptorDM
    INTEGER FUNCTION DftiFreeDescriptorDM_internal(H)
        USE MKL_CDFT_DM_TYPE
        TYPE(DFTI_DESCRIPTOR_DM), POINTER :: H
    END FUNCTION
END INTERFACE
PRIVATE DftiFreeDescriptorDM_internal

CONTAINS

!INTERFACE DftiCreateDescriptorDM
INTEGER FUNCTION DftiCreateDescriptorDMn(C,H,P1,P2,D,L)
        USE MKL_CDFT_DM_TYPE
        TYPE(DFTI_DESCRIPTOR_DM), POINTER :: H
        INTEGER(4) C
        INTEGER P1,P2,D,L(*)
        INTENT(IN) :: C,P1,P2,D,L
        INTERFACE
          INTEGER FUNCTION DftiCreateDescriptorDMn_internal(C,H,P1,P2,D,L)
            USE MKL_CDFT_DM_TYPE
            TYPE(DFTI_DESCRIPTOR_DM), POINTER :: H
            INTEGER(4) C
            INTEGER P1,P2,D,L(*)
            INTENT(IN) :: C,P1,P2,D,L
          END FUNCTION
        END INTERFACE
        DftiCreateDescriptorDMn = DftiCreateDescriptorDMn_internal(C,H,P1,P2,D,L)
END FUNCTION
INTEGER FUNCTION DftiCreateDescriptorDM1(C,H,P1,P2,D,L)
        USE MKL_CDFT_DM_TYPE
        TYPE(DFTI_DESCRIPTOR_DM), POINTER :: H
        INTEGER(4) C
        INTEGER P1,P2,D,L
        INTENT(IN) :: C,P1,P2,D,L
        INTERFACE
          INTEGER FUNCTION DftiCreateDescriptorDM1_internal(C,H,P1,P2,D,L)
            USE MKL_CDFT_DM_TYPE
            TYPE(DFTI_DESCRIPTOR_DM), POINTER :: H
            INTEGER(4) C
            INTEGER P1,P2,D,L
            INTENT(IN) :: C,P1,P2,D,L
          END FUNCTION
        END INTERFACE
        DftiCreateDescriptorDM1 = DftiCreateDescriptorDM1_internal(C,H,P1,P2,D,L)
END FUNCTION
INTEGER FUNCTION DftiCreateDescriptorDM1_s(C,H,P1R,P2,D,L)
        USE MKL_CDFT_DM_TYPE
        TYPE(DFTI_DESCRIPTOR_DM), POINTER :: H
        REAL(4), INTENT(IN) :: P1R
        INTEGER(4) C
        INTEGER P1,P2,D,L
        INTENT(IN) :: C,P2,D,L
        P1 = P1R
        DftiCreateDescriptorDM1_s = DftiCreateDescriptorDM1(C,H,P1,P2,D,L)
END FUNCTION
INTEGER FUNCTION DftiCreateDescriptorDM1_d(C,H,P1R,P2,D,L)
        USE MKL_CDFT_DM_TYPE
        TYPE(DFTI_DESCRIPTOR_DM), POINTER :: H
        REAL(8), INTENT(IN) :: P1R
        INTEGER(4) C
        INTEGER P1,P2,D,L
        INTENT(IN) :: C,P2,D,L
        P1 = P1R
        DftiCreateDescriptorDM1_d = DftiCreateDescriptorDM1(C,H,P1,P2,D,L)
END FUNCTION
INTEGER FUNCTION DftiCreateDescriptorDMn_s(C,H,P1R,P2,D,L)
        USE MKL_CDFT_DM_TYPE
        TYPE(DFTI_DESCRIPTOR_DM), POINTER :: H
        REAL(4), INTENT(IN) :: P1R
        INTEGER(4) C
        INTEGER P1,P2,D,L(*)
        INTENT(IN) :: C,P2,D,L
        P1 = P1R
        DftiCreateDescriptorDMn_s = DftiCreateDescriptorDMn(C,H,P1,P2,D,L)
END FUNCTION
INTEGER FUNCTION DftiCreateDescriptorDMn_d(C,H,P1R,P2,D,L)
        USE MKL_CDFT_DM_TYPE
        TYPE(DFTI_DESCRIPTOR_DM), POINTER :: H
        REAL(8), INTENT(IN) :: P1R
        INTEGER(4) C
        INTEGER P1,P2,D,L(*)
        INTENT(IN) :: C,P2,D,L
        P1 = P1R
        DftiCreateDescriptorDMn_d = DftiCreateDescriptorDMn(C,H,P1,P2,D,L)
END FUNCTION
!END INTERFACE DftiCreateDescriptorDM
!INTERFACE DftiSetValueDM
INTEGER FUNCTION DftiSetValueDMs(H,P,V)
        USE MKL_CDFT_DM_TYPE
        TYPE(DFTI_DESCRIPTOR_DM), POINTER :: H
        INTEGER P
        REAL(4) V
        INTENT(IN) :: P,V
        INTERFACE
          INTEGER FUNCTION DftiSetValueDMf_internal(H,P,V)
            USE MKL_CDFT_DM_TYPE
            TYPE(DFTI_DESCRIPTOR_DM), POINTER :: H
            INTEGER P
            REAL(4) V
            INTENT(IN) :: P,V
          END FUNCTION
        END INTERFACE
        DftiSetValueDMs = DftiSetValueDMf_internal(H,P,V)
END FUNCTION
INTEGER FUNCTION DftiSetValueDMd(H,P,V)
        USE MKL_CDFT_DM_TYPE
        TYPE(DFTI_DESCRIPTOR_DM), POINTER :: H
        INTEGER P
        REAL(8) V
        INTENT(IN) :: P,V
        INTERFACE
          INTEGER FUNCTION DftiSetValueDMd_internal(H,P,V)
            USE MKL_CDFT_DM_TYPE
            TYPE(DFTI_DESCRIPTOR_DM), POINTER :: H
            INTEGER P
            REAL(8) V
            INTENT(IN) :: P,V
          END FUNCTION
        END INTERFACE
        DftiSetValueDMd = DftiSetValueDMd_internal(H,P,V)
END FUNCTION
INTEGER FUNCTION DftiSetValueDMi(H,P,V)
        USE MKL_CDFT_DM_TYPE
        TYPE(DFTI_DESCRIPTOR_DM), POINTER :: H
        INTEGER P,V
        INTENT(IN) :: P,V
        INTERFACE
          INTEGER FUNCTION DftiSetValueDMi_internal(H,P,V)
            USE MKL_CDFT_DM_TYPE
            TYPE(DFTI_DESCRIPTOR_DM), POINTER :: H
            INTEGER P,V
            INTENT(IN) :: P,V
          END FUNCTION
        END INTERFACE
        DftiSetValueDMi = DftiSetValueDMi_internal(H,P,V)
END FUNCTION
INTEGER FUNCTION DftiSetValueDMpc(H,P,V)
        USE MKL_CDFT_DM_TYPE
        TYPE(DFTI_DESCRIPTOR_DM), POINTER :: H
        INTEGER P
        COMPLEX(4) V(*)
        INTENT(IN) :: P,V
        INTERFACE
          INTEGER FUNCTION DftiSetValueDMp_internal(H,P,V)
            USE MKL_CDFT_DM_TYPE
            TYPE(DFTI_DESCRIPTOR_DM), POINTER :: H
            INTEGER P
            COMPLEX(4) V(*)
            INTENT(IN) :: P,V
          END FUNCTION
        END INTERFACE
        DftiSetValueDMpc = DftiSetValueDMp_internal(H,P,V)
END FUNCTION
INTEGER FUNCTION DftiSetValueDMpz(H,P,V)
        USE MKL_CDFT_DM_TYPE
        TYPE(DFTI_DESCRIPTOR_DM), POINTER :: H
        INTEGER P
        COMPLEX(8) V(*)
        INTENT(IN) :: P,V
        INTERFACE
          INTEGER FUNCTION DftiSetValueDMp_internal(H,P,V)
            USE MKL_CDFT_DM_TYPE
            TYPE(DFTI_DESCRIPTOR_DM), POINTER :: H
            INTEGER P
            COMPLEX(8) V(*)
            INTENT(IN) :: P,V
          END FUNCTION
        END INTERFACE
        DftiSetValueDMpz = DftiSetValueDMp_internal(H,P,V)
END FUNCTION
INTEGER FUNCTION DftiSetValueDMps(H,P,V)
        USE MKL_CDFT_DM_TYPE
        TYPE(DFTI_DESCRIPTOR_DM), POINTER :: H
        INTEGER P
        REAL(4) V(*)
        INTENT(IN) :: P,V
        INTERFACE
          INTEGER FUNCTION DftiSetValueDMp_internal(H,P,V)
            USE MKL_CDFT_DM_TYPE
            TYPE(DFTI_DESCRIPTOR_DM), POINTER :: H
            INTEGER P
            REAL(4) V(*)
            INTENT(IN) :: P,V
          END FUNCTION
        END INTERFACE
        DftiSetValueDMps = DftiSetValueDMp_internal(H,P,V)
END FUNCTION
INTEGER FUNCTION DftiSetValueDMpd(H,P,V)
        USE MKL_CDFT_DM_TYPE
        TYPE(DFTI_DESCRIPTOR_DM), POINTER :: H
        INTEGER P
        REAL(8) V(*)
        INTENT(IN) :: P,V
        INTERFACE
          INTEGER FUNCTION DftiSetValueDMp_internal(H,P,V)
            USE MKL_CDFT_DM_TYPE
            TYPE(DFTI_DESCRIPTOR_DM), POINTER :: H
            INTEGER P
            REAL(8) V(*)
            INTENT(IN) :: P,V
          END FUNCTION
        END INTERFACE
        DftiSetValueDMpd = DftiSetValueDMp_internal(H,P,V)
END FUNCTION
INTEGER FUNCTION DftiSetValueDMpi(H,P,V)
        USE MKL_CDFT_DM_TYPE
        TYPE(DFTI_DESCRIPTOR_DM), POINTER :: H
        INTEGER P,V(*)
        INTENT(IN) :: P,V
        INTERFACE
          INTEGER FUNCTION DftiSetValueDMp_internal(H,P,V)
            USE MKL_CDFT_DM_TYPE
            TYPE(DFTI_DESCRIPTOR_DM), POINTER :: H
            INTEGER P,V(*)
            INTENT(IN) :: P,V
          END FUNCTION
        END INTERFACE
        DftiSetValueDMpi = DftiSetValueDMp_internal(H,P,V)
END FUNCTION
!END INTERFACE DftiSetValueDM
!INTERFACE DftiGetValueDM
INTEGER FUNCTION DftiGetValueDMs(H,P,V)
        USE MKL_CDFT_DM_TYPE
        TYPE(DFTI_DESCRIPTOR_DM), POINTER :: H
        INTEGER P
        REAL(4) V
        INTENT(IN)  :: P
        INTENT(OUT) :: V
        INTERFACE
          INTEGER FUNCTION DftiGetValueDM_internal(H,P,V)
            USE MKL_CDFT_DM_TYPE
            TYPE(DFTI_DESCRIPTOR_DM), POINTER :: H
            INTEGER P
            REAL(4) V
            INTENT(IN)  :: P
            INTENT(OUT) :: V
          END FUNCTION
        END INTERFACE
        DftiGetValueDMs = DftiGetValueDM_internal(H,P,V)
END FUNCTION
INTEGER FUNCTION DftiGetValueDMd(H,P,V)
        USE MKL_CDFT_DM_TYPE
        TYPE(DFTI_DESCRIPTOR_DM), POINTER :: H
        INTEGER P
        REAL(8) V
        INTENT(IN)  :: P
        INTENT(OUT) :: V
        INTERFACE
          INTEGER FUNCTION DftiGetValueDM_internal(H,P,V)
            USE MKL_CDFT_DM_TYPE
            TYPE(DFTI_DESCRIPTOR_DM), POINTER :: H
            INTEGER P
            REAL(8) V
            INTENT(IN)  :: P
            INTENT(OUT) :: V
          END FUNCTION
        END INTERFACE
        DftiGetValueDMd = DftiGetValueDM_internal(H,P,V)
END FUNCTION
INTEGER FUNCTION DftiGetValueDMi(H,P,V)
        USE MKL_CDFT_DM_TYPE
        TYPE(DFTI_DESCRIPTOR_DM), POINTER :: H
        INTEGER P,V
        INTENT(IN)  :: P
        INTENT(OUT) :: V
        INTERFACE
          INTEGER FUNCTION DftiGetValueDM_internal(H,P,V)
            USE MKL_CDFT_DM_TYPE
            TYPE(DFTI_DESCRIPTOR_DM), POINTER :: H
            INTEGER P,V
            INTENT(IN)  :: P
            INTENT(OUT) :: V
          END FUNCTION
        END INTERFACE
        DftiGetValueDMi = DftiGetValueDM_internal(H,P,V)
END FUNCTION
!END INTERFACE DftiGetValueDM
!INTERFACE DftiComputeForwardDM
    INTEGER FUNCTION DftiComputeForwardDMoc(H,IN,OUT)
        USE MKL_CDFT_DM_TYPE
        TYPE(DFTI_DESCRIPTOR_DM), POINTER :: H
        COMPLEX(4) IN(*),OUT(*)
        INTENT(IN)  :: IN
        INTENT(OUT) :: OUT
        INTERFACE
          INTEGER FUNCTION DftiComputeForwardDMo_internal(H,IN,OUT)
            USE MKL_CDFT_DM_TYPE
            TYPE(DFTI_DESCRIPTOR_DM), POINTER :: H
            COMPLEX(4) IN(*),OUT(*)
            INTENT(IN)  :: IN
            INTENT(OUT) :: OUT
          END FUNCTION
        END INTERFACE
        DftiComputeForwardDMoc = DftiComputeForwardDMo_internal(H,IN,OUT) 
    END FUNCTION
    INTEGER FUNCTION DftiComputeForwardDMoz(H,IN,OUT)
        USE MKL_CDFT_DM_TYPE
        TYPE(DFTI_DESCRIPTOR_DM), POINTER :: H
        COMPLEX(8) IN(*),OUT(*)
        INTENT(IN)  :: IN
        INTENT(OUT) :: OUT
        INTERFACE
          INTEGER FUNCTION DftiComputeForwardDMo_internal(H,IN,OUT)
            USE MKL_CDFT_DM_TYPE
            TYPE(DFTI_DESCRIPTOR_DM), POINTER :: H
            COMPLEX(8) IN(*),OUT(*)
            INTENT(IN)  :: IN
            INTENT(OUT) :: OUT
          END FUNCTION
        END INTERFACE
        DftiComputeForwardDMoz = DftiComputeForwardDMo_internal(H,IN,OUT) 
    END FUNCTION
    INTEGER FUNCTION DftiComputeForwardDMic(H,IN)
        USE MKL_CDFT_DM_TYPE
        TYPE(DFTI_DESCRIPTOR_DM), POINTER :: H
        COMPLEX(4) IN(*)
        INTERFACE
          INTEGER FUNCTION DftiComputeForwardDMi_internal(H,IN)
            USE MKL_CDFT_DM_TYPE
            TYPE(DFTI_DESCRIPTOR_DM), POINTER :: H
            COMPLEX(4) IN(*)
          END FUNCTION
        END INTERFACE
        DftiComputeForwardDMic = DftiComputeForwardDMi_internal(H,IN) 
    END FUNCTION
    INTEGER FUNCTION DftiComputeForwardDMiz(H,IN)
        USE MKL_CDFT_DM_TYPE
        TYPE(DFTI_DESCRIPTOR_DM), POINTER :: H
        COMPLEX(8) IN(*)
        INTERFACE
          INTEGER FUNCTION DftiComputeForwardDMi_internal(H,IN)
            USE MKL_CDFT_DM_TYPE
            TYPE(DFTI_DESCRIPTOR_DM), POINTER :: H
            COMPLEX(8) IN(*)
          END FUNCTION
        END INTERFACE
        DftiComputeForwardDMiz = DftiComputeForwardDMi_internal(H,IN) 
    END FUNCTION
!END INTERFACE DftiComputeForwardDM
!INTERFACE DftiComputeBackwardDM
INTEGER FUNCTION DftiComputeBackwardDMoc(H,IN,OUT)
        USE MKL_CDFT_DM_TYPE
        TYPE(DFTI_DESCRIPTOR_DM), POINTER :: H
        COMPLEX(4) IN(*),OUT(*)
        INTENT(IN)  :: IN
        INTENT(OUT) :: OUT
        INTERFACE
          INTEGER FUNCTION DftiComputeBackwardDMo_internal(H,IN,OUT)
            USE MKL_CDFT_DM_TYPE
            TYPE(DFTI_DESCRIPTOR_DM), POINTER :: H
            COMPLEX(4) IN(*),OUT(*)
            INTENT(IN)  :: IN
            INTENT(OUT) :: OUT
          END FUNCTION
        END INTERFACE
        DftiComputeBackwardDMoc = DftiComputeBackwardDMo_internal(H,IN,OUT) 
END FUNCTION
INTEGER FUNCTION DftiComputeBackwardDMoz(H,IN,OUT)
        USE MKL_CDFT_DM_TYPE
        TYPE(DFTI_DESCRIPTOR_DM), POINTER :: H
        COMPLEX(8) IN(*),OUT(*)
        INTENT(IN)  :: IN
        INTENT(OUT) :: OUT
        INTERFACE
          INTEGER FUNCTION DftiComputeBackwardDMo_internal(H,IN,OUT)
            USE MKL_CDFT_DM_TYPE
            TYPE(DFTI_DESCRIPTOR_DM), POINTER :: H
            COMPLEX(8) IN(*),OUT(*)
            INTENT(IN)  :: IN
            INTENT(OUT) :: OUT
          END FUNCTION
        END INTERFACE
        DftiComputeBackwardDMoz = DftiComputeBackwardDMo_internal(H,IN,OUT) 
END FUNCTION
INTEGER FUNCTION DftiComputeBackwardDMic(H,IN)
        USE MKL_CDFT_DM_TYPE
        TYPE(DFTI_DESCRIPTOR_DM), POINTER :: H
        COMPLEX(4) IN(*)
        INTERFACE
          INTEGER FUNCTION DftiComputeBackwardDMi_internal(H,IN)
            USE MKL_CDFT_DM_TYPE
            TYPE(DFTI_DESCRIPTOR_DM), POINTER :: H
            COMPLEX(4) IN(*)
          END FUNCTION
        END INTERFACE
        DftiComputeBackwardDMic = DftiComputeBackwardDMi_internal(H,IN) 
END FUNCTION
INTEGER FUNCTION DftiComputeBackwardDMiz(H,IN)
        USE MKL_CDFT_DM_TYPE
        TYPE(DFTI_DESCRIPTOR_DM), POINTER :: H
        COMPLEX(8) IN(*)
        INTERFACE
          INTEGER FUNCTION DftiComputeBackwardDMi_internal(H,IN)
            USE MKL_CDFT_DM_TYPE
            TYPE(DFTI_DESCRIPTOR_DM), POINTER :: H
            COMPLEX(8) IN(*)
          END FUNCTION
        END INTERFACE
        DftiComputeBackwardDMiz = DftiComputeBackwardDMi_internal(H,IN) 
END FUNCTION
!END INTERFACE DftiComputeBackwardDM
END MODULE
