!!# COMMAND CARD SWITCHBOARD: DiscreteOrdinates
MODULE CCS_DiscreteOrdinates
!!## PURPOSE
!! The command card switchboard for the <DiscreteOrdinates> (<DOR>)
!! package.

!!### EXTERNAL PARAMETERS
USE PAR_DiscreteOrdinates            !!((03-A-PAR_DiscreteOrdinates.f90))

!!### FORTRAN STANDARDS MODULES
USE ISO_varying_string               !!((03-A-ISO_varying_string.f90))

!!### user MODULES
USE USR_fdbk                         !!((08-C-USR_fdbk.f90))

!!### FUNCTION MODULES
!! * extension to intrinsic INDEX
USE FUN_INDEXa                       !!((08-B-FUN_INDEXa.f90))
USE SUB_Pause                        !!((04-B-SUB_Pause.f90))
USE FUN_STR                          !!((05-B-FUN_STR.f90))
USE FUN_VSTR                         !!((05-B-FUN_VSTR.f90))
USE CC1_simple,ONLY: DORuniz=>simple !!((11-A-CC1_simple.f90))

!!### GLOBAL LIBRARIES
USE LIB_Prompts                      !!((06-B-LIB_Prompts.f90))

!!### GLOBAL TOOLBOXES
!! * input/output toolbox
!! * feeback object user module
USE TBX_SIO                          !!((10-A-TBX_SIO.f90))

!!### GLOBAL VARIABLES
USE VAR_DiscreteOrdinates            !!((47-B-VAR_DiscreteOrdinates.f90))

!!### DEFAULT IMPLICIT
IMPLICIT NONE

!!### PARAMTERS
!! * module name <mod_>
!! * residing file name <file_>
CHARACTER(*),PARAMETER :: mod_="CCS_DiscreteOrdinates"
CHARACTER(*),PARAMETER :: file_="48-B-CCS_DiscreteOrdinates.f90"

!!### DEFAULT ACCESS
PRIVATE

!!## PRE-CALCULATED HASH VALUES
INTEGER,PARAMETER :: prodquad_ = 00146232
INTEGER,PARAMETER :: levquad_  = 00076998
INTEGER,PARAMETER :: genquad_  = 00062632
INTEGER,PARAMETER :: uniz_     = 00014662

!!## ACCESS
PUBLIC :: SWITCHBOARD_DiscreteOrdinates


!!### CONTAINED PROCEDURES
CONTAINS


SUBROUTINE SWITCHBOARD_DiscreteOrdinates( sio , FdBk )
!!#### PURPOSE
!! The command DataBaSe for the DiscreteOrdinates package.

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <sio>
TYPE(TYPE_sio),POINTER :: sio

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <FdBk>
TYPE(TYPE_FdBk),OPTIONAL,INTENT(INOUT) :: FdBk

!!--begin--
!pick routine to execute which loads some data
SELECTCASE(sio%HASH(2))
 CASE(prodquad_) ; CALL DORprodquad( sio , QuadratureType , aQuadrature , pQuadrature , aOrder , pOrder , FdBk )
 CASE(levquad_)  ; CALL DORlevquad ( sio , QuadratureType , Quadrature  ,               aOrder , pOrder , FdBk )
 CASE(genquad_)  ; CALL DORgenquad ( sio , QuadratureType , Quadrature  , Order                         , FdBk )
 CASE(uniz_)     ; CALL DORuniz    ( sio , UniformZGeometry , FdBk )
 CASE DEFAULT    ; WRITE(*,"(a)")WarningPrompt//"DOR (Discrete ORdinates) command: \"//TRIM(STR(sio%cmd))&
                          //", NOT RECOGNIZED ON LINE="//TRIM(STR(sio%LINE_NUM))//" of FILE="//TRIM(STR(sio%FILE))
                    CALL Pause()
ENDSELECT

!!--end--
END SUBROUTINE


SUBROUTINE DORprodquad( sio , QuadratureType , &
  aQuadrature , pQuadrature , aOrder , pOrder , FdBk )
!!#### PURPOSE
!! Reads in product quadrature information.

!!#### PARAMETERS
!! * local procedure name <proc_>
CHARACTER(*),PARAMETER :: proc_="DORprodquad"

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <sio>
!! * type of the angular discretization <QuadratureType>
TYPE(TYPE_sio),POINTER     :: sio
INTEGER     ,INTENT(INOUT) :: QuadratureType
CHARACTER(*),INTENT(INOUT) :: aQuadrature,pQuadrature
INTEGER     ,INTENT(INOUT) :: aOrder,pOrder


!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <FdBk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
INTEGER :: jerr

!!--begin--

QuadratureType = DOR_PRODUCT

CALL BEGIN_ARGUMENTS( sio , (/"aQuadrature","pQuadrature",&
                              "aOrder     ","pOrder     "/) , FdBk )

CALL ARGUMENT( sio , aQuadrature , FdBk )
CALL ARGUMENT( sio , pQuadrature , FdBk )
CALL ARGUMENT( sio , aOrder      , FdBk )
CALL ARGUMENT( sio , pOrder      , FdBk )
CALL END_ARGUMENTS( sio , FdBk )

!!--end--
END SUBROUTINE


SUBROUTINE DORlevquad( sio , QuadratureType , &
  Quadrature , aOrder , pOrder , FdBk )
!!#### PURPOSE
!! Reads in level quadrature information.

!!#### PARAMETERS
!! * local procedure name <proc_>
CHARACTER(*),PARAMETER :: proc_="DORlevquad"

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <sio>
!! * type of the angular discretization <QuadratureType>
TYPE(TYPE_sio),POINTER     :: sio
INTEGER     ,INTENT(INOUT) :: QuadratureType
CHARACTER(*),INTENT(INOUT) :: Quadrature
INTEGER     ,INTENT(INOUT) :: aOrder,pOrder


!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <FdBk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
INTEGER :: jerr

!!--begin--

QuadratureType = DOR_LEVEL

CALL BEGIN_ARGUMENTS( sio , (/"Quadrature",&
                              "aOrder    ",&
                                                          "pOrder    "/) , FdBk , &
                                                          Optional=(/.FALSE.,.FALSE.,.TRUE./) )

CALL ARGUMENT( sio , Quadrature , FdBk )
CALL ARGUMENT( sio , aOrder     , FdBk )
CALL ARGUMENT( sio , pOrder     , FdBk , DEFAULT=aOrder )
CALL END_ARGUMENTS( sio , FdBk )

!!--end--
END SUBROUTINE


SUBROUTINE DORgenquad( sio , QuadratureType , &
  Quadrature , Order , FdBk )
!!#### PURPOSE
!! Reads in general quadrature information.

!!#### PARAMETERS
!! * local procedure name <proc_>
CHARACTER(*),PARAMETER :: proc_="DORgenquad"

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <sio>
!! * type of the angular discretization <QuadratureType>
TYPE(TYPE_sio),POINTER     :: sio
INTEGER     ,INTENT(INOUT) :: QuadratureType
CHARACTER(*),INTENT(INOUT) :: Quadrature
INTEGER     ,INTENT(INOUT) :: Order


!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <FdBk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
INTEGER :: jerr

!!--begin--

QuadratureType = DOR_GENERAL

CALL BEGIN_ARGUMENTS( sio , (/"Quadrature",&
                              "Order     "/) , FdBk , &
                            Optional=(/.FALSE.,.TRUE./) )

CALL ARGUMENT( sio , Quadrature , FdBk )
CALL ARGUMENT( sio , Order     , FdBk )
CALL END_ARGUMENTS( sio , FdBk )

!!--end--
END SUBROUTINE


END MODULE
