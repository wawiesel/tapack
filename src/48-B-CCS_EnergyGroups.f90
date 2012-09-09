!!# COMMAND CARD SWITCHBOARD: <<CCS_EnergyGroups>>
MODULE CCS_EnergyGroups
!!## PURPOSE
!! The command card switchboard for the <EnergyGroups>
!! (<NRG>) package.

!!## FORTRAN STANDARDS MODULES
USE ISO_varying_string !!((03-A-ISO_varying_string.f90))

!!## EXTERNAL PROCEDURES
USE SUB_Pause          !!((04-B-SUB_Pause.f90))
USE FUN_STR            !!((05-B-FUN_STR.f90))
USE FUN_VSTR           !!((05-B-FUN_VSTR.f90))

!!## GLOBAL USER MODULE
USE USR_fdbk           !!((08-C-USR_fdbk.f90))

!!## GLOBAL LIBRARIES
USE LIB_Prompts        !!((06-B-LIB_Prompts.f90))

!!## GLOBAL TOOLBOXES
USE TBX_SIO            !!((10-A-TBX_SIO.f90))

!!## GLOBAL VARIABLES
USE VAR_EnergyGroups   !!((47-B-VAR_EnergyGroups.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## DEFAULT ACCESS
PRIVATE

!!## LOCAL PARAMETERS
INTEGER,PARAMETER :: init_    = 0000009704

!!## IDENTIFICATION
CHARACTER(*),PARAMETER :: file_ = "48-B-CCS_EnergyGroups.f90"
CHARACTER(*),PARAMETER :: mod_  = "CCS_EnergyGroups"

!!## PUBLIC ACCESS LIST
PUBLIC :: SWITCHBOARD_EnergyGroups


!!## MODULE PROCEDURES
CONTAINS


!!### SUBROUTINE: SWITCHBOARD_EnergyGroups
SUBROUTINE SWITCHBOARD_EnergyGroups( sio , FdBk )
!!#### PURPOSE
!! The command switchboard for the EnergyGroups package.

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object [sio]
TYPE(TYPE_sio),POINTER :: sio

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object [FdBk]
TYPE(TYPE_FdBk),OPTIONAL,INTENT(INOUT) :: FdBk

!!--begin--
!pick routine to execute which loads some data
SELECTCASE(sio%HASH(2))

 CASE(init_)   ; CALL NRGinit( sio , NG , NGFast , NGThermal , FdBk )

 CASE DEFAULT  ; WRITE(*,"(a)")WarningPrompt//"NRG (energy) command: \"//TRIM(STR(sio%cmd))&
                   //", NOT RECOGNIZED ON LINE="//TRIM(STR(sio%LINE_NUM))//" of FILE="//TRIM(STR(sio%FILE))
                 CALL Pause()

END SELECT

!!--end--
ENDSUBROUTINE



SUBROUTINE NRGinit( sio , NG , NGfast , NGthermal , FdBk )
!!#### PURPOSE
!! Initialize.

!!#### PARAMETERS
CHARACTER(*),PARAMETER :: proc_="NRGinit"

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object [sio]
!! * number of energy groups [NG,NGfast,NGthermal]
TYPE(TYPE_sio),POINTER       :: sio
INTEGER       ,INTENT(INOUT) :: NG,NGfast,NGthermal

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object [FdBk]
TYPE(TYPE_FdBk),OPTIONAL,INTENT(INOUT) :: FdBk

!!--begin--
CALL BEGIN_ARGUMENTS( sio , (/"NG       ",&
                              "NGfast   ",&
                                                         "NGthermal"/) , FdBk )
CALL ARGUMENT( sio , NG        , FdBk )
CALL ARGUMENT( sio , NGfast    , FdBk )
CALL ARGUMENT( sio , NGthermal , FdBk )
CALL END_ARGUMENTS( sio , FdBk )

!!--end--
ENDSUBROUTINE


ENDMODULE
