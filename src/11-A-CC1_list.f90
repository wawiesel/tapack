!!## COMMAND CARD: list
MODULE CC1_list
!!### PURPOSE
!! Defines the command card which reads in a list of strings and turns
!! them into integers.

!!#### USAGE
!! The command appears as follows:
!
! \list{num}
!
!  string-1
!  string-2
!  string-3
!     .
!     .
!     .
!  string-num
!
!! and the possible values for string-i are found
!! in the array <KEYS> passed into this routine.  The output
!! is the index of each string in the <KEYS> array.


!!#### MODULES
!! @ input/output toolbox
!! @ feeback object
!! @ special index function
USE TBX_SIO     !!((10-A-TBX_SIO.f90))
USE USR_fdbk    !!((08-C-USR_fdbk.f90))
USE FUN_INDEXa  !!((08-B-FUN_INDEXa.f90))
USE FUN_Default !!((04-A-FUN_Default.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

!!#### ACCESS
PUBLIC :: list


CONTAINS



SUBROUTINE list( sio , INDICES , KEYS , FdBk , CaseSen )
!!#### REQUIRED INPUT/OUTPUT
!! @ input/output object <io>
!! @ list of integers which correspond to those names
!! @ list of names
TYPE(TYPE_SIO),POINTER       :: sio
INTEGER       ,POINTER       :: INDICES(:)
CHARACTER(*)  ,INTENT(IN)    :: KEYS(:)
LOGICAL       ,INTENT(IN),OPTIONAL :: CaseSen

!!#### OPTIONAL INPUT/OUTPUT
!! @ feedback object <FdBk>
TYPE(TYPE_FdBk),OPTIONAL,INTENT(INOUT) :: FdBk

!!#### LOCAL VARIABLES
INTEGER :: num
LOGICAL :: CaseSen_

!!--begin--

CaseSen_ = Default( .TRUE. , CaseSen )

CALL BEGIN_ARGUMENTS(sio,(/"num"/),FdBk)
CALL ARGUMENT(sio,num,FdBk)
CALL END_ARGUMENTS(sio,FdBk)

CALL DATABLOCK(sio,(/1,num/),INDICES,FdBk,Keys=KEYS,CaseSen=CaseSen_)

!!--end--
END SUBROUTINE


END MODULE
