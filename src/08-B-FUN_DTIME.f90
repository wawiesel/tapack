MODULE FUN_DTIME
!!#### PURPOSE
!! Provides a simple function that returns the change in CPU_TIME
!! from the argument and updates the argument.

!!#### EXTERNAL PROCEDURES
USE SUB_Reallocate !!((04-B-SUB_Reallocate.f90))

!!#### USAGE 1
!
!!  DT = DTIME( T0 )
!
!! where DT is the change in time since T0 and T0 is now set
!! by a call to CALL CPU_TIME(T0).  If T0 is not supplied, then
!! DT is the change in time since CPU_TIME() was CALLed.

!!#### USAGE 2
!
!!  DT = DTIME( T1 , j )
!
!! where T1 is an array of time values.  On entry T1(j+1) is the
!! last time value.  On exit T1(j) is the change in time since
!! the last call.  j is icremented by one in preparation for the
!! next call.

!!#### SUGGESTIONS
!!  The subroutine DTIME is a USEful way to time blocks of code in
!!  procedures.
!
!!   T1 = 0.
!!   j  = 0
!!   DT = DTIME( T1 , j )
!!   <---code block 1 -------->
!!   DT = DTIME( T1 , j )
!!   <---code block 2 -------->
!!   DT = DTIME( T1 , j )
!!   <---code block 3 -------->
!!   DT = DTIME( T1 , j )
!!   <---code block 4 -------->
!!   DT = DTIME( T1 , j )
!
!!  produces the following values in T1.
!!
!!    T1(1) - the time before code block 1
!!    T1(2) - the time before code block 2
!!    T1(3) - the time before code block 3
!!    T1(4) - the time after code block 4


!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

!!#### GLOBAL PROCEDURE OVERLOADING
INTERFACE DTIME
 MODULE PROCEDURE DTIME0
 MODULE PROCEDURE DTIME1
END INTERFACE

!!#### GLOBAL VARIABLES
INTEGER,SAVE :: Increment_T1 = 3
REAL   ,SAVE :: Remember_T0  = 0.0

!!#### PUBLIC ACCESS LIST
PUBLIC :: DTIME
PUBLIC :: Remember_T0

CONTAINS

FUNCTION DTIME1(T1,j) RESULT(DT)
!!#### REQUIRED INPUT
REAL   ,POINTER       :: T1(:)
INTEGER,INTENT(INOUT) :: j
!!#### REQUIRED OUTPUT
REAL :: DT

!!--begin--
!initialize
IF( j<=0 )THEN
 IF( ASSOCIATED(T1) )THEN
  DEALLOCATE(T1)
  NULLIFY(T1)
 END IF
 j = 1

!increment
ELSE
 j = j + 1
END IF


!reallocate
IF( .NOT.ASSOCIATED(T1) )THEN
 CALL Reallocate( T1 , Increment_T1 , fill=0.)
ELSE IF( j>SIZE(T1) )THEN
 CALL Reallocate( T1 , Increment_T1 , fill=0.)
END IF

!update
CALL CPU_TIME(T1(j))

!choose the return value
IF( j==1 )THEN
 DT = 0.
ELSE
 DT = T1(j)-T1(j-1)
END IF

!!--end--
END FUNCTION


FUNCTION DTIME0(T0) RESULT(DT)
!!#### OPTIONAL INPUT/OUTPUT
REAL,OPTIONAL,INTENT(INOUT) :: T0

!!#### REQUIRED OUTPUT
REAL :: DT

!!#### LOCAL VARIABLES
REAL :: t

!!--begin--
CALL CPU_TIME(t)
IF( PRESENT(T0) )THEN
 DT = t - T0
 T0 = t
ELSE
 DT = t - remember_T0
END IF
remember_T0 = t
!!--end--
END FUNCTION

END MODULE
