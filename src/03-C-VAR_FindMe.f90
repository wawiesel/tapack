!!# VARIABLE MODULE <<VAR_FindMe>>
MODULE VAR_FindMe

!!## PURPOSE
!! A simple variable which I use to set a "manual breakpoint"
!! in a certain part of a code under conditions that can
!! only be specified easily somewhere else in the code.



!!## USAGE
!! First set <FindMe> where the conditions
!! are available.
!
!  =================
!     conditional
!  =================
!
!  IF( SomeCondition )THEN
!   FindMe = 776
!  ELSE
!   FindMe = 0
!  END IF
!
!  =================
!
!
!! Then in another part, add <USE VAR_FindMe> in the
!! appropriate preamble, and then have some breakpoint.
!
!  =================
!      breakpoint
!  =================
!
!  IF( FindMe==776 )THEN
!   776 CONTINUE
!  END IF
!
!  =================
!
!!
!! This keeps from having to have the variables needed to
!! calculate <SomeCondition> local to the part of the code

INTEGER :: FindMe = 0

END MODULE
