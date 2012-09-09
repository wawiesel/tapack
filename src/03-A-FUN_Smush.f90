!!#### MODULE: FUNCTION <Smush>
MODULE FUN_Smush
!!### PURPOSE
!! Reduce multiple consecutive spaces to one space.
!!


!!### AUTHOR
!! William A. Wieselquist | william.wieselquist@gmail.com


!!### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_S !!((01-A-KND_IntrinsicTypes.f90))


!!### DEFAULT IMPLICIT
IMPLICIT NONE


!!### LOCAL KINDS
!INTEGER,PARAMETER :: KIND_S = KIND("ABCDEFGHIJKLMNOPQRSTUVWXYZ")


!!### INTERNAL IDENTIFICATION
CHARACTER(LEN=*,KIND=KIND_S),PARAMETER :: file_ = "03-A-FUN_Smush.f90"
CHARACTER(LEN=*,KIND=KIND_S),PARAMETER :: mod_  = "FUN_Smush"


!!### DEFAULT ACCESS
PRIVATE


!!### INTERFACES
!! @ the version invoked with <Smush>
INTERFACE Smush
 MODULE PROCEDURE Smush_mixed
END INTERFACE
!! @ <Smush0>, mixed sequential/parallel operations
INTERFACE Smush0
 MODULE PROCEDURE Smush_mixed
END INTERFACE
!! @ <Smush1>, strict sequential
INTERFACE Smush1
 MODULE PROCEDURE Smush_sequential
END INTERFACE
!! @ <Smush2>, strict parallel
INTERFACE Smush2
 MODULE PROCEDURE Smush_sequential
END INTERFACE


!!### PUBLIC
PUBLIC :: Smush0
PUBLIC :: Smush1
PUBLIC :: Smush2
PUBLIC :: Smush


!!### MODULE PROCEDURES
CONTAINS


!!## PURE FUNCTION: Smush_mixed
PURE FUNCTION Smush_mixed( STRING ) RESULT(Smush)
!!# CONTRIBUTOR
!! NuclearWizard on usenet's fortran group.

!!# REQUIRED INPUT
!! @ input string <STRING>
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN) :: STRING

!!# RESULT
!! @ resultant string <Smush>
CHARACTER(LEN=LEN(STRING),KIND=KIND_S) :: Smush

!!# LOCAL VARIABLES
INTEGER                       :: i,j
CHARACTER(LEN=1,KIND=KIND_S) :: a(1:LEN(string))
LOGICAL                       :: b(1:LEN(string))

!!--begin--

!transfer to an array of characters to speed up assignments
!and references
a = TRANSFER(string,a)

!determine the array of blank"s existence, b
b = a==" "

!replace multiple blanks with a single one overwriting
!what"s in a already---we can do this because this is a
!"forward reaching" operation---we will never overwrite
!something we could potentiall use
j = 1
DO i=1,SIZE(a)-1
 IF( b(i) .AND. b(i+1) )CYCLE
 a(j) = a(i)
 j = j + 1
END DO
a(j) = a(i)

!fill the rest with blanks
FORALL(i=j+1:SIZE(a)) a(i) = " "

!transfer back to the string representation
Smush = transfer(a,Smush)

!!--end--
END FUNCTION



!!## PURE FUNCTION: Smush_parallel
pure function Smush_parallel(STRING)  result (Smush)
!!# CONTRIBUTOR
!! Frank on usenet's fortran group.

!!# REQUIRED INPUT
character(LEN=*,KIND=KIND_S),INTENT(IN) :: STRING

!!# REQUIRED OUTPUT
character(LEN=len(STRING),KIND=KIND_S) :: Smush

!!# LOCALS
integer                       :: i
character(LEN=1,KIND=KIND_S) :: a(1:len(STRING))
logical                       :: b(1:len(STRING))

!!--begin--

a = transfer(STRING,a)
b = a==" "
forall (i=1:size(a)-1, b(i).and.b(i+1)) a(i) = char(0)
Smush = transfer(pack(a,a /= char(0), spread(" ",1,size(a))),Smush)

!!--end--
end function Smush_parallel


!!## PURE FUNCTION: Smush_parallel
pure function Smush_sequential(STRING)  result (Smush)
!!# CONTRIBUTOR
!! Chandler of usenet's fortran group.

!!# REQUIRED INPUT
character(LEN=*,KIND=KIND_S),INTENT(IN) :: STRING

!!# REQUIRED OUTPUT
character(LEN=len(STRING),KIND=KIND_S) :: Smush

!!# LOCAL VARIABLES
integer :: p, q, l
logical :: f
character(LEN=1,KIND=KIND_S) :: c

!!--begin--

q=0
l=len(STRING)
f=.false.
Smush=" "

do p=1,l
  c=STRING(p:p)
  if (c .eq. " ") then
    if (.not. f) then
      q = q + 1
      Smush(q:q) = c
      f = .true.
    end if
  else
    q = q + 1
    Smush(q:q) = c
    f = .false.
  end if
end do

!!--end--
end function smush_sequential



END MODULE
