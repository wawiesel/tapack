!!## MODULE: FUNCTION  ptr_INDICESa
MODULE FUN_ptr_INDICESa
!!### PURPOSE
!! The <ptr_INDICESa> function returns a pointer to a list
!! of indices that match <STRING> the contents of a string list.
!! of strings which is an exact match for STRING, ignoring
!! all blanks and providing a search from the back of
!! the list and case (in)sensitivity options.


!!### USAGE
!
!    I => ptr_INDICESa( STRING_LIST , STRING [, BACK , CASESEN ] )
!
!!  where <I> are the indices of <STRING> in the <STRING_LIST>
!!  and <BACK> specifies whether to start at the back of the array,
!!  and <CASESEN> is whether or not the search is case-sensitive.


!!### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_S !!((01-A-KND_IntrinsicTypes.f90))

!!### GLOBAL BINARY OPERATORS
USE BOP_sEQ                         !!((03-A-BOP_sEQ.f90))

!!### EXTERNAL PROCEDURES
USE FUN_Default                     !!((04-A-FUN_Default.f90))
USE SUB_Find                        !!((05-B-SUB_Find.f90))
USE SUB_CLEAR                       !!((04-A-SUB_CLEAR.f90))
USE SUB_Replace                     !!((06-B-SUB_Replace.f90))
USE SUB_Reallocate                  !!((04-B-SUB_Reallocate.f90))
USE FUN_INDEXa                      !!((08-B-FUN_INDEXa.f90))

!!### DEFAULT IMPLICIT
IMPLICIT NONE

!!### LOCAL PARAMETERS
!! @ default case sensitivity
!! @ default backwards search
LOGICAL,PARAMETER :: DEFAULT_CASESEN = .TRUE.
LOGICAL,PARAMETER :: DEFAULT_BACK    = .FALSE.

!!### DEFAULT ACCESS
PRIVATE

!!### PROCEDURE OVERLOADING
INTERFACE ptr_INDICESa
 MODULE PROCEDURE ptr_INDICESa_
END INTERFACE

!!### PUBLIC ACCESS LIST
PUBLIC :: ptr_INDICESa


!!## MODULE PROCEDURES
CONTAINS


FUNCTION ptr_INDICESa_( STRING_LIST , STRING , BACK , &
  CASESEN , START , WILDCARD ) RESULT(I)

!!#### REQUIRED INPUT
!! @ the list of strings to be checked <STRING_LIST>
!! @ the string to match <STRING>
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN) :: STRING_LIST(:)
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN) :: STRING

!!#### REQUIRED OUTPUT
!! @ the index in STRING_LIST (NULL() if not found) <I>
INTEGER,POINTER :: I(:)

!!#### OPTIONAL INPUT
!! @ whether to return the index from the back <BACK>
!! @ whether to consider the search case-sensitive <CASESEN>
!! @ where to start the search <START>
!! @ the wildcard character <WILDCARD>
LOGICAL                    ,INTENT(IN),OPTIONAL :: BACK
LOGICAL                    ,INTENT(IN),OPTIONAL :: CASESEN
INTEGER                    ,INTENT(IN),OPTIONAL :: START
CHARACTER(LEN=1,KIND=KIND_S),INTENT(IN),OPTIONAL :: WILDCARD

!!#### LOCAL VARIABLES
INTEGER :: N,J,K,max_n

!!--begin--
!initialize
ALLOCATE( I(SIZE(STRING_LIST)) )
n=1
max_n = SIZE(STRING_LIST)
j=0

!loop
DO

 k = INDEXa(STRING_LIST(n:max_n),STRING,BACK,CASESEN,START,WILDCARD)

 !kick out if nothing found
 IF( k==0 )EXIT

 !increment
 j = j + 1

 !shift index
 i(j) = k + n - 1

 !increment starting spot and allowed cells to check
 n = i(j)+1

 !if <n> is at the end of the cells array
 IF( n>max_n )EXIT


END DO

IF( j==0 )THEN
 DEALLOCATE( I )
 NULLIFY( I )
ELSE
 CALL REALLOCATE( I , j-SIZE(I) )
END IF

!!--end--
END FUNCTION


END MODULE
