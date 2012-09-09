MODULE SUB_Words
!!#### PURPOSE
!! Set elements of word array <WORD_LIST> equal to chunks of
!! string S or array of strings A1S, separated by delim
!! or <DEFAULT_delim> if delim is not present.

!!#### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_S !!((01-A-KND_IntrinsicTypes.f90))

!!#### AUTHOR
!! William A. Wieselquist | william.wieselquist@gmail.com

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

!!#### Words PROCEDURE OVERLOADING
INTERFACE Words
 MODULE PROCEDURE Words0
END INTERFACE

!!#### LOCAL PARAMETERS
CHARACTER(LEN=1,KIND=KIND_S) :: DEFAULT_delim = " "

!!#### PUBLIC ACCESS LIST
PUBLIC :: Words


CONTAINS


PURE SUBROUTINE Words0(WORD_LIST,S,delim)
!!#### REQUIRED INPUT
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN)          :: S
CHARACTER(LEN=1,KIND=KIND_S),INTENT(IN),OPTIONAL :: delim
!!#### REQUIRED OUTPUT
CHARACTER(LEN=*,KIND=KIND_S),INTENT(OUT)         :: WORD_LIST(:)
!!#### LOCAL VARIABLES
INTEGER                          :: i,j,l
CHARACTER(LEN=LEN(S),KIND=KIND_S) :: ws
CHARACTER(LEN=1     ,KIND=KIND_S) :: delim_

!!--begin--
!get the delim
IF( PRESENT(delim) )THEN
 delim_ = delim
ELSE
 delim_ = DEFAULT_delim
END IF

!get all the words we can fit
ws = ADJUSTL(S)
i = 1
DO
 j = INDEX(ws,delim)
 IF( j/=0 )THEN
  WORD_LIST(i) = ws(1:j-1)
  FORALL( l=1:j ) ws(l:l) =" "
  ws = ADJUSTL(ws)
  IF( j>1 )THEN
   i = i + 1
  END IF

 ELSE
  i = i + 1
  IF( i>SIZE(WORD_LIST) )EXIT
  WORD_LIST(i) = ws
  EXIT
 END IF
 IF( i>SIZE(WORD_LIST) )EXIT
END DO

!!--end--
END SUBROUTINE


END MODULE
