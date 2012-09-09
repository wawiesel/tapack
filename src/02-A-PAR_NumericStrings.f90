!!# PARAMETERS MODULE: <PAR_NumericStrings>
MODULE PAR_NumericStrings

!!## PURPOSE
!! Define the characters which can appear in
!! numeric strings.



!!## EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_S !!((01-A-KND_IntrinsicTypes.f90))



!!## HISTORY
! 1.[waw] Author   = William A. Wieselquist
!         Modified = 104.2006
!         Contact  = william.wieselquist AT gmail.com



!!## DEFAULT IMPLICIT
IMPLICIT NONE



!!## DEFAULT ACCESS
PUBLIC



!!## EXTERNAL PARAMETERS
CHARACTER(LEN=*,KIND=KIND_S),PARAMETER :: decimal_     = "."
CHARACTER(LEN=*,KIND=KIND_S),PARAMETER :: digits_      = "0123456789"
CHARACTER(LEN=*,KIND=KIND_S),PARAMETER :: exponents_   = "Ee"
CHARACTER(LEN=*,KIND=KIND_S),PARAMETER :: signs_       = "+-"
CHARACTER(LEN=*,KIND=KIND_S),PARAMETER :: bool_        = "TF"
CHARACTER(LEN=*,KIND=KIND_S),PARAMETER :: orderedpair_ = "(,)"
CHARACTER(LEN=*,KIND=KIND_S),PARAMETER :: float_       = signs_//decimal_//digits_
CHARACTER(LEN=*,KIND=KIND_S),PARAMETER :: scientific_  = signs_//decimal_//digits_//exponents_
CHARACTER(LEN=*,KIND=KIND_S),PARAMETER :: cfloat_      = signs_//decimal_//digits_//orderedpair_
CHARACTER(LEN=*,KIND=KIND_S),PARAMETER :: cscientific_ = signs_//decimal_//digits_//exponents_//orderedpair_
CHARACTER(LEN=*,KIND=KIND_S),PARAMETER :: signedint_   = signs_//digits_
CHARACTER(LEN=*,KIND=KIND_S),PARAMETER :: unsignedint_ = digits_



END MODULE
