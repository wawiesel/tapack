!!!xtratex -useversion=0.5 -comment="see http://www.qbazaar.net/xtratex"

!!# PARAMETERS MODULE: <PAR_Constants_Rdp>
MODULE PAR_Constants_Rdp

!!## PURPOSE
!! Provide a user with named real constants of double precision to
!! avoid defining values in each procedure (or module) that needs them.



!!## USAGE
!! The constants available through this module all appear in the
!! header file "02-A-PAR_Constants_R.f90.hdr".  The naming convention
!! to reference a constant is as follows:
!!
!! @ All constants are preceeded by "c_".
!! @ Tokens are delimeted by underscores.
!! @ Tokens may be numeric 0-9, named constants, or
!!   mathematical operators.
!!   @ mathematical operators
!!      1. <BY>      : division of numerator (all tokens before <BY>)
!!                     and denomenator (all tokens after <BY>)
!!      2. <SQRT>    : square root of next token
!!      3. <SQR>     : square of next token
!!      4. <TIMES>   : multiplication of previous and next token
!!   @ named constants
!!      1. <PI>      : irrational number $\pi$
!!      2. <E>       : irrational number $e$



!!## EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R => KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))



!!## HISTORY
! 1.[waw] Author   = W. A. Wieselquist
!         Modified = 104.2006
!         Contact  = william.wieselquist AT gmail.com


!!## DEFAULT IMPLICIT
IMPLICIT NONE



!!## DEFAULT ACCESS
PUBLIC



!!## GLOBAL PARAMETERS
INCLUDE "02-A-PAR_Constants_R.f90.hdr"



!!## PRIVATE ACCESS LIST
PRIVATE :: KIND_R



END MODULE
