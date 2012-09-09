MODULE SUB_Randomize
!!#### PURPOSE
!! Returns RANDOMized x-values in the interval defined by [l]:
!
!!              CALL Randomize( x , l(1:2) )
!
!! where x satisfies l(1) <= x <  l(2) for reals, complex, and integer x.

!!#### DETAILS
!! Note this USEs a basic call to the intrinsic Randomize number generator,
!! CALL RANDOM_NUMBER( rand ).  Therefore, one should initialize the
!! Randomize number generator seed with CALL RANDOM_SEED() before
!! calling Randomize.

!!#### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp,& !!((01-A-KND_IntrinsicTypes.f90))
              KIND_Csp,KIND_Cdp,&
              KIND_I1,KIND_I2,KIND_I4,KIND_I8

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

!!#### SUBROUTINE OVERLOADING
INTERFACE Randomize
 MODULE PROCEDURE Randomize_A0I1
 MODULE PROCEDURE Randomize_A0I2
 MODULE PROCEDURE Randomize_A0I4
 MODULE PROCEDURE Randomize_A0I8
 MODULE PROCEDURE Randomize_A0Rsp
 MODULE PROCEDURE Randomize_A0Rdp
 MODULE PROCEDURE Randomize_A0Csp
 MODULE PROCEDURE Randomize_A0Cdp
 MODULE PROCEDURE Randomize_A1I1
 MODULE PROCEDURE Randomize_A1I2
 MODULE PROCEDURE Randomize_A1I4
 MODULE PROCEDURE Randomize_A1I8
 MODULE PROCEDURE Randomize_A1Rsp
 MODULE PROCEDURE Randomize_A1Rdp
 MODULE PROCEDURE Randomize_A1Csp
 MODULE PROCEDURE Randomize_A1Cdp
 MODULE PROCEDURE Randomize_A2I1
 MODULE PROCEDURE Randomize_A2I2
 MODULE PROCEDURE Randomize_A2I4
 MODULE PROCEDURE Randomize_A2I8
 MODULE PROCEDURE Randomize_A2Rsp
 MODULE PROCEDURE Randomize_A2Rdp
 MODULE PROCEDURE Randomize_A2Csp
 MODULE PROCEDURE Randomize_A2Cdp
END INTERFACE

!!#### PUBLIC ACCESS LIST
PUBLIC :: Randomize


CONTAINS


SUBROUTINE Randomize_A0I1( x , l )
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I1     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-SUB_Randomize_A0I.f90.hdr"
!!--begin--
INCLUDE "03-A-SUB_Randomize_I.f90.bdy"
!!--end--
END SUBROUTINE


SUBROUTINE Randomize_A0I2( x , l )
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I2     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-SUB_Randomize_A0I.f90.hdr"
!!--begin--
INCLUDE "03-A-SUB_Randomize_I.f90.bdy"
!!--end--
END SUBROUTINE


SUBROUTINE Randomize_A0I4( x , l )
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I4     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-SUB_Randomize_A0I.f90.hdr"
!!--begin--
INCLUDE "03-A-SUB_Randomize_I.f90.bdy"
!!--end--
END SUBROUTINE


SUBROUTINE Randomize_A0I8( x , l )
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I8     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-SUB_Randomize_A0I.f90.hdr"
!!--begin--
INCLUDE "03-A-SUB_Randomize_I.f90.bdy"
!!--end--
END SUBROUTINE


SUBROUTINE Randomize_A0Rsp( x , l )
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp    !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-SUB_Randomize_A0R.f90.hdr"
!!--begin--
INCLUDE "03-A-SUB_Randomize_R.f90.bdy"
!!--end--
END SUBROUTINE


SUBROUTINE Randomize_A0Rdp( x , l )
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp    !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-SUB_Randomize_A0R.f90.hdr"
!!--begin--
INCLUDE "03-A-SUB_Randomize_R.f90.bdy"
!!--end--
END SUBROUTINE


SUBROUTINE Randomize_A0Csp( x , l )
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp    !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-SUB_Randomize_A0C.f90.hdr"
!!--begin--
INCLUDE "03-A-SUB_Randomize_C.f90.bdy"
!!--end--
END SUBROUTINE


SUBROUTINE Randomize_A0Cdp( x , l )
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp    !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-SUB_Randomize_A0C.f90.hdr"
!!--begin--
INCLUDE "03-A-SUB_Randomize_C.f90.bdy"
!!--end--
END SUBROUTINE


SUBROUTINE Randomize_A1I1( y , l )
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I1     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-SUB_Randomize_A1I.f90.hdr"
!!--begin--
DO i=1,SIZE(y)
 INCLUDE "03-A-SUB_Randomize_I.f90.bdy"
 y(i) = x
END DO
!!--end--
END SUBROUTINE


SUBROUTINE Randomize_A1I2( y , l )
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I2     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-SUB_Randomize_A1I.f90.hdr"
!!--begin--
DO i=1,SIZE(y)
 INCLUDE "03-A-SUB_Randomize_I.f90.bdy"
 y(i) = x
END DO
!!--end--
END SUBROUTINE


SUBROUTINE Randomize_A1I4( y , l )
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I4     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-SUB_Randomize_A1I.f90.hdr"
!!--begin--
DO i=1,SIZE(y)
 INCLUDE "03-A-SUB_Randomize_I.f90.bdy"
 y(i) = x
END DO
!!--end--
END SUBROUTINE


SUBROUTINE Randomize_A1I8( y , l )
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I8     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-SUB_Randomize_A1I.f90.hdr"
!!--begin--
DO i=1,SIZE(y)
 INCLUDE "03-A-SUB_Randomize_I.f90.bdy"
 y(i) = x
END DO
!!--end--
END SUBROUTINE


SUBROUTINE Randomize_A1Rsp( y , l )
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp    !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-SUB_Randomize_A1R.f90.hdr"
!!--begin--
DO i=1,SIZE(y)
 INCLUDE "03-A-SUB_Randomize_R.f90.bdy"
 y(i) = x
END DO
!!--end--
END SUBROUTINE


SUBROUTINE Randomize_A1Rdp( y , l )
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp    !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-SUB_Randomize_A1R.f90.hdr"
!!--begin--
DO i=1,SIZE(y)
 INCLUDE "03-A-SUB_Randomize_R.f90.bdy"
 y(i) = x
END DO
!!--end--
END SUBROUTINE


SUBROUTINE Randomize_A1Csp( y , l )
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp    !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-SUB_Randomize_A1C.f90.hdr"
!!--begin--
DO i=1,SIZE(y)
 INCLUDE "03-A-SUB_Randomize_C.f90.bdy"
 y(i) = x
END DO
!!--end--
END SUBROUTINE


SUBROUTINE Randomize_A1Cdp( y , l )
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp    !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-SUB_Randomize_A1C.f90.hdr"
!!--begin--
DO i=1,SIZE(y)
 INCLUDE "03-A-SUB_Randomize_C.f90.bdy"
 y(i) = x
END DO
!!--end--
END SUBROUTINE


SUBROUTINE Randomize_A2I1( y , l )
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I1     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-SUB_Randomize_A2I.f90.hdr"
!!--begin--
DO j=1,SIZE(y,2)
 DO i=1,SIZE(y,1)
  INCLUDE "03-A-SUB_Randomize_I.f90.bdy"
  y(i,j) = x
 END DO
END DO
!!--end--
END SUBROUTINE


SUBROUTINE Randomize_A2I2( y , l )
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I2     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-SUB_Randomize_A2I.f90.hdr"
!!--begin--
DO j=1,SIZE(y,2)
 DO i=1,SIZE(y,1)
  INCLUDE "03-A-SUB_Randomize_I.f90.bdy"
  y(i,j) = x
 END DO
END DO
!!--end--
END SUBROUTINE


SUBROUTINE Randomize_A2I4( y , l )
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I4     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-SUB_Randomize_A2I.f90.hdr"
!!--begin--
DO j=1,SIZE(y,2)
 DO i=1,SIZE(y,1)
  INCLUDE "03-A-SUB_Randomize_I.f90.bdy"
  y(i,j) = x
 END DO
END DO
!!--end--
END SUBROUTINE


SUBROUTINE Randomize_A2I8( y , l )
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I8     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-SUB_Randomize_A2I.f90.hdr"
!!--begin--
DO j=1,SIZE(y,2)
 DO i=1,SIZE(y,1)
  INCLUDE "03-A-SUB_Randomize_I.f90.bdy"
  y(i,j) = x
 END DO
END DO
!!--end--
END SUBROUTINE


SUBROUTINE Randomize_A2Rsp( y , l )
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp    !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-SUB_Randomize_A2R.f90.hdr"
!!--begin--
DO j=1,SIZE(y,2)
 DO i=1,SIZE(y,1)
  INCLUDE "03-A-SUB_Randomize_R.f90.bdy"
  y(i,j) = x
 END DO
END DO
!!--end--
END SUBROUTINE


SUBROUTINE Randomize_A2Rdp( y , l )
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp    !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-SUB_Randomize_A2R.f90.hdr"
!!--begin--
DO j=1,SIZE(y,2)
 DO i=1,SIZE(y,1)
  INCLUDE "03-A-SUB_Randomize_R.f90.bdy"
  y(i,j) = x
 END DO
END DO
!!--end--
END SUBROUTINE


SUBROUTINE Randomize_A2Csp( y , l )
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp    !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-SUB_Randomize_A2C.f90.hdr"
!!--begin--
DO j=1,SIZE(y,2)
 DO i=1,SIZE(y,1)
  INCLUDE "03-A-SUB_Randomize_C.f90.bdy"
  y(i,j) = x
 END DO
END DO
!!--end--
END SUBROUTINE


SUBROUTINE Randomize_A2Cdp( y , l )
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp    !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-SUB_Randomize_A2C.f90.hdr"
!!--begin--
DO j=1,SIZE(y,2)
 DO i=1,SIZE(y,1)
  INCLUDE "03-A-SUB_Randomize_C.f90.bdy"
  y(i,j) = x
 END DO
END DO
!!--end--
END SUBROUTINE


ENDMODULE
