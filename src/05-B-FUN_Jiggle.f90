MODULE FUN_Jiggle
!!#### PURPOSE
!! Defines the jiggle function, Jiggle(x), which alters
!! x by a very small (on the order of machine precsion),
!! *and* random amount.

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

!!#### FUNCTION OVERLOADING
INTERFACE Jiggle
 MODULE PROCEDURE Jiggle_A0Rsp
 MODULE PROCEDURE Jiggle_A0Rdp
 MODULE PROCEDURE Jiggle_A1Rsp
 MODULE PROCEDURE Jiggle_A1Rdp
 MODULE PROCEDURE Jiggle_A2Rsp
 MODULE PROCEDURE Jiggle_A2Rdp
END INTERFACE


!!#### PUBLIC ACCESS LIST
PUBLIC :: Jiggle


CONTAINS

FUNCTION Jiggle_A0Rsp( x , f ) RESULT(y)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-FUN_Jiggle_A0.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_Jiggle.f90.bdy"
!!--end--
END FUNCTION

FUNCTION Jiggle_A0Rdp( x , f ) RESULT(y)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-FUN_Jiggle_A0.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_Jiggle.f90.bdy"
!!--end--
END FUNCTION


FUNCTION Jiggle_A1Rsp( x ,f ) RESULT(y)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-FUN_Jiggle_A1.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_Jiggle.f90.bdy"
!!--end--
END FUNCTION

FUNCTION Jiggle_A1Rdp( x , f ) RESULT(y)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-FUN_Jiggle_A1.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_Jiggle.f90.bdy"
!!--end--
END FUNCTION

FUNCTION Jiggle_A2Rsp( x ,f ) RESULT(y)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-FUN_Jiggle_A2.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_Jiggle.f90.bdy"
!!--end--
END FUNCTION

FUNCTION Jiggle_A2Rdp( x , f ) RESULT(y)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-FUN_Jiggle_A2.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_Jiggle.f90.bdy"
!!--end--
END FUNCTION

END MODULE
