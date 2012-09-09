!!## FUNCTION: RGBA_Random
MODULE FUN_RGBA_Random
!!### PURPOSE
!! Get a Hot (yellow->orange->red->pink) color scheme in
!! RGBA format with components <R,G,B,A> all in $[0,1]$.


!!### USAGE
!
!      RGBA = RGBA_Random( X )
!
!! where <RGBA(1:4)> is an array containing each of the components
!! in that order, <X> is the input intensity to generate the color.


!!### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))

!!### EXTERNAL PROCEDURES
USE SUB_Randomize                              !!((03-A-SUB_Randomize.f90))

!!### DEFAULT IMPLICIT
IMPLICIT NONE

!!### DEFAULT ACCESS
PRIVATE

!!### PROCEDURE OVERLOADING
INTERFACE RGBA_Random
 MODULE PROCEDURE RGBA_Random_Rsp
 MODULE PROCEDURE RGBA_Random_Rdp
END INTERFACE

!!### PUBLIC ACCESS LIST
PUBLIC :: RGBA_Random


!!## MODULE PROCEDURES
CONTAINS

!!### FUNCTION: RGBA_Random_Rsp
FUNCTION RGBA_Random_Rsp( X ) RESULT(RGBA)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-FUN_RGBA_Random.f90.hdr"
!!--begin--
INCLUDE "04-B-FUN_RGBA_Random.f90.bdy"
!!--end--
END FUNCTION


!!### FUNCTION: RGBA_Random_Rdp
FUNCTION RGBA_Random_Rdp( X ) RESULT(RGBA)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-FUN_RGBA_Random.f90.hdr"
!!--begin--
INCLUDE "04-B-FUN_RGBA_Random.f90.bdy"
!!--end--
END FUNCTION


END MODULE
