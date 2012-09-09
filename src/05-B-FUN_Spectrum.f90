!!## FUNCTION: Spectrum
MODULE FUN_Spectrum
!!### PURPOSE
!! Get an RGBA spectrum from a provided intensity
!! <X>, in $[0,1]$, a <method>, and an optional <scheme>
!! and scheme specifiers.

!!### USAGE
!!
!
!    Spectrum( X , Method , [Scheme , ro , gamma , beta , alpha] )
!

!!#### EXTERNAL PROCEDURES
USE FUN_RGBA_Random                           !!((04-B-FUN_RGBA_Random.f90))
USE FUN_RGBA_Exp1                             !!((04-B-FUN_RGBA_Exp1.f90))
USE FUN_RGBA_Hot                              !!((04-B-FUN_RGBA_Hot.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

!!#### PROCEDURE OVERLOADING
INTERFACE Spectrum
 MODULE PROCEDURE Spectrum_Rsp
 MODULE PROCEDURE Spectrum_Rdp
END INTERFACE

!!#### PUBLIC ACCESS LIST
PUBLIC :: Spectrum

CONTAINS

FUNCTION Spectrum_Rsp( X , Method , Scheme , ro , gamma , beta , alpha ) RESULT(RGBA)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-FUN_Spectrum.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_Spectrum.f90.bdy"
!!--end--
END FUNCTION

FUNCTION Spectrum_Rdp( X , Method , Scheme  , ro , gamma , beta , alpha ) RESULT(RGBA)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-FUN_Spectrum.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_Spectrum.f90.bdy"
!!--end--
END FUNCTION

END MODULE
