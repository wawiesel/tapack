MODULE FUN_SelectColor
!!#### PURPOSE
!! Return a color in RGBA real format, R,G,B,A in [0,1], corresponding
!! to an input color name.  See PAR_Colors_RGBA for the color names.

!!#### LOCAL KINDS
!! @ kind for colors
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))

!!#### EXTERNAL PARAMETERS
USE PAR_Colors_RGBA_sp                        !!((02-A-PAR_Colors_RGBA_sp.f90))

!!#### EXTERNAL PROCEDURES
USE FUN_RandomColor                           !!((04-B-FUN_RandomColor.f90))
USE FUN_Upcase                                !!((03-A-FUN_Upcase.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE


!!#### PUBLIC ACCESS LIST
PUBLIC :: SelectColor

CONTAINS


FUNCTION SelectColor( COLOR_NAME ) RESULT(RGBA)
!!#### REQUIRED INPUT
CHARACTER(*),INTENT(IN) :: COLOR_NAME
!!#### REQUIRED OUTPUT
REAL(KIND_R) :: RGBA(1:4)
!!#### LOCAL VARIABLES
CHARACTER(20) :: COLOR_NAME_
!!--begin--
INCLUDE "05-A-FUN_SelectColor_RGBA.f90.hdr"
!!--end--
END FUNCTION


END MODULE
