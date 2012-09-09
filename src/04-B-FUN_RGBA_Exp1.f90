!!## FUNCTION: RGBA_Exp1
MODULE FUN_RGBA_Exp1
!!### PURPOSE
!! Get a Experimental color scheme in RGBA format with
!! components <R,G,B,A> all in $[0,1]$.


!!### USAGE
!
!      RGBA = RGBA_Exp1( X  [,ro] [,beta] [,gamma] [,alpha] )
!
!! where <RGBA(1:4)> is an array containing each of the components
!! in that order, <X> is the input intensity to generate the color
!! for and <ro(1:5)>, <beta(1:5)>, <gamma(1:5)>, and <alpha(1:5)>
!! are optional 5-part factors used to generate the componets,
!! if present.


!!### METHOD
!! Each component <R,G,B,A> is calculated based on the intensity provided,
!! <X>, and 5-part factors <ro,gamma,beta,alpha>, respectively.
!!
!! The formula for component $R$ is based on intensity $X$
!! and 5-part factors, $\{ \ro_i | i=1,..,5 \}$, as follows:
!!
!!   $$ R = {\ro_1}^{\ro_2 + \ro_3*\abs{ X - \ro_4 }^{\ro_5} } $$
!!
!! The formulas for the other components $G,B,A$ are calculated the
!! same way, using $\gamma,\beta,\alpha$ instead.


!!### COMMENTS
!! To fix a component $Y$ at a certain value $Z$ provide
!! $Z={y_1}^{y_2}$ and $y_3=0$.


!!### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))

!!### DEFAULT IMPLICIT
IMPLICIT NONE

!!### DEFAULT ACCESS
PRIVATE

!!### PROCEDURE OVERLOADING
INTERFACE RGBA_Exp1
 MODULE PROCEDURE RGBA_Exp1_Rsp
 MODULE PROCEDURE RGBA_Exp1_Rdp
END INTERFACE

!!### PUBLIC ACCESS LIST
PUBLIC :: RGBA_Exp1


!!## MODULE PROCEDURES
CONTAINS

!!### FUNCTION: RGBA_Exp1_Rsp
FUNCTION RGBA_Exp1_Rsp( X , ro , gamma , beta , alpha ) RESULT(RGBA)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-FUN_RGBA_Exp1.f90.hdr"
!!--begin--
INCLUDE "04-B-FUN_RGBA_Exp1.f90.bdy"
!!--end--
END FUNCTION


!!### FUNCTION: RGBA_Exp1_Rdp
FUNCTION RGBA_Exp1_Rdp( X , ro , gamma , beta , alpha ) RESULT(RGBA)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-FUN_RGBA_Exp1.f90.hdr"
!!--begin--
INCLUDE "04-B-FUN_RGBA_Exp1.f90.bdy"
!!--end--
END FUNCTION


END MODULE
