MODULE USR_DiscreteOrdinates
!!#### PURPOSE
!! Contains USEr-routines for the discrete ordinates toolbox.

!!#### EXTERNAL KINDS
USE KND_Mesh                                               !!((05-B-KND_Mesh.f90))
USE KND_DiscreteOrdinates                                  !!((02-A-KND_DiscreteOrdinates.f90))

!!#### EXTERNAL PROCEDURES
USE FUN_xyDOT                                              !!((03-A-FUN_xyDOT.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

!!#### PROCEDURE OVERLOADING
!! @ Moment0
INTERFACE Moment0
 MODULE PROCEDURE AngularMoment0_Rsp
 MODULE PROCEDURE AngularMoment0_Rdp
END INTERFACE
!! @ Moment1
INTERFACE Moment1
 MODULE PROCEDURE AngularMoment1_Rsp
 MODULE PROCEDURE AngularMoment1_Rdp
END INTERFACE
!! @ Moment0 with normal vector in integral
INTERFACE Moment0_Normal
 MODULE PROCEDURE AngularMoment0_Normal_Rsp
 MODULE PROCEDURE AngularMoment0_Normal_Rdp
END INTERFACE
!! @ Moment1 with normal vector in integral
INTERFACE Moment1_Normal
 MODULE PROCEDURE AngularMoment1_Normal_Rsp
 MODULE PROCEDURE AngularMoment1_Normal_Rdp
END INTERFACE
!! @ Moment0 with normal and integration over
!!   inward directions only
INTERFACE Moment0_NormalIN
 MODULE PROCEDURE AngularMoment0_NormalIN_Rsp
 MODULE PROCEDURE AngularMoment0_NormalIN_Rdp
END INTERFACE
!! @ Moment0 with normal and integration over
!!   inward directions only
INTERFACE Moment1_NormalIN
 MODULE PROCEDURE AngularMoment1_NormalIN_Rsp
 MODULE PROCEDURE AngularMoment1_NormalIN_Rdp
END INTERFACE
!! @ Moment0 with normal and integration over
!!   outward directions only
INTERFACE Moment0_NormalOUT
 MODULE PROCEDURE AngularMoment0_NormalOUT_Rsp
 MODULE PROCEDURE AngularMoment0_NormalOUT_Rdp
END INTERFACE
!! @ Moment1 with normal and integration over
!!   outward directions only
INTERFACE Moment1_NormalOUT
 MODULE PROCEDURE AngularMoment1_NormalOUT_Rsp
 MODULE PROCEDURE AngularMoment1_NormalOUT_Rdp
END INTERFACE
!! @ Calculate the Eddington factors
INTERFACE Eddington
 MODULE PROCEDURE Eddington_Rsp
 MODULE PROCEDURE Eddington_Rdp
END INTERFACE


!!#### PUBLIC ACCESS LIST
!PUBLIC :: Moment0
!PUBLIC :: Moment1
!PUBLIC :: Moment0_Normal
!PUBLIC :: Moment1_Normal
!PUBLIC :: Moment0_NormalIN
!PUBLIC :: Moment1_NormalIN
!PUBLIC :: Moment0_NormalOUT
!PUBLIC :: Moment1_NormalOUT
!PUBLIC :: Eddington


CONTAINS

!**IMPORTANT**
!! @ these routines calculate the angular flux moments for ALL verts
FUNCTION AngularMoment0_Rsp( AngularFlux , &
  m_ , PolWts , AziWts ) RESULT(Moment0)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp              !!((01-A-KND_IntrinsicTypes.f90))
USE PAR_Constants_Rsp,ONLY: c_4_times_PI,c_1_by_4_times_PI !!((02-A-PAR_Constants_Rsp.f90))
INCLUDE "34-B-USR_DiscreteOrdinates__AngularMoment0.f90.hdr"
!!--begin--
INCLUDE "34-B-USR_DiscreteOrdinates__AngularMoment0.f90.bdy"
!!--end--
END FUNCTION

FUNCTION AngularMoment0_Rdp( AngularFlux , &
  m_ , PolWts , AziWts ) RESULT(Moment0)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp              !!((01-A-KND_IntrinsicTypes.f90))
USE PAR_Constants_Rdp,ONLY: c_4_times_PI,c_1_by_4_times_PI !!((02-A-PAR_Constants_Rdp.f90))
INCLUDE "34-B-USR_DiscreteOrdinates__AngularMoment0.f90.hdr"
!!--begin--
INCLUDE "34-B-USR_DiscreteOrdinates__AngularMoment0.f90.bdy"
!!--end--
END FUNCTION



FUNCTION AngularMoment1_Rsp( AngularFlux , DirCos , &
  m_ , PolWts , AziWts , AziSin ) RESULT(Moment1)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp              !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "34-B-USR_DiscreteOrdinates__AngularMoment1.f90.hdr"
!!--begin--
INCLUDE "34-B-USR_DiscreteOrdinates__AngularMoment1.f90.bdy"
!!--end--
END FUNCTION

FUNCTION AngularMoment1_Rdp( AngularFlux , DirCos , &
  m_ , PolWts , AziWts , AziSin ) RESULT(Moment1)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp              !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "34-B-USR_DiscreteOrdinates__AngularMoment1.f90.hdr"
!!--begin--
INCLUDE "34-B-USR_DiscreteOrdinates__AngularMoment1.f90.bdy"
!!--end--
END FUNCTION


!! @ these routines calculate the angular flux moments for ONE vert/face
FUNCTION AngularMoment0_Normal_Rsp( AngularFlux , DirCos , Normal , &
  m_ , PolWts , AziWts ) RESULT(Moment0)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp              !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "34-B-USR_DiscreteOrdinates__AngularMoment0_Normal.f90.hdr"
!!--begin--
INCLUDE "34-B-USR_DiscreteOrdinates__AngularMoment0_Normal.f90.bdy"
!!--end--
END FUNCTION

FUNCTION AngularMoment0_Normal_Rdp( AngularFlux , DirCos , Normal , &
  m_ , PolWts , AziWts ) RESULT(Moment0)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp              !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "34-B-USR_DiscreteOrdinates__AngularMoment0_Normal.f90.hdr"
!!--begin--
INCLUDE "34-B-USR_DiscreteOrdinates__AngularMoment0_Normal.f90.bdy"
!!--end--
END FUNCTION


FUNCTION AngularMoment1_Normal_Rsp( AngularFlux , DirCos , Normal , &
  m_ , PolWts , AziWts , AziSin ) RESULT(Moment1)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp              !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "34-B-USR_DiscreteOrdinates__AngularMoment1_Normal.f90.hdr"
!!--begin--
INCLUDE "34-B-USR_DiscreteOrdinates__AngularMoment1_Normal.f90.bdy"
!!--end--
END FUNCTION

FUNCTION AngularMoment1_Normal_Rdp( AngularFlux , DirCos , Normal , &
  m_ , PolWts , AziWts , AziSin ) RESULT(Moment1)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp              !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "34-B-USR_DiscreteOrdinates__AngularMoment1_Normal.f90.hdr"
!!--begin--
INCLUDE "34-B-USR_DiscreteOrdinates__AngularMoment1_Normal.f90.bdy"
!!--end--
END FUNCTION



FUNCTION AngularMoment0_NormalIN_Rsp( AngularFlux , DirCos , Normal , &
  m_ , PolWts , AziWts ) RESULT(Moment0)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp              !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "34-B-USR_DiscreteOrdinates__AngularMoment0_Normal.f90.hdr"
!!--begin--
INCLUDE "34-B-USR_DiscreteOrdinates__AngularMoment0_NormalIN.f90.bdy"
!!--end--
END FUNCTION

FUNCTION AngularMoment0_NormalIN_Rdp( AngularFlux , DirCos , Normal , &
  m_ , PolWts , AziWts ) RESULT(Moment0)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp              !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "34-B-USR_DiscreteOrdinates__AngularMoment0_Normal.f90.hdr"
!!--begin--
INCLUDE "34-B-USR_DiscreteOrdinates__AngularMoment0_NormalIN.f90.bdy"
!!--end--
END FUNCTION


FUNCTION AngularMoment1_NormalIN_Rsp( AngularFlux , DirCos , Normal , &
  m_ , PolWts , AziWts , AziSin ) RESULT(Moment1)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp              !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "34-B-USR_DiscreteOrdinates__AngularMoment1_Normal.f90.hdr"
!!--begin--
INCLUDE "34-B-USR_DiscreteOrdinates__AngularMoment1_NormalIN.f90.bdy"
!!--end--
END FUNCTION

FUNCTION AngularMoment1_NormalIN_Rdp( AngularFlux , DirCos , Normal , &
  m_ , PolWts , AziWts , AziSin ) RESULT(Moment1)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp              !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "34-B-USR_DiscreteOrdinates__AngularMoment1_Normal.f90.hdr"
!!--begin--
INCLUDE "34-B-USR_DiscreteOrdinates__AngularMoment1_NormalIN.f90.bdy"
!!--end--
END FUNCTION




FUNCTION AngularMoment0_NormalOUT_Rsp( AngularFlux , DirCos , Normal , &
  m_ , PolWts , AziWts ) RESULT(Moment0)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp              !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "34-B-USR_DiscreteOrdinates__AngularMoment0_Normal.f90.hdr"
!!#### BEGOUT
INCLUDE "34-B-USR_DiscreteOrdinates__AngularMoment0_NormalOUT.f90.bdy"
!!--end--
END FUNCTION

FUNCTION AngularMoment0_NormalOUT_Rdp( AngularFlux , DirCos , Normal , &
  m_ , PolWts , AziWts ) RESULT(Moment0)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp              !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "34-B-USR_DiscreteOrdinates__AngularMoment0_Normal.f90.hdr"
!!#### BEGOUT
INCLUDE "34-B-USR_DiscreteOrdinates__AngularMoment0_NormalOUT.f90.bdy"
!!--end--
END FUNCTION


FUNCTION AngularMoment1_NormalOUT_Rsp( AngularFlux , DirCos , Normal , &
  m_ , PolWts , AziWts , AziSin ) RESULT(Moment1)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp              !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "34-B-USR_DiscreteOrdinates__AngularMoment1_Normal.f90.hdr"
!!#### BEGOUT
INCLUDE "34-B-USR_DiscreteOrdinates__AngularMoment1_NormalOUT.f90.bdy"
!!--end--
END FUNCTION

FUNCTION AngularMoment1_NormalOUT_Rdp( AngularFlux , DirCos , Normal , &
  m_ , PolWts , AziWts , AziSin ) RESULT(Moment1)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp              !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "34-B-USR_DiscreteOrdinates__AngularMoment1_Normal.f90.hdr"
!!#### BEGOUT
INCLUDE "34-B-USR_DiscreteOrdinates__AngularMoment1_NormalOUT.f90.bdy"
!!--end--
END FUNCTION




FUNCTION Eddington_Rsp( AngularFlux , DirCos1 , DirCos2 , &
  m_ , PolWts , AziWts ) RESULT(Eddington)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp              !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "34-B-USR_DiscreteOrdinates__Eddington.f90.hdr"
!!--begin--
INCLUDE "34-B-USR_DiscreteOrdinates__Eddington.f90.bdy"
!!--end--
END FUNCTION


FUNCTION Eddington_Rdp( AngularFlux , DirCos1 , DirCos2 , &
  m_ , PolWts , AziWts ) RESULT(Eddington)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp              !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "34-B-USR_DiscreteOrdinates__Eddington.f90.hdr"
!!--begin--
INCLUDE "34-B-USR_DiscreteOrdinates__Eddington.f90.bdy"
!!--end--
END FUNCTION



END MODULE
