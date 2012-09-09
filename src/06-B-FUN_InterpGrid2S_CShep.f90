!!### MODULE: FUNCTION  InterpGrid2S_CShep
MODULE FUN_InterpGrid2S_CShep
!!#### PURPOSE
!! This subroutine interpolates a list of discrete function values
!! <F(1:N)> provided at points <(x,y)=r(:,1:N)>, to a grid,
!! given by <xout(1:M)> and <yout(1:M)>.

!!#### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))

!!#### GLOBAL PROCEDURE MODULES
USE FUN_Default                                !!((04-A-FUN_Default.f90))
USE FUN_Error                                  !!((04-A-FUN_Error.f90))
USE SUB_CLEAR                                  !!((04-A-SUB_CLEAR.f90))
USE FUN_STR                                    !!((05-B-FUN_STR.f90))

!!#### GLOBAL USER MODULES
USE USR_CShep2                                 !!((03-A-USR_CShep2.f))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

!!#### PROCEDURE OVERLOADING
INTERFACE InterpGrid2S_CShep
 MODULE PROCEDURE InterpGrid2S_CShep_Rsp
 MODULE PROCEDURE InterpGrid2S_CShep_Rdp
END INTERFACE

!!#### PUBLIC ACCESS LIST
PUBLIC :: InterpGrid2S_CShep

CONTAINS


FUNCTION InterpGrid2S_CShep_Rsp( F , r , xout , yout , errmsg , &
  NC,NW,NR,MAX_NC,MAX_NW,&
  Rmax,xmin,ymin,dx,dy,errint)  RESULT(Fout)
!!#### PARAMETERS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-B-FUN_InterpGrid2S_CShep_R.f90.hdr"
!!--begin--
INCLUDE "06-B-FUN_InterpGrid2S_CShep_R.f90.bdy"
!!--end--
END FUNCTION


FUNCTION InterpGrid2S_CShep_Rdp( F , r , xout , yout , errmsg , &
  NC,NW,NR,MAX_NC,MAX_NW,&
  Rmax,xmin,ymin,dx,dy,errint)  RESULT(Fout)
!!#### PARAMETERS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-B-FUN_InterpGrid2S_CShep_R.f90.hdr"
!!--begin--
INCLUDE "06-B-FUN_InterpGrid2S_CShep_R.f90.bdy"
!!--end--
END FUNCTION

END MODULE
