!!## MODULE: FUNCTION  InterpGrid2S_Nearest
MODULE FUN_InterpGrid2S_Nearest
!!### PURPOSE
!! This subroutine interpolates a list of discrete
!! function values <F(1:N)> provided at points
!! <(x,y)=r(:,1:N)>, to a grid, given by <xout(1:M)>
!! and <yout(1:M)>, by simply using the value closest
!! to the grid point.

!!### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))

!!### GLOBAL PROCEDURE MODULES
USE FUN_NEARLOC                                !!((06-A-FUN_NEARLOC.f90))
USE SUB_CLEAR                                  !!((04-A-SUB_CLEAR.f90))

!!### DEFAULT IMPLICIT
IMPLICIT NONE

!!### DEFAULT ACCESS
PRIVATE

!!### PROCEDURE OVERLOADING
INTERFACE InterpGrid2S_Nearest
 MODULE PROCEDURE InterpGrid2S_Nearest_Rsp
 MODULE PROCEDURE InterpGrid2S_Nearest_Rdp
END INTERFACE

!!### PUBLIC ACCESS LIST
PUBLIC :: InterpGrid2S_Nearest


!!### PROCEDURES
CONTAINS


!!### FUNCTION: InterpGrid2S_Nearest_Rsp
FUNCTION InterpGrid2S_Nearest_Rsp( F , r , xout , yout , errmsg , &
  errint)  RESULT(Fout)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "07-B-FUN_InterpGrid2S_Nearest_R.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_InterpGrid2S_Nearest_R.f90.bdy"
!!--end--
END FUNCTION


!!### FUNCTION: InterpGrid2S_Nearest_Rdp
FUNCTION InterpGrid2S_Nearest_Rdp( F , r , xout , yout , errmsg , &
  errint)  RESULT(Fout)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "07-B-FUN_InterpGrid2S_Nearest_R.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_InterpGrid2S_Nearest_R.f90.bdy"
!!--end--
END FUNCTION


END MODULE
