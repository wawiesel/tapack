!!# FUNCTION MODULE: <InterpGrid2S_BiVar>
MODULE FUN_InterpGrid2S_BiVar

!!## PURPOSE
!! This subroutine interpolates a list of discrete function values
!! <F(1:N)> provided at points <(x,y)=r(:,1:N)>, to a grid,
!! given by <xout(1:M)> and <yout(1:M)>.


!!## EXTERNAL
USE KND_IntrinsicTypes                        !!((01-A-KND_IntrinsicTypes.f90))
USE FUN_Default                               !!((04-A-FUN_Default.f90))
USE LIB_BiVar                                 !!((04-A-LIB_BiVar.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## DEFAULT ACCESS
PRIVATE

!!## PROCEDURE OVERLOADING
INTERFACE InterpGrid2S_BiVar
 MODULE PROCEDURE InterpGrid2S_BiVar_Rsp
 MODULE PROCEDURE InterpGrid2S_BiVar_Rdp
END INTERFACE

!!## PUBLIC ACCESS LIST
PUBLIC :: InterpGrid2S_BiVar


!!## PROCEDURES
CONTAINS


!!### FUNCTION: InterpGrid2S_Bivar_Rsp
FUNCTION InterpGrid2S_BiVar_Rsp( F , r , xout , yout , errmsg , &
  md,errint)  RESULT(Fout)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-B-FUN_InterpGrid2S_BiVar_R.f90.hdr"
!!--begin--
INCLUDE "06-B-FUN_InterpGrid2S_BiVar_R.f90.bdy"
!!--end--
END FUNCTION


!!### FUNCTION: InterpGrid2S_Bivar_Rdp
FUNCTION InterpGrid2S_BiVar_Rdp( F , r , xout , yout , errmsg , &
  md,errint)  RESULT(Fout)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-B-FUN_InterpGrid2S_BiVar_R.f90.hdr"
!!--begin--
INCLUDE "06-B-FUN_InterpGrid2S_BiVar_R.f90.bdy"
!!--end--
END FUNCTION


END MODULE
