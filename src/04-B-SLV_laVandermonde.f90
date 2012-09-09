!!#### SOLVER MODULE: FUNCTION  SLV_laVandermonde
MODULE SLV_laVandermonde
!!#### PUPROSE
!! A solver function which solves a linear algrebraic system
!! known as a Vandermonde system in which columns of the matrix A
!! are powers of some vector x equal to some vector f,
!
!                   A(x) u = f
!
!! where the elements of A_{i,j} = (x_i)^{j-1}.

!!#### EXTRA
!! The solution of this system is the coefficients of the polynomial P
!! that goes through the points, x_i, i=1,...,N, with values f_i = P(x_i).
!! If any x_i==x_i' for i/=i' then the matrix A is singular and the
!! system has no solution.

!!#### DEPENDENCIES
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))

!!#### SOURCE
!!   Inversion of Vandermonde Matrices in FPGAs
!!
!! Master thesis in Division of Electronics Systems
!!      Department of Electrical Engineering
!!      Linköping Institute of Technology
!
!!                    by
!
!!          Shiqiang Hu, Qingxin Yan
!
!!           LiTH-ISY-EX-3695-2004
!!      (för civilingenjörsutbildningen)
!!                   eller
!!           iTH-ISY-EX-ET-XXXX-YYYY
!!      (för elektroingenjörsutbildningen)



!!#### INFO
!! author : William Wieselquist [waw]
!! contact: william.wieselquist@gmail.com

IMPLICIT NONE

PRIVATE

INTERFACE laVandermonde
 MODULE PROCEDURE laVandermonde_xRsp_fRsp
 MODULE PROCEDURE laVandermonde_xRsp_fRdp
 MODULE PROCEDURE laVandermonde_xRdp_fRsp
 MODULE PROCEDURE laVandermonde_xRdp_fRdp
ENDINTERFACE
PUBLIC :: laVandermonde

CONTAINS

PURE FUNCTION laVandermonde_xRsp_fRsp( x , f ) RESULT(u)
INTEGER,PARAMETER :: KIND_x = KIND_Rsp , KIND_f = KIND_Rsp
INCLUDE "04-B-SLV_laVandermonde.f90.hdr"
!!--begin--
INCLUDE "04-B-SLV_laVandermonde.f90.bdy"
!!--end--
ENDFUNCTION


PURE FUNCTION laVandermonde_xRdp_fRdp( x , f ) RESULT(u)
INTEGER,PARAMETER :: KIND_x = KIND_Rdp , KIND_f = KIND_Rdp
INCLUDE "04-B-SLV_laVandermonde.f90.hdr"
!!--begin--
INCLUDE "04-B-SLV_laVandermonde.f90.bdy"
!!--end--
ENDFUNCTION

PURE FUNCTION laVandermonde_xRdp_fRsp( x , f ) RESULT(u)
INTEGER,PARAMETER :: KIND_x = KIND_Rdp , KIND_f = KIND_Rsp
INCLUDE "04-B-SLV_laVandermonde.f90.hdr"
!!--begin--
INCLUDE "04-B-SLV_laVandermonde.f90.bdy"
!!--end--
ENDFUNCTION

PURE FUNCTION laVandermonde_xRsp_fRdp( x , f ) RESULT(u)
INTEGER,PARAMETER :: KIND_x = KIND_Rsp , KIND_f = KIND_Rdp
INCLUDE "04-B-SLV_laVandermonde.f90.hdr"
!!--begin--
INCLUDE "04-B-SLV_laVandermonde.f90.bdy"
!!--end--
ENDFUNCTION

ENDMODULE
