!!# FUNCTION MODULE: <FUN_xyINTEGRALF>
MODULE FUN_xyINTEGRALF

!!## PURPOSE
!! Integrates a general function $F$ over shapes in 2D.


!!## METHOD
!! A 19th order cubature given in TOMS 706.


!!## AUTHORS
! Jarle Berntsen, Terje Espelid


!!## REFERENCES
!
!    Jarle Berntsen, Terje Espelid,
!    Algorithm 706:
!    DCUTRI, an Algorithm for Adaptive Cubature over a Collection
!    of Triangles,
!    ACM Transactions on Mathematical Software,
!    Volume 18, Number 3, September 1992, pages 329-342.


!!## NOTES
!! Wrapped in a user-friendly function by W. A. Wieselquist [waw].


!!## EXTERNAL MODULES
USE KND_IntrinsicTypes                          !!((01-A-KND_IntrinsicTypes.f90))
USE SUB_dcutri                                  !!((03-A-SUB_dcutri.f90))
USE FUN_Default                                 !!((04-A-FUN_Default.f90))
USE FUN_xySAREA                                 !!((03-A-FUN_xySAREA.f90))
USE FUN_xyCENTROID                              !!((05-B-FUN_xyCENTROID.f90))
USE FUN_xyTRIANGULATE                           !!((07-B-FUN_xyTRIANGULATE.f90))
USE FUN_xyDIST                                  !!((04-B-FUN_xyDIST.f90))
USE FUN_xyDIRECTION                             !!((04-B-FUN_xyDIRECTION.f90))
USE FUN_Integrate1_aq                           !!((06-B-FUN_Integrate1_aq.f90))
USE FUN_Error                                   !!((04-A-FUN_Error.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## DEFAULT ACCESS
PRIVATE


!!## PROCEDURE OVERLOADING
INTERFACE xyINTEGRALF_Trx
 MODULE PROCEDURE xyINTEGRALF_Trx_Rsp
 MODULE PROCEDURE xyINTEGRALF_Trx_Rdp
ENDINTERFACE
INTERFACE xyINTEGRALF_Tr
 MODULE PROCEDURE xyINTEGRALF_Tr_Rsp
 MODULE PROCEDURE xyINTEGRALF_Tr_Rdp
ENDINTERFACE
INTERFACE xyINTEGRALF_Pg
 MODULE PROCEDURE xyINTEGRALF_Pg_Rsp
 MODULE PROCEDURE xyINTEGRALF_Pg_Rdp
ENDINTERFACE
INTERFACE xyINTEGRALF_Ls
 MODULE PROCEDURE xyINTEGRALF_Ls_Rsp
 MODULE PROCEDURE xyINTEGRALF_Ls_Rdp
ENDINTERFACE

!!## PUBLIC ACCESS LIST
PUBLIC :: xyINTEGRALF_Trx
PUBLIC :: xyINTEGRALF_Trx_Rsp
PUBLIC :: xyINTEGRALF_Trx_Rdp
PUBLIC :: xyINTEGRALF_Tr
PUBLIC :: xyINTEGRALF_Tr_Rsp
PUBLIC :: xyINTEGRALF_Tr_Rdp
PUBLIC :: xyINTEGRALF_Pg
PUBLIC :: xyINTEGRALF_Pg_Rsp
PUBLIC :: xyINTEGRALF_Pg_Rdp
PUBLIC :: xyINTEGRALF_Ls
PUBLIC :: xyINTEGRALF_Ls_Rsp
PUBLIC :: xyINTEGRALF_Ls_Rdp


!!## CONTAINED PROCEDURES
CONTAINS




!!### FUNCTION <xyINTEGRALF_Ls_Rsp>
FUNCTION xyINTEGRALF_Ls_Rsp( f , Ls , reltol , abstol , &
  nmin,nmax , err,num_eval,ierr ) RESULT(INTEGRALF)

!!#### LOCAL MAPPINGS
USE PAR_Constants_Rsp                           !!((02-A-PAR_Constants_Rsp.f90))
USE KND_IntrinsicTypes,ONLY: KIND_R => KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-INT_Function2_Rsp.f90.hdr"
INCLUDE "08-A-FUN_xyINTEGRALF_Ls.f90.hdr"

!!--begin--
INCLUDE "08-A-FUN_xyINTEGRALF_Ls.f90.bdy"
!!--end--

END FUNCTION


!!### FUNCTION <xyINTEGRALF_Ls_Rdp>
FUNCTION xyINTEGRALF_Ls_Rdp( f , Ls , reltol , abstol , &
  nmin,nmax , err,num_eval,ierr ) RESULT(INTEGRALF)

!!#### LOCAL MAPPINGS
USE PAR_Constants_Rsp                           !!((02-A-PAR_Constants_Rsp.f90))
USE KND_IntrinsicTypes,ONLY: KIND_R => KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-INT_Function2_Rdp.f90.hdr"
INCLUDE "08-A-FUN_xyINTEGRALF_Ls.f90.hdr"

!!--begin--
INCLUDE "08-A-FUN_xyINTEGRALF_Ls.f90.bdy"
!!--end--

END FUNCTION



!!### FUNCTION <xyINTEGRALF_Trx_Rdp>
FUNCTION xyINTEGRALF_Trx_Rdp( f , N , Trx , reltol , abstol , &
  nmin,nmax , err,num_eval,ierr ) RESULT(INTEGRALF)

!!#### LOCAL MAPPINGS
USE PAR_Constants_Rdp                           !!((02-A-PAR_Constants_Rdp.f90))
USE KND_IntrinsicTypes,ONLY: KIND_R => KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-INT_Function2_Rdp.f90.hdr"
INCLUDE "08-A-FUN_xyINTEGRALF_Trx.f90.hdr"

!!--begin--

INCLUDE "08-A-FUN_xyINTEGRALF_Trx.f90.bdy"

!!--end--
END FUNCTION




!!### FUNCTION <xyINTEGRALF_Trx_Rsp>
FUNCTION xyINTEGRALF_Trx_Rsp( f , N , Trx , reltol , abstol , &
  nmin,nmax , err,num_eval,ierr ) RESULT(INTEGRALF)

!!#### LOCAL MAPPINGS
USE PAR_Constants_Rsp                           !!((02-A-PAR_Constants_Rsp.f90))
USE KND_IntrinsicTypes,ONLY: KIND_R => KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-INT_Function2_Rsp.f90.hdr"
INCLUDE "08-A-FUN_xyINTEGRALF_Trx.f90.hdr"

!!--begin--

INCLUDE "08-A-FUN_xyINTEGRALF_Trx.f90.bdy"

!!--end--
END FUNCTION



!!### FUNCTION <xyINTEGRALF_Tr_Rsp>
FUNCTION xyINTEGRALF_Tr_Rsp( f , Tr , reltol , abstol , &
  nmin,nmax , err,num_eval,ierr ) RESULT(INTEGRALF)

!!#### LOCAL MAPPINGS
USE PAR_Constants_Rsp                           !!((02-A-PAR_Constants_Rsp.f90))
USE KND_IntrinsicTypes,ONLY: KIND_R => KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-INT_Function2_Rsp.f90.hdr"
INCLUDE "08-A-FUN_xyINTEGRALF_Tr.f90.hdr"

!!--begin--
INCLUDE "08-A-FUN_xyINTEGRALF_Tr.f90.bdy"
!!--end--
END FUNCTION


!!### FUNCTION <xyINTEGRALF_Tr_Rdp>
FUNCTION xyINTEGRALF_Tr_Rdp( f , Tr , reltol , abstol , &
  nmin,nmax , err,num_eval,ierr ) RESULT(INTEGRALF)

!!#### LOCAL MAPPINGS
USE PAR_Constants_Rdp                           !!((02-A-PAR_Constants_Rdp.f90))
USE KND_IntrinsicTypes,ONLY: KIND_R => KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-INT_Function2_Rdp.f90.hdr"
INCLUDE "08-A-FUN_xyINTEGRALF_Tr.f90.hdr"
!!--begin--
INCLUDE "08-A-FUN_xyINTEGRALF_Tr.f90.bdy"

!!--end--
END FUNCTION




!!### FUNCTION <xyINTEGRALF_Pg_Rsp>
FUNCTION xyINTEGRALF_Pg_Rsp( f , N , Pg , P_centroid , Area , reltol , abstol , &
  nmin,nmax , err,num_eval,ierr ) RESULT(INTEGRALF)

!!#### LOCAL MAPPINGS
USE PAR_Constants_Rsp                           !!((02-A-PAR_Constants_Rsp.f90))
USE KND_IntrinsicTypes,ONLY: KIND_R => KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-INT_Function2_Rsp.f90.hdr"
INCLUDE "08-A-FUN_xyINTEGRALF_Pg.f90.hdr"

!!--begin--
INCLUDE "08-A-FUN_xyINTEGRALF_Pg.f90.bdy"
!!--end--

END FUNCTION


!!### FUNCTION <xyINTEGRALF_Tr_Rdp>
FUNCTION xyINTEGRALF_Pg_Rdp( f , N , Pg , P_centroid , Area , reltol , abstol , &
  nmin,nmax , err,num_eval,ierr ) RESULT(INTEGRALF)

!!#### LOCAL MAPPINGS
USE PAR_Constants_Rdp                           !!((02-A-PAR_Constants_Rdp.f90))
USE KND_IntrinsicTypes,ONLY: KIND_R => KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-INT_Function2_Rdp.f90.hdr"
INCLUDE "08-A-FUN_xyINTEGRALF_Pg.f90.hdr"
!!--begin--
INCLUDE "08-A-FUN_xyINTEGRALF_Pg.f90.bdy"

!!--end--
END FUNCTION


END MODULE
