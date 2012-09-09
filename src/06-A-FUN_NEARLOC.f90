!!# MODULE <<FUN_NEARLOC>>
MODULE FUN_NEARLOC

!!## PURPOSE
!! Determines the location of the nearest scalar/vector to
!! from a list of scalars/vectors, using one of
!! three norms: L-1, L-2, infinity for vectors.



!!## USAGE
!
!        loc = NEARLOC( SCLS , SCL )
!
!!  returns the location $m$ in a list of $M$ scalars,
!!  <SCLS(1:M)>, where <ABS(SCLS(m)-SCL)> is
!!  minimized.
!
!
!        loc = NEARLOC( VECS , VEC [,NormSpec] )
!
!!  returns the location $m$ in a list of $M$ vectors,
!!  <VECS(1:N,1:M)>, in $R^N$,  where <Norm(VECS(1:N,m)-VEC(1:N))>
!!  is minimized.
!!
!!  The L-2 norm is used unless the optional
!!  <NormSpec> is present, in which case the norm is selected
!!  from the <NormSpec> in the following way:
!!
!!   @ <NormSpec="L1"> , L-1 norm is used,
!!   @ <NormSpec="Inf">, Infinity norm is used,
!!   @ <NormSpec="L2"> , L-2 norm (squared) is used,



!!## NOTES
!! For the L-2 norm, the square of the norm is used (this does
!! not ever affect the result.



!!## METHOD
!! For real vectors, a call of the form
!
!     FORALL( n=1:SIZE(VECS,2) )
!       proximity(n) = NORM( VECS(:,n) - VEC )
!     END FORALL
!
!     loc = MINLOC( proximity )
!
!! is used to determine the nearest neighbor.
!!
!! For complex vectors, the <VECS> and <VEC> are converted
!! from $Z^{N}$ to $R^{2N}$ and then sent to the
!! real <NEARLOC> routine.
!!
!! For integer vectors, the <VECS> and <VEC>
!! are converted to single precision real for 1-byte
!! and 2-byte integers and double precision real for
!! 4-byte and 8-byte integers.



!!## MODULES
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp,KIND_Csp,KIND_Cdp,& !!((01-A-KND_IntrinsicTypes.f90))
                             KIND_I1,KIND_I2,KIND_I4,KIND_I8
USE LIB_Norm                                                       !!((04-B-LIB_Norm.f90))


!!## DEFAULT IMPLICIT
IMPLICIT NONE


!!## DEFAULT ACCESS
PRIVATE


!!## INTERFACES
INTERFACE NEARLOC
 MODULE PROCEDURE NEARLOC_A1Rsp
 MODULE PROCEDURE NEARLOC_A1Rdp
 MODULE PROCEDURE NEARLOC_A1Csp
 MODULE PROCEDURE NEARLOC_A1Cdp
 MODULE PROCEDURE NEARLOC_A1I1
 MODULE PROCEDURE NEARLOC_A1I2
 MODULE PROCEDURE NEARLOC_A1I4
 MODULE PROCEDURE NEARLOC_A1I8
 MODULE PROCEDURE NEARLOC_A2Rsp
 MODULE PROCEDURE NEARLOC_A2Rdp
 MODULE PROCEDURE NEARLOC_A2Csp
 MODULE PROCEDURE NEARLOC_A2Cdp
 MODULE PROCEDURE NEARLOC_A2I1
 MODULE PROCEDURE NEARLOC_A2I2
 MODULE PROCEDURE NEARLOC_A2I4
 MODULE PROCEDURE NEARLOC_A2I8
END INTERFACE


!!## PUBLIC ACCESS LIST
PUBLIC :: NEARLOC


!!## MODULE PROCEDURES
CONTAINS


!!### PURE FUNCTION: NEARLOC_A1Rsp
PURE FUNCTION NEARLOC_A1Rsp( SCLS , SCL )  RESULT(loc)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp                      !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-A-FUN_NEARLOC_A1R.f90.hdr"
!!--begin--
INCLUDE "06-A-FUN_NEARLOC_A1R.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION: NEARLOC_A1Rdp
PURE FUNCTION NEARLOC_A1Rdp( SCLS , SCL )  RESULT(loc)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp                      !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-A-FUN_NEARLOC_A1R.f90.hdr"
!!--begin--
INCLUDE "06-A-FUN_NEARLOC_A1R.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION: NEARLOC_A1Csp
PURE FUNCTION NEARLOC_A1Csp( SCLS , SCL )  RESULT(loc)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp                      !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-A-FUN_NEARLOC_A1C.f90.hdr"
!!--begin--
INCLUDE "06-A-FUN_NEARLOC_A1C.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION: NEARLOC_A1Cdp
PURE FUNCTION NEARLOC_A1Cdp( SCLS , SCL )  RESULT(loc)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp                      !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-A-FUN_NEARLOC_A1C.f90.hdr"
!!--begin--
INCLUDE "06-A-FUN_NEARLOC_A1C.f90.bdy"
!!--end--
END FUNCTION




!!### PURE FUNCTION: NEARLOC_A1I1
PURE FUNCTION NEARLOC_A1I1( SCLS , SCL )  RESULT(loc)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I1                       !!((01-A-KND_IntrinsicTypes.f90))
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp                      !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-A-FUN_NEARLOC_A1I.f90.hdr"
!!--begin--
INCLUDE "06-A-FUN_NEARLOC_A1I.f90.bdy"
!!--end--
END FUNCTION




!!### PURE FUNCTION: NEARLOC_A1I2
PURE FUNCTION NEARLOC_A1I2( SCLS , SCL )  RESULT(loc)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I2                       !!((01-A-KND_IntrinsicTypes.f90))
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp                      !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-A-FUN_NEARLOC_A1I.f90.hdr"
!!--begin--
INCLUDE "06-A-FUN_NEARLOC_A1I.f90.bdy"
!!--end--
END FUNCTION




!!### PURE FUNCTION: NEARLOC_A1I4
PURE FUNCTION NEARLOC_A1I4( SCLS , SCL )  RESULT(loc)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I4                       !!((01-A-KND_IntrinsicTypes.f90))
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp                      !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-A-FUN_NEARLOC_A1I.f90.hdr"
!!--begin--
INCLUDE "06-A-FUN_NEARLOC_A1I.f90.bdy"
!!--end--
END FUNCTION



!!### PURE FUNCTION: NEARLOC_A1I8
PURE FUNCTION NEARLOC_A1I8( SCLS , SCL )  RESULT(loc)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I8                       !!((01-A-KND_IntrinsicTypes.f90))
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp                      !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-A-FUN_NEARLOC_A1I.f90.hdr"
!!--begin--
INCLUDE "06-A-FUN_NEARLOC_A1I.f90.bdy"
!!--end--
END FUNCTION



!!### PURE FUNCTION: NEARLOC_A2Rsp
PURE FUNCTION NEARLOC_A2Rsp( VECS , VEC , NormSpec )  RESULT(loc)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp                      !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-A-FUN_NEARLOC_A2R.f90.hdr"
!!--begin--
INCLUDE "06-A-FUN_NEARLOC_A2R.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION: NEARLOC_A2Rdp
PURE FUNCTION NEARLOC_A2Rdp( VECS , VEC , NormSpec )  RESULT(loc)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp                      !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-A-FUN_NEARLOC_A2R.f90.hdr"
!!--begin--
INCLUDE "06-A-FUN_NEARLOC_A2R.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION: NEARLOC_A2Csp
PURE FUNCTION NEARLOC_A2Csp( VECS , VEC , NormSpec )  RESULT(loc)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp                      !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-A-FUN_NEARLOC_A2C.f90.hdr"
!!--begin--
INCLUDE "06-A-FUN_NEARLOC_A2C.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION: NEARLOC_A2Cdp
PURE FUNCTION NEARLOC_A2Cdp( VECS , VEC , NormSpec )  RESULT(loc)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp                      !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-A-FUN_NEARLOC_A2C.f90.hdr"
!!--begin--
INCLUDE "06-A-FUN_NEARLOC_A2C.f90.bdy"
!!--end--
END FUNCTION




!!### PURE FUNCTION: NEARLOC_A2I1
PURE FUNCTION NEARLOC_A2I1( VECS , VEC , NormSpec )  RESULT(loc)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I1                       !!((01-A-KND_IntrinsicTypes.f90))
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp                      !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-A-FUN_NEARLOC_A2I.f90.hdr"
!!--begin--
INCLUDE "06-A-FUN_NEARLOC_A2I.f90.bdy"
!!--end--
END FUNCTION




!!### PURE FUNCTION: NEARLOC_A2I2
PURE FUNCTION NEARLOC_A2I2( VECS , VEC , NormSpec )  RESULT(loc)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I2                       !!((01-A-KND_IntrinsicTypes.f90))
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp                      !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-A-FUN_NEARLOC_A2I.f90.hdr"
!!--begin--
INCLUDE "06-A-FUN_NEARLOC_A2I.f90.bdy"
!!--end--
END FUNCTION




!!### PURE FUNCTION: NEARLOC_A2I4
PURE FUNCTION NEARLOC_A2I4( VECS , VEC , NormSpec )  RESULT(loc)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I4                       !!((01-A-KND_IntrinsicTypes.f90))
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp                      !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-A-FUN_NEARLOC_A2I.f90.hdr"
!!--begin--
INCLUDE "06-A-FUN_NEARLOC_A2I.f90.bdy"
!!--end--
END FUNCTION



!!### PURE FUNCTION: NEARLOC_A2I8
PURE FUNCTION NEARLOC_A2I8( VECS , VEC , NormSpec )  RESULT(loc)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I8                       !!((01-A-KND_IntrinsicTypes.f90))
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp                      !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-A-FUN_NEARLOC_A2I.f90.hdr"
!!--begin--
INCLUDE "06-A-FUN_NEARLOC_A2I.f90.bdy"
!!--end--
END FUNCTION



END MODULE
