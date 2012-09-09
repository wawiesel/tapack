!!# MODULE FUNCTION: \<FUN_PACK2\>
MODULE FUN_PACK2

!!## PURPOSE
!! Pack a list of vectors on the SECOND DIMENSION (in the
!! Fortran sense of dimensions) ONLY, assumed
!! to be an index of the point.


!!## COMMENTS
!! The normal <PACK> operation is only for 1D arrays,
!! so we provide this extension for the special
!! case when you always want to preserve the components along
!! the first dimension, as with vectors representing
!! coordinates, directions, etc.


!!## USAGE
!
!     Y = PACK2( X , MASK )
!
!! where <X> is a list of vectors <Y(1:N,:)>, where
!! <N> is the number of dimensions and <MASK>, is the MASK of
!! which vectors to include in the output <Y>.


!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## DEFAULT ACCESS
PRIVATE


!!## OPERATOR DEFINITION
INTERFACE PACK2
 MODULE PROCEDURE PACK2_Rsp
 MODULE PROCEDURE PACK2_Rdp
 MODULE PROCEDURE PACK2_Csp
 MODULE PROCEDURE PACK2_Cdp
 MODULE PROCEDURE PACK2_I1
 MODULE PROCEDURE PACK2_I2
 MODULE PROCEDURE PACK2_I4
 MODULE PROCEDURE PACK2_I8
 MODULE PROCEDURE PACK2_L1
 MODULE PROCEDURE PACK2_L2
 MODULE PROCEDURE PACK2_L4
 MODULE PROCEDURE PACK2_S
END INTERFACE


!!## PUBLIC ACCESS LIST
PUBLIC :: PACK2


!!## MODULE PROCEDURES
CONTAINS


!!### PURE FUNCTION \<PACK2_Rsp\>
PURE FUNCTION PACK2_Rsp(X,MASK) RESULT(Y)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_PACK2_R.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_PACK2.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION \<PACK2_Rdp\>
PURE FUNCTION PACK2_Rdp(X,MASK) RESULT(Y)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_PACK2_R.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_PACK2.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION \<PACK2_Csp\>
PURE FUNCTION PACK2_Csp(X,MASK) RESULT(Y)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_PACK2_C.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_PACK2.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION \<PACK2_Cdp\>
PURE FUNCTION PACK2_Cdp(X,MASK) RESULT(Y)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_PACK2_C.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_PACK2.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION \<PACK2_I1\>
PURE FUNCTION PACK2_I1(X,MASK) RESULT(Y)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I1  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_PACK2_I.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_PACK2.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION \<PACK2_I2\>
PURE FUNCTION PACK2_I2(X,MASK) RESULT(Y)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I2  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_PACK2_I.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_PACK2.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION \<PACK2_I4\>
PURE FUNCTION PACK2_I4(X,MASK) RESULT(Y)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I4  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_PACK2_I.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_PACK2.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION \<PACK2_I8\>
PURE FUNCTION PACK2_I8(X,MASK) RESULT(Y)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I8  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_PACK2_I.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_PACK2.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION \<PACK2_L1\>
PURE FUNCTION PACK2_L1(X,MASK) RESULT(Y)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L1  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_PACK2_L.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_PACK2.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION \<PACK2_L2\>
PURE FUNCTION PACK2_L2(X,MASK) RESULT(Y)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L2  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_PACK2_L.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_PACK2.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION \<PACK2_L4\>
PURE FUNCTION PACK2_L4(X,MASK) RESULT(Y)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L4  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_PACK2_L.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_PACK2.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION \<PACK2_S\>
PURE FUNCTION PACK2_S(X,MASK) RESULT(Y)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_S           !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_PACK2_S.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_PACK2.f90.bdy"
!!--end--
END FUNCTION


END MODULE
