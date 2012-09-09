!!# MODULE <<FUN_Reorder>>
MODULE FUN_Reorder

!!## PURPOSE
!! Performs the a reordering of a list according to the argument "order",
!! which gives the new location of each element.  Useful for reordering
!! something else according to the same order of a Sort operation.  Ignores
!! values of order <=0.


!!## USAGE
!
!      new_list = Reorder( old_list , order [, side ])
!
!! where <old_list> is a 1D array of any intrinsic type,
!! <order> is a 1D array of  the same size as <old_list>.
!!
!! On return, < new_list(order(i)) = old_list(i) >,
!! forall <order(i)>0>, in the default case or if <side="L">.
!! In the case of <side="R">, right-side reordering is performed
!! which leads to a different output,  < new_list(i) = old_list(order(i)) >,
!! forall <order(i)>0>.


!!## NOTES
!! The return value of the string reordering <Reorder_S> is the
!! variable length string <varying_string>.  I wish it could be the
!! a normal string with length set by <LEN> but the compiler
!! will not allow it.


!!## MODULES
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp,& !!((01-A-KND_IntrinsicTypes.f90))
              KIND_Csp,KIND_Cdp,&
              KIND_I1,KIND_I2,KIND_I4,KIND_I8,&
              KIND_L1,KIND_L2,KIND_L4,&
              KIND_S
USE ISO_varying_string                           !!((03-A-ISO_varying_string.f90))
USE FUN_Default                                  !!((04-A-FUN_Default.f90))


!!## DEFAULT IMPLICIT
IMPLICIT NONE


!!## DEFAULT ACCESS
PRIVATE


!!## INTERFACES
INTERFACE Reorder
 MODULE PROCEDURE Reorder_Rsp
 MODULE PROCEDURE Reorder_Rdp
 MODULE PROCEDURE Reorder_I1
 MODULE PROCEDURE Reorder_I2
 MODULE PROCEDURE Reorder_I4
 MODULE PROCEDURE Reorder_I8
 MODULE PROCEDURE Reorder_L1
 MODULE PROCEDURE Reorder_L2
 MODULE PROCEDURE Reorder_L4
 MODULE PROCEDURE Reorder_S
 MODULE PROCEDURE Reorder_VS
END INTERFACE


!!## PUBLIC ACCESS LIST
PUBLIC :: Reorder


!!## MODULE PROCEDURES
CONTAINS


!!### PURE FUNCTION  <<Reorder_Rsp>>
PURE FUNCTION Reorder_Rsp( old_list , order , side )  RESULT(new_list)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp    !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-A-FUN_Reorder_R.f90.hdr"
!!--begin--
INCLUDE "05-A-FUN_Reorder.f90.bdy"
!!--end--
END FUNCTION

!!### PURE FUNCTION  <<Reorder_Rdp>>
PURE FUNCTION Reorder_Rdp( old_list , order , side )  RESULT(new_list)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp    !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-A-FUN_Reorder_R.f90.hdr"
!!--begin--
INCLUDE "05-A-FUN_Reorder.f90.bdy"
!!--end--
END FUNCTION

!!### PURE FUNCTION  <<Reorder_Csp>>
PURE FUNCTION Reorder_Csp( old_list , order , side )  RESULT(new_list)
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp    !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-A-FUN_Reorder_C.f90.hdr"
!!--begin--
INCLUDE "05-A-FUN_Reorder.f90.bdy"
!!--end--
END FUNCTION

!!### PURE FUNCTION  <<Reorder_Cdp>>
PURE FUNCTION Reorder_Cdp( old_list , order , side )  RESULT(new_list)
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp    !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-A-FUN_Reorder_C.f90.hdr"
!!--begin--
INCLUDE "05-A-FUN_Reorder.f90.bdy"
!!--end--
END FUNCTION

!!### PURE FUNCTION  <<Reorder_I1>>
PURE FUNCTION Reorder_I1( old_list , order , side )  RESULT(new_list)
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I1     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-A-FUN_Reorder_I.f90.hdr"
!!--begin--
INCLUDE "05-A-FUN_Reorder.f90.bdy"
!!--end--
END FUNCTION

!!### PURE FUNCTION  <<Reorder_I2>>
PURE FUNCTION Reorder_I2( old_list , order , side )  RESULT(new_list)
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I2     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-A-FUN_Reorder_I.f90.hdr"
!!--begin--
INCLUDE "05-A-FUN_Reorder.f90.bdy"
!!--end--
END FUNCTION

!!### PURE FUNCTION  <<Reorder_I4>>
PURE FUNCTION Reorder_I4( old_list , order , side )  RESULT(new_list)
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I4     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-A-FUN_Reorder_I.f90.hdr"
!!--begin--
INCLUDE "05-A-FUN_Reorder.f90.bdy"
!!--end--
END FUNCTION

!!### PURE FUNCTION  <<Reorder_I8>>
PURE FUNCTION Reorder_I8( old_list , order , side )  RESULT(new_list)
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I8     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-A-FUN_Reorder_I.f90.hdr"
!!--begin--
INCLUDE "05-A-FUN_Reorder.f90.bdy"
!!--end--
END FUNCTION

!!### PURE FUNCTION  <<Reorder_L1>>
PURE FUNCTION Reorder_L1( old_list , order , side )  RESULT(new_list)
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L1     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-A-FUN_Reorder_L.f90.hdr"
!!--begin--
INCLUDE "05-A-FUN_Reorder.f90.bdy"
!!--end--
END FUNCTION

!!### PURE FUNCTION  <<Reorder_L2>>
PURE FUNCTION Reorder_L2( old_list , order , side )  RESULT(new_list)
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L2     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-A-FUN_Reorder_L.f90.hdr"
!!--begin--
INCLUDE "05-A-FUN_Reorder.f90.bdy"
!!--end--
END FUNCTION

!!### PURE FUNCTION  <<Reorder_L4>>
PURE FUNCTION Reorder_L4( old_list , order , side )  RESULT(new_list)
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L4     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-A-FUN_Reorder_L.f90.hdr"
!!--begin--
INCLUDE "05-A-FUN_Reorder.f90.bdy"
!!--end--
END FUNCTION

!!### PURE FUNCTION  <<Reorder_S>>
PURE FUNCTION Reorder_S( old_list , order , side )  RESULT(new_list)
INCLUDE "05-A-FUN_Reorder_S.f90.hdr"
!!--begin--
INCLUDE "05-A-FUN_Reorder.f90.bdy"
!!--end--
END FUNCTION

!!### PURE FUNCTION  <<Reorder_VS>>
PURE FUNCTION Reorder_VS( old_list , order , side )  RESULT(new_list)
INCLUDE "05-A-FUN_Reorder_VS.f90.hdr"
!!--begin--
INCLUDE "05-A-FUN_Reorder.f90.bdy"
!!--end--
END FUNCTION

END MODULE
