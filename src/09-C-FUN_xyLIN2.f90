!!# FUNCTION MODULE <<FUN_xyLIN2>>
MODULE FUN_xyLIN2

!!## PURPOSE
!! Return a linear function in 2D, <Lin2>.


!!## DETAILS
!! The function is stored as <Lin2=(a,b,c)> where
!!
!! $$ f(x,y) = a + b x + c y. $$


!!## EXTERNAL MODULES
USE FUN_Error                                   !!((04-A-FUN_Error.f90))
USE INT_LAPACK2                                 !!((08-B-INT_LAPACK2.f90))


!!## DEFAULT IMPLICIT
IMPLICIT NONE


!!## PROCEDURE OVERLOADING
INTERFACE xyLIN2_Tr
 MODULE PROCEDURE xyLIN2_Tr_Rsp
 MODULE PROCEDURE xyLIN2_Tr_Rdp
END INTERFACE


!!## PUBLIC ACCESS LIST
PUBLIC :: xyLIN2_Tr


!!## CONTAINED PROCEDURES
CONTAINS


!!### FUNCTION <<xyLIN2_Tr_Rsp>>
FUNCTION xyLIN2_Tr_Rsp( Tr , Fv ) RESULT(Lin2)

!!#### LOCAL MAPPINGS
USE PAR_Constants_Rsp                           !!((02-A-PAR_Constants_Rsp.f90))
USE KND_IntrinsicTypes,ONLY: KIND_R => KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-C-FUN_xyLIN2_Tr.f90.hdr"
!!--begin--
INCLUDE "09-C-FUN_xyLIN2_Tr.f90.bdy"
!!--end--
END FUNCTION



!!### FUNCTION <<xyLIN2_Tr_Rdp>>
FUNCTION xyLIN2_Tr_Rdp( Tr , Fv ) RESULT(Lin2)

!!#### LOCAL MAPPINGS
USE PAR_Constants_Rsp                           !!((02-A-PAR_Constants_Rsp.f90))
USE KND_IntrinsicTypes,ONLY: KIND_R => KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-C-FUN_xyLIN2_Tr.f90.hdr"
!!--begin--
INCLUDE "09-C-FUN_xyLIN2_Tr.f90.bdy"
!!--end--
END FUNCTION



END MODULE
