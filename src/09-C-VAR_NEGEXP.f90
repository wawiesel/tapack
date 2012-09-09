!!# MODULE <<VAR_NEGEXP>>
MODULE VAR_NEGEXP

!!## PURPOSE
!! Variables for the negative exponential user routines.

!!## MODULES
USE KND_IntrinsicTypes !!((01-A-KND_IntrinsicTypes.f90))

!!## VARIABLES
REAL(KIND_Rsp),POINTER :: x_Rsp(:)=>NULL(),y_Rsp(:)=>NULL()
REAL(KIND_Rdp),POINTER :: x_Rdp(:)=>NULL(),y_Rdp(:)=>NULL()


END MODULE
