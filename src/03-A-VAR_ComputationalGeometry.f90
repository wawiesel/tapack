!!# MODULE <<VAR_ComputationalGeometry>>
MODULE VAR_ComputationalGeometry

!!## PURPOSE
!! This module contains the variables needed for computational
!! geometry.


!!## DEPENDENCIES
USE KND_IntrinsicTypes        !!((01-A-KND_IntrinsicTypes.f90))
USE PAR_ComputationalGeometry !!((02-A-PAR_ComputationalGeometry.f90))


!!## DEFAULT IMPLICIT
IMPLICIT NONE


!!## DEFAULT ACCESS
PRIVATE


!!## GLOBAL VARIABLES
!! * tolerances for single and double precision computations
REAL(KIND_Rsp) :: DEFAULT_TOL_Rsp = 1.E-06_KIND_Rdp
REAL(KIND_Rdp) :: DEFAULT_TOL_Rdp = 1.E-12_KIND_Rdp


!!## PUBLIC
PUBLIC :: DEFAULT_TOL_Rsp
PUBLIC :: DEFAULT_TOL_Rdp


END MODULE
