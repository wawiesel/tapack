!!# MODULE <<VAR_DiscreteOrdinates>>
MODULE VAR_DiscreteOrdinates

!!## PURPOSE
!! Variables for the discrete ordinates angular discretization.

!!## MODULES
USE KND_DiscreteOrdinates !!((02-A-KND_DiscreteOrdinates.f90))
USE PAR_DiscreteOrdinates !!((03-A-PAR_DiscreteOrdinates.f90))

!!## CHANGES
!! * v2.23 - added polar_angles and azimuthal_angles

!!## VARIABLES
REAL(KIND_DOR),POINTER :: Ordinates(:,:)=>NULL()
REAL(KIND_DOR),POINTER :: Azimuthal_Angles(:)=>NULL()
REAL(KIND_DOR),POINTER :: Polar_Angles(:)=>NULL()
REAL(KIND_DOR),POINTER :: Weights(:)=>NULL()
LOGICAL :: UniformZGeometry = DEFAULT_UniformZGeometry
INTEGER :: Order=DEFAULT_Order
INTEGER :: aOrder=DEFAULT_aOrder
INTEGER :: pOrder=DEFAULT_pOrder
CHARACTER(10) :: Quadrature=DEFAULT_Quadrature
CHARACTER(10) :: aQuadrature=DEFAULT_aQuadrature
CHARACTER(10) :: pQuadrature=DEFAULT_pQuadrature
INTEGER :: QuadratureType = DEFAULT_QuadratureType


END MODULE
