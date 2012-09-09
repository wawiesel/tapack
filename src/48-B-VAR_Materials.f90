!!# VARIABLE MODULE: <<VAR_Materials>>
MODULE VAR_Materials

!!## PURPOSE
!! Variables storage for material-oriented variables.

!!## GLOBAL USER MODULES
USE USR_Materials    !!((47-B-USR_Materials.f90))
USE KND_ScalarFluxes !!((02-A-KND_ScalarFluxes.f90))

!!## GLOBAL VARIABLES
!! * forward mapping of cell index to material index <l_>
!! * backward mapping of material index to cell index <i_>
!! * material names <LABEL_Materials>
!! * material fills <MaterialFill>
INTEGER                ,POINTER :: l_( : )=>NULL()
INTEGER                ,POINTER :: i_( : )=>NULL()
CHARACTER(LEN_Material),POINTER :: LABEL_Materials(:)=>NULL()
TYPE(TYPE_MaterialFill),POINTER :: MaterialFill(:)=>NULL()
!
!! * Scalar Flux Coefficient by Material (sum of scattering and nu*fission)
REAL(KIND_ScalarFlux),POINTER :: CoeffScalarFluxM(:,:) !g,l


!!## ACCESS
PUBLIC :: TYPE_MaterialFill
PUBLIC :: Reallocate
PUBLIC :: CoeffScalarFluxM

END MODULE
