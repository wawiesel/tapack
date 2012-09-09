!!# VARIABLES MODULE <<VAR_AngularFluxes>>
MODULE VAR_AngularFluxes

!!## PURPOSE
!! Store angular flux variables.


!!## USED MODULES
USE KND_AngularFluxes !!((02-A-KND_AngularFluxes.f90))


!!## DEFAULT IMPLICIT
IMPLICIT NONE
PRIVATE


!!## VARIABLES
!! * cell angular fluxes <AngularFluxC>
REAL(KIND_AngularFlux),POINTER :: AngularFluxC(:,:,:) => NULL()
!! * face angular fluxes <AngularFluxF>
REAL(KIND_AngularFlux),POINTER :: AngularFluxF(:,:,:) => NULL()
!! * vert angular fluxes <AngularFluxV>
REAL(KIND_AngularFlux),POINTER :: AngularFluxV(:,:,:) => NULL()


!!## PUBLIC VARIABLES
PUBLIC :: KIND_AngularFlux
PUBLIC :: AngularFluxC,AngularFluxF,AngularFluxV

END MODULE

