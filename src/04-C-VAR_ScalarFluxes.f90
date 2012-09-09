!!# MODULE <<VAR_ScalarFluxes>>
MODULE VAR_ScalarFluxes

!!## PURPOSE
!! Stores scalar fluxes.


!!## MODULES
USE KND_ScalarFluxes !!((02-A-KND_ScalarFluxes.f90))


!!## DEFAULT IMPLICIT
IMPLICIT NONE


!!## DEFAULT ACCESS
PRIVATE


!!## GLOBAL VARIABLES
!!
!!### High Order Variables
!! * high order cell scalar fluxes <ScalarFluxC>
!! * high order face scalar fluxes <ScalarFluxF>
!! * high order vert scalar fluxes <ScalarFluxV>
REAL(KIND_ScalarFlux) ,POINTER :: ScalarFluxC(:,:) => NULL()
REAL(KIND_ScalarFlux) ,POINTER :: ScalarFluxF(:,:) => NULL()
REAL(KIND_ScalarFlux) ,POINTER :: ScalarFluxV(:,:) => NULL()
!
!! * last iterations scalar flux <LastScalarFluxV>
REAL(KIND_ScalarFlux) ,POINTER :: LastScalarFluxV(:,:) => NULL()
REAL(KIND_ScalarFlux) ,POINTER :: LastScalarFluxF(:,:) => NULL()
REAL(KIND_ScalarFlux) ,POINTER :: LastScalarFluxC(:,:) => NULL()
!
!! * cell scalar flux cell function <ScalarFluxCellFunction>
REAL(KIND_ScalarFlux) ,POINTER :: ScalarFluxCellFunction(:,:,:) => NULL()
!!   e.g. a linear function for group $g$ and cell $i$ is represented as
!!   $\phi(x,y) = a + bx + cy$ where
!                 a=ScalarFluxFunction(1,g,i),
!                 b=ScalarFluxFunction(2,g,i),
!                 c=ScalarFluxFunction(3,g,i).
!
!! * "incoming" scalar fluxes on boundary <ScalarFluxIN>
REAL(KIND_ScalarFlux),POINTER :: ScalarFluxIN(:,:)   => NULL()
!
!
!!### Low Order Variables
!!
!! * low order cell scalar fluxes <LO_ScalarFluxC>
!! * low order face scalar fluxes <LO_ScalarFluxF>
!! * low order vert scalar fluxes <LO_ScalarFluxV>
REAL(KIND_ScalarFlux) ,POINTER :: LO_ScalarFluxC(:,:) => NULL()
REAL(KIND_ScalarFlux) ,POINTER :: LO_ScalarFluxF(:,:) => NULL()
REAL(KIND_ScalarFlux) ,POINTER :: LO_ScalarFluxV(:,:) => NULL()
REAL(KIND_ScalarFlux) ,POINTER :: LastLO_ScalarFluxV(:,:) => NULL()
REAL(KIND_ScalarFlux) ,POINTER :: LastLO_ScalarFluxF(:,:) => NULL()
REAL(KIND_ScalarFlux) ,POINTER :: LastLO_ScalarFluxC(:,:) => NULL()

!!## PUBLIC ACCESS
PUBLIC :: KIND_ScalarFlux
PUBLIC :: ScalarFluxV,ScalarFluxF,ScalarFluxC
PUBLIC :: LO_ScalarFluxV,LO_ScalarFluxF,LO_ScalarFluxC
PUBLIC :: LastScalarFluxV,LastScalarFluxF,LastScalarFluxC
PUBLIC :: LastLO_ScalarFluxV,LastLO_ScalarFluxF,LastLO_ScalarFluxC
PUBLIC :: ScalarFluxCellFunction
PUBLIC :: ScalarFluxIN

END MODULE

