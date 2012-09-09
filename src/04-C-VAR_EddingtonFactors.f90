!!# MODULE <<VAR_EddingtonFactors>>
MODULE VAR_EddingtonFactors

!!## PURPOSE
!! The Eddington factor variables.

!!## USED MODULES
USE KND_EddingtonFactors !!((02-A-KND_EddingtonFactors.f90))

!!## IMPLICITNESS
IMPLICIT NONE
PRIVATE

!!## VARIABLES

!! * the actual moments of the transport equation
REAL(KIND_Eddington),POINTER :: &
  KxxV(:,:)=>NULL(),KyyV(:,:)=>NULL(),KxyV(:,:)=>NULL(),&
  KxxF(:,:)=>NULL(),KyyF(:,:)=>NULL(),KxyF(:,:)=>NULL(),&
  KxxC(:,:)=>NULL(),KyyC(:,:)=>NULL(),KxyC(:,:)=>NULL()
!
!! * vert Eddington factors <EddingtonV>
REAL(KIND_Eddington),POINTER :: EddingtonxxV(:,:)  => NULL()
REAL(KIND_Eddington),POINTER :: EddingtonxyV(:,:)  => NULL()
REAL(KIND_Eddington),POINTER :: EddingtonyyV(:,:)  => NULL()
!
!! * face Eddington factors <EddingtonF>
REAL(KIND_Eddington),POINTER :: EddingtonxxF(:,:)  => NULL()
REAL(KIND_Eddington),POINTER :: EddingtonxyF(:,:)  => NULL()
REAL(KIND_Eddington),POINTER :: EddingtonyyF(:,:)  => NULL()
!
!! * cell Eddington factors <EddigntonC>
REAL(KIND_Eddington),POINTER :: EddingtonxxC(:,:)  => NULL()
REAL(KIND_Eddington),POINTER :: EddingtonxyC(:,:)  => NULL()
REAL(KIND_Eddington),POINTER :: EddingtonyyC(:,:)  => NULL()
!
!! * Boundary Eddington Factors <CBoundary>
REAL(KIND_Eddington),POINTER :: CBoundary(:,:) => NULL()

PUBLIC :: KxxV,KyyV,KxyV,KxxF,KyyF,KxyF,KxxC,KyyC,KxyC
PUBLIC :: EddingtonxxV,EddingtonxyV,EddingtonyyV
PUBLIC :: EddingtonxxF,EddingtonxyF,EddingtonyyF
PUBLIC :: EddingtonxxC,EddingtonxyC,EddingtonyyC
PUBLIC :: CBoundary
PUBLIC :: KIND_Eddington

END MODULE

