!!# VARIABLES MODULE: <VAR_Currents>
MODULE VAR_Currents

USE KND_Currents !!((02-A-KND_Currents.f90))

IMPLICIT NONE
PRIVATE

!!## GLOBAL VARIBLES
!!
!!### High order variables.
!!
!! * Current at verts <CurrentV(:,g,j)>
!!  : = vector -> 1:2 for 2D, 1:3 for 3D,
!!  g = energy,
!!  k = vertex index,
REAL(KIND_Current)   ,POINTER :: CurrentV( : , : , : )=>NULL()
!! * Current at faces <CurrentF(:,g,j)>
!!  : = vector -> 1:2 for 2D, 1:3 for 3D,
!!  g = energy,
!!  j = face/edge index,
REAL(KIND_Current)   ,POINTER :: CurrentF( : , : , : )=>NULL()
!! * Current at cells <CurrentC(:,g,j)>
!!  : = vector -> 1:2 for 2D, 1:3 for 3D,
!!  g = energy,
!!  j = face/edge index,
REAL(KIND_Current)   ,POINTER :: CurrentC( : , : , : )=>NULL()
!! * Current at faces projected in the direction of face normals
!!   <CurrentFN(g,j)>
!!  g = energy,
!!  j = face/edge index,
REAL(KIND_Current)   ,POINTER :: CurrentFN( : , : )=>NULL()
!
!! * "incoming" currents on boundary <CurrentIN>
REAL(KIND_Current),POINTER :: CurrentIN(:,:)      => NULL()


!!### Low order variables
!!
!! * Current at verts <LO_CurrentV(:,g,j)>
!!  : = vector -> 1:2 for 2D, 1:3 for 3D,
!!  g = energy,
!!  k = vertex index,
REAL(KIND_Current)   ,POINTER :: LO_CurrentV( : , : , : )=>NULL()
!! * Current at faces <LO_CurrentF(:,g,j)>
!!  : = vector -> 1:2 for 2D, 1:3 for 3D,
!!  g = energy,
!!  j = face/edge index,
REAL(KIND_Current)   ,POINTER :: LO_CurrentF( : , : , : )=>NULL()
!! * Current at cells <LO_CurrentC(:,g,j)>
!!  : = vector -> 1:2 for 2D, 1:3 for 3D,
!!  g = energy,
!!  j = face/edge index,
REAL(KIND_Current)   ,POINTER :: LO_CurrentC( : , : , : )=>NULL()
!! * Current at faces projected in the direction of face normals
!!   <LO_CurrentFN(g,j)>
!!  g = energy,
!!  j = face/edge index,
REAL(KIND_Current)   ,POINTER :: LO_CurrentFN( : , : )=>NULL()
REAL(KIND_Current)   ,POINTER :: LastLO_CurrentFN( : , : )=>NULL()
REAL(KIND_Current)   ,POINTER :: LastCurrentFN( : , : )=>NULL()

PUBLIC :: CurrentFN,CurrentV,CurrentF,CurrentC,LastCurrentFN
PUBLIC :: CurrentIN
PUBLIC :: LO_CurrentFN,LO_CurrentV,LO_CurrentF,LO_CurrentC,LastLO_CurrentFN
PUBLIC :: KIND_Current

END MODULE

