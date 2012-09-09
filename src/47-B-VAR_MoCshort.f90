!!# MODULE <<VAR_MoCshort>>
MODULE VAR_MOCshort

!!## PURPOSE
!! Provides access to variables needed with the MOCshort
!! transport method.

!!## EXTERNAL KINDS
USE KND_Mesh              !!((05-B-KND_Mesh.f90))
USE KND_MoCshort          !!((03-A-KND_MoCshort.f90))
USE KND_DiscreteOrdinates !!((02-A-KND_DiscreteOrdinates.f90))
USE KND_XSExpansion       !!((02-A-KND_XSExpansion.f90))
USE KND_ScalarFluxes      !!((02-A-KND_ScalarFluxes.f90))

!!## EXTERNAL PARAMETERS
USE PAR_MoCshort          !!((03-A-PAR_MoCshort.f90))

!!## GLOBAL USER MODULES
USE ISO_varying_string    !!((03-A-ISO_varying_string.f90))
USE USR_pThread           !!((03-A-USR_pThread.f90))
USE FUN_MEMORYn           !!((04-A-FUN_MEMORYn.f90))
USE FUN_SIZEn             !!((07-B-FUN_SIZEn.f90))
USE FUN_Default           !!((04-A-FUN_Default.f90))
USE VAR_Units             !!((03-A-VAR_Units.f90))

!!## PARAMETERS
INTEGER,PARAMETER :: LEN_File = 67

!!## GLOBAL VARIABLES
!! * RecipSin( mz ) is the inverse of the sin of the azimuthal angle for direction z, (1/SIN(azi(mz)))
INTEGER             ,POINTER :: BC(:)=>NULL()
CHARACTER(LEN_File) ,POINTER :: initfile_AngularFlux(:)=>NULL()
REAL(KIND_MCs)      ,POINTER :: FixedAngularFlux(:)=>NULL()
INTEGER             ,POINTER :: FunctionAngularFlux(:)=>NULL()

REAL(KIND_MCs)      ,POINTER :: RecipSin( : )=>NULL()
REAL(KIND_MCs)      ,POINTER :: PolSin( : )=>NULL()

INTEGER             ,POINTER :: WithinCell(:,:)=>NULL()
REAL(KIND_MSH)      ,POINTER :: SourceDist(:,:)=>NULL()
REAL(KIND_MSH)      ,POINTER :: StreamDist(:,:,:)=>NULL()
INTEGER             ,POINTER :: k_(:,:,:)=>NULL()
TYPE(TYPE_pThread)  ,POINTER :: pThread(:)=>NULL()
REAL(KIND_MSH)      ,POINTER :: FrontPos(:,:,:)=>NULL()
INTEGER             ,POINTER :: NearestFace(:,:)=>NULL()

INTEGER                      :: InterpOrder  = 1 , SourceOrder = 1
LOGICAL                      :: OnlyGeometry = .FALSE.
TYPE(varying_string) :: NO_BALANCE_MSG
LOGICAL :: CHECKING_BALANCE = .TRUE.
CHARACTER(20) :: affile=''
REAL(KIND_MCS),POINTER :: PointList_DebugPsiV(:,:)=>NULL()
INTEGER :: Unit_DebugPsiV=0

!11. Options
!! * which plane to use when calculating the streaming part of the angular flux
!!   solution by interpolating the projection of other intra-cell vertices
!!   onto a plane defined by the vert in which you are calculating the angular
!!   flux and some direction <U> which you must choose <InterpolationPlaneU>
INTEGER :: InterpPlaneU = MCS_Face
!INTEGER :: InterpPlaneU = MCS_Perp
!INTEGER :: InterpPlaneU = MCS_Diag

!!#### CONVERGENCE CRITERIA
REAL(KIND_ScalarFlux) :: residPhiVInf=1
REAL(KIND_ScalarFlux) :: normPhiVInf_1=1
REAL(KIND_ScalarFlux) :: normPhiVInf_2=1
REAL(KIND_ScalarFlux) :: residPhiCL1=1
REAL(KIND_ScalarFlux) :: normPhiCL1_1=1
REAL(KIND_ScalarFlux) :: normPhiCL1_2=1
REAL(KIND_ScalarFlux) :: residPhiCL2=1
REAL(KIND_ScalarFlux) :: normPhiCL2_1=1
REAL(KIND_ScalarFlux) :: normPhiCL2_2=1
REAL(KIND_ScalarFlux) :: spec_rad=0
INTEGER :: COUNT_Monotonizations = 0

INTEGER :: Unit_LinearSourceTest = 0
LOGICAL :: Using_LinearSourceTest = .FALSE.
LOGICAL :: InteractiveAngularFlux = .FALSE.
LOGICAL :: Using_Monotonization=.TRUE.
LOGICAL :: Using_MonoLin = .FALSE.
LOGICAL :: Using_Splitting = .FALSE.
LOGICAL :: Using_LogTransform = .FALSE.
LOGICAL :: Using_Jiggle =.FALSE.
LOGICAL :: Using_ExplodeFix=.FALSE.
LOGICAL :: Using_NoBacksies=.FALSE.
LOGICAL :: Using_LongCharacteristics=.FALSE.
LOGICAL :: Using_SBCharacteristics=.FALSE.
LOGICAL :: Using_CachedLongCharacteristics=.TRUE.
LOGICAL :: Using_PackedCaching=.FALSE.
LOGICAL :: Using_AFSymmetryCheck=.FALSE.
LOGICAL :: Using_Cache=.FALSE.
LOGICAL :: Using_AF_ScaleFactor=.FALSE.
LOGICAL :: Allow_Discontinuous_Corners=.FALSE.
REAL(KIND_AngularFlux) :: MAX_AngularFlux=0.2_KIND_AngularFlux
REAL(KIND_AngularFlux) :: MIN_AngularFlux=0.0_KIND_AngularFlux
REAL(KIND_AngularFlux) :: AF_ScaleFactor=1.0_KIND_AngularFlux

LOGICAL,POINTER :: FaceIntersectMask(:,:,:)=>NULL()
REAL(KIND_AngularFlux),POINTER :: Ln_Front(:,:,:,:)=>NULL()
REAL(KIND_AngularFlux) :: P1sym(2),P2sym(2)
INTEGER :: Unit_AFSymmetryCheck=0
INTEGER :: CharacteristicsSolver=0
REAL(KIND_AngularFlux),POINTER :: PointList_LongChar(:,:)=>NULL()
INTEGER :: Unit_LongChar=0
PRIVATE :: LEN_File
INTEGER :: CellFunctionMethod = MCS_LINEAR_GAUSS
INTEGER :: CellFunctionMethodA = MCS_LINEAR_GAUSS
LOGICAL :: Using_AnalyticTransportTest = .FALSE.
LOGICAL :: Print_RayEffectsInfo = .FALSE.
CHARACTER(64) :: rayeffectsfile

INTEGER :: NonlinearFixup     = MCS_NONE

CONTAINS

SUBROUTINE PRINT_MCS_Memory(Unit)
INTEGER,INTENT(IN),OPTIONAL :: Unit

INTEGER :: Unit_

!!--begin--

Unit_ = Default( DEFAULT_OUTPUT_UNIT , Unit )

11 FORMAT(10x,a26,f12.5,a)
WRITE(Unit_,"(a)")"Memory Usage Summary for MCS"
WRITE(Unit_,11)"BC",&
     MEMORYn(BC)/1.d6," MB"
WRITE(Unit_,11)"initfile_AngularFlux",&
     MEMORYn(initfile_AngularFlux)/1.d6," MB"
WRITE(Unit_,11)"FixedAngularFlux",&
     MEMORYn(FixedAngularFlux)/1.d6," MB"
WRITE(Unit_,11)"FunctionAngularFlux",&
     MEMORYn(FunctionAngularFlux)/1.d6," MB"
WRITE(Unit_,11)"RecipSin",&
     MEMORYn(RecipSin)/1.d6," MB"
WRITE(Unit_,11)"PolSin",&
     MEMORYn(PolSin)/1.d6," MB"
WRITE(Unit_,11)"WithinCell",&
     MEMORYn(WithinCell)/1.d6," MB"
WRITE(Unit_,11)"SourceDist",&
     MEMORYn(SourceDist)/1.d6," MB"
WRITE(Unit_,11)"StreamDist",&
     MEMORYn(StreamDist)/1.d6," MB"
WRITE(Unit_,11)"k_",&
     MEMORYn(k_)/1.d6," MB"
WRITE(Unit_,11)"pThread",&
     MEMORYn_pThread(pThread)/1.d6," MB"
WRITE(Unit_,11)"FrontPos",&
     MEMORYn(FrontPos)/1.d6," MB"
WRITE(Unit_,11)"NearestFace",&
     MEMORYn(NearestFace)/1.d6," MB"

!LOGICAL,POINTER :: FaceIntersectMask(:,:,:)=>NULL()
!REAL(KIND_AngularFlux),POINTER :: Ln_Front(:,:,:,:)=>NULL()

WRITE(Unit_,"(a)")"  *only reporting non-scalar variables"

END SUBROUTINE

FUNCTION MEMORYn_pThread(pThread) RESULT(MEMORY)
TYPE(TYPE_pThread),POINTER :: pThread(:)
INTEGER :: MEMORY
INTEGER :: m,p

!!--begin--
IF( ASSOCIATED(pThread) )THEN
 MEMORY = 0
 DO m = 1,SIZE(pThread)
  IF( ASSOCIATED(pThread(m)%path) )THEN
   DO p = 1,SIZE(pThread(m)%path)
    MEMORY = MEMORY + SIZEn(pThread(m)%path(p)%order)
   END DO
  END IF
 END DO
ELSE
 MEMORY = 0
END IF

!!--end--
END FUNCTION

END MODULE
