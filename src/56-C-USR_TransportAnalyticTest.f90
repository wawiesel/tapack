!!# USER MODULE <<USR_TransportAnalyticTest>>
MODULE USR_TransportAnalyticTest

USE KND_MoCshort                     !!((03-A-KND_MoCshort.f90))
USE VAR_DiscreteOrdinates            !!((47-B-VAR_DiscreteOrdinates.f90))
USE LIB_genMoments                   !!((13-B-LIB_genMoments.f90))
USE USR_fdbk                         !!((08-C-USR_fdbk.f90))
USE KND_XSExpansion                  !!((02-A-KND_XSExpansion.f90))
USE USR_Mesh                         !!((14-B-USR_Mesh.f90))
USE FUN_STR                          !!((05-B-FUN_STR.f90))
USE USR_TransportAnalyticTest_WARSA  !!((55-C-USR_TransportAnalyticTest_WARSA.f90))
USE USR_TransportAnalyticTest_WIESEL !!((55-C-USR_TransportAnalyticTest_WIESEL.f90))
USE ISO_varying_string               !!((03-A-ISO_varying_string.f90))
USE PRN_Mesh                         !!((16-C-PRN_Mesh.f90))
USE USR_TAPACK                       !!((48-C-USR_TAPACK.f90))
USE TBX_Mesh                         !!((15-B-TBX_Mesh.f90))
USE KND_EddingtonFactors             !!((02-A-KND_EddingtonFactors.f90))
USE KND_Currents                     !!((02-A-KND_Currents.f90))
USE FUN_NewFile                      !!((05-B-FUN_NewFile.f90))
USE USR_QDAnalyticTest, ONLY: EXACT_LOQD_Phi=>EXACT_Phi,&
EXACT_LOQD_Exx=>EXACT_Exx,EXACT_LOQD_Exy=>EXACT_Exy,EXACT_LOQD_Eyy=>EXACT_Eyy,&
EXACT_LOQD_Q=>EXACT_Q,&
EXACT_LOQD_Jy=>EXACT_Jy,EXACT_LOQD_Jx=>EXACT_Jx

IMPLICIT NONE

PRIVATE

!!## LOCAL PARAMETERS
CHARACTER(*),PARAMETER :: KEYS_TRANSPORT_ANALYTIC_TEST(2)=(/"WARSA ",&
                                                            "WIESEL"/)
INTEGER,PARAMETER :: TRANSPORT_ANALYTIC_TEST_WARSA  = 1
INTEGER,PARAMETER :: TRANSPORT_ANALYTIC_TEST_WIESEL = 2

!!## LOCAL VARIABLES
INTEGER        :: TransportAnalyticTest = TRANSPORT_ANALYTIC_TEST_WARSA
REAL(KIND_MCS) :: TRANSPORT_ANALYTIC_SIGMAT
REAL(KIND_MCS) :: TRANSPORT_ANALYTIC_SIGMAS
REAL(KIND_MCS) :: TRANSPORT_ANALYTIC_CONST

PUBLIC :: SET_AnalyticTest
PUBLIC :: SETUP_AnalyticTest
PUBLIC :: UPDATE_CellAvgMom0QextA
PUBLIC :: UPDATE_FaceAvgMom1QextA
PUBLIC :: UPDATE_QextA
PUBLIC :: gmv_AnalyticTestOutput
PUBLIC :: gmv_LOQDAnalyticTestOutput
!PUBLIC :: TransportAnalyticTest
!PUBLIC :: TRANSPORT_ANALYTIC_TEST_WARSA
!PUBLIC :: TRANSPORT_ANALYTIC_TEST_WIESEL
!PUBLIC :: TRANSPORT_ANALYTIC_SIGMAT
!PUBLIC :: TRANSPORT_ANALYTIC_SIGMAS
!PUBLIC :: TRANSPORT_ANALYTIC_CONST
!PUBLIC :: KEYS_TRANSPORT_ANALYTIC_TEST
!!#### temps
PUBLIC :: EXACT_Exx
PUBLIC :: EXACT_SnExx
PUBLIC :: EXACT_Kxx
PUBLIC :: EXACT_SnKxx
PUBLIC :: EXACT_Qext
PUBLIC :: OutputVertAFValues
CONTAINS

SUBROUTINE SET_AnalyticTest(Name,sigt,sigs,fdbk)
CHARACTER(*),INTENT(IN) :: Name
REAL(KIND_MCS),INTENT(IN) :: sigt,sigs
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk
!!--begin--
SELECT CASE(Name)
 CASE("WARSA")  ; TransportAnalyticTest = TRANSPORT_ANALYTIC_TEST_WARSA
 CASE("WIESEL") ; TransportAnalyticTest = TRANSPORT_ANALYTIC_TEST_WIESEL
 CASE DEFAULT
  CALL UpdateAndDump(fdbk_error,fdbk,s="     No analytic test by that name!")
END SELECT
TRANSPORT_ANALYTIC_SIGMAT = sigt
TRANSPORT_ANALYTIC_SIGMAS = sigs
!!--end--
END SUBROUTINE

SUBROUTINE SETUP_AnalyticTest(Caller,&
  MacS,MacF,MacNu,MacT,l_,CoeffScalarFluxM,&
  Mesh,&
  fdbk)
CHARACTER(*),INTENT(IN) :: Caller
REAL(KIND_Mac),INTENT(INOUT) :: MacS(:,:),MacF(:,:),MacNu(:,:),MacT(:,:)
INTEGER,INTENT(IN) :: l_(:)
REAL(KIND_Mac),INTENT(INOUT) :: CoeffScalarFluxM(:,:)
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk
!!--begin--
SELECT CASE(TransportAnalyticTest)
 CASE(TRANSPORT_ANALYTIC_TEST_WARSA)
   CALL SETUP_AnalyticTest_WARSA(Caller,&
    TRANSPORT_ANALYTIC_SIGMAT,&
    TRANSPORT_ANALYTIC_SIGMAS,&
    TRANSPORT_ANALYTIC_CONST,&
    MacS,MacF,MacNu,MacT,l_,CoeffScalarFluxM,&
    Mesh,&
    fdbk)

 CASE(TRANSPORT_ANALYTIC_TEST_WIESEL)
   CALL SETUP_AnalyticTest_WIESEL(Caller,&
    TRANSPORT_ANALYTIC_SIGMAT,&
    TRANSPORT_ANALYTIC_SIGMAS,&
    TRANSPORT_ANALYTIC_CONST,&
    MacS,MacF,MacNu,MacT,l_,CoeffScalarFluxM,&
    Mesh,&
    fdbk)

 CASE DEFAULT
   CALL UpdateAndDump(fdbk_error,fdbk,s=Caller//" The variable <TransportAnalyticTest>&
     & has not been set.")

END SELECT

!!--end--
END SUBROUTINE


SUBROUTINE UPDATE_FaceAvgMom1QextA(Mesh,FaceAvgMom1ExtSource)
!!#### PURPOSE
!! Produce the face-average 1st moment of the analytic external source.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
REAL(KIND_MCS) ,POINTER    :: FaceAvgMom1ExtSource(:,:,:)

!!#### LOCAL VARIABLES
INTEGER :: g,Ng,j,Nj
INTEGER :: n
REAL(KIND_MCS) :: x,y

!!--begin--

Ng=SIZE(FaceAvgMom1ExtSource,2)
Nj=NUM_Faces(Mesh)

DO j=1,Nj
 DO g=1,Ng
  FaceAvgMom1ExtSource(1,g,j) = FaceAverage_F( Mesh , j , EXACT_SnMom1xQext )
  FaceAvgMom1ExtSource(2,g,j) = FaceAverage_F( Mesh , j , EXACT_SnMom1yQext )
 END DO
END DO

!!--end--
END SUBROUTINE



SUBROUTINE UPDATE_CellAvgMom0QextA(Method,Mesh,ExtSourceCellFunction,ReleaseCache)
!!#### PURPOSE
!! Produce the analytic external source which has been angle-integrated.

!!#### REQUIRED INPUT
CHARACTER(*)   ,INTENT(IN) :: Method
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
REAL(KIND_MCS) ,POINTER    :: ExtSourceCellFunction(:,:,:)
!caching variables
LOGICAL,INTENT(IN),OPTIONAL :: ReleaseCache

!!#### LOCAL VARIABLES
REAL(KIND_MCS),ALLOCATABLE :: ExtSourceC(:,:)
REAL(KIND_MCS),ALLOCATABLE :: ExtSourceF(:,:)
INTEGER :: i,Ni,g,Ng,j,Nj

!caching variables
LOGICAL,PARAMETER :: CACHE_SOURCE=.FALSE.
INTEGER,PARAMETER :: NUMBER_OF_CELLS_CACHE_LIMIT=4000
LOGICAL,SAVE :: HaveCached=.FALSE.
LOGICAL :: UseCached
REAL(KIND_MCS),POINTER :: CACHE_ExtSourceCellFunction(:,:,:)

!!--begin--

IF( PRESENT(ReleaseCache) )THEN
 IF( ReleaseCache )THEN
  DEALLOCATE( CACHE_ExtSourceCellFunction )
  RETURN
 END IF
END IF

Ni=NUM_Cells(Mesh)
Ng=SIZE(ExtSourceCellFunction,2)
Nj=NUM_Faces(Mesh)

UseCached = CACHE_SOURCE .AND. Ni<=NUMBER_OF_CELLS_CACHE_LIMIT

IF( .NOT.UseCached )THEN

 CALL Generate_From_Scratch(ExtSourceCellFunction)

ELSE
 IF( .NOT.HaveCached )THEN
  ALLOCATE( CACHE_ExtSourceCellFunction(&
    SIZE(ExtSourceCellFunction,1),&
    SIZE(ExtSourceCellFunction,2),&
    SIZE(ExtSourceCellFunction,3)) )
  CALL Generate_From_Scratch(CACHE_ExtSourceCellFunction)
  HaveCached = .TRUE.
 END IF
 ExtSourceCellFunction = CACHE_ExtSourceCellFunction
END IF

!!--end--
CONTAINS

SUBROUTINE Generate_From_Scratch(ExtSourceCellFunction)
REAL(KIND_MCS),INTENT(INOUT) :: ExtSourceCellFunction(:,:,:)

!get cell-average and face-average external sources
 ALLOCATE( ExtSourceC(Ng,Ni) )
 DO i=1,Ni
  DO g=1,Ng
   ExtSourceC(g,i) = CellAverage_F( Mesh , i , EXACT_SnMom0Qext )
  END DO
 END DO

 ALLOCATE( ExtSourceF(Ng,Nj) )
 DO j=1,Nj
  DO g=1,Ng
   ExtSourceF(g,j) = FaceAverage_F( Mesh , j , EXACT_SnMom0Qext )
  END DO
 END DO

 !get cell function
 CALL UPDATE_CellFunction(Mesh,ExtSourceC,ExtSourceF,ExtSourceCellFunction,&
    Method=Method,&
    NonlinearFixup="None")

 DEALLOCATE( ExtSourceC , ExtSourceF )
END SUBROUTINE

END SUBROUTINE


SUBROUTINE UPDATE_QextA(Method,Mesh,ExtSourceCellFunction,Omega,mIndices,ReleaseCache)
!!#### PURPOSE
!! Produce the analytic external source which has NOT been angle-integrated for
!! direction Omega.

!!#### REQUIRED INPUT
CHARACTER(*)   ,INTENT(IN) :: Method
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
REAL(KIND_MCS) ,INTENT(INOUT) :: ExtSourceCellFunction(:,:,:)
REAL(KIND_MCS) ,INTENT(IN) :: Omega(1:2)
INTEGER        ,INTENT(IN),OPTIONAL :: mIndices(2)

!caching variables
LOGICAL,INTENT(IN),OPTIONAL :: ReleaseCache

!caching variables
LOGICAL,PARAMETER :: CACHE_SOURCE=.FALSE.
INTEGER,PARAMETER :: NUMBER_OF_CELLS_CACHE_LIMIT=4000
LOGICAL,SAVE,ALLOCATABLE :: HaveCached(:)
LOGICAL :: UseCached
INTEGER :: m,Nm
REAL(KIND_MCS),POINTER :: CACHE_ExtSourceCellFunction(:,:,:,:)

!!--begin--
IF( PRESENT(ReleaseCache) )THEN
 IF( ReleaseCache )THEN
  DEALLOCATE( CACHE_ExtSourceCellFunction )
  RETURN
 END IF
END IF

UseCached = CACHE_SOURCE .AND. &
  (NUM_Cells(Mesh)<=NUMBER_OF_CELLS_CACHE_LIMIT .AND. PRESENT(mIndices))

IF( .NOT.UseCached )THEN
 !WRITE(*,*)"not using cached"
 CALL Generate_From_Scratch(Method,Mesh,Omega,ExtSourceCellFunction)

ELSE
 WRITE(*,*)"using cached"
 m=mIndices(1)
 Nm=mIndices(2)
 WRITE(*,*)"m=",m
 WRITE(*,*)"Nm=",Nm

 IF( .NOT.ALLOCATED(HaveCached) )THEN
  ALLOCATE( HaveCached(Nm) )
  HaveCached = .FALSE.
 END IF
 WRITE(*,*)"after allocating havecached"

 IF( .NOT.HaveCached(m) )THEN
  ALLOCATE( CACHE_ExtSourceCellFunction(&
    SIZE(ExtSourceCellFunction,1),&
    SIZE(ExtSourceCellFunction,2),&
    SIZE(ExtSourceCellFunction,3),Nm) )
  WRITE(*,*)"after "
  DO m=1,Nm
   WRITE(*,*)"generating m=",m
   CALL Generate_From_Scratch(Method,Mesh,Omega,CACHE_ExtSourceCellFunction(:,:,:,m))
  END DO
  HaveCached=.TRUE.
 END IF
 WRITE(*,*)"After allocating the cache_source"
 ExtSourceCellFunction = CACHE_ExtSourceCellFunction(:,:,:,m)

END IF

!!--end--
END SUBROUTINE


SUBROUTINE Generate_From_Scratch(Method,Mesh,Omega,ExtSourceCellFunction)

!!#### REQUIRED INPUT
CHARACTER(*)   ,INTENT(IN)    :: Method
TYPE(TYPE_Mesh),INTENT(IN)    :: Mesh
REAL(KIND_MCS) ,INTENT(INOUT) :: ExtSourceCellFunction(:,:,:)
REAL(KIND_MCS) ,INTENT(IN)    :: Omega(1:2)

!!#### LOCAL VARIABLES
REAL(KIND_MCS),ALLOCATABLE :: ExtSourceC(:,:)
REAL(KIND_MCS),ALLOCATABLE :: ExtSourceF(:,:)
REAL(KIND_MCS) :: int1,intx,inty,v1,vx,vy
REAL(KIND_MCS) :: csm(6),x2_e,y2_e,xy_e,det,cv,ax,ay
INTEGER :: i,Ni,g,Ng,j,Nj

!!--begin--

Ng=SIZE(ExtSourceCellFunction,2)
Ni=NUM_Cells(Mesh)
Nj=NUM_Faces(Mesh)

!failed attempt at moment matching
  DO i=1,Ni
   DO g=1,Ng
    CALL GET_CellSpatialMoments( Mesh , i , csm , Center=.TRUE. )

    cv   = csm(1)
    x2_e = csm(4)/cv
    y2_e = csm(5)/cv
    xy_e = csm(6)/cv


    int1 = CellAverage_F( Mesh , i , QextLocal , Center=.TRUE. )
    intx = CellAverage_F( Mesh , i , QextLocalX , Center=.TRUE. )
    inty = CellAverage_F( Mesh , i , QextLocalY , Center=.TRUE. )

    !inverse transformation (application of $R_{xy}^{-1}$ to $(int1,intx,inty)$.
    ax = xy_e/x2_e
    ay = xy_e/y2_e
    det = (1.d0 - ax*ay)
    v1 = +int1
    vx = +   intx/det - ax*inty/det
    vy = -ay*intx/det +    inty/det

    !external source update
    ExtSourceCellFunction(:,g,i) = (/v1,vx,vy/)

   END DO
  END DO


SELECT CASE(TRIM(Method))
 CASE("Moments")
  !see failure above
  STOP

 CASE DEFAULT
 !get cell-average and face-average external sources
 !write(*,*)"Ng=",Ng
 !write(*,*)"Ni=",Ni
 !write(*,*)"before allocateC"
 ALLOCATE( ExtSourceC(Ng,Ni) )
 !write(*,*)"after allocateC"
 DO i=1,Ni
  DO g=1,Ng
   !WRITE(*,*)"i,g=",i,g
   ExtSourceC(g,i) = CellAverage_F( Mesh , i , QextLocal )
  END DO
 END DO

 !write(*,*)"before allocateF"
 ALLOCATE( ExtSourceF(Ng,Nj) )
 DO j=1,Nj
  DO g=1,Ng
   !write(*,*)"g,j=",g,j
   ExtSourceF(g,j) = FaceAverage_F( Mesh , j , QextLocal )
  END DO
 END DO

 !get cell function
 CALL UPDATE_CellFunction(Mesh,ExtSourceC,ExtSourceF,ExtSourceCellFunction,&
    Method=Method,&
    NonlinearFixup="None")
 DEALLOCATE( ExtSourceC , ExtSourceF )

END SELECT

CONTAINS

FUNCTION QextLocal(x,y) RESULT(val)
REAL(KIND_MCS),INTENT(IN) :: x,y
REAL(KIND_MCS) :: val
!!--begin--
val = EXACT_SnQext(x,y,Omega)
!!--end--
END FUNCTION

FUNCTION QextLocalX(x,y) RESULT(val)
REAL(KIND_MCS),INTENT(IN) :: x,y
REAL(KIND_MCS) :: val
!!--begin--
val = EXACT_SnQext(x,y,Omega)*x
!!--end--
END FUNCTION

FUNCTION QextLocalY(x,y) RESULT(val)
REAL(KIND_MCS),INTENT(IN) :: x,y
REAL(KIND_MCS) :: val
!!--begin--
val = EXACT_SnQext(x,y,Omega)*y
!!--end--
END FUNCTION

END SUBROUTINE



SUBROUTINE gmv_AnalyticTestOutput(Mesh,LO_Mesh,&
  ScalarFluxV,ScalarFluxF,ScalarFluxC,&
  LO_ScalarFluxV,LO_ScalarFluxF,LO_ScalarFluxC,&
  EddingtonxxV,EddingtonyyV,EddingtonxyV,&
  EddingtonxxF,EddingtonyyF,EddingtonxyF,&
  EddingtonxxC,EddingtonyyC,EddingtonxyC,&
  KxxV,KyyV,KxyV,&
  KxxF,KyyF,KxyF,&
  KxxC,KyyC,KxyC,&
  CurrentFN,LO_CurrentFN,&
  AngularFluxV,AngularFluxF,AngularFLuxC,&
  Directions,&
  OutputFileBase,Unit)
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
TYPE(TYPE_Mesh),INTENT(IN) :: LO_Mesh
CHARACTER(*),INTENT(IN) :: OutputFileBase
REAL(KIND_ScalarFlux),POINTER :: ScalarFluxV(:,:)
REAL(KIND_ScalarFlux),POINTER :: ScalarFluxF(:,:)
REAL(KIND_ScalarFlux),POINTER :: ScalarFluxC(:,:)
REAL(KIND_ScalarFlux),POINTER :: LO_ScalarFluxV(:,:)
REAL(KIND_ScalarFlux),POINTER :: LO_ScalarFluxF(:,:)
REAL(KIND_ScalarFlux),POINTER :: LO_ScalarFluxC(:,:)
REAL(KIND_Eddington),POINTER :: EddingtonxxV(:,:),EddingtonyyV(:,:),EddingtonxyV(:,:)
REAL(KIND_Eddington),POINTER :: EddingtonxxF(:,:),EddingtonyyF(:,:),EddingtonxyF(:,:)
REAL(KIND_Eddington),POINTER :: EddingtonxxC(:,:),EddingtonyyC(:,:),EddingtonxyC(:,:)
REAL(KIND_Eddington),POINTER :: KxxV(:,:),KyyV(:,:),KxyV(:,:)
REAL(KIND_Eddington),POINTER :: KxxF(:,:),KyyF(:,:),KxyF(:,:)
REAL(KIND_Eddington),POINTER :: KxxC(:,:),KyyC(:,:),KxyC(:,:)
REAL(KIND_Current),POINTER :: CurrentFN(:,:),LO_CurrentFN(:,:)
REAL(KIND_ScalarFlux),POINTER :: AngularFluxV(:,:,:),AngularFluxF(:,:,:),AngularFluxC(:,:,:)
REAL(KIND_MCS),INTENT(IN) :: Directions(:,:)
INTEGER,INTENT(IN) :: Unit
INTEGER :: UnitAF
LOGICAL :: OutputSnResults=.TRUE.
!!--begin--

UnitAF=NewFile(TRIM(OutputFileBase)//".Psi.gmvvar")

CALL OutputVertValues(Mesh,"ExactPhiV",EXACT_Phi,Unit)
CALL OutputVertValues(Mesh,"ExactJxV",EXACT_Jx,Unit)
CALL OutputVertValues(Mesh,"ExactJyV",EXACT_Jy,Unit)
CALL OutputVertValues(Mesh,"ExactExxV",EXACT_Exx,Unit)
CALL OutputVertValues(Mesh,"ExactEyyV",EXACT_Eyy,Unit)
CALL OutputVertValues(Mesh,"ExactExyV",EXACT_Exy,Unit)
CALL OutputCellValues(Mesh,"ExactPhiC",EXACT_Phi,Unit)
CALL OutputCellValues(Mesh,"ExactJxC",EXACT_Jx,Unit)
CALL OutputCellValues(Mesh,"ExactJyC",EXACT_Jy,Unit)
CALL OutputCellValues(Mesh,"ExactExxC",EXACT_Exx,Unit)
CALL OutputCellValues(Mesh,"ExactEyyC",EXACT_Eyy,Unit)
CALL OutputCellValues(Mesh,"ExactExyC",EXACT_Exy,Unit)
CALL OutputCellValues(Mesh,"ExactKxxC",EXACT_Kxx,Unit)
CALL OutputCellValues(Mesh,"ExactKyyC",EXACT_Kyy,Unit)
CALL OutputCellValues(Mesh,"ExactKxyC",EXACT_Kxy,Unit)

IF( OutputSnResults )THEN
 CALL OutputVertValues(Mesh,"ExactSnPhiV",EXACT_SnPhi,Unit)
 CALL OutputVertValues(Mesh,"ExactSnJxV",EXACT_SnJx,Unit)
 CALL OutputVertValues(Mesh,"ExactSnJyV",EXACT_SnJy,Unit)
 CALL OutputVertValues(Mesh,"ExactSnExxV",EXACT_SnExx,Unit)
 CALL OutputVertValues(Mesh,"ExactSnEyyV",EXACT_SnEyy,Unit)
 CALL OutputVertValues(Mesh,"ExactSnExyV",EXACT_SnExy,Unit)

 CALL OutputCellValues(Mesh,"ExactSnPhiC",EXACT_SnPhi,Unit)
 CALL OutputCellValues(Mesh,"ExactSnJxC",EXACT_SnJx,Unit)
 CALL OutputCellValues(Mesh,"ExactSnJyC",EXACT_SnJy,Unit)
 CALL OutputCellValues(Mesh,"ExactSnExxC",EXACT_SnExx,Unit)
 CALL OutputCellValues(Mesh,"ExactSnEyyC",EXACT_SnEyy,Unit)
 CALL OutputCellValues(Mesh,"ExactSnExyC",EXACT_SnExy,Unit)

 CALL OutputCellValues(Mesh,"ExactSnKxxC",EXACT_SnKxx,Unit)
 CALL OutputCellValues(Mesh,"ExactSnKyyC",EXACT_SnKyy,Unit)
 CALL OutputCellValues(Mesh,"ExactSnKxyC",EXACT_SnKxy,Unit)
END IF


!vertex psi errors
CALL OutputVertAFErrors(Mesh,&
  ext="errpsiv",VarName="PsiV",&
  ExactFunction=EXACT_Psi,HIVar=AngularFluxV(1,:,:),&
  Directions=Directions,&
  gmvUnit=UnitAF,OutputFileBase=OutputFileBase)


!vertex errors
CALL OutputVertErrors(Mesh,LO_Mesh,&
  ext="errexxv",VarName="ExxV",&
  ExactFunction=EXACT_Exx,LOVar=EddingtonxxV(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)

CALL OutputVertErrors(Mesh,LO_Mesh,&
  ext="erreyyv",VarName="EyyV",&
  ExactFunction=EXACT_Eyy,LOVar=EddingtonyyV(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)

CALL OutputVertErrors(Mesh,LO_Mesh,&
  ext="errexyv",VarName="ExyV",&
  ExactFunction=EXACT_Exy,LOVar=EddingtonxyV(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)

CALL OutputVertErrors(Mesh,LO_Mesh,&
  ext="errkxxv",VarName="KxxV",&
  ExactFunction=EXACT_Kxx,LOVar=KxxV(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)

CALL OutputVertErrors(Mesh,LO_Mesh,&
  ext="errkyyv",VarName="KyyV",&
  ExactFunction=EXACT_Kyy,LOVar=KyyV(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)

CALL OutputVertErrors(Mesh,LO_Mesh,&
  ext="errkxyv",VarName="KxyV",&
  ExactFunction=EXACT_Kxy,LOVar=KxyV(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)

CALL OutputVertErrors(Mesh,LO_Mesh,&
  ext="errphiv",VarName="PhiV",&
  ExactFunction=EXACT_Phi,HIVar=ScalarFluxV(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)

IF( OutputSnResults )THEN
 CALL OutputVertErrors(Mesh,LO_Mesh,&
  ext="errsnexxv",VarName="SnExxV",&
  ExactFunction=EXACT_SnExx,LOVar=EddingtonxxV(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)

 CALL OutputVertErrors(Mesh,LO_Mesh,&
  ext="errsneyyv",VarName="SnEyyV",&
  ExactFunction=EXACT_SnEyy,LOVar=EddingtonyyV(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)

 CALL OutputVertErrors(Mesh,LO_Mesh,&
  ext="errsnexyv",VarName="SnExyV",&
  ExactFunction=EXACT_SnExy,LOVar=EddingtonxyV(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)

 CALL OutputVertErrors(Mesh,LO_Mesh,&
  ext="errsnkxxv",VarName="SnKxxV",&
  ExactFunction=EXACT_SnKxx,LOVar=KxxV(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)

 CALL OutputVertErrors(Mesh,LO_Mesh,&
  ext="errsnkyyv",VarName="SnKyyV",&
  ExactFunction=EXACT_SnKyy,LOVar=KyyV(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)

 CALL OutputVertErrors(Mesh,LO_Mesh,&
  ext="errsnkxyv",VarName="SnKxyV",&
  ExactFunction=EXACT_SnKxy,LOVar=KxyV(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)

 CALL OutputVertErrors(Mesh,LO_Mesh,&
  ext="errsnphiv",VarName="SnPhiV",&
  ExactFunction=EXACT_SnPhi,HIVar=ScalarFluxV(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)

END IF


!face normal errors
IF( ASSOCIATED(LO_CurrentFN) .AND. ASSOCIATED(CurrentFN) )THEN
CALL OutputFaceNormalErrors(Mesh,LO_Mesh,&
  ext="errjfn",VarName="JFN",&
  ExactFunction1=EXACT_Jx,ExactFunction2=EXACT_Jy,&
  HIVar=CurrentFN(1,:),&
  LOVar=LO_CurrentFN(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)

IF( OutputSnResults )THEN
 CALL OutputFaceNormalErrors(Mesh,LO_Mesh,&
  ext="errsnjfn",VarName="SnJFN",&
  ExactFunction1=EXACT_SnJx,ExactFunction2=EXACT_SnJy,&
  HIVar=CurrentFN(1,:),&
  LOVar=LO_CurrentFN(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)
END IF
END IF


!face psi errors
CALL OutputFaceAFErrors(Mesh,&
  ext="errpsif",VarName="PsiF",&
  ExactFunction=EXACT_Psi,HIVar=AngularFluxF(1,:,:),&
  Directions=Directions,&
  gmvUnit=UnitAF,OutputFileBase=OutputFileBase)


!face errors
CALL OutputFaceErrors(Mesh,LO_Mesh,&
  ext="errexxf",VarName="ExxF",&
  ExactFunction=EXACT_Exx,LOVar=EddingtonxxF(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)

CALL OutputFaceErrors(Mesh,LO_Mesh,&
  ext="erreyyf",VarName="EyyF",&
  ExactFunction=EXACT_Eyy,LOVar=EddingtonyyF(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)

CALL OutputFaceErrors(Mesh,LO_Mesh,&
  ext="errexyf",VarName="ExyF",&
  ExactFunction=EXACT_Exy,LOVar=EddingtonxyF(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)

CALL OutputFaceErrors(Mesh,LO_Mesh,&
  ext="errkxxf",VarName="KxxF",&
  ExactFunction=EXACT_Kxx,LOVar=KxxF(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)

CALL OutputFaceErrors(Mesh,LO_Mesh,&
  ext="errkyyf",VarName="KyyF",&
  ExactFunction=EXACT_Kyy,LOVar=KyyF(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)

CALL OutputFaceErrors(Mesh,LO_Mesh,&
  ext="errkxyf",VarName="KxyF",&
  ExactFunction=EXACT_Kxy,LOVar=KxyF(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)

IF( ASSOCIATED(ScalarFluxF) .AND. ASSOCIATED(LO_ScalarFluxF) )THEN
CALL OutputFaceErrors(Mesh,LO_Mesh,&
  ext="errphif",VarName="PhiF",&
  ExactFunction=EXACT_Phi,HIVar=ScalarFluxF(1,:),&
  LOVar=LO_ScalarFluxF(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)
IF( OutputSnResults )THEN
 CALL OutputFaceErrors(Mesh,LO_Mesh,&
  ext="errsnphif",VarName="SnPhiF",&
  ExactFunction=EXACT_SnPhi,HIVar=ScalarFluxF(1,:),&
  LOVar=LO_ScalarFluxF(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)

END IF
END IF

IF( OutputSnResults )THEN

 CALL OutputFaceErrors(Mesh,LO_Mesh,&
  ext="errsnexxf",VarName="SnExxF",&
  ExactFunction=EXACT_SnExx,LOVar=EddingtonxxF(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)

 CALL OutputFaceErrors(Mesh,LO_Mesh,&
  ext="errsneyyf",VarName="SnEyyF",&
  ExactFunction=EXACT_SnEyy,LOVar=EddingtonyyF(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)

 CALL OutputFaceErrors(Mesh,LO_Mesh,&
  ext="errsnexyf",VarName="SnExyF",&
  ExactFunction=EXACT_SnExy,LOVar=EddingtonxyF(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)

 CALL OutputFaceErrors(Mesh,LO_Mesh,&
  ext="errsnkyyf",VarName="SnKyyF",&
  ExactFunction=EXACT_SnKyy,LOVar=KyyF(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)

 CALL OutputFaceErrors(Mesh,LO_Mesh,&
  ext="errsnkxyf",VarName="SnKxyF",&
  ExactFunction=EXACT_SnKxy,LOVar=KxyF(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)

 CALL OutputFaceErrors(Mesh,LO_Mesh,&
  ext="errsnkxxf",VarName="SnKxxF",&
  ExactFunction=EXACT_SnKxx,LOVar=KxxF(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)

END IF


!cell psi errors
CALL OutputCellAFErrors(Mesh,&
  ext="errpsic",VarName="PsiC",&
  ExactFunction=EXACT_Psi,HIVar=AngularFluxC(1,:,:),&
  Directions=Directions,&
  gmvUnit=UnitAF,OutputFileBase=OutputFileBase)

!cell errors
CALL OutputCellErrors(Mesh,LO_Mesh,&
  ext="errexxc",VarName="ExxC",&
  ExactFunction=EXACT_Exx,LOVar=EddingtonxxC(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)

CALL OutputCellErrors(Mesh,LO_Mesh,&
  ext="erreyyc",VarName="EyyC",&
  ExactFunction=EXACT_Eyy,LOVar=EddingtonyyC(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)

CALL OutputCellErrors(Mesh,LO_Mesh,&
  ext="errexyc",VarName="ExyC",&
  ExactFunction=EXACT_Exy,LOVar=EddingtonxyC(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)

CALL OutputCellErrors(Mesh,LO_Mesh,&
  ext="errkxxc",VarName="KxxC",&
  ExactFunction=EXACT_Kxx,LOVar=KxxC(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)

CALL OutputCellErrors(Mesh,LO_Mesh,&
  ext="errkyyc",VarName="KyyC",&
  ExactFunction=EXACT_Kyy,LOVar=KyyC(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)

CALL OutputCellErrors(Mesh,LO_Mesh,&
  ext="errkxyc",VarName="KxyC",&
  ExactFunction=EXACT_Kxy,LOVar=KxyC(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)

IF( ASSOCIATED(ScalarFluxC) .AND. ASSOCIATED(LO_ScalarFluxC) )THEN
CALL OutputCellErrors(Mesh,LO_Mesh,&
  ext="errphic",VarName="PhiC",&
  ExactFunction=EXACT_Phi,HIVar=ScalarFluxC(1,:),&
  LOVar=LO_ScalarFluxC(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)
IF( OutputSnResults )THEN
 CALL OutputCellErrors(Mesh,LO_Mesh,&
  ext="errsnphic",VarName="SnPhiC",&
  ExactFunction=EXACT_SnPhi,HIVar=ScalarFluxC(1,:),&
  LOVar=LO_ScalarFluxC(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)
END IF
END IF

IF( OutputSnResults )THEN

 CALL OutputCellErrors(Mesh,LO_Mesh,&
  ext="errsnexxc",VarName="SnExxC",&
  ExactFunction=EXACT_SnExx,LOVar=EddingtonxxC(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)

 CALL OutputCellErrors(Mesh,LO_Mesh,&
  ext="errsneyyc",VarName="SnEyyC",&
  ExactFunction=EXACT_SnEyy,LOVar=EddingtonyyC(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)

 CALL OutputCellErrors(Mesh,LO_Mesh,&
  ext="errsnexyc",VarName="SnExyC",&
  ExactFunction=EXACT_SnExy,LOVar=EddingtonxyC(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)

 CALL OutputCellErrors(Mesh,LO_Mesh,&
  ext="errsnkxxc",VarName="SnKxxC",&
  ExactFunction=EXACT_SnKxx,LOVar=KxxC(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)

 CALL OutputCellErrors(Mesh,LO_Mesh,&
  ext="errsnkyyc",VarName="SnKyyC",&
  ExactFunction=EXACT_SnKyy,LOVar=KyyC(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)

 CALL OutputCellErrors(Mesh,LO_Mesh,&
  ext="errsnkxyc",VarName="SnKxyC",&
  ExactFunction=EXACT_SnKxy,LOVar=KxyC(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)

END IF


!0th moment
CALL OutputVertValues(Mesh,"ExactMom0QextV",EXACT_Mom0Qext,Unit)
CALL OutputCellValues(Mesh,"ExactMom0QextC",EXACT_Mom0Qext,Unit)

!1st x moment
CALL OutputVertValues(Mesh,"ExactMom1xQextV",EXACT_Mom1xQext,Unit)
CALL OutputCellValues(Mesh,"ExactMom1xQextC",EXACT_Mom1xQext,Unit)

!1st y moment
CALL OutputVertValues(Mesh,"ExactMom1yQextV",EXACT_Mom1yQext,Unit)
CALL OutputCellValues(Mesh,"ExactMom1yQextC",EXACT_Mom1yQext,Unit)


IF( OutputSnResults )THEN
 CALL OutputVertValues(Mesh,"ExactSnMom0QextV",EXACT_SnMom0Qext,Unit)
 CALL OutputCellValues(Mesh,"ExactSnMom0QextC",EXACT_SnMom0Qext,Unit)
 CALL OutputVertValues(Mesh,"ExactSnMom1xQextV",EXACT_SnMom1xQext,Unit)
 CALL OutputCellValues(Mesh,"ExactSnMom1xQextC",EXACT_SnMom1xQext,Unit)
 CALL OutputVertValues(Mesh,"ExactSnMom1yQextV",EXACT_SnMom1yQext,Unit)
 CALL OutputCellValues(Mesh,"ExactSnMom1yQextC",EXACT_SnMom1yQext,Unit)
END IF

CLOSE(UnitAF)

!!--end--
END SUBROUTINE



SUBROUTINE gmv_LOQDAnalyticTestOutput(Mesh,LO_Mesh,&
 ScalarFluxF,ScalarFluxC,&
  LO_ScalarFluxF,LO_ScalarFluxC,&
  EddingtonxxV,EddingtonyyV,EddingtonxyV,&
  EddingtonxxF,EddingtonyyF,EddingtonxyF,&
  EddingtonxxC,EddingtonyyC,EddingtonxyC,&
  CurrentFN,LO_CurrentFN,&
  OutputFileBase,Unit)

TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
TYPE(TYPE_Mesh),INTENT(IN) :: LO_Mesh
CHARACTER(*),INTENT(IN) :: OutputFileBase
REAL(KIND_ScalarFlux),POINTER :: ScalarFluxF(:,:)
REAL(KIND_ScalarFlux),POINTER :: ScalarFluxC(:,:)
REAL(KIND_ScalarFlux),POINTER :: LO_ScalarFluxF(:,:)
REAL(KIND_ScalarFlux),POINTER :: LO_ScalarFluxC(:,:)
REAL(KIND_Eddington),POINTER :: EddingtonxxV(:,:),EddingtonyyV(:,:),EddingtonxyV(:,:)
REAL(KIND_Eddington),POINTER :: EddingtonxxF(:,:),EddingtonyyF(:,:),EddingtonxyF(:,:)
REAL(KIND_Eddington),POINTER :: EddingtonxxC(:,:),EddingtonyyC(:,:),EddingtonxyC(:,:)
REAL(KIND_Current),POINTER :: CurrentFN(:,:),LO_CurrentFN(:,:)
INTEGER,INTENT(IN) :: Unit
!!--begin--

CALL OutputVertValues(Mesh,"ExactPhiV",EXACT_LOQD_Phi,Unit)
CALL OutputVertValues(Mesh,"ExactJxV",EXACT_LOQD_Jx,Unit)
CALL OutputVertValues(Mesh,"ExactJyV",EXACT_LOQD_Jy,Unit)
CALL OutputVertValues(Mesh,"ExactExxV",EXACT_LOQD_Exx,Unit)
CALL OutputVertValues(Mesh,"ExactEyyV",EXACT_LOQD_Eyy,Unit)
CALL OutputVertValues(Mesh,"ExactExyV",EXACT_LOQD_Exy,Unit)
CALL OutputCellValues(Mesh,"ExactPhiC",EXACT_LOQD_Phi,Unit)
CALL OutputCellValues(Mesh,"ExactJxC",EXACT_LOQD_Jx,Unit)
CALL OutputCellValues(Mesh,"ExactJyC",EXACT_LOQD_Jy,Unit)
CALL OutputCellValues(Mesh,"ExactExxC",EXACT_LOQD_Exx,Unit)
CALL OutputCellValues(Mesh,"ExactEyyC",EXACT_LOQD_Eyy,Unit)
CALL OutputCellValues(Mesh,"ExactExyC",EXACT_LOQD_Exy,Unit)

!vertex errors
CALL OutputVertErrors(Mesh,LO_Mesh,&
  ext="errexxv",VarName="ExxV",&
  ExactFunction=EXACT_LOQD_Exx,LOVar=EddingtonxxV(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)

CALL OutputVertErrors(Mesh,LO_Mesh,&
  ext="erreyyv",VarName="EyyV",&
  ExactFunction=EXACT_LOQD_Eyy,LOVar=EddingtonyyV(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)

CALL OutputVertErrors(Mesh,LO_Mesh,&
  ext="errexyv",VarName="ExyV",&
  ExactFunction=EXACT_LOQD_Exy,LOVar=EddingtonxyV(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)

!face normal errors
IF( ASSOCIATED(LO_CurrentFN) .AND. ASSOCIATED(CurrentFN) )THEN
CALL OutputFaceNormalErrors(Mesh,LO_Mesh,&
  ext="errjfn",VarName="JFN",&
  ExactFunction1=EXACT_LOQD_Jx,ExactFunction2=EXACT_LOQD_Jy,&
  HIVar=CurrentFN(1,:),&
  LOVar=LO_CurrentFN(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)
END IF



!face errors
CALL OutputFaceErrors(Mesh,LO_Mesh,&
  ext="errexxf",VarName="ExxF",&
  ExactFunction=EXACT_LOQD_Exx,LOVar=EddingtonxxF(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)

CALL OutputFaceErrors(Mesh,LO_Mesh,&
  ext="erreyyf",VarName="EyyF",&
  ExactFunction=EXACT_LOQD_Eyy,LOVar=EddingtonyyF(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)

CALL OutputFaceErrors(Mesh,LO_Mesh,&
  ext="errexyf",VarName="ExyF",&
  ExactFunction=EXACT_LOQD_Exy,LOVar=EddingtonxyF(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)


IF( ASSOCIATED(ScalarFluxF) .AND. ASSOCIATED(LO_ScalarFluxF) )THEN
CALL OutputFaceErrors(Mesh,LO_Mesh,&
  ext="errphif",VarName="PhiF",&
  ExactFunction=EXACT_LOQD_Phi,HIVar=ScalarFluxF(1,:),&
  LOVar=LO_ScalarFluxF(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)
END IF

!cell errors
CALL OutputCellErrors(Mesh,LO_Mesh,&
  ext="errexxc",VarName="ExxC",&
  ExactFunction=EXACT_LOQD_Exx,LOVar=EddingtonxxC(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)

CALL OutputCellErrors(Mesh,LO_Mesh,&
  ext="erreyyc",VarName="EyyC",&
  ExactFunction=EXACT_LOQD_Eyy,LOVar=EddingtonyyC(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)

CALL OutputCellErrors(Mesh,LO_Mesh,&
  ext="errexyc",VarName="ExyC",&
  ExactFunction=EXACT_LOQD_Exy,LOVar=EddingtonxyC(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)

IF( ASSOCIATED(ScalarFluxC) .AND. ASSOCIATED(LO_ScalarFluxC) )THEN
CALL OutputCellErrors(Mesh,LO_Mesh,&
  ext="errphic",VarName="PhiC",&
  ExactFunction=EXACT_LOQD_Phi,HIVar=ScalarFluxC(1,:),&
  LOVar=LO_ScalarFluxC(1,:),&
  gmvUnit=Unit,OutputFileBase=OutputFileBase)
END IF


!0th moment
CALL OutputVertValues(Mesh,"ExactMom0QextV",EXACT_LOQD_Q,Unit)
CALL OutputCellValues(Mesh,"ExactMom0QextC",EXACT_LOQD_Q,Unit)

!!--end--
END SUBROUTINE

SUBROUTINE OutputVertValues(Mesh,VarName,ExactFunction,gmvUnit)

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
CHARACTER(*)   ,INTENT(IN) :: VarName
INTERFACE
 FUNCTION ExactFunction(x,y)
  USE KND_MoCshort                   !!((03-A-KND_MoCshort.f90))
  REAL(KIND_MCS),INTENT(IN) :: x,y
  REAL(KIND_MCS) :: ExactFunction
 END FUNCTION
END INTERFACE

!!#### OPTIONAL INPUT
INTEGER,OPTIONAL,INTENT(IN) :: gmvUnit

!!#### LOCAL VARIABLES
REAL(KIND_MCS),ALLOCATABLE :: dummy(:)
REAL(KIND_MCS) :: x,y,r(2)
INTEGER :: k,Nk

!!--begin--

Nk = NUM_Verts(Mesh)
ALLOCATE( dummy(Nk) )

DO k=1,Nk
 r = Vert(Mesh,k)
 x = r(1); y=r(2)
 dummy(k) = ExactFunction(x,y)
END DO
CALL gmvSet_NodeData(gmvUnit,Mesh,VarName,dummy)

DEALLOCATE(dummy)

!!--end--
END SUBROUTINE



SUBROUTINE OutputVertAFValues(Mesh,VarName,ExactFunction,&
  Directions,gmvUnit,mtarget)

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
CHARACTER(*)   ,INTENT(IN) :: VarName
INTERFACE
 FUNCTION ExactFunction(x,y,Omega)
  USE KND_MoCshort                   !!((03-A-KND_MoCshort.f90))
  REAL(KIND_MCS),INTENT(IN) :: x,y,Omega(2)
  REAL(KIND_MCS) :: ExactFunction
 END FUNCTION
END INTERFACE
REAL(KIND_MCS),INTENT(IN) :: Directions(:,:)

!!#### OPTIONAL INPUT
INTEGER,OPTIONAL,INTENT(IN) :: gmvUnit
INTEGER,OPTIONAL,INTENT(IN) :: mtarget

!!#### LOCAL VARIABLES
REAL(KIND_MCS),ALLOCATABLE :: dummy(:)
REAL(KIND_MCS) :: x,y,r(2)
INTEGER :: k,Nk,m,Nm
REAL(KIND_MCS) :: Omega(2)

!!--begin--

Nk = NUM_Verts(Mesh)
Nm = SIZE(Directions,2)
ALLOCATE( dummy(Nk) )

DO m=1,Nm
 IF( PRESENT(mtarget) )THEN
  IF( m/=mtarget )THEN
   CYCLE
  END IF
 END IF
 Omega=Directions(1:2,m)
 DO k=1,Nk
  r = Vert(Mesh,k)
  x = r(1); y=r(2)
  dummy(k) = ExactFunction(x,y,Omega)
 END DO
 CALL gmvSet_NodeData(gmvUnit,Mesh,VarName//"("//TRIM(STR(m))//")",dummy)
END DO

DEALLOCATE(dummy)

!!--end--
END SUBROUTINE


SUBROUTINE OutputCellValues(Mesh,VarName,ExactFunction,gmvUnit)

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
CHARACTER(*)   ,INTENT(IN) :: VarName
INTERFACE
 FUNCTION ExactFunction(x,y)
  USE KND_MoCshort                   !!((03-A-KND_MoCshort.f90))
  REAL(KIND_MCS),INTENT(IN) :: x,y
  REAL(KIND_MCS) :: ExactFunction
 END FUNCTION
END INTERFACE

!!#### OPTIONAL INPUT
INTEGER,OPTIONAL,INTENT(IN) :: gmvUnit

!!#### LOCAL VARIABLES
REAL(KIND_MCS),ALLOCATABLE :: dummy(:)
INTEGER :: i,Ni

!!--begin--

Ni=NUM_Cells(Mesh)
ALLOCATE( dummy(Ni) )

DO i=1,Ni
 dummy(i) = CellAverage_F(Mesh,i,ExactFunction)
END DO
CALL gmvSet_CellData(gmvUnit,Mesh,VarName,dummy)

DEALLOCATE(dummy)

!!--end--
END SUBROUTINE



SUBROUTINE OutputCellAFValues(Mesh,VarName,ExactFunction,&
  Directions,gmvUnit)

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
CHARACTER(*)   ,INTENT(IN) :: VarName
INTERFACE
 FUNCTION ExactFunction(x,y,Omega)
  USE KND_MoCshort                   !!((03-A-KND_MoCshort.f90))
  REAL(KIND_MCS),INTENT(IN) :: x,y,Omega(2)
  REAL(KIND_MCS) :: ExactFunction
 END FUNCTION
END INTERFACE
REAL(KIND_MCS),INTENT(IN) :: Directions(:,:)

!!#### OPTIONAL INPUT
INTEGER,OPTIONAL,INTENT(IN) :: gmvUnit

!!#### LOCAL VARIABLES
REAL(KIND_MCS),ALLOCATABLE :: dummy(:)
INTEGER :: i,Ni,m,Nm
REAL(KIND_MCS) :: Omega(2)

!!--begin--

Nm=SIZE(Directions,2)
Ni=NUM_Cells(Mesh)
ALLOCATE( dummy(Ni) )

DO m=1,Nm
 Omega=Directions(1:2,m)
 DO i=1,Ni
  dummy(i) = CellAverage_F(Mesh,i,ExactLocal)
 END DO
 CALL gmvSet_CellData(gmvUnit,Mesh,VarName,dummy)
END DO

DEALLOCATE(dummy)

!!--end--
CONTAINS

FUNCTION ExactLocal(x,y)
REAL(KIND_MCS),INTENT(IN) :: x,y
REAL(KIND_MCS) :: ExactLocal
!!--begin--
ExactLocal = ExactFunction(x,y,Omega)
!!--end--
END FUNCTION

END SUBROUTINE



SUBROUTINE OutputVertAFErrors(Mesh,&
  ext,VarName,ExactFunction,HIVar,Directions,&
  gmvUnit,OutputFileBase)

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
CHARACTER(*)   ,INTENT(IN) :: ext,VarName
INTERFACE
 FUNCTION ExactFunction(x,y,Omega)
  USE KND_MoCshort                   !!((03-A-KND_MoCshort.f90))
  REAL(KIND_MCS),INTENT(IN) :: x,y,Omega(2)
  REAL(KIND_MCS) :: ExactFunction
 END FUNCTION
END INTERFACE

!!#### OPTIONAL INPUT
REAL(KIND_MCS) ,INTENT(IN) :: HIVar(:,:),Directions(:,:)
INTEGER,OPTIONAL,INTENT(IN) :: gmvUnit
CHARACTER(*),OPTIONAL,INTENT(IN) :: OutputFileBase

!!#### LOCAL PARAMETERS
CHARACTER(*),PARAMETER :: FMTv1="(a32,a6,2a26)"
CHARACTER(*),PARAMETER :: FMTv2="(a32,i6,2Es26.13)"

!!#### LOCAL VARIABLES
REAL(KIND_MCS) :: cErr_HIVar
REAL(KIND_MCS) :: c_HIVar
TYPE(varying_string) :: path
INTEGER :: Unit_
REAL(KIND_MCS) :: Omega(2)
INTEGER :: m,Nm
CHARACTER(4) :: DirStr
!!--begin--

!1.b. high order vertex error norm calculations
Nm = SIZE(Directions,2)
DO m=1,Nm
 Omega = Directions(1:2,m)
 DirStr=TRIM(STR(m,"(i4.4)"))

 cErr_HIVar=0._KIND_MCS
 c_HIVar=0._KIND_MCS
 path = TRIM(OutputFileBase)//ext//&
   DirStr//"c" 
 Unit_=NewFile(STR(path))
 CALL OutputVerts_cErr(cErr_HIVar,c_HIVar,&
       Mesh,VarName//"("//DirStr//")",HIVar(:,m),LocalFunction,&
       gmvUnit=gmvUnit,indUnit=Unit_)
 CLOSE(Unit_)

 !1.c. output errors
 path = TRIM(OutputFileBase)//ext//&
   DirStr 
 Unit_=NewFile(STR(path))
 WRITE(Unit_,FMTv1)"FileName","m",&
   "cErr_"//VarName,"c_"//VarName
 WRITE(Unit_,FMTv2)TRIM(OutputFileBase),m,cErr_HIVar,c_HIVar
 CLOSE(Unit_)

END DO

!!--end--
CONTAINS

FUNCTION LocalFunction(x,y)
REAL(KIND_MCS),INTENT(IN) :: x,y
REAL(KIND_MCS) :: LocalFunction
!!--begin--
LocalFunction = ExactFunction(x,y,Omega)
!!--end--
END FUNCTION

END SUBROUTINE


SUBROUTINE OutputVertErrors(Mesh,LO_Mesh,&
  ext,VarName,ExactFunction,HIVar,LOVar,ExactVars,&
  gmvUnit,OutputFileBase)

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
TYPE(TYPE_Mesh),INTENT(IN) :: LO_Mesh
CHARACTER(*)   ,INTENT(IN) :: ext,VarName
INTERFACE
 FUNCTION ExactFunction(x,y)
  USE KND_MoCshort                   !!((03-A-KND_MoCshort.f90))
  REAL(KIND_MCS),INTENT(IN) :: x,y
  REAL(KIND_MCS) :: ExactFunction
 END FUNCTION
END INTERFACE

!!#### OPTIONAL INPUT
REAL(KIND_MCS) ,INTENT(IN),OPTIONAL :: HIVar(:),LOVar(:)
REAL(KIND_MCS),POINTER,OPTIONAL :: ExactVars(:)
INTEGER,OPTIONAL,INTENT(IN) :: gmvUnit
CHARACTER(*),OPTIONAL,INTENT(IN) :: OutputFileBase

!!#### LOCAL PARAMETERS
CHARACTER(*),PARAMETER :: FMTv1="(a32,4a26)"
CHARACTER(*),PARAMETER :: FMTv2="(a32,4Es26.13)"

!!#### LOCAL VARIABLES
REAL(KIND_MCS) :: cErr_LOVar,cErr_HIVar
REAL(KIND_MCS) :: c_LOVar,c_HIVar
TYPE(varying_string) :: path
INTEGER :: Unit_

!!--begin--

!1.a. low order vertex error norm calculations
cErr_LOVar=0._KIND_MCS
c_LOVar=0._KIND_MCS
IF( PRESENT(LOVar) )THEN
 path = TRIM(OutputFileBase)//ext//"LOc" 
 Unit_=NewFile(STR(path))
 CALL OutputVerts_cErr(cErr_LOVar,c_LOVar,&
   LO_Mesh,"LO_"//VarName,LOVar,ExactFunction,ExactVars,gmvUnit,Unit_)
 CLOSE(Unit_)
END IF

!1.b. high order vertex error norm calculations
cErr_HIVar=0._KIND_MCS
c_HIVar=0._KIND_MCS
IF( PRESENT(HIVar) )THEN
 path = TRIM(OutputFileBase)//ext//"HIc" 
 Unit_=NewFile(STR(path))
 CALL OutputVerts_cErr(cErr_HIVar,c_HIVar,&
      Mesh,"HI_"//VarName,HIVar,ExactFunction,ExactVars,gmvUnit,Unit_)
 CLOSE(Unit_)
END IF

!1.c. output errors
path = TRIM(OutputFileBase)//ext 
Unit_=NewFile(STR(path))
WRITE(Unit_,FMTv1)"FileName",&
  "cErr_"//"HI_"//VarName,"c_"//"HI_"//VarName,&
  "cErr_"//"LO_"//VarName,"c_"//"LO_"//VarName
WRITE(Unit_,FMTv2)TRIM(OutputFileBase),cErr_HIVar,c_HIVar,cErr_LOVar,c_LOVar
CLOSE(Unit_)

!!--end--
END SUBROUTINE


SUBROUTINE OutputVerts_cErr(cErr,c,&
  Mesh,VarName,Var,ExactFunction,ExactVars,gmvUnit,indUnit)
REAL(KIND_MCS),INTENT(OUT) :: cErr,c
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
CHARACTER(*),INTENT(IN) :: VarName
REAL(KIND_MCS),INTENT(IN) :: Var(:)
INTERFACE
 FUNCTION ExactFunction(x,y)
  USE KND_MoCshort                   !!((03-A-KND_MoCshort.f90))
  REAL(KIND_MCS),INTENT(IN) :: x,y
  REAL(KIND_MCS) :: ExactFunction
 END FUNCTION
END INTERFACE
REAL(KIND_MCS),POINTER,OPTIONAL :: ExactVars(:)
INTEGER,OPTIONAL,INTENT(IN) :: gmvUnit,indUnit

!!#### LOCAL VARIABLES
REAL(KIND_MCS),ALLOCATABLE :: dummy(:)
INTEGER :: k,Nk
REAL(KIND_MCS) :: V(2),x,y,b,ExactVar

!!--begin--
cErr = TINY(0._KIND_MCS)
c    = TINY(0._KIND_MCS)
Nk = NUM_Verts(Mesh)
ALLOCATE(dummy(Nk))
IF( PRESENT(indUnit) )THEN
 WRITE(indUnit,"(a10,3a26)")"k","ExactVar","Var(k)","cErr"
END IF
DO k=1,Nk
 IF( PRESENT(ExactVars) )THEN
  ExactVar = ExactVars(k)
 ELSE
  V = Vert(Mesh,k)
  x = V(1); y = V(2)
  ExactVar = ExactFunction(x,y)
 END IF
 b = ABS( ExactVar-Var(k) )
 dummy(k) = b
 cErr = MAX(cErr,b)
 c = MAX(c,ABS(ExactVar))
 IF( PRESENT(indUnit) )THEN
  WRITE(indUnit,"(i10,3Es26.12)")k,ExactVar,Var(k),b
 END IF
END DO

IF( PRESENT(gmvUnit) )THEN
 CALL gmvSet_NodeData(gmvUnit,Mesh,"cErr_"//VarName,dummy)
 IF( cErr>1.d-30 )THEN
  CALL gmvSet_NodeData(gmvUnit,Mesh,"frac_cErr_"//VarName,dummy/cErr)
 END IF
 IF( c>1.d-30 )THEN
  CALL gmvSet_NodeData(gmvUnit,Mesh,"rel_cErr_"//VarName,dummy/c)
 END IF
END IF

DEALLOCATE( dummy )

!!--end--
END SUBROUTINE


SUBROUTINE OutputFaces_cErr(cErr,c,&
  Mesh,VarName,Var,ExactFunction,ExactVars,gmvUnit,indUnit)
REAL(KIND_MCS),INTENT(OUT) :: cErr,c
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
CHARACTER(*),INTENT(IN) :: VarName
REAL(KIND_MCS),INTENT(IN) :: Var(:)
INTERFACE
 FUNCTION ExactFunction(x,y)
  USE KND_MoCshort                   !!((03-A-KND_MoCshort.f90))
  REAL(KIND_MCS),INTENT(IN) :: x,y
  REAL(KIND_MCS) :: ExactFunction
 END FUNCTION
END INTERFACE
REAL(KIND_MCS),POINTER,OPTIONAL :: ExactVars(:)
INTEGER,OPTIONAL,INTENT(IN) :: gmvUnit,indUnit

!!#### LOCAL VARIABLES
REAL(KIND_MCS),ALLOCATABLE :: dummy(:)
INTEGER :: j,Nj
REAL(KIND_MCS) :: b,ExactVar

!!--begin--

cErr = TINY(0._KIND_MCS)
c    = TINY(0._KIND_MCS)
Nj = NUM_Faces(Mesh)
ALLOCATE( dummy(Nj) )
IF( PRESENT(indUnit) )THEN
 WRITE(indUnit,"(a10,3a26)")"j","ExactVar","Var(j)","cErr"
END IF
DO j=1,Nj
 IF( PRESENT(ExactVars) )THEN
  ExactVar = ExactVars(j)
 ELSE
  ExactVar = FaceAverage_F(Mesh,j,ExactFunction)
 END IF
 b = ABS( ExactVar-Var(j) )
 dummy(j) = b
 cErr = MAX(cErr,b)
 c = MAX(c,ABS(ExactVar))
 IF( PRESENT(indUnit) )THEN
  WRITE(indUnit,"(i10,3Es26.12)")j,ExactVar,Var(j),b
 END IF
END DO

IF( PRESENT(gmvUnit) )THEN
 !CALL gmvSet_FaceData(gmvUnit,Mesh,"cErr_"//VarName,dummy)
 !CALL gmvSet_FaceData(gmvUnit,Mesh,"frac_cErr_"//VarName,dummy/cErr)
 !CALL gmvSet_FaceData(gmvUnit,Mesh,"rel_cErr_"//VarName,dummy/c)
END IF
DEALLOCATE(dummy)

!!--end--
END SUBROUTINE


SUBROUTINE OutputFaceNormals_cErr(cErr,c,&
  Mesh,VarName,Var,ExactFunction1,ExactFunction2,ExactVars,&
  gmvUnit,indUnit)
REAL(KIND_MCS),INTENT(OUT) :: cErr,c
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
CHARACTER(*),INTENT(IN) :: VarName
REAL(KIND_MCS),INTENT(IN) :: Var(:)
INTERFACE
 FUNCTION ExactFunction1(x,y)
  USE KND_MoCshort                   !!((03-A-KND_MoCshort.f90))
  REAL(KIND_MCS),INTENT(IN) :: x,y
  REAL(KIND_MCS) :: ExactFunction1
 END FUNCTION
END INTERFACE
INTERFACE
 FUNCTION ExactFunction2(x,y)
  USE KND_MoCshort                   !!((03-A-KND_MoCshort.f90))
  REAL(KIND_MCS),INTENT(IN) :: x,y
  REAL(KIND_MCS) :: ExactFunction2
 END FUNCTION
END INTERFACE
REAL(KIND_MCS),POINTER,OPTIONAL :: ExactVars(:,:)
INTEGER,OPTIONAL,INTENT(IN) :: gmvUnit,indUnit

!!#### LOCAL VARIABLES
REAL(KIND_MCS),ALLOCATABLE :: dummy(:)
INTEGER :: j,Nj
REAL(KIND_MCS) :: d,b,ExactVar(2),FN(2)

!!--begin--

cErr = TINY(0._KIND_MCS)
c    = TINY(0._KIND_MCS)
Nj = NUM_Faces(Mesh)
ALLOCATE( dummy(Nj) )
IF( PRESENT(indUnit) )THEN
 WRITE(indUnit,"(a10,3a26)")"j","ExactVar","Var(j)","cErr"
END IF
DO j=1,Nj
 IF( PRESENT(ExactVars) )THEN
  ExactVar(1) = ExactVars(1,j)
  ExactVar(2) = ExactVars(2,j)
 ELSE
  ExactVar(1) = FaceAverage_F(Mesh,j,ExactFunction1)
  ExactVar(2) = FaceAverage_F(Mesh,j,ExactFunction2)
 END IF
 FN = FaceNormal(Mesh,j)
 d = DOT_PRODUCT(ExactVar,FN)
 b = ABS( d-Var(j) )
 dummy(j) = b
 cErr = MAX(cErr,b)
 c = MAX(c,ABS(d))
 IF( PRESENT(indUnit) )THEN
  WRITE(indUnit,"(i10,3Es26.12)")j,d,Var(j),b
 END IF
END DO

IF( PRESENT(gmvUnit) )THEN
 !CALL gmvSet_FaceData(gmvUnit,Mesh,"cErr_"//VarName,dummy)
 !CALL gmvSet_FaceData(gmvUnit,Mesh,"frac_cErr_"//VarName,dummy/cErr)
 !CALL gmvSet_FaceData(gmvUnit,Mesh,"rel_cErr_"//VarName,dummy/c)
END IF
DEALLOCATE(dummy)

!!--end--
END SUBROUTINE



SUBROUTINE OutputFaceNormals_L2Err(L2Err,L2,&
  Mesh,VarName,Var,ExactFunction1,ExactFunction2,ExactVars,&
  gmvUnit,indUnit)
REAL(KIND_MCS),INTENT(OUT) :: L2Err,L2
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
CHARACTER(*),INTENT(IN) :: VarName
REAL(KIND_MCS),INTENT(IN) :: Var(:)
INTERFACE
 FUNCTION ExactFunction1(x,y)
  USE KND_MoCshort                   !!((03-A-KND_MoCshort.f90))
  REAL(KIND_MCS),INTENT(IN) :: x,y
  REAL(KIND_MCS) :: ExactFunction1
 END FUNCTION
END INTERFACE
INTERFACE
 FUNCTION ExactFunction2(x,y)
  USE KND_MoCshort                   !!((03-A-KND_MoCshort.f90))
  REAL(KIND_MCS),INTENT(IN) :: x,y
  REAL(KIND_MCS) :: ExactFunction2
 END FUNCTION
END INTERFACE
REAL(KIND_MCS),POINTER,OPTIONAL :: ExactVars(:,:)
INTEGER,OPTIONAL,INTENT(IN) :: gmvUnit,indUnit

!!#### LOCAL VARIABLES
!REAL(KIND_MCS),ALLOCATABLE :: dummy(:)
INTEGER :: j,Nj
REAL(KIND_MCS) :: d,l2e,l2vme,ExactVar(2),FN(2),tot_face_area

!!--begin--

L2Err = TINY(0._KIND_MCS)
L2    = TINY(0._KIND_MCS)
Nj = NUM_Faces(Mesh)
!ALLOCATE( dummy(Nj) )
IF( PRESENT(indUnit) )THEN
 WRITE(indUnit,"(a10,4a26)")"j","ExactVar","Var","L2(ExactVar)","L2(Var-ExactVar)"
END IF

tot_face_area=0.d0
DO j=1,Nj
 tot_face_area=tot_face_area+ABS(FaceArea(Mesh,j))
END DO


DO j=1,Nj
 IF( PRESENT(ExactVars) )THEN
  ExactVar(1) = ExactVars(1,j)
  ExactVar(2) = ExactVars(2,j)
 ELSE
  ExactVar(1) = FaceAverage_F(Mesh,j,ExactFunction1)
  ExactVar(2) = FaceAverage_F(Mesh,j,ExactFunction2)
 END IF
 FN = FaceNormal(Mesh,j)
 d = DOT_PRODUCT(ExactVar,FN)

 l2vme = ABS(FaceArea(Mesh,j)/tot_face_area)*( Var(j) - d )**2
 !dummy(j) = l2vme
 L2Err = L2Err+l2vme

 l2e = ABS(FaceArea(Mesh,j)/tot_face_area)*(d**2)
 L2 = L2+l2e

 IF( PRESENT(indUnit) )THEN
  WRITE(indUnit,"(i10,4Es26.12)")j,d,Var(j),l2e,l2vme
 END IF

END DO
L2Err = SQRT(L2Err)
L2 = SQRT(L2)

!IF( PRESENT(gmvUnit) )THEN
 !CALL gmvSet_FaceData(gmvUnit,Mesh,"L2Err_"//VarName,dummy)
 !CALL gmvSet_FaceData(gmvUnit,Mesh,"frac_L2Err_"//VarName,dummy/L2Err)
 !CALL gmvSet_FaceData(gmvUnit,Mesh,"rel_L2Err_"//VarName,dummy/L2)
!END IF
!DEALLOCATE(dummy)

!!--end--
END SUBROUTINE



SUBROUTINE OutputCells_cErr(cErr,c,&
  Mesh,VarName,Var,ExactFunction,ExactVars,gmvUnit,indUnit)
REAL(KIND_MCS),INTENT(OUT) :: cErr,c
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
CHARACTER(*),INTENT(IN) :: VarName
REAL(KIND_MCS),INTENT(IN) :: Var(:)
INTERFACE
 FUNCTION ExactFunction(x,y)
  USE KND_MoCshort                   !!((03-A-KND_MoCshort.f90))
  REAL(KIND_MCS),INTENT(IN) :: x,y
  REAL(KIND_MCS) :: ExactFunction
 END FUNCTION
END INTERFACE
REAL(KIND_MCS),POINTER,OPTIONAL :: ExactVars(:)
INTEGER,OPTIONAL,INTENT(IN) :: gmvUnit,indUnit

!!#### LOCAL VARIABLES
REAL(KIND_MCS),ALLOCATABLE :: dummy(:)
INTEGER :: i,Ni
REAL(KIND_MCS) :: b,ExactVar

!!--begin--

cErr = TINY(0._KIND_MCS)
c    = TINY(0._KIND_MCS)
Ni = NUM_Cells(Mesh)
ALLOCATE( dummy(Ni) )
IF( PRESENT(indUnit) )THEN
 WRITE(indUnit,"(a10,3a26)")"i","ExactVar","Var(i)","cErr"
END IF
DO i=1,Ni
 IF( PRESENT(ExactVars) )THEN
  ExactVar=ExactVars(i)
 ELSE
  ExactVar = CellAverage_F(Mesh,i,ExactFunction)
 END IF
 b = ABS(ExactVar-Var(i))
 dummy(i) = b
 cErr = MAX(cErr,b)
 c = MAX(c,ABS(ExactVar))
 IF( PRESENT(indUnit) )THEN
  WRITE(indUnit,"(i10,3Es26.12)")i,ExactVar,Var(i),b
 END IF
END DO
IF( PRESENT(gmvUnit) )THEN
 CALL gmvSet_CellData(gmvUnit,Mesh,"cErr_"//VarName,dummy)
 IF( cerr>1.d-30 )THEN
  CALL gmvSet_CellData(gmvUnit,Mesh,"frac_cErr_"//VarName,dummy/cErr)
 END IF
 IF( c>1.d-30 )THEN
  CALL gmvSet_CellData(gmvUnit,Mesh,"rel_cErr_"//VarName,dummy/c)
 END IF
END IF
DEALLOCATE(dummy)

!!--end--
END SUBROUTINE



SUBROUTINE OutputFaces_L2Err(L2Err,L2,&
  Mesh,VarName,Var,ExactFunction,ExactVars,gmvUnit,indUnit)
REAL(KIND_MCS),INTENT(OUT) :: L2Err,L2
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
CHARACTER(*),INTENT(IN) :: VarName
REAL(KIND_MCS),INTENT(IN) :: Var(:)
INTERFACE
 FUNCTION ExactFunction(x,y)
  USE KND_MoCshort                   !!((03-A-KND_MoCshort.f90))
  REAL(KIND_MCS),INTENT(IN) :: x,y
  REAL(KIND_MCS) :: ExactFunction
 END FUNCTION
END INTERFACE
REAL(KIND_MCS),POINTER,OPTIONAL :: ExactVars(:)
INTEGER,OPTIONAL,INTENT(IN) :: gmvUnit,indUnit

!!#### LOCAL VARIABLES
!REAL(KIND_MCS),ALLOCATABLE :: dummy(:)
INTEGER :: j,Nj
REAL(KIND_MCS) :: ExactVar,l2e,l2vme,tot_face_area

!!--begin--

L2Err = TINY(0._KIND_MCS)
L2    = TINY(0._KIND_MCS)
Nj = NUM_Faces(Mesh)
!ALLOCATE( dummy(Nj) )
IF( PRESENT(indUnit) )THEN
 WRITE(indUnit,"(a10,4a26)")"j","ExactVar","Var","L2(ExactVar)","L2(Var-ExactVar)"
END IF


tot_face_area=0.d0
DO j=1,Nj
 tot_face_area=tot_face_area+ABS(FaceArea(Mesh,j))
END DO


DO j=1,Nj
 IF( PRESENT(ExactVars) )THEN
  ExactVar = ExactVars(j)
 ELSE
  ExactVar = FaceAverage_F(Mesh,j,ExactFunction)
 END IF

 l2vme = ABS(FaceArea(Mesh,j)/tot_face_area)*( ExactVar-Var(j) )**2
 !dummy(j) = b
 L2Err = L2Err + l2vme

 l2e = ABS(FaceArea(Mesh,j)/tot_face_area)*( ExactVar )**2
 L2    = L2 + l2e
 IF( PRESENT(indUnit) )THEN
  WRITE(indUnit,"(i10,4Es26.12)")j,ExactVar,Var(j),l2e,l2vme
 END IF

END DO
L2Err = SQRT(L2Err)
L2    = SQRT(L2)

!don't have a face data output to gmv yet
! IF( PRESENT(gmvUnit) )THEN 
!  CALL gmvSet_FaceData(gmvUnit,Mesh,"L2Err_"//VarName,dummy)
! END IF
! DEALLOCATE(dummy)

!!--end--
END SUBROUTINE



SUBROUTINE OutputCells_L2Err(L2Err,L2,&
  Mesh,VarName,Var,ExactFunction,ExactVars,gmvUnit,indUnit)
REAL(KIND_MCS),INTENT(OUT) :: L2Err,L2
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
CHARACTER(*),INTENT(IN) :: VarName
REAL(KIND_MCS),INTENT(IN) :: Var(:)
INTERFACE
 FUNCTION ExactFunction(x,y)
  USE KND_MoCshort                   !!((03-A-KND_MoCshort.f90))
  REAL(KIND_MCS),INTENT(IN) :: x,y
  REAL(KIND_MCS) :: ExactFunction
 END FUNCTION
END INTERFACE
REAL(KIND_MCS),POINTER,OPTIONAL :: ExactVars(:)
INTEGER,OPTIONAL,INTENT(IN) :: gmvUnit,indUnit

!!#### LOCAL VARIABLES
REAL(KIND_MCS),ALLOCATABLE :: dummy(:)
INTEGER :: i,Ni
REAL(KIND_MCS) :: b,ExactVar

!!--begin--

L2Err = TINY(0._KIND_MCS)
L2    = TINY(0._KIND_MCS)
Ni = NUM_Cells(Mesh)
ALLOCATE( dummy(Ni) )
IF( PRESENT(indUnit) )THEN
 WRITE(indUnit,"(a10,3a26)")"i","ExactVar","Var(i)","L2Err"
END IF
DO i=1,Ni
 IF( PRESENT(ExactVars) )THEN
  ExactVar = ExactVars(i)
 ELSE
  ExactVar = CellAverage_F(Mesh,i,ExactFunction)
 END IF

 b = ABS(CellVolume(Mesh,i))*( ExactVar-Var(i) )**2
 dummy(i) = b
 L2Err = L2Err + b

 b = ABS(CellVolume(Mesh,i))*( ExactVar )**2
 L2 = L2 + b

 IF( PRESENT(indUnit) )THEN
  WRITE(indUnit,"(i10,3Es26.12)")i,ExactVar,Var(i),dummy(i)
 END IF

END DO
L2Err = SQRT(L2Err)
L2    = SQRT(L2)

IF( PRESENT(gmvUnit) )THEN
 CALL gmvSet_CellData(gmvUnit,Mesh,"L2Err_"//VarName,dummy)
 IF( L2Err>1.d-30 )THEN
  CALL gmvSet_CellData(gmvUnit,Mesh,"frac_L2Err_"//VarName,dummy/L2Err)
 END IF
 IF( L2>1.d-30 )THEN
  CALL gmvSet_CellData(gmvUnit,Mesh,"rel_L2Err_"//VarName,dummy/L2)
 END IF
END IF
DEALLOCATE(dummy)

!!--end--
END SUBROUTINE



SUBROUTINE OutputFaceErrors(Mesh,LO_Mesh,&
  ext,VarName,ExactFunction,HIVar,LOVar,ExactVars,&
  gmvUnit,OutputFileBase)

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
TYPE(TYPE_Mesh),INTENT(IN) :: LO_Mesh
CHARACTER(*)   ,INTENT(IN) :: ext,VarName
INTERFACE
 FUNCTION ExactFunction(x,y)
  USE KND_MoCshort                   !!((03-A-KND_MoCshort.f90))
  REAL(KIND_MCS),INTENT(IN) :: x,y
  REAL(KIND_MCS) :: ExactFunction
 END FUNCTION
END INTERFACE

!!#### OPTIONAL INPUT
REAL(KIND_MCS) ,INTENT(IN),OPTIONAL :: HIVar(:),LOVar(:)
REAL(KIND_MCS),POINTER,OPTIONAL :: ExactVars(:)
INTEGER,OPTIONAL,INTENT(IN) :: gmvUnit
CHARACTER(*),OPTIONAL,INTENT(IN) :: OutputFileBase

!!#### LOCAL PARAMETERS
CHARACTER(*),PARAMETER :: FMTv1="(a32,8a26)"
CHARACTER(*),PARAMETER :: FMTv2="(a32,8Es26.13)"

!!#### LOCAL VARIABLES
REAL(KIND_MCS) :: cErr_LOVar,cErr_HIVar
REAL(KIND_MCS) :: L2Err_LOVar,L2Err_HIVar
REAL(KIND_MCS) :: c_LOVar,c_HIVar
REAL(KIND_MCS) :: L2_LOVar,L2_HIVar
TYPE(varying_string) :: path
INTEGER :: Unit_

!!--begin--

!1.a. low order face error norm calculations
cErr_LOVar=0._KIND_MCS
c_LOVar=0._KIND_MCS
L2Err_LOVar=0._KIND_MCS
L2_LOVar=0._KIND_MCS
IF( PRESENT(LOVar) )THEN
 path = TRIM(OutputFileBase)//ext//"LOc" 
 Unit_=NewFile(STR(path))
 CALL OutputFaces_cErr(cErr_LOVar,c_LOVar,&
   LO_Mesh,"LO_"//VarName,LOVar,ExactFunction,ExactVars,gmvUnit,Unit_)
 CLOSE(Unit_)

 path = TRIM(OutputFileBase)//ext//"LOl2" 
 Unit_=NewFile(STR(path))
 CALL OutputFaces_L2Err(L2Err_LOVar,L2_LOVar,&
   LO_Mesh,"LO_"//VarName,LOVar,ExactFunction,ExactVars,gmvUnit,Unit_)
 CLOSE(Unit_)
END IF

!1.b. high order face error norm calculations
cErr_HIVar=0._KIND_MCS
c_HIVar=0._KIND_MCS
L2Err_HIVar=0._KIND_MCS
L2_HIVar=0._KIND_MCS
IF( PRESENT(HIVar) )THEN
 path = TRIM(OutputFileBase)//ext//"HIc" 
 Unit_=NewFile(STR(path))
 CALL OutputFaces_cErr(cErr_HIVar,c_HIVar,&
      Mesh,"HI_"//VarName,HIVar,ExactFunction,ExactVars,gmvUnit,Unit_)
 CLOSE(Unit_)

 path = TRIM(OutputFileBase)//ext//"HIl2" 
 Unit_=NewFile(STR(path))
 CALL OutputFaces_L2Err(L2Err_HIVar,L2_HIVar,&
      Mesh,"HI_"//VarName,HIVar,ExactFunction,ExactVars,gmvUnit,Unit_)
 CLOSE(Unit_)
END IF

!1.c. output errors
path = TRIM(OutputFileBase)//ext 
Unit_=NewFile(STR(path))
WRITE(Unit_,FMTv1)"FileName",&
  "cErr_"//"HI_"//VarName,"c_"//"HI_"//VarName,&
  "L2Err_"//"HI_"//VarName,"L2_"//"HI_"//VarName,&
  "cErr_"//"LO_"//VarName,"c_"//"LO_"//VarName,&
  "L2Err_"//"LO_"//VarName,"L2_"//"LO_"//VarName
WRITE(Unit_,FMTv2)TRIM(OutputFileBase),&
  cErr_HIVar,c_HIVar,&
  L2Err_HIVar,L2_HIVar,&
  cErr_LOVar,c_LOVar,&
  L2Err_LOVar,L2_LOVar
CLOSE(Unit_)

!!--end--
END SUBROUTINE



SUBROUTINE OutputCellAFErrors(Mesh,&
  ext,VarName,ExactFunction,HIVar,Directions,&
  gmvUnit,OutputFileBase)

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
CHARACTER(*)   ,INTENT(IN) :: ext,VarName
INTERFACE
 FUNCTION ExactFunction(x,y,Omega)
  USE KND_MoCshort                   !!((03-A-KND_MoCshort.f90))
  REAL(KIND_MCS),INTENT(IN) :: x,y,Omega(2)
  REAL(KIND_MCS) :: ExactFunction
 END FUNCTION
END INTERFACE

!!#### OPTIONAL INPUT
REAL(KIND_MCS) ,INTENT(IN) :: HIVar(:,:)
REAL(KIND_MCS) ,INTENT(IN) :: Directions(:,:)
INTEGER,OPTIONAL,INTENT(IN) :: gmvUnit
CHARACTER(*),OPTIONAL,INTENT(IN) :: OutputFileBase

!!#### LOCAL PARAMETERS
CHARACTER(*),PARAMETER :: FMTv1="(a32,a6,4a26)"
CHARACTER(*),PARAMETER :: FMTv2="(a32,i6,4Es26.13)"

!!#### LOCAL VARIABLES
REAL(KIND_MCS) :: cErr_HIVar
REAL(KIND_MCS) :: L2Err_HIVar
REAL(KIND_MCS) :: c_HIVar
REAL(KIND_MCS) :: L2_HIVar
TYPE(varying_string) :: path
INTEGER :: Unit_
INTEGER :: m,Nm
REAL(KIND_MCS) :: Omega(2)
CHARACTER(4) :: DirStr
!!--begin--


Nm = SIZE(Directions,2)
DO m=1,Nm

 Omega = Directions(1:2,m)
 DirStr = TRIM(STR(m,"(i4.4)"))

 !1.b. high order face error norm calculations
 cErr_HIVar=0._KIND_MCS
 c_HIVar=0._KIND_MCS
 L2Err_HIVar=0._KIND_MCS
 L2_HIVar=0._KIND_MCS
 path = TRIM(OutputFileBase)//ext//&
   DirStr//"c" 
 Unit_=NewFile(STR(path))
 CALL OutputCells_cErr(cErr_HIVar,c_HIVar,&
      Mesh,VarName//"("//DirStr//")",HIVar(:,m),LocalFunction,&
      gmvUnit=gmvUnit,indUnit=Unit_)
 CLOSE(Unit_)

 path = TRIM(OutputFileBase)//ext//&
   DirStr//"l2" 
 Unit_=NewFile(STR(path))
 CALL OutputCells_L2Err(L2Err_HIVar,L2_HIVar,&
      Mesh,VarName//"("//DirStr//")",HIVar(:,m),LocalFunction,&
      gmvUnit=gmvUnit,indUnit=Unit_)
 CLOSE(Unit_)

 !1.c. output errors
 path = TRIM(OutputFileBase)//ext//DirStr 
 Unit_=NewFile(STR(path))
 WRITE(Unit_,FMTv1)"FileName","m",&
   "cErr_"//VarName,"c_"//VarName,&
   "L2Err_"//VarName,"L2_"//VarName
 WRITE(Unit_,FMTv2)TRIM(OutputFileBase),m,&
   cErr_HIVar,c_HIVar,&
   L2Err_HIVar,L2_HIVar
 CLOSE(Unit_)
END DO

!!--end--
CONTAINS

FUNCTION LocalFunction(x,y)
REAL(KIND_MCS),INTENT(IN) :: x,y
REAL(KIND_MCS) :: LocalFunction
!!--begin--
LocalFunction = ExactFunction(x,y,Omega)
!!--end--
END FUNCTION

END SUBROUTINE



SUBROUTINE OutputFaceAFErrors(Mesh,&
  ext,VarName,ExactFunction,HIVar,Directions,&
  gmvUnit,OutputFileBase)

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
CHARACTER(*)   ,INTENT(IN) :: ext,VarName
INTERFACE
 FUNCTION ExactFunction(x,y,Omega)
  USE KND_MoCshort                   !!((03-A-KND_MoCshort.f90))
  REAL(KIND_MCS),INTENT(IN) :: x,y,Omega(2)
  REAL(KIND_MCS) :: ExactFunction
 END FUNCTION
END INTERFACE

!!#### OPTIONAL INPUT
REAL(KIND_MCS) ,INTENT(IN) :: HIVar(:,:)
REAL(KIND_MCS) ,INTENT(IN) :: Directions(:,:)
INTEGER,OPTIONAL,INTENT(IN) :: gmvUnit
CHARACTER(*),OPTIONAL,INTENT(IN) :: OutputFileBase

!!#### LOCAL PARAMETERS
CHARACTER(*),PARAMETER :: FMTv1="(a32,a6,4a26)"
CHARACTER(*),PARAMETER :: FMTv2="(a32,i6,4Es26.13)"

!!#### LOCAL VARIABLES
REAL(KIND_MCS) :: cErr_HIVar
REAL(KIND_MCS) :: L2Err_HIVar
REAL(KIND_MCS) :: c_HIVar
REAL(KIND_MCS) :: L2_HIVar
TYPE(varying_string) :: path
INTEGER :: Unit_
INTEGER :: m,Nm
REAL(KIND_MCS) :: Omega(2)
CHARACTER(4) :: DirStr
!!--begin--


Nm = SIZE(Directions,2)
DO m=1,Nm

 Omega = Directions(1:2,m)
 DirStr = TRIM(STR(m,"(i4.4)"))

 !1.b. high order face error norm calculations
 cErr_HIVar=0._KIND_MCS
 c_HIVar=0._KIND_MCS
 L2Err_HIVar=0._KIND_MCS
 L2_HIVar=0._KIND_MCS
 path = TRIM(OutputFileBase)//ext//&
   DirStr//"c" 
 Unit_=NewFile(STR(path))
 CALL OutputFaces_cErr(cErr_HIVar,c_HIVar,&
      Mesh,VarName//"("//DirStr//")",HIVar(:,m),LocalFunction,&
      gmvUnit=gmvUnit,indUnit=Unit_)
 CLOSE(Unit_)

 path = TRIM(OutputFileBase)//ext//&
   DirStr//"l2" 
 Unit_=NewFile(STR(path))
 CALL OutputFaces_L2Err(L2Err_HIVar,L2_HIVar,&
      Mesh,VarName//"("//DirStr//")",HIVar(:,m),LocalFunction,&
      gmvUnit=gmvUnit,indUnit=Unit_)
 CLOSE(Unit_)

 !1.c. output errors
 path = TRIM(OutputFileBase)//ext//DirStr 
 Unit_=NewFile(STR(path))
 WRITE(Unit_,FMTv1)"FileName","m",&
   "cErr_"//VarName,"c_"//VarName,&
   "L2Err_"//VarName,"L2_"//VarName
 WRITE(Unit_,FMTv2)TRIM(OutputFileBase),m,&
   cErr_HIVar,c_HIVar,&
   L2Err_HIVar,L2_HIVar
 CLOSE(Unit_)
END DO

!!--end--
CONTAINS

FUNCTION LocalFunction(x,y)
REAL(KIND_MCS),INTENT(IN) :: x,y
REAL(KIND_MCS) :: LocalFunction
!!--begin--
LocalFunction = ExactFunction(x,y,Omega)
!!--end--
END FUNCTION

END SUBROUTINE



SUBROUTINE OutputFaceNormalErrors(Mesh,LO_Mesh,&
  ext,VarName,ExactFunction1,ExactFunction2,HIVar,LOVar,ExactVars,&
  gmvUnit,OutputFileBase)

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
TYPE(TYPE_Mesh),INTENT(IN) :: LO_Mesh
CHARACTER(*)   ,INTENT(IN) :: ext,VarName
INTERFACE
 FUNCTION ExactFunction1(x,y)
  USE KND_MoCshort                   !!((03-A-KND_MoCshort.f90))
  REAL(KIND_MCS),INTENT(IN) :: x,y
  REAL(KIND_MCS) :: ExactFunction1
 END FUNCTION
END INTERFACE
INTERFACE
 FUNCTION ExactFunction2(x,y)
  USE KND_MoCshort                   !!((03-A-KND_MoCshort.f90))
  REAL(KIND_MCS),INTENT(IN) :: x,y
  REAL(KIND_MCS) :: ExactFunction2
 END FUNCTION
END INTERFACE

!!#### OPTIONAL INPUT
REAL(KIND_MCS) ,INTENT(IN),OPTIONAL :: HIVar(:),LOVar(:)
REAL(KIND_MCS),POINTER,OPTIONAL :: ExactVars(:,:)
INTEGER,OPTIONAL,INTENT(IN) :: gmvUnit
CHARACTER(*),OPTIONAL,INTENT(IN) :: OutputFileBase

!!#### LOCAL PARAMETERS
CHARACTER(*),PARAMETER :: FMTv1="(a32,8a26)"
CHARACTER(*),PARAMETER :: FMTv2="(a32,8Es26.13)"

!!#### LOCAL VARIABLES
REAL(KIND_MCS) :: cErr_LOVar,cErr_HIVar
REAL(KIND_MCS) :: L2Err_LOVar,L2Err_HIVar
REAL(KIND_MCS) :: c_LOVar,c_HIVar
REAL(KIND_MCS) :: L2_LOVar,L2_HIVar
TYPE(varying_string) :: path
INTEGER :: Unit_

!!--begin--

!1.a. low order face error norm calculations
cErr_LOVar=0._KIND_MCS
c_LOVar=0._KIND_MCS
L2Err_LOVar=0._KIND_MCS
L2_LOVar=0._KIND_MCS
IF( PRESENT(LOVar) )THEN
 path = TRIM(OutputFileBase)//ext//"LOc" 
 Unit_=NewFile(STR(path))
 CALL OutputFaceNormals_cErr(cErr_LOVar,c_LOVar,&
   LO_Mesh,"LO_"//VarName,LOVar,ExactFunction1,ExactFunction2,ExactVars,&
   gmvUnit,Unit_)
 CLOSE(Unit_)

 path = TRIM(OutputFileBase)//ext//"LOl2" 
 Unit_=NewFile(STR(path))
 CALL OutputFaceNormals_L2Err(L2Err_LOVar,L2_LOVar,&
   LO_Mesh,"LO_"//VarName,LOVar,ExactFunction1,ExactFunction2,ExactVars,&
   gmvUnit,Unit_)
 CLOSE(Unit_)
END IF

!1.b. high order face error norm calculations
cErr_HIVar=0._KIND_MCS
c_HIVar=0._KIND_MCS
L2Err_HIVar=0._KIND_MCS
L2_HIVar=0._KIND_MCS
IF( PRESENT(HIVar) )THEN
 path = TRIM(OutputFileBase)//ext//"HIc" 
 Unit_=NewFile(STR(path))
 CALL OutputFaceNormals_cErr(cErr_HIVar,c_HIVar,&
      Mesh,"HI_"//VarName,HIVar,ExactFunction1,ExactFunction2,ExactVars,gmvUnit)
 CLOSE(Unit_)

 path = TRIM(OutputFileBase)//ext//"HIl2" 
 Unit_=NewFile(STR(path))
 CALL OutputFaceNormals_L2Err(L2Err_HIVar,L2_HIVar,&
      Mesh,"HI_"//VarName,HIVar,ExactFunction1,ExactFunction2,ExactVars,gmvUnit)
 CLOSE(Unit_)
END IF

!1.c. output errors
path = TRIM(OutputFileBase)//ext 
Unit_=NewFile(STR(path))
WRITE(Unit_,FMTv1)"FileName",&
  "cErr_"//"HI_"//VarName,"c_"//"HI_"//VarName,&
  "L2Err_"//"HI_"//VarName,"L2_"//"HI_"//VarName,&
  "cErr_"//"LO_"//VarName,"c_"//"LO_"//VarName,&
  "L2Err_"//"LO_"//VarName,"L2_"//"LO_"//VarName
WRITE(Unit_,FMTv2)TRIM(OutputFileBase),&
  cErr_HIVar,c_HIVar,&
  L2Err_HIVar,L2_HIVar,&
  cErr_LOVar,c_LOVar,&
  L2Err_LOVar,L2_LOVar
CLOSE(Unit_)

!!--end--
END SUBROUTINE


SUBROUTINE OutputCellErrors(Mesh,LO_Mesh,&
  ext,VarName,ExactFunction,HIVar,LOVar,ExactVars,&
  gmvUnit,OutputFileBase)

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
TYPE(TYPE_Mesh),INTENT(IN) :: LO_Mesh
CHARACTER(*)   ,INTENT(IN) :: ext,VarName
INTERFACE
 FUNCTION ExactFunction(x,y)
  USE KND_MoCshort                   !!((03-A-KND_MoCshort.f90))
  REAL(KIND_MCS),INTENT(IN) :: x,y
  REAL(KIND_MCS) :: ExactFunction
 END FUNCTION
END INTERFACE

!!#### OPTIONAL INPUT
REAL(KIND_MCS) ,INTENT(IN),OPTIONAL :: HIVar(:),LOVar(:)
REAL(KIND_MCS),POINTER,OPTIONAL :: ExactVars(:)
INTEGER,OPTIONAL,INTENT(IN) :: gmvUnit
CHARACTER(*),OPTIONAL,INTENT(IN) :: OutputFileBase

!!#### LOCAL PARAMETERS
CHARACTER(*),PARAMETER :: FMTv1="(a32,8a26)"
CHARACTER(*),PARAMETER :: FMTv2="(a32,8Es26.13)"

!!#### LOCAL VARIABLES
REAL(KIND_MCS) :: cErr_LOVar,cErr_HIVar
REAL(KIND_MCS) :: L2Err_LOVar,L2Err_HIVar
REAL(KIND_MCS) :: c_LOVar,c_HIVar
REAL(KIND_MCS) :: L2_LOVar,L2_HIVar
TYPE(varying_string) :: path
INTEGER :: Unit_

!!--begin--

!1.a. low order cell error norm calculations
cErr_LOVar=0._KIND_MCS
L2Err_LOVar=0._KIND_MCS
c_LOVar=0._KIND_MCS
L2_LOVar=0._KIND_MCS
IF( PRESENT(LOVar) )THEN
 path = TRIM(OutputFileBase)//ext//"LOc" 
 Unit_=NewFile(STR(path))
 CALL OutputCells_cErr(cErr_LOVar,c_LOVar,&
   LO_Mesh,"LO_"//VarName,LOVar,ExactFunction,ExactVars,gmvUnit,Unit_)
 CLOSE(Unit_)

 path = TRIM(OutputFileBase)//ext//"LOl2" 
 Unit_=NewFile(STR(path))
 CALL OutputCells_L2Err(L2Err_LOVar,L2_LOvar,&
   LO_Mesh,"LO_"//VarName,LOVar,ExactFunction,ExactVars,gmvUnit,Unit_)
 CLOSE(Unit_)
END IF

!1.b. high order cell error norm calculations
cErr_HIVar=0._KIND_MCS
L2Err_HIVar=0._KIND_MCS
c_HIVar=0._KIND_MCS
L2_HIVar=0._KIND_MCS
IF( PRESENT(HIVar) )THEN
 path = TRIM(OutputFileBase)//ext//"HIc" 
 Unit_=NewFile(STR(path))
 CALL OutputCells_cErr(cErr_HIVar,c_HIVar,&
      Mesh,"HI_"//VarName,HIVar,ExactFunction,ExactVars,gmvUnit,Unit_)
 CLOSE(Unit_)

 path = TRIM(OutputFileBase)//ext//"HIl2" 
 Unit_=NewFile(STR(path))
 CALL OutputCells_L2Err(L2Err_HIVar,L2_HIVar,&
      Mesh,"HI_"//VarName,HIVar,ExactFunction,ExactVars,gmvUnit,Unit_)
 CLOSE(Unit_)
END IF

!1.c. output errors
path = TRIM(OutputFileBase)//ext 
Unit_=NewFile(STR(path))
WRITE(Unit_,FMTv1)"FileName",&
  "cErr_"//"HI_"//VarName,"c_"//"HI_"//VarName,&
  "L2Err_"//"HI_"//VarName,"L2_"//"HI_"//VarName,&
  "cErr_"//"LO_"//VarName,"c_"//"LO_"//VarName,&
  "L2Err_"//"LO_"//VarName,"L2_"//"LO_"//VarName
WRITE(Unit_,FMTv2)TRIM(OutputFileBase),&
  cErr_HIVar,c_HIVar,&
  L2Err_HIVar,L2_HIVar,&
  cErr_LOVar,c_LOVar,&
  L2Err_LOVar,L2_LOVar
CLOSE(Unit_)

!!--end--
END SUBROUTINE



FUNCTION EXACT_Psi(x,y,Omega) RESULT(Psi)
REAL(KIND_MCS),INTENT(IN) :: x,y,Omega(2)
REAL(KIND_MCS) :: Psi
!!--begin--
SELECT CASE(TransportAnalyticTest)
 CASE(TRANSPORT_ANALYTIC_TEST_WARSA)  ; Psi=EXACT_Psi_WARSA (x,y,Omega)
 CASE(TRANSPORT_ANALYTIC_TEST_WIESEL) ; Psi=EXACT_Psi_WIESEL(x,y,Omega)
END SELECT
!!--end--
END FUNCTION


FUNCTION EXACT_SnPhi(x,y) RESULT(SnPhi)
REAL(KIND_MCS),INTENT(IN) :: x,y
REAL(KIND_MCS) :: SnPhi
!!--begin--
SELECT CASE(TransportAnalyticTest)
 CASE(TRANSPORT_ANALYTIC_TEST_WARSA)  ; SnPhi=EXACT_SnPhi_WARSA (x,y)
 CASE(TRANSPORT_ANALYTIC_TEST_WIESEL) ; SnPhi=EXACT_SnPhi_WIESEL(x,y)
END SELECT
!!--end--
END FUNCTION


FUNCTION EXACT_SnExx(x,y) RESULT(SnExx)
REAL(KIND_MCS),INTENT(IN) :: x,y
REAL(KIND_MCS) :: SnExx
!!--begin--
SELECT CASE(TransportAnalyticTest)
 CASE(TRANSPORT_ANALYTIC_TEST_WARSA)  ; SnExx=EXACT_SnExx_WARSA (x,y)
 CASE(TRANSPORT_ANALYTIC_TEST_WIESEL) ; SnExx=EXACT_SnExx_WIESEL(x,y)
END SELECT
!!--end--
END FUNCTION

FUNCTION EXACT_SnEyy(x,y) RESULT(SnEyy)
REAL(KIND_MCS),INTENT(IN) :: x,y
REAL(KIND_MCS) :: SnEyy
!!--begin--
SELECT CASE(TransportAnalyticTest)
 CASE(TRANSPORT_ANALYTIC_TEST_WARSA)  ; SnEyy=EXACT_SnEyy_WARSA (x,y)
 CASE(TRANSPORT_ANALYTIC_TEST_WIESEL) ; SnEyy=EXACT_SnEyy_WIESEL(x,y)
END SELECT
!!--end--
END FUNCTION

FUNCTION EXACT_SnExy(x,y) RESULT(SnExy)
REAL(KIND_MCS),INTENT(IN) :: x,y
REAL(KIND_MCS) :: SnExy
!!--begin--
SELECT CASE(TransportAnalyticTest)
 CASE(TRANSPORT_ANALYTIC_TEST_WARSA)  ; SnExy=EXACT_SnExy_WARSA (x,y)
 CASE(TRANSPORT_ANALYTIC_TEST_WIESEL) ; SnExy=EXACT_SnExy_WIESEL(x,y)
END SELECT
!!--end--
END FUNCTION


FUNCTION EXACT_Exx(x,y) RESULT(Exx)
REAL(KIND_MCS),INTENT(IN) :: x,y
REAL(KIND_MCS) :: Exx
!!--begin--
SELECT CASE(TransportAnalyticTest)
 CASE(TRANSPORT_ANALYTIC_TEST_WARSA)  ; Exx=EXACT_Exx_WARSA (x,y)
 CASE(TRANSPORT_ANALYTIC_TEST_WIESEL) ; Exx=EXACT_Exx_WIESEL(x,y)
END SELECT
!!--end--
END FUNCTION

FUNCTION EXACT_Eyy(x,y) RESULT(Eyy)
REAL(KIND_MCS),INTENT(IN) :: x,y
REAL(KIND_MCS) :: Eyy
!!--begin--
SELECT CASE(TransportAnalyticTest)
 CASE(TRANSPORT_ANALYTIC_TEST_WARSA)  ; Eyy=EXACT_Eyy_WARSA (x,y)
 CASE(TRANSPORT_ANALYTIC_TEST_WIESEL) ; Eyy=EXACT_Eyy_WIESEL(x,y)
END SELECT
!!--end--
END FUNCTION

FUNCTION EXACT_Exy(x,y) RESULT(Exy)
REAL(KIND_MCS),INTENT(IN) :: x,y
REAL(KIND_MCS) :: Exy
!!--begin--
SELECT CASE(TransportAnalyticTest)
 CASE(TRANSPORT_ANALYTIC_TEST_WARSA)  ; Exy=EXACT_Exy_WARSA (x,y)
 CASE(TRANSPORT_ANALYTIC_TEST_WIESEL) ; Exy=EXACT_Exy_WIESEL(x,y)
END SELECT
!!--end--
END FUNCTION



FUNCTION EXACT_Kxx(x,y) RESULT(Kxx)
REAL(KIND_MCS),INTENT(IN) :: x,y
REAL(KIND_MCS) :: Kxx
!!--begin--
SELECT CASE(TransportAnalyticTest)
 CASE(TRANSPORT_ANALYTIC_TEST_WARSA)  ; Kxx=EXACT_Kxx_WARSA (x,y)
 CASE(TRANSPORT_ANALYTIC_TEST_WIESEL) ; Kxx=EXACT_Kxx_WIESEL(x,y)
END SELECT
!!--end--
END FUNCTION

FUNCTION EXACT_Kyy(x,y) RESULT(Kyy)
REAL(KIND_MCS),INTENT(IN) :: x,y
REAL(KIND_MCS) :: Kyy
!!--begin--
SELECT CASE(TransportAnalyticTest)
 CASE(TRANSPORT_ANALYTIC_TEST_WARSA)  ; Kyy=EXACT_Kyy_WARSA (x,y)
 CASE(TRANSPORT_ANALYTIC_TEST_WIESEL) ; Kyy=EXACT_Kyy_WIESEL(x,y)
END SELECT
!!--end--
END FUNCTION

FUNCTION EXACT_Kxy(x,y) RESULT(Kxy)
REAL(KIND_MCS),INTENT(IN) :: x,y
REAL(KIND_MCS) :: Kxy
!!--begin--
SELECT CASE(TransportAnalyticTest)
 CASE(TRANSPORT_ANALYTIC_TEST_WARSA)  ; Kxy=EXACT_Kxy_WARSA (x,y)
 CASE(TRANSPORT_ANALYTIC_TEST_WIESEL) ; Kxy=EXACT_Kxy_WIESEL(x,y)
END SELECT
!!--end--
END FUNCTION



FUNCTION EXACT_SnKxx(x,y) RESULT(SnKxx)
REAL(KIND_MCS),INTENT(IN) :: x,y
REAL(KIND_MCS) :: SnKxx
!!--begin--
SELECT CASE(TransportAnalyticTest)
 CASE(TRANSPORT_ANALYTIC_TEST_WARSA)  ; SnKxx=EXACT_SnKxx_WARSA (x,y)
 CASE(TRANSPORT_ANALYTIC_TEST_WIESEL) ; SnKxx=EXACT_SnKxx_WIESEL(x,y)
END SELECT
!!--end--
END FUNCTION

FUNCTION EXACT_SnKyy(x,y) RESULT(SnKyy)
REAL(KIND_MCS),INTENT(IN) :: x,y
REAL(KIND_MCS) :: SnKyy
!!--begin--
SELECT CASE(TransportAnalyticTest)
 CASE(TRANSPORT_ANALYTIC_TEST_WARSA)  ; SnKyy=EXACT_SnKyy_WARSA (x,y)
 CASE(TRANSPORT_ANALYTIC_TEST_WIESEL) ; SnKyy=EXACT_SnKyy_WIESEL(x,y)
END SELECT
!!--end--
END FUNCTION

FUNCTION EXACT_SnKxy(x,y) RESULT(SnKxy)
REAL(KIND_MCS),INTENT(IN) :: x,y
REAL(KIND_MCS) :: SnKxy
!!--begin--
SELECT CASE(TransportAnalyticTest)
 CASE(TRANSPORT_ANALYTIC_TEST_WARSA)  ; SnKxy=EXACT_SnKxy_WARSA (x,y)
 CASE(TRANSPORT_ANALYTIC_TEST_WIESEL) ; SnKxy=EXACT_SnKxy_WIESEL(x,y)
END SELECT
!!--end--
END FUNCTION



FUNCTION EXACT_Phi(x,y) RESULT(Phi)
REAL(KIND_MCS),INTENT(IN) :: x,y
REAL(KIND_MCS) :: Phi
!!--begin--
SELECT CASE(TransportAnalyticTest)
 CASE(TRANSPORT_ANALYTIC_TEST_WARSA)  ; Phi=EXACT_Phi_WARSA (x,y)
 CASE(TRANSPORT_ANALYTIC_TEST_WIESEL) ; Phi=EXACT_Phi_WIESEL(x,y)
END SELECT
!!--end--
END FUNCTION


FUNCTION EXACT_SnMom0Qext(x,y) RESULT(SnMom0Qext)
REAL(KIND_MCS),INTENT(IN) :: x,y
REAL(KIND_MCS) :: SnMom0Qext
!!--begin--
SELECT CASE(TransportAnalyticTest)
 CASE(TRANSPORT_ANALYTIC_TEST_WARSA)  ; SnMom0Qext=EXACT_SnMom0Qext_WARSA (x,y)
 CASE(TRANSPORT_ANALYTIC_TEST_WIESEL) ; SnMom0Qext=EXACT_SnMom0Qext_WIESEL(x,y)
END SELECT
!!--end--
END FUNCTION


FUNCTION EXACT_SnMom1yQext(x,y) RESULT(SnMom1yQext)
REAL(KIND_MCS),INTENT(IN) :: x,y
REAL(KIND_MCS) :: SnMom1yQext
!!--begin--
SELECT CASE(TransportAnalyticTest)
 CASE(TRANSPORT_ANALYTIC_TEST_WARSA)  ; SnMom1yQext=EXACT_SnMom1yQext_WARSA (x,y)
 CASE(TRANSPORT_ANALYTIC_TEST_WIESEL) ; SnMom1yQext=EXACT_SnMom1yQext_WIESEL(x,y)
END SELECT
!!--end--
END FUNCTION


FUNCTION EXACT_SnMom1xQext(x,y) RESULT(SnMom1xQext)
REAL(KIND_MCS),INTENT(IN) :: x,y
REAL(KIND_MCS) :: SnMom1xQext
!!--begin--
SELECT CASE(TransportAnalyticTest)
 CASE(TRANSPORT_ANALYTIC_TEST_WARSA)  ; SnMom1xQext=EXACT_SnMom1xQext_WARSA (x,y)
 CASE(TRANSPORT_ANALYTIC_TEST_WIESEL) ; SnMom1xQext=EXACT_SnMom1xQext_WIESEL(x,y)
END SELECT
!!--end--
END FUNCTION


FUNCTION EXACT_Mom0Qext(x,y) RESULT(Mom0Qext)
REAL(KIND_MCS),INTENT(IN) :: x,y
REAL(KIND_MCS) :: Mom0Qext
!!--begin--
SELECT CASE(TransportAnalyticTest)
 CASE(TRANSPORT_ANALYTIC_TEST_WARSA)  ; Mom0Qext=EXACT_Mom0Qext_WARSA (x,y)
 CASE(TRANSPORT_ANALYTIC_TEST_WIESEL) ; Mom0Qext=EXACT_Mom0Qext_WIESEL(x,y)
END SELECT
!!--end--
END FUNCTION


FUNCTION EXACT_Mom1yQext(x,y) RESULT(Mom1yQext)
REAL(KIND_MCS),INTENT(IN) :: x,y
REAL(KIND_MCS) :: Mom1yQext
!!--begin--
SELECT CASE(TransportAnalyticTest)
 CASE(TRANSPORT_ANALYTIC_TEST_WARSA)  ; Mom1yQext=EXACT_Mom1yQext_WARSA (x,y)
 CASE(TRANSPORT_ANALYTIC_TEST_WIESEL) ; Mom1yQext=EXACT_Mom1yQext_WIESEL(x,y)
END SELECT
!!--end--
END FUNCTION


FUNCTION EXACT_Mom1xQext(x,y) RESULT(Mom1xQext)
REAL(KIND_MCS),INTENT(IN) :: x,y
REAL(KIND_MCS) :: Mom1xQext
!!--begin--
SELECT CASE(TransportAnalyticTest)
 CASE(TRANSPORT_ANALYTIC_TEST_WARSA)  ; Mom1xQext=EXACT_Mom1xQext_WARSA (x,y)
 CASE(TRANSPORT_ANALYTIC_TEST_WIESEL) ; Mom1xQext=EXACT_Mom1xQext_WIESEL(x,y)
END SELECT
!!--end--
END FUNCTION


FUNCTION EXACT_SnJx(x,y) RESULT(SnJx)
REAL(KIND_MCS),INTENT(IN) :: x,y
REAL(KIND_MCS) :: SnJx
!!--begin--
SELECT CASE(TransportAnalyticTest)
 CASE(TRANSPORT_ANALYTIC_TEST_WARSA)  ; SnJx=EXACT_SnJx_WARSA (x,y)
 CASE(TRANSPORT_ANALYTIC_TEST_WIESEL) ; SnJx=EXACT_SnJx_WIESEL(x,y)
END SELECT
!!--end--
END FUNCTION

FUNCTION EXACT_SnJy(x,y) RESULT(SnJy)
REAL(KIND_MCS),INTENT(IN) :: x,y
REAL(KIND_MCS) :: SnJy
!!--begin--
SELECT CASE(TransportAnalyticTest)
 CASE(TRANSPORT_ANALYTIC_TEST_WARSA)  ; SnJy=EXACT_SnJy_WARSA (x,y)
 CASE(TRANSPORT_ANALYTIC_TEST_WIESEL) ; SnJy=EXACT_SnJy_WIESEL(x,y)
END SELECT
!!--end--
END FUNCTION


FUNCTION EXACT_Jx(x,y) RESULT(Jx)
REAL(KIND_MCS),INTENT(IN) :: x,y
REAL(KIND_MCS) :: Jx
!!--begin--
SELECT CASE(TransportAnalyticTest)
 CASE(TRANSPORT_ANALYTIC_TEST_WARSA)  ; Jx=EXACT_Jx_WARSA (x,y)
 CASE(TRANSPORT_ANALYTIC_TEST_WIESEL) ; Jx=EXACT_Jx_WIESEL(x,y)
END SELECT
!!--end--
END FUNCTION

FUNCTION EXACT_Jy(x,y) RESULT(Jy)
REAL(KIND_MCS),INTENT(IN) :: x,y
REAL(KIND_MCS) :: Jy
!!--begin--
SELECT CASE(TransportAnalyticTest)
 CASE(TRANSPORT_ANALYTIC_TEST_WARSA)  ; Jy=EXACT_Jy_WARSA (x,y)
 CASE(TRANSPORT_ANALYTIC_TEST_WIESEL) ; Jy=EXACT_Jy_WIESEL(x,y)
END SELECT
!!--end--
END FUNCTION

FUNCTION EXACT_Qext(x,y,Omega) RESULT(Qext)
REAL(KIND_MCS),INTENT(IN) :: x,y,Omega(2)
REAL(KIND_MCS) :: Qext
!!--begin--
SELECT CASE(TransportAnalyticTest)
 CASE(TRANSPORT_ANALYTIC_TEST_WARSA)  ; Qext=EXACT_Qext_WARSA (x,y,Omega)
 CASE(TRANSPORT_ANALYTIC_TEST_WIESEL) ; Qext=EXACT_Qext_WIESEL(x,y,Omega)
END SELECT
!!--end--
END FUNCTION

FUNCTION EXACT_SnQext(x,y,Omega) RESULT(SnQext)
REAL(KIND_MCS),INTENT(IN) :: x,y,Omega(2)
REAL(KIND_MCS) :: SnQext
!!--begin--
SELECT CASE(TransportAnalyticTest)
 CASE(TRANSPORT_ANALYTIC_TEST_WARSA)  ; SnQext=EXACT_SnQext_WARSA (x,y,Omega)
 CASE(TRANSPORT_ANALYTIC_TEST_WIESEL) ; SnQext=EXACT_SnQext_WIESEL(x,y,Omega)
END SELECT
!!--end--
END FUNCTION



END MODULE
