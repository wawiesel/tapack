!!# MODULE <<TBX_QuasiDiffusion>>
MODULE TBX_QuasiDiffusion

!!## PURPOSE
!! The toolbox of quasi-diffusion routines.

!!## EXTERNAL KINDS
USE KND_MoCshort,ONLY: KIND_AngularFlux                            !!((03-A-KND_MoCshort.f90))
USE KND_QuasiDiffusion                                             !!((02-A-KND_QuasiDiffusion.f90))

!!## EXTERNAL PROCEDURES
USE FUN_EQUILOC                                                    !!((03-A-FUN_EQUILOC.f90))
USE FUN_NewFile                                                    !!((05-B-FUN_NewFile.f90))
USE SUB_Reallocate                                                 !!((04-B-SUB_Reallocate.f90))
USE SUB_Pause                                                      !!((04-B-SUB_Pause.f90))
USE SUB_Stop                                                       !!((04-B-SUB_Stop.f90))
USE FUN_STR                                                        !!((05-B-FUN_STR.f90))
USE FUN_VSTR                                                       !!((05-B-FUN_VSTR.f90))
USE FUN_Jiggle                                                     !!((05-B-FUN_Jiggle.f90))
USE FUN_IsError                                                    !!((05-A-FUN_IsError.f90))
USE FUN_SmartOption                                                !!((07-B-FUN_SmartOption.f90))
USE FUN_STRTIME                                                    !!((06-C-FUN_STRTIME.f90))
USE FUN_Substitute                                                 !!((06-C-FUN_Substitute.f90))
USE ISO_varying_string                                             !!((03-A-ISO_varying_string.f90))
USE PAR_Constants_Rdp                                              !!((02-A-PAR_Constants_Rdp.f90))
USE SUB_CLEAR                                                      !!((04-A-SUB_CLEAR.f90))

!!## GLOBAL LIBRARIES
USE LIB_Norm                                                       !!((04-B-LIB_Norm.f90))

!!## GLOBAL PRINTING SUBROUTINES
USE PRN_Text                                                       !!((07-B-PRN_Text.f90))
USE PRN_Matrix_dmat                                                !!((09-B-PRN_Matrix_dmat.f90))
USE PRN_MatrixSymbolic_dmat                                        !!((03-A-PRN_MatrixSymbolic_dmat.f90))
![novis]!USE PLT_QuasiDiffusion                                             !!((54-B-PLT_QuasiDiffusion.f90))

!!## USER MODULES
!! * feedback user module
USE USR_fdbk                                                       !!((08-C-USR_fdbk.f90))
USE USR_SimpleSet                                                  !!((12-B-USR_SimpleSet.f90))
USE USR_DiscreteOrdinates                                          !!((34-B-USR_DiscreteOrdinates.f90))
USE USR_QuasiDiffusion                                             !!((67-B-USR_QuasiDiffusion.f90))
USE USR_QuasiDiffusion01                                           !!((68-B-USR_QuasiDiffusion01.f90))
USE USR_QuasiDiffusion02                                           !!((68-B-USR_QuasiDiffusion02.f90))
USE USR_QuasiDiffusion03                                           !!((68-B-USR_QuasiDiffusion03.f90))
USE USR_QuasiDiffusion04                                           !!((68-B-USR_QuasiDiffusion04.f90))
USE USR_QuasiDiffusion05                                           !!((68-B-USR_QuasiDiffusion05.f90))
USE USR_QDAnalyticTest                                             !!((47-C-USR_QDAnalyticTest.f90))
USE USR_TAPACK,ONLY: Update_BoundaryFactors,Update_Eddingtons,&    !!((48-C-USR_TAPACK.f90))
  CHECK_TotSourcePositive,CHECK_ScalarBalanceEquation,&
  UPDATE_CellFunction
USE USR_Source                                                     !!((35-B-USR_Source.f90))

!!## GLOBAL TOOLBOXES
!! * SMLib sparse matrix computation
USE TBX_ComputationalGeometry                                      !!((09-A-TBX_ComputationalGeometry.f90))
USE TBX_Mesh,ONLY: UPDATE_BoundaryFaces,FaceCentroid,&             !!((15-B-TBX_Mesh.f90))
  FaceAverage_F,TEST_DualMesh,CREATE_Mesh_CellBasedDual,&
  GET_Inv_JBdryE,MAPTO_CellBased_Boundary,MAPTO_CellBased,&
  NUM_Verts,NUM_Faces,NUM_Cells,NUM_Dimensions,IsBoundaryFace,FaceArea

!!## EXTERNAL PARAMETERS
USE PAR_QuasiDiffusion                                             !!((05-C-PAR_QuasiDiffusion.f90))
USE SUB_SAVE                                                       !!((06-B-SUB_SAVE.f90))
USE SUB_SAVEn                                                      !!((06-B-SUB_SAVEn.f90))
USE SUB_LOADn                                                      !!((06-B-SUB_LOADn.f90))

!!## GLOBAL LIBRARIES
USE LIB_Prompts                                                    !!((06-B-LIB_Prompts.f90))
USE LIB_GenericPhrases                                             !!((07-B-LIB_GenericPhrases.f90))

USE VAR_QuasiDiffusion                                             !!((46-B-VAR_QuasiDiffusion.f90))
USE VAR_EddingtonFactors                                           !!((04-C-VAR_EddingtonFactors.f90))
USE VAR_EnergyGroups ,ONLY: Ng                                     !!((47-B-VAR_EnergyGroups.f90))
USE VAR_Mesh         ,ONLY: Mesh                                   !!((46-B-VAR_Mesh.f90))
USE VAR_AngularFluxes,ONLY: AngularFluxC,AngularFluxF,AngularFluxV !!((04-C-VAR_AngularFluxes.f90))
USE VAR_ScalarFluxes ,ONLY: LO_ScalarFluxC,LO_ScalarFluxF,&        !!((04-C-VAR_ScalarFluxes.f90))
                            ScalarFluxCellFunction,ScalarFluxIN,&
                            LastLO_ScalarFluxC,LastLO_ScalarFluxF,&
                            ScalarFluxC,ScalarFluxF,LastScalarFluxC,LastScalarFluxF
USE VAR_Currents     ,ONLY: LO_CurrentFN,CurrentIN,CurrentFN,LastLO_CurrentFN,LastCurrentFN       !!((04-C-VAR_Currents.f90))
USE VAR_DiscreteOrdinates,ONLY: Ordinates,Weights                  !!((47-B-VAR_DiscreteOrdinates.f90))
USE VAR_Source                                                     !!((47-B-VAR_Source.f90))
USE VAR_Materials                                                  !!((48-B-VAR_Materials.f90))
USE VAR_XSMonkey                                                   !!((47-B-VAR_XSMonkey.f90))
USE VAR_MoCshort     ,ONLY: BC,Using_AnalyticTransportTest         !!((47-B-VAR_MoCshort.f90))
USE VAR_TAPACK       ,ONLY: Author,Label,OutputFileBase,&              !!((66-C-VAR_TAPACK.f90))
  tolPhiVInf,&
  FactorGenerationStyle,FactorEvalMethod,CellFunctionMethod,&
  tolBalRel,tolBalAbs
USE USR_TransportAnalyticTest                                      !!((56-C-USR_TransportAnalyticTest.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## DEFAULT ACCESS
PRIVATE

!!## ACCESS
PUBLIC :: SETUP_QuasiDiffusion
PUBLIC :: SOLVE_QuasiDiffusion
PUBLIC :: REFINE_QuasiDiffusion
PUBLIC :: WRAPUP_QuasiDiffusion


!!## CONTAINED PROCEDURES
CONTAINS


!!### SUBROUTINE <<SETUP_QuasiDiffusion>>
SUBROUTINE SETUP_QuasiDiffusion( fdbk )

!!#### PURPOSE
!! Set up the quasi-diffusion matrix and right hand side
!! and prepare the sparse matrix solution.

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
REAL    :: tin,tout,dt
INTEGER :: Unit_

!!--begin--

!begin time
CALL CPU_TIME(tin)

!test the dual mesh
CALL Test_DualMesh(Mesh,fdbk)

SELECT CASE( System )
 !CBoundary (on essential)
 CASE( QDF_System01 )
   !this method operates on an essential quad mesh
   LO_Mesh => Mesh
   CALL SETUP_QDF01( Ng , LO_Mesh , A , b , x , Jbdry , Jdomn , fdbk )

 !CBoundary (on cellbased)
 CASE( QDF_System02 )
   !this method operates on a cell-based quad mesh
   ALLOCATE( LO_Mesh )
   CALL CREATE_Mesh_CellBasedDual(Mesh,LO_Mesh,FaceMapping)
   CALL SETUP_QDF02( Ng , LO_Mesh , A , b , x , Jbdry , Jdomn , fdbk , &
       Return_Interfaces=LO_Interfaces)

 !Aristova
 CASE( QDF_System03 )
   !this method operates on a cell-based quad mesh
   ALLOCATE( LO_Mesh )
   CALL CREATE_Mesh_CellBasedDual(Mesh,LO_Mesh,FaceMapping)
   CALL SETUP_QDF03( Ng , LO_Mesh , A , b , x , Jbdry , Jdomn , fdbk , &
       Return_Interfaces=LO_Interfaces)

 !Morel
 CASE( QDF_System04 )
   !this method operates on a cell-based quad mesh
   ALLOCATE( LO_Mesh )
   CALL CREATE_Mesh_CellBasedDual(Mesh,LO_Mesh,FaceMapping)
   CALL SETUP_QDF04( Ng , LO_Mesh , A , b , x , Jbdry , Jdomn , fdbk , &
       Return_Interfaces=LO_Interfaces )

 !Morel+Consistency
 CASE( QDF_System05 )
   !this method operates on a cell-based quad mesh
   ALLOCATE( LO_Mesh )
   CALL CREATE_Mesh_CellBasedDual(Mesh,LO_Mesh,FaceMapping)
   CALL SETUP_QDF05( Ng , LO_Mesh , A , b , x , Jbdry , Jdomn , fdbk , &
       Return_Interfaces=LO_Interfaces )

END SELECT

Njbdry = SIZE(Jbdry)
Ni     = NUM_Cells(LO_Mesh)
Nj     = NUM_Faces(LO_Mesh)
Nk     = NUM_Verts(LO_Mesh)

!! Nullify convergence history.
IF( Solver==QDF_Iterative )THEN
 CALL NULLIFY_ConvHist( ConvergenceHistory )
END IF

!! Allocate the rest of the stuff.
CALL ALLOCATE_QDF(Ng,Ni,Nj,Nk,Njbdry,&
  EddingtonxxV,EddingtonyyV,EddingtonxyV,&
  EddingtonxxF,EddingtonyyF,EddingtonxyF,&
  EddingtonxxC,EddingtonyyC,EddingtonxyC,&
  KxxV,KyyV,KxyV,&
  KxxF,KyyF,KxyF,&
  KxxC,KyyC,KxyC,&
  ScalarFluxIN,CurrentIN,CBoundary)

!! Allocate the scalar flux function.
IF( .NOT.ASSOCIATED(ScalarFluxCellFunction) )THEN
 ALLOCATE( ScalarFluxCellFunction(1:LO_Mesh%Ndim+1,1:Ng,1:NUM_Cells(LO_Mesh)) )
 ScalarFluxCellFunction = REAL(0,KIND(ScalarFluxCellFunction))
END IF

!! Check face integration.

!Unit_ = NewFile(TRIM(OutputFileBase)//".hiif")
!WRITE(*,*)"HI_Mesh::TEST_IntegrateFaces=",&
!  MERGE("Pass","Fail",TEST_FaceIntegration_WARSA(Mesh,Noisy=.TRUE.,&
!  Noisy_Unit=Unit_))
!CLOSE(Unit_)

!Unit_ = NewFile(TRIM(OutputFileBase)//".loif")
!WRITE(*,*)"LO_Mesh::TEST_IntegrateFaces=",&
!  MERGE("Pass","Fail",TEST_FaceIntegration_WARSA(LO_Mesh,Noisy=.TRUE.,&
!  Noisy_Unit=Unit_))
!CLOSE(Unit_)


!! Determine runtime for this routine.
CALL CPU_TIME(tout)
dt = (tout-tin)

!! Print message.
CALL UPDATE(fdbk_comment,fdbk,s="[[QDF]] Setup completed in &
  &[time="//STRTIME(dt)//"].")

!!--end--
END SUBROUTINE




!!### REFINEMENT ROUTINE: REFINE_QuasiDiffusion
FUNCTION REFINE_QuasiDiffusion( iter , dt0 , fdbk , Unit ) RESULT(Refined)
!!#### PURPOSE
!! This is the driver for the QuasiDiffusion refinement/convergence checking.

!!#### REQUIRED INPUT/OUTPUT
!! * global iteration count
!! * global runtime
!! * feedback variable <fdbk>
INTEGER        ,INTENT(INOUT) :: iter
REAL           ,INTENT(INOUT) :: dt0
TYPE(TYPE_fdbk),INTENT(INOUT) :: fdbk

!!#### REQUIRED OUTPUT
!! * whether refinement is needed or not <Refined>
LOGICAL :: Refined

!!#### OPTIONAL INPUT
INTEGER,INTENT(IN),OPTIONAL :: Unit

!!#### LOCAL VARIABLES
INTEGER :: Unit_

!!--begin--
IF( PRESENT(Unit) )THEN
 Unit_ = Unit
ELSE
 Unit_ = OutputUnit(fdbK)
END IF
100 FORMAT(a8  ,1a18   ,2x,a8  ,2x, a13  ,1x ,2x,a8  )
200 FORMAT(i8.5,1es18.9,2x,i8.5,2x,es13.5,'%',2x,i8.5)

Refined = .FALSE.

!2. Special first iteration.
IF( iter==0 )THEN
 WRITE(Unit_,100)"iter","residmat","maxloc_relres","maxval_relres","max_iter"
END IF

!3. write out information
WRITE(Unit_,200)iter,residmat,maxloc_relres0,maxval_relres0*100.,iter_relres

!! Save last iteration's scalar fluxes
IF( (.NOT. ASSOCIATED(LastLO_ScalarFluxC)) .AND. (.NOT. ASSOCIATED(LastLO_ScalarFluxF)) .AND. (.NOT. ASSOCIATED(LastLO_CurrentFN)) )THEN              
    ALLOCATE( LastLO_ScalarFluxC(SIZE(LO_ScalarFluxC,1),SIZE(LO_ScalarFluxC,2)) )
    ALLOCATE( LastLO_ScalarFluxF(SIZE(LO_ScalarFluxF,1),SIZE(LO_ScalarFluxF,2)) )
    ALLOCATE( LastLO_CurrentFN(SIZE(LO_CurrentFN,1),SIZE(LO_CurrentFN,2)) )
END IF
LastLO_ScalarFluxC=LO_ScalarFluxC
LastLO_ScalarFluxF=LO_ScalarFluxF
LastLO_CurrentFN=LO_CurrentFN

!!--end--
END FUNCTION


!!### SUBROUTINE <<get_FV_QDData>>
SUBROUTINE get_FV_QDData( iter,LO_Mesh,&
  AvgExtSource,ExxC,EyyC,ExyC,ExxF,EyyF,ExyF,&
  Jbdry,Jdomn,C,ScalarFluxIN,CurrentIN,fdbk)
!!#### PURPOSE
!! Update finite volume-oriented data for the low-order
!! quasidiffusion solution.

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: iter

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Mesh),POINTER :: LO_Mesh
REAL(KIND_QDF) ,POINTER :: AvgExtSource(:,:)
REAL(KIND_QDF) ,POINTER :: ScalarFluxIN(:,:)
REAL(KIND_QDF) ,POINTER :: CurrentIN(:,:)
REAL(KIND_QDF) ,POINTER :: C(:,:)
REAL(KIND_QDF) ,POINTER :: ExxF(:,:)
REAL(KIND_QDF) ,POINTER :: EyyF(:,:)
REAL(KIND_QDF) ,POINTER :: ExyF(:,:)
REAL(KIND_QDF) ,POINTER :: ExxC(:,:)
REAL(KIND_QDF) ,POINTER :: EyyC(:,:)
REAL(KIND_QDF) ,POINTER :: ExyC(:,:)
INTEGER        ,POINTER :: JBdry(:),Jdomn(:)

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_="UpdateData"

!!#### LOCAL VARIABLES
LOGICAL :: DiffusionOverride_,EOverride_,COverride_

INTEGER :: i,g
LOGICAL :: NeedMapping
REAL(KIND_QDF),POINTER :: HI_ScalarFluxIN(:,:)
REAL(KIND_QDF),POINTER :: HI_CurrentIN(:,:)
REAL(KIND_QDF),POINTER :: HI_C(:,:)
REAL(KIND_QDF),POINTER :: HI_ExxF(:,:)
REAL(KIND_QDF),POINTER :: HI_EyyF(:,:)
REAL(KIND_QDF),POINTER :: HI_ExyF(:,:)
REAL(KIND_QDF),POINTER :: HI_KxxF(:,:)
REAL(KIND_QDF),POINTER :: HI_KyyF(:,:)
REAL(KIND_QDF),POINTER :: HI_KxyF(:,:)
INTEGER       ,POINTER :: HI_Jbdry(:)
INTEGER       ,POINTER :: HI_JDomn(:)
INTEGER       ,POINTER :: Inv_JBdry(:)

!!--begin--

!! Get this iteration's whether to override the calculation
!! with diffusion coefficients.
DiffusionOverride_ = SmartOption(.TRUE.,DiffusionOverride,INDEX=iter+1)
!Boundary factor override option
COverride_         = SmartOption(.FALSE.,COverride,INDEX=iter+1)
!Eddington factor override option
EOverride_         = SmartOption(.FALSE.,EOverride,INDEX=iter+1)



NULLIFY( HI_JBdry , HI_JDomn )
NULLIFY( HI_ScalarFluxIN , HI_CurrentIN , HI_C )
NULLIFY( HI_ExxF , HI_EyyF , HI_ExyF )
NULLIFY( HI_KxxF , HI_KyyF , HI_KxyF )

!check if we need a mapping of faces
NeedMapping = .NOT.ASSOCIATED(LO_Mesh,Mesh)

!set up mapping for boundary factors
IF( NeedMapping )THEN

 CALL UPDATE_BoundaryFaces(Mesh,JBdry=HI_Jbdry,JDomn=HI_JDomn)

 ALLOCATE( HI_ScalarFluxIN(Ng,SIZE(HI_Jbdry)) )
 ALLOCATE( HI_CurrentIN   (Ng,SIZE(HI_Jbdry)) )
 ALLOCATE( HI_C           (Ng,SIZE(HI_Jbdry)) )
ELSE
 HI_Jbdry        => JBdry
 HI_ScalarFluxIN => ScalarFluxIN
 HI_CurrentIN    => CurrentIN
 HI_C            => C
 HI_JDomn        => JDomn
END IF

!! Update the boundary factors.
CALL UPDATE_BoundaryFactors( &
  FactorGenerationStyle , &
  FactorEvalMethod , &
  AngularFluxF , &
  HI_ScalarFluxIN , HI_CurrentIN , HI_C , HI_Jbdry , AngularFluxV , &
  Ordinates, Weights, &
  Mesh , HI_JDomn , LOBC , fdbk ,&
  DiffusionOverride=DiffusionOverride_,&
  COverride=COverride_,&
  COverrideVals=COverrideVals)

!map boundary values to the LO_Mesh
IF( NeedMapping )THEN
 ALLOCATE( Inv_JBdry(SIZE(JBdry)) )
 CALL GET_Inv_JBdryE(Inv_JBdry,FaceMapping,JBdry,HI_JBdry)

 CALL MAPTO_CellBased_Boundary(LO_Mesh,&
   FaceMapping,JBdry,ScalarFluxIN,&
   HI_ScalarFluxIN,Inv_JBdry,HI_JBdry)
 CALL CLEAR( HI_ScalarFLuxIN )

 CALL MAPTO_CellBased_Boundary(LO_Mesh,&
   FaceMapping,JBdry,CurrentIN,&
   HI_CurrentIN,Inv_JBdry,HI_JBdry)
 CALL CLEAR( HI_CurrentIN )

 CALL MAPTO_CellBased_Boundary(LO_Mesh,&
   FaceMapping,JBdry,C,&
   HI_C,Inv_JBdry,HI_JBdry)
 CALL CLEAR( HI_C )
 CALL CLEAR( HI_Jbdry )
 CALL CLEAR( Inv_JBdry )
 CALL CLEAR( HI_JDomn )
ELSE
 NULLIFY( HI_ScalarFluxIN )
 NULLIFY( HI_CurrentIN )
 NULLIFY( HI_C )
 NULLIFY( HI_JBdry )
 NULLIFY( HI_JDomn )
END IF



IF( AnalyticTestCase/=0 )THEN

 CALL SETUP_AnalyticData(LO_Mesh,ExxF,EyyF,ExyF,ExxC,EyyC,ExyC)

ELSE

 IF( NeedMapping )THEN
  ALLOCATE( HI_ExxF(Ng,NUM_Faces(Mesh)) )
  ALLOCATE( HI_EyyF(Ng,NUM_Faces(Mesh)) )
  ALLOCATE( HI_ExyF(Ng,NUM_Faces(Mesh)) )
  ALLOCATE( HI_KxxF(Ng,NUM_Faces(Mesh)) )
  ALLOCATE( HI_KyyF(Ng,NUM_Faces(Mesh)) )
  ALLOCATE( HI_KxyF(Ng,NUM_Faces(Mesh)) )
 ELSE
  HI_ExxF => ExxF
  HI_EyyF => EyyF
  HI_ExyF => ExyF
  HI_KxxF => KxxF
  HI_KyyF => KyyF
  HI_KxyF => KxyF
 END IF

 !! Update the Eddington factors.
 CALL UPDATE_Eddingtons( FactorGenerationStyle , &
  FactorEvalMethod , &
  AngularFluxV , &
  AngularFluxF , &
  AngularFluxC , &
  EddingtonxxV,EddingtonyyV,EddingtonxyV,&
  HI_ExxF,HI_EyyF,HI_ExyF,&
     ExxC,   EyyC,   ExyC,&
     KxxV,   KyyV,   KxyV,&
  HI_KxxF,HI_KyyF,HI_KxyF,&
     KxxC,   KyyC,   KxyC,&
  Ordinates,Weights,&
  Mesh,fdbk,&
  DiffusionOverride=DiffusionOverride_,&
  EOverride=EOverride_,&
  EOverrideVals=EOverrideVals)

 IF( NeedMapping )THEN

  CALL MAPTO_CellBased(LO_Mesh,FaceMapping,LO_Interfaces,ExxF,HI_ExxF)
  CALL CLEAR( HI_ExxF )

  CALL MAPTO_CellBased(LO_Mesh,FaceMapping,LO_Interfaces,EyyF,HI_EyyF)
  CALL CLEAR( HI_EyyF )

  CALL MAPTO_CellBased(LO_Mesh,FaceMapping,LO_Interfaces,ExyF,HI_ExyF)
  CALL CLEAR( HI_ExyF )

  CALL MAPTO_CellBased(LO_Mesh,FaceMapping,LO_Interfaces,&
    KxxF,HI_KxxF)
  CALL CLEAR( HI_KxxF )

  CALL MAPTO_CellBased(LO_Mesh,FaceMapping,LO_Interfaces,&
    KyyF,HI_KyyF)
  CALL CLEAR( HI_KyyF )

  CALL MAPTO_CellBased(LO_Mesh,FaceMapping,LO_Interfaces,&
    KxyF,HI_KxyF)
  CALL CLEAR( HI_KxyF )

 ELSE
  NULLIFY( HI_ExxF )
  NULLIFY( HI_EyyF )
  NULLIFY( HI_ExyF )
  NULLIFY( HI_KxxF )
  NULLIFY( HI_KyyF )
  NULLIFY( HI_KxyF )

 END IF

END IF

!CALL CHECK_FaceMoments(Mesh,AngularFluxF,Ordinates,Weights,&
!  FaceMapping,LO_Mesh,ExxF,EyyF,ExyF,KxxF,KyyF,KxyF)

!get average source
IF( .NOT.ASSOCIATED(AvgExtSource) )THEN
 ALLOCATE( AvgExtSource(Ng,NUM_Cells(LO_Mesh)) )
END IF
DO i=1,NUM_Cells(LO_Mesh)
 DO g=1,SIZE(ExtSourceCellFunction,2)
  AvgExtSource(g,i) = EVAL_SourceAverage(Mesh,i,ExtSourceCellFunction(:,g,i))
 END DO
END DO
IF( Using_AnalyticTransportTest )THEN
 CALL UpdateAndDump(fdbk_comment,fdbk,&
   s="[[QDF]] Updating face-average first moments of &
   &the analytic test source.")
 IF( .NOT.ASSOCIATED(FaceAvgExtSource1) )THEN
  ALLOCATE( FaceAvgExtSource1(2,Ng,NUM_Faces(LO_Mesh)) )
 END IF
 CALL UPDATE_FaceAvgMom1QextA(LO_Mesh,FaceAvgExtSource1)
 FaceAvgExtSource1 = FaceAvgExtSource1/c_4_times_PI
END IF

!!--end--
END SUBROUTINE


!!### SUBROUTINE <<CHECK_FaceMoments>>
SUBROUTINE CHECK_FaceMoments(Mesh,AngularFluxF,Ordinates,Weights,&
  FaceMapping,LO_Mesh,ExxF,EyyF,ExyF,KxxF,KyyF,KxyF)

USE LIB_genMoments                                                 !!((13-B-LIB_genMoments.f90))
USE USR_TransportAnalyticTest,ONLY: EXACT_Exx,EXACT_SnExx,&        !!((56-C-USR_TransportAnalyticTest.f90))
  EXACT_SnKxx,EXACT_Kxx

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
REAL(KIND_AngularFlux),INTENT(IN) :: AngularFluxF(:,:,:)
REAL(KIND_QDF),INTENT(IN) :: Ordinates(:,:),Weights(:)
INTEGER,INTENT(IN) :: FaceMapping(:)
TYPE(TYPE_Mesh),INTENT(IN) :: LO_Mesh
REAL(KIND_Eddington),INTENT(IN) :: ExxF(:,:),EyyF(:,:),ExyF(:,:)
REAL(KIND_Eddington),INTENT(IN) :: KxxF(:,:),KyyF(:,:),KxyF(:,:)

!!#### LOCAL VARIABLES
INTEGER :: j,Ul1,Ul2,jh
REAL(KIND_MSH) :: x,y,FC(2),xh,yh
REAL(KIND_AngularFlux) :: Kstore(6),PhiStore,Kxx,Kyy,Kxy,Exx,Eyy,Exy
REAL(KIND_AngularFlux) :: Exact_SnKxxF,Exact_SnKxxFh
REAL(KIND_AngularFlux) :: Exact_SnExxF,Exact_SnExxFh
!!--begin--

!ensure
Ul1=NewFile("Fresh.txt")
DO j=1,NUM_Faces(Mesh)
 Kstore = Moment2( AngularFluxF(1,j,:) , Ordinates , Weights )
 PhiStore = Moment0( AngularFluxF(1,j,:) , Ordinates , Weights )
 Kxx = Kstore(1)
 Kyy = Kstore(2)
 Kxy = Kstore(4)
 Exx = Kxx/Phistore
 Eyy = Kyy/Phistore
 Exy = Kxy/Phistore
 FC = FaceCentroid(Mesh,j)
 x=FC(1) ; y=FC(2)
 WRITE(Ul1,"(i8,2f8.3,6Es19.9)")j,x,y,Kxx,Kyy,Kxy,Exx,Eyy,Exy
END DO
CLOSE(Ul1)

Ul2=NewFile("Store.txt")
DO j=1,NUM_Faces(LO_Mesh)
 FC = FaceCentroid(LO_Mesh,j)
 x=FC(1) ; y=FC(2)
 WRITE(Ul2,"(i8,2f8.3,6Es19.9)")j,x,y,KxxF(1,j),KyyF(1,j),KxyF(1,j),&
   ExxF(1,j),EyyF(1,j),ExyF(1,j)
END DO
CLOSE(Ul1)

Ul1=NewFile("Diff.txt")
DO j=1,NUM_Faces(LO_Mesh)
 jh = ABS(FaceMapping(j))
 Kstore = Moment2( AngularFluxF(1,jh,:) , Ordinates , Weights )
 PhiStore = Moment0( AngularFluxF(1,jh,:) , Ordinates , Weights )
 Kxx = Kstore(1)
 Kyy = Kstore(2)
 Kxy = Kstore(4)
 Exx = Kxx/Phistore
 Eyy = Kyy/Phistore
 Exy = Kxy/Phistore
 FC = FaceCentroid(Mesh,jh)
 xh=FC(1) ; yh=FC(2)
 FC = FaceCentroid(LO_Mesh,j)
 x=FC(1) ; y=FC(2)
 WRITE(Ul1,"(2i8,4f8.3,6Es19.9)")j,jh,x,xh,y,yh,&
   KxxF(1,j)-Kxx,KyyF(1,j)-Kyy,KxyF(1,j)-Kxy,&
   ExxF(1,j)-Exx,EyyF(1,j)-Eyy,ExyF(1,j)-Exy
END DO
CLOSE(Ul1)

Ul1=NewFile("Diff_Exx.txt")
WRITE(Ul1,"(2a8,4a8,12a19)")"j","jh","x","xh","y","yh",&
  "LO_EXACT_KxxF","LO_KxxF","HI_EXACT_KxxF","HI_KxxF",&
  "diff(LO_KxxF)","diff(HI_KxxF)",&
  "LO_EXACT_ExxF","LO_ExxF","HI_EXACT_ExxF","HI_ExxF",&
  "diff(LO_ExxF)","diff(HI_ExxF)"

DO j=1,NUM_Faces(LO_Mesh)
 jh = ABS(FaceMapping(j))
 Kstore = Moment2( AngularFluxF(1,jh,:) , Ordinates , Weights )
 PhiStore = Moment0( AngularFluxF(1,jh,:) , Ordinates , Weights )
 Kxx = Kstore(1)
 Kyy = Kstore(2)
 Kxy = Kstore(4)
 Exx = Kxx/Phistore
 Eyy = Kyy/Phistore
 Exy = Kxy/Phistore
 FC = FaceCentroid(Mesh,jh)
 xh=FC(1) ; yh=FC(2)
 FC = FaceCentroid(LO_Mesh,j)
 x=FC(1) ; y=FC(2)

 Exact_SnKxxF=FaceAverage_F(LO_Mesh,j,EXACT_SnKxx)
 Exact_SnKxxFh=FaceAverage_F(Mesh,jh,EXACT_SnKxx)
 Exact_SnExxF=FaceAverage_F(LO_Mesh,j,EXACT_SnExx)
 Exact_SnExxFh=FaceAverage_F(Mesh,jh,EXACT_SnExx)

 WRITE(Ul1,"(2i8,4f8.3,12Es19.9)")j,jh,x,xh,y,yh,&
   Exact_SnKxxF,KxxF(1,j),Exact_SnKxxFh,Kxx,&
   Exact_SnKxxF-KxxF(1,j),Exact_SnKxxFh-Kxx,&
   Exact_SnExxF,ExxF(1,j),Exact_SnExxFh,Exx,&
   Exact_SnExxF-ExxF(1,j),Exact_SnExxFh-Exx
END DO
CLOSE(Ul1)
!!--end--
END SUBROUTINE


!!### SUBROUTINE <<SOLVE_QuasiDiffusion>>
SUBROUTINE SOLVE_QuasiDiffusion( iter , dt0 , fdbk )

!!#### PURPOSE
!! Solve the quasi-diffusion problem, returning scalar fluxes and
!! currents.

!!#### REQUIRED INPUT/OUTPUT
INTEGER,INTENT(IN) :: iter
REAL,INTENT(INOUT) :: dt0

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_="SOLVE_QuasiDiffusion"

!!#### LOCAL VARIABLES
INTEGER               :: inform,Unit_resid
REAL                  :: tin,tout,dt,t1,t2
TYPE(varying_string)  :: file
INTEGER               :: g,Unit,i,j,jhi
REAL(KIND_QDF)        :: residx,relresidmat,LO_tol
LOGICAL               :: PassInftyNorm,PassL2Norm
REAL(KIND_MAC),POINTER :: COEFF_macs(:,:),COEFF_mact(:,:)
! REAL(KIND_MAC),POINTER :: PScalarFluxC(:,:)
! REAL(KIND_MAC),POINTER :: PScalarFluxF(:,:)
! REAL(KIND_MAC),POINTER :: PCurrentFN(:,:)
REAL(KIND_MAC),POINTER :: SAVE_AvgExtSource(:,:)
INTEGER               :: subiter
LOGICAL :: pure_acceleration

!!--begin--

!start the timer
CALL CPU_TIME(tin)

!============
!++UPDATE++
!============

!only one of these at most can be true (maybe none)
pure_acceleration=.false.
IF( pure_acceleration .AND. System/=QDF_System05 )THEN
    CALL UpdateAndDump(fdbk_error,fdbk,s='[[QDF]] pure acceleration can only be used with \QDFsystem{05}')
END IF

!overall setting
NULLIFY( COEFF_macs,COEFF_mact )
NULLIFY( SAVE_AvgExtSource )
!NULLIFY(PCurrentFN,PScalarFluxC,PScalarFluxF )

!0.1 get the data
CALL get_FV_QDData(iter,LO_Mesh,&
  AvgExtSource,EddingtonxxC,EddingtonyyC,EddingtonxyC,&
  EddingtonxxF,EddingtonyyF,EddingtonxyF,& !output
  Jbdry,Jdomn,CBoundary,ScalarFluxIN,CurrentIN,fdbk)   !output

!0.2 form the coefficient matrix and right-hand side
SELECT CASE( System )

 CASE( QDF_System01 )
   CALL UPDATE_QDF01( A , b , LO_Mesh , mact , macs , l_ , &
     AvgExtSource , EddingtonxxC , EddingtonyyC , EddingtonxyC , EddingtonxxF , EddingtonyyF , EddingtonxyF , Jbdry , Jdomn , CBoundary , &
     ScalarFluxIN , CurrentIN , BC , LOBC , FixBdryVals , fdbk , Noisy,Unit_Noisy,Interactive)

 CASE( QDF_System02 )
   CALL UPDATE_QDF02( A , b , LO_Mesh , mact , macs , l_ , &
     AvgExtSource , EddingtonxxC , EddingtonyyC , EddingtonxyC , EddingtonxxF , EddingtonyyF , EddingtonxyF , Jbdry , Jdomn , CBoundary , &
     ScalarFluxIN , CurrentIN , BC , LOBC , FixBdryVals , fdbk , Noisy,Unit_Noisy,Interactive)

 CASE( QDF_System03 )
   CALL UPDATE_QDF03( A , b , LO_Mesh , mact , macs , l_ , &
     AvgExtSource , EddingtonxxC , EddingtonyyC , EddingtonxyC , EddingtonxxF , EddingtonyyF , EddingtonxyF , Jbdry , Jdomn , CBoundary , &
     ScalarFluxIN , CurrentIN , BC , LOBC , FixBdryVals , fdbk , Noisy,Unit_Noisy,Interactive)

 CASE( QDF_System04 )
   CALL UPDATE_QDF04( A , b , LO_Mesh , mact , macs , l_ , &
     AvgExtSource , EddingtonxxC , EddingtonyyC , EddingtonxyC , EddingtonxxF , EddingtonyyF , EddingtonxyF , Jbdry , Jdomn , CBoundary , &
     ScalarFluxIN , CurrentIN , BC , LOBC , FixBdryVals , fdbk , Noisy,Unit_Noisy,Interactive, &
     ExtSourceF1=FaceAvgExtSource1 )

 CASE( QDF_System05 )
            
    IF( iter>0 .AND. pure_acceleration .AND. ASSOCIATED(ScalarFluxC) .AND. ASSOCIATED(LastLO_ScalarFluxC) )THEN
        CALL UpdateAndDump(fdbk_comment,fdbk,s='[[QDF]] initializing AvgExtSource for pure acceleration')
        ALLOCATE( SAVE_AvgExtSource(1,NUM_Cells(LO_Mesh)) )                    
        SAVE_AvgExtSource=AvgExtSource
        ScalarFluxIN=0.d0
        CurrentIN=0.d0
        DO i=1,NUM_Cells(LO_Mesh)
            DO g=1,SIZE(ScalarFluxC,1)
                AvgExtSource=macs(g,l_(i))*(ScalarFluxC(g,i)-LastLO_ScalarFluxC(g,i))/c_4_times_PI
            END DO
        END DO
    END IF

   CALL UPDATE_QDF05( A , b , LO_Mesh , mact , macs , l_ , &
     AvgExtSource , EddingtonxxC , EddingtonyyC , EddingtonxyC , EddingtonxxF , EddingtonyyF , EddingtonxyF , Jbdry , Jdomn , CBoundary , &
     ScalarFluxIN , CurrentIN , BC , LOBC , FixBdryVals , COEFF_macs, COEFF_mact, fdbk , Noisy,Unit_Noisy,Interactive, &
     ExtSourceF1=FaceAvgExtSource1 )

END SELECT

!end the timer
CALL CPU_TIME(tout)
dt = tout-tin

CALL UpdateAndDump(fdbk_comment,fdbk,s="[[QDF]] updating coefficient matrix &
  &completed in [time="//TRIM(STRTIME(dt))//"]")

!update global
dt0 = dt0 + dt


!============
!++SOLUTION++
!============

!begin another timer
CALL CPU_TIME(tin)

!1. check if the coeficient matrix is okay
CALL UpdateAndDump(fdbk_comment,fdbk,s="[[QDF]] Checking sparse matrix: the &
  &coefficient matrix is <"//TRIM(MERGE("GOOD","BAD ",Is_ok(A)))//">.")

65 CONTINUE

!**************
!CALL ConsistencyFix_Matrix(iter,A)
!**************

!4.a. Direct solution.
IF( Solver==QDF_Direct )THEN

 SELECT CASE( SubSolver )
  CASE( QDF_LAPACK_denseGauss ) ; CALL SOLVE_LAPACK_dense( A , b , x , fdbk )
  CASE( QDF_SMLIB_bandedGauss ) ; CALL SOLVE_SMLIB_banded( A , b , x , fdbk )
  CASE( QDF_nspiv )             ; CALL SOLVE_nspiv       ( A , b , x , fdbk )
 END SELECT

!4.b. Iterative solution.
ELSE IF( Solver==QDF_Iterative )THEN

 SELECT CASE( SubSolver )
  CASE( QDF_Botchev_BiCGstab2 ) ; CALL SOLVE_bicgstab2(A,b,x,fdbk)

  CASE( QDF_SPARSKIT_BiCGstab ) ; CALL SOLVE_SPARSKIT("BCGSTAB",A,b,x,fdbk)
  CASE( QDF_SPARSKIT_CG       ) ; CALL SOLVE_SPARSKIT("CG",A,b,x,fdbk)
  CASE( QDF_SPARSKIT_CGNR     ) ; CALL SOLVE_SPARSKIT("CGNR",A,b,x,fdbk)
  CASE( QDF_SPARSKIT_BCG      ) ; CALL SOLVE_SPARSKIT("BCG",A,b,x,fdbk)
  CASE( QDF_SPARSKIT_DBCG     ) ; CALL SOLVE_SPARSKIT("DBCG",A,b,x,fdbk)
  CASE( QDF_SPARSKIT_TFQMR    ) ; CALL SOLVE_SPARSKIT("TFQMR",A,b,x,fdbk)
  CASE( QDF_SPARSKIT_FOM      ) ; CALL SOLVE_SPARSKIT("FOM",A,b,x,fdbk)
  CASE( QDF_SPARSKIT_GMRES    ) ; CALL SOLVE_SPARSKIT("GMRES",A,b,x,fdbk)
  CASE( QDF_SPARSKIT_FGMRES   ) ; CALL SOLVE_SPARSKIT("FGMRES",A,b,x,fdbk)
  CASE( QDF_SPARSKIT_DQGMRES  ) ; CALL SOLVE_SPARSKIT("DQGMRES",A,b,x,fdbk)

  CASE( QDF_SMLIB_GMRES       ) ; CALL SOLVE_SMLIB_GMRES(A,b,x,fdbk)
  CASE( QDF_SMLIB_BiCGStab    ) ; CALL SOLVE_SMLIB_BiCGStab(A,b,x,fdbk)
 END SELECT

END IF
CALL Dump(fdbk)


!4. Output matrix options.
IF( OutputCoefficientMatrix )THEN
 CALL UpdateAndDump(fdbk_comment,fdbk,s='[[QDF]] Outputting the sparse &
   &coefficient matrix and right-hand side vector to files &
   &"A*.dat" and "b*.dat" (in directory "data") where * is &
   &the iteration number (3-digit integer.)  The sparse &
   &coefficient matrix <A> is in a three column &
   &(space-delimited) coordinate format. You may use &
   &<A=spconvert(load("A.dat"))> to load the data in MATLAB.')
 CALL OUTPUT_QDF( OutputFileBase , iter , A , x , b )
END IF


!=============
!++RESIDUALS++
!=============

!! 5.a. Calculate residual from solution.

ALLOCATE( res(1:SIZE(x)) )
res = (A*x - b)
residmat = NormInfty( res )
residx   = NormInfty( x )
CALL UpdateAndDump(fdbk_comment,fdbk,s="[[QDF]] the solution vector &
  & has infinity-norm [residx="//TRIM(STR(residx,"(Es14.4)"))//"]")
relresidmat = residmat/residx

CALL UpdateAndDump(fdbk_comment,fdbk,s="[[QDF]] evaluating norm. matrix residuals: &
  &[AbsInftyNorm="//TRIM(STR(residmat,"(Es14.4)"))//"] and &
  &[RelInftyNorm="//TRIM(STR(relresidmat,"(Es14.4)"))//"]")

PassInftyNorm = residmat<=tolPhiVInf
IF( PassInftyNorm )THEN
 CALL UpdateAndDump(fdbk_comment,fdbk,s="[[QDF]] solution InftyNorm residual [check=PASS]")
ELSE
 CALL UpdateAndDump(fdbk_warning,fdbk,s="[[QDF]] solution InftyNorm residual [check=FAIL]")
END IF

!5.a. L2Norm check
residmat = NormEll2( res )
residx   = NormEll2( x )
CALL UpdateAndDump(fdbk_comment,fdbk,s="[[QDF]] the solution vector &
  & has ell2-norm [residx="//TRIM(STR(residx,"(Es14.4)"))//"]")
relresidmat = residmat/residx

CALL UpdateAndDump(fdbk_comment,fdbk,s="[[QDF]] evaluating norm. matrix residual: &
  &[AbsL2Norm="//TRIM(STR(residmat,"(Es14.4)"))//"] and &
  &[RelL2Norm="//TRIM(STR(relresidmat,"(Es14.4)"))//"]")

PassL2Norm = residmat<=tolPhiVInf
IF( PassL2Norm )THEN
 CALL UpdateAndDump(fdbk_comment,fdbk,s="[[QDF]] solution L2Norm residual [check=PASS]")
ELSE
 CALL UpdateAndDump(fdbk_warning,fdbk,s="[[QDF]] solution L2Norm residual [check=FAIL]")
END IF


!! 5.b. Allocate.
ALLOCATE( relres(1:SIZE(x)) )

!! 5.c. Residual unit.
IF( PrintResiduals )THEN
 file = TRIM(OutputFileBase)//"__ResidLo_%p"
 file = Substitute( TRIM(STR(file)) , (/"%p"/) , (/iter/) )
 file = TRIM(STR(file))//".txt"
 Unit_resid = NewFile(TRIM(STR(file)),STATUS="Replace",IfOpened="R")
ELSE
 Unit_resid = 0
END IF

!! 5.d. Calculate residual from equations.
SELECT CASE( System )

 CASE( QDF_System01 )
   CALL RESIDUAL_QDF01( res , relres , x , iter , LO_Mesh , mact , macs , l_ , &
     AvgExtSource , EddingtonxxC , EddingtonyyC , EddingtonxyC , EddingtonxxF , EddingtonyyF , EddingtonxyF , Jbdry , Jdomn , CBoundary , &
     ScalarFluxIN , CurrentIN , BC , LOBC , FixBdryVals , fdbk , Noisy,Unit_Noisy,Interactive,Unit_resid)

 CASE( QDF_System02 )
   CALL RESIDUAL_QDF02( res , relres , x , iter , LO_Mesh , mact , macs , l_ , &
     AvgExtSource , EddingtonxxC , EddingtonyyC , EddingtonxyC , EddingtonxxF , EddingtonyyF , EddingtonxyF , Jbdry , Jdomn , CBoundary , &
     ScalarFluxIN , CurrentIN , BC , LOBC , FixBdryVals , fdbk , Noisy,Unit_Noisy,Interactive,Unit_resid)

 CASE( QDF_System03 )
   CALL RESIDUAL_QDF03( res , relres , x , iter , LO_Mesh , mact , macs , l_ , &
     AvgExtSource , EddingtonxxC , EddingtonyyC , EddingtonxyC , EddingtonxxF , EddingtonyyF , EddingtonxyF , Jbdry , Jdomn , CBoundary , &
     ScalarFluxIN , CurrentIN , BC , LOBC , FixBdryVals , fdbk , Noisy,Unit_Noisy,Interactive,Unit_resid)

 CASE( QDF_System04 )
   CALL RESIDUAL_QDF04( res , relres , x , iter , LO_Mesh , mact , macs , l_ , &
     AvgExtSource , EddingtonxxC , EddingtonyyC , EddingtonxyC , EddingtonxxF , EddingtonyyF , EddingtonxyF , Jbdry , Jdomn , CBoundary , &
     ScalarFluxIN , CurrentIN , BC , LOBC , FixBdryVals , fdbk , Noisy,Unit_Noisy,Interactive,Unit_resid,&
     ExtSourceF1=FaceAvgExtSource1)

 CASE( QDF_System05 )
   CALL RESIDUAL_QDF05( res , relres , x , iter , LO_Mesh , mact , macs , l_ , &
     AvgExtSource , EddingtonxxC , EddingtonyyC , EddingtonxyC , EddingtonxxF , EddingtonyyF , EddingtonxyF , Jbdry , Jdomn , CBoundary , &
     ScalarFluxIN , CurrentIN , BC , LOBC , FixBdryVals , COEFF_macs, COEFF_mact, fdbk , Noisy,Unit_Noisy,Interactive,Unit_resid,&
     ExtSourceF1=FaceAvgExtSource1)

END SELECT

!! 5.c. Save important residual info.
CALL SaveResidualInfo(iter,res,relres,&
  maxloc_res,maxval_res,iter_res,&
  maxloc_relres,maxval_relres,iter_relres,&
  maxloc_relres0,maxval_relres0,&
  maxloc_res0,maxval_res0,fdbk)


!! 5.d. Deallocate.
DEALLOCATE( res,relres )



!==================
!++RECONSTRUCTION++
!==================

!! 6. Query system for scalar fluxes and currents.
SELECT CASE( System )
 CASE( QDF_System01 )
   CALL RECALL_QDF01( x , LO_ScalarFluxC , LO_ScalarFluxF , LO_CurrentFN )
 CASE( QDF_System02 )
   CALL RECALL_QDF02( x , LO_ScalarFluxC , LO_ScalarFluxF , LO_CurrentFN )
 CASE( QDF_System03 )
   CALL RECALL_QDF03( x , LO_ScalarFluxC , LO_ScalarFluxF , LO_CurrentFN )
 CASE( QDF_System04 )
   CALL RECALL_QDF04( x , LO_ScalarFluxC , LO_ScalarFluxF , LO_CurrentFN )
 CASE( QDF_System05 )
   CALL RECALL_QDF05( x , LO_ScalarFluxC , LO_ScalarFluxF , LO_CurrentFN )
END SELECT

IF( iter>0 .AND. pure_acceleration .AND. ASSOCIATED(ScalarFluxC) .AND. ASSOCIATED(LastLO_ScalarFluxC) )THEN
        !update low data
        CALL MAP_HI2LO(LO_Mesh,JBdry,FaceMapping,LO_Interfaces,&
            ScalarFluxC,   ScalarFluxF,   CurrentFN,&
        LastLO_ScalarFluxC,LastLO_ScalarFluxF,LastLO_CurrentFN &
        )
        LO_ScalarFluxC=LastLO_ScalarFluxC+LO_ScalarFluxC
        LO_ScalarFluxF=LastLO_ScalarFluxF+LO_ScalarFluxF
        LO_CurrentFN  =LastLO_CurrentFN  +LO_CurrentFN
        AvgExtSource=SAVE_AvgExtSource
        DEALLOCATE(SAVE_AvgExtSource)
END IF



!=====================
!++SOURCE GENERATION++
!=====================

CALL msg_UPDATE_CellFunction(CellFunctionMethod,QDF_KEY_CellFunction,&
  NonlinearFixup,QDF_KEY_NonlinearFixup,fdbk)

CALL UPDATE_CellFunction(LO_Mesh,LO_ScalarFluxC,LO_ScalarFluxF,ScalarFluxCellFunction,&
Method=QDF_KEY_CellFunction(CellFunctionMethod),&
NonlinearFixUp=QDF_KEY_NonlinearFixup(NonlinearFixup))

CALL UpdateAndDump(fdbk_comment,fdbk,s="[[QDF]] QuasiDiffusion is updating the total TotSourceCellFunction...")

!because scattering is isotropic, just use ScalarFluxCellFunction as the sole
!moment to pass
CALL UPDATE_TotSource_1Mom( LO_Mesh,Ng,ExtSourceCellFunction , &
    CoeffScalarFluxM, l_ , ScalarFluxCellFunction , &
    TotSourceCellFunction ) !output

!Check if the source is positive.
CALL CHECK_TotSourcePositive(LO_Mesh,TotSourceCellFunction,fdbk)

!check the balance equation.
CALL CHECK_ScalarBalanceEquation(LO_Mesh,&
    LO_ScalarFluxC,LO_CurrentFN,c_4_times_PI*TotSourceCellFunction,&
    MacT,l_,fdbk,reltol=tolBalRel,abstol=tolBalAbs,caller="[[QDF]]")


!==========
!++WRAPUP++
!==========

!end time
CALL CPU_TIME(tout)
dt = (tout-tin)

!timing update
CALL UPDATE(fdbk_profile,fdbk,s="[[QDF]] LO solve completed in [time="//&
  TRIM(STRTIME(dt))//"]")

!dump feeback
CALL DUMP(fdbk)

!update global time
dt0 = dt0 + dt

IF( OutputCoefficientMatrix )THEN
 !3. Print solution (.dat appended automatically with Save).
 file = Substitute( "x_%p" , (/"%p"/) , (/iter/) )
 CALL Save( TRIM(STR(file)) , x )
END IF

!!--end--
END SUBROUTINE



SUBROUTINE MAP_HI2LO(LO_Mesh,JBdry,FaceMapping,Interfaces,&
     ScalarFluxC,   ScalarFluxF,   CurrentFN,&
  LO_ScalarFluxC,LO_ScalarFluxF,LO_CurrentFN &
)
TYPE(TYPE_Mesh)      ,POINTER :: LO_Mesh
INTEGER              ,POINTER :: JBdry(:)
INTEGER              ,POINTER :: FaceMapping(:)
TYPE(TYPE_Interfaces),POINTER :: Interfaces(:)
REAL(KIND_MSH)       ,INTENT(IN) :: ScalarFluxC(:,:),ScalarFluxF(:,:),CurrentFN(:,:) !input
REAL(KIND_MSH)       ,POINTER :: LO_ScalarFluxC(:,:),LO_ScalarFluxF(:,:),LO_CurrentFN(:,:) !output

INTEGER :: n,j,Ns,s,je,m,Njlo,Ni,Njb,jhi,jb,i

!!--begin--

LO_ScalarFluxC = ERROR(1._KIND_MSH)
LO_ScalarFluxF = ERROR(1._KIND_MSH)
LO_CurrentFN = ERROR(1._KIND_MSH)
Njlo=SIZE(ScalarFluxF,2)
Ni=SIZE(ScalarFluxC,2)
Njb=SIZE(JBdry)

DO i=1,Ni
    LO_ScalarFluxC(:,i)=ScalarFluxC(:,i)
END DO

!first do the boundary faces
DO jb=1,Njb
    j = JBdry(jb)
    jhi = FaceMapping(j)
    LO_ScalarFluxF(:,j) = ScalarFluxF(:,jhi)
    LO_CurrentFN(:,j) = CurrentFN(:,jhi)
END DO


!map all the subfaces
DO m=1,SIZE(Interfaces)
    n=Interfaces(m)%master
    IF( ASSOCIATED(Interfaces(m)%subs) )THEN
        Ns=SIZE(Interfaces(m)%subs)
        DO s=1,Ns
            j = Interfaces(m)%subs(s)
            jhi = FaceMapping(j)
            LO_ScalarFluxF(:,j) = ScalarFluxF(:,ABS(jhi))
            LO_CurrentFN(:,j) = SIGN(1,jhi)*CurrentFN(:,ABS(jhi))
        END DO
    END IF
END DO

!map master faces
DO m=1,SIZE(Interfaces)
    n=Interfaces(m)%master
    IF( ASSOCIATED(Interfaces(m)%subs) )THEN
        Ns=SIZE(Interfaces(m)%subs)
        LO_ScalarFluxF(:,n) = 0.d0
        LO_CurrentFN(:,n) = 0.d0
        DO s=1,Ns
            j = Interfaces(m)%subs(s)
            LO_ScalarFluxF(:,n) = LO_ScalarFluxF(:,n) + LO_ScalarFluxF(:,j)*FaceArea(LO_Mesh,j)
            LO_CurrentFN(:,n) = LO_CurrentFN(:,n) + LO_CurrentFN(:,j)*FaceArea(LO_Mesh,j)
        END DO
        LO_ScalarFluxF(:,n) = LO_ScalarFluxF(:,n)/FaceArea(LO_Mesh,n)
        LO_CurrentFN(:,n) = LO_CurrentFN(:,n)/FaceArea(LO_Mesh,n)
    END IF
END DO

!!--end--
END SUBROUTINE

SUBROUTINE msg_UPDATE_CellFunction(CellFunctionMethod,QDF_KEY_CellFunction,&
                                   NonlinearFixup,QDF_KEY_NonlinearFixup,fdbk)
INTEGER,INTENT(IN) :: CellFunctionMethod,NonlinearFixup
CHARACTER(*),INTENT(IN) :: QDF_KEY_CellFunction(:)
CHARACTER(*),INTENT(IN) :: QDF_KEY_NonlinearFixup(:)
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk
!!--begin--
IF( CellFunctionMethod>=1 .AND. CellFunctionMethod<=SIZE(QDF_KEY_CellFunction) )THEN
 CALL UpdateAndDump(fdbk_comment,fdbk,s="[[QDF]] Updating the total &
   &ScalarFluxCellFunction from LO_ScalarFluxC and LO_ScalarFluxF using [method="//&
   TRIM( QDF_KEY_CellFunction(CellFunctionMethod) )//"]")
ELSE
 CALL UpdateAndDump(fdbk_error,fdbk,s="[[QDF]] the CellFunctionMethod is not set.")
END IF
IF( NonlinearFixup>=1 .AND. NonlinearFixup<=SIZE(QDF_KEY_NonlinearFixup) )THEN
 CALL UpdateAndDump(fdbk_comment,fdbk,s="[[QDF]] Fixing up the total &
   &ScalarFluxCellFunction from LO_ScalarFluxC and LO_ScalarFluxF using [method="//&
   TRIM( QDF_KEY_NonlinearFixup(NonlinearFixup) )//"]")
ELSE
 CALL UpdateAndDump(fdbk_error,fdbk,s="[[QDF]] the NonlinearFixup is not recognized.")
END IF
!!--end--
END SUBROUTINE

SUBROUTINE WRAPUP_QuasiDiffusion( fdbk )
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk
!!--begin--
CALL DEALLOCATE_Matrix( A )
DEALLOCATE( x , b )
!!--end--
END SUBROUTINE


END MODULE

