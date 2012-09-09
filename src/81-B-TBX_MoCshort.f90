!!# TOOLBOX MODULE <<TBX_MoCshort>>
MODULE TBX_MoCshort

!!## PURPOSE
!! The toolbox of routines for the Method of Characteristics with
!! short (intra-cell) interpolation.

!!## VERSION 2.23

!!## HISTORY
!! * v2.23 - Corrected EVAL_ReflectiveBC2 for double reflective boundary problems
!!           at corners.

!!## EXTERNAL KINDS
USE KND_MoCshort         ,ONLY: &                           !!((03-A-KND_MoCshort.f90))
  KIND_ScalarFlux,KIND_AngularFlux,KIND_ExtSource
USE KND_Mesh             ,ONLY: KIND_MSH                    !!((05-B-KND_Mesh.f90))
USE KND_XSExpansion                                         !!((02-A-KND_XSExpansion.f90))
USE KND_DiscreteOrdinates,ONLY: KIND_DOR                    !!((02-A-KND_DiscreteOrdinates.f90))

!!## FORTRAN STANDARDS
USE ISO_varying_string                                      !!((03-A-ISO_varying_string.f90))

!!## GLOBAL USER MODULES
!! * feedback needed all over
USE USR_fdbk                                                !!((08-C-USR_fdbk.f90))

!!## EXTERNAL PARAMETERS
USE PAR_MoCshort                                            !!((03-A-PAR_MoCshort.f90))
USE PAR_Constants_Rdp   ,ONLY: c_4_times_PI                 !!((02-A-PAR_Constants_Rdp.f90))

!!## GLOBAL SOLVERS
USE SLV_laVandermonde                                       !!((04-B-SLV_laVandermonde.f90))

!!## GLOBAL USER MODULES
USE USR_DiscreteOrdinates                                   !!((34-B-USR_DiscreteOrdinates.f90))
USE USR_TransportAnalyticTest                               !!((56-C-USR_TransportAnalyticTest.f90))
USE USR_pThread                                             !!((03-A-USR_pThread.f90))
USE USR_MoCshort                                            !!((48-C-USR_MoCshort.f90))
USE USR_SimpleSet                                           !!((12-B-USR_SimpleSet.f90))
USE USR_SBCharacteristics                                   !!((26-C-USR_SBCharacteristics.f90))
USE USR_Source                                              !!((35-B-USR_Source.f90))
USE USR_TAPACK                                              !!((48-C-USR_TAPACK.f90))

!!## EXTERNAL PROCEDURES
USE FUN_Default                                             !!((04-A-FUN_Default.f90))
USE FUN_Jiggle                                              !!((05-B-FUN_Jiggle.f90))
USE FUN_Interp1S_Linear                                     !!((05-B-FUN_Interp1S_Linear.f90))
USE FUN_InterpGrid2S_CShep,&                                !!((06-B-FUN_InterpGrid2S_CShep.f90))
  InterpGrid2S=>InterpGrid2S_CShep
USE FUN_CellGradient_Gauss                                  !!((03-A-FUN_CellGradient_Gauss.f90))
USE FUN_STR                                                 !!((05-B-FUN_STR.f90))
USE FUN_VSTR                                                !!((05-B-FUN_VSTR.f90))
USE FUN_Random                                              !!((03-A-FUN_Random.f90))
USE FUN_Sequence                                            !!((03-A-FUN_Sequence.f90))
USE FUN_TimeStamp                                           !!((05-B-FUN_TimeStamp.f90))
USE FUN_Warning                                             !!((04-B-FUN_Warning.f90))
USE FUN_xyDIST                                              !!((04-B-FUN_xyDIST.f90))
USE FUN_SIZEa                                               !!((06-B-FUN_SIZEa.f90))
USE SUB_Pause                                               !!((04-B-SUB_Pause.f90))
USE SUB_Sort_quick,&                                        !!((03-A-SUB_Sort_quick.f90))
  ONLY: Sort=>Sort_quick
USE SUB_Reallocate                                          !!((04-B-SUB_Reallocate.f90))
USE FUN_IsError                                             !!((05-A-FUN_IsError.f90))
USE FUN_NEARLOC                                             !!((06-A-FUN_NEARLOC.f90))
USE SUB_Stop                                                !!((04-B-SUB_Stop.f90))
USE FUN_Substitute                                          !!((06-C-FUN_Substitute.f90))
USE SUB_genQuadrature_FROM_lev                              !!((07-B-SUB_genQuadrature_FROM_lev.f90))
USE SUB_genQuadrature_FROM_prod                             !!((07-B-SUB_genQuadrature_FROM_prod.f90))
USE FUN_Integrate1_aq                                       !!((06-B-FUN_Integrate1_aq.f90))
USE FUN_STRTIME                                             !!((06-C-FUN_STRTIME.f90))
USE FUN_NewUnit                                             !!((04-B-FUN_NewUnit.f90))
USE FUN_Reorder                                             !!((05-A-FUN_Reorder.f90))

!!## GLOBAL PRINTING
USE PRN_Function1                                           !!((05-B-PRN_Function1.f90))
USE PRN_Text                                                !!((07-B-PRN_Text.f90))
USE PRN_MoCshort                                            !!((77-B-PRN_MoCshort.f90))

!!## GLOBAL TOOLBOXES
USE TBX_DiscreteOrdinates                                   !!((56-B-TBX_DiscreteOrdinates.f90))
USE TBX_ComputationalGeometry                               !!((09-A-TBX_ComputationalGeometry.f90))
USE PAR_Mesh
USE USR_Mesh
USE TBX_Mesh,ONLY: &                                        !!((15-B-TBX_Mesh.f90))
 IsBoundaryFace,DomainFace,FaceArea,MAX_FacesPerCell,&
 IsCellBased,DomainFaceNormal,&
 IntersectFace,IsBoundaryVert,IntersectFaces_Omega,&
 EVAL_CellTraversal,SETUP_meshcelliterator,SliceCell,&
 GET_MemberCells,BoundaryFacesFromVert,SETUP_VertToFaceLink

USE TBX_Materials                                           !!((53-B-TBX_Materials.f90))

!!## GLOBAL LIBRARIES
USE LIB_Prompts                                             !!((06-B-LIB_Prompts.f90))
USE LIB_GenericPhrases                                      !!((07-B-LIB_GenericPhrases.f90))
USE LIB_Norm                                                !!((04-B-LIB_Norm.f90))
USE LIB_genMoments                                          !!((13-B-LIB_genMoments.f90))
USE INT_LAPACK2                                             !!((08-B-INT_LAPACK2.f90))
USE USR_SimpleList                                          !!((09-B-USR_SimpleList.f90))

!!## GLOBAL VARIABLES
USE VAR_MoCshort                                            !!((47-B-VAR_MoCshort.f90))
USE VAR_Materials                                           !!((48-B-VAR_Materials.f90))
USE VAR_XSMonkey                                            !!((47-B-VAR_XSMonkey.f90))
USE VAR_Mesh                                                !!((46-B-VAR_Mesh.f90))
USE VAR_EnergyGroups                                        !!((47-B-VAR_EnergyGroups.f90))
USE VAR_Source                                              !!((47-B-VAR_Source.f90))
USE VAR_TAPACK                                              !!((66-C-VAR_TAPACK.f90))
USE VAR_Units                                               !!((03-A-VAR_Units.f90))
USE VAR_FindMe                                              !!((03-C-VAR_FindMe.f90))
USE VAR_ScalarFluxes                                        !!((04-C-VAR_ScalarFluxes.f90))
USE VAR_Currents                                            !!((04-C-VAR_Currents.f90))
USE VAR_Source                                              !!((47-B-VAR_Source.f90))
USE VAR_AngularFluxes                                       !!((04-C-VAR_AngularFluxes.f90))
USE VAR_QDAnalyticTest                                      !!((33-C-VAR_QDAnalyticTest.f90))
USE VAR_DiscreteOrdinates,ONLY: Azimuthal_Angles,Polar_Angles

!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## DEFAULT ACCESS
PRIVATE

!!## ACCESS
PUBLIC :: SETUP_MoCshort
PUBLIC :: SOLVE_MoCshort
PUBLIC :: WRAPUP_MoCshort
PUBLIC :: REFINE_MocSHort
PUBLIC :: SOURCE_UPDATE_MoCshort
PUBLIC :: RayEffectsStatistics_2D

!!## MODULE PROCEDURES
CONTAINS



!!### SUBROUTINE <SETUP_ExtraVarsAndBC>
SUBROUTINE SETUP_ExtraVarsAndBC( Mesh , AngularFluxV , Ordinates , &
  WithinCell , NearestFace , k_ , fdbk )

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh) ,INTENT(INOUT) :: Mesh
REAL(KIND_AngularFlux),POINTER :: AngularFluxV(:,:,:)
REAL(KIND_DOR)  ,POINTER       :: Ordinates(:,:)
INTEGER         ,POINTER       :: WithinCell(:,:),NearestFace(:,:),k_(:,:,:)
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### LOCAL VARIABLES
INTEGER        :: Nk,m
REAL(KIND_DOR) :: U(2)
!!--begin--

Nk = NUM_Verts(Mesh)

!current allocation and initialization
CALL TAP_ALLOCATE_P3("CurrentV",CurrentV,(/Mesh%NDim,Ng,Nk/),fdbk,init=0.d-20)

!and then allocate the spatial flux variable
CALL TAP_ALLOCATE_P2("ScalarFluxV",ScalarFluxV,(/Ng,Nk/),fdbk,init=1.d-20)

ALLOCATE( LastScalarFluxV(1:Ng,1:Nk) )
LastScalarFluxV = 0.d0

!allocate and initialize angular flux
CALL SETUP_AngularFlux( AngularFluxV , WithinCell , MacT , MacS , fdbk )

!get azimuthal angles
CALL UpdateAndDump(fdbk_warning,fdbk,s="[[MCS]] Azimuthal and polar angles from the ordinates &
  &(required for precise slicing) are being approximated.")
ALLOCATE( Azimuthal_Angles(SIZE(Ordinates,2)) )
ALLOCATE( Polar_Angles(SIZE(Ordinates,2)) )
DO m=1,SIZE(Ordinates,2)
    !get XY direction
    U = xyDIRECTION_V( Ordinates(1:2,m) )
    Azimuthal_Angles(m)=xyANGLE_UU((/1.0_KIND_DOR,0.0_KIND_DOR/),U)
    Polar_Angles(m)=ACOS(Ordinates(3,m))
END DO

!incorporate boundary conditions
CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MCS]] Boundary conditions are &
  &being incorporated into the [AngularFlux] object.")

!CALL SETUP_BoundaryConditions_2Dgen ( Mesh , AngularFluxV , Ordinates , WithinCell )

!!--end--
END SUBROUTINE



!!### SUBROUTINE <<SETUP_ExtraStuff>>
SUBROUTINE SETUP_ExtraStuff( Ordinates , fdbk )

!!#### MODULES
USE USR_GraphTraversal                                      !!((06-C-USR_GraphTraversal.f90))
USE VAR_EnergyGroups,ONLY: Ng                               !!((47-B-VAR_EnergyGroups.f90))


!!#### REQUIRED INPUT
REAL(KIND_DOR)               ,POINTER  :: Ordinates(:,:)
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### LOCAL VARIABLES
REAL(KIND_AngularFlux) :: err
LOGICAL                :: Test,Test1
TYPE(varying_string)   :: VS
INTEGER                :: Ni,Nj,Nm,m
REAL(KIND_DOR)         :: U(2)
INTEGER,POINTER        :: CellDeps(:,:)

!!--begin--

NULLIFY( CellDeps )

!test the set of directions for validity
TEST = TEST_Directions( Ordinates , tol=1.d-15 , err=err )
VS = VSErrMsg(err)
CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MCS]] Testing if directions are &
   & really unit vectors... &
   &[test="//MERGE("PASS","FAIL",TEST)//"] with "//STR(VS) )


!long characteristics
IF( Using_LongCharacteristics )THEN

 IF( Using_CachedLongCharacteristics )THEN

  CALL SETUP_TransportSweep_MCL_Cached( AngularFluxV , &
   Ordinates , Mesh , pThread , WithinCell , fdbk )


  !check the linear source if so desired
  !!--
  IF( Using_LinearSourceTest )THEN
   CALL TAP_ALLOCATE_P2("ScalarFluxC",ScalarFluxC,(/Ng,Mesh%NCells/),&
     fdbk,init=0.d0)
   CALL TAP_ALLOCATE_P3("ScalarFluxCellFunction",ScalarFluxCellFunction,(/1+Mesh%NDim,Ng,Mesh%NCells/),&
     fdbk,init=0.d0)
   TEST = TEST_MCLCached_LinSrc( Ordinates , Mesh , pThread , &
     WithinCell , fdbk , Unit=Unit_LinearSourceTest , err=err )
   VS = VSErrMsg(err)
   CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MCS]] testing long characteristics solution against &
     &analytic solution with linear source and no scattering, &
     &[test="//MERGE("PASS","FAIL",TEST)//"] with "//STR(VS) )
  END IF
  !!--

 END IF


!subcell balance characteristics
ELSE IF( Using_SBCharacteristics )THEN

 Ni = NUM_Cells(Mesh)
 Nj = NUM_Faces(Mesh)
 Nm = SIZE(Ordinates,2)

 !!allocate some variables we don't need elsewhere
 CALL TAP_ALLOCATE_P3("AngularFluxF",AngularFluxF,(/Ng,Nj,Nm/),fdbk,init=0._KIND_AngularFlux)
 CALL TAP_ALLOCATE_P3("AngularFluxC",AngularFluxC,(/Ng,Ni,Nm/),fdbk,init=0._KIND_AngularFlux)
 CALL UPDATE_ReflectiveBC2(Mesh,Ordinates,AngularFluxV,AngularFluxF,BC)

 CellDeps=>NULL()
 TEST = .TRUE.

 DO m=1,Nm

  !reallocate to number of cells
  IF( ASSOCIATED(pThread(m)%path(1)%order) )THEN
   DEALLOCATE( pThread(m)%path(1)%order )
  END IF
  ALLOCATE( pThread(m)%path(1)%order(Ni) )
   
  !get the celldeps and order for this direction
  CALL SETUP_MeshCellIterator(Mesh,Azimuthal_Angles(m),CellDeps,pThread(m)%path(1)%order )

  !test it
  TEST1 = TEST_GraphOrdering( CellDeps , pThread(m)%path(1)%order )
  TEST = TEST.AND.TEST1
  U(1)=cos(Azimuthal_Angles(m))
  U(2)=sin(Azimuthal_Angles(m))
  CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MCS]] CellDeps ordering &
     & for direction m="//TRIM(STR(m))//&
     " Ox="//TRIM(STR(U(1),"(f5.2)"))//&
     " Oy="//TRIM(STR(U(2),"(f5.2)"))//&
     " [test="//MERGE("PASS","FAIL",TEST1)//"]" )

  !deallocate what we don't need
  DEALLOCATE( CellDeps )

 END DO
 CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MCS]] CellDeps ordering &
    &[test="//MERGE("PASS","FAIL",TEST)//"]" )

END IF

!!--end--
END  SUBROUTINE




!!### SUBROUTINE <<SETUP_MoCshort>>
SUBROUTINE SETUP_MoCshort( fdbk )

!!#### PURPOSE
!! Set up to run.

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback variable [fdbk]
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_="SETUP_MoCshort"

!!#### LOCAL VARIABLES
REAL :: tin,tout,dt
INTEGER :: Nm,MAX_l,Nl

!!--begin--

!0.
CALL CPU_TIME(tin)

IF( InterpOrder<1 .OR. InterpOrder>2 )THEN
 CALL UpdateAndDump(fdbk_error,fdbk,s=&
   "[[MCS]] The interpolation order must be set with \MCSorder{x}")
END IF

!NEW MESH SETUP
CALL SETUP_VertToFaceLink(Mesh)

!external source
CALL SETUP_ExtSource( Mesh , ExtSourceCellFunction , fdbk )

!materials
CALL SETUP_Materials( Mesh , MAX_l,Nl,MacT,MacS,MacNu,MacF, CoeffScalarFluxM , fdbk )

!possibly use an analytic source
IF( Using_AnalyticTransportTest )THEN
 CALL UpdateAndDump(fdbk_comment,fdbk,&
   s="[[MCS]] Initializing sources for Analytic Transport Test.")
 CALL SETUP_AnalyticTest("[[MCS]]",&
   MacS,MacF,MacNu,MacT,l_,CoeffScalarFluxM,&
   Mesh,&
   fdbk)
 CALL UPDATE_CellAvgMom0QextA(MCS_KEY_CellFunction(CellFunctionMethod),&
   Mesh,ExtSourceCellFunction)
 ExtSourceCellFunction=ExtSourceCellFunction/c_4_times_PI
END IF


!get the reciprocal of the sin
Nm = SIZE(Ordinates,2)
ALLOCATE( RecipSin(Nm) , PolSin(Nm) )
PolSin = SQRT(1._KIND_DOR-Ordinates(3,:)**2)
RecipSin = 1._KIND_DOR/(1.d-30+PolSin)

!the following routines are expecting x-y and z directions so we modify Ordinates
!temporarily
Ordinates(1,:) = Ordinates(1,:)*RecipSin(:)
Ordinates(2,:) = Ordinates(2,:)*RecipSin(:)

!setup the intracell sweep parameters
CALL SETUP_IntraCell( WithinCell , SourceDist , NearestFace , k_ ,&
   fdbk , TravelDirections = Ordinates(1:2,:) , InterpOrder = InterpOrder , &
   Mesh = Mesh )

!set the extra variables and boundary conditions
CALL SETUP_ExtraVarsAndBC( Mesh , AngularFluxV , Ordinates , &
  WithinCell , NearestFace , k_ , fdbk )

!CALL Save("WithinCell",WithinCell)
!CALL Save("NearestFace",NearestFace)
!CALL Save("k_",k_)

CALL SETUP_SweepOrder( InterpOrder , Ordinates , pThread , fdbk )

CALL SETUP_InterpPlane( Ordinates , InterpPlaneU , FrontPos , StreamDist , fdbk )

!change boundary conditions to face-oriented ones
CALL EVAL_FaceBoundaryConditions(Mesh,&
  Ordinates,&
  NearestFace,WithinCell,k_,&
  StreamDist,SourceDist)

!revert back
Ordinates(1,:) = Ordinates(1,:)/RecipSin(:)
Ordinates(2,:) = Ordinates(2,:)/RecipSin(:)

CALL UPDATE_Reflective_2Dgen (AngularFluxV,WithinCell)



!set up extra stuff
CALL SETUP_ExtraStuff( Ordinates , fdbk )

!shouldn't have any ERROR number in these distances
IF( ANY(StreamDist==ERROR(1._KIND_MSH)) )THEN
 CALL UpdateAndDump(fdbk_warning,fdbk,s="[[MCS]] Some StreamDist elements&
  & have not been initialized which indicates a serious problem with ray&
  & tracing.")
END IF

!6. extra output
IF( Print_RayEffectsInfo )THEN
 CALL RayEffectsStatistics_2D(Mesh,Ordinates,rayeffectsfile,fdbk)
END IF

!compute final time.
CALL CPU_TIME(tout)
dt = (tout-tin)

!timing update
CALL UpdateAndDump(fdbk_profile,fdbk,s="[[MCS]] Setup completed in &
  &[time="//STRTIME(dt)//"].")

!!--end--
END SUBROUTINE


!!### SUBROUTINE <<EnsureQuality>>
SUBROUTINE EnsureQuality(fdbk)
!!#### PURPOSE
!! Ensure quality of various runtime parameters during the
!! first solver iteration.

USE LIB_xy                                                  !!((10-B-LIB_xy.f90))

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
LOGICAL :: CHECK
REAL(KIND_AngularFlux) :: err,err2
TYPE(varying_string) :: VS,VS2

!!--begin--

!! 1. ensure that the <NearestFace> array is updated if the
!!    mesh changed to <CellBased>
IF( IsCellBased(Mesh) )THEN

 CALL UPDATE(fdbk_comment,fdbk,s=&
  "[[MCS]] updating nearest faces because mesh is cellbased ...")
 CALL DUMP(fdbk)
 NearestFace = IntersectFaces_Omega(Mesh,&
                Omega         = -Ordinates , &
                InteriorCells = WithinCell          )
END IF

CHECK = TEST_NearestFace0( NearestFace )
CALL UPDATE(fdbk_comment,fdbk,s=&
  "[[MCS]] Checked the nearest-faces test 0, [test="//MERGE("PASS","FAIL",CHECK)//"]")
CALL DUMP(fdbk)

! Removed as of v2.23 
! CHECK = TEST_NearestFace1( Mesh , NearestFace )
! CALL UPDATE(fdbk_comment,fdbk,s=&
!    "[[MCS]] Checked the nearest-faces test 1, [test="//MERGE("PASS","FAIL",CHECK)//"]")
! CALL DUMP(fdbk)
! 

CHECK = TEST_Interpolation(relerr=err)
VS = VSErrMsg(err)
CALL UPDATE(fdbk_comment,fdbk,s=&
   "[[MCS]] Checked interpolation kernel, [test="//MERGE("PASS","FAIL",CHECK)//"] with "//&
    STR(VS)  )
CALL DUMP(fdbk)

!CHECK = TESTSUITE_PolygonSlices(Noisy=.FALSE.,&
!  MinTol=err,MaxAreaRelDiff=err2)
VS = VSTolMsg(err)
VS2= VSRelErrMsg(err2)
CALL UPDATE(fdbk_comment,fdbk,s=&
   "[[MCS]] Checked polygon slicing operations, [test="//MERGE("PASS","FAIL",CHECK)//&
     "], yielded minimum tolerance for rotation/translation invariance "//STR(VS)//&
     " and maximum relative difference between slice area and polygon area "//STR(VS2)  )
!CALL PRINT_InterpInfo(Unit=80)
!CALL PRINT_CharacteristicInfo(Unit=81)

CALL MODIFY( fdbk , PauseLevel=fdbk_error)
!!--end--
END SUBROUTINE



!!### SUBROUTINE <SETUP_InterpPlane>
SUBROUTINE SETUP_InterpPlane( Ordinates , InterpPlaneU , FrontPos , StreamDist , fdbk )

!!#### PURPOSE
!! Determine two quantities associated with the interpolation plane:
!! 1) the distance traveled to the plane from each vert given by k_
!! 2) the position on that plane

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: InterpPlaneU
REAL(KIND_DOR),INTENT(IN) :: Ordinates(:,:)

!!#### REQUIRED OUTPUT
REAL(KIND_MSH),POINTER :: FrontPos(:,:,:)
REAL(KIND_MSH),POINTER :: StreamDist(:,:,:)

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!--begin--
CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MCS]] Determining the front-positions and streaming distances...")

SELECT CASE( InterpPlaneU )

 CASE( MCS_Perp )
   CALL SETUP_InterpPlane_Perp( FrontPos , StreamDist , &
     k_ , NearestFace , WithinCell , Ordinates , Mesh , fdbk )

 CASE( MCS_Face )
   CALL SETUP_InterpPlane_Face( FrontPos , StreamDist , &
     k_ , NearestFace , WithinCell , Ordinates , Mesh , fdbk )

 CASE( MCS_Diag )
   CALL SETUP_InterpPlane_Diag( FrontPos , StreamDist , &
     k_ , NearestFace , WithinCell , Ordinates , Mesh , fdbk )

 CASE DEFAULT
   CALL UpdateAndDump(fdbk_error,fdbk,s="[[MCS]] The interpolation plane must&
     & be set!")

END SELECT

!!--end--
END SUBROUTINE





SUBROUTINE SETUP_InterpPlane_Perp( FrontPos , StreamDist , &
  k_ , NearestFace , WithinCell , Ordinates , Mesh , fdbk )

!!#### METHOD
!! The interpolation plane is set up with a point on the vert we are calculating
!! the angular flux at, and a direction pointing in the direction particles
!! travel.


!!#### TOOLBOXES
USE TBX_Mesh                                                !!((15-B-TBX_Mesh.f90))
USE TBX_ComputationalGeometry                               !!((09-A-TBX_ComputationalGeometry.f90))
USE USR_fdbk                                                !!((08-C-USR_fdbk.f90))

!!#### KINDS
USE KND_DiscreteOrdinates                                   !!((02-A-KND_DiscreteOrdinates.f90))

!!#### REQUIRED OUTPUT
REAL(KIND_MSH),POINTER :: FrontPos(:,:,:)
REAL(KIND_MSH),POINTER :: StreamDist(:,:,:)

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!####  REQUIRED INPUT
!! * the first face intersected along a short characteristic [NearestFace]
!! * the cell containing each vert+direction short
!!   characteristic [WithinCell]
!! * the mesh object [Mesh]
INTEGER             ,INTENT(IN) :: NearestFace(:,:)
INTEGER             ,INTENT(IN) :: WithinCell(:,:)
REAL(KIND_DOR),INTENT(IN) :: Ordinates(:,:)
TYPE(TYPE_Mesh)      ,INTENT(IN) :: Mesh

!!#### REQUIRED INPUT/OUTPUT
!! * the list of interpolant verts for each short characteristic (direction+vert) <k_>
INTEGER             ,INTENT(INOUT) :: k_(:,:,:)

!!#### LOCAL VARIABLES
INTEGER :: InterpOrder,MAX_k,MAX_ma,j,n,ma,k
REAL(KIND_MSH),ALLOCATABLE :: P_base(:),Pn_front(:),Pn_Dir(:),P_front(:),P_interp(:)
REAL(KIND_MSH),ALLOCATABLE :: Ln(:,:)
LOGICAL                   :: INTERSECT
INTEGER                   :: NDim
INTEGER,ALLOCATABLE :: order(:)

!!--begin--

InterpOrder = SIZE(k_,1) - 1
MAX_k       = SIZE(WithinCell,1)
MAX_ma      = SIZE(WithinCell,2)
NDim        = NUM_Dimensions_Mesh(Mesh)

!allocate the attenuation distance
ALLOCATE( StreamDist(1:InterpOrder+1,1:MAX_k,1:MAX_ma) )
StreamDist = SQRT(HUGE(0._KIND_MSH))

!allocate the position of vertices on the front
ALLOCATE( FrontPos  (1:InterpOrder+1,1:MAX_k,1:MAX_ma) )
FrontPos = SQRT(HUGE(0._KIND_MSH))

!allocate local stuff
ALLOCATE( P_interp(1:NDim) )
ALLOCATE( P_front (1:NDim) )
ALLOCATE( P_base  (1:NDim) )
ALLOCATE( Ln      (1:NDim,1:2) )
ALLOCATE( Pn_front(1:NDim+1  ) )
ALLOCATE( Pn_dir  (1:NDim+1  ) )
ALLOCATE( order   (1:InterpOrder+1) )


!**FRONT POSITIONS**
!finally we are ready to proceed with the determination of the "front
!position" of each vertex used in interpolation
DO ma=1,MAX_ma
 DO k=1,MAX_k

  !cycle on boundary condition for this vert+direction
  IF( WithinCell(k,ma)<=0 )CYCLE

  !set the face
  j = NearestFace(k,ma)

  !get the base point
  P_base = Vert(Mesh,k)

  !determine the plane of the front with a vertex and the direction of motion
  Pn_front = xyPLANE_PV( P_base , Ordinates(:,ma) )

  !determine another plane that divides the interpolation plane (front) into
  !two pieces
  Pn_dir = xyPLANE_PV( P_base , xyPERPCCW_U( xyDIRECTION_Pn(Pn_front) )  )


  DO n=1,InterpOrder+1

   IF( k_(n,k,ma)<=0 )CYCLE

   !determine the point to use in the interpolation
   P_interp = Vert(Mesh,k_(n,k,ma))

   !construct a ray from this point and the direction of travel
   Ln = xyLINE_PU( P_interp , REAL(Ordinates(:,ma),KIND_MSH) )

   !determine the intersection of this ray with the interpolation plane
   INTERSECT = xyINTERSECT_PnLn( Pn_front , Ln , &
                                 SDIST       = StreamDist( n , k , ma ) , &
                                 P_intersect = P_front )
   IF( .NOT.INTERSECT )THEN
    WRITE(*,*)"There is a problem because we have should have found an intersection here..."
   END IF

   !by using this plane and determining distances the points are from the plane
   !now we get the correctly signed values for the points of the line of the front
   FrontPos ( n , k , ma ) = xySDIST_PnP( Pn_dir , P_front )
  END DO

  !sort the front positions
  CALL Sort( FrontPos(:,k,ma) , order )
  k_(:,k,ma)         = Reorder( k_(:,k,ma) , order , side="R" )
  StreamDist(:,k,ma) = Reorder( StreamDist(:,k,ma) , order , side="R" )
 END DO
END DO

DEALLOCATE( Pn_dir , Ln , P_front , Pn_front , P_interp , P_base , order )

!!--end--
END SUBROUTINE



SUBROUTINE SETUP_InterpPlane_Face( FrontPos , StreamDist , &
  k_ , NearestFace , WithinCell , Ordinates , Mesh , fdbk )

!!#### METHOD
!! The interpolation plane is set up with a point on the vert we are calculating
!! the angular flux at, and a direction parallel to the opposite face.

!!#### TOOLBOXES
USE TBX_Mesh                                                !!((15-B-TBX_Mesh.f90))
USE TBX_ComputationalGeometry                               !!((09-A-TBX_ComputationalGeometry.f90))
USE USR_fdbk                                                !!((08-C-USR_fdbk.f90))

!!#### KINDS
USE KND_DiscreteOrdinates                                   !!((02-A-KND_DiscreteOrdinates.f90))

!!#### REQUIRED OUTPUT
REAL(KIND_MSH),POINTER :: FrontPos(:,:,:)
REAL(KIND_MSH),POINTER :: StreamDist(:,:,:)

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!####  REQUIRED INPUT
!! * the first face intersected along a short characteristic [NearestFace]
!! * the cell containing each vert+direction short
!!   characteristic [WithinCell]
!! * the mesh object [Mesh]
INTEGER             ,INTENT(IN) :: NearestFace(:,:)
INTEGER             ,INTENT(IN) :: WithinCell(:,:)
REAL(KIND_DOR),INTENT(IN) :: Ordinates(:,:)
TYPE(TYPE_Mesh)      ,INTENT(IN) :: Mesh

!!#### REQUIRED INPUT/OUTPUT
!! * the list of interpolant verts for each short characteristic (direction+vert) <k_>
INTEGER             ,INTENT(INOUT) :: k_(:,:,:)

!!#### LOCAL VARIABLES
INTEGER :: InterpOrder,MAX_k,MAX_ma,j,n,ma,k,i,jp
REAL(KIND_MSH),ALLOCATABLE :: P_base(:),Pn_front(:),Pn_Dir(:),P_front(:),P_interp(:)
REAL(KIND_MSH),ALLOCATABLE :: Ln(:,:)
LOGICAL                   :: INTERSECT
INTEGER                   :: NDim
INTEGER,ALLOCATABLE :: order(:)

!!--begin--
InterpOrder = SIZE(k_,1) - 1
MAX_k       = SIZE(WithinCell,1)
MAX_ma     = SIZE(WithinCell,2)
NDim        = NUM_Dimensions_Mesh(Mesh)

!allocate the attenuation distance
ALLOCATE( StreamDist(1:InterpOrder+1,1:MAX_k,1:MAX_ma) )
StreamDist = SQRT(HUGE(0._KIND_MSH))

!allocate the position of vertices on the front
ALLOCATE( FrontPos  (1:InterpOrder+1,1:MAX_k,1:MAX_ma) )
FrontPos = SQRT(HUGE(0._KIND_MSH))

!allocate local stuff
ALLOCATE( P_interp(1:NDim) )
ALLOCATE( P_front (1:NDim) )
ALLOCATE( P_base  (1:NDim) )
ALLOCATE( Ln      (1:NDim,1:2) )
ALLOCATE( Pn_front(1:NDim+1  ) )
ALLOCATE( Pn_dir  (1:NDim+1  ) )
ALLOCATE( order   (1:InterpOrder+1) )


!**FRONT POSITIONS**
!finally we are ready to proceed with the determination of the "front
!position" of each vertex used in interpolation
DO ma=1,MAX_ma
 DO k=1,MAX_k

  !get cell
  i = WithinCell(k,ma)

  !cycle on boundary condition for this vert+direction
  IF( i<=0 )CYCLE

  !set the face
  j = NearestFace(k,ma)

  !get the base point
  P_base = Vert(Mesh,k)

  !get the parent face
  jp = ParentFace(Mesh,k,i)
  !WRITE(*,*)"ma,k,i,j,jp",ma,k,i,j,jp

  !this is a hanging vert
  IF( jp/=0 )THEN
   !make the plane in line with the face it lives on
   Pn_front = xyPLANE_PV( P_base , FaceNormal(Mesh,jp) )
  ELSE
   !determine the plane of the front with a vertex and the mesh face normal
   Pn_front = xyPLANE_PV( P_base , FaceNormal(Mesh,j) )
  END IF



  !sign change to make direction of face in the direction of motion
  IF( xyDOT_VV( xyDIRECTION_Pn(Pn_front) , &
                REAL(Ordinates(:,ma),KIND_MSH) )<0._KIND_MSH )THEN
   Pn_front =  -Pn_front
  END IF

  !determine another plane that divides the interpolation plane (front) into
  !two pieces
  Pn_dir = xyPLANE_PV( P_base , xyPERPCCW_U( xyDIRECTION_Pn(Pn_front) )  )


  DO n=1,InterpOrder+1
   IF( k_(n,k,ma)<=0 )CYCLE

   !determine the point to use in the interpolation
   P_interp = Vert(Mesh,k_(n,k,ma))

   !construct a ray from this point and the direction of travel
   Ln = xyLINE_PU( P_interp , REAL(Ordinates(:,ma),KIND_MSH) )

   !determine the intersection of this line the interpolation plane
   INTERSECT = xyINTERSECT_PnLn( Pn_front , Ln , &
                                 SDIST       = StreamDist( n , k , ma ) , &
                                 P_intersect = P_front )
   IF( .NOT.INTERSECT )THEN
    WRITE(*,*)"There is a problem for n="//TRIM(STR(n))//", k="//TRIM(STR(k))//&
      ", ma="//TRIM(STR(ma))//"."
    WRITE(*,*)"We have should have found an intersection here..."
   END IF

   !by using this plane and determining distances the points are from the plane
   !now we get the correctly signed values for the points of the line of the front
   FrontPos ( n , k , ma ) = xySDIST_PnP( Pn_dir , P_front )
  END DO

  !sort the front positions in ascending order
  CALL Sort( FrontPos(:,k,ma) , order )
  k_(:,k,ma)         = Reorder(         k_(:,k,ma) , order , side="R" )
  StreamDist(:,k,ma) = Reorder( StreamDist(:,k,ma) , order , side="R" )

 END DO
END DO

DEALLOCATE( Pn_dir , Ln , P_front , Pn_front , P_interp , P_base , order )

!!--end--
END SUBROUTINE




SUBROUTINE SETUP_InterpPlane_Diag( FrontPos , StreamDist , &
  k_ , NearestFace , WithinCell , Ordinates , Mesh , fdbk )

!!#### METHOD
!! The interpolation plane is set up using a diagonal of the cell closest to the
!! vert of interest, such that the interpolation plane is entirely contained
!! within the cell.

!!#### TOOLBOXES
USE TBX_Mesh                                                !!((15-B-TBX_Mesh.f90))
USE TBX_ComputationalGeometry                               !!((09-A-TBX_ComputationalGeometry.f90))
USE USR_fdbk                                                !!((08-C-USR_fdbk.f90))

!!#### KINDS
USE KND_DiscreteOrdinates                                   !!((02-A-KND_DiscreteOrdinates.f90))

!!#### REQUIRED OUTPUT
REAL(KIND_MSH),POINTER :: FrontPos(:,:,:)
REAL(KIND_MSH),POINTER :: StreamDist(:,:,:)

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!####  REQUIRED INPUT
!! * the first face intersected along a short characteristic [NearestFace]
!! * the cell containing each vert+direction short
!!   characteristic [WithinCell]
!! * the mesh object [Mesh]
INTEGER        ,INTENT(IN) :: NearestFace(:,:)
INTEGER        ,INTENT(IN) :: WithinCell(:,:)
REAL(KIND_DOR) ,INTENT(IN) :: Ordinates(:,:)
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh

!!#### REQUIRED INPUT/OUTPUT
!! * the list of interpolant verts for each short characteristic (direction+vert) <k_>
INTEGER             ,INTENT(INOUT) :: k_(:,:,:)

!!#### LOCAL PARAMETERS
LOGICAL,PARAMETER :: Noisy_=.FALSE.

!!#### LOCAL VARIABLES
INTEGER                    :: InterpOrder,MAX_k,MAX_m,kh,k,m,i,j,NDim
REAL(KIND_MSH),ALLOCATABLE :: Pn_front(:),Pn_Dir(:)
INTEGER       ,ALLOCATABLE :: k1(:),k2(:),j1(:),j2(:)

!!--begin--

!WRITE(*,*)"SIZE(k_,1)=",SIZE(k_,1)
InterpOrder = SIZE(k_,1) - 1
MAX_k       = SIZE(WithinCell,1)
MAX_m       = SIZE(WithinCell,2)
NDim        = NUM_Dimensions_Mesh(Mesh)

!allocate the attenuation distance
ALLOCATE( StreamDist(1:InterpOrder+1,1:MAX_k,1:MAX_m) )
StreamDist = SQRT(HUGE(0._KIND_MSH))

!allocate the corner verts for each diagonal
ALLOCATE( k1(MAX_k) , k2(MAX_k) )
k1 = 0
k2 = 0
ALLOCATE( j1(MAX_k) , j2(MAX_k) )
j1 = 0
j2 = 0

!allocate the position of vertices on the front
ALLOCATE( FrontPos  (1:InterpOrder+1,1:MAX_k,1:MAX_m) )
FrontPos = SQRT(HUGE(0._KIND_MSH))

!allocate local stuff
ALLOCATE( Pn_front(1:NDim+1  ) )
ALLOCATE( Pn_dir  (1:NDim+1  ) )


!**FRONT POSITIONS**
!finally we are ready to proceed with the determination of the "front
!position" of each vertex used in interpolation
DO m=1,MAX_m
 DO k=1,MAX_k

  !get cell
  i = WithinCell(k,m)

  !cycle on boundary condition for this vert+direction
  IF( i<=0 )CYCLE

  !check on tracing back to a boundary
  j = NearestFace(k,m)
  IF( IsBoundaryFace(Mesh,j) )THEN
   k1(k) = GET_VertOnFace(Mesh,j,First=.TRUE.)
   k2(k) = GET_VertOnFace(Mesh,j,Last=.TRUE.)
   j1(k) = j
   j2(k) = 0
  ELSE
   !handle hanging Verts a different way
   IF( IsHangingVert(Mesh,k,i) )CYCLE

   !search through for corner verts
   !and faces, k1,k2,j1,j2
   CALL GET_DiagCorners_Corner(Mesh,k,i,k1(k),k2(k),j1(k),j2(k) )

  END IF

  !get the interior cell parameters
  CALL GET_IntraCellParams( Mesh , k , k1(k) , k2(k) , Ordinates(:,m) , &
    Pn_front , Pn_dir , SourceDist(k,m) )

  !get the interior cell positions
  CALL GET_IntraCellPos   ( Mesh , k , m , &
    Ordinates(:,m) , Pn_front , Pn_dir , &
    k_(:,k,m) , SourceDist(k,m) , StreamDist(:,k,m) , FrontPos(:,k,m) )

 END DO

 !ADD FIXUP FOR HANGING VERTS HERE
 DO kh=1,MAX_k
  !get cell
  i = WithinCell(kh,m)

  !cycle on boundary condition for this vert+direction
  IF( i<=0 )CYCLE

  !cycle on trace back to a boundary face (handled above)
  j = NearestFace(kh,m)
  IF( IsBoundaryFace(Mesh,j) )CYCLE

  !handle hanging Verts now
  IF( .NOT.IsHangingVert(Mesh,kh,i) )CYCLE

  !search through for corner verts
  !and faces, k1,k2,j1,j2
  CALL GET_DiagCorners_Hanging(Mesh,kh,i,Ordinates(:,m),j1(:),j2(:),k1(:),k2(:) )

  !get the interior cell parameters
  CALL GET_IntraCellParams( Mesh , kh , k1(kh) , k2(kh) , Ordinates(:,m) , &
    Pn_front , Pn_dir , SourceDist(kh,m) )

  !get the interior cell positions
  CALL GET_IntraCellPos   ( Mesh , kh , m , &
    Ordinates(:,m) , Pn_front , Pn_dir , &
        k_(:,kh,m) , SourceDist(kh,m) , StreamDist(:,kh,m) , FrontPos(:,kh,m) )
 END DO


END DO

DEALLOCATE( Pn_front , Pn_dir )

!!--end--
END SUBROUTINE




SUBROUTINE SETUP_SweepOrder( InterpOrder , Ordinates , pThread , fdbk )
!!#### PURPOSE
!! Determine the order to sweep through the mesh as a function
!! of discrete ordinate index first and *then* as a function
!! of path.  So for each discrete ordinate index there are paths of verts stored
!! in %path(1:Npath).  One one processor, you cannot gain performance using more
!! than one path.  With multiple processors, you can have many paths through the
!! mesh (along verts, pointing in to certain cells)

!!#### DEPENDENCIES
USE USR_fdbk                                                !!((08-C-USR_fdbk.f90))
USE USR_pThread                                             !!((03-A-USR_pThread.f90))
USE PAR_MoCshort                                            !!((03-A-PAR_MoCshort.f90))

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: InterpOrder
REAL(KIND_DOR)    ,INTENT(IN) :: Ordinates(:,:)

!!#### REQUIRED OUTPUT
TYPE(TYPE_pThread),POINTER :: pThread(:)

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),INTENT(INOUT) :: fdbk

!!--begin--
CALL UPDATE(fdbk_comment,fdbk,s="[[MCS]] Determining the sweep ordering...")
CALL DUMP(fdbk)

CALL SETUP_SweepOrder_Proper ( InterpOrder , Ordinates , pThread , fdbk )

!!--end--
END SUBROUTINE




SUBROUTINE SETUP_SweepOrder_Serial0( Ordinates , pThread , fdbk )
!!#### METHOD
!! Right now we are just doing a simple ordering of each vertex
!! in the order in the the "front plane" hits verts, starting at s=-infinity
!! and going to s=infinity where s is the variable which parametrizes the family
!! of parallel planes whose normals point in the direction of travel.

!!#### TYPES AND BASIC PROCEDURES PACKAGES
USE USR_pThread                                             !!((03-A-USR_pThread.f90))

!!#### TOOLBOXES
USE TBX_Mesh                                                !!((15-B-TBX_Mesh.f90))
USE TBX_ComputationalGeometry                               !!((09-A-TBX_ComputationalGeometry.f90))
USE FUN_EQUILOC                                             !!((03-A-FUN_EQUILOC.f90))
USE SUB_Swap                                                !!((04-A-SUB_Swap.f90))

!!#### EXTERNAL ROUTINES
USE SUB_Sort_quick,ONLY: Sort=>Sort_quick                   !!((03-A-SUB_Sort_quick.f90))

!!#### REQUIRED OUTPUT
!! * the parallel threads variable which contains the ordering of vertices
TYPE(TYPE_pThread),POINTER :: pThread(:)

!!#### REQUIRED INPUT
REAL(KIND_DOR)    ,INTENT(IN) :: Ordinates(:,:)

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
LOGICAL :: TEST,TEST_Lin,TEST_Para
INTEGER :: traversal
INTEGER,PARAMETER :: FwdPara_=2,FwdLin_=1,BwdPara_=-2,BwdLin_=-1
REAL :: tin,tout,dt

!!--begin--

!do the simplest ordering first
CALL SETUP_pthread_PlaneWave( pthread , k_ , Mesh , Ordinates )

!test the ordering now to see if miraculously we have passed
TEST = TEST_sweeporder( k_ , pthread , fdbk )
IF( .NOT.TEST )THEN
 CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MCS]] traversal results for&
   & <PlaneWave> variant are not adequate...moving on to more complicated ones.")
ELSE
  CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MCS]] traversal results for&
   & <PlaneWave> variant worked!")
END IF

!start time
CALL CPU_time(tin)
traversal=-2

IF( .NOT.TEST )THEN
 SELECT CASE(traversal)

  CASE( FwdPara_ ) ; CALL SETUP_pthread_FwdPara( pthread , k_ , Mesh , Ordinates )
  CASE( FwdLin_  ) ; CALL SETUP_pthread_FwdLin ( pthread , k_ , Mesh , Ordinates )
  CASE( BwdPara_ ) ; CALL SETUP_pthread_BwdPara( pthread , k_ , Mesh , Ordinates )
  CASE( BwdLin_  ) ; CALL SETUP_pthread_BwdLin ( pthread , k_ , Mesh , Ordinates )

 END SELECT

END IF

!end timing
CALL CPU_time(tout)
dt = tout-tin
CALL UPDATE(fdbk_comment,fdbk,s="[[MCS]] total time to &
  &calculate 'first pass' sweep order [time="//STRTIME(dt)//"]")

!test linear sweep
TEST_lin = TEST_sweeporder(k_(1:2,:,:),pthread,fdbk)
CALL UPDATE(fdbk_comment,fdbk,s="[[MCS]] linear sweep ordering results &
  &(before cycle breaking), [test="//&
  MERGE("PASS","FAIL",TEST_lin)//"]")
IF( .NOT.TEST_Lin )THEN
 CALL UPDATE( fdbk_error , fdbk , s="[[MCS]] the linear sweep didn't &
   &result in a reasonable order---mesh must be bad.")
END IF
CALL DUMP(fdbk)


!test parabolic sweep
TEST_para = TEST_sweeporder(k_( : ,:,:),pthread,fdbk)
CALL UPDATE(fdbk_comment,fdbk,s="[[MCS]] parabolic sweep ordering &
  &results (before cycle breaking), [test="//&
  MERGE("PASS","FAIL",TEST_para)//"]")
IF( .NOT.TEST_Para )THEN
 CALL UPDATE( fdbk_comment , fdbk , s="[[MCS]] the parabolic sweep &
   &didn't result in a reasonable order---this only indicates an &
   &error if the mesh is orthogonal.")
END IF
CALL DUMP(fdbk)

!!--end--
END SUBROUTINE





SUBROUTINE SETUP_SweepOrder_Proper( InterpOrder , Ordinates , pThread , fdbk )
!!#### METHOD
!! Determine the proper <SweepOrder> through depth-first traversal
!! of the vertex dependency tree.

!!#### TYPES AND BASIC PROCEDURES PACKAGES
USE USR_pThread                                             !!((03-A-USR_pThread.f90))

!!#### TOOLBOXES
USE TBX_Mesh                                                !!((15-B-TBX_Mesh.f90))
USE TBX_ComputationalGeometry                               !!((09-A-TBX_ComputationalGeometry.f90))
USE FUN_EQUILOC                                             !!((03-A-FUN_EQUILOC.f90))
USE SUB_Swap                                                !!((04-A-SUB_Swap.f90))

!!#### EXTERNAL ROUTINES
USE SUB_Sort_quick,ONLY: Sort=>Sort_quick                   !!((03-A-SUB_Sort_quick.f90))

!!#### GLOBAL ACCESS
USE VAR_Mesh             ,ONLY: Mesh                        !!((46-B-VAR_Mesh.f90))

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: InterpOrder
REAL(KIND_DOR),INTENT(IN) :: Ordinates(:,:)

!!#### REQUIRED OUTPUT
!! * the parallel threads variable which contains the ordering of vertices
TYPE(TYPE_pThread),POINTER :: pThread(:)

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
INTEGER :: m,Nm,k,Nk,o,k1,k2,k3,j,i,n
LOGICAL,ALLOCATABLE :: BeenVisited(:)
INTEGER :: cycle_count
INTEGER :: tot_count,interp_count(SIZE(k_,1)),bc_count,bf_count
REAL    :: finterp_count(SIZE(k_,1)),fbc_count,fbf_count

!!--begin--
Nm          = SIZE(Ordinates,2)
Nk          = NUM_Verts(Mesh)
cycle_count = 0
tot_count   = 0

!get the simple ordering in pthread
CALL SETUP_SweepOrder_Serial0( Ordinates , pThread , fdbk )

IF( InterpOrder==1 )GOTO 333

!break the cycles
ALLOCATE( BeenVisited(Nk) )

DO m=1,Nm
 BeenVisited = .FALSE.
 !WRITE(*,*)"m,k,k1,k2,k3"
 o = 0
 DO
  o = o + 1
  IF( o>SIZE(pthread(m)%path(1)%order) )EXIT

  !get the vert we are at
  k = pthread(m)%path(1)%order(o)

  !get the verts it depends on
  k1 = k_(1,k,m)
  k2 = k_(2,k,m)
  k3 = k_(3,k,m)

  !WRITE(*,*)m,k,k1,k2,k3

  !vert is a boundary condition
  IF( k1==k2 .AND. k2==k3 .AND. k3==0 )THEN
   BeenVisited(k) = .TRUE.
   CYCLE
  END IF

  tot_count = tot_count + 1

  !check 1
  IF( .NOT.BeenVisited(k1) )THEN
   CALL UPDATE(fdbk_warning,fdbk,s="[[MCS]] SERIOUS SWEEP ORDER ERROR! &
     & for vert [k1="//TRIM(STR(k1))//"] and direction [m="//TRIM(STR(m))//"]")
  END IF

  !check 2
  IF( .NOT.BeenVisited(k2) )THEN
   CALL UPDATE(fdbk_warning,fdbk,s="[[MCS]] SERIOUS SWEEP ORDER ERROR! &
     & for vert [k2="//TRIM(STR(k2))//"] and direction [m="//TRIM(STR(m))//"]")
  END IF

  !the only cycle can be in vert 3
  IF( k3/=0 )THEN
   IF( (.NOT.BeenVisited(k3)) )THEN
    CALL UPDATE(fdbk_comment,fdbk,s="[[MCS]] cycle found! &
      & for vert [k="//TRIM(STR(k))//"] and direction [m="//TRIM(STR(m))//"]")
    cycle_count = cycle_count + 1
    k_(3,k,m) = 0
   END IF
  END IF

  BeenVisited(k) = .TRUE.

 END DO


END DO

DEALLOCATE( BeenVisited )

333 CONTINUE

!tot_count how mny times we use 1 vert/2 verts/3 verts/etc. in
!the interpolation with the <interp_count>
tot_count = 0
bc_count  = 0
bf_count  = 0
interp_count = 0
DO m=1,Nm
 DO k=1,Nk
  tot_count = tot_count+1
  j = NearestFace(k,m)
  i = WithinCell(k,m)
  IF( i<=0 )THEN
   bc_count = bc_count + 1
   CYCLE
  ELSE IF( IsBoundaryFace(Mesh,j) )THEN
   bf_count = bf_count + 1
   CYCLE
  END IF
  n = COUNT(k_(:,k,m)/=0)
  interp_count(n) = interp_count(n) + 1
 END DO
END DO

!get fractions
fbc_count     = 100.*REAL(bc_count)/REAL(tot_count)
fbf_count     = 100.*REAL(bf_count)/REAL(tot_count)
finterp_count = 100.*REAL(interp_count)/REAL(tot_count-bf_count-bc_count)


!print out fractions
CALL UPDATE(fdbk_comment,fdbk,s="[[MCS]] Boundary Values "//&
  TRIM(STR(bc_count))//":"//TRIM(STR(tot_count))//" or "//&
  TRIM(STR(fbc_count,"(f5.1)"))//"% of All")
CALL UPDATE(fdbk_comment,fdbk,s="[[MCS]] Interps with Boundary Faces "//&
  TRIM(STR(bf_count))//":"//TRIM(STR(tot_count))//" or "//&
  TRIM(STR(fbf_count,"(f5.1)"))//"% of All")
DO n=1,SIZE(interp_count)
 CALL UPDATE(fdbk_comment,fdbk,s="[[MCS]] Interps in Interior with "//TRIM(STR(n))//" Vert(s) "//&
   TRIM(STR(interp_count(n)))//":"//TRIM(STR(tot_count))//" or "//&
   TRIM(STR(finterp_count(n),"(f5.1)"))//"% of Interior Interps")
END DO

CALL UPDATE(fdbk_comment,fdbk,s=&
  "[[MCS]] Cycle Count Fraction        = "//TRIM(STR(REAL(cycle_count)/REAL(tot_count))))
CALL UPDATE(fdbk_comment,fdbk,s=&
  "[[MCS]] Test_SweepOrder(k_,pthread) = "//MERGE("PASS","FAIL",TEST_sweeporder(k_,pthread,fdbk)))

!!--end--
END SUBROUTINE



!!### FUNCTION <<TEST_SweepOrder>>
FUNCTION TEST_SweepOrder(k_,pthread,fdbk) RESULT(pass)

!!#### PURPOSE
!! Test a sweep ordering.

!!#### REQUIRED INPUT
INTEGER           ,INTENT(IN) :: k_(:,:,:)
TYPE(TYPE_pthread),INTENT(IN) :: pthread(:)

!!#### REQUIRED OUTPUT
LOGICAL :: pass

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### LOCAL VARIABLES
LOGICAL,ALLOCATABLE :: BeenVisited(:)
INTEGER :: m,Nm,Nk,o,ks,s,k

!!--begin--

Nk = SIZE(k_,2)
Nm = SIZE(k_,3)

pass = .FALSE.

!break the cycles
ALLOCATE( BeenVisited(Nk) )

DO m=1,Nm
 BeenVisited = .FALSE.
 o = 0
 DO
  o = o + 1
  IF( o>SIZE(pthread(m)%path(1)%order) )EXIT

  !get the vert we are at
  k = pthread(m)%path(1)%order(o)

  !boundary condition
  IF( ALL(k_(:,k,m)==0) )THEN
   BeenVisited(k) = .TRUE.
   CYCLE
  END IF

  DO s=1,SIZE(k_,1)
   ks = k_(s,k,m)

   IF( ks==0 )THEN
    !not enough verts
    IF( s<2 )THEN
     CALL UpdateAndDump(fdbk_comment,fdbk,s=&
       "[[MCS]] SweepOrder_Serial0 not adequate: not enough verts for k="//TRIM(STR(k))//&
       " m="//TRIM(STR(m))//".")
     GOTO 666
    !end of verts
     ELSE
     EXIT
    END IF

   !hasn't been visited
   ELSE IF( .NOT.BeenVisited(ks) )THEN
    CALL UpdateAndDump(fdbk_comment,fdbk,s=&
       "[[MCS]] SweepOrder_Serial0 not adequate: unvisited ks="//TRIM(STR(ks))//&
       " m="//TRIM(STR(m))//".")
    GOTO 666
   END IF

  END DO

  BeenVisited(k) = .TRUE.

 END DO

END DO
IF( ALL(BeenVisited) )THEN
 Pass = .TRUE.
END IF

666 CONTINUE
DEALLOCATE( BeenVisited )

!!--end--
END FUNCTION



!!### WRAPUP ROUTINE: WRAPUP_MoCshort
SUBROUTINE WRAPUP_MoCshort( fdbk )
!!#### PURPOSE
!! Wrapup the variables introduced.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_fdbk),INTENT(INOUT) :: fdbk

!!--begin--

CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MCS]] The MoCshort package reports the following &
  &memory usage:")
CALL PRINT_MCS_Memory(OutputUnit(fdbk))

CALL CLEARn(WithinCell)
!add more here

!!--end--
END SUBROUTINE




FUNCTION TEST_WithinCell0( WithinCell ) RESULT(Pass)
INTEGER :: WithinCell(:,:)
INTEGER :: k,m
LOGICAL :: Pass,check0

!!--begin--
Pass = .TRUE.

!check within cell all interior verts to make sure
!that all directions have interior cells
DO k=1,NUM_Verts(Mesh)

 IF( .NOT.IsBoundaryVert(Mesh,k) )THEN

  !method 1
  check0 = .TRUE.
  DO m=1,SIZE(WithinCell,2)
   IF( WithinCell(k,m)==0 )THEN
    check0 = .FALSE.
        WRITE(*,*)"WithinCell failed for k="//TRIM(STR(k))//" m="//TRIM(STR(m))
   END IF
  END DO
  !--

  !method 2
  !check0 = ALL( WithinCell(k,:)>0 )
  !--

  Pass = Pass .AND. check0
 END IF

END DO

!!--end--
END FUNCTION



FUNCTION TEST_NearestFace0( NearestFace ) RESULT(Pass)
INTEGER :: NearestFace(:,:)
INTEGER :: k,m
LOGICAL :: Pass,check0

!!--begin--
Pass = .TRUE.

!check within cell all interior verts to make sure
!that all directions have interior cells
DO k=1,NUM_Verts(Mesh)

 IF( .NOT.IsBoundaryVert(Mesh,k) )THEN

  !method 1
  check0 = .TRUE.
  DO m=1,SIZE(NearestFace,2)
   IF( NearestFace(k,m)==0 )THEN
    check0 = .FALSE.
        WRITE(*,*)"NearestFace failed for k="//TRIM(STR(k))//" m="//TRIM(STR(m))
   END IF
  END DO
  !--

  !method 2
  !check0 = ALL( NearestFace(k,:)>0 )
  !--

  Pass = Pass .AND. check0
 END IF

END DO

!!--end--
END FUNCTION


! 
! FUNCTION TEST_NearestFace1( Mesh , NearestFace ) RESULT(Pass)
! TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh
! INTEGER :: NearestFace(:,:)
! INTEGER,ALLOCATABLE :: face_used(:)
! LOGICAL :: Pass
! INTEGER :: j
! 
! !!--begin--
! 
! ALLOCATE( face_used(NUM_Faces(Mesh)) )
! face_used = 0
! 
! DO j=1,NUM_Faces(Mesh)
!  face_used(j) = COUNT(NearestFace==j)
!  IF( .NOT.face_used(j) )THEN
!   WRITE(*,*)'fatal error: face j=',j,', is not used in the mesh sweep'
!   STOP
!  END IF
! END DO
! 
! Pass = ALL(face_used>0)
! !!--end--
! END FUNCTION




SUBROUTINE EVAL_FaceBoundaryConditions(Mesh,&
  Ordinates,&
  NearestFace,WithinCell,k_,&
  StreamDist,SourceDist)

TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh
REAL(KIND_MSH),INTENT(IN) :: Ordinates(:,:)
INTEGER,INTENT(IN) :: NearestFace(:,:)
INTEGER,INTENT(IN) :: WithinCell(:,:)
INTEGER,INTENT(INOUT) :: k_(:,:,:)
REAL(KIND_MSH),INTENT(INOUT) :: StreamDist(:,:,:)
REAL(KIND_MSH),INTENT(INOUT) :: SourceDist(:,:)

INTEGER :: Nm,Nk
INTEGER :: k,m,j,i
INTEGER :: jtest

!!--begin--

Nm = SIZE(Ordinates,2)
Nk = NUM_Verts(Mesh)

DO m=1,Nm
 DO k=1,Nk

  j = NearestFace(k,m)

  IF( j==0 )CYCLE

  IF( .NOT.IsBoundaryFace(Mesh,j) )CYCLE

  StreamDist(:,k,m) = 0.d0

  IF( SIZE(k_,1)>2 )THEN
   k_(3:,k,m) = 0
  END IF

  i = WithinCell(k,m)

  jtest = IntersectFace(Mesh,&
                r0            = Vert(Mesh,k)      , &
                Omega0        = -Ordinates(:,m)   , &
                SDIST         = SourceDist(k,m)   , &
                KnownInteriorCell = i             )

 END DO

END DO

!!--end--
END SUBROUTINE


SUBROUTINE CALC_InterpVerts_FixedPlane( Mesh , InterpOrder , i , &
  P_intersect , InterpVerts , fdbk , &
  Pn_front , DontUseVerts , UseVerts )
!!#### PURPOSE
!! Calculate the verts to use to interpolate with to determine the streaming
!! part of the angular flux solution, using a radial, from point-
!! of-intersection-specified metric.  Also, any vertices in front
!! of the plane of the front are not allowed.

!!#### DEPENDENCIES
USE KND_DiscreteOrdinates,ONLY: KIND_DOR                    !!((02-A-KND_DiscreteOrdinates.f90))
USE TBX_Mesh                                                !!((15-B-TBX_Mesh.f90))
USE TBX_ComputationalGeometry                               !!((09-A-TBX_ComputationalGeometry.f90))
USE FUN_Warning                                             !!((04-B-FUN_Warning.f90))
USE FUN_Random                                              !!((03-A-FUN_Random.f90))

!!#### REQUIRED INPUT
!! * the Finalized Mesh object <Mesh>
!! * the point of intersection
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER        ,INTENT(IN) :: InterpOrder
INTEGER        ,INTENT(IN) :: i
REAL(KIND_MSH) ,INTENT(IN) :: P_intersect(1:Mesh%NDim)

!!#### REQUIRED OUTPUT
!! * the list of interpolant verts for one short characteristic <InterpVerts>
INTEGER,INTENT(OUT) :: InterpVerts(1:InterpOrder+1)

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback variable <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### OPTIONAL INPUT
!! * a plane which eliminate verts from consideration if they
!!   are in front of the plane (positive signed distance to plane)
!! * list of verts to use in the interpolation, always
REAL(KIND_MSH),INTENT(IN),OPTIONAL :: Pn_front(:)
INTEGER,INTENT(IN),OPTIONAL :: DontUseVerts(:)
INTEGER,INTENT(IN),OPTIONAL :: UseVerts(:)

!!#### LOCAL VARIABLES
INTEGER :: s,Ns,Nj_,j_,j,Nkl,kl,ma,k,Ns1,Nd,j0
INTEGER,POINTER :: Order(:),kInterp(:)
REAL(KIND_MSH),POINTER :: DisInterp(:),Pn(:),U(:)
REAL(KIND_MSH) :: SDIST,Ufront(2)
LOGICAL,PARAMETER :: Noisy_=.FALSE.

!!--begin--

!initialize
InterpVerts = 0
Ns1 = 0
Nd = SIZE( P_intersect )

IF( Noisy_ )write(*,*)"begin"
!get the verts we know we can use
IF( PRESENT(UseVerts) )THEN
 Ns1 = MIN(InterpOrder+1,SIZE(UseVerts))
 !allocate
 ALLOCATE( DisInterp(Ns1) , order(Ns1) , Pn(Nd+1) , &
   U(Nd) )

 !1) get the distance of those verts are from the transverse plane
 U  = xyDIRECTION_Pn(Pn_front)
 U  = xyPERPCCW_V( U )
 Pn = xyPLANE_PV( P_intersect , U )
 !2) get the distance from the actual plane
 !Pn=Pn_front
 DO s=1,Ns1
  k = UseVerts(s)
  DisInterp(s) = ABS(xySDIST_PnP( Pn , Vert(Mesh,k) ))
 END DO

 !sort the results
 CALL Sort(DisInterp,order)
 DO s=1,Ns1
  InterpVerts(s) = UseVerts(order(s))
 END DO

END IF
!deallocate temps
DEALLOCATE( order , DisInterp , Pn )

!quick return if verts are completely specified
IF( Ns1==InterpOrder+1 )GOTO 333
IF( Noisy_ )write(*,*)"middle"

!otherwise determine the extra verts we could possibly use
NULLIFY( kInterp )
Nj_ = NUM_Faces(Mesh%Cells(i))
DO j_=1,Nj_

 !get the signed face
 j0 = Mesh%Cells(i)%FaceList(j_)

 !illumination check on this face
 U = FaceNormal(Mesh,j0)
 Ufront = xyDIRECTION_Pn(Pn_front)
 IF( xyDOT_VV( U , Ufront )>0._KIND_MSH )CYCLE

 j = ABS(j0)
 Nkl = NUM_Verts(Mesh%Faces(j))

 !iterate through verts
 DO kl=1,Nkl

  k = Mesh%Faces(j)%vertlist(kl)

  IF( PRESENT(DontUseVerts) )THEN
   IF( ANY(DontUseVerts==k) )CYCLE
  END IF

  IF( PRESENT(UseVerts) )THEN
   IF( ANY(UseVerts==k) )CYCLE
  END IF

  CALL ADD_TO_SET( kInterp , k )

 END DO
END DO

DEALLOCATE( U )

CALL FINALIZE_SET( kInterp )


!get number of verts in the extras set
IF( ASSOCIATED(kInterp) )THEN
 Ns = SIZE(kInterp)
ELSE
 Ns = 0
END IF

!if we found some more verts then figure out which ones to use
IF( Ns/=0 )THEN
 IF( Noisy_ )write(*,*)"end"
 !allocate
 ALLOCATE( DisInterp(Ns) , order(Ns) , U(Nd) , Pn(Nd+1) )

 !1) get the distance of those verts are from the transverse plane
 U  = xyDIRECTION_Pn(Pn_front)
 U  = xyPERPCCW_V( U )
 Pn = xyPLANE_PV( P_intersect , U )
 !2) get the distance from the actual plane
 !Pn=Pn_front
 DO s=1,Ns
  k = kInterp(s)
  DisInterp(s) = ABS(xySDIST_PnP( Pn , Vert(Mesh,k) ))
 END DO

 !sort the results
 CALL Sort(DisInterp,order)


 !continue where we left off, getting more verts to participate in the interpolation
 DO s=1,InterpOrder+1-Ns1
  InterpVerts(Ns1+s) = kInterp(order(s))
 END DO

 !deallocate temps
 DEALLOCATE( order , DisInterp , kInterp , Pn , U )

END IF

333 CONTINUE

!!--end--
END SUBROUTINE



FUNCTION REFINE_MoCshort( iter,dt0,fdbk , Unit ) RESULT(Refined)
!!#### PURPOSE
!! This is the driver for the MoCshort refinement/convergence checking.

!!#### PROCEDURES
USE LIB_Norm                                                !!((04-B-LIB_Norm.f90))

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
REAL    :: tin,tout,dt
INTEGER :: i,k,g,normPhiVInf_LOC
REAL(KIND_QDF) :: mag_PhiVInf,mag_PhiCL1,mag_PhiCL2,relresidPhiVInf
REAL(KIND_QDF),ALLOCATABLE :: w(:,:),flux(:,:)

!!--begin--

!0.a. begin time
CALL CPU_TIME(tin)

!0.b. optional arguments
IF( PRESENT(Unit) )THEN
 Unit_ = Unit
ELSE
 Unit_ = OutputUnit(fdbk)
END IF

!0.c. Initialize
Refined = .TRUE.

!0.d. Format statements
100 FORMAT(a8  ,2a12   ,7a22,a8,a22)
200 FORMAT(i8.5,2es12.5,7es22.13,i8.5,es22.13)


!1. print heading on first iteration.
IF( iter==0 )THEN
 WRITE(Unit_,100)"iter","spec_rad","conv_ratio",&
   "cNorm(PhiV-PhiVlast)","rel. cNorm cvg.","ptw. rel. cNorm cvg.",&
   "L2Norm(PhiC-PhiClast)","rel. L2Norm cvg.",&
   "L1Norm(PhiC-PhiClast)","rel. L1Norm cvg.",'kmax','PhiV-PhiVlast'
END IF

!2.a. calculate current norm of scalar flux - last scalar flux
!hack assume 1 group
normPhiVInf_LOC = MAXLOC( ABS( ScalarFluxV(1,:) - LastScalarFluxV(1,:) ) , 1 )
normPhiVInf_2 = ABS( ScalarFluxV(1,normPhiVInf_LOC) - LastScalarFluxV(1,normPhiVInf_LOC) )


!2.b. calculate spectral radius
IF( iter==0 .OR. normPhiVInf_1==REAL(0,KIND(normPhiVInf_1)) )THEN
 spec_rad = 1.d0 - EPSILON(1.d0)
ELSE
 spec_rad = MIN( ABS(normPhiVInf_2/normPhiVInf_1) , 1.d0-EPSILON(1.d0) )
END IF

!2.c. calculate (relative) residuals
!experimental relative error
!DO k=1,NUM_Verts(Mesh)
! DO g=1,Ng
!  IF( ScalarFluxV(g,k)/=0.d0 )THEN
!   LastScalarFluxV(g,k) = 1.d0 - ABS(LastScalarFluxV(g,k)/ScalarFluxV(g,k))
!  END IF
! END DO
!END DO
!experimental relative norm
mag_PhiVInf = ABS(ScalarFluxV(1,normPhiVInf_LOC))
relresidPhiVInf = (normPHiVInf_2/mag_PhiVInf)/(1.d0-spec_rad)

!standard relative norm
mag_PhiVInf = NormInfty(ScalarFluxV)
residPhiVInf = (normPHiVInf_2/mag_PhiVInf)/(1.d0-spec_rad)

!2.a.0. use flux as a workspace
IF( ASSOCIATED(ScalarFluxC) )THEN
    IF( .NOT.ASSOCIATED(LastScalarFluxC) )THEN
        ALLOCATE( LastScalarFluxC(SIZE(ScalarFluxF,1),SIZE(ScalarFluxF,2)) )
        LastScalarFluxC=0.d0
    END IF

    ALLOCATE( w(Ng,NUM_Cells(Mesh)), flux(Ng,NUM_Cells(Mesh)))
    
    !2.a.1. workspace to get differences
    DO i=1,NUM_Cells(Mesh)
        flux(:,i) = (ScalarFluxC(:,i)-LastScalarFluxC(:,i))
        w(:,i) = CellVolume(Mesh,i)
    END DO
    normPhiCL1_2  = NormEll1( flux, w )
    normPhiCL2_2  = NormEll2( flux, w )

    !2.a.2. workspace to get magnitude
    mag_PhiCL1  = NormEll1( ScalarFluxC , w )
    mag_PhiCL2  = NormEll2( ScalarFluxC , w )
    DEALLOCATE( w )
    CALL UpdateAndDump(fdbk_comment,fdbk,&
    s="[[MCS]] The scalar flux magnitudes are [mag_PhiCL1="//&
    TRIM(STR(mag_PhiCL1,"(Es11.4)"))//"] and [mag_PhiCL2="//&
    TRIM(STR(mag_PHiCL2,"(Es11.4)"))//"]")

    !old relative norm
    residPhiCL1  = abs(normPhiCL1_2/(1.d0-spec_rad))/mag_PhiCL1
    residPhiCL2  = abs(normPhiCL2_2/(1.d0-spec_rad))/mag_PhiCL2

ELSE
    normPhiCL1_2 = 0.d0
    normPHiCL2_2 = 0.d0
    residPhiCL1  = 0.d0
    residPhiCL2  = 0.d0
END IF


IF( iter>0 )THEN

    !3.a. write out information
    WRITE(Unit_,200)iter,spec_rad,1.d0/spec_rad,&
        normPhiVInf_2,residPhiVInf,relresidPhiVInf,&
        normPhiCL2_2,residPhiCL2,&
        normPhiCL1_2,residPhiCL1,normPhiVInf_LOC,&
        ScalarFluxV(1,normPhiVInf_LOC) - LastScalarFluxV(1,normPhiVInf_LOC)

    !3.b. update feedback
    CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MCS]] Transport sweep &
        &iteration [iter="//TRIM(STR(iter,"(i6.3)"))//"] &
        &relative scalar flux residual [residPhiVInf="//&
            TRIM(STR(residPhiVInf,"(eS9.2)"))//"/"//TRIM(STR(tolPhiVInf,"(eS9.2)"))//"] &
        &estimated spectral radius [spec_rad="//TRIM(STR(spec_rad,"(eS9.2)"))//"]." )

    CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MCS]] Transport sweep &
        &iteration [iter="//TRIM(STR(iter,"(i6.3)"))//"] &
        &pointwise relative scalar flux residual [relresidPhiVInf="//&
        TRIM(STR(relresidPhiVInf,"(eS9.2)"))//"/"//TRIM(STR(reltolPhiVInf,"(eS9.2)"))//"]." )

    !3.c. kick out on meeting residuals
    IF( residPhiVInf<=tolPhiVInf .AND. &
        relresidPhiVInf<=reltolPhiVInf )THEN

        Refined = .FALSE.

        CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MCS]] High-order solver has met &
            &residual criteria.")

    END IF

END IF

!4. update scalar flux
LastScalarFluxV = ScalarFluxV
IF( ASSOCIATED(ScalarFluxC) )THEN
    LastScalarFluxC = ScalarFluxC
END IF


IF( ASSOCIATED(ScalarFluxF) )THEN
    IF( .NOT.ASSOCIATED(LastScalarFluxF) )THEN
        ALLOCATE( LastScalarFluxF(SIZE(ScalarFluxF,1),SIZE(ScalarFluxF,2)) )
    END IF
    LastScalarFluxF = ScalarFluxF
END IF

IF( ASSOCIATED(CurrentFN) )THEN
    IF( .NOT.ASSOCIATED(LastCurrentFN) )THEN
        ALLOCATE( LastCurrentFN(SIZE(CurrentFN,1),SIZE(CurrentFN,2)) )
    END IF
    LastCurrentFN = CurrentFN
END IF

!6. update scalar flux norm
normPhiVInf_1 = normPhiVInf_2
normPhiCL1_1  = normPhiCL1_2
normPhiCL2_1  = normPhiCL2_2

!7.a. end time
CALL CPU_TIME(tout)
dt = tout-tin

!7.b. timing update
CALL UpdateAndDump(fdbk_profile,fdbk,s="[[MCS]] refinement completed after [time="//STRTIME(dt)//"]")

!7.c. update global time
dt0 = dt0 + dt

!!--end--
END FUNCTION



!!### SUBROUTINE <<SOLVE_MoCshort>>
SUBROUTINE SOLVE_MoCshort( iter , dt0 , fdbk )

!!#### PURPOSE
!! This is the driver for the MoCshort solver.

!!#### REQUIRED INPUT/OUTPUT
!! * global iteration count
!! * global runtime
!! * feedback variable <fdbk>
INTEGER        ,INTENT(INOUT) :: iter
REAL           ,INTENT(INOUT) :: dt0
TYPE(TYPE_fdbk),INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
REAL                 :: tin,tout,dt
INTEGER              :: g,k,i,j,refl_its,n
INTEGER,SAVE         :: Nrefl_its=1
INTEGER,POINTER,SAVE :: seq_m(:)=>NULL()
LOGICAL :: Pass
INTEGER :: afunit

!!--begin--


!! 0.1. Start timer.
CALL CPU_TIME(tin)

!! 0.2. Check quality on the first iteration.
IF( iter==0 )THEN
 CALL EnsureQuality(fdbk)
 CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MCS]] transport sweep &
   & skipped because [iter=0]" )
 RETURN

ELSE IF( MIN_iter==-1 )THEN
 CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MCS]] transport sweep &
   & skipped because [MIN_iter=-1]" )
 RETURN

ELSE
 CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MCS]] Entering transport sweep &
   & iteration [iter="//TRIM(STR(iter))//"]..." )
END IF


!! 1.1. Determine sequence of directions.
CALL Determine_SeqGen( BC , Nm=SIZE(Ordinates,2) , Ordinates=Ordinates , &
  seq_m=seq_m , Nrefl_its=Nrefl_its )

!! 1.2. Enter reflection iterations
DO refl_its=1,Nrefl_its

 !! 1.3.0.
 CALL CharacteristicsSolverChooser( CharacteristicsSolver )

 !! 1.3.1. Select the solver.
 SELECT CASE( CharacteristicsSolver )

  CASE( MCS_CACHED_LONG )
    CALL TransportSweep_MCL_Cached( AngularFluxV , &
      Ordinates , Mesh , pThread , WithinCell , fdbk )

  CASE( MCS_LONG )
    CALL TransportSweep_MCL_OnFly(  AngularFluxV , &
      Ordinates , Mesh , pThread , WithinCell , fdbk )

  CASE( MCS_SB )
    CALL TransportSweep_MCSB_2Dgen( Mesh , Ordinates , &
      AngularFluxV , AngularFluxF , AngularFluxC , pThread , &
      l_ , MacT , RecipSin , &
      TotSourceCellFunction , fdbk, seq_m )    
    CALL UPDATE_ReflectiveBC2(Mesh,Ordinates,AngularFluxV,AngularFluxF,BC)

  CASE( MCS_SB_PIECEWISE )
    IF( iter==1 .AND. (LEN_TRIM(affile).NE.0) )THEN
        CALL UPDATEandDUMP(fdbk_comment,fdbk,s="[[MCS]] Reading angular flux dump file ["//TRIM(affile)//"]")
        afunit=NewUnit()
        OPEN(FILE=TRIM(affile),UNIT=afunit,STATUS='old',FORM='UNFORMATTED')
        READ(afunit)n
        READ(afunit)AngularFluxV
        READ(afunit)n
        READ(afunit)AngularFluxF
        READ(afunit)n
        READ(afunit)AngularFluxC
        READ(afunit)n
        READ(afunit)TotSourceCellFunction
        CLOSE(afunit)
        CALL UPDATEandDUMP(fdbk_comment,fdbk,s="[[MCS]] Finished reading angular flux dump file ["//TRIM(affile)//"]")
    END IF
    CALL TransportSweep_MCSB_2DgenP( Mesh , Ordinates , &
      AngularFluxV , AngularFluxF , AngularFluxC , pThread , &
      l_ , MacT , RecipSin , &
      TotSourceCellFunction , fdbk, seq_m )
    CALL UPDATEandDUMP(fdbk_comment,fdbk,s="[[MCS]] Exiting TransportSweep_MCSB_2DgenP.")
    CALL UPDATE_ReflectiveBC2(Mesh,Ordinates,AngularFluxV,AngularFluxF,BC)
    CALL UPDATEandDUMP(fdbk_comment,fdbk,s="[[MCS]] Finished updating reflective BC.")

    CALL UPDATEandDUMP(fdbk_comment,fdbk,s="[[MCS]] Writing angular flux dump file.")
    WRITE(affile,'(a7,i0)')'af.dump',iter
    afunit=NewUnit()
    OPEN(FILE=affile,UNIT=afunit,STATUS='replace',FORM='UNFORMATTED')
    WRITE(afunit)SIZE(AngularFluxV)
    WRITE(afunit)AngularFluxV
    WRITE(afunit)SIZE(AngularFluxF)
    WRITE(afunit)AngularFluxF
    WRITE(afunit)SIZE(AngularFluxC)
    WRITE(afunit)AngularFluxC
    WRITE(afunit)SIZE(TotSourceCellFunction)
    WRITE(afunit)TotSourceCellFunction
    CLOSE(afunit)

  CASE( MCS_SHORT )
    CALL TransportSweep_MCS03_2Dgen( AngularFluxV , &
      pThread , WithinCell , k_ , l_ , &
      MacT , RecipSin , SourceDist , StreamDist , FrontPos , &
      TotSourceCellFunction, fdbk , seq_m )
    !! 1.3.2. Do the reflective update.
    CALL UPDATE_Reflective_2Dgen (AngularFluxV,WithinCell)

  CASE DEFAULT
    CALL UpdateAndDump(fdbk_error,fdbk,&
      s="[[MCS]] No CharacteristicsSolver specified.")

 END SELECT

END DO

CALL UPDATE(fdbk_comment,fdbk,s="[[MCS]] Finished characteristics solver loop.")

!! 2. Update Moments of the AngularFlux.
!IF(ASSOCIATED(ScalarFluxC) )WRITE(*,*)'iter,before_ho_upd=',iter,ScalarFluxC(1,1)
CALL UPDATE_HighOrder_Variables("ALL")
!IF(ASSOCIATED(ScalarFluxC) )WRITE(*,*)'iter,after_ho_upd=',iter,ScalarFluxC(1,1)
CALL UPDATE(fdbk_comment,fdbk,s="[[MCS]] Finished updating high-order variables.")
CALL SET_LowOrder_FactorCalc(CharacteristicsSolver)
CALL UPDATE(fdbk_comment,fdbk,s="[[MCS]] Finished updating low-order factors.")

![2A] change to a angular-dependent source (see [1A] elsewhere)
IF( Using_AnalyticTransportTest )THEN
 CALL UPDATE_CellAvgMom0QextA(MCS_KEY_CellFunction(CellFunctionMethod),&
   Mesh,ExtSourceCellFunction)
 ExtSourceCellFunction=ExtSourceCellFunction/c_4_times_PI
END IF

!! 3. Wrap up.

!! 3.1. Diagnostic message for negative angular fluxes.
CALL ReportNonPositivity(AngularFluxV,"AngularFluxV",(/"g","k","m"/),fdbk)
CALL ReportNonPositivity(AngularFluxF,"AngularFluxF",(/"g","j","m"/),fdbk)
CALL ReportNonPositivity(AngularFluxC,"AngularFluxC",(/"g","i","m"/),fdbk)

!! 3.2. Calculate Balances.
!! \todo Make this an optional check at some point using face-average
!! and cell-average angular fluxes from vertex values.
IF( ASSOCIATED(AngularFluxF) .AND. ASSOCIATED(AngularFluxC) .AND. CHECKING_BALANCE )THEN
 CALL CHECK_BalanceEquation(Mesh,Ordinates,&
  AngularFluxC,AngularFluxF,TotSourceCellFunction,MacT,l_,RecipSin,fdbk,&
    reltol=tolBalRel,abstol=tolBalAbs)
 CALL UPDATE(fdbk_comment,fdbk,s="[[MCS]] Finished checking balance equation.")
 IF( Using_AnalyticTransportTest )THEN
  CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MCS]] You are running&
    & an analytic test, which will report incorrect results if the&
    & analytic source is anisotropic.")
 END IF
END IF


!! 3.2. Symmetry check.
IF( Using_AFSymmetryCheck )THEN
 CALL ReportSymmetryCheck(fdbk)
END IF

!! 3.3. Calculate point values with long characteristics (no interpolation
!! error)
IF( ASSOCIATED(PointList_LongChar) )THEN
 CALL UPDATEandDUMP(fdbk_comment,fdbk,s="[[MCS]] calculating point values with &
  &long characteristics." )
 CALL TransportSweep_MCL_PointList( Unit_LongChar , PointList_LongChar , &
  Ordinates , Mesh , fdbk )
END IF

!! 3.4. detailed output of a single vertex 
IF( ASSOCIATED(PointList_DebugPsiV) )THEN
 CALL UPDATEandDUMP(fdbk_comment,fdbk,s="[[MCS]] outputting detailed psi at vertices" )
 CALL OUTPUT_DebugPsiV( Unit_DebugPsiV, iter, PointList_DebugPsiV, Mesh , fdbk )
END IF

!! 4.0. end timer
CALL CPU_TIME(tout)
dt = (tout-tin)

!! 4.1.
CALL UPDATE(fdbk_comment,fdbk,s="[[MCS]] transport sweep &
  &[iter="//TRIM(STR(iter,"(i6.3)"))//"]"//" completed in &
  &[time="//STRTIME(dt)//"]" )
CALL DUMP(fdbk)

!! 4.2. update global time
dt0 = dt0 + dt

!!--end--
END SUBROUTINE


SUBROUTINE SET_LowOrder_FactorCalc(CharacteristicsSolver,fdbk)
USE VAR_TAPACK,ONLY: FactorGenerationStyle,FactorEvalMethod !!((66-C-VAR_TAPACK.f90))
INTEGER,INTENT(IN) :: CharacteristicsSolver
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk
!!--begin--

 SELECT CASE( CharacteristicsSolver )

  CASE( MCS_CACHED_LONG )
    FactorGenerationStyle = TAP_EvalFactorFirst
    FactorEvalMethod      = TAP_IntegrateTrapezoidal

  CASE( MCS_LONG )
    FactorGenerationStyle = TAP_EvalFactorFirst
    FactorEvalMethod      = TAP_IntegrateTrapezoidal

  CASE( MCS_SB )
    FactorGenerationStyle = TAP_PsiKnown
    FactorEvalMethod      = -1 !should never need it

  CASE( MCS_SB_PIECEWISE )
    FactorGenerationStyle = TAP_PsiKnown
    FactorEvalMethod      = -1 !should never need it

  CASE( MCS_SHORT )
    FactorGenerationStyle = TAP_EvalFactorFirst
    FactorEvalMethod      = TAP_IntegrateTrapezoidal

  CASE DEFAULT
    CALL UpdateAndDump(fdbk_error,fdbk,s="[[MCS]] No CharacteristicsSolver specified.")

 END SELECT
!!--end--
END SUBROUTINE


!!### SUBROUTINE <<CharacteristicsSolverChooser>>
SUBROUTINE CharacteristicsSolverChooser( CharacteristicsSolver )
INTEGER,INTENT(OUT) :: CharacteristicsSolver
!!--begin--
CharacteristicsSolver = 0

IF( Using_LongCharacteristics )THEN

 IF( Using_CachedLongCharacteristics )THEN
  CharacteristicsSolver = MCS_CACHED_LONG
 ELSE
  CharacteristicsSolver = MCS_LONG
 END IF

ELSE IF( Using_SBCharacteristics )THEN

 IF( EdgeInterpolator==QMC_ParabolicKarpov )THEN
  CharacteristicsSolver = MCS_SB_PIECEWISE
 ELSE
  CharacteristicsSolver = MCS_SB
 END IF

ELSE

 CharacteristicsSolver = MCS_SHORT

END IF

!!--end--
END SUBROUTINE







!!### SUBROUTINE <<ReportSymmetryCheck>>
SUBROUTINE ReportSymmetryCheck(fdbk)

!!#### PURPOSE
!! Report whether or not the angular flux is symmetric.

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### LOCAL VARIABLES
REAL(KIND_AngularFlux)    :: symrelerr
LOGICAL                   :: TEST
REAL(KIND_MSH)            :: P2(2,2)
TYPE(varying_string)      :: VS

!!--begin--

!! 1. get the two points that create the enforced symmetry plane
P2 = RESHAPE( (/P1sym,P2sym/) , (/2,2/) )

!! 2a. print the info for verts
CALL PRINT_AFSymmetryCheck(Unit_AFSymmetryCheck,&
  Pn_sym=xyPLANE_P2( P2 ) , relerr=symrelerr)

!! 2b. print the info for faces
CALL PRINT_AFSymmetryCheckF(Unit_AFSymmetryCheck,&
  Pn_sym=xyPLANE_P2( P2 ) , relerr=symrelerr)

!!##SPECIAL
CALL GNUPLOT_AngularFlux()

!! 3. test it
TEST = ( symrelerr < SQRT(EPSILON(symrelerr)) )

!! 4. write the string
VS = VSRelErrMsg(symrelerr)
CALL UPDATE(fdbk_comment,fdbk,s="[[MCS]] testing symmetry, &
   &[test="//MERGE("PASS","FAIL",TEST)//"] with "//STR(VS)//".")
CALL DUMP(fdbk)

!!--end--
END SUBROUTINE





SUBROUTINE GroupIntoQuadrants( Ordinates , QuadI,QuadII,QuadIII,QuadIV )
REAL(KIND_DOR),INTENT(IN) :: Ordinates(:,:)
INTEGER,POINTER :: QuadI(:),QuadII(:),QuadIII(:),QuadIV(:)
INTEGER :: m
!!--begin--

!determine the indices of each quadrant
DO m=1,SIZE(Ordinates,2)

 IF(      IsQuadrant(1,Ordinates(:,m)) )THEN
  CALL ADD_TO_LIST( QuadI  , m )

 ELSE IF( IsQuadrant(2,Ordinates(:,m)) )THEN
  CALL ADD_TO_LIST( QuadII , m )

 ELSE IF( IsQuadrant(3,Ordinates(:,m)) )THEN
  CALL ADD_TO_LIST( QuadIII, m )

 ELSE IF( IsQuadrant(4,Ordinates(:,m)) )THEN
  CALL ADD_TO_LIST( QuadIV , m )

 ELSE
  WRITE(*,*)"ERROR: the direction must belong to one of 4 quadrants!"
  STOP
 END IF

END DO

CALL FINALIZE_LIST( QuadI  )
CALL FINALIZE_LIST( QuadII )
CALL FINALIZE_LIST( QuadIII)
CALL FINALIZE_LIST( QuadIV )

!!--end--
END SUBROUTINE


!!### SUBROUTINE <<Determin_SeqGen>>
SUBROUTINE Determine_SeqGen( BC , Nm , Ordinates , seq_m , Nrefl_its )

!!#### PURPOSE
!! Determine the angular sweep sequence. NOTE: this routine
!! does not replace a sequence if it already exists.

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: BC(1:4)
INTEGER,INTENT(IN) :: Nm
REAL(KIND_DOR),INTENT(IN) :: Ordinates(:,:)

!!#### REQUIRED OUTPUT
INTEGER,POINTER     :: seq_m(:)
INTEGER,INTENT(OUT) :: Nrefl_its

!!#### LOCAL VARIABLES
INTEGER :: bt,tp,lf,rt
INTEGER,POINTER :: quadI(:),quadII(:),quadIII(:),quadIV(:)

!!--begin--

IF( ASSOCIATED(seq_m) )RETURN
ALLOCATE( seq_m(Nm) )

NULLIFY( quadI,quadII,quadIII,quadIV )
CALL GroupIntoQuadrants( Ordinates , QuadI,QuadII,QuadIII,QuadIV )
Nrefl_its=1

!set boundary conditions
bt = BC(1)
rt = BC(2)
tp = BC(3)
lf = BC(4)

!top/bottom reflective
IF( IsReflective(bt) .AND.IsReflective(tp) )THEN
 seq_m = (/quadI,quadIV,quadII,quadIII/)

!left/right reflective
ELSE IF( IsReflective(lf).AND.IsReflective(rt) )THEN
 seq_m = (/quadI,quadIV,quadII,quadIII/)

!bottom left corner reflective
ELSE IF( IsReflective(lf).AND.IsReflective(bt) )THEN
 seq_m = (/quadIII,quadIV,quadII,quadI/)

!bottom right corner reflective
ELSE IF( IsReflective(rt).AND.IsReflective(bt) )THEN
 seq_m = (/quadIV,quadI,quadIII,quadII/)

!top left corner reflective
ELSE IF( IsReflective(lf).AND.IsReflective(tp) )THEN
 seq_m = (/quadII,quadIII,quadI,quadIV/)

!top right corner reflective
ELSE IF( IsReflective(rt).AND.IsReflective(tp) )THEN
 seq_m = (/quadI,quadII,quadIV,quadIII/)

ELSE
 seq_m = (/quadI,quadII,quadIII,quadIV/)

END IF

DEALLOCATE( quadI,quadII,quadIII,quadIV )

!!--end--
END SUBROUTINE


FUNCTION IsQuadrant( Quad , Direction ) RESULT(Is)
INTEGER,INTENT(IN) :: Quad
REAL(KIND_DOR),INTENT(IN) :: Direction(:)
LOGICAL :: Is
!!--begin--

Is = .FALSE.

SELECT CASE(Quad)
 CASE(1) ; Is = Direction(1)>=0._KIND_DOR .AND. Direction(2)>=0._KIND_DOR
 CASE(2) ; Is = Direction(1)<=0._KIND_DOR .AND. Direction(2)>=0._KIND_DOR
 CASE(3) ; Is = Direction(1)<=0._KIND_DOR .AND. Direction(2)<=0._KIND_DOR
 CASE(4) ; Is = Direction(1)>=0._KIND_DOR .AND. Direction(2)<=0._KIND_DOR
END SELECT

!!--end--
END FUNCTION


!SUBROUTINE Visualize_AngularFlux(AngularFluxV,m_map,Message)
!!!#### REQUIRED INPUT
!REAL(KIND_AngularFlux),POINTER :: AngularFluxV(:,:,:)
!INTEGER,INTENT(IN) :: m_map(:,:)
!
!!!#### OPTIONAL INPUT
!CHARACTER(LEN=*,KIND=KIND_S),OPTIONAL,INTENT(IN) :: Message
!
!!!#### LOCAL VARIABLES
!INTEGER :: g,Ng,k,Nk,mp,Nmp
!
!!!--begin--
!Ng = SIZE(AngularFluxV,1)
!Nk = SIZE(AngularFluxV,2)
!Nmp = SIZE(m_map,1)
!
!DO
!  IF( PRESENT(Message) )THEN
!   CALL Print_Text(s=InfoPrompt//Message)
!  END IF
!  CALL Print_Text(s=InfoPrompt//"Enter the energy group, vertex, &
!    &and azimuthal index.  (Enter 0s to exit the loop.)")
!  CALL userPrompt()
!  READ(*,*)g,k,mp
!  IF( g<0 .OR. g>Ng )CYCLE
!  IF( k<0 .OR. k>Nk )CYCLE
!  IF( mp<0 .OR. mp>Nmp )CYCLE
!  IF( g==0 .OR. k==0 .OR. mp==0 )EXIT
!
!  CALL UPDATE_Function_AngularFlux( g,k,mp )
!
!  CALL Print_Text(s="## AngularFluxV(g="//TRIM(STR(g))//",k="//&
!    TRIM(STR(k))//",mp="//TRIM(STR(mp))//")")
!  CALL PRINT_Function1( Function_AngularFlux , (/0._KIND_DOR,6.14_KIND_DOR/) )
!
! END DO
!END SUBROUTINE





SUBROUTINE Monotonize( psi , x1,x2,x3 , &
             y1,y2,y3 , c12,c23 ,k,m ,MAX_k,MAX_m,NoBacksies)
!!#### PURPOSE
!! Based on the values <(/y1,y2,y3/)> and psi, potentially
!! apply a monotonization, such that psi is not a maximum
!! or minimum.

!!#### REQUIRED INPUT/OUTPUT
REAL(KIND_AngularFlux),INTENT(INOUT) :: psi

!!#### REQUIRED INPUT
REAL(KIND_AngularFlux),INTENT(IN) :: x1,x2,x3
REAL(KIND_AngularFlux),INTENT(IN) :: y1,y2,y3

!!#### OPTIONAL INPUT
REAL(KIND_AngularFlux),OPTIONAL,INTENT(IN) :: c12,c23
INTEGER               ,OPTIONAL,INTENT(IN) :: k,m,MAX_k,MAX_m
LOGICAL               ,OPTIONAL,INTENT(IN) :: NoBacksies

!!#### LOCAL VARIABLES
INTEGER,SAVE,ALLOCATABLE :: Monotone(:,:)
LOGICAL :: MinMonotone,MaxMonotone,NoBacksies_

!!--begin--

!Initialization logic to keep track of if this characteristic
!needs monotonization or reset if NoBacksies is false
IF( Using_NoBacksies .AND. (&
  PRESENT(NoBacksies) .AND. PRESENT(k) .AND. PRESENT(m) .AND. &
  PRESENT(MAX_k) .AND. PRESENT(MAX_m) )&
 )THEN
 IF( NoBacksies )THEN
  NoBacksies_ = .TRUE.
  IF( .NOT.ALLOCATED(Monotone) )THEN
   ALLOCATE( Monotone(MAX_k,MAX_m) )
   Monotone = 0
  END IF
 ELSE
  NoBacksies_ = .FALSE.
  IF( ALLOCATED(Monotone) )THEN
   DEALLOCATE( Monotone )
  END IF
 END IF
ELSE
 NoBacksies_ = .FALSE.
END IF


!For this combination, determine whether to enforce monotonization
MinMonotone = .FALSE.
MaxMonotone = .FALSE.
IF( NoBacksies_ )THEN
 MinMonotone = Monotone(k,m)==-1
 MaxMonotone = Monotone(k,m)==+1
END IF


!between x1 and x2
IF( c12<0._KIND_AngularFlux )THEN

 !minimum
 IF( (psi<y1 .AND. psi<y2).OR.MinMonotone )THEN

  IF( Using_MonoLin )THEN
   psi = ( y1*x2 - y2*x1 )/( x2 - x1 )
  ELSE
   psi = MIN(y1,y2)
  END IF
  !WRITE(43,"(5(e12.4,2x))")Vert(Mesh,k),Ordinates(:,m)
  COUNT_Monotonizations = COUNT_Monotonizations + 1

  !add this to the monotonization list
  IF( NoBacksies_ )THEN
   Monotone(k,m) = -1
  END IF


 !maximum
 ELSE IF( (psi>y1 .AND. psi>y2).OR.MaxMonotone )THEN
  IF( Using_MonoLin )THEN
   psi = ( y1*x2 - y2*x1 )/( x2 - x1 )
  ELSE
   psi = MAX(y1,y2)
  END IF
  !WRITE(44,"(5(e12.4,2x))")Vert(Mesh,k),Ordinates(:,m)
  COUNT_Monotonizations = COUNT_Monotonizations + 1

  !add this to the monotonization list
  IF( NoBacksies_ )THEN
   Monotone(k,m) = +1
  END IF

 END IF

!between x2 and x3
ELSE IF( c23<0._KIND_AngularFlux )THEN

 !minimum
 IF( (psi<y2 .AND. psi<y3).OR.MinMonotone )THEN
  IF( Using_MonoLin )THEN
   psi = ( y2*x3 - y3*x2 )/( x3 - x2 )
  ELSE
   psi = MIN(y2,y3)
  END IF
  !WRITE(45,"(5(e12.4,2x))")Vert(Mesh,k),Ordinates(:,m)
  COUNT_Monotonizations = COUNT_Monotonizations + 1

  !add this to the monotonization list
  IF( NoBacksies_ )THEN
   Monotone(k,m) = -1
  END IF


 !maximum
 ELSE IF( (psi>y2 .AND. psi>y3).OR.MaxMonotone )THEN
  IF( Using_MonoLin )THEN
   psi = ( y2*x3 - y3*x2 )/( x3 - x2 )
  ELSE
   psi = MAX(y2,y3)
  END IF
  !WRITE(46,"(5(e12.4,2x))")Vert(Mesh,k),Ordinates(:,m)
  COUNT_Monotonizations = COUNT_Monotonizations + 1

  !add this to the monotonization list
  IF( NoBacksies_ )THEN
   Monotone(k,m) = +1
  END IF

 END IF


END IF

!!--end--
END SUBROUTINE






SUBROUTINE Mono1S( f , x , f0 , &
                   c ,k,m ,MAX_k,MAX_m,&
                   NoBacksies,SetBacksies)
!!#### PURPOSE
!! Based on the values we have <(/f/)> and <(/x/)> and an estimate <f0>
!! at <x0=0>, we see if we need a monotonization, such that f0
!! cannot be a maximum or minimum.

!!#### REQUIRED INPUT
REAL(KIND_AngularFlux),INTENT(IN) :: f(:)
REAL(KIND_AngularFlux),INTENT(IN) :: x(SIZE(f))

!!#### REQUIRED INPUT/OUTPUT
REAL(KIND_AngularFlux),INTENT(INOUT) :: f0

!!#### OPTIONAL INPUT
REAL(KIND_AngularFlux),OPTIONAL,INTENT(IN) :: c(SIZE(f)-1)
INTEGER               ,OPTIONAL,INTENT(IN) :: k,m
INTEGER               ,OPTIONAL,INTENT(IN) :: MAX_k,MAX_m
LOGICAL               ,OPTIONAL,INTENT(IN) :: NoBacksies
LOGICAL               ,OPTIONAL,INTENT(IN) :: SetBacksies

!!#### LOCAL VARIABLES
LOGICAL,SAVE :: Using_NoBacksies_ = .FALSE.
INTEGER,SAVE,ALLOCATABLE :: Monotone(:,:)
LOGICAL :: MinMonotone,MaxMonotone,NoBacksies_
REAL(KIND_AngularFlux) :: c_(SIZE(f)-1)
INTEGER :: n

!!--begin--
Using_NoBacksies_ = DEFAULT( SetBacksies , Using_NoBacksies_ )

!Initialization logic to keep track of if this characteristic
!needs monotonization or reset if NoBacksies is false
IF( Using_NoBacksies_ .AND. (&
  PRESENT(NoBacksies) .AND. PRESENT(k) .AND. PRESENT(m) .AND. &
  PRESENT(MAX_k) .AND. PRESENT(MAX_m) )&
 )THEN
 IF( NoBacksies )THEN
  NoBacksies_ = .TRUE.
  IF( .NOT.ALLOCATED(Monotone) )THEN
   ALLOCATE( Monotone(MAX_k,MAX_m) )
   Monotone = 0
  END IF
 ELSE
  NoBacksies_ = .FALSE.
  IF( ALLOCATED(Monotone) )THEN
   DEALLOCATE( Monotone )
  END IF
 END IF
ELSE
 NoBacksies_ = .FALSE.
END IF


!For this combination, determine whether to enforce monotonization
MinMonotone = .FALSE.
MaxMonotone = .FALSE.
IF( NoBacksies_ )THEN
 MinMonotone = Monotone(k,m)==-1
 MaxMonotone = Monotone(k,m)==+1
END IF

IF( PRESENT(c) )THEN
 c_ = c
ELSE
 DO n=1,SIZE(f)-1
  c_(n) = x(n)*x(n+1)
 END DO
END IF

!look for interval and select monotonization
DO n=1,SIZE(f)-1


 !unknown is between x(n) and x(n+1)
 IF( c_(n)<0._KIND_AngularFlux )THEN

  !minimum
  IF( f0<f(n) .AND. f0<f(n+1) .OR. MinMonotone )THEN

   IF( Using_MonoLin )THEN
    f0 = ( f(n)*x(n+1) - f(n+1)*x(n) )/( x(n+1) - x(n) )
   ELSE
    f0 = MIN(f(n),f(n+1))
   END IF

   COUNT_Monotonizations = COUNT_Monotonizations + 1

   !add this to the monotonization list
   IF( NoBacksies_ )THEN
    Monotone(k,m) = -1
   END IF

  !maximum
  ELSE IF( f0>f(n) .AND. f0>f(n+1) .OR. MaxMonotone )THEN

   IF( Using_MonoLin )THEN
    f0 = ( f(n)*x(n+1) - f(n+1)*x(n) )/( x(n+1) - x(n) )
   ELSE
    f0 = MIN(f(n),f(n+1))
   END IF

   COUNT_Monotonizations = COUNT_Monotonizations + 1

   !add this to the monotonization list
   IF( NoBacksies_ )THEN
    Monotone(k,m) = +1
   END IF

  END IF

 END IF


END DO

!!--end--
END SUBROUTINE



SUBROUTINE MonoCount(COUNT_Monotonizations,NUM_Characteristics,fdbk)

INTEGER,INTENT(IN) :: COUNT_Monotonizations,NUM_Characteristics
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

REAL(KIND_AngularFlux) :: f

!!--begin--

f = 100*REAL(COUNT_Monotonizations)/REAL(NUM_Characteristics)
CALL Update(fdbk_comment,fdbk,s="[[MCS]] monotonization used [monos="//&
  TRIM(STR(COUNT_Monotonizations))//"/"//TRIM(STR(NUM_Characteristics))//"] or "//&
  TRIM(STR(f,"(f4.1)"))//"% all characteristics")

!!--end--
END SUBROUTINE



SUBROUTINE NegativeMsg(prepend,y1,y2,y3,g,k1,k2,k3,m__,fdbk)
INTEGER,INTENT(IN) :: g,k1,k2,k3,m__
REAL(KIND_AngularFlux),INTENT(IN) :: y1,y2,y3
CHARACTER(*),INTENT(IN) :: prepend
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!--begin--


IF( y1<0._KIND_AngularFlux )THEN
 CALL Update(fdbk_warning,fdbk,s=prepend//" for y1(k="//TRIM(STR(k1))//",m="//TRIM(STR(m__))//")="//TRIM(STR(y1,"(E10.3)")))
END IF

IF( y2<0._KIND_AngularFlux )THEN
 CALL Update(fdbk_warning,fdbk,s=prepend//" for y2(k="//TRIM(STR(k2))//",m="//TRIM(STR(m__))//")="//TRIM(STR(y2,"(E10.3)")))
END IF

IF( y3<0._KIND_AngularFlux )THEN
 CALL Update(fdbk_warning,fdbk,s=prepend//" for y3(k="//TRIM(STR(k3))//",m="//TRIM(STR(m__))//")="//TRIM(STR(y3,"(E10.3)")))
END IF

CALL Dump(fdbk)

!!--end--
END SUBROUTINE



SUBROUTINE TransportSweep_MCS03_2Dgen( AngularFluxV , &
  pThread , WithinCell , k_ , l_ , &
  MacT , RecipSin , SourceDist , StreamDist , FrontPos , &
  TotSourceCellFunction , fdbk , seq_m )

!!#### PURPOSE
!! Performs a transport sweep (TransportSweep) using a Method of
!! Characteristics algorithm with Short (intracell) quadratic interpolation.

!!#### METHOD
!! The angular flux (the first argument) at each vertex k is updated via
!! a transport sweep in all directions m through all spatial cells i.
!
!!#### DETAILS
!! This routine is only for the 2D case where each azimuthal
!! angle has the same set of polar directions associated with it (or each
!! polar angle has the same set of azimuthal directions associated
!! with it---same thing) denoted UPA (Uniform Polar Azimuthal ordinates).


!!#### EXTERNAL KINDS
USE KND_DiscreteOrdinates,ONLY: KIND_DOR                    !!((02-A-KND_DiscreteOrdinates.f90))
USE KND_Mesh             ,ONLY: KIND_MSH                    !!((05-B-KND_Mesh.f90))
USE KND_MoCshort         ,ONLY: KIND_AngularFlux            !!((03-A-KND_MoCshort.f90))
USE KND_XSExpansion      ,ONLY: KIND_Mac                    !!((02-A-KND_XSExpansion.f90))

!!#### GLOBAL USER MODULES
USE USR_pThread                                             !!((03-A-USR_pThread.f90))
USE USR_fdbk                                                !!((08-C-USR_fdbk.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE


!!#### REQUIRED INPUT
!! * source function
!! * path threads
!! * which cell vertex k direction ma is in
!! * mapping of cell indices i to material indices l
!! * total cross section
!! * reciprocal of the sin of the azimuthal angle
!! * distance across cell in which the source accumulates
!! * position of intra-cell vertices on the front plane
REAL(KIND_AngularFlux),INTENT(IN) :: TotSourceCellFunction(:,:,:)
TYPE(TYPE_PThread)    ,INTENT(IN) :: pThread(:)
INTEGER               ,INTENT(IN) :: WithinCell(:,:)
INTEGER               ,INTENT(IN) :: k_(:,:,:)
INTEGER               ,INTENT(IN) :: l_(:)
REAL(KIND_Mac)        ,INTENT(IN) :: MacT(:,:)
REAL(KIND_DOR)        ,INTENT(IN) :: RecipSin(:)
REAL(KIND_MSH)        ,INTENT(IN) :: SourceDist(:,:)
REAL(KIND_MSH)        ,INTENT(IN) :: StreamDist(:,:,:)
REAL(KIND_MSH)        ,INTENT(IN) :: FrontPos(:,:,:)

!!#### REQUIRED OUTPUT
!! * angular flux
REAL(KIND_AngularFlux),POINTER :: AngularFluxV(:,:,:)

!!#### OPTIONAL INPUT
INTEGER,OPTIONAL,INTENT(IN) :: seq_m(:)

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object [fdbk]
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
INTEGER :: m0,m,m_refl
INTEGER :: k,k__,k1,k2,k3,Nm,Nk,Ng
INTEGER :: g,i,l,p
REAL(KIND_AngularFlux) :: x1,x2,x3,y1,y2,y3,c12,c23,c31
REAL(KIND_AngularFlux) :: psi,psi0,ExtSourceC1_char
REAL(KIND_AngularFlux) :: Ln(2,2),P_base(2,3),err
LOGICAL,PARAMETER :: UpdateReflOnFly = .TRUE.
LOGICAL,PARAMETER :: CheckAccuracy=.FALSE.
INTEGER :: NaNCheck,j,jd
LOGICAL,PARAMETER :: Noisy_=.FALSE.
LOGICAL :: gtl12,gtl23

!!--begin--

CALL UpdateAndDump(fdbk_comment,fdbk,&
 s="[[MCS]] Entering [TransportSweep_MCS03_2Dgen] solver...")

!get indices
Nm = SIZE(AngularFluxV,3)
Nk = SIZE(AngularFluxV,2)
Ng = SIZE(AngularFluxV,1)
COUNT_Monotonizations = 0
y3 = -HUGE(y3)

!enter main loop over polar angles
m_loop: DO m0 = 1,SIZE(pthread)
 !apply a special sequencing to ma
 IF( PRESENT(seq_m) )THEN
  m = seq_m(m0)
 ELSE
  m = m0
 END IF

 !proceed down each path
 path_loop: DO p   = 1,SIZE(pthread(m)%path)

  !in the proper order of verts
  order_loop: DO k__ = 1,SIZE(pthread(m)%path(p)%order)

   !get the vert index
   k = pthread(m)%path(p)%order(k__)

   !get the cell index
   i = WithinCell(k,m)

   IF( Noisy_ )WRITE(*,*)"k|m|i = ",k,"|",m,"|",i

   !cycle if there is no cell index
   !a) boundary vert with fixed value
   IF( i==0 )THEN

    IF( Noisy_ )WRITE(*,*)" fixed bc!"
    CYCLE

   !b) reflective boundary
   ELSE IF( i<0 )THEN

    !update reflections on the fly
    IF( UpdateReflOnFly )THEN

     !for each azimuthal index determine the reflection
     IF( Noisy_ )WRITE(*,*)" reflective BC (updating on the fly)"
     m_refl = ABS(i)
     IF( Noisy_ )WRITE(*,*)"Psi("//TRIM(STR(m))//")=Psi("//TRIM(STR(m_refl))//")"
     AngularFluxV( : , k , m ) = AngularFluxV( : , k , m_refl )

    ELSE
     IF( Noisy_ )WRITE(*,*)" reflective BC (not updating on the fly)"
    END IF

    !this reflective direction is finished
    CYCLE

   END IF

   !if there is a cell index, we continue
   jd = 0
   j = NearestFace(k,m)
   IF( j/=0 )THEN
    IF( IsBoundaryFace(Mesh,j) )THEN
     !get the domain face we are on
     jd = DomainFace(Mesh,j)
     psi = EVAL_fixedBC( BC , FixedAngularFlux,FunctionAngularFlux , fdbk, jd , Mesh , m=m , r=FaceCentroid(Mesh,j) )
    END IF
   END IF

   !get the material index
   l = l_(i)
   IF( Noisy_ )WRITE(*,*)" material l=",l

   !get the two vertices for the interpolation of the scalar flux
   k1 = k_(1,k,m)
   k2 = k_(2,k,m)
   k3 = MERGE(k_(3,k,m),0,InterpOrder>=2)
   IF( Noisy_ )THEN
    WRITE(*,*)"k1=",k1
    WRITE(*,*)"k2=",k2
    WRITE(*,*)"k3=",k3
   END IF

   !get front positions
   x1 = FrontPos( 1 , k , m )
   x2 = FrontPos( 2 , k , m )
   IF( k3/=0 )x3 = FrontPos( 3 , k , m )
   IF( Noisy_ )THEN
    WRITE(*,*)"x1=",x1
    WRITE(*,*)"x2=",x2
    WRITE(*,*)"x3=",x3
   END IF

   !energy groups loop
   g_loop: DO g=1,SIZE(MacT,1)

    !NaN checking
    NaNCheck = 0
    IF( Noisy_ )WRITE(*,*)"g=",g

    y1 = 0._KIND_AngularFlux
    y2 = 0._KIND_AngularFlux
    y3 = 0._KIND_AngularFlux

    !get angular fluxes
    IF( k1/=0 )y1 = AngularFluxV(g,k1,m)
    IF( k2/=0 )y2 = AngularFluxV(g,k2,m)
    IF( k3/=0 )y3 = AngularFluxV(g,k3,m)
    IF( Noisy_ )THEN
     WRITE(*,*)"y1=",y1
     WRITE(*,*)"y2=",y2
     WRITE(*,*)"y3=",y3
    END IF
    CALL NegativeMsg("[[MCS]] negative initial AngularFluxV ",y1,y2,y3,g,k1,k2,k3,m,fdbk)

    !silly skip ahead
    IF( jd/=0 )GOTO 333

    !determine full flux attenuation terms
    y1 = y1*exp( -MacT(g,l)*StreamDist(1,k,m)*RecipSin(m) )
    y2 = y2*exp( -MacT(g,l)*StreamDist(2,k,m)*RecipSin(m) )
    IF( k3/=0 )y3 = y3*exp( -MacT(g,l)*StreamDist(3,k,m)*RecipSin(m) )
    IF( Noisy_ )THEN
     WRITE(*,*)"y1s=y1*exp(-MacT*sxy/sint)=",y1
     WRITE(*,*)"y2s=y2*exp(-MacT*sxy/sint)=",y2
     WRITE(*,*)"y3s=y3*exp(-MacT*sxy/sint)=",y3
    END IF
    CALL NegativeMsg("[[MCS]] negative attenuated AngularFluxV ",y1,y2,y3,g,k1,k2,k3,m,fdbk)

    !get new points after first half of characteristic evaluation
    P_base(:,1) = Vert(Mesh,k1) + StreamDist(1,k,m)*Ordinates(1:2,m)*RecipSin(m)
    P_base(:,2) = Vert(Mesh,k2) + StreamDist(2,k,m)*Ordinates(1:2,m)*RecipSin(m)
    IF( k3/=0 )THEN
     P_base(:,3) = Vert(Mesh,k3) + StreamDist(3,k,m)*Ordinates(1:2,m)*RecipSin(m)
    END IF

    !calculate source update angular fluxes
    IF( SourceOrder<=0 )THEN
      y1 = y1 + SourceIntegral0( TotSourceCellFunction(1,g,i) , StreamDist(1,k,m) , &
                                 RecipSin(m) , MacT(g,l) )
      y2 = y2 + SourceIntegral0( TotSourceCellFunction(1,g,i) , StreamDist(2,k,m) , &
                                 RecipSin(m) , MacT(g,l) )
     IF( k3/=0 )y3 = y3 + SourceIntegral0( TotSourceCellFunction(1,g,i) , StreamDist(3,k,m) , &
                                           RecipSin(m) , MacT(g,l) )
    ELSE

     ExtSourceC1_char = DOT_PRODUCT( Ordinates(1:2,m) , ExtSourceCellFunction(2:3,g,i) )

     y1 = y1 + SourceIntegral1( TotSourceCellFunction(1,g,i) , StreamDist(1,k,m) , &
                                RecipSin(m) , MacT(g,l) , &
                                g , i , P_base(:,1) , CoeffScalarFluxM(g,l) , &
                                ExtSourceCellFunction(1,g,i) , ExtSourceC1_char , &
                                Ordinates(1:2,m)*RecipSin(m) )

     y2 = y2 + SourceIntegral1( TotSourceCellFunction(1,g,i) , StreamDist(2,k,m) , &
                                RecipSin(m) , MacT(g,l) , &
                                g , i , P_base(:,2) , CoeffScalarFluxM(g,l) , &
                                ExtSourceCellFunction(1,g,i) , ExtSourceC1_char , &
                                Ordinates(1:2,m)*RecipSin(m) )

     IF( k3/=0 )THEN
      y3 = y3 + SourceIntegral1( TotSourceCellFunction(1,g,i) , StreamDist(3,k,m) , &
                                 RecipSin(m) , MacT(g,l) , &
                                 g , i , P_base(:,3) , CoeffScalarFluxM(g,l) , &
                                 ExtSourceCellFunction(1,g,i) , ExtSourceC1_char , &
                                 Ordinates(1:2,m)*RecipSin(m) )
     END IF

    END IF
    CALL NegativeMsg("[[MCS]] negative source-integrated AngularFluxV ",y1,y2,y3,g,k1,k2,k3,m,fdbk)

    IF( Noisy_ )THEN
     WRITE(*,*)"y1ss=y1s+int(q(s)*exp(-mact*sxy/sint),s=0,smax)=",y1
     WRITE(*,*)"y2ss=y2s+int(q(s)*exp(-mact*sxy/sint),s=0,smax)=",y2
     WRITE(*,*)"y3ss=y3s+int(q(s)*exp(-mact*sxy/sint),s=0,smax)=",y3
    END IF

    !checks #2
    IF( y1/=y1 )THEN
     NaNCheck = 2
     IF( Noisy_ )WRITE(*,*)"y1=NaN"
    END IF

    IF( y2/=y2 )THEN
     NaNCheck = 3
     IF( Noisy_ )WRITE(*,*)"y2=NaN"
    END IF

    IF( y3/=y3 )THEN
     NaNCheck = 4
     IF( Noisy_ )WRITE(*,*)"y3=NaN"
    END IF

    !interpolate
    psi = EVAL_Interpolation( k1,k2,k3 , x1,x2,x3 , y1,y2,y3 ,k,m,Nk,Nm)
    IF( Noisy_ )THEN
     WRITE(*,*)"psi_interp=",psi
    END IF

    !checks #3
    IF( psi<0._KIND_AngularFlux )THEN
     CALL NegativeMsg("[[MCS]] negative AngularFluxV after interpolation ",y1,y2,y3,g,k1,k2,k3,m,fdbk)
     psi = 0._KIND_AngularFlux
    END IF

    IF( NaNCheck/=0 )THEN
     CALL NegativeMsg("[[MCS]] NaN AngularFluxV fix-up applied ",y1,y2,y3,g,k1,k2,k3,m,fdbk)
     NaNCheck = 0
     34 CONTINUE
    END IF

    333 CONTINUE
    !calculate streaming
    IF( InterpPlaneU==MCS_diag .OR. jd/=0 )THEN
     IF( SourceDist(k,m)==ERROR(1._KIND_MSH) )THEN
      CALL UpdateAndDump(fdbk_warning,fdbk,s="[[MCS]] A SourceDist element is not initialized&
        & and will be used now and probably cause a fatal error.")
     END IF
     psi = psi*exp( -MacT(g,l)*SourceDist(k,m)*RecipSin(m) )

     !calculate source update angular fluxes
     IF( SourceOrder<=0 )THEN
      psi = psi + SourceIntegral0( TotSourceCellFunction(1,g,i) , SourceDist(k,m) , &
                                   RecipSin(m) , MacT(g,l) )
     ELSE
      ExtSourceC1_char = DOT_PRODUCT( Ordinates(1:2,m) , ExtSourceCellFunction(2:3,g,i) )
      psi = psi + SourceIntegral1( TotSourceCellFunction(1,g,i) , SourceDist(k,m) , &
                                   RecipSin(m) , MacT(g,l) , &
                                   g , i , Vert(Mesh,k) , CoeffScalarFluxM(g,l) , &
                                   ExtSourceCellFunction(1,g,i) , ExtSourceC1_char , &
                                   Ordinates(1:2,m)*RecipSin(m) )
     END IF

    END IF

    IF( Noisy_ )THEN
     WRITE(*,*)"psi_fin=",psi
    END IF

    !update angular flux
    IF( Noisy_ )WRITE(65,"(3i6,4es21.13)")iter,k,m,y1,y2,y3,psi
    AngularFluxV( g , k , m ) = psi


   END DO g_loop

  END DO order_loop

 END DO path_loop

END DO m_loop

CALL MonoCount(COUNT_Monotonizations,SIZE(AngularFluxV(1,:,:)),fdbk)

!!--end--
END SUBROUTINE



SUBROUTINE TransportSweep_MCSB_2Dgen( Mesh , Directions , &
     AngularFluxV , AngularFluxF , AngularFluxC , pThread , &
     l_ , MacT , RecipSin , &
     TotSourceCellFunction , fdbk , seq_m )

!!#### PURPOSE
!! Performs a transport sweep (TransportSweep) using a Method of
!! Characteristics Subcell balance algorithm solving for
!! Vertex, Face-Average, and Cell-average Angular Fluxes.

!!#### EXTERNAL KINDS
USE KND_DiscreteOrdinates,ONLY: KIND_DOR                    !!((02-A-KND_DiscreteOrdinates.f90))
USE KND_Mesh             ,ONLY: KIND_MSH                    !!((05-B-KND_Mesh.f90))
USE KND_MoCshort         ,ONLY: KIND_AngularFlux            !!((03-A-KND_MoCshort.f90))
USE KND_XSExpansion      ,ONLY: KIND_Mac                    !!((02-A-KND_XSExpansion.f90))

!!#### GLOBAL USER MODULES
USE USR_fdbk                                                !!((08-C-USR_fdbk.f90))
USE USR_pThread                                             !!((03-A-USR_pThread.f90))
USE USR_SBCharacteristics                                   !!((26-C-USR_SBCharacteristics.f90))
!USE TBX_SBCharacteristics                                  !!((27-C-TBX_SBCharacteristics.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### REQUIRED INPUT
!! * mesh
!! * order of cells to visit
!! * mapping of cell indices i to material indices l
!! * total cross section
!! * reciprocal of the sin of the azimuthal angle
!! * flat source
!! * linear source
TYPE(TYPE_Mesh)       ,INTENT(IN) :: Mesh
REAL(KIND_AngularFlux),INTENT(IN) :: Directions(:,:)
TYPE(TYPE_PThread)    ,INTENT(IN) :: pThread(:)
INTEGER               ,INTENT(IN) :: l_(:)
REAL(KIND_Mac)        ,INTENT(IN) :: MacT(:,:)
REAL(KIND_DOR)        ,INTENT(IN) :: RecipSin(:)
REAL(KIND_AngularFlux),POINTER :: TotSourceCellFunction(:,:,:)

!!#### REQUIRED OUTPUT
!! * angular fluxes
REAL(KIND_AngularFlux),POINTER :: AngularFluxV(:,:,:)
REAL(KIND_AngularFlux),POINTER :: AngularFluxF(:,:,:)
REAL(KIND_AngularFlux),POINTER :: AngularFluxC(:,:,:)

!!#### OPTIONAL INPUT
INTEGER,OPTIONAL,INTENT(IN) :: seq_m(:)

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object [fdbk]
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL PARAMETERS
LOGICAL,PARAMETER :: Noisy_ = .FALSE.

!!#### LOCAL VARIABLES
INTEGER                            :: m0,m,p,i0,i,g,l
INTEGER                            :: Ng
INTEGER                            :: n,Nfpc
INTEGER               ,POINTER     :: InSide(:),OutSide(:)
INTEGER               ,POINTER     :: InVert(:),OutVert(:)
REAL(KIND_AngularFlux),ALLOCATABLE :: PsiIn(:,:,:)
REAL(KIND_AngularFlux),POINTER     :: Sli(:,:)
INTEGER                            :: kin1,kin2,jin
INTEGER                            :: kout1,kout2,jout
INTEGER                            :: id,xxx
REAL(KIND_AngularFlux)             :: q_(3),U(2),corig(3),crot(3),theta,psi1,psi2,psiface_int,psicell_int
REAL(KIND_AngularFlux)             :: MAX_BalCheck,MAX_SBBalCheck
REAL(KIND_AngularFlux)             :: MAX_relBalCheck,MAX_relSBBalCheck
REAL(KIND_AngularFlux)             :: BalCheck,SBBalCheck,AvgSource
REAL(KIND_AngularFlux)             :: relBalCheck,relSBBalCheck
REAL(KIND_AngularFlux),ALLOCATABLE :: psiface_avg(:),psicell_avg(:),face_wts(:),cell_wts(:)
LOGICAL :: check_jin,check_jout,check_kin1,check_kin2
LOGICAL :: check_i,check_kout1,check_kout2
INTEGER :: kout
INTEGER            :: Nh,itest,mtest
LOGICAL,SAVE       :: Cache_run=.true.
CHARACTER(20),SAVE :: Cache_file=''
INTEGER,SAVE       :: Cache_unit=0
INTEGER,SAVE       :: Nmax=0
!!--begin--

NULLIFY( InSide, OutSide, InVert, OutVert, Sli )

!cache file handling (open for writing or reading)
IF( Using_cache .AND. Cache_run )THEN
    !write
    Cache_unit=NewUnit()
    CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MCS]] Opening a new cache file (.cache/slicing)")
    OPEN(FILE='.cache/slicing',UNIT=Cache_unit,ACTION='WRITE',STATUS='REPLACE',FORM='UNFORMATTED',ACCESS='SEQUENTIAL')
ELSE IF ( Using_cache .AND. (.NOT.Cache_run) )THEN
    !read
    Cache_unit=NewUnit()
    CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MCS]] Opening an old cache file (.cache/slicing) with Nmax="//TRIM(STR(Nmax)))
    ALLOCATE( OutSide(Nmax-1) , InSide(Nmax-1) , Sli(3,Nmax) , InVert(Nmax) , OutVert(Nmax) )
    OPEN(FILE='.cache/slicing',UNIT=Cache_unit,ACTION='READ',STATUS='OLD',FORM='UNFORMATTED',ACCESS='SEQUENTIAL')
END IF

IF( Noisy_ )THEN
 WRITE(*,*)"iter=",iter
END IF

id   = 0
Ng   = SIZE(AngularFluxV,1)
Nfpc = MAX_FacesPerCell(Mesh)

xxx = qmc_ThinEdgeCount(init=.TRUE.)

ALLOCATE( PsiIn(3,Ng,Nfpc) )
ALLOCATE( psiface_avg(Nfpc),psicell_avg(Nfpc),cell_wts(Nfpc),face_wts(Nfpc) )

MAX_SBBalCheck    = 0._KIND_AngularFlux
MAX_BalCheck      = 0._KIND_AngularFlux
MAX_relSBBalCheck = 0._KIND_AngularFlux
MAX_relBalCheck   = 0._KIND_AngularFlux

!enter main loop over angles
m_loop: DO m0 = 1,SIZE(pthread)

 !apply a special sequencing to m
 IF( PRESENT(seq_m) )THEN
  m = seq_m(m0)
 ELSE
  m = m0
 END IF

 IF( Using_AnalyticTransportTest )THEN
  CALL UPDATE_QextA(MCS_KEY_CellFunction(CellFunctionMethodA),&
   Mesh,ExtSourceCellFunction,&
   Omega=Ordinates(1:2,m),mIndices=(/m,SIZE(pThread)/))
  CALL UPDATE_TotSource_1Mom( Mesh,Ng,ExtSourceCellFunction , &
       CoeffScalarFluxM, l_ , ScalarFluxCellFunction , &
       TotSourceCellFunction ) !output
 END IF

 !proceed down each path
 path_loop: DO p = 1,SIZE(pthread(m)%path)

  !in the proper order of cells
  order_loop: DO i0 = 1,SIZE(pthread(m)%path(p)%order)

   !get the cell index
   i = pthread(m)%path(p)%order(i0)
   IF( Noisy_ )THEN
    WRITE(*,*)" m=",m
    WRITE(*,*)" Omega_x=",Directions(1,m)
    WRITE(*,*)" Omega_y=",Directions(2,m)
    WRITE(*,*)" Omega_z=",Directions(3,m)
    WRITE(*,*)" i=",i
   END IF

   !get the direction in 2D
   theta=Azimuthal_Angles(m) 
   

    !now get the slices and
    !the inside/outside edges
    !the inside and outside verts
    IF( Using_cache .AND. Cache_run )THEN
	CALL SliceCell( Mesh,i,theta,& !U,&
	    Sli,OutSide,InSide,OutVert,InVert,corig,crot)
	Nh=SIZE(InVert)
	WRITE(Cache_unit)i,m,Nh
	Nmax=MAX(Nh,Nmax)
	WRITE(Cache_unit)OutSide,InSide,Sli,InVert,OutVert
    !read from cache
    ELSE IF( Using_cache .AND. (.NOT. Cache_run) )THEN
	READ(Cache_unit)itest,mtest,Nh
	IF( itest /= i )STOP 'cache error with cell'
	IF( mtest /= m )STOP 'cache error with direction'
	READ(Cache_unit)OutSide(1:Nh-1),InSide(1:Nh-1),Sli(1:3,1:Nh),InVert(1:Nh),OutVert(1:Nh)
    ELSE
	CALL SliceCell( Mesh,i,theta,& !U,&
	    Sli,OutSide,InSide,OutVert,InVert,corig,crot)
        Nh=SIZE(InVert)
    END IF

   !determine incoming face functions as a function of t for each
   !incoming slice
   !non-piecewise
   U = xyDIRECTION_V( Directions(1:2,m) )
   CALL GetIncomingFaceFunctions( Mesh , U , &      !input
     m , i , Sli(1:3,1:Nh) , InVert(1:Nh) , InSide(1:Nh-1) , Directions , & !input
     AngularFluxV , AngularFluxF , &                !output
     PsiIn(1:3,1:Ng,1:Nh-1), fdbk )               !output

    IF( Noisy_ )THEN
        write(*,*)'i=',i,'m=',m
        write(*,*)'Sli=',Sli(1:3,1:Nh)
        write(*,*)'outvert=',OutVert(1:Nh)
        write(*,*)'invert=',InVert(1:Nh)
        write(*,*)'outside=',OutSide(1:Nh-1)
        write(*,*)'inside=',InSide(1:Nh-1)
        write(*,*)'-----'
    END IF
   !get the material index
   l = l_(i)

   !energy groups loop
   g_loop: DO g=1,SIZE(MacT,1)
    IF( Noisy_ )THEN
     WRITE(*,*)"  g=",g
    END IF

    !determine source as a function of (s,t)
    q_ = qmc_Q_from_QXY( U , TotSourceCellFunction(:,g,i) )

    !0.a. initialize face values (initialize some more than once but
    !     easier than alternatives)
    DO n=1,Nh-1
     jout = ABS( OutSide(n) )
     AngularFluxF(g,jout,m) = 0._KIND_AngularFlux
    END DO

    !0.b. initialize cell values
    AngularFluxC(g,i,m) = 0._KIND_AngularFlux

    !0.c. initialize vert values
    DO n=1,Nh
     kout = OutVert(n)
     IF( kout/=0 )THEN
      AngularFluxV(g,kout,m) = HUGE(0._KIND_AngularFlux) !due to new min condition
     END IF
    END DO

    face_wts=0.d0
    cell_wts=0.d0
    psicell_avg=0.d0
    psiface_avg=0.d0

    !1. get integrals over slices
    DO n=1,Nh-1

     id = id + 1

     !1.a. initialize solver for this slice
     !non-piecewise
     CALL qmc_INIT( id , Sli(:,n:n+1) , psiin(:,g,n) , q_  , MacT(g,l) , RecipSin(m) )

     !1.b. get outgoing vert indices
     jout  = ABS( OutSide(n) )
     kout1 = OutVert(n)
     kout2 = Outvert(n+1)
     IF( Noisy_ )THEN
      WRITE(*,*)"   subcell n=",n
      WRITE(*,*)"   jout=",jout
      WRITE(*,*)"   kout1=",kout1
      WRITE(*,*)"   kout2=",kout2
     END IF

     psi1  = qmc_PsiSB_out1(id)
     psi2  = qmc_PsiSB_out2(id)
     psiface_int  = qmc_PsiSB_intout( id,psiface_avg(n) ) !get average passed through arguments
     psicell_int  = qmc_PsiSB_int( id,psicell_avg(n) )
     face_wts(n) = qmc_LenSliOut(id)/FaceArea(Mesh,jout)
     cell_wts(n) = qmc_AreaSli(id)/CellVolume(Mesh,i)

     !balance equation check
     SBBalCheck = qmc_CalcBalance(id,relerr=relSBBalCheck)
      IF( Noisy_ )THEN
       WRITE(*,'(a,ES9.2)')"   abs. error in subcell balance=",SBBalCheck
       WRITE(*,'(a,ES9.2)')"   rel. error in subcell balance=",relSBBalCheck
      END IF
      MAX_SBBalCheck = MAX(ABS(SBBalCheck),MAX_relSBBalCheck)
      MAX_relSBBalCheck = MAX(relSBBalCheck,MAX_relSBBalCheck)

     !set vert values
     IF( kout1/=0 )THEN
      AngularFluxV(g,kout1,m) = MIN(psi1,AngularFluxV(g,kout1,m))
      IF( Noisy_ )THEN
       write(*,*)"   AngularFluxV(g,kout1,m) = ",AngularFluxV(g,kout1,m)
      END IF
     END IF

     IF( kout2/=0 )THEN
      AngularFluxV(g,kout2,m) = MIN(psi2,AngularFluxV(g,kout2,m))
      IF( Noisy_ )THEN
       write(*,*)"   AngularFluxV(g,kout2,m) = ",AngularFluxV(g,kout2,m)
      END IF
     END IF

    END DO

    CALL Normalize_Subcell_Wts(Outside(1:Nh-1),&
      face_wts(1:Nh-1),cell_wts(1:Nh-1))

    !2.b. get cell quantities
    DO n=1,Nh-1
     jout = ABS( OutSide(n) )
     AngularFluxF(g,jout ,m) = AngularFluxF(g,jout,m) + face_wts(n) * psiface_avg(n)
     AngularFluxC(g,i    ,m) = AngularFluxC(g,i   ,m) + cell_wts(n) * psicell_avg(n)
     IF( Noisy_ )THEN
      WRITE(*,*)"   jout=",jout
      WRITE(*,*)"   AngularFluxF(g,jout,m) = ",AngularFluxF(g,ABS(jout) ,m)
     END IF
    END DO
    IF( Noisy_ )THEN
     WRITE(*,*)"   i=",i
     WRITE(*,*)"   AngularFluxC(g,i,m)=",AngularFluxC(g,i,m)
    END IF

    !balance check on the cell
    AvgSource = EVAL_SourceAverage(Mesh,i,TotSourceCellFunction(:,g,i))
    BalCheck = EVAL_BalanceEquation0(Mesh,i,Directions(:,m),&
       AngularFluxC(g,i,m),AngularFluxF(g,:,m),AvgSource,MacT(g,l),&
       RecipSin(m),relerr=relBalCheck)
    IF( Noisy_ )THEN
     write(*,'(a,ES9.2)')"  abs. error in cell balance equation = ",BalCheck
     write(*,'(a,ES9.2)')"  rel. error in cell balance equation = ",relBalCheck
    END IF
    MAX_BalCheck = MAX(ABS(BalCheck),MAX_BalCheck)
    MAX_relBalCheck = MAX(relBalCheck,MAX_relBalCheck)

   END DO g_loop

   !wrapup without caching
   IF( .NOT.Using_cache .OR. (Using_cache .AND. Cache_run) )THEN
	DEALLOCATE( Sli,OutSide,InSide,OutVert,InVert )
	NULLIFY( Sli,OutSide,InSide,OutVert,InVert )
   END IF

   IF( Noisy_ )THEN
    WRITE(*,*)
    WRITE(*,*)
   END IF

  END DO order_loop

 END DO path_loop

END DO m_loop

DEALLOCATE( PsiIn )
DEALLOCATE( psiface_avg,psicell_avg,face_wts,cell_wts )


!post processing for cache
IF( Using_cache )THEN
    IF( Cache_run )THEN
	Cache_run=.FALSE.
	CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MCS]] Closing the new cache file with Nmax="//TRIM(STR(Nmax)))
    ELSE 
	DEALLOCATE( Sli,OutSide,InSide,OutVert,InVert )
	CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MCS]] Closing the old cache file")
    END IF
    CLOSE(Cache_unit)
END IF

CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MCS]] Subcell balance used thin edge conditions&
  & "//TRIM(STR(qmc_ThinEdgeCount()))//" times.")

CALL UPDATE(fdbk_comment,fdbk,s="[[MCS]] TransportSweep_MCSB_2Dgen: "//&
  " max. abs./rel. error in SUBCELL balance equation is "//&
    TRIM(STR(MAX_SBBalCheck,"(ES9.2)"))//"/"//TRIM(STR(MAX_relSBBalCheck,"(ES9.2)")))
CALL UPDATE(fdbk_comment,fdbk,s="[[MCS]] TransportSweep_MCSB_2Dgen: "//&
  " max. abs./rel. error in FULL CELL balance equation is "//&
    TRIM(STR(MAX_BalCheck,"(ES9.2)"))//"/"//TRIM(STR(MAX_relBalCheck,"(ES9.2)")))
CALL DUMP(fdbk)


!!--end--
END SUBROUTINE



!normalize weights
SUBROUTINE Normalize_Subcell_Wts(OutSide,face_wts,cell_wts)
INTEGER,INTENT(IN) :: Outside(:)
REAL(KIND_Rdp),INTENT(INOUT) :: face_wts(SIZE(OutSide))
REAL(KIND_Rdp),INTENT(INOUT) :: cell_wts(SIZE(OutSide))
REAL(KIND_Rdp) :: face_wts_accum(SIZE(OutSide))
INTEGER :: n,joutlast,njout,jout
!!--begin--

!cells are easy
cell_wts(1:SIZE(OutSide)) = cell_wts(1:SIZE(OutSide))/SUM(cell_wts(1:SIZE(Outside)))

!faces are not
!accumulation stage
joutlast=0
njout=0
DO n=1,SIZE(Outside)
 jout = ABS(outside(n))
 IF( jout/=joutlast )THEN
  njout=njout+1
  face_wts_accum(njout) = face_wts(n)
 ELSE
  face_wts_accum(njout) = face_wts_accum(njout) + face_wts(n)
 END IF
joutlast = jout
END DO

!redistribution stage
njout = 0
joutlast=0
DO n=1,SIZE(OutSide)
 jout = ABS( OutSide(n) )
 IF( joutlast/=jout )THEN
  njout = njout+1
 END IF

 face_wts(n) = face_wts(n) / face_wts_accum(njout)
 joutlast = jout
END DO

!!--end--
END SUBROUTINE



!!### SUBROUTINE <<Normalize_Subcell_Wts_Piecewise>>
SUBROUTINE Normalize_Subcell_Wts_Piecewise(OutSide,face_wts,cell_wts)

!!#### PURPOSE
!! Normalize subcell weights for a piecewise representation.

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: Outside(:)

!!#### REQUIRED INPUT/OUTPUT
REAL(KIND_Rdp),INTENT(INOUT) :: face_wts(SIZE(OutSide),2)
REAL(KIND_Rdp),INTENT(INOUT) :: cell_wts(SIZE(OutSide),2)

!!#### LOCAL VARIABLES
REAL(KIND_Rdp) :: face_wts_accum(SIZE(OutSide)),cell_wts_accum
INTEGER :: n,joutlast,njout,jout,z

!!--begin--
face_wts_accum = 0._KIND_Rdp
cell_wts_accum = 0._KIND_Rdp
DO z=1,2

 !cells are easy
 DO n=1,SIZE(outside)
  cell_wts_accum = cell_wts_accum + cell_wts(n,z)
 END DO

 !faces are not
 !accumulation stage
 joutlast=0
 njout=0
 DO n=1,SIZE(Outside)
  jout = ABS(outside(n))
  IF( jout/=joutlast )THEN
   njout=njout+1
  END IF
  face_wts_accum(njout) = face_wts_accum(njout) + face_wts(n,z)
  joutlast = jout
 END DO

END DO

DO z=1,2

 DO n=1,SIZE(OutSide)
  cell_wts(n,z) = cell_wts(n,z)/cell_wts_accum
 END DO

 !redistribution stage
 njout = 0
 joutlast=0
 DO n=1,SIZE(OutSide)
  jout = ABS( OutSide(n) )
  IF( joutlast/=jout )THEN
   njout = njout+1
  END IF

  face_wts(n,z) = face_wts(n,z) / face_wts_accum(njout)
  joutlast = jout
 END DO

END DO

!!--end--
END SUBROUTINE


    SUBROUTINE Flarg(N,InVert,OutVert,InSide,OutSide,Sli)
    INTEGER,INTENT(INOUT) :: N,InVert(N),OutVert(N),InSide(N-1),OutSide(N-1)
    INTEGER :: i,j
    REAL(KIND_Rdp),INTENT(INOUT) :: Sli(1:3,N-1)
    !REAL(KIND_Rdp_ :: SliOut(1:3,N-1)
    INTEGER :: Nout
    Nout=N
    !SliOut=Sli
    DO i=1,N
        if( InVert(i)==0 .AND. OutVert(i)==0 )then
!             write(*,*)'fatal error slicing N,Pg=',N
!             write(*,'(2a12)')'x','y'
!             DO j=1,N
!                 write(*,'(2e12.5)')Pg(1,j),Pg(2,j)
!             END DO
            !write(*,*)'U=',U
            write(*,'(2a12)')'invert','outvert'
            DO j=1,N
                write(*,'(2i12)')InVert(j),OutVert(j)
            END DO
            write(*,'(2a12)')'inside','outside'
            DO j=1,N-1
                write(*,'(2i12)')InSide(j),OutSide(j)
            END DO
            if(i>1 .AND. i<N)then
                Nout=Nout-1
                WRITE(*,*)'Nout=',Nout
                Sli(:,i:Nout)=Sli(:,i+1:Nout+1)
                OutVert(i:Nout)=OutVert(i+1:Nout+1)
                InVert(i:Nout)=InVert(i+1:Nout+1)
                OutSide(i:Nout)=OutSide(i+1:Nout)
                InSide(i:Nout)=InSide(i+1:Nout)                
                if( Outvert(i-1)==0 )then
                    !Outvert(i)=Outvert(i+1)
                    !Outvert(i+1)=0

                    !Sli(1,i)=Sli(1,i)-EPSILON(1._KIND_Rdp)
                else 
                    !overwrite i
                    !Outvert(i)=Outvert(i-1)
                    !Outvert(i-1)=0
                    !Sli(1,i)=Sli(1,i)+EPSILON(1._KIND_Rdp)
                end if
            end if
        end if
    END DO
    N=Nout
    write(*,'(2a12)')'Finvert','Foutvert'
    DO j=1,N
        write(*,'(2i12)')InVert(j),OutVert(j)
    END DO
    END SUBROUTINE

!!### SUBROUTINE <<qmc_DiscontinuousFix2>>
SUBROUTINE qmc_DiscontinuousFix2(Nh,InVert,OutVert,InSide,OutSide,Sli,tstar,PsiIn2)
INTEGER,INTENT(INOUT) :: Nh,InVert(Nh),OutVert(Nh),InSide(Nh-1),OutSide(Nh-1)
REAL(KIND_Rdp),INTENT(INOUT) :: Sli(1:3,Nh-1),tstar(1:Ng,1:Nh-1),PsiIn2(1:3,1:2,1:Ng,1:Nh-1) 
REAL(KIND_RDP) :: DT,t1,t2
INTEGER :: n
!!--begin--

    DT=Sli(1,Nh)-Sli(1,1)
    n=2
    DO WHILE (n<Nh-1)
        t1=Sli(1,n  )
        t2=Sli(1,n+1)
        !vertex ''sharing'' for thin edges
        IF( qmc_ThinEdgeCheck(DT,(/t1,t2/)) )THEN
            IF( InVert(n)==0 )THEN
                InVert(n)=InVert(n+1)
                InSide(n)=InSide(n+1)
            END IF
            InVert(n+1:Nh-1)=InVert(n+2:Nh)
            InSide(n+1:Nh-2)=InSide(n+2:Nh-1)

            IF( OutVert(n)==0 )THEN
                OutVert(n)=OutVert(n+1)
                OutSide(n)=OutSide(n+1)
            END IF
            OutVert(n+1:Nh-1)=OutVert(n+2:Nh)
            OutSide(n+1:Nh-2)=OutSide(n+2:Nh-1)

            Sli(:,n)=0.5d0*(Sli(:,n)+Sli(:,n+1))
            Sli(:,n+1:Nh-1)=Sli(:,n+2:Nh)
            !tstar(1:Ng,Nh-1)=0.5d0*(tstar(1:Ng,Nh-1)+tstar(1:Ng,Nh))
            !PsiIn2(1:3,1:2,1:Ng,Nh-1)=PsiIn2(1:3,1:2,1:Ng,Nh-1)+PsiIn2(1:3,1:2,1:Ng,Nh) 
            Nh=Nh-1
        ELSE
            n=n+1
        END IF
    END DO

!!--end--
END SUBROUTINE

!!### SUBROUTINE <<TransportSweep_MCSB_2DgenP>>
SUBROUTINE TransportSweep_MCSB_2DgenP( Mesh , Directions , &
     AngularFluxV , AngularFluxF , AngularFluxC , pThread , &
     l_ , MacT , RecipSin , &
     TotSourceCellFunction , fdbk , seq_m )

!!#### PURPOSE
!! Performs a transport sweep (TransportSweep) using a Method of
!! Characteristics Subcell balance algorithm solving for
!! Vertex, Face-Average, and Cell-average Angular Fluxes, allowing
!! for subcells to have piecewise incoming fluxes.

!!#### EXTERNAL KINDS
USE KND_DiscreteOrdinates,ONLY: KIND_DOR                    !!((02-A-KND_DiscreteOrdinates.f90))
USE KND_Mesh             ,ONLY: KIND_MSH                    !!((05-B-KND_Mesh.f90))
USE KND_MoCshort         ,ONLY: KIND_AngularFlux            !!((03-A-KND_MoCshort.f90))
USE KND_XSExpansion      ,ONLY: KIND_Mac                    !!((02-A-KND_XSExpansion.f90))

!!#### GLOBAL USER MODULES
USE USR_fdbk                                                !!((08-C-USR_fdbk.f90))
USE USR_pThread                                             !!((03-A-USR_pThread.f90))
USE USR_SBCharacteristics                                   !!((26-C-USR_SBCharacteristics.f90))
USE TBX_SBCharacteristics                                   !!((46-C-TBX_SBCharacteristics.f90))
USE PRN_Array2                                              !!((08-B-PRN_Array2.f90))
USE VAR_FindMe                                              !!((03-C-VAR_FindMe.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### REQUIRED INPUT
!! * mesh
!! * order of cells to visit
!! * mapping of cell indices i to material indices l
!! * total cross section
!! * reciprocal of the sin of the azimuthal angle
!! * flat source
!! * linear source
TYPE(TYPE_Mesh)       ,INTENT(IN) :: Mesh
REAL(KIND_AngularFlux),INTENT(IN) :: Directions(:,:)
TYPE(TYPE_PThread)    ,INTENT(IN) :: pThread(:)
INTEGER               ,INTENT(IN) :: l_(:)
REAL(KIND_Mac)        ,INTENT(IN) :: MacT(:,:)
REAL(KIND_DOR)        ,INTENT(IN) :: RecipSin(:)
REAL(KIND_AngularFlux),POINTER    :: TotSourceCellFunction(:,:,:)

!!#### REQUIRED OUTPUT
!! * angular fluxes
REAL(KIND_AngularFlux),POINTER :: AngularFluxV(:,:,:)
REAL(KIND_AngularFlux),POINTER :: AngularFluxF(:,:,:)
REAL(KIND_AngularFlux),POINTER :: AngularFluxC(:,:,:)

!!#### OPTIONAL INPUT
INTEGER,OPTIONAL,INTENT(IN) :: seq_m(:)

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object [fdbk]
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL PARAMETERS
LOGICAL,PARAMETER :: Noisy_ = .FALSE.

!!#### LOCAL VARIABLES
INTEGER                            :: m0,m,p,i0,i,g,l
INTEGER                            :: Ng
INTEGER                            :: n,Nfpc
INTEGER               ,POINTER     :: InSide(:),OutSide(:)
INTEGER               ,POINTER     :: InVert(:),OutVert(:)
REAL(KIND_AngularFlux),ALLOCATABLE :: PsiIn(:,:,:),PsiIn2(:,:,:,:),tstar(:,:)
REAL(KIND_AngularFlux),POINTER     :: Sli(:,:)
INTEGER                            :: kin1,kin2,jin
INTEGER                            :: kout1,kout2,jout
INTEGER                            :: joutlast
INTEGER                            :: id,xxx
REAL(KIND_AngularFlux)             :: q_(3),theta,U(2),psi1,psi2,SliStar(3,2),t1,t2
REAL(KIND_AngularFlux)             :: corig(3),crot(3)
LOGICAL                            :: IsPiecewise,LfSideOnly,RtSideOnly
INTEGER,SAVE,ALLOCATABLE           :: MonoTracking(:,:)
INTEGER                            :: UNITO,z,kout
REAL(KIND_AngularFlux)             :: psiface_int,psicell_int,AvgSource
REAL(KIND_AngularFlux)             :: MAX_BalCheck,MAX_SBBalCheck
REAL(KIND_AngularFlux)             :: MAX_relBalCheck,MAX_relSBBalCheck
REAL(KIND_AngularFlux)             :: BalCheck,SBBalCheck
REAL(KIND_AngularFlux)             :: relBalCheck,relSBBalCheck
REAL(KIND_AngularFlux),ALLOCATABLE :: psiface_avg(:,:),psicell_avg(:,:),face_wts(:,:),cell_wts(:,:)

INTEGER            :: Nh,itest,mtest
LOGICAL,SAVE       :: Cache_run=.true.
CHARACTER(20),SAVE :: Cache_file=''
INTEGER,SAVE       :: Cache_unit=0
INTEGER,SAVE       :: Nmax=0

!!--begin--

IF( .NOT.ALLOCATED(MonoTracking) )THEN
 ALLOCATE(MonoTracking(SIZE(AngularFluxF,2),SIZE(AngularFluxF,3)))
 MonoTracking=iter
END IF

!cache file handling (open for writing or reading)
IF( Using_cache .AND. Cache_run )THEN
    !write
    Cache_unit=NewUnit()
    CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MCS]] Opening a new cache file (.cache/slicing)")
    OPEN(FILE='.cache/slicing',UNIT=Cache_unit,ACTION='WRITE',STATUS='REPLACE',FORM='UNFORMATTED',ACCESS='SEQUENTIAL')
ELSE IF ( Using_cache .AND. (.NOT.Cache_run) )THEN
    !read
    Cache_unit=NewUnit()
    CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MCS]] Opening an old cache file (.cache/slicing) with Nmax="//TRIM(STR(Nmax)))
    ALLOCATE( OutSide(Nmax-1) , InSide(Nmax-1) , Sli(3,Nmax) , InVert(Nmax) , OutVert(Nmax) )
    OPEN(FILE='.cache/slicing',UNIT=Cache_unit,ACTION='READ',STATUS='OLD',FORM='UNFORMATTED',ACCESS='SEQUENTIAL')
END IF

IF( Noisy_ )THEN
 WRITE(*,*)"iter=",iter
END IF

xxx = qmc_ThinEdgeCount(init=.TRUE.)

id   = 0
Nfpc = MAX_FacesPerCell(Mesh)
Ng   = SIZE(AngularFluxV,1)
ALLOCATE( PsiIn(3,Ng,Nfpc) )
ALLOCATE( PsiIn2(3,2,Ng,Nfpc) )
ALLOCATE( tstar(Ng,Nfpc) )
ALLOCATE( psiface_avg(Nfpc,2),psicell_avg(Nfpc,2),cell_wts(Nfpc,2),face_wts(Nfpc,2) )

MAX_SBBalCheck = 0._KIND_AngularFlux
MAX_BalCheck = 0._KIND_AngularFlux
MAX_relSBBalCheck = 0._KIND_AngularFlux
MAX_relBalCheck = 0._KIND_AngularFlux

CALL msg_KarpovTest(fdbk)

!enter main loop over angles
m_loop: DO m0 = 1,SIZE(pthread)

 !apply a special sequencing to m
 IF( PRESENT(seq_m) )THEN
  m = seq_m(m0)
 ELSE
  m = m0
 END IF

 ![1A] change to a angular-dependent source (see [2A] elsewhere)
 IF( Using_AnalyticTransportTest )THEN
  CALL UPDATE_QextA(MCS_KEY_CellFunction(CellFunctionMethodA),&
   Mesh,ExtSourceCellFunction,&
   Omega=Ordinates(1:2,m),mIndices=(/m,SIZE(pThread)/))
  CALL UPDATE_TotSource_1Mom( Mesh,Ng,ExtSourceCellFunction , &
       CoeffScalarFluxM, l_ , ScalarFluxCellFunction , &
       TotSourceCellFunction ) !output
 END IF

 !proceed down each path
 path_loop: DO p = 1,SIZE(pthread(m)%path)

  !in the proper order of verts
  order_loop: DO i0 = 1,SIZE(pthread(m)%path(p)%order)

   !get the cell index
   i = pthread(m)%path(p)%order(i0)
   IF( Noisy_ )THEN
    WRITE(*,*)" m=",m
    WRITE(*,*)" Omega_x=",Directions(1,m)
    WRITE(*,*)" Omega_y=",Directions(2,m)
    WRITE(*,*)" Omega_z=",Directions(3,m)
    WRITE(*,*)" i=",i
   END IF

    !get the direction in 2D
    theta=Azimuthal_Angles(m)

    !the inside/outside edges
    !the inside and outside verts
    IF( Using_cache .AND. Cache_run )THEN
        CALL SliceCell( Mesh,i,theta,& !U,&
            Sli,OutSide,InSide,OutVert,InVert,corig,crot)
        Nh=SIZE(InVert)
        WRITE(Cache_unit)i,m,Nh
        Nmax=MAX(Nh,Nmax)
        WRITE(Cache_unit)OutSide,InSide,Sli,InVert,OutVert
    !read from cache
    ELSE IF( Using_cache .AND. (.NOT. Cache_run) )THEN
        READ(Cache_unit)itest,mtest,Nh
        IF( itest /= i )STOP 'cache error with cell'
        IF( mtest /= m )STOP 'cache error with direction'
        READ(Cache_unit)OutSide(1:Nh-1),InSide(1:Nh-1),Sli(1:3,1:Nh),InVert(1:Nh),OutVert(1:Nh)
    ELSE
        CALL SliceCell( Mesh,i,theta,& !U,&
            Sli,OutSide,InSide,OutVert,InVert,corig,crot)
        Nh=SIZE(InVert)
    END IF

   !determine incoming face functions as a function of t for each
   !incoming slice piecewise
   U = xyDIRECTION_V( Directions(1:2,m) )
   CALL GetIncomingFaceFunctions2( Mesh , U , &                         !input
     m , i , Sli(1:3,1:Nh) , InVert(1:Nh) , InSide(1:Nh-1) , Directions , &                     !input
     AngularFluxV , AngularFluxF , &                                    !output
     tstar(1:Ng,1:Nh-1),PsiIn2(1:3,1:2,1:Ng,1:Nh-1), fdbk )   !output
    if( Noisy_ )THEN
        write(*,*)'tstar'
        WRITE(*,'(99(1x,es24.16))')tstar(1,1:Nh-1)
        write(*,*)'PisIn2_1'
        WRITE(*,'(99(1x,es24.16))')PsiIn2(1:3,1,1,1:Nh-1)
        write(*,*)'PisIn2_2'
        WRITE(*,'(99(1x,es24.16))')PsiIn2(1:3,2,1,1:Nh-1)
        WRITE(*,*)'sli'
        WRITE(*,'(99(1x,es24.16))')sli(3,1:Nh)
        WRITE(*,'(99(1x,es24.16))')sli(2,1:Nh)
        WRITE(*,'(99(1x,es24.16))')sli(1,1:Nh)
        WRITE(*,*)'vout/vin '
        WRITE(*,'(99(1x,i24))')OutVert(1:Nh)
        WRITE(*,'(99(1x,i24))')InVert(1:Nh)
        WRITE(*,*)'eout/ein'
        WRITE(*,'(99(1x,i24))')OutSide(1:Nh-1)
        WRITE(*,'(99(1x,i24))')InSide(1:Nh-1)
    end if

   !nan check
   IF( ANY(PsiIn2(1:3,1:2,1:Ng,1:Nh-1)/=PsiIn2(1:3,1:2,1:Ng,1:Nh-1)) )THEN
    CALL UPDATE(fdbk_error,fdbk,s="[[MCS]] Error: incoming flux is NaN for &
      & direction [m="//TRIM(STR(m))//"]"//&
      " and cell [i="//TRIM(STR(i))//"]")
   END IF

   !get the material index
   l = l_(i)

   !energy groups loop
   g_loop: DO g=1,SIZE(MacT,1)
    IF( Noisy_ )THEN
     WRITE(*,*)"  g=",g
    END IF

    !determine source as a function of (s,t)
    q_ = qmc_Q_from_QXY( U , TotSourceCellFunction(1:3,g,i) , corig , crot )

    !new scaling factor of the source and incoming
    IF( Using_AF_ScaleFactor )THEN
        AF_ScaleFactor=0.d0
        DO n=1,Nh-1
            jin=ABS(InSide(n))
            AF_ScaleFactor=MAX(AF_ScaleFactor,AngularFluxF(g,jin,m))
        END DO
        AF_ScaleFactor=MAX(AF_ScaleFactor,AngularFluxC(g,i,m))
        IF( AF_ScaleFactor==0.d0 )THEN
            AF_ScaleFactor=1.d0
        END IF
        WRITE(*,*)"i=",i," m=",m," scale=",AF_ScaleFactor
        DO n=1,Nh-1
            jin=ABS(InSide(n))
            AngularFluxF(g,jin,m)=AngularFluxF(g,jin,m)/AF_ScaleFactor
        END DO
        AngularFluxC(g,i,m)=AngularFluxC(g,i,m)/AF_ScaleFactor
    END IF

    !0.a. initialize face values (initialize some more than once but
    !     easier than alternatives)
    DO n=1,Nh-1
     jout = ABS( OutSide(n) )
     AngularFluxF(g,jout,m) = 0._KIND_AngularFlux
    END DO

    DO n=1,Nh
     kout = OutVert(n)
     IF( kout/=0 )THEN
      AngularFluxV(g,kout,m) = HUGE(0._KIND_AngularFlux) !due to new min condition
     END IF
    END DO

    !0.b. initialize cell values
    AngularFluxC(g,i,m) = 0._KIND_AngularFlux

    face_wts=0.d0
    cell_wts=0.d0
    psicell_avg=0.d0
    psiface_avg=0.d0
    !1. get integrals over slices
    DO n=1,Nh-1

     IsPiecewise = .NOT.IsError(tstar(g,n))

     !also may not have true piecewise if slice is only on one side of function
     IF( IsPiecewise )THEN
      t1 = Sli(1,n)
      t2 = Sli(1,n+1)
      LfSideOnly = t1<=tstar(g,n) .AND. t2<=tstar(g,n)
      RtSideOnly = t1>=tstar(g,n) .AND. t2>=tstar(g,n)
      IF( LfSideOnly .OR. RtSideOnly )THEN
       IsPiecewise = .FALSE.
      END IF
     ELSE
      LfSideOnly = .TRUE.
      RtSideOnly = .FALSE.
     END IF

     !check on nonlinear instability
     IF( IsPiecewise )THEN
      IF( iter>2 )THEN
       IF( MonoTracking(ABS(Inside(n)),m)>iter-1 )THEN
        CALL UPDATE(fdbk_comment,fdbk,s="[[MCS]] Possible Nonlinear instability for face [j="//&
          TRIM(STR(ABS(Inside(n))))//"] direction [m="//TRIM(STR(m))//&
          " iteration [iter="//TRIM(STR(iter))//"].")
       END IF
       MonoTracking(ABS(Inside(n)),m) = iter
      END IF
     END IF

     IF( IsPiecewise )THEN

      id = id + 1

      !1.a.0. initialize solver for this slice
      !dummy call to get the qmc_smin and qmc_smax functions initialized
      CALL qmc_INIT( id , Sli(1:3,n:n+1) , psiin(:,g,n) , q_  , MacT(g,l) , RecipSin(m) )
      SliStar(1:3,1) = Sli(1:3,n)
      SliStar(1,2) = tstar(g,n)
      SliStar(2,2) = qmc_smin(id,tstar(g,n))
      SliStar(3,2) = qmc_smax(id,tstar(g,n))

      !1.a. form and initialize for sub slice A
      id = id + 1
      CALL qmc_INIT( id , Slistar , psiin2(:,1,g,n) , q_  , MacT(g,l) , RecipSin(m) )

      !1.b. get outgoing vert indices (only need vert 1 for subslice A)
      jout  = ABS( OutSide(n) )
      kout1 = OutVert(n)

      IF( Noisy_ )THEN
       WRITE(*,*)"   subcell A"
       WRITE(*,*)"   subcell n=",n
       WRITE(*,*)"   jout=",jout
       WRITE(*,*)"   kout1=",kout1
      END IF

      psi1  = qmc_PsiSB_out1(id)
      psiface_int  = qmc_PsiSB_intout( id,psiface_avg(n,1) ) !get average passed through arguments
      psicell_int  = qmc_PsiSB_int( id,psicell_avg(n,1) )
      face_wts(n,1) = qmc_LenSliOut(id)/FaceArea(Mesh,jout)
      cell_wts(n,1) = qmc_AreaSli(id)/CellVolume(Mesh,i)

      !balance equation check
      SBBalCheck = qmc_CalcBalance(id,relerr=relSBBalCheck)
      IF( Noisy_ )THEN
       WRITE(*,'(a,ES9.2)')"   abs. error in subcell balance=",SBBalCheck
       WRITE(*,'(a,ES9.2)')"   rel. error in subcell balance=",relSBBalCheck
      END IF
      MAX_SBBalCheck = MAX(ABS(SBBalCheck),MAX_SBBalCheck)
      MAX_relSBBalCheck = MAX(relSBBalCheck,MAX_relSBBalCheck)

      IF( kout1/=0 )THEN
       !take minimum value across discontinuities and only get positive value
       AngularFluxV(g,kout1,m) = MAX(MIN(psi1,AngularFluxV(g,kout1,m)),0.d0)
       IF( Using_AF_ScaleFactor )THEN
            AngularFluxV(g,kout1,m) = AF_ScaleFactor*AngularFluxV(g,kout1,m)
       END IF
       IF( Noisy_ )THEN
        write(*,*)"   AngularFluxV(g,kout1,m) = ",AngularFluxV(g,kout1,m)
       END IF
      END IF

      !1.f. proceed to subslice B
      id = id + 1

      !1.g. initialize solver for this slice (subslice B)
      SliStar(:,1) = SliStar(:,2)
      SliStar(:,2) = Sli(:,n+1)
      CALL qmc_INIT( id , Slistar , psiin2(:,2,g,n) , q_  , MacT(g,l) , RecipSin(m) )

      !1.h. get outgoing vert indices (only need vert 2 for subslice B)
      jout  = ABS( OutSide(n) )
      kout2 = Outvert(n+1)
      IF( Noisy_ )THEN
       WRITE(*,*)'   subslice B'
       WRITE(*,*)"   subcell n=",n
       WRITE(*,*)"   jout=",jout
       WRITE(*,*)"   kout2=",kout2
      END IF

      psi2  = qmc_PsiSB_out2(id)
      psiface_int  = qmc_PsiSB_intout( id,psiface_avg(n,2) ) !get average passed through arguments
      psicell_int  = qmc_PsiSB_int( id,psicell_avg(n,2) )
      face_wts(n,2) = qmc_LenSliOut(id)/FaceArea(Mesh,jout)
      cell_wts(n,2) = qmc_AreaSli(id)/CellVolume(Mesh,i)

      !balance equation check
      SBBalCheck = qmc_CalcBalance(id,relerr=relSBBalCheck)
      IF( Noisy_ )THEN
       WRITE(*,'(a,ES9.2)')"   abs. error in subcell balance=",SBBalCheck
       WRITE(*,'(a,ES9.2)')"   rel. error in subcell balance=",relSBBalCheck
      END IF
      MAX_SBBalCheck = MAX(ABS(SBBalCheck),MAX_SBBalCheck)
      MAX_relSBBalCheck = MAX(relSBBalCheck,MAX_relSBBalCheck)

      IF( kout2/=0 )THEN
       !take minimum value across discontinuities
       AngularFluxV(g,kout2,m) = MAX(MIN(psi2,AngularFluxV(g,kout2,m)),0.d0)
       IF( Using_AF_ScaleFactor )THEN
            AngularFluxV(g,kout2,m) = AF_ScaleFactor*AngularFluxV(g,kout2,m)
       END IF
       IF( Noisy_ )THEN
        write(*,*)"   AngularFluxV(g,kout2,m) = ",AngularFluxV(g,kout2,m)
       END IF
      END IF

     !!--begin--nonpiecewise-case
     ELSE

      !get the normal (non-piecewise) function
      IF( LfSideOnly )THEN
       PsiIn(:,g,n) = PsiIn2(:,1,g,n)
      ELSE IF( RtSideOnly )THEN
       PsiIn(:,g,n) = PsiIn2(:,2,g,n)
      ELSE
       WRITE(*,*)"fatal side choosing error!"
       STOP
      END IF

      id = id + 1
      !1.a. initialize solver for this slice
      !non-piecewise
      CALL qmc_INIT( id , Sli(:,n:n+1) , psiin(:,g,n) , q_  , MacT(g,l) , RecipSin(m) )

      !1.b. get outgoing vert indices
      jout  = ABS( OutSide(n) )
      kout1 = OutVert(n)
      kout2 = Outvert(n+1)
      IF( Noisy_ )THEN
       WRITE(*,*)"   subcell n=",n
       WRITE(*,*)"   jout=",jout
       WRITE(*,*)"   kout1=",kout1
       WRITE(*,*)"   kout2=",kout2
      END IF

      psi1  = qmc_PsiSB_out1(id)
      psi2  = qmc_PsiSB_out2(id)
      psiface_int  = qmc_PsiSB_intout( id,psiface_avg(n,1) ) !get average passed through arguments
      psicell_int  = qmc_PsiSB_int( id,psicell_avg(n,1) )
      face_wts(n,1) = qmc_LenSliOut(id)/FaceArea(Mesh,jout)
      cell_wts(n,1) = qmc_AreaSli(id)/CellVolume(Mesh,i)
      psiface_avg(n,2)=ERROR(1._KIND_AngularFlux)
      psicell_avg(n,2)=ERROR(1._KIND_AngularFlux)
      face_wts(n,2) = 0
      cell_wts(n,2) = 0
      !balance equation check
      SBBalCheck = qmc_CalcBalance(id,relerr=relSBBalCheck)
      IF( Noisy_ )THEN
       WRITE(*,'(a,ES9.2)')"   abs. error in subcell balance=",SBBalCheck
       WRITE(*,'(a,ES9.2)')"   rel. error in subcell balance=",relSBBalCheck
      END IF
      MAX_SBBalCheck = MAX(ABS(SBBalCheck),MAX_SBBalCheck)
      MAX_relSBBalCheck = MAX(relSBBalCheck,MAX_relSBBalCheck)

      IF( kout1/=0 )THEN
       !take minimum value across discontinuities
       AngularFluxV(g,kout1,m) = MAX(MIN(psi1,AngularFluxV(g,kout1,m)),0.d0)
       IF( Using_AF_ScaleFactor )THEN
            AngularFluxV(g,kout1,m) = AF_ScaleFactor*AngularFluxV(g,kout1,m)
       END IF
       IF( Noisy_ )THEN
        write(*,*)"   AngularFluxV(g,kout1,m) = ",AngularFluxV(g,kout1,m)
       END IF
      END IF

      IF( kout2/=0 )THEN
       !take minimum value across discontinuities
       AngularFluxV(g,kout2,m) = MAX(MIN(psi2,AngularFluxV(g,kout2,m)),0.d0)
       IF( Using_AF_ScaleFactor )THEN
            AngularFluxV(g,kout2,m) = AF_ScaleFactor*AngularFluxV(g,kout2,m)
       END IF
       IF( Noisy_ )THEN
        write(*,*)"   AngularFluxV(g,kout2,m) = ",AngularFluxV(g,kout2,m)
       END IF
      END IF

     END IF

     !!--end--nonpiecewise-case
    END DO

    CALL Normalize_Subcell_Wts_Piecewise(Outside(1:Nh-1),&
      face_wts(1:Nh-1,1:2),cell_wts(1:Nh-1,1:2))

    !!----------AVERAGING PART---------------------------

    !2.b. get cell quantities
    DO n=1,Nh-1
     jout = ABS( OutSide(n) )
     DO z=1,2
      AngularFluxF(g,jout ,m) = AngularFluxF(g,jout,m) + face_wts(n,z) * psiface_avg(n,z)
      AngularFluxC(g,i    ,m) = AngularFluxC(g,i   ,m) + cell_wts(n,z) * psicell_avg(n,z)
     END DO
    END DO

    !apply new scaling factor
    IF( Using_AF_ScaleFactor )THEN
        DO n=1,Nh-1
            jout = ABS(Outside(n))
            AngularFluxF(g,jout,m) = AF_ScaleFactor*AngularFluxF(g,jout,m)
            IF( AngularFluxF(g,jout,m)/=AngularFluxF(g,jout,m))THEN
                WRITE(*,*)'problem for jout=',jout,' m=',m
            END IF
        END DO
        AngularFluxC(g,i,m) = AF_ScaleFactor*AngularFluxC(g,i,m)
        IF( AngularFluxC(g,i,m)/=AngularFluxC(g,i,m))THEN
            WRITE(*,*)'problem for i=',i,' m=',m
            STOP
        END IF
    END IF

    DO n=1,Nh-1
     jout = ABS(Outside(n))
     AngularFluxF(g,jout,m) = MAX(0.d0,AngularFluxF(g,jout,m))
     IF( Noisy_ )THEN
      WRITE(*,*)"  jout=",jout
      WRITE(*,*)"  AngularFluxF(g,jout,m) = ",AngularFluxF(g,ABS(jout) ,m)
     END IF
    END DO
    AngularFluxC(g,i,m) = MAX(0.d0,AngularFluxC(g,i,m))
    IF( Noisy_ )THEN
     WRITE(*,*)"  i=",i
     WRITE(*,*)"  AngularFluxC(g,i,m)=",AngularFluxC(g,i,m)
    END IF


    !balance check on the cell
    AvgSource = EVAL_SourceAverage(Mesh,i,TotSourceCellFunction(:,g,i))
    BalCheck = EVAL_BalanceEquation0(Mesh,i,Directions(:,m),&
       AngularFluxC(g,i,m),AngularFluxF(g,:,m),AvgSource,MacT(g,l),&
       RecipSin(m),relerr=relBalCheck)
    IF( Noisy_ )THEN
     write(*,'(a,ES9.2)')"  abs. error in cell balance equation = ",BalCheck
     write(*,'(a,ES9.2)')"  rel. error in cell balance equation = ",relBalCheck
    END IF
    MAX_BalCheck = MAX(ABS(BalCheck),MAX_BalCheck)
    MAX_relBalCheck = MAX(relBalCheck,MAX_relBalCheck)

   END DO g_loop

   !wrapup without caching
   IF( .NOT.Using_cache .OR. (Using_cache .AND. Cache_run) )THEN
        DEALLOCATE( Sli,OutSide,InSide,OutVert,InVert )
        NULLIFY( Sli,OutSide,InSide,OutVert,InVert )
   END IF

   IF( Noisy_ )THEN
    WRITE(*,*)
    WRITE(*,*)
   END IF

  END DO order_loop

 END DO path_loop

END DO m_loop

DEALLOCATE( PsiIn,PsiIn2,tstar)
DEALLOCATE( psiface_avg,psicell_avg,face_wts,cell_wts )

!post processing for cache
IF( Using_cache )THEN
    IF( Cache_run )THEN
        Cache_run=.FALSE.
        CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MCS]] Closing the new cache file with Nmax="//TRIM(STR(Nmax)))
    ELSE 
        DEALLOCATE( Sli,OutSide,InSide,OutVert,InVert )
        CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MCS]] Closing the old cache file")
    END IF
    CLOSE(Cache_unit)
END IF

CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MCS]] Subcell balance used thin edge conditions&
  & "//TRIM(STR(qmc_ThinEdgeCount()))//" times.")

CALL UpdateAndDUmp(fdbk_comment,fdbk,s="[[MCS]] TransportSweep_MCSB_2DgenP: "//&
  " max. abs./rel. error in SUBCELL balance equation is "//&
    TRIM(STR(MAX_SBBalCheck,"(ES9.2)"))//"/"//TRIM(STR(MAX_relSBBalCheck,"(ES9.2)")))
CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MCS]] TransportSweep_MCSB_2DgenP: "//&
  " max. abs./rel. error in FULL CELL balance equation is "//&
    TRIM(STR(MAX_BalCheck,"(ES9.2)"))//"/"//TRIM(STR(MAX_relBalCheck,"(ES9.2)")))

CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MCS]] Exiting TransportSweep_MCSB_2DgenP with allocations:")
CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MCS]]         AngualarFluxV:"//TRIM(STR(ASSOCIATED(AngularFluxV))))
CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MCS]]         AngualarFluxF:"//TRIM(STR(ASSOCIATED(AngularFluxF))))
CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MCS]]         AngualarFluxC:"//TRIM(STR(ASSOCIATED(AngularFluxC))))
CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MCS]] TotSourceCellFunction:"//TRIM(STR(ASSOCIATED(TotSourceCellFunction))))

!!--end--
END SUBROUTINE




! !!### SUBROUTINE <<TransportSweep_MCSB_2DgenP>>
! SUBROUTINE TransportSweep_MCSB_2DgenP_A( Mesh , Directions , &
!      AngularFluxV , AngularFluxF , AngularFluxC , pThread , &
!      l_ , MacT , RecipSin , &
!      TotSourceCellFunction , fdbk , seq_m )
! 
! !!#### PURPOSE
! !! Performs a transport sweep (TransportSweep) using a Method of
! !! Characteristics Subcell balance algorithm solving for
! !! Vertex, Face-Average, and Cell-average Angular Fluxes, allowing
! !! for subcells to have piecewise incoming fluxes.
! 
! !!#### EXTERNAL KINDS
! USE KND_DiscreteOrdinates,ONLY: KIND_DOR                    !!((02-A-KND_DiscreteOrdinates.f90))
! USE KND_Mesh             ,ONLY: KIND_MSH                    !!((05-B-KND_Mesh.f90))
! USE KND_MoCshort         ,ONLY: KIND_AngularFlux            !!((03-A-KND_MoCshort.f90))
! USE KND_XSExpansion      ,ONLY: KIND_Mac                    !!((02-A-KND_XSExpansion.f90))
! 
! !!#### GLOBAL USER MODULES
! USE USR_fdbk                                                !!((08-C-USR_fdbk.f90))
! USE USR_pThread                                             !!((03-A-USR_pThread.f90))
! USE USR_SBCharacteristics                                   !!((26-C-USR_SBCharacteristics.f90))
! USE TBX_SBCharacteristics                                   !!((46-C-TBX_SBCharacteristics.f90))
! USE PRN_Array2                                              !!((08-B-PRN_Array2.f90))
! USE VAR_FindMe                                              !!((03-C-VAR_FindMe.f90))
! 
! !!#### DEFAULT IMPLICIT
! IMPLICIT NONE
! 
! !!#### REQUIRED INPUT
! !! * mesh
! !! * order of cells to visit
! !! * mapping of cell indices i to material indices l
! !! * total cross section
! !! * reciprocal of the sin of the azimuthal angle
! !! * flat source
! !! * linear source
! TYPE(TYPE_Mesh)       ,INTENT(IN) :: Mesh
! REAL(KIND_AngularFlux),INTENT(IN) :: Directions(:,:)
! TYPE(TYPE_PThread)    ,INTENT(IN) :: pThread(:)
! INTEGER               ,INTENT(IN) :: l_(:)
! REAL(KIND_Mac)        ,INTENT(IN) :: MacT(:,:)
! REAL(KIND_DOR)        ,INTENT(IN) :: RecipSin(:)
! REAL(KIND_AngularFlux),POINTER    :: TotSourceCellFunction(:,:,:)
! 
! !!#### REQUIRED OUTPUT
! !! * angular fluxes
! REAL(KIND_AngularFlux),POINTER :: AngularFluxV(:,:,:)
! REAL(KIND_AngularFlux),POINTER :: AngularFluxF(:,:,:)
! REAL(KIND_AngularFlux),POINTER :: AngularFluxC(:,:,:)
! 
! !!#### OPTIONAL INPUT
! INTEGER,OPTIONAL,INTENT(IN) :: seq_m(:)
! 
! !!#### OPTIONAL INPUT/OUTPUT
! !! * feedback object [fdbk]
! TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk
! 
! !!#### LOCAL PARAMETERS
! LOGICAL,PARAMETER :: Noisy_ = .FALSE.
! 
! !!#### LOCAL VARIABLES
! INTEGER                            :: m0,m,p,i0,i,g,l
! INTEGER                            :: Ng
! INTEGER                            :: n,Nfpc
! INTEGER               ,POINTER     :: InSide(:),OutSide(:)
! INTEGER               ,POINTER     :: InVert(:),OutVert(:)
! REAL(KIND_AngularFlux),ALLOCATABLE :: PsiIn(:,:,:),PsiIn2(:,:,:,:),tstar(:,:)
! REAL(KIND_AngularFlux),POINTER     :: Sli(:,:)
! INTEGER                            :: kin1,kin2,jin
! INTEGER                            :: kout1,kout2,jout
! INTEGER                            :: joutlast
! INTEGER                            :: id,xxx
! REAL(KIND_AngularFlux)             :: q_(3),theta,psi1,psi2,SliStar(3,2),t1,t2
! REAL(KIND_AngularFlux)             :: corig(3),crot(3)
! LOGICAL                            :: IsPiecewise,LfSideOnly,RtSideOnly
! INTEGER,SAVE,ALLOCATABLE           :: MonoTracking(:,:)
! INTEGER                            :: UNITO,z,kout
! REAL(KIND_AngularFlux)             :: psiface_int,psicell_int,AvgSource
! REAL(KIND_AngularFlux)             :: MAX_BalCheck,MAX_SBBalCheck
! REAL(KIND_AngularFlux)             :: MAX_relBalCheck,MAX_relSBBalCheck
! REAL(KIND_AngularFlux)             :: BalCheck,SBBalCheck
! REAL(KIND_AngularFlux)             :: relBalCheck,relSBBalCheck
! REAL(KIND_AngularFlux),ALLOCATABLE :: psiface_avg(:,:),psicell_avg(:,:),face_wts(:,:),cell_wts(:,:)
! !!--begin--
! 
! IF( .NOT.ALLOCATED(MonoTracking) )THEN
!  ALLOCATE(MonoTracking(SIZE(AngularFluxF,2),SIZE(AngularFluxF,3)))
!  MonoTracking=iter
! END IF
! 
! IF( Noisy_ )THEN
!  WRITE(*,*)"iter=",iter
! END IF
! 
! xxx = qmc_ThinEdgeCount(init=.TRUE.)
! 
! id   = 0
! Nfpc = MAX_FacesPerCell(Mesh)
! Ng   = SIZE(AngularFluxV,1)
! ALLOCATE( PsiIn(3,Ng,Nfpc) )
! ALLOCATE( PsiIn2(3,2,Ng,Nfpc) )
! ALLOCATE( tstar(Ng,Nfpc) )
! ALLOCATE( psiface_avg(Nfpc,2),psicell_avg(Nfpc,2),cell_wts(Nfpc,2),face_wts(Nfpc,2) )
! 
! MAX_SBBalCheck = 0._KIND_AngularFlux
! MAX_BalCheck = 0._KIND_AngularFlux
! MAX_relSBBalCheck = 0._KIND_AngularFlux
! MAX_relBalCheck = 0._KIND_AngularFlux
! 
! CALL msg_KarpovTest(fdbk)
! 
! !enter main loop over angles
! m_loop: DO m0 = 1,SIZE(pthread)
! 
!  !apply a special sequencing to m
!  IF( PRESENT(seq_m) )THEN
!   m = seq_m(m0)
!  ELSE
!   m = m0
!  END IF
! 
!  ![1A] change to a angular-dependent source (see [2A] elsewhere)
!  IF( Using_AnalyticTransportTest )THEN
!   CALL UPDATE_QextA(MCS_KEY_CellFunction(CellFunctionMethodA),&
!    Mesh,ExtSourceCellFunction,&
!    Omega=Ordinates(1:2,m),mIndices=(/m,SIZE(pThread)/))
!   CALL UPDATE_TotSource_1Mom( Mesh,Ng,ExtSourceCellFunction , &
!        CoeffScalarFluxM, l_ , ScalarFluxCellFunction , &
!        TotSourceCellFunction ) !output
!  END IF
! 
!  !proceed down each path
!  path_loop: DO p = 1,SIZE(pthread(m)%path)
! 
!   !in the proper order of verts
!   order_loop: DO i0 = 1,SIZE(pthread(m)%path(p)%order)
! 
!    !get the cell index
!    i = pthread(m)%path(p)%order(i0)
!    IF( Noisy_ )THEN
!     WRITE(*,*)" m=",m
!     WRITE(*,*)" Omega_x=",Directions(1,m)
!     WRITE(*,*)" Omega_y=",Directions(2,m)
!     WRITE(*,*)" Omega_z=",Directions(3,m)
!     WRITE(*,*)" i=",i
!    END IF
! 
!    !get the direction in 2D
!    theta=Azimuthal_Angles(m)
!    !U = xyDIRECTION_V( Directions(1:2,m) )
! 
!    !now get the slices and the inside/outside edges, the inside and outside verts
!    CALL SliceCell( Mesh,i,theta,& !U,&
!      Sli,OutSide,InSide,OutVert,InVert,corig,crot)
! 
!    !determine incoming face functions as a function of t for each
!    !incoming slice piecewise
!    CALL GetIncomingFaceFunctions2( Mesh , U , &                         !input
!      m , i , Sli , InVert , InSide , Directions , &                     !input
!      AngularFluxV , AngularFluxF , &                                    !output
!      tstar(1:Ng,1:SIZE(InSide)),PsiIn2(1:3,1:2,1:Ng,1:SIZE(InSide)) )   !output
!    !nan check
!    IF( ANY(PsiIn2(1:3,1:2,1:Ng,1:SIZE(InSide))/=PsiIn2(1:3,1:2,1:Ng,1:SIZE(InSide))) )THEN
!     CALL UPDATE(fdbk_error,fdbk,s="[[MCS]] Error: incoming flux is NaN for &
!       & direction [m="//TRIM(STR(m))//"]"//&
!       " and cell [i="//TRIM(STR(i))//"]")
!    END IF
! 
!    !discontinuous fix because for very simple problems there will be no symmetry
!    !because of the possibility that the Karpov parabolic is discontinous on two
!    !sides of an incoming vert which, if there is an outgoing vertex that is
!    !practically directly across from the vert, there is only a 50% chance we will
!    !get the right OutVert indices to correctly handle the discontinuity
!    CALL qmc_DiscontinuousFix( Sli , OutVert )
! 
!    !get the material index
!    l = l_(i)
! 
!    !energy groups loop
!    g_loop: DO g=1,SIZE(MacT,1)
!     IF( Noisy_ )THEN
!      WRITE(*,*)"  g=",g
!     END IF
! 
!     !modify source so that we deal with absorption along characteristic
!     !*********************
!     !*********************
!     !*********************
!     TotSourceCellFunction(1,g,i) = TotSourceCellFunction(1,g,i) &
!      - MacS(g,l)*AngularFluxC(g,i,m)
!     !*********************
!     !*********************
!     !*********************
! 
!     !determine source as a function of (s,t)
!     q_ = qmc_Q_from_QXY( U , TotSourceCellFunction(1:3,g,i) , corig , crot )
! 
!     !0.a. initialize face values (initialize some more than once but
!     !     easier than alternatives)
!     DO n=1,SIZE(OutSide)
!      jout = ABS( OutSide(n) )
!      AngularFluxF(g,jout,m) = 0._KIND_AngularFlux
!     END DO
! 
!     DO n=1,SIZE(OutVert)
!      kout = OutVert(n)
!      IF( kout/=0 )THEN
!       AngularFluxV(g,kout,m) = HUGE(0._KIND_AngularFlux) !due to new min condition
!      END IF
!     END DO
! 
!     !0.b. initialize cell values
!     AngularFluxC(g,i,m) = 0._KIND_AngularFlux
! 
!     face_wts=0.d0
!     cell_wts=0.d0
!     psicell_avg=0.d0
!     psiface_avg=0.d0
!     !1. get integrals over slices
!     DO n=1,SIZE(OutSide)
! 
!      IsPiecewise = .NOT.IsError(tstar(g,n))
! 
!      !also may not have true piecewise if slice is only on one side of function
!      IF( IsPiecewise )THEN
!       t1 = Sli(1,n)
!       t2 = Sli(1,n+1)
!       LfSideOnly = t1<=tstar(g,n) .AND. t2<=tstar(g,n)
!       RtSideOnly = t1>=tstar(g,n) .AND. t2>=tstar(g,n)
!       IF( LfSideOnly .OR. RtSideOnly )THEN
!        IsPiecewise = .FALSE.
!       END IF
!      ELSE
!       LfSideOnly = .TRUE.
!       RtSideOnly = .FALSE.
!      END IF
! 
!      !check on nonlinear instability
!      IF( IsPiecewise )THEN
!       IF( iter>2 )THEN
!        IF( MonoTracking(ABS(Inside(n)),m)>iter-1 )THEN
!         CALL UPDATE(fdbk_comment,fdbk,s="[[MCS]] Possible Nonlinear instability for face [j="//&
!           TRIM(STR(ABS(Inside(n))))//"] direction [m="//TRIM(STR(m))//&
!           " iteration [iter="//TRIM(STR(iter))//"].")
!        END IF
!        MonoTracking(ABS(Inside(n)),m) = iter
!       END IF
!      END IF
! 
!      IF( IsPiecewise )THEN
! 
!       id = id + 1
! 
!       !1.a.0. initialize solver for this slice
!       !dummy call to get the qmc_smin and qmc_smax functions initialized
!       !*********************
!       !*********************
!       !*********************
!       CALL qmc_INIT( id , Sli(:,n:n+1) , psiin(:,g,n) , q_  , MacT(g,l)-MacS(g,l) , RecipSin(m) )
!       SliStar(:,1) = Sli(:,n)
!       SliStar(1,2) = tstar(g,n)
!       SliStar(2,2) = qmc_smin(id,tstar(g,n))
!       SliStar(3,2) = qmc_smax(id,tstar(g,n))
! 
!       !1.a. form and initialize for sub slice A
!       id = id + 1
!       !*********************
!       !*********************
!       !*********************
!       CALL qmc_INIT( id , Slistar , psiin2(:,1,g,n) , q_  , MacT(g,l)-MacS(g,l) , RecipSin(m) )
! 
!       !1.b. get outgoing vert indices (only need vert 1 for subslice A)
!       jout  = ABS( OutSide(n) )
!       kout1 = OutVert(n)
! 
!       IF( Noisy_ )THEN
!        WRITE(*,*)"   subcell n=",n
!        WRITE(*,*)"   jout=",jout
!        WRITE(*,*)"   kout1=",kout1
!       END IF
! 
!       psi1  = qmc_PsiSB_out1(id)
!       psiface_int  = qmc_PsiSB_intout( id,psiface_avg(n,1) ) !get average passed through arguments
!       psicell_int  = qmc_PsiSB_int( id,psicell_avg(n,1) )
!       face_wts(n,1) = qmc_LenSliOut(id)/FaceArea(Mesh,jout)
!       cell_wts(n,1) = qmc_AreaSli(id)/CellVolume(Mesh,i)
! 
!       !balance equation check
!       SBBalCheck = qmc_CalcBalance(id,relerr=relSBBalCheck)
!       IF( Noisy_ )THEN
!        WRITE(*,*)"   abs. error in subcell balance=",SBBalCheck
!        WRITE(*,*)"   rel. error in subcell balance=",relSBBalCheck
!       END IF
!       MAX_SBBalCheck = MAX(ABS(SBBalCheck),MAX_SBBalCheck)
!       MAX_relSBBalCheck = MAX(relSBBalCheck,MAX_relSBBalCheck)
! 
!       IF( kout1/=0 )THEN
!        !take minimum value across discontinuities
!        AngularFluxV(g,kout1,m) = MIN(psi1,AngularFluxV(g,kout1,m))
!        IF( Noisy_ )THEN
!         write(*,*)"   AngularFluxV(g,kout1,m) = ",AngularFluxV(g,kout1,m)
!        END IF
!       END IF
! 
!       !1.f. proceed to subslice B
!       id = id + 1
! 
!       !1.g. initialize solver for this slice (subslice B)
!       SliStar(:,1) = SliStar(:,2)
!       SliStar(:,2) = Sli(:,n+1)
!       CALL qmc_INIT( id , Slistar , psiin2(:,2,g,n) , q_  , MacT(g,l)-MacS(g,l) , RecipSin(m) )
! 
!       !1.h. get outgoing vert indices (only need vert 2 for subslice A)
!       jout  = ABS( OutSide(n) )
!       kout2 = Outvert(n+1)
!       IF( Noisy_ )THEN
!        WRITE(*,*)"   subcell n=",n
!        WRITE(*,*)"   jout=",jout
!        WRITE(*,*)"   kout2=",kout2
!       END IF
! 
!       psi2  = qmc_PsiSB_out2(id)
!       psiface_int  = qmc_PsiSB_intout( id,psiface_avg(n,2) ) !get average passed through arguments
!       psicell_int  = qmc_PsiSB_int( id,psicell_avg(n,2) )
!       face_wts(n,2) = qmc_LenSliOut(id)/FaceArea(Mesh,jout)
!       cell_wts(n,2) = qmc_AreaSli(id)/CellVolume(Mesh,i)
! 
!       !balance equation check
!       SBBalCheck = qmc_CalcBalance(id,relerr=relSBBalCheck)
!       IF( Noisy_ )THEN
!        WRITE(*,*)"   abs. error in subcell balance=",SBBalCheck
!        WRITE(*,*)"   rel. error in subcell balance=",relSBBalCheck
!       END IF
!       MAX_SBBalCheck = MAX(ABS(SBBalCheck),MAX_SBBalCheck)
!       MAX_relSBBalCheck = MAX(relSBBalCheck,MAX_relSBBalCheck)
! 
!       IF( kout2/=0 )THEN
!        !take minimum value across discontinuities
!        AngularFluxV(g,kout2,m) = MIN(psi2,AngularFluxV(g,kout2,m))
!        IF( Noisy_ )THEN
!         write(*,*)"   AngularFluxV(g,kout2,m) = ",AngularFluxV(g,kout2,m)
!        END IF
!       END IF
! 
!      !!--begin--nonpiecewise-case
!      ELSE
! 
!       !get the normal (non-piecewise) function
!       IF( LfSideOnly )THEN
!        PsiIn(:,g,n) = PsiIn2(:,1,g,n)
!       ELSE IF( RtSideOnly )THEN
!        PsiIn(:,g,n) = PsiIn2(:,2,g,n)
!       ELSE
!        WRITE(*,*)"fatal side choosing error!"
!        STOP
!       END IF
! 
!       id = id + 1
!       !1.a. initialize solver for this slice
!       !non-piecewise
!       CALL qmc_INIT( id , Sli(:,n:n+1) , psiin(:,g,n) , q_  , MacT(g,l)-MacS(g,l) , RecipSin(m) )
! 
!       !1.b. get outgoing vert indices
!       jout  = ABS( OutSide(n) )
!       kout1 = OutVert(n)
!       kout2 = Outvert(n+1)
!       IF( Noisy_ )THEN
!        WRITE(*,*)"   subcell n=",n
!        WRITE(*,*)"   jout=",jout
!        WRITE(*,*)"   kout1=",kout1
!        WRITE(*,*)"   kout2=",kout2
!       END IF
! 
!       psi1  = qmc_PsiSB_out1(id)
!       psi2  = qmc_PsiSB_out2(id)
!       psiface_int  = qmc_PsiSB_intout( id,psiface_avg(n,1) ) !get average passed through arguments
!       psicell_int  = qmc_PsiSB_int( id,psicell_avg(n,1) )
!       face_wts(n,1) = qmc_LenSliOut(id)/FaceArea(Mesh,jout)
!       cell_wts(n,1) = qmc_AreaSli(id)/CellVolume(Mesh,i)
!       psiface_avg(n,2)=ERROR(1._KIND_AngularFlux)
!       psicell_avg(n,2)=ERROR(1._KIND_AngularFlux)
!       face_wts(n,2) = 0
!       cell_wts(n,2) = 0
!       !balance equation check
!       SBBalCheck = qmc_CalcBalance(id,relerr=relSBBalCheck)
!       IF( Noisy_ )THEN
!        WRITE(*,*)"   abs. error in subcell balance=",SBBalCheck
!        WRITE(*,*)"   rel. error in subcell balance=",relSBBalCheck
!       END IF
!       MAX_SBBalCheck = MAX(ABS(SBBalCheck),MAX_SBBalCheck)
!       MAX_relSBBalCheck = MAX(relSBBalCheck,MAX_relSBBalCheck)
! 
!       IF( kout1/=0 )THEN
!        !take minimum value across discontinuities
!        AngularFluxV(g,kout1,m) = MIN(psi1,AngularFluxV(g,kout1,m))
!        IF( Noisy_ )THEN
!         write(*,*)"   AngularFluxV(g,kout1,m) = ",AngularFluxV(g,kout1,m)
!        END IF
!       END IF
! 
!       IF( kout2/=0 )THEN
!        !take minimum value across discontinuities
!        AngularFluxV(g,kout2,m) = MIN(psi2,AngularFluxV(g,kout2,m))
!        IF( Noisy_ )THEN
!         write(*,*)"   AngularFluxV(g,kout2,m) = ",AngularFluxV(g,kout2,m)
!        END IF
!       END IF
! 
!      END IF
! 
!      !!--end--nonpiecewise-case
!     END DO
! 
!     CALL Normalize_Subcell_Wts_Piecewise(Outside,&
!       face_wts(1:SIZE(Outside),1:2),cell_wts(1:SIZE(Outside),1:2))
! 
!     !!----------AVERAGING PART---------------------------
! 
!     !2.b. get cell quantities
!     DO n=1,SIZE(OutSide)
!      jout = ABS( OutSide(n) )
!      DO z=1,2
!       AngularFluxF(g,jout ,m) = AngularFluxF(g,jout,m) + face_wts(n,z) * psiface_avg(n,z)
!       AngularFluxC(g,i    ,m) = AngularFluxC(g,i   ,m) + cell_wts(n,z) * psicell_avg(n,z)
!      END DO
!      IF( Noisy_ )THEN
!       WRITE(*,*)"  jout=",jout
!       WRITE(*,*)"  AngularFluxF(g,jout,m) = ",AngularFluxF(g,ABS(jout) ,m)
!      END IF
!     END DO
!     IF( Noisy_ )THEN
!      WRITE(*,*)"  i=",i
!      WRITE(*,*)"  AngularFluxC(g,i,m)=",AngularFluxC(g,i,m)
!     END IF
! 
!     !balance check on the cell
!     AvgSource = EVAL_SourceAverage(Mesh,i,TotSourceCellFunction(:,g,i))
!     BalCheck = EVAL_BalanceEquation0(Mesh,i,Directions(:,m),&
!        AngularFluxC(g,i,m),AngularFluxF(g,:,m),AvgSource,MacT(g,l)-MacS(g,l),&
!        RecipSin(m),relerr=relBalCheck)
!     IF( Noisy_ )THEN
!      write(*,*)"  abs. error in cell balance equation = ",BalCheck
!      write(*,*)"  rel. error in cell balance equation = ",relBalCheck
!     END IF
!     MAX_BalCheck = MAX(ABS(BalCheck),MAX_BalCheck)
!     MAX_relBalCheck = MAX(relBalCheck,MAX_relBalCheck)
! 
!    END DO g_loop
! 
!    !wrapup
!    DEALLOCATE( Sli,OutSide,InSide,OutVert,InVert )
!    IF( Noisy_ )THEN
!     WRITE(*,*)
!     WRITE(*,*)
!    END IF
! 
!   END DO order_loop
! 
!  END DO path_loop
! 
! END DO m_loop
! 
! DEALLOCATE( PsiIn,PsiIn2,tstar)
! DEALLOCATE( psiface_avg,psicell_avg,face_wts,cell_wts )
! 
! CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MCS]] Subcell balance used thin edge conditions&
!   & "//TRIM(STR(qmc_ThinEdgeCount()))//" times.")
! 
! CALL UpdateAndDUmp(fdbk_comment,fdbk,s="[[MCS]] TransportSweep_MCSB_2DgenP: "//&
!   " max. abs./rel. error in SUBCELL balance equation is "//&
!     TRIM(STR(MAX_SBBalCheck,"(ES9.2)"))//"/"//TRIM(STR(MAX_relSBBalCheck,"(ES9.2)")))
! CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MCS]] TransportSweep_MCSB_2DgenP: "//&
!   " max. abs./rel. error in FULL CELL balance equation is "//&
!     TRIM(STR(MAX_BalCheck,"(ES9.2)"))//"/"//TRIM(STR(MAX_relBalCheck,"(ES9.2)")))
! 
! 
! !!--end--
! END SUBROUTINE



!!### SUBROUTINE <<GetIncomingFaceFunctions>>
SUBROUTINE GetIncomingFaceFunctions(Mesh,U,&
  m,i,Sli,InVert,InSide,Ordinates,AngularFluxV,AngularFluxF, &
  PsiIn,fdbk)

!!#### PURPOSE
!! For direction <m> and cell <i>, return the functional expansion of the
!! angular flux on the incoming faces.


!!#### MODULES
USE VAR_Units                                               !!((03-A-VAR_Units.f90))
USE USR_SBCharacteristics                                   !!((26-C-USR_SBCharacteristics.f90))
!USE TBX_SBCharacteristics                                                              !!((27-C-TBX_SBCharacteristics.f90))
USE USR_Characteristics,ONLY: GNUPLOT_f1                    !!((78-C-USR_Characteristics.f90))
USE FUN_ptr_Sequence                                        !!((04-A-FUN_ptr_Sequence.f90))

!!#### DETAILS
!! The incoming face function is parabolic and has a total dimensionality,
!! <PsiIn(1:3,1:Ng,1:Njin)> for the number of energy groups <Ng> and
!! incoming faces <Njin>.


!!#### REQUIRED INPUT
!! * mesh <Mesh>
!! * direction of travel projected to xy-plane
!! * current direction index <m> and cell index <i>
!! * incoming faces <InSide>
!! * energy dep. vertex angular fluxes <AngularFluxV>
!! * energy dep. face angular fluxes <AngularFluxF>
TYPE(TYPE_Mesh)       ,INTENT(IN) :: Mesh
REAL(KIND_AngularFlux),INTENT(IN) :: U(2)
INTEGER               ,INTENT(IN) :: m,i
REAL(KIND_AngularFlux),INTENT(IN) :: Sli(:,:)
INTEGER               ,INTENT(IN) :: InVert(:),InSide(:)
REAL(KIND_DOR)        ,INTENT(IN) :: Ordinates(:,:)

!!#### REQUIRED INPUT/OUTPUT
REAL(KIND_AngularFlux),INTENT(INOUT) :: AngularFluxV(:,:,:)
REAL(KIND_AngularFlux),INTENT(INOUT) :: AngularFluxF(:,:,:)

!!#### REQUIRED OUTPUT
!! * incoming angular flux functions <PsiIn>
REAL(KIND_AngularFlux),INTENT(OUT) :: PsiIn(3,&
                                            SIZE(AngularFluxV,1),&
                                            SIZE(InSide,1))
!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### LOCAL VARIABLES
INTEGER :: n
INTEGER :: jin,kin1,kin2
INTEGER :: jin0
INTEGER :: jout,kout1,kout2
INTEGER :: jd,g
REAL(KIND_AngularFlux) :: Area
REAL(KIND_AngularFlux) :: psi1  (SIZE(AngularFluxV,1))
REAL(KIND_AngularFlux) :: psi2  (SIZE(AngularFluxV,1))
REAL(KIND_AngularFlux) :: psiavg(SIZE(AngularFluxF,1))
LOGICAL                :: HasExtrema
REAL(KIND_AngularFlux) :: Extrema,Ut(2),V1(2),V2(2),Tl(2)
REAL(KIND_AngularFlux) :: PsiInTest(3)
INTEGER                :: OutUnit,xxx
REAL(KIND_qmc),POINTER :: xvals(:)

!!--begin--

OutUnit = DEFAULT_OUTPUT_UNIT

PsiIn = 0._KIND_AngularFlux

DO n=1,SIZE(InSide)

 jin  = ABS( InSide(n) )
 kin1 =  LeftVert( InVert , n   )
 kin2 = RightVert( InVert , n+1 )

 !get area
 Area = FaceArea(Mesh,jin)

 !get boundary conditions
 IF( IsBoundaryFace(Mesh,jin) )THEN

  jd        = DomainFace(Mesh,jin)
  jin0      = Inside(n)  !need it with the negative sign

  IF( .NOT.IsReflective(BC(jd)) )THEN
   !use special vertex boundary condition lookups to handle discontinuous corners
   AngularFluxV(:,kin1,m) = EVAL_VertFixedBC(BC , FixedAngularFlux,FunctionAngularFlux,&
        Mesh,kin1,Ordinates,m,PolSin,Allow_Discontinuous_Corners,fdbk)
   AngularFluxV(:,kin2,m) = EVAL_VertFixedBC(BC , FixedAngularFlux,FunctionAngularFlux,&
        Mesh,kin2,Ordinates,m,PolSin,Allow_Discontinuous_Corners,fdbk)
   AngularFluxF(:,jin ,m) = EVAL_fixedBC( BC , FixedAngularFlux,FunctionAngularFlux , fdbk, jd , Mesh , &
     m=m , j=jin0, Ordinates=Ordinates)
  END IF

 END IF

 !get set local data
 psi1(:)   = AngularFluxV(:,kin1,m)
 psi2(:)   = AngularFluxV(:,kin2,m)
 psiavg(:) = AngularFluxF(:,jin ,m)


 !first in local <l> coordinates
 DO g=1,Ng
  PsiIn(:,g,n) = qmc_PsiEdge_from_2Pt1Avg( &
     psiavg(g)  , &
     Area       , &
     psi1(g)    , &
     psi2(g)    , HasExtrema , Extrema )

  !WRITE(OutUnit,"(a,i)")"Function al^2 + bl + c generated for edge ",n
  !WRITE(OutUnit,"(a,i)")"                             energy group ",g
  !WRITE(OutUnit,"(a,e)")" Length = ",Area
  !WRITE(OutUnit,"(a,e)")" a = ",PsiIn(3,g,n)
  !WRITE(OutUnit,"(a,e)")" b = ",PsiIn(2,g,n)
  !WRITE(OutUnit,"(a,e)")" c = ",PsiIn(1,g,n)
  !xxx = 100000*m + 1000*i + 10*ABS(jin) + n
  !xvals => ptr_Sequence(0._KIND_qmc,Area/10._KIND_qmc,11)
  !CALL GNUPLOT_f1( "PsiInL"//TRIM(STR(xxx)) , PsiIn_Function(xvals) , xvals )
  !CALL GNUPLOT_f1( "PsiL1"//TRIM(STR(xxx)) , Psi1_Function(xvals) , xvals )
  !CALL GNUPLOT_f1( "PsiL2"//TRIM(STR(xxx)) , Psi2_Function(xvals) , xvals )
  !CALL GNUPLOT_f1( "PsiLA"//TRIM(STR(xxx)) , PsiA_Function(xvals) , xvals )
  !DEALLOCATE( xvals )



!  IF( HasExtrema )THEN
!   WRITE(OutUnit,"(a,e)")" Extrema located at l=",Extrema
!   count_extrema = count_extrema + 1
!  ELSE
!   WRITE(OutUnit,"(a  )")" No Extrema "
!  END IF


  Tl = DeltaPerp_Mesh(Mesh,kin1,kin2,i,U)

  !now in <t> coordinates
  PsiIn(:,g,n) = qmc_PsiInOut_from_PsiEdge( PsiIn(:,g,n) , Area , Tl , &
    PsiAvg(g) )

  !now in <t> coordinates
  !PsiInTest(1:3) = qmc_PsiInOut_from_2Pt1Avg( PsiAvg(g) , Area , Tl , &
  !  Psi1(g),Psi2(g) )

  !WRITE(11,"(5e23.16)")Tl(1),Tl(2),PsiInTest(3),PsiInTest(2),PsiInTest(1)
  !WRITE(12,"(5e23.16)")Tl(1),Tl(2),PsiIn(3,g,n),PsiIn(2,g,n),PsiIn(1,g,n)

  !WRITE(OutUnit,"(a,i)")"Function at^2 + bt + c converted for edge ",n
  !WRITE(OutUnit,"(a,i)")"                             energy group ",g
  !WRITE(OutUnit,"(a,e)")" Length = ",Area
  !WRITE(OutUnit,"(a,e)")" a = ",PsiIn(3,g,n)
  !WRITE(OutUnit,"(a,e)")" b = ",PsiIn(2,g,n)
  !WRITE(OutUnit,"(a,e)")" c = ",PsiIn(1,g,n)

 END DO


END DO

!!--end--
!CONTAINS

!
!ELEMENTAL FUNCTION PsiIn_function(l) RESULT(eval)
!REAL(KIND_qmc),INTENT(IN) :: l
!REAL(KIND_qmc) :: eval
!eval = PsiIn(3,g,n)*l**2 + PsiIn(2,g,n)*l + PsiIn(1,g,n)
!END FUNCTION
!
!ELEMENTAL FUNCTION Psi1_function(l) RESULT(eval)
!REAL(KIND_qmc),INTENT(IN) :: l
!REAL(KIND_qmc) :: eval
!eval = psi1(g)
!END FUNCTION
!
!ELEMENTAL FUNCTION Psi2_function(l) RESULT(eval)
!REAL(KIND_qmc),INTENT(IN) :: l
!REAL(KIND_qmc) :: eval
!eval = psi2(g)
!END FUNCTION
!
!ELEMENTAL FUNCTION PsiA_function(l) RESULT(eval)
!REAL(KIND_qmc),INTENT(IN) :: l
!REAL(KIND_qmc) :: eval
!eval = psiavg(g)
!END FUNCTION
!
!
!
END SUBROUTINE





!!### SUBROUTINE <<GetIncomingFaceFunctions2>>
SUBROUTINE GetIncomingFaceFunctions2(Mesh,U,&
  m,i,Sli,InVert,InSide,Ordinates,AngularFluxV,AngularFluxF, &
  tstar,PsiIn2,fdbk)

!!#### PURPOSE
!! For direction <m> and cell <i>, return the functional expansion of the
!! angular flux on the incoming faces, with a 2 part piecewise parabolic.


!!#### MODULES
USE VAR_Units                                               !!((03-A-VAR_Units.f90))
USE USR_SBCharacteristics                                   !!((26-C-USR_SBCharacteristics.f90))
!USE TBX_SBCharacteristics                                                              !!((27-C-TBX_SBCharacteristics.f90))
USE USR_Characteristics,ONLY: GNUPLOT_f1                    !!((78-C-USR_Characteristics.f90))
USE FUN_ptr_Sequence                                        !!((04-A-FUN_ptr_Sequence.f90))
USE FUN_IsError                                             !!((05-A-FUN_IsError.f90))
USE SUB_Swap

!!#### DETAILS
!! The incoming face function is parabolic and has a total dimensionality,
!! <PsiIn2(1:3,1:2,1:Ng,1:Njin)> for the number of energy groups <Ng> and
!! incoming faces <Njin>.


!!#### REQUIRED INPUT
!! * mesh <Mesh>
!! * direction of travel projected to xy-plane
!! * current direction index <m> and cell index <i>
!! * incoming faces <InSide>
!! * energy dep. vertex angular fluxes <AngularFluxV>
!! * energy dep. face angular fluxes <AngularFluxF>
TYPE(TYPE_Mesh)       ,INTENT(IN) :: Mesh
REAL(KIND_AngularFlux),INTENT(IN) :: U(2)
INTEGER               ,INTENT(IN) :: m,i
REAL(KIND_AngularFlux),INTENT(IN) :: Sli(:,:)
INTEGER               ,INTENT(IN) :: InVert(:),InSide(:)
REAL(KIND_DOR)        ,INTENT(IN) :: Ordinates(:,:)

!!#### REQUIRED INPUT/OUTPUT
REAL(KIND_AngularFlux),INTENT(INOUT) :: AngularFluxV(:,:,:)
REAL(KIND_AngularFlux),INTENT(INOUT) :: AngularFluxF(:,:,:)

!!#### REQUIRED OUTPUT
!! * incoming angular flux functions <PsiIn2>
!! * $\ell$ value at which we use parabolic 2 instead of 1
REAL(KIND_AngularFlux),INTENT(OUT) :: tstar(SIZE(AngularFluxV,1),&
                                            SIZE(InSide,1))
REAL(KIND_AngularFlux),INTENT(OUT) :: PsiIn2(3,2,&
                                            SIZE(AngularFluxV,1),&
                                            SIZE(InSide,1))
!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### LOCAL VARIABLES
INTEGER :: n,mr,jd1,jd2,jb1,jb2,m2,k_,k
INTEGER :: jin,kin1,kin2
INTEGER :: jin0
INTEGER :: jout,kout1,kout2
INTEGER :: jd,g
REAL(KIND_AngularFlux) :: Area
REAL(KIND_AngularFlux) :: psi1  (SIZE(AngularFluxV,1))
REAL(KIND_AngularFlux) :: psi2  (SIZE(AngularFluxV,1))
REAL(KIND_AngularFlux) :: psiavg(SIZE(AngularFluxF,1))
LOGICAL                :: HasExtrema
REAL(KIND_AngularFlux) :: Extrema,Ut(2),V1(2),V2(2),Tl(2)
REAL(KIND_AngularFlux) :: PsiInTest(3)
INTEGER                :: OutUnit,xxx,pp
REAL(KIND_qmc),POINTER :: xvals(:)
REAL(KIND_qmc) :: tstar_dummy_array(1),outward_normal1(2),outward_normal2(2)

!!--begin--

OutUnit = DEFAULT_OUTPUT_UNIT

PsiIn2 = 0._KIND_AngularFlux

DO n=1,SIZE(InSide)

    jin  = ABS( InSide(n) )
    kin1 =  LeftVert( InVert , n   )
    kin2 = RightVert( InVert , n+1 )

    !get area
    Area = FaceArea(Mesh,jin)

    !get boundary conditions
    IF( IsBoundaryFace(Mesh,jin) )THEN

        jd        = DomainFace(Mesh,jin)
        jin0      = Inside(n)  !need it with the optional negative sign as found in the actual case

        IF( .NOT.IsReflective(BC(jd)) )THEN
            !use special vertex boundary condition lookups to handle discontinuous corners
            AngularFluxV(:,kin1,m) = EVAL_VertFixedBC(BC , FixedAngularFlux,FunctionAngularFlux,&
                    Mesh,kin1,Ordinates,m,PolSin,Allow_Discontinuous_Corners,fdbk)
            AngularFluxV(:,kin2,m) = EVAL_VertFixedBC(BC , FixedAngularFlux,FunctionAngularFlux,&
                    Mesh,kin2,Ordinates,m,PolSin,Allow_Discontinuous_Corners,fdbk)
            AngularFluxF(:,jin ,m) = EVAL_fixedBC( BC , FixedAngularFlux,FunctionAngularFlux , fdbk, jd , Mesh , &
                m=m , j=jin0 , Ordinates=Ordinates )
        ELSE
            outward_normal1 = DomainFaceNormal(Mesh,jd)
            mr = EVAL_ReflectiveBC_POINT( m , Ordinates , outward_normal1 ) !note negative
            !WRITE(*,*)'face mr=',mr,' m=',m
            AngularFluxF(:,jin,m) = AngularFluxF(:,jin,mr)
            !for the two verts
            DO k_=1,2
                IF( k_==1 )THEN
                    k=kin1
                ELSE
                    k=kin2
                END IF
                CALL BoundaryFacesFromVert(Mesh,k,jb1,jb2,jd1,jd2)
                !fix up for hanging verts
                IF( jd2==0 )THEN
                jd2 = jd1
                jb2 = jb1
                END IF

                !make sure jd1 is jd (swap if not)
                IF( jd1/=jd )THEN
                    CALL Swap(jd1,jd2)
                    CALL Swap(jb1,jb2)
                END IF

                !corner verts
                IF( jd1/=jd2 .AND. IsReflective(BC(jd1)) .AND. IsReflective(BC(jd2)) )THEN

                    !WRITE(*,*)'jd1=',jd1,' jd2=',jd2
                    !v2.23 correction: assuming bottom left corner, need reflections to quad II and quad IV
                    mr = EVAL_ReflectiveBC_POINT( m , Ordinates , outward_normal1 )
                    !WRITE(*,*)'set1 m=',m,' mr=',mr
                    AngularFluxV(:,k,m) = AngularFluxV(:,k,mr)

                    !double reflection
                    outward_normal2 = DomainFaceNormal(Mesh,jd2)
                    m2 = EVAL_ReflectiveBC_POINT( m , Ordinates , outward_normal1 , outward_normal2 )
                    AngularFluxV(:,k,m2) = AngularFluxV(:,k,m)
                    !WRITE(*,*)'set2: m=',m,' m2=',m2

                !non-corner verts
                ELSE

                    mr = EVAL_ReflectiveBC_POINT( m , Ordinates , outward_normal1 )
                    AngularFluxV(:,k,m) = AngularFluxV(:,k,mr)

                END IF

            END DO

        END IF

    END IF

    !get set local data
    psi1(:)   = AngularFluxV(:,kin1,m)
    psi2(:)   = AngularFluxV(:,kin2,m)
    psiavg(:) = AngularFluxF(:,jin ,m)

    !  WRITE(*,*)'kin1=',kin1,' kin2=',kin2,' jin=',jin
    !  WRITE(*,*)"AngularFluxV(:,kin1,m)=",AngularFluxV(:,kin1,m)
    !  WRITE(*,*)"AngularFluxV(:,kin2,m)=",AngularFluxV(:,kin2,m)
    !  WRITE(*,*)"AngularFluxV(:,jin,m)=",AngularFluxF(:,jin,m)
    ! 

    !first in local <l> coordinates
    DO g=1,Ng
        PsiIn2(:,:,g,n) = qmc_PsiEdge2_from_2Pt1Avg( &
            psiavg(g)  , &
            Area       , &
            psi1(g)    , &
            psi2(g)    , &
            tstar(g,n) , HasExtrema , Extrema )
        Tl = DeltaPerp_Mesh(Mesh,kin1,kin2,i,U)

        !now in <t> coordinates

        !no piecewise
        IF( IsError(tstar(g,n)) )THEN

            !check for error in an entry (it means the other piecewise part has the function)
            IF( IsError(PsiIn2(1,1,g,n)) )THEN
                PsiIn2(:,2,g,n) = qmc_PsiInOut_from_PsiEdge( PsiIn2(:,2,g,n) , Area , Tl , &
                PsiAvg(g) )
            ELSE
                PsiIn2(:,1,g,n) = qmc_PsiInOut_from_PsiEdge( PsiIn2(:,1,g,n) , Area , Tl , &
                PsiAvg(g) )
            END IF

        !piecewise
        ELSE
            PsiIn2(:,1,g,n) = qmc_PsiInOut_from_PsiEdge( PsiIn2(:,1,g,n) , Area , Tl , &
                PsiAvg(g) )

            tstar_dummy_array(1) = tstar(g,n)
            PsiIn2(:,2,g,n) = qmc_PsiInOut_from_PsiEdge( PsiIn2(:,2,g,n) , Area , Tl , &
                PsiAvg(g) , tstar_dummy_array )
            tstar(g,n) = tstar_dummy_array(1) !because function calls for a list of values
        END IF
        !CALL PRINT_PPara(tstar(g,n),PsiIn2(:,:,g,n))
    END DO

END DO

!!--end--
END SUBROUTINE


ELEMENTAL FUNCTION IsInfinity(x) RESULT(Is)
REAL(KIND_Rdp),INTENT(IN) :: x
LOGICAL :: Is
!!--begin--
Is=( x>HUGE(1._KIND_Rdp) .OR. x<-HUGE(1._KIND_Rdp) )
!!--end--
END FUNCTION


FUNCTION LeftVert(VertSeq,n)
INTEGER,INTENT(IN) :: VertSeq(:),n
INTEGER :: LeftVert
INTEGER :: o
LeftVert = 0
DO o=n,1,-1
 LeftVert = VertSeq(o)
 IF( LeftVert/=0 )EXIT
END DO
END FUNCTION

FUNCTION RightVert(VertSeq,n)
INTEGER,INTENT(IN) :: VertSeq(:),n
INTEGER :: RightVert
INTEGER :: o
RightVert = 0
DO o=n,SIZE(VertSeq),+1
 RightVert = VertSeq(o)
 IF( RightVert/=0 )EXIT
END DO
END FUNCTION

FUNCTION DeltaPerp_Mesh(Mesh,k1,k2,i,U) RESULT(Tl)
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER        ,INTENT(IN) :: k1,k2,i
REAL(KIND_MSH) ,INTENT(IN) :: U(2)
REAL(KIND_MSH) :: Tl(2)
!!
REAL(KIND_AngularFlux) :: Ut(2),V1(2),V2(2)

!!--begin--
IF( k1==0 .OR. k2==0 )THEN
 CALL Pause(s="Fatal error in DeltaPerp_Mesh")
END IF

!clockwise rotation of <s> direction
Ut = (/U(2),-U(1)/)

!get local coordinates
V1 = Vert(Mesh,k1) - CellCentroid(Mesh,i)
V2 = Vert(Mesh,k2) - CellCentroid(Mesh,i)

!get <t> coordinates of this edges endpoints
Tl(1) = DOT_PRODUCT( V1 , Ut )
Tl(2) = DOT_PRODUCT( V2 , Ut )

!!--end--
END FUNCTION







FUNCTION INDEX_Characteristic(k,m) RESULT(c)
!return the 1D index of a characteristic based on vert k
!and direction m
INTEGER :: c,k,m
!!--begin--
c = k + (m-1)*SIZE(AngularFluxV,2)
!!--end--
END FUNCTION


FUNCTION INDEX_Characteristic2(k,m) RESULT(c)
!return the 1D index of a characteristic based independent
!of vertex k and m (just in the order they come)
INTEGER :: c,k,m
INTEGER,SAVE :: csave=0
!!--begin--
csave=csave+1
c = csave
!!--end--
END FUNCTION


!!### FUNCTION <<NUM_Characteristics>>
FUNCTION NUM_Characteristics(AngularFluxV) RESULT(Nc)

!!#### PURPOSE
!! Return the total number of characteristics.

!!#### REQUIRED INPUT
REAL(KIND_AngularFlux),INTENT(IN) :: AngularFluxV(:,:,:)

!!#### LOCAL VARIABLES
INTEGER :: Nc

!!--begin--
Nc = SIZE(AngularFluxV,3)*SIZE(AngularFluxV,2)
!!--end--
END FUNCTION



!!### SUBROUTINE <<TransportSweep_MCL_OnFly>>
SUBROUTINE TransportSweep_MCL_OnFly( AngularFluxV , &
  Directions , Mesh , pThread , WithinCell , fdbk , tol )

!!#### PURPOSE
!! Performs a long characteristics transport sweep on the
!! fly.

!!#### EXTERNAL KINDS
USE KND_IntrinsicTypes                                      !!((01-A-KND_IntrinsicTypes.f90))
USE KND_DiscreteOrdinates,ONLY: KIND_DOR                    !!((02-A-KND_DiscreteOrdinates.f90))
USE KND_Mesh             ,ONLY: KIND_MSH                    !!((05-B-KND_Mesh.f90))
USE KND_MoCshort         ,ONLY: KIND_AngularFlux            !!((03-A-KND_MoCshort.f90))

!!#### GLOBAL USER MODULES
USE USR_Mesh                                                !!((14-B-USR_Mesh.f90))
USE USR_pThread                                             !!((03-A-USR_pThread.f90))
USE USR_fdbk                                                !!((08-C-USR_fdbk.f90))
USE USR_Characteristics                                     !!((78-C-USR_Characteristics.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### REQUIRED INPUT
!! * directions
!! * mesh
!! * path threads
REAL(KIND_DOR)    ,POINTER       :: Directions(:,:)
TYPE(TYPE_Mesh)   ,INTENT(INOUT) :: Mesh
TYPE(TYPE_pThread),INTENT(IN)    :: pThread(:)
INTEGER           ,INTENT(IN)    :: WithinCell(:,:)

!!#### REQUIRED OUTPUT
!! * angular flux
REAL(KIND_AngularFlux),POINTER :: AngularFluxV(:,:,:)

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
!! * tolerance for long characteristics solver <tol>
TYPE(TYPE_fdbk)       ,OPTIONAL,INTENT(INOUT) :: fdbk
REAL(KIND_AngularFlux),OPTIONAL,INTENT(IN)    :: tol

!!#### LOCAL VARIABLES
INTEGER                                        :: g,m,p,k__,k,i,m_refl
REAL(KIND_MSH)                                 :: s01
REAL(KIND_MSH),DIMENSION(NUM_Dimensions_Mesh(Mesh)) :: r0,r1
REAL(KIND_qmc),DIMENSION(3)                    :: Omega
REAL(KIND_AngularFlux)                         :: psi0,psi1
INTEGER                                        :: Ng,Nd,Ns_
INTEGER        ,POINTER                        :: iA(:)
REAL(KIND_MSH) ,POINTER                        :: sA(:)
REAL(KIND_AngularFlux)                         :: tol_,DEFAULT_tol

!!#### LOCAL PARAMETERS
LOGICAL,PARAMETER :: UpdateReflOnFly = .TRUE.


!!--begin--

!setup variables
CALL CLEARn( iA )
CALL CLEARn( sA )

!get default tolerance
DEFAULT_tol = 10._KIND_AngularFlux*EPSILON(1._KIND_AngularFlux)

!get tolerance
tol_ = DEFAULT( DEFAULT_tol , tol )

!get numbers of stuff
Ng = NUM_EnergyGroups( AngularFluxV )
Nd = NUM_Dimensions_Mesh(Mesh)

!initialize <qmc>
CALL qmc_init( Nd )

!set options for <qmc>
CALL qmc_SetOptions( tol=tol_ , NSamples=10 )


!energy loop
ener_loop: DO g   = 1,Ng

 !enter main loop over angles
 dire_loop: DO m   = 1,SIZE(pthread)

  !proceed down each path
  path_loop: DO p   = 1,SIZE(pthread(m)%path)

   !in the proper order of verts
   vert_loop: DO k__ = 1,SIZE(pthread(m)%path(p)%order)

    !get the vert index
    k = pthread(m)%path(p)%order(k__)
    !WRITE(*,*)"k=",k
    !WRITE(*,*)"m=",m

    !check to see if we need to just use a boundary value
    i = WithinCell(k,m)

    !a) boundary vert with fixed value
    IF( i==0 )THEN

         CYCLE

    !b) reflective boundary
    ELSE IF( i<0 )THEN

     !update reflections on the fly
     IF( UpdateReflOnFly )THEN

      !for each azimuthal index determine the reflection
      m_refl = ABS(i)
      AngularFluxV( : , k , m ) = AngularFluxV( : , k , m_refl )

     END IF

     CYCLE

    END IF


    !get the vertex
    r1 = Vert(Mesh,k)
    !WRITE(*,*)"r1=",r1

    !get the direction
    Omega = Ordinates(:,m)
    !WRITE(*,*)"Omega=",Omega

    !calculate distance to boundary and boundary psi
    CALL GET_LongBC(Mesh,BC,FixedAngularFlux,FunctionAngularFlux,Ordinates,fdbk,m,&
      r1,s01,r0,psi0)
    !WRITE(*,*)"s01=",s01
    !WRITE(*,*)"r0=",r0
    !WRITE(*,*)"pis0=",psi0

    !relocate the current position
    CALL qmc_relocate( r0 , Omega , s01 , psi0 )

    !traverse the mesh from the current position to the vert <r1>
    CALL EVAL_celltraversal( Mesh , r0 , Omega , s01 , &
           Ns_ , iA=iA , sA=sA )
    !WRITE(*,*)"iA=",iA(1:Ns)
    !WRITE(*,*)"sA=",sA(1:Ns)

    !determine the source and cache it
    CALL qmc_Discover_Q(sA=sA(1:Ns_))

    !determine the total cross section and cache it
    CALL qmc_Discover_sigma(sA=sA(1:Ns_))

    !integrate using the cached source and cross section
    !WRITE(*,"(a)")"fwdpsi(k="//TRIM(STR(k))//",m="//TRIM(STR(m))//")="
    psi1 = qmc_fwdpsi_linsrc()

    !WRITE(*,"(5x,e18.9)")psi1
    AngularFluxV( g , k , m ) = psi1

   END DO vert_loop

  END DO path_loop

 END DO dire_loop

END DO ener_loop


!kill and release memory
CALL qmc_kill()


!wrapup variables
CALL CLEARn( iA )
CALL CLEARn( sA )

!!--end--
END SUBROUTINE




!!### SUBROUTINE <<TransportSweep_MCL_PointList>>
SUBROUTINE TransportSweep_MCL_PointList( Unit , PointList , &
  Directions , Mesh , fdbk , tol )

!!#### PURPOSE
!! Performs a long characteristics transport sweep on the
!! fly, just on a list of points and outputs them.

!!#### WARNING
!! Does not account for reflective boundaries!

!!#### EXTERNAL KINDS
USE KND_IntrinsicTypes                                      !!((01-A-KND_IntrinsicTypes.f90))
USE KND_DiscreteOrdinates,ONLY: KIND_DOR                    !!((02-A-KND_DiscreteOrdinates.f90))
USE KND_Mesh             ,ONLY: KIND_MSH                    !!((05-B-KND_Mesh.f90))
USE KND_MoCshort         ,ONLY: KIND_AngularFlux            !!((03-A-KND_MoCshort.f90))

!!#### GLOBAL USER MODULES
USE USR_Mesh                                                !!((14-B-USR_Mesh.f90))
USE USR_pThread                                             !!((03-A-USR_pThread.f90))
USE USR_fdbk                                                !!((08-C-USR_fdbk.f90))
USE USR_Characteristics                                     !!((78-C-USR_Characteristics.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### REQUIRED INPUT
!! * list of points
!! * directions
!! * mesh
REAL(KIND_Rdp) ,POINTER       :: PointList(:,:)
REAL(KIND_DOR) ,POINTER       :: Directions(:,:)
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED OUTPUT
!! * output goes directly to <Unit>
INTEGER,INTENT(IN) :: Unit


!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
!! * tolerance for long characteristics solver <tol>
TYPE(TYPE_fdbk)       ,OPTIONAL,INTENT(INOUT) :: fdbk
REAL(KIND_AngularFlux),OPTIONAL,INTENT(IN)    :: tol

!!#### LOCAL VARIABLES
INTEGER                                        :: g,m,z
REAL(KIND_MSH)                                 :: s01
REAL(KIND_MSH),DIMENSION(NUM_Dimensions_Mesh(Mesh)) :: r0,r1
REAL(KIND_qmc),DIMENSION(3)                    :: Omega
REAL(KIND_AngularFlux)                         :: psi0,psi1
INTEGER                                        :: Ng,Nd,Ns_
INTEGER        ,POINTER                        :: iA(:)
REAL(KIND_MSH) ,POINTER                        :: sA(:)
REAL(KIND_AngularFlux)                         :: tol_,DEFAULT_tol

!!--begin--

!setup variables
CALL CLEARn( iA )
CALL CLEARn( sA )

!get default tolerance
DEFAULT_tol = 10._KIND_AngularFlux*EPSILON(1._KIND_AngularFlux)

!get tolerance
tol_ = DEFAULT( DEFAULT_tol , tol )

!get numbers of stuff
Ng = NUM_EnergyGroups( AngularFluxV )
Nd = NUM_Dimensions_Mesh(Mesh)

!initialize <qmc>
CALL qmc_init( Nd )

!set options for <qmc>
CALL qmc_SetOptions( tol=tol_ , NSamples=10 )


!energy loop
ener_loop: DO g   = 1,Ng

 !enter main loop over angles
 dire_loop: DO m   = 1,SIZE(Directions,2)

  !in the proper order of verts
  points_loop: DO z = 1,SIZE(PointList,2)

    !get the vertex
    r1 = PointList(:,z)
    !WRITE(*,*)"r1=",r1

    !get the direction
    Omega = Ordinates(:,m)
    !WRITE(*,*)"Omega=",Omega

    !calculate distance to boundary and boundary psi
    CALL GET_LongBC(Mesh,BC,FixedAngularFlux,FunctionAngularFlux,&
      Ordinates,fdbk,m,r1,s01,r0,psi0)
    !WRITE(*,*)"s01=",s01
    !WRITE(*,*)"r0=",r0
    !WRITE(*,*)"pis0=",psi0

    !relocate the current position
    CALL qmc_relocate( r0 , Omega , s01 , psi0 )

    !traverse the mesh from the current position to the vert <r1>
    CALL EVAL_celltraversal( Mesh , r0 , Omega , s01 , &
           Ns_ , iA=iA , sA=sA )
    IF( Ns==0 )THEN
     !WRITE(*,*)"given by BC"
     psi1=psi0
    ELSE
     !WRITE(*,*)"iA=",iA(1:Ns_)
     !WRITE(*,*)"sA=",sA(1:Ns_)

     !determine the source and cache it
     CALL qmc_Discover_Q(sA=sA(1:Ns_))

     !determine the total cross section and cache it
     CALL qmc_Discover_sigma(sA=sA(1:Ns_))

     !integrate using the cached source and cross section
     psi1 = qmc_fwdpsi_linsrc()
     !WRITE(*,*)"psi1=",psi1
    END IF

    CALL WRITELINE_AngularFlux( Unit , r1,Omega,Weights(m),psi1 )

   END DO points_loop

 END DO dire_loop

END DO ener_loop


!kill and release memory
CALL qmc_kill()


!wrapup variables
CALL CLEARn( iA )
CALL CLEARn( sA )

!!--end--
END SUBROUTINE



!!### SUBROUTINE <<WRITELINE_AngularFlux>>
SUBROUTINE WRITELINE_AngularFLux( Unit , r,Omega,Weight,Psi )
!!#### PURPOSE
!! Write a line of angular flux information.

!!#### REQUIRED INPUT
INTEGER       ,INTENT(IN) :: Unit
REAL(KIND_Rdp),INTENT(IN) :: r(2),Omega(3),Weight,Psi

!!--begin--

WRITE(Unit,"(7(1x,es23.15))")r(1),r(2),Omega(1),Omega(2),Omega(3),Weight,Psi

!!--end--
END SUBROUTINE


!!### SUBROUTINE <<OUTPUT_DebugPsiV>>
SUBROUTINE OUTPUT_DebugPsiV( Unit, iter, PointList, Mesh , fdbk )
!!#### PURPOSE
!! Write a line of angular flux information.

!!#### REQUIRED INPUT
INTEGER       ,INTENT(IN) :: Unit
INTEGER       ,INTENT(IN) :: iter
REAL(KIND_Rdp),INTENT(IN) :: PointList(:,:)
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk
REAL(KIND_Rdp) :: r(Mesh%NDim)
INTEGER :: n,k,m

!!--begin--
DO n=1,SIZE(PointList,2)
    r=PointList(:,n)
    k=GET_Vert(Mesh,Vert=r,tol=1.d-12)
    IF( k == 0 )CYCLE
    DO m=1,SIZE(AngularFluxV,3)
        WRITE(Unit,"(i5,5(1x,es23.15))")iter,r(1),r(2),Azimuthal_Angles(m),Polar_Angles(m),AngularFluxV(1,k,m)
    END DO
END DO

!!--end--
END SUBROUTINE


!!### SUBROUTINE <<TransportSweep_MCL_Cached>>
SUBROUTINE TransportSweep_MCL_Cached( AngularFluxV , &
  Directions , Mesh , pThread , WithinCell , fdbk , tol )

!!#### PURPOSE
!! Performs a long characteristics transport sweep using
!! distances and cells traversed cached in a setup stage.

!!#### EXTERNAL KINDS
USE KND_IntrinsicTypes                                      !!((01-A-KND_IntrinsicTypes.f90))
USE KND_DiscreteOrdinates,ONLY: KIND_DOR                    !!((02-A-KND_DiscreteOrdinates.f90))
USE KND_Mesh             ,ONLY: KIND_MSH                    !!((05-B-KND_Mesh.f90))
USE KND_MoCshort         ,ONLY: KIND_AngularFlux            !!((03-A-KND_MoCshort.f90))

!!#### GLOBAL USER MODULES
USE USR_Mesh                                                !!((14-B-USR_Mesh.f90))
USE USR_pThread                                             !!((03-A-USR_pThread.f90))
USE USR_fdbk                                                !!((08-C-USR_fdbk.f90))
USE USR_Characteristics                                     !!((78-C-USR_Characteristics.f90))
USE VAR_ScalarFluxes,ONLY: ScalarFluxC                      !!((04-C-VAR_ScalarFluxes.f90))
USE VAR_Characteristics,ONLY: Omega,r0,s01,Q,Q1,sigma       !!((75-C-VAR_Characteristics.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### REQUIRED INPUT
!! * directions
!! * mesh
!! * path threads
REAL(KIND_DOR)    ,POINTER       :: Directions(:,:)
TYPE(TYPE_Mesh)   ,INTENT(INOUT) :: Mesh
TYPE(TYPE_pThread),INTENT(IN)    :: pThread(:)
INTEGER           ,INTENT(IN)    :: WithinCell(:,:)

!!#### REQUIRED OUTPUT
!! * angular flux
REAL(KIND_AngularFlux),POINTER :: AngularFluxV(:,:,:)

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
!! * tolerance for long characteristics solver <tol>
TYPE(TYPE_fdbk)       ,OPTIONAL,INTENT(INOUT) :: fdbk
REAL(KIND_AngularFlux),OPTIONAL,INTENT(IN)    :: tol

!!#### LOCAL VARIABLES
INTEGER                :: g,m,p,k__,k,i,m_refl,c
INTEGER                :: Ng
REAL(KIND_AngularFlux) :: tol_,DEFAULT_tol

!!#### LOCAL PARAMETERS
LOGICAL,PARAMETER :: UpdateReflOnFly = .TRUE.

!!--begin--

CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MCS]] entering &
  &TransportSweep_MCL_Cached ... ")

!get default tolerance
DEFAULT_tol = 10._KIND_AngularFlux*EPSILON(1._KIND_AngularFlux)

!get tolerance
tol_ = DEFAULT( DEFAULT_tol , tol )

!get numbers of stuff
Ng = NUM_EnergyGroups( AngularFluxV )


!energy loop
ener_loop: DO g   = 1,Ng

 !enter main loop over angles
 dire_loop: DO m   = 1,SIZE(pthread)

  !proceed down each path
  path_loop: DO p   = 1,SIZE(pthread(m)%path)

   !in the proper order of verts
   vert_loop: DO k__ = 1,SIZE(pthread(m)%path(p)%order)

    !get the vert index
    k = pthread(m)%path(p)%order(k__)

    !check to see if we need to just use a boundary value
    i = WithinCell(k,m)

    !a) boundary vert with fixed value
    IF( i==0 )THEN

     CYCLE

    !b) reflective boundary
    ELSE IF( i<0 )THEN

     !update reflections on the fly
     IF( UpdateReflOnFly )THEN

      !for each azimuthal index determine the reflection
      m_refl = ABS(i)
      AngularFluxV( : , k , m ) = AngularFluxV( : , k , m_refl )

     END IF

     CYCLE

    END IF

    !relocate the current position
    c = INDEX_Characteristic(k,m)
    CALL qmc_Relocate( c )

    !determine the source and cache it
    CALL qmc_Discover_Q( UseCached=.TRUE. )

    !determine the total cross section and cache it
    CALL qmc_Discover_sigma( UseCached=.TRUE. )

    !calculate angular flux by forward characteristic
    AngularFluxV( g , k , m ) = qmc_fwdpsi_linsrc()

   END DO vert_loop

  END DO path_loop

 END DO dire_loop

END DO ener_loop


CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MCS]] exiting &
  &TransportSweep_MCL_Cached ... ")

!!--end--
END SUBROUTINE





!!### FUNCTION <<TEST_MCLCached_LinSrc>>
FUNCTION TEST_MCLCached_LinSrc( Directions , Mesh , pThread , &
  WithinCell , fdbk , tol , err , Unit )  RESULT(Pass)

!!#### PURPOSE
!! Performs a test on long characteristics transport sweep using
!! distances and cells traversed cached in a setup stage.

!!#### NOTES
!! This test is ONLY valid if there is:
!!  * no scattering,
!!  * constant total cross section,
!!  * and a linear external source of the form $q(x,y)=ax+by+c$.
!!  * non-reflective BC


!!#### EXTERNAL KINDS
USE KND_IntrinsicTypes                                      !!((01-A-KND_IntrinsicTypes.f90))
USE KND_DiscreteOrdinates,ONLY: KIND_DOR                    !!((02-A-KND_DiscreteOrdinates.f90))
USE KND_Mesh             ,ONLY: KIND_MSH                    !!((05-B-KND_Mesh.f90))
USE KND_MoCshort         ,ONLY: KIND_AngularFlux            !!((03-A-KND_MoCshort.f90))

!!#### GLOBAL USER MODULES
USE USR_Mesh                                                !!((14-B-USR_Mesh.f90))
USE USR_pThread                                             !!((03-A-USR_pThread.f90))
USE USR_fdbk                                                !!((08-C-USR_fdbk.f90))
USE USR_Characteristics                                     !!((78-C-USR_Characteristics.f90))
USE VAR_ScalarFluxes,ONLY: ScalarFluxC                      !!((04-C-VAR_ScalarFluxes.f90))
USE VAR_Characteristics,ONLY: Omega,r0,s01,Q,Q1,sigma,psi0  !!((75-C-VAR_Characteristics.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### REQUIRED INPUT
!! * directions
!! * mesh
!! * path threads
REAL(KIND_DOR)    ,POINTER       :: Directions(:,:)
TYPE(TYPE_Mesh)   ,INTENT(INOUT) :: Mesh
TYPE(TYPE_pThread),INTENT(IN)    :: pThread(:)
INTEGER           ,INTENT(IN)    :: WithinCell(:,:)

!!#### REQUIRED OUTPUT
!! * pass (or fail)
LOGICAL :: Pass

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
!! * tolerance for long characteristics solver <tol>
TYPE(TYPE_fdbk)       ,OPTIONAL,INTENT(INOUT) :: fdbk
REAL(KIND_AngularFlux),OPTIONAL,INTENT(IN)    :: tol
INTEGER               ,OPTIONAL,INTENT(IN)    :: Unit

REAL(KIND_AngularFlux),OPTIONAL,INTENT(OUT) :: err

!!#### LOCAL VARIABLES
INTEGER                :: g,m,p,k__,k,i,c
INTEGER                :: Ng
REAL(KIND_AngularFlux) :: tol_,DEFAULT_tol,approx,exact,maxdiff

!!--begin--

!write header to unit
IF( PRESENT(Unit) )THEN
 IF( Unit/=0 )THEN
  WRITE(Unit,"(3a6,17(1x,A))")"k","m","c","x0","y0","x1","y1","Ox","Oy","s01","Oz",&
    "Psi_approx","c_0","c_1","sigt","psi0","Psi_exact"
 END IF
END IF

!get default tolerance
DEFAULT_tol = 10._KIND_AngularFlux*EPSILON(1._KIND_AngularFlux)

!get tolerance
tol_ = DEFAULT( DEFAULT_tol , tol )

!get numbers of stuff
Ng = NUM_EnergyGroups( AngularFluxV )

maxdiff = 0.d0

!energy loop
ener_loop: DO g   = 1,Ng

 !enter main loop over angles
 dire_loop: DO m   = 1,SIZE(pthread)

  !proceed down each path
  path_loop: DO p   = 1,SIZE(pthread(m)%path)

   !in the proper order of verts
   vert_loop: DO k__ = 1,SIZE(pthread(m)%path(p)%order)

    !get the vert index
    k = pthread(m)%path(p)%order(k__)

    !check to see if we need to just use a boundary value
    i = WithinCell(k,m)

    !a) boundary vert with fixed value
    IF( i==0 )THEN

     CYCLE

    END IF

    !relocate the current position
    c = INDEX_Characteristic(k,m)
    CALL qmc_Relocate( c )

    !determine the source and cache it
    CALL qmc_Discover_Q( UseCached=.TRUE. )

    !determine the total cross section and cache it
    CALL qmc_Discover_sigma( UseCached=.TRUE. )

    !calculate angular flux by forward characteristic
    approx = qmc_fwdpsi_linsrc()
    exact  = EXACT_Psi_LinSrc(g,r0(1),r0(2),s01,Omega(1),Omega(2),sigma(1),psi0)

    IF( PRESENT(Unit) )THEN
     IF( Unit/=0 )THEN
      WRITE(Unit,"(3i6,17(1x,E26.14))")k,m,c,r0(1),r0(2),Vert(Mesh,k),Omega,s01,SQRT(Omega(1)**2+Omega(2)**2),&
        approx,Q(1),Q1(1),sigma(1),psi0,exact
     END IF
    END IF

    IF( .NOT.(ABS(approx-exact)<=tol_) )THEN
     WRITE(*,"(a)")"    problem with [k,m,c]=["//TRIM(STR(k))//","//TRIM(STR(m))//","//TRIM(STR(c))//"]"
     WRITE(*,"(a)")"    ABS(approx-exact)="//TRIM(STR(ABS(approx-exact)))
    END IF

    maxdiff = MAX(ABS(approx-exact),maxdiff)

   END DO vert_loop

  END DO path_loop

 END DO dire_loop

END DO ener_loop

Pass = (maxdiff<=tol_)

IF( PRESENT(err) )THEN
 err = maxdiff
END IF

!!--end--
END FUNCTION



!!### FUNCTION <<EXACT_Psi_LinSrc>>
FUNCTION EXACT_Psi_LinSrc(g,x0,y0,sN,Ox,Oy,sigt,psi0) RESULT(Psi)

!!#### PURPOSE
!! Return the exact angular flux for a global linear external
!! source, $q(x,y)=ax+by+c$.

!!#### DETAILS
!! The total cross section must be constant and the linear
!! source must be contained as the first and only source component
!! in the external source arrays.

!!#### REQUIRED INPUT
INTEGER       ,INTENT(IN) :: g
REAL(KIND_MSH),INTENT(IN) :: x0,y0,sN
REAL(KIND_DOR),INTENT(IN) :: Ox,Oy
REAL(KIND_AngularFlux),INTENT(IN) :: sigt,psi0

!!#### REQUIRED OUTPUT
REAL(KIND_AngularFlux) :: Psi

!!#### LOCAL VARIABLES
!! * parameters of the linear source
REAL(KIND_MCS) :: a,b,c,c0,c1,exp1

!!--begin--

a = Source(1)%strength1(1,g) / c_4_times_PI
b = Source(1)%strength1(2,g) / c_4_times_PI
c = Source(1)%strength0(  g) / c_4_times_PI

exp1 = 1._KIND_MCS - exp(-sigt*sN)

c0 = a*x0 + b*y0 + c
c1 = a*Ox + b*Oy

Psi = psi0*exp(-sigt*sN)  + &
      c0*exp1/sigt + c1*(sN-exp1/sigt)/sigt

!!--end--
END FUNCTION



!!### SUBROUTINE: <TransportSweep_MCL_Cached>
SUBROUTINE SETUP_TransportSweep_MCL_Cached( AngularFluxV , &
  Directions , Mesh , pThread , WithinCell , fdbk , tol )

!!#### PURPOSE
!! Caches long transport sweep variables.

!!#### EXTERNAL KINDS
USE KND_IntrinsicTypes                                      !!((01-A-KND_IntrinsicTypes.f90))
USE KND_DiscreteOrdinates,ONLY: KIND_DOR                    !!((02-A-KND_DiscreteOrdinates.f90))
USE KND_Mesh             ,ONLY: KIND_MSH                    !!((05-B-KND_Mesh.f90))
USE KND_MoCshort         ,ONLY: KIND_AngularFlux            !!((03-A-KND_MoCshort.f90))

!!#### GLOBAL USER MODULES
USE USR_Mesh                                                !!((14-B-USR_Mesh.f90))
USE USR_pThread                                             !!((03-A-USR_pThread.f90))
USE USR_fdbk                                                !!((08-C-USR_fdbk.f90))
USE USR_Characteristics                                     !!((78-C-USR_Characteristics.f90))
USE FUN_IsApprox                                            !!((03-A-FUN_IsApprox.f90))
USE FUN_Sentence                                            !!((04-B-FUN_Sentence.f90))
USE VAR_Characteristics,ONLY: SAVE_Characteristics,&        !!((75-C-VAR_Characteristics.f90))
                              LOAD_Characteristics,&
                              EXIST_Characteristics,&
                              TEST_Characteristics

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### REQUIRED INPUT
!! * directions
!! * mesh
!! * path threads
REAL(KIND_DOR)    ,POINTER       :: Directions(:,:)
TYPE(TYPE_Mesh)   ,INTENT(INOUT) :: Mesh
TYPE(TYPE_pThread),INTENT(IN)    :: pThread(:)
INTEGER           ,INTENT(IN)    :: WithinCell(:,:)

!!#### REQUIRED OUTPUT
!! * angular flux
REAL(KIND_AngularFlux),POINTER :: AngularFluxV(:,:,:)

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
!! * tolerance for long characteristics solver <tol>
TYPE(TYPE_fdbk)       ,OPTIONAL,INTENT(INOUT) :: fdbk
REAL(KIND_AngularFlux),OPTIONAL,INTENT(IN)    :: tol

!!#### LOCAL VARIABLES
INTEGER                                        :: g,m,p,k__,k,i,m_refl
REAL(KIND_MSH)                                 :: s01
REAL(KIND_MSH),DIMENSION(NUM_Dimensions_Mesh(Mesh)) :: r0,r1
REAL(KIND_qmc),DIMENSION(3)                    :: Omega
REAL(KIND_AngularFlux)                         :: psi0,psi1
INTEGER                                        :: Ng,Nd,Ns_,Nk,Nm,c,Nc,count
INTEGER        ,POINTER                        :: iA(:)
REAL(KIND_MSH) ,POINTER                        :: sA(:)
REAL(KIND_AngularFlux)                         :: tol_,DEFAULT_tol
REAL :: t0,t1,t2,t3,dt01,dt12,dt23
INTEGER :: NCellsTraversed
LOGICAL :: FEXIST
INTEGER,POINTER :: MemberCells(:,:)

!!#### LOCAL PARAMETERS
LOGICAL,PARAMETER :: UpdateReflOnFly = .TRUE.
LOGICAL,PARAMETER :: Testing_CharacteristicsOutput=.FALSE.
!!--begin--

!setup variables
CALL CLEARn( iA )
CALL CLEARn( sA )

!get default tolerance
DEFAULT_tol = 10._KIND_AngularFlux*EPSILON(1._KIND_AngularFlux)

!get tolerance
tol_ = DEFAULT( DEFAULT_tol , tol )

!get numbers of stuff
Ng = NUM_EnergyGroups( AngularFluxV )
Nd = NUM_Dimensions_Mesh(Mesh)
Nk = NUM_Verts( Mesh )
Nm = NUM_Directions( AngularFluxV )
Nc = NUM_Characteristics( AngularFluxV )

!get a reasonable value for the number of cells a
!characteristic travels through
NCellsTraversed = INT(SQRT(REAL(NUM_Cells(Mesh)))+1)

!get the cells each face belongs to
CALL GET_MemberCells(Mesh,MemberCells)

!initialize <qmc>
IF( Using_PackedCaching )THEN
 CALL qmc_SetOptions( NSamples=NCellsTraversed , ArrayOpt="1D" )
ELSE
 CALL qmc_SetOptions( NSamples=NCellsTraversed , ArrayOpt="2D" )
END IF

CALL qmc_init( Nd , Nk*Nm )

!set options for <qmc>
CALL qmc_SetOptions( tol=tol_ )

FEXIST = EXIST_Characteristics(TRIM(OutputFileBase))
IF( FEXIST )THEN
 CALL LOAD_Characteristics(TRIM(OutputFileBase))
 IF( qmc_Nc()==Nc )THEN
  CALL UPDATE(fdbk_comment,fdbk,s="[[MCS]] &
    &Loading cached characteristics--assuming it is the right data because &
    &number of characteristics is the same as expected.")
  RETURN
 ELSE
  CALL UPDATE(fdbk_comment,fdbk,s="[[MCS]] &
   &While loading cached characteristics the number of characteristics &
   &was not the same as expected---regenerating...")
 END IF
END IF

count = 0
dt01 = 0.
dt12 = 0.
dt23 = 0.

!energy loop
ener_loop: DO g   = 1,Ng

 !enter main loop over angles
 dire_loop: DO m   = 1,SIZE(pthread)

  !proceed down each path
  path_loop: DO p   = 1,SIZE(pthread(m)%path)

   !in the proper order of verts
   vert_loop: DO k__ = 1,SIZE(pthread(m)%path(p)%order)

    !get the vert index
    k = pthread(m)%path(p)%order(k__)
    count = count + 1

    !get the vertex
    r1 = Vert(Mesh,k)

    !get the direction
    Omega = Ordinates(:,m)

    CALL CPU_TIME(t0)

    !calculate distance to boundary and boundary psi
    CALL GET_LongBC(Mesh,BC,FixedAngularFlux,FunctionAngularFlux,&
      Ordinates,fdbk,m,r1,s01,r0,psi0)

    CALL CPU_TIME(t1)

    !get the index of the characteristic
    c = INDEX_characteristic(k,m)

    !evaluate the cell traversal
    CALL EVAL_celltraversal( Mesh , r0 , Omega , s01 , &
      Ns_ , iA=iA , sA=sA , MemberCells=MemberCells )

    CALL CPU_TIME(t2)

    !cache some stuff
    IF( MOD(c,100)==0 )THEN
     WRITE(*,*)"caching characteristic c=",c,"/",Nc,": ",TRIM(STR(REAL(count)*100/REAL(Nc),"(F5.1)")),"%"
    END IF

    CALL qmc_Cache( c , r0 , Omega , s01 , &
      psi0 , sQ=sA(1:Ns_) , iQ=iA(1:Ns_) , &
      ssigma=sA(1:Ns_) , isigma=iA(1:Ns_) )

    CALL CPU_TIME(t3)

    dt01 = dt01 + t1-t0
    dt12 = dt12 + t2-t1
    dt23 = dt23 + t3-t2

   END DO vert_loop

   WRITE(*,*)"long bc calculation    time: ",STRTIME(dt01)
   WRITE(*,*)"cell traversal calc    time: ",STRTIME(dt12)
   WRITE(*,*)"characteristic caching time: ",STRTIME(dt23)

  END DO path_loop

 END DO dire_loop

END DO ener_loop

!wrapup variables
CALL CLEARn( iA )
CALL CLEARn( sA )

!save the file
CALL SAVE_Characteristics(TRIM(OutputFileBase))
IF( Testing_CharacteristicsOutput )THEN
 WRITE(*,*)MERGE("PASS","FAIL",TEST_Characteristics(TRIM(OutputFileBase)))
END IF

!!--end--
END SUBROUTINE








SUBROUTINE UPDATE_Reflective_2Dprod( AngularFluxV , WithinCell , m_map , seq_ma , seq_k )
!!#### PURPOSE
!! Update the reflective boundaries.

!!#### COMMENTS
!! Optional orderings allow reflections to be updated correctly.  With
!! the default ordering this 2D routine runs N times (where N is the
!! the number of reflective boundary conditions) in order to make sure
!! all the directions are updated. Both the directional sequence AND
!! vertex sequence must be present in order to prevent the loop.

!!#### DEPENDENCIES
USE KND_MoCshort,ONLY: KIND_AngularFlux                     !!((03-A-KND_MoCshort.f90))

!!#### REQUIRED INPUT/OUTPUT
REAL(KIND_AngularFlux),POINTER :: AngularFluxV(:,:,:)

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: WithinCell(:,:)
INTEGER,INTENT(IN) :: m_map(:,:)

!!#### OPTIONAL INPUT
INTEGER,OPTIONAL,INTENT(IN) :: seq_ma(1:SIZE(m_map,2))
INTEGER,OPTIONAL,INTENT(IN) :: seq_k (1:SIZE(WithinCell,1))

!#LOCAL VARIABLES
INTEGER :: i,ma,k,mp,m_init,m_refl,mp0,k0,n

!!--begin--
DO n=1,MERGE(1,COUNT(BC==reflective_),PRESENT(seq_ma).AND.PRESENT(seq_k))
 DO mp0 = 1,SIZE(m_map,2)

  IF( PRESENT(seq_ma) )THEN
   ma = seq_ma(mp0)
  ELSE
   ma = mp0
  END IF

  DO k0 = 1,SIZE(WithinCell,1)

   IF( PRESENT(seq_k) )THEN
    k = seq_k(k0)
   ELSE
    k = k0
   END IF

   !determine within cell
   i = WithinCell(k,ma)

   !cycle on fixed boundary conditions (=0) or solution direction (>0)
   IF( i>=0 )CYCLE

   !update loop
   DO mp=1,SIZE(m_map,1)
    m_init = m_map(mp,ma)
    m_refl = m_map(mp,ABS(i))
    AngularFluxV( : , k , m_init ) = AngularFluxV( : , k , m_refl )
   END DO

  END DO
 END DO
END DO

!!--end--
END SUBROUTINE



!!### SUBROUTINE: <UPDATE_Reflective_2Dgen>
SUBROUTINE UPDATE_Reflective_2Dgen( AngularFluxV , WithinCell , seq_m , seq_k )
!!#### PURPOSE
!! Update the reflective boundaries.

!!#### COMMENTS
!! Optional orderings allow reflections to be updated correctly.  With
!! the default ordering this 2D routine runs N times (where N is the
!! the number of reflective boundary conditions) in order to make sure
!! all the directions are updated. Both the directional sequence AND
!! vertex sequence must be present in order to prevent the loop.

!!#### DEPENDENCIES
USE KND_MoCshort,ONLY: KIND_AngularFlux                     !!((03-A-KND_MoCshort.f90))

!!#### REQUIRED INPUT/OUTPUT
REAL(KIND_AngularFlux),POINTER :: AngularFluxV(:,:,:)

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: WithinCell(:,:)

!!#### OPTIONAL INPUT
INTEGER,OPTIONAL,INTENT(IN) :: seq_m(SIZE(AngularFluxV,3))
INTEGER,OPTIONAL,INTENT(IN) :: seq_k(SIZE(AngularFluxV,2))

!#LOCAL VARIABLES
INTEGER :: i,m,k,m_refl,m0,k0,n

!!--begin--
DO n=1,MERGE(1,COUNT(BC==reflective_),PRESENT(seq_m).AND.PRESENT(seq_k))
 DO m0 = 1,SIZE(AngularFluxV,3)

  IF( PRESENT(seq_m) )THEN
   m = seq_m(m0)
  ELSE
   m = m0
  END IF

  DO k0 = 1,SIZE(AngularFluxV,2)

   IF( PRESENT(seq_k) )THEN
    k = seq_k(k0)
   ELSE
    k = k0
   END IF

   !determine within cell
   i = WithinCell(k,m)

   !cycle on fixed boundary conditions (=0) or solution direction (>0)
   IF( i>=0 )CYCLE

   !update
   m_refl = ABS(i)
   AngularFluxV( : , k , m ) = AngularFluxV( : , k , m_refl )

  END DO
 END DO
END DO

!!--end--
END SUBROUTINE




!!### SUBROUTINE: <TransportSweep_MCS11_2Dprod>
SUBROUTINE TransportSweep_MCS11_2Dprod( AngularFluxV , CoeffScalarFluxM , ScalarFlux , ExtSourceC0 , &
  pThread , WithinCell , k_ , l_ , m_map , verts , MacT , &
  Ordinates , RecipSin , SourceDist , StreamDist , FrontPos , &
  fdbk )

!!#### PURPOSE
!! Express the scalar flux as a linear function.

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### REQUIRED INPUT
!! * coefficient of the flux
!! * scalar flux
!! * path threads
!! * which cell vertex k direction ma is in
!! * total cross section
!! * reciprocal of the sin of the azimuthal angle
!! * distance across cell in which the source accumulates
!! * position of intra-cell vertices on the front plane
REAL(KIND_ScalarFlux) ,INTENT(IN) :: CoeffScalarFluxM(:,:)
REAL(KIND_ScalarFlux) ,INTENT(IN) :: ScalarFlux(:,:)
REAL(KIND_ExtSource)  ,INTENT(IN) :: ExtSourceC0(:,:)
TYPE(TYPE_PThread)    ,INTENT(IN) :: pThread(:)
INTEGER               ,INTENT(IN) :: WithinCell(:,:)
INTEGER               ,INTENT(IN) :: k_(:,:,:)
INTEGER               ,INTENT(IN) :: l_(:)
INTEGER               ,INTENT(IN) :: m_map(:,:)
REAL(KIND_MSH)        ,INTENT(IN) :: verts(:,:)
REAL(KIND_Mac)        ,INTENT(IN) :: MacT(:,:)
REAL(KIND_DOR)        ,INTENT(IN) :: Ordinates(:,:)
REAL(KIND_DOR)        ,INTENT(IN) :: RecipSin(:)
REAL(KIND_MSH)        ,INTENT(IN) :: SourceDist(:,:)
REAL(KIND_MSH)        ,INTENT(IN) :: StreamDist(:,:,:)
REAL(KIND_MSH)        ,INTENT(IN) :: FrontPos(:,:,:)


!!#### REQUIRED OUTPUT
!! * angular flux sweep solution
REAL(KIND_AngularFlux),POINTER  :: AngularFluxV(:,:,:)


!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object [fdbk]
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk


!!#### LOCAL VARIABLES
REAL(KIND_DOR) :: PolSin(1:SIZE(RecipSin))
REAL(KIND_MSH) :: P_fi(1:SIZE(verts,1))
INTEGER :: p,ma,k__,i,k,l,k1,k2,g,mp,m__,m_init,m_refl
REAL(KIND_MSH) :: x(1:SIZE(verts,1)),z(1:SIZE(verts,1))
REAL(KIND_AngularFlux) :: f(1:SIZE(verts,1)),y(1:SIZE(verts,1))
REAL(KIND_AngularFlux) :: a(1:SIZE(verts,1)),satten_term,z_satten_term
REAL(KIND_AngularFlux) :: r(1:SIZE(verts,1)),exp1,exp2,y1,y2,x1,x2,exps

!!--begin--
!determine azimuthal sin (only needed in here)
PolSin = 1._KIND_DOR/RecipSin

!enter main loop over xy-plane angles
mxy_loop: DO ma = 1,SIZE(k_,3)
 !proceed down each path
 path_loop: DO p   = 1,SIZE(pthread(ma)%path)
  !in the proper order of verts
  order_loop: DO k__ = 1,SIZE(pthread(ma)%path(p)%order)

   !get the vert index
   k = pthread(ma)%path(p)%order(k__)

   !get the cell index
   i = WithinCell(k,ma)

   !cycle if there is no cell index (boundary vert with fixed value)
   IF( i==0 )CYCLE

   !if cell index <0 then we have a pure reflective boundary condition
   BC_clause: IF( i<0 )THEN

    !for each azimuthal index determine the reflection
    DO mp=1,SIZE(m_map,1)
     m_init = m_map(mp,ma)
     m_refl = m_map(mp,ABS(i))
     AngularFluxV( : , k , m_init ) = AngularFluxV( : , k , m_refl )
    END DO

   ELSE

    !get the material index
    l = l_(i)

    !get the two vertices for the interpolation of the scalar flux
    k1 = k_(1,k,ma)
    k2 = k_(2,k,ma)

    !get the point of face intersection
    P_fi = verts(:,k) - Ordinates(:,ma)*SourceDist(k,ma)

    !get the two x-values of the edge-endpoints
    x(1) = -xyDIST_PP( verts(:,k1) , P_fi )
    x(2) = +xyDIST_PP( P_fi , verts(:,k2) )

    !get the two z-values to use in coefficient construction
    z(1) = 0
    z(2) = SourceDist(k,ma)

    !get front positions
    x1 = FrontPos( 1 , k , ma )
    x2 = FrontPos( 2 , k , ma )

    !energy groups loop
    g_loop: DO g=1,SIZE(MacT,1)

     !calculate flux attenuation terms
     exps = exp( -MacT(g,l)*SourceDist(  k,ma) )
     exp1 = exp( -MacT(g,l)*StreamDist(1,k,ma) )
     exp2 = exp( -MacT(g,l)*StreamDist(2,k,ma) )

     !get the edge endpoint fluxes
     y(1) = ScalarFlux( g , k1 )
     y(2) = ScalarFlux( g , k2 )

     !get the intersection point fluxes and set the vertex fluxes
     f(1) = Interp1S_Linear( 0._KIND_MSH , y , x )
     f(2) = ScalarFlux(g,k)

     !determine the coefficients of the polynomial flux
     a = laVandermonde( z , f )

     !calculate source attenuation term
     satten_term   = exp( -MacT(g,l)*SourceDist(k,ma) )

     !for each azimuthal index
     mz_loop: DO mp=1,SIZE(m_map,1)

      !determine the mapped angular index
      m__ = m_map(mp,ma)

      !attenuation term with z-dependence added
      z_satten_term = satten_term**RecipSin(mp)

      !recursive level 0 (flat) solution
      r(1) = ( 1._KIND_AngularFlux  -                 z_satten_term )/(MacT(g,l)+1.D-20)
      !recursive level 1 (linear) solution
      r(2) = ( r(1)                - SourceDist(k,ma)*z_satten_term )/(MacT(g,l)+1.D-20)

      !compute source part of angular flux
      AngularFluxV( g , k , m__ ) = CoeffScalarFluxM(g,l)*DOT_PRODUCT(a,r) + &
        ExtSourceCellFunction(1,g,i)*r(1)

      !calculate streaming angular fluxes
      y1 = AngularFluxV( g , k1 , m__ )*(exp1**RecipSin(mp))
      y2 = AngularFluxV( g , k2 , m__ )*(exp2**RecipSin(mp))

      !fix up for discontinuous corners
      IF( y1<0._KIND_AngularFlux )THEN
       y1 = y2
      ELSE IF( y2<0._KIND_AngularFlux )THEN
       y2 = y1
      END IF

      !use linear interpolation to approximate unknown angular flux
      AngularFluxV( g , k , m__ ) = AngularFluxV( g , k , m__ ) + &
        ( y1*x2 - y2*x1 )/( x2 - x1 )

     END DO mz_loop


    END DO g_loop


   END IF BC_clause


  END DO order_loop

 END DO path_loop

END DO mxy_loop

!!--end--
END SUBROUTINE




!!### SUBROUTINE <SETUP_ExtSource>
SUBROUTINE SETUP_ExtSource( Mesh , ExtSourceCellFunction , fdbk , &
  SplitBoundarySource )

!!#### PURPOSE
!! Set up the external source.

!!#### DEPENDENCIES
USE TBX_Mesh                                                !!((15-B-TBX_Mesh.f90))
USE TBX_ComputationalGeometry                               !!((09-A-TBX_ComputationalGeometry.f90))
USE FUN_NewFile                                             !!((05-B-FUN_NewFile.f90))
USE VAR_Materials                                           !!((48-B-VAR_Materials.f90))
USE VAR_EnergyGroups                                        !!((47-B-VAR_EnergyGroups.f90))
USE PAR_Constants_Rdp,ONLY: c_4_times_PI                    !!((02-A-PAR_Constants_Rdp.f90))
USE USR_QDAnalyticTest,ONLY: Using_AnalyticSource,EXACT_QC  !!((47-C-USR_QDAnalyticTest.f90))
USE KND_Source                                              !!((02-A-KND_Source.f90))

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh)     ,INTENT(INOUT) :: Mesh
REAL(KIND_ExtSource),POINTER       :: ExtSourceCellFunction(:,:,:)

!!#### OPTIONAL INPUT
LOGICAL,OPTIONAL,INTENT(IN) :: SplitBoundarySource
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### LOCAL PARAMETERS
CHARACTER(*),PARAMETER :: proc_= "SETUP_ExtSource"
LOGICAL     ,PARAMETER :: Noisy_=.FALSE.

!!#### LOCAL VARIABLES
INTEGER                             :: i,n,a,g
REAL(KIND_MSH),DIMENSION(Mesh%NDim) :: P_centroid,CC
REAL(KIND_MSH)                      :: SAREA
LOGICAL                             :: Split_
REAL(KIND_ExtSource)                :: c0,c1(Mesh%NDim)
!!--begin--


CALL UPDATE(fdbk_comment,fdbk,s="[[MCS]] Determining external &
  &sources and locations...")
CALL DUMP(fdbk)

!initialize
Split_ = DEFAULT( .FALSE. , SplitBoundarySource )


!reorganize the polygon points for the source
DO a=1,SIZE(Source)
 n = SIZE(Source(a)%coeff,2)
 Source(a)%coeff = xyPOLYGON_Px( n , Source(a)%coeff )
END DO


!split the cells according to the source polygons
IF( Split_ )THEN
 DO a=1,SIZE(Source)
  CALL Split_Mesh_Pg( Mesh , SIZE(Source(a)%coeff,2) , Source(a)%coeff )
 END DO
END IF


!allocate external source
ALLOCATE( ExtSourceCellFunction(1+Mesh%Ndim,1:NG,1:Mesh%NCells) )
ExtSourceCellFunction = 0


!set the source in cells according to test cases
IF( Using_AnalyticSource )THEN

 IF( Ng/=1 )THEN
  CALL UpdateAndDump(fdbk_error,fdbk,s="[[MCS]] The exact QC is only for 1 group.")
 END IF

 DO i=1,NUM_Cells(Mesh)
  DO g=1,Ng
   !!\todo Fix this EXACT_QC to allow multigroup and more than flat source
   ExtSourceCellFunction(1,g,i) = EXACT_QC( Mesh , i ) /c_4_times_Pi
  END DO
 END DO

 IF( Using_AnalyticExtSourceOnly )THEN
  Using_Analyticsource = .FALSE.
  AnalyticTestCase = 0
 END IF

ELSE

 !set the source in cells by
 DO a=1,SIZE(Source)

  SAREA      = xySAREA_Pg   ( n , Source(a)%coeff )
  P_centroid = xyCENTROID_Pg( n , Source(a)%coeff , SAREA )

  DO i=1,NUM_Cells(Mesh)
   CC = CellCentroid(Mesh,i)
   IF( xyINTERIOR_PgP( n , Source(a)%coeff , CC , P_centroid ) )THEN
    DO g=1,NG
     c0 = Source(a)%strength0(  g) / c_4_times_PI
     c1 = Source(a)%strength1(:,g) / c_4_times_PI
     ExtSourceCellFunction(1:3,g,i) = (/c0 + DOT_PRODUCT(c1,CC),c1(1),c1(2)/)
    END DO
   END IF
  END DO
 END DO

END IF

!!--end--
END SUBROUTINE



!!### SUBROUTINE: <SETUP_AngularFlux>
SUBROUTINE SETUP_AngularFlux( AngularFluxV , WithinCell , &
  MacT , MacS , fdbk )

!!#### PURPOSE
!! Setup and initialize the angular flux variable.

!!#### REQUIRED INPUT
INTEGER               ,INTENT(IN) :: WithinCell(:,:)
REAL(KIND_Mac)        ,INTENT(IN) :: MacT(:,:)
REAL(KIND_Mac)        ,INTENT(IN) :: MacS(:,:)

!!#### REQUIRED OUTPUT
REAL(KIND_AngularFlux),POINTER :: AngularFluxV(:,:,:)

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_= "SETUP_AngularFlux"

!!#### LOCAL PARAMETERS
LOGICAL,PARAMETER :: Noisy_=.FALSE.

!!#### LOCAL VARIABLES
INTEGER :: i,ma,Nma,k,Nk,mp,Nmp,Nm,g,Ng

!!--begin--
!calculate numbers
Nk = SIZE(WithinCell,1)
Ng = SIZE(MacT,1)
Nm = SIZE(WithinCell,2)

!allocate
CALL TAP_ALLOCATE_P3("AngularFluxV",AngularFluxV,(/Ng,Nk,Nm/),&
  fdbk,init=0.1234567890123456789_KIND_AngularFlux)

!!--end--
END SUBROUTINE






!!### SUBROUTINE: <SETUP_BoundaryConditions_2Dgen>
SUBROUTINE SETUP_BoundaryConditions_2Dgen( Mesh , AngularFluxV , &
  Ordinates , WithinCell, fdbk )

!!#### PURPOSE
!! Set up the boundary condtions for a general set of directions.

!!#### DEPENDENCIES
USE KND_DiscreteOrdinates,ONLY: KIND_DOR                    !!((02-A-KND_DiscreteOrdinates.f90))
USE PAR_MoCshort                                            !!((03-A-PAR_MoCshort.f90))
USE VAR_MoCshort         ,ONLY: BC                          !!((47-B-VAR_MoCshort.f90))
USE TBX_Mesh                                                !!((15-B-TBX_Mesh.f90))
USE TBX_ComputationalGeometry                               !!((09-A-TBX_ComputationalGeometry.f90))
USE FUN_NewFile                                             !!((05-B-FUN_NewFile.f90))

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh)       ,INTENT(IN)    :: Mesh
REAL(KIND_AngularFlux),POINTER       :: AngularFluxV(:,:,:)
REAL(KIND_DOR)        ,INTENT(IN)    :: Ordinates(:,:)
INTEGER               ,INTENT(INOUT) :: WithinCell(:,:)

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_= "SETUP_BoundaryConditions"

!!#### LOCAL PARAMETERS
LOGICAL,PARAMETER :: Noisy_=.FALSE.

!!#### LOCAL VARIABLES
INTEGER :: i,j,jd,k,m,jb1,jb2,jd1,jd2
REAL(KIND_MSH),DIMENSION(Mesh%NDim)   :: outward_normal1,&
                                         outward_normal2
REAL(KIND_MSH) :: D1,D2,max0
INTEGER :: Unit,m_refl,Unit_mesh

!!--begin--

IF( Noisy_ )THEN
 Unit = NewFile(FILE=proc_//".noisy",STATUS="Replace")
 100 FORMAT(a,i8.5)
 101 FORMAT(a)
 102 FORMAT(a,2(i8.5,1x))
 103 FORMAT(a,2(e13.5,1x))
 104 FORMAT(a,i8.5,a,i8.5)
END IF

!get maximum value of the sin(theta) so we can
!make a good plane wave boundary condition
max0 = ABS(maxval( PolSin ))

LOOP_Directions: DO m=1,SIZE(WithinCell,2)

 IF( Noisy_ )THEN
  WRITE(Unit,100)"* Direction m=",m
 END IF

 LOOP_Verts: DO k=1,NUM_Verts(Mesh)

  IF( Noisy_ )THEN
   WRITE(Unit,100)"  * Vertex k=",k
  END IF

  !determine within cell
  i = WithinCell(k,m)

  IF( Noisy_ )THEN

   IF( i>0 )THEN
    WRITE(Unit,100)"    * particle trace is within cell i=",i
   ELSE
    WRITE(Unit,101)"    * particle trace is given by boundary conditions."
   END IF

  END IF

  !if it is NOT a boundary condition, go on
  IF( i>0 )CYCLE

  !get the boundary faces (jb1,jb2) and domain faces (jd1,jd2) that
  !the vert is on
  CALL BoundaryFacesFromVert(Mesh,k,jb1,jb2,jd1,jd2)

  !*Boundary condition resolution begins here*
  IF( Noisy_ )THEN
   WRITE(Unit,102)"    * vertex k="//TRIM(STR(k,"(i8.5)"))//" is part of boundary-faces, jb=",jb1,jb2
   IF( jd1==jd2 )THEN
    WRITE(Unit,100)"    * both boundary-faces are on the same domain-face, jd=",jd1
   ELSE
    WRITE(Unit,102)"    * boundary-face jb="//TRIM(STR(jb1,"(i8.5)"))//" is on domain-face, jd=",jd1
    WRITE(Unit,102)"    * boundary-face jb="//TRIM(STR(jb2,"(i8.5)"))//" is on domain-face, jd=",jd2
   END IF
  END IF

  !calculate each of the two boundary face's outward normals
  outward_normal1 = DomainFaceNormal(Mesh,jd1)
  IF( Noisy_ )THEN
   WRITE(Unit,103)"    * domain-face jd="//TRIM(STR(jd1,"(i8.5)"))//" has outward normal",outward_normal1
  END IF

  !fix up for hanging verts
  IF( jd2==0 )THEN
   jd2 = jd1
   jb2 = jb1
  END IF

  !corner verts
  IF( jd1/=jd2 )THEN
   outward_normal2 = DomainFaceNormal(Mesh,jd2)
   IF( Noisy_ )THEN
    WRITE(Unit,103)"    * domain-face jd="//TRIM(STR(jd2,"(i8.5)"))//&
      " has outward normal",outward_normal2
   END IF

  !non-corner verts
  ELSE
   outward_normal2 = outward_normal1
  END IF

  !CASE 1. Two Different Reflective Surfaces
  IF( (jd1/=jd2) .AND. (IsReflective(BC,jd1) .AND. IsReflective(BC,jd2))  )THEN

   WithinCell(k,m) = -EVAL_reflectiveBC( BC(jd1) , Ordinates(:,m) , &
      Ordinates(1:2,:) , WithinCell(k,:) , outward_normal1 , outward_normal2 )

   m_refl = ABS(WithinCell(k,m))

   IF( Noisy_ )THEN
    WRITE(Unit,104)"    * BC resolution is (double reflection): m=",&
      m," => m'=",m_refl
   END IF

  !CASE 2. Surface 1 is the proper Reflective Surface
  ELSE IF( IsReflective(BC,jd1) .AND. &
     xyDOT_VV( outward_normal1 , REAL(Ordinates(:,m),KIND_MSH) )<0._KIND_MSH )THEN

    WithinCell(k,m) = -EVAL_reflectiveBC( BC(jd1) , Ordinates(:,m) , &
      Ordinates , WithinCell(k,:) , outward_normal1 )

    m_refl = ABS(WithinCell(k,m))

    IF( Noisy_ )THEN
     WRITE(Unit,104)"    * BC resolution is (single reflection) off domain-face &
       &jd="//TRIM(STR(jd1,"(i8.5)"))//" yields: m=",m," => m'=",m_refl
    END IF

  !CASE 3. Surfce 2 is the proper Reflective Surface
  ELSE IF( IsReflective(BC,jd2) .AND. &
    xyDOT_VV( outward_normal2 , REAL(Ordinates(:,m),KIND_MSH) )<0._KIND_MSH )THEN

    WithinCell(k,m) = -EVAL_reflectiveBC( BC(jd2) , Ordinates(:,m) , &
      Ordinates , WithinCell(k,:) , outward_normal2 )

    m_refl = ABS(WithinCell(k,m))

    IF( Noisy_ )THEN
     WRITE(Unit,104)"    * BC resolution is (single reflection) off domain-face &
       &jd="//TRIM(STR(jd2,"(i8.5)"))//" yields: m=",m," => m'=",m_refl
    END IF


  !CASE 4. Fixed surfaces
  ELSE

   IF( Noisy_ )THEN
    WRITE(Unit,101)"    * BC resolution is (fixed) from &
      &domain-faces jd="//TRIM(STR(jd1,"(i8.5)"))//" and "//&
      TRIM(STR(jd2,"(i8.5)"))
   END IF

   ![waw] use a 45 degree discontinuity to run loren's test
   !(took absolute values away because they shouldn't be there)[waw]
   D1 = DOT_PRODUCT(Ordinates(1:2,m),DomainFaceNormal(Mesh,jd1))
   D2 = DOT_PRODUCT(Ordinates(1:2,m),DomainFaceNormal(Mesh,jd2))
   IF( IsApprox(D1,D2) )THEN
    AngularFluxV(:,k,m) = 0.5d0*(&
       EVAL_fixedBC( BC , FixedAngularFlux,FunctionAngularFlux , fdbk, jd1 , Mesh , Ordinates , m , &
       max0-abs(Ordinates(3,m))  , r=FaceCentroid(Mesh,jb1) ) + &
       EVAL_fixedBC( BC , FixedAngularFlux,FunctionAngularFlux , fdbk, jd2 , Mesh , Ordinates , m , &
       max0-abs(Ordinates(3,m))  , r=FaceCentroid(Mesh,jb2) ) )
   ELSE

    IF( D1<D2 )THEN
     AngularFluxV(:,k,m) = EVAL_fixedBC( BC , FixedAngularFlux,FunctionAngularFlux, fdbk , jd1 , Mesh , Ordinates, m , &
       max0-abs(Ordinates(3,m))  , r=FaceCentroid(Mesh,jb1) )
    ELSE
     AngularFluxV(:,k,m) = EVAL_fixedBC( BC , FixedAngularFlux,FunctionAngularFlux, fdbk , jd2 , Mesh , Ordinates, m , &
       max0-abs(Ordinates(3,m))  , r=FaceCentroid(Mesh,jb2) )
    END IF

   END IF

  END IF

  !*Boundary condition resolution ends here*

 END DO LOOP_Verts
END DO LOOP_Directions

IF( Noisy_ )CLOSE(Unit)

CALL UPDATE_Reflective_2Dgen(AngularFluxV,WithinCell)

!!--end--
END SUBROUTINE




SUBROUTINE UPDATE_ReflectiveBC2(Mesh,Ordinates,AngularFluxV,AngularFluxF,BC)
USE SUB_Swap
!!#### REQUIRED INPUT
TYPE(TYPE_Mesh)       ,INTENT(IN)    :: Mesh
REAL(KIND_AngularFlux),INTENT(INOUT) :: AngularFluxV(:,:,:),AngularFluxF(:,:,:)
REAL(KIND_DOR)        ,INTENT(IN)    :: Ordinates(:,:)
INTEGER               ,INTENT(IN)    :: BC(:)

!!#### LOCAL VARIABLES
INTEGER :: jd,jb1,jb2,jd1,jd2,k_,Nk,k,j,g,m,m0,m2
REAL(KIND_MSH),DIMENSION(Mesh%NDim) :: outward_normal1,&
                                       outward_normal2,r1,r2
REAL(KIND_MSH) :: D1,D2,max0,val1,val2
LOGICAL :: Incoming

!!--begin--
g=1
DO m0=1,SIZE(Ordinates,2)
 DO j=1,NUM_Faces(Mesh)
  IF( IsBoundaryFace(Mesh,j) )THEN
   jd = DomainFace(Mesh,j)
   IF( .NOT.IsReflective(BC(jd)) )CYCLE
   outward_normal1 = DomainFaceNormal(Mesh,jd)
   Incoming = DOT_PRODUCT( Ordinates(1:2,m0) , outward_normal1 )<0._KIND_MSH
   IF( Incoming )THEN
    m = EVAL_ReflectiveBC_POINT( m0 , Ordinates , outward_normal1 )

    AngularFluxF(g,j,m0) = AngularFluxF(g,j,m)

    Nk = SIZE(Mesh%Faces(j)%VertList)
    DO k_=1,Nk
     k = Mesh%Faces(j)%VertList(k_)

     CALL BoundaryFacesFromVert(Mesh,k,jb1,jb2,jd1,jd2)
     !fix up for hanging verts
     IF( jd2==0 )THEN
      jd2 = jd1
      jb2 = jb1
     END IF

     !make sure jd1 is jd (swap if not)
     IF( jd1/=jd )THEN
        CALL Swap(jd1,jd2)
        CALL Swap(jb1,jb2)
     END IF

     !corner verts
     IF( jd1/=jd2 .AND. IsReflective(BC(jd1)) .AND. IsReflective(BC(jd2)) )THEN

        !WRITE(*,*)
        !WRITE(*,*)'jd1=',jd1,' jd2=',jd2
        outward_normal2 = DomainFaceNormal(Mesh,jd2)
        m = EVAL_ReflectiveBC_POINT( m0 , Ordinates , outward_normal1 , outward_normal2 )
        !WRITE(*,*)'corner1 setting m0=',m0,'to m=',m
        AngularFluxV(g,k,m0) = AngularFluxV(g,k,m)

        !v2.23 correction: assuming bottom left corner, need reflections to quad II and quad IV
        m2 = EVAL_ReflectiveBC_POINT( m0 , Ordinates , outward_normal1 )
        !WRITE(*,*)'corner2 setting m2=',m2,'to m=',m
        AngularFluxV(g,k,m2) = AngularFluxV(g,k,m)
        m2 = EVAL_ReflectiveBC_POINT( m0 , Ordinates , outward_normal2 )
        !WRITE(*,*)'corner3 setting m2=',m2,'to m=',m
        AngularFluxV(g,k,m2) = AngularFluxV(g,k,m)

     !non-corner verts
     ELSE

        m = EVAL_ReflectiveBC_POINT( m0 , Ordinates , outward_normal1 )
        AngularFluxV(g,k,m0) = AngularFluxV(g,k,m)

     END IF

     

    END DO

   END IF

  END IF

 END DO
END DO

!!--end--
END SUBROUTINE



SUBROUTINE SETUP_IntraCell( WithinCell , &
                            SourceDist , &
                            NearestFace , &
                            k_ , &
                            fdbk , &
                            TravelDirections , InterpOrder , &
                            Mesh    )
!!#### PURPOSE
!! Calculate the IntraCell sweep parameters, WithinCell,
!! SourceDist, and NearestFace.  Also, calculate the verts
!! to use to interpolate with to determine the streaming
!! part of the angular flux solution.

!!#### DEPENDENCIES
USE KND_DiscreteOrdinates,ONLY: KIND_DOR                    !!((02-A-KND_DiscreteOrdinates.f90))
USE TBX_Mesh                                                !!((15-B-TBX_Mesh.f90))
USE TBX_ComputationalGeometry                               !!((09-A-TBX_ComputationalGeometry.f90))
USE FUN_Warning                                             !!((04-B-FUN_Warning.f90))
USE FUN_Random                                              !!((03-A-FUN_Random.f90))

!!#### REQUIRED OUTPUT
!! * the cell containing each vert+direction short
!!   characteristic [WithinCell]
!! * the distance along which the source accumulates
!!   (distance to face intersection) [SourceDist]
!! * the first face intersected along a short characteristic [NearestFace]
!! * the list of interpolant verts for each short characteristic (direction+vert) <k_>
INTEGER      ,POINTER :: WithinCell(:,:)
REAL(KIND_MSH),POINTER :: SourceDist(:,:)
INTEGER      ,POINTER :: NearestFace(:,:)
INTEGER      ,POINTER :: k_(:,:,:)

!!#### REQUIRED INPUT
!! * the directions of travel [TravelDirections]
!! * the FinalizedMesh object [Mesh]
INTEGER,INTENT(IN) :: InterpOrder
REAl(KIND_DOR),INTENT(IN) :: TravelDirections(:,:)
TYPE(TYPE_Mesh)      ,INTENT(INOUT) :: Mesh

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback variable [fdbk]
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
INTEGER        :: k,m,i,j,Ni,Nk,Nm
REAL(KIND_MSH) :: P_intersect(1:Mesh%NDim),Pn_front(1:Mesh%NDim+1)
REAL(KIND_MSH) :: r0(1:Mesh%NDim),Omega0(1:Mesh%NDim)
REAL :: tin,tout,dt
LOGICAL :: CHECK

!!--begin--


!initialize
Nm = SIZE(TravelDirections,2)
Nk = NUM_Verts(Mesh)
Ni = NUM_Cells(Mesh)



CALL UPDATE(fdbk_comment,fdbk,s=&
"[[MCS]] Determining the within-cell for each vert+direction...")
CALL DUMP(fdbk)
!!***********************************************************
CALL CPU_TIME(tin)
ALLOCATE( WithinCell(1:Nk,1:Nm) )
WithinCell = InteriorCells_Omega( Mesh , -TravelDirections )
CHECK = TEST_WithinCell0( WithinCell )
CALL UPDATE(fdbk_comment,fdbk,s=&
"[[MCS]] Checked the within-cell [test="//MERGE("PASS","FAIL",CHECK)//"]")
CALL DUMP(fdbk)
CALL CPU_TIME(tout)
dt = tout-tin
!!***********************************************************
CALL UPDATE(fdbk_comment,fdbk,s=&
"[[MCS]] Within-cell determined in [dt="//STRTIME(dt)//"]")
CALL DUMP(fdbk)



CALL UPDATE(fdbk_comment,fdbk,s=&
"[[MCS]] Finding the nearest faces...")
CALL DUMP(fdbk)
!!***********************************************************
CALL CPU_TIME(tin)
ALLOCATE( NearestFace(1:Nk,1:Nm) )
ALLOCATE( SourceDist(1:Nk,1:Nm) )

NearestFace = IntersectFaces_Omega(Mesh,&
                Omega         = -TravelDirections , &
                SDIST         = SourceDist        , &
                InteriorCells = WithinCell        , &
                PreferBoundaryFace = .TRUE.  )

CHECK = TEST_NearestFace0( NearestFace )
CALL UPDATE(fdbk_comment,fdbk,s=&
"[[MCS]] Checked the nearest-faces [test="//MERGE("PASS","FAIL",CHECK)//"]")
CALL DUMP(fdbk)
CALL CPU_TIME(tout)
dt = tout-tin
!!***********************************************************
CALL UPDATE(fdbk_comment,fdbk,s=&
"[[MCS]] Nearest faces determined in [dt="//STRTIME(dt)//"]")
CALL DUMP(fdbk)




CALL UPDATE(fdbk_comment,fdbk,s=&
"[[MCS]] Finding allowed interpolation verts...")
CALL DUMP(fdbk)
!!***********************************************************
CALL CPU_TIME(tin)

ALLOCATE( k_(1:InterpOrder+1,1:Nk,1:Nm) )
k_ = 0

!begin finding the nearest faces and boundary condition directions
DO m=1,Nm
 DO k=1,Nk
  j = NearestFace(k,m)
  IF( j==0 )CYCLE

  i = WithinCell(k,m)

  r0     = Vert(Mesh,k)
  Omega0 = TravelDirections(1:Mesh%NDim,m)

  P_intersect = r0 - Omega0*SourceDist(k,m)

  Pn_front = xyPLANE_PV( r0 , Omega0 )

  CALL CALC_InterpVerts_FixedPlane( Mesh , InterpOrder , i , P_intersect , &
       k_(:,k,m) , fdbk , Pn_front , DontUseVerts = (/k/) , &
       UseVerts=Mesh%Faces(ABS(j))%VertList )

 END DO

END DO

CALL CPU_TIME(tout)
dt = tout-tin

!!***********************************************************
CALL UpdateAndDump(fdbk_comment,fdbk,s=&
"[[MCS]] interpolation verts found in [dt="//STRTIME(dt)//"]")

!!--end--
END SUBROUTINE




SUBROUTINE DUMP_TrackingFile( Mesh , Directions , pThread , fdbk )

!!#### PURPOSE
!! Performs a long characteristics transport sweep.

!!#### EXTERNAL KINDS
USE KND_DiscreteOrdinates,ONLY: KIND_DOR                    !!((02-A-KND_DiscreteOrdinates.f90))
USE KND_Mesh             ,ONLY: KIND_MSH                    !!((05-B-KND_Mesh.f90))
USE KND_MoCshort         ,ONLY: KIND_AngularFlux            !!((03-A-KND_MoCshort.f90))
USE KND_XSExpansion      ,ONLY: KIND_Mac                    !!((02-A-KND_XSExpansion.f90))
USE KND_IntrinsicTypes                                      !!((01-A-KND_IntrinsicTypes.f90))
USE KND_Characteristics                                     !!((03-C-KND_Characteristics.f90))

!!#### GLOBAL USER MODULES
USE USR_pThread                                             !!((03-A-USR_pThread.f90))
USE USR_fdbk                                                !!((08-C-USR_fdbk.f90))
USE USR_tracks                                              !!((34-C-USR_tracks.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### REQUIRED INPUT
!! * path threads <pThread>
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh
REAL(KIND_qmc)    ,INTENT(IN) :: Directions(:,:)
TYPE(TYPE_pThread),INTENT(IN) :: pThread(:)

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
REAL(KIND_qmc)         :: r0(3),r1(3),Omega(3),s01,psi0
INTEGER       ,POINTER :: IA(:)
REAL(KIND_qmc),POINTER :: SA(:)
INTEGER                :: k,m
INTEGER                :: Nd,Nk,Nm
INTEGER                :: Unit_tracks
INTEGER                :: p,n
INTEGER                :: Ns

!!--begin--

!get numbers of stuff
Nd = NUM_Dimensions_Mesh(Mesh)
Nm = SIZE(Directions,2)
Nk = NUM_Verts(Mesh)


!open the tracks file
Unit_tracks = INIT_tracks(Nd,Nm,Nk,&
  Directions , &
  Mesh%Verts(:,1:Nk) )



!enter main loop over directions
dire_loop: DO m = 1,SIZE(pThread)

 !proceed down each path
 path_loop: DO p = 1,SIZE(pthread(m)%path)

  !in the proper order of verts
  order_loop: DO n = 1,SIZE(pthread(m)%path(p)%order)


   !get the vert index
   k = pthread(m)%path(p)%order(n)

   !get the current position
   r1(1:Nd) = Vert(Mesh,k)

   !get the current direction
   Omega = Directions(:,m)

   !calculate distance to boundary and boundary psi
   CALL GET_LongBC(Mesh,BC,FixedAngularFlux,FunctionAngularFlux,Ordinates,fdbk,m,&
     r1 , s01 , r0 , psi0 )

   !get the cells and distances traversed
   CALL EVAL_celltraversal( Mesh , r0 , Omega , s01 , Ns , iA , sA )

   !save this track
   CALL SAVE_track( Unit_tracks , m , k , Ns , iA , sA , psi0 )


  END DO order_loop

 END DO path_loop

END DO dire_loop


!close the tracks file
CALL CLOSE_tracks(Unit_tracks)


!!--end--
END SUBROUTINE




!!### FUNCTION <SourceIntegral0>
FUNCTION SourceIntegral0( TotSourceC0, SourceDist , RecipSin , MacT ) RESULT(psi_src)

!!#### PURPOSE
!! Integrate a flat isotropic source.

!!#### REQUIRED INPUT
REAL(KIND_AngularFlux),INTENT(IN) :: TotSourceC0,SourceDist,RecipSin,MacT

!!#### REQUIRED OUTPUT
REAL(KIND_AngularFlux) :: psi_src

!!--begin--

!void case - expand exponential into series, neglect cubic and higher terms
IF( MacT<1.d-6 )THEN

 psi_src =    TotSourceC0*( &
              SourceDist*RecipSin + &        !linear term
       MacT*((SourceDist*RecipSin)**2) )     !quadratic term

!non-void case - use the full characteristic
ELSE

 psi_src = ( TotSourceC0/MacT )*&
   ( 1._KIND_AngularFlux - exp(-MacT*SourceDist*RecipSin) )

END IF

!!--end--
END FUNCTION




!!### FUNCTION <SourceIntegral1>
FUNCTION SourceIntegral1( TotSourceC0, SourceDist , RecipSin , MacT , &
  g , i , P , CoeffScalarFluxM , ExtSourceC0 , ExtSourceC1 , Ordinates ) RESULT(psi_src)

!!#### PURPOSE
!! Integrate a linear isotropic source.

!!#### REQUIRED INPUT
!TYPE(TYPE_Mesh)       ,INTENT(IN) :: Mesh
REAL(KIND_AngularFlux),INTENT(IN) :: TotSourceC0,SourceDist,RecipSin,MacT
INTEGER               ,INTENT(IN) :: g,i
REAL(KIND_AngularFlux),INTENT(IN) :: P(2),CoeffScalarFluxM,ExtSourceC0,ExtSourceC1,Ordinates(2)

!!#### REQUIRED OUTPUT
REAL(KIND_AngularFlux) :: psi_src

!!#### LOCAL VARIABLES
REAL(KIND_AngularFlux) :: satt,r(2),f(2),z(2),u(2),a(2),aa(2,2)


!!--begin--

!void case - expand exponential into series, neglect cubic and higher terms
IF( MacT<1.d-6 )THEN

 psi_src =    TotSourceC0*( &
              SourceDist*RecipSin + &       !linear term
       MacT*((SourceDist*RecipSin)**2) )    !quadratic term

!non-void case - use the full characteristic
ELSE

 IF( SourceDist==0._KIND_AngularFlux )THEN
  psi_src = 0._KIND_AngularFlux
  RETURN
 END IF

 !get the barycentric vertex location
 u = P - CellCentroid(Mesh,i)

 !get the intersection point fluxes and set the vertex fluxes
 f(1) = EVAL_ScalarfluxLinear( g , i , u)
 f(2) = EVAL_ScalarfluxLinear( g , i , u - Ordinates*SourceDist  )

 !get the two z-values to use in coefficient construction
 z(1) = 0._KIND_AngularFlux
 z(2) = SourceDist

 !determine the coefficients of the polynomial flux
 a = laVandermonde( z , f )

 !aA(1,1) = 1._KIND_AngularFlux ; aA(1,2) = z(1)
 !aA(2,1) = 1._KIND_AngularFlux ; aA(2,2) = z(2)
 !CALL LA_GESV( aa, f )

 !calculate source attenuation term
 satt = exp( -MacT*SourceDist*RecipSin )

 !recursive level 0 (flat) solution
 r(1) = ( 1._KIND_AngularFlux  -            satt )/(MacT)
 !recursive level 1 (linear) solution
 r(2) = (       r(1)/recipsin  - SourceDist*satt )/(MacT)

 !compute source part of angular flux
 psi_src = CoeffScalarFluxM*a(1)*r(1) + ExtSourceC0*r(1) + ExtSourceC1*r(2)

 IF( a(2)*r(2)>0._KIND_AngularFlux )THEN
  psi_src = psi_src + CoeffScalarFluxM*a(2)*r(2)
 END IF
 !uncomment to verify that the symbolic coding of the integration
 !is correct by comparing to a numeric evaluation of the same integral
 !WRITE(*,*)"symbolic",psi_src
 !WRITE(*,*)"numeric ",CoeffScalarFlux*Integrate1_aq(SourceIntegrand,&
 !  (/0._KIND_AngularFlux,SourceDist/),N=20,tol=1.d-16 )
END IF

!!--end--
CONTAINS

PURE FUNCTION SourceIntegrand( s ) RESULT(f)
REAL(KIND_AngularFlux),INTENT(IN) :: s
REAL(KIND_AngularFlux) :: f
!!--begin--
f = recipsin*(a(1)+s*a(2))*exp(-MacT*s*recipsin)
!!--end--
END FUNCTION

END FUNCTION



FUNCTION EVAL_ScalarFluxLinear( g , i , r ) RESULT(val)
!!#### REQUIRED INPUT
INTEGER               ,INTENT(IN) :: g,i
REAL(KIND_AngularFlux),INTENT(IN) :: r(2)

!!#### REQUIRED OUTPUT
REAL(KIND_AngularFlux) :: val

!!--begin--
val = EVAL_SourceFunction(ScalarFluxCellFunction(:,g,i),r)
!!--end--
END FUNCTION

!!### FUNCTION <EVAL_ScalarFluxLinear>
FUNCTION EVAL_ScalarFluxLinear_old( g , i , r ) RESULT(val)

!!#### PURPOSE
!! Determine a scalar flux surface in the cell.

!!#### METHOD
!! The equation $f(x,y)=dx+ey$ is fit through least-squares
!! to the difference between the face-average and cell-average
!! scalar fluxes, assumed to lie on the face-centroid and cell-centroid
!! points (p), respectively.
!!
!! The cell-average scalar flux is then added to the resultant surface
!! cell-average scalar flux $val = f(r) + c$.
!!
!! A monotonization could be applied, such that the returned value
!! (the surface evaluated at $r$) must be contained within the range of
!! the face-average and cell-average values.


!!#### DETAILS
!! The problem with this method is that the surface only preserves the
!! cell-average scalar flux in the case that the cell is rectangular.

!!#### REQUIRED INPUT
!TYPE(TYPE_Mesh)       ,INTENT(IN) :: Mesh
!REAL(KIND_ScalarFlux) ,INTENT(IN) :: ScalarFluxF(:,:)
!REAL(KIND_ScalarFlux) ,INTENT(IN) :: ScalarFluxC(:,:)
INTEGER               ,INTENT(IN) :: g,i
REAL(KIND_AngularFlux),INTENT(IN) :: r(2)

!!#### REQUIRED OUTPUT
REAL(KIND_AngularFlux) :: val

!!#### LOCAL VARIABLES
INTEGER                     :: n,j,Nn
REAL(KIND_AngularFlux)      :: minf,maxf
!hack: a cell may have 20 faces maximum
REAL(KIND_AngularFlux),SAVE :: A(20,2),b(20,1),f(2),t(2)
INTEGER               ,SAVE :: g0=0,i0=0,info=0
LOGICAL          ,PARAMETER :: Using_Monotonization=.FALSE.

!!--begin--

IF( g==g0 .AND. i==i0 )GOTO 33

!set new saved values
g0 = g
i0 = i

!get rectangular matrix
minf = ScalarFluxC(g,i)
maxf = ScalarFluxC(g,i)
Nn = NUM_Faces(Mesh%Cells(i))
IF( Nn>20 )THEN
 WRITE(*,*)"EVAL_ScalarFluxLinear: 20 faces maximum per cell"
 STOP
END IF
DO n=1,Nn
 j = Mesh%Cells(i)%FaceList(n)
 A(n,:) = FaceCentroid(Mesh,ABS(j))-CellCentroid(Mesh,i)
 b(n,1) = ScalarFluxF(g,ABS(j))-ScalarFluxC(g,i)
 minf = MIN(minf,ScalarFluxF(g,ABS(j)))
 maxf = MAX(maxf,ScalarFluxF(g,ABS(j)))
END DO

!get least squares solution
CALL LA_GELS( A(1:Nn,1:2), b(1:Nn,1), TRANS="N", INFO=info )

33 CONTINUE

val = DOT_PRODUCT(b(1:2,1),r) + ScalarFluxC(g,i)

!monotonization
IF( Using_Monotonization )THEN
 IF( val<minf )THEN
  val = minf
 END IF
 IF( val>maxf )THEN
  val = maxf
 END IF
END IF

!!--end--
END FUNCTION



FUNCTION EVAL_Interpolation( k1,k2,k3 , x1,x2,x3 , y1,y2,y3 ,k,m,MAX_k,MAX_m ) RESULT(psi)
!!#### PURPOSE
!! Evaluate the 1D interpolation of y(x) for 3 values.

!!#### REQUIRED INPUT
INTEGER               ,INTENT(IN) :: k1,k2,k3
REAL(KIND_AngularFlux),INTENT(IN) :: x1,x2,x3
REAL(KIND_AngularFlux),INTENT(IN) :: y1,y2,y3

!!#### REQUIRED OUTPUT
REAL(KIND_AngularFlux) :: psi

!!#### OPTIONAL INPUT
INTEGER,INTENT(IN),OPTIONAL :: k,m,MAX_k,MAX_m

!!#### LOCAL VARIABLES
REAL(KIND_AngularFlux) :: c12,c23,c31

!!--begin--

IF( k1==0 )THEN
 IF( Using_LogTransform )THEN
  psi = exp( ( log(y2)*x3 - log(y3)*x2 )/( x3 - x2 ) )
 ELSE
  psi = ( y2*x3 - y3*x2 )/( x3 - x2 )
 END IF
ELSE IF( k2==0 )THEN
 IF( Using_LogTransform )THEN
  psi = exp( ( log(y1)*x3 - log(y3)*x1 )/( x3 - x1 ) )
 ELSE
  psi = ( y1*x3 - y3*x1 )/( x3 - x1 )
 END IF

ELSE IF( k3==0 )THEN
 IF( Using_LogTransform )THEN
  psi = exp( ( log(y1)*x2 - log(y2)*x1 )/( x2 - x1 ) )
 ELSE
  psi = ( y1*x2 - y2*x1 )/( x2 - x1 )
 END IF
ELSE

 !use quadratic interpolation to approximate unknown angular flux
 !(all this comes from symbolic evalutation in maple)
 IF( Using_LogTransform )THEN
  c12 = x1*x2
  c23 = x2*x3
  c31 = x3*x1
  psi = exp( c23*log(y1)/(-c12+c23-c31+x1**2) - &
             c31*log(y2)/(+c12+c23-c31-x2**2) + &
             c12*log(y3)/(+c12-c23-c31+x3**2)   )
 ELSE
  c12 = x1*x2
  c23 = x2*x3
  c31 = x3*x1
  psi = c23*y1/(-c12+c23-c31+x1**2) - &
        c31*y2/(+c12+c23-c31-x2**2) + &
        c12*y3/(+c12-c23-c31+x3**2)
 END IF

 !monotonization procedure
 IF( Using_Monotonization )THEN

  CALL Monotonize( psi , x1,x2,x3 , &
    y1,y2,y3 , c12,c23 ,k,m,MAX_k,MAX_m,NoBacksies=iter>5.AND.iter<80)

 !make some global enforcement so things don't explode
 ELSE IF( Using_ExplodeFix )THEN

  IF( psi<MIN_AngularFlux )THEN
   psi = MIN_AngularFlux
  ELSE IF( psi>MAX_AngularFlux )THEN
   psi = MAX_AngularFlux
  END IF

 END IF

END IF

!!--end--
END FUNCTION



!!### SUBROUTINE <<SOURCE_UPDATE_MoCshort>>
SUBROUTINE SOURCE_UPDATE_MoCshort( iter , dt0 , fdbk )

!!#### PURPOSE
!! Update the cell-average scalar flux, face-average scalar flux, and
!! face-average current in a simple way.

!!#### REQUIRED INPUT/OUTPUT
INTEGER,INTENT(IN) :: iter
REAL,INTENT(INOUT) :: dt0

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_="SOURCE_UPDATE_MoCshort"

!!#### LOCAL VARIABLES
REAL                  :: tin,tout,dt
TYPE(varying_string)  :: file
INTEGER               :: i,j,g,Unit,num,m
INTEGER               :: Nj,Ni,Ndim
REAL(KIND_AngularFlux) :: tau
!!--begin--

!=========
!++SETUP++
!=========

!! allocate stuff
Nj = NUM_Faces(Mesh)
Ni = NUM_Cells(Mesh)
Ndim = NUM_Dimensions_Mesh(Mesh)

CALL TAP_ALLOCATE_P2("CurrentFN",CurrentFN,(/Ng,Nj/),&
  fdbk,init=0.d0)
CALL TAP_ALLOCATE_P2("ScalarFluxC",ScalarFluxC,(/Ng,Ni/),&
  fdbk,init=0.d0)
CALL TAP_ALLOCATE_P2("ScalarFluxF",ScalarFluxF,(/Ng,Nj/),&
  fdbk,init=0.d0)

IF( .NOT.Using_SBCharacteristics )THEN

 CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MCS]] Generating <HI_ScalarFluxF> &
   &by approx. integration...")
 CALL UPDATE_HI_ScalarFluxF( ScalarFluxF , ScalarFluxV               , &
   Mesh , fdbk , varname="HI_ScalarFluxF" )

 CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MCS]] Generating <HI_ScalarFluxC> &
   &by approx. integration...")
 CALL UPDATE_HI_ScalarFluxC( ScalarFluxC , ScalarFluxF , ScalarFluxV , &
   Mesh , fdbk , varname="HI_ScalarFluxC" )

 CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MCS]] Generating <HI_CurrentFN> &
   &by approx. integration...")
 CALL UPDATE_HI_CurrentFN  ( CurrentFN   , CurrentV                  , &
   Mesh , fdbk , varname="HI_CurrentV" )

END IF

!! Allocate the sources functions.
CALL TAP_ALLOCATE_P3("TotSourceCellFunction",TotSourceCellFunction,&
  (/Ndim+1,Ng,NUM_Cells(Mesh)/),fdbk,init=0.d0)

CALL TAP_ALLOCATE_P3("ScalarFluxCellFunction",ScalarFluxCellFunction,&
  (/Ndim+1,Ng,NUM_Cells(Mesh)/),fdbk,init=0.d0)

!start the timer
CALL CPU_TIME(tin)


!=====================
!++SOURCE GENERATION++
!=====================

!! 7. Update the sources.
CALL msg_UPDATE_CellFunction(CellFunctionMethod,MCS_KEY_CellFunction,fdbk)
CALL UPDATE_CellFunction(Mesh,ScalarFluxC,ScalarFluxF,ScalarFluxCellFunction,&
  Method=MCS_KEY_CellFunction(CellFunctionMethod),&
  NonlinearFixup=MCS_KEY_NonlinearFixup(NonlinearFixup) )

CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MCS]] Generating total sources...")
CALL UPDATE_TotSource_1Mom( Mesh,Ng,ExtSourceCellFunction , &
       CoeffScalarFluxM, l_ , ScalarFluxCellFunction , &
       TotSourceCellFunction ) !output

!Check if the source is positive.
CALL CHECK_TotSourcePositive(Mesh,TotSourceCellFunction,fdbk)
IF( Using_AnalyticTransportTest )THEN
 !check each direction's source
 DO m=1,SIZE(pThread)
  CALL UPDATE_QextA(MCS_KEY_CellFunction(CellFunctionMethodA),&
   Mesh,ExtSourceCellFunction,&
   Omega=Ordinates(1:2,m),mIndices=(/m,SIZE(pThread)/))
  CALL UPDATE_TotSource_1Mom( Mesh,Ng,ExtSourceCellFunction , &
       CoeffScalarFluxM, l_ , ScalarFluxCellFunction , &
       TotSourceCellFunction ) !output
  CALL CHECK_TotSourcePositive(Mesh,TotSourceCellFunction,fdbk)
 END DO
END IF

IF( iter>0 )THEN

 IF( CHECKING_BALANCE )THEN
  CALL CHECK_ScalarBalanceEquation(Mesh,&
    ScalarFluxC,CurrentFN,c_4_times_PI*TotSourceCellFunction,&
    MacT,l_,fdbk,abstol=tolBalAbs,reltol=tolBalRel,caller="[[MCS]]")
 ELSE
  IF( LEN(NO_BALANCE_MSG)==0 )THEN
   NO_BALANCE_MSG = "[[MCS]] the balance equation is not being evaluated ..."
  END IF
  CALL UpdateAndDump(fdbk_comment,fdbk,s=STR(NO_BALANCE_MSG))
 END IF

END IF

!==========
!++WRAPUP++
!==========

!end time
CALL CPU_TIME(tout)
dt = (tout-tin)

!timing update
CALL UpdateAndDump(fdbk_profile,fdbk,s="[[MCS]] Source update completed in [time="//&
  STRTIME(dt)//"]")

!update global time
dt0 = dt0 + dt



!!--end--
END SUBROUTINE




SUBROUTINE msg_UPDATE_CellFunction(CellFunctionMethod,MCS_KEY_CellFunction,fdbk)
INTEGER,INTENT(IN) :: CellFunctionMethod
CHARACTER(*),INTENT(IN) :: MCS_KEY_CellFunction(:)
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk
!!--begin--
IF( CellFunctionMethod>=1 .AND. CellFunctionMethod<=SIZE(MCS_KEY_CellFunction) )THEN
 CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MCS]] Updating the total &
   &ScalarFluxCellFunction from ScalarFluxC and ScalarFluxF using [method="//&
   TRIM( MCS_KEY_CellFunction(CellFunctionMethod) )//"]")
ELSE
 CALL UpdateAndDump(fdbk_error,fdbk,s="[[MCS]] the CellFunctionMethod is not set.")
END IF
!!--end--
END SUBROUTINE


SUBROUTINE msg_KarpovTest(fdbk)
USE USR_fdbk                                                !!((08-C-USR_fdbk.f90))
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk
!!--begin--
IF( TEST_Karpov(.FALSE.) )THEN
 CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MCS]] USR_SBCharacteristics :: KarpovPParabolic [test=PASS]")
ELSE
 CALL UpdateAndDump(fdbk_error  ,fdbk,s="[[MCS]] USR_SBCharacteristics :: KarpovPParabolic [test=FAIL]")
END IF
!!--end--
END SUBROUTINE






SUBROUTINE RayEffectsStatistics_2D(Mesh,Directions,rayeffectsfile,fdbk)
USE FUN_EXIST                                               !!((04-B-FUN_EXIST.f90))
USE FUN_STR                                                 !!((05-B-FUN_STR.f90))
!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
REAL(KIND_Rdp) ,INTENT(IN) :: Directions(:,:)
CHARACTER(*)   ,INTENT(IN) :: rayeffectsfile

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### LOCAL VARIABLES
REAL(KIND_Rdp) :: MeanCellVolume,StdDevCellVolume
REAL(KIND_Rdp) :: MeanAngle,StdDevAngle
REAL(KIND_Rdp) :: OmegaStar,Lbar,hstar
REAL(KIND_Rdp) :: StdDevhstar,StdDevOmegaStar,u1(2),u2(2)
REAL(KIND_Rdp),ALLOCATABLE :: MinVals(:,:),Angles(:,:)
INTEGER :: i,m1,m2,n,Nm,unit,Ntest
REAL :: Wnumber
TYPE(varying_string) :: rayeffectsfile_

!!--begin--

rayeffectsfile_ = ""

MeanCellVolume = 0.d0
DO i=1,NUM_Cells(Mesh)
 MeanCellVolume = MeanCellVolume + CellVolume(Mesh,i)
END DO
MeanCellVolume = MeanCellVolume/NUM_Cells(Mesh)

StdDevCellVolume = 0.d0
DO i=1,NUM_Cells(Mesh)
 StdDevCellVolume = StdDevCellVolume + (MeanCellVolume-CellVolume(Mesh,i))**2
END DO
StdDevCellVolume = SQRT(StdDevCellVolume/NUM_Cells(Mesh))

Lbar=SQRT(Mesh%Domain%Cell%CellVolume)
CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MCS]] The domain has characteristic length &
  &[Lbar="//TRIM(STR(Lbar,"(Es11.4)"))//"].")

CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MCS]] The mesh cell statistics are &
  &[MeanCellVolume="//TRIM(STR(MeanCellVolume,"(Es9.2)"))//"] with standard &
  &deviation [StdDevCellVolume="//TRIM(STR(StdDevCellVolume,"(Es9.2)"))//"].")


hstar = SQRT(MeanCellVolume)
StdDevhstar = SQRT(0.5d0/MeanCellVolume)*StdDevCellVolume
CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MCS]] The characteristic mesh cell &
  &width is [hstar="//TRIM(STR(hstar,"(Es11.4)"))//"] with standard &
  &deviation [StdDevhstar="//TRIM(STR(StdDevhstar,"(Es11.4)"))//"].")

Nm = SIZE(Directions,2)
Ntest = 3
WRITE(*,*)"Nm=",Nm
ALLOCATE( Angles(Nm,Nm) , MinVals(Nm,Ntest) )
DO m1=1,Nm
 DO m2=1,Nm
  u1=Directions(1:2,m1)/SQRT(SUM(Directions(1:2,m1)**2))
  u2=Directions(1:2,m2)/SQRT(SUM(Directions(1:2,m2)**2))
    Angles(m1,m2) = ABS(ACOS(DOT_PRODUCT( u1,u2 )))
  WRITE(*,*)"Angles=",Angles(m1,m2)
 END DO
END DO

!find Ntest nearest neighbors for each direction
DO m1=1,Nm
 DO n=1,Ntest
  m2 = MINLOC( Angles(m1,:) , 1 )
  MinVals(m1,n) = Angles(m1,m2)
  Angles(m1,m2)=HUGE(1.d0)
  WRITE(*,*)"minvals=",Angles(m1,m2)
 END DO
END DO
DEALLOCATE( Angles )

MeanAngle = 0.d0
DO m1=1,SIZE(Directions,2)
 DO n=1,Ntest
  MeanAngle = MeanAngle + MinVals(m1,n)
 END DO
END DO
MeanAngle=MeanAngle/(Nm*Ntest)

StdDevAngle = 0.d0
DO m1=1,SIZE(Directions,2)
 DO n=1,Ntest
  StdDevAngle = StdDevAngle + (MinVals(m1,n)-MeanAngle)**2
 END DO
END DO
StdDevAngle=SQRT(StdDevAngle/(Nm*Ntest))

CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MCS]] The quadrature set statistics are &
  &[MeanAngle="//TRIM(STR(MeanAngle,"(Es9.2)"))//"] with standard &
  &deviation [StdDevAngle="//TRIM(STR(StdDevAngle,"(Es9.2)"))//"].")

OmegaStar=Lbar*MeanAngle
StdDevOmegaStar=Lbar*StdDevAngle

CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MCS]] The characteristic arc length &
  & is [OmegaStar="//TRIM(STR(OmegaStar,"(Es9.2)"))//"] with standard &
  &deviation [StdDevOmegaStar="//TRIM(STR(StdDevOmegaStar,"(Es9.2)"))//"].")

!WRITE(*,*)"OmegaStar<hStar=",OmegaStar<hStar
IF( OmegaStar<hStar )THEN
 CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MCS]] Solution is safe from ray effects &
  &because OmegaStar<hstar.")
ELSE
 CALL UpdateAndDump(fdbk_warning,fdbk,s="[[MCS]] Solution may have bad ray effects &
  &because OmegaStar>hstar.")
END IF

IF( TRIM(rayeffectsfile)=="" )THEN
 rayeffectsfile_ = OutputFileBase//".rayinfo"
ELSE
 rayeffectsfile_ = rayeffectsfile
END IF

unit=NewUnit()
IF( .NOT.Exist(STR(rayeffectsfile_)) )THEN
 OPEN(unit=unit,file=STR(rayeffectsfile_),Status="new")
 WRITE(unit,"(a40,12(a16,1x))")"OutputFileBase",&
  "NCells","NAng",&
  "MeanCellVol","SDCellVol",&
  "MeanHstar","SDHstar",&
  "MeanAngle","SDAngle",&
  "Lbar","OmegaStar","SDOmegaStar","RPC","CHECK"
ELSE
 OPEN(unit=unit,file=STR(rayeffectsfile_),Status="old",Position="append")

END IF
Wnumber = hStar/OmegaStar !number of rays per cell
 CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MCS]] The approximate number of rays &
  &per cell in the worst case is [RPC="//TRIM(STR(Wnumber,"(f8.3)"))//"].")

WRITE(unit,"(a40,2(i7,10x),9(Es16.6,1x),f5.1,a5)")TRIM(STR(OutputFileBase)),&
  NUM_Cells(Mesh),SIZE(Directions,2),&
  MeanCellVolume,StdDevCellVolume,&
  hstar,StdDevhstar,&
  MeanAngle,StdDevAngle,&
  Lbar,OmegaStar,StdDevOmegaStar,Wnumber,MERGE("PASS","FAIL",Wnumber>1)

CLOSE( Unit )

!!--end--
END SUBROUTINE


END MODULE
