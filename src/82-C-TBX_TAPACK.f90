!!# TOOLBOX MODULE >>TBX_TAPACK<<
MODULE TBX_TAPACK

!!## PURPOSE
!! The toolbox routines that support TAPACK.

!!## MODULES
USE KND_DiscreteOrdinates     !!((02-A-KND_DiscreteOrdinates.f90))
USE PAR_Constants_Rdp         !!((02-A-PAR_Constants_Rdp.f90))
USE FUN_CellGradient_Gauss    !!((03-A-FUN_CellGradient_Gauss.f90))
USE FUN_IsApprox              !!((03-A-FUN_IsApprox.f90))
USE FUN_Smush                 !!((03-A-FUN_Smush.f90))
USE ISO_varying_string        !!((03-A-ISO_varying_string.f90))
USE PAR_MoCshort,ONLY: KEY_BC !!((03-A-PAR_MoCshort.f90))
USE VAR_Units                 !!((03-A-VAR_Units.f90))
USE FUN_Default               !!((04-A-FUN_Default.f90))
USE SUB_CLEAR                 !!((04-A-SUB_CLEAR.f90))
USE FUN_NewUnit               !!((04-B-FUN_NewUnit.f90))
USE SUB_Reallocate            !!((04-B-SUB_Reallocate.f90))
USE SUB_Stop                  !!((04-B-SUB_Stop.f90))
USE VAR_AngularFluxes         !!((04-C-VAR_AngularFluxes.f90))
USE VAR_Currents              !!((04-C-VAR_Currents.f90))
USE VAR_EddingtonFactors      !!((04-C-VAR_EddingtonFactors.f90))
USE VAR_ScalarFluxes          !!((04-C-VAR_ScalarFluxes.f90))
USE FUN_Jiggle                !!((05-B-FUN_Jiggle.f90))
USE FUN_NewFile               !!((05-B-FUN_NewFile.f90))
USE FUN_STR                   !!((05-B-FUN_STR.f90))
USE FUN_TimeStamp             !!((05-B-FUN_TimeStamp.f90))
USE FUN_VSTR                  !!((05-B-FUN_VSTR.f90))
USE FUN_xyANGLE               !!((05-B-FUN_xyANGLE.f90))
USE PAR_TAPACK                !!((05-C-PAR_TAPACK.f90))
USE SUB_SAVE                  !!((06-B-SUB_SAVE.f90))
USE SUB_SAVEn                 !!((06-B-SUB_SAVEn.f90))
USE FUN_STRTIME               !!((06-C-FUN_STRTIME.f90))
USE FUN_Substitute            !!((06-C-FUN_Substitute.f90))
USE PRN_Text                  !!((07-B-PRN_Text.f90))
USE INT_LAPACK2               !!((08-B-INT_LAPACK2.f90))
USE USR_fdbk                  !!((08-C-USR_fdbk.f90))
USE TBX_SIO                   !!((10-A-TBX_SIO.f90))
USE PRN_Table                 !!((11-B-PRN_Table.f90))
USE KND_Mesh                  !!((05-B-KND_Mesh.f90))
USE PAR_Mesh                  !!((06-B-PAR_Mesh.f90))
USE USR_Mesh                  !!((14-B-USR_Mesh.f90))
USE TBX_Mesh,ONLY:&           !!((15-B-TBX_Mesh.f90))
    IsBoundaryFace,&
    FaceArea,PRINT_Mesh,WRAPUP_Mesh,&
    SETUP_Mesh,SETUP_IntegralRegions
USE FUN_VSTROPTION            !!((16-B-FUN_VSTROPTION.f90))
USE PRN_Mesh                  !!((16-C-PRN_Mesh.f90))
USE USR_Source                !!((35-B-USR_Source.f90))
USE VAR_Mesh                  !!((46-B-VAR_Mesh.f90))
USE VAR_QuasiDiffusion        !!((46-B-VAR_QuasiDiffusion.f90))
USE CCS_Mesh                  !!((47-B-CCS_Mesh.f90))
USE VAR_DiscreteOrdinates     !!((47-B-VAR_DiscreteOrdinates.f90))
USE VAR_EnergyGroups          !!((47-B-VAR_EnergyGroups.f90))
USE VAR_MoCshort              !!((47-B-VAR_MoCshort.f90))
USE VAR_Source                !!((47-B-VAR_Source.f90))
USE VAR_XSMonkey              !!((47-B-VAR_XSMonkey.f90))
USE VAR_Materials             !!((48-B-VAR_Materials.f90))
USE USR_MoCshort              !!((48-C-USR_MoCshort.f90))
USE USR_TAPACK                !!((48-C-USR_TAPACK.f90))
USE PRN_QuasiDiffusion,ONLY:& !!((68-B-PRN_QuasiDiffusion.f90))
    PRINT_LO_SystemInfo,&
    PRINT_IterTable
USE TBX_DiscreteOrdinates     !!((56-B-TBX_DiscreteOrdinates.f90))
USE USR_TransportAnalyticTest !!((56-C-USR_TransportAnalyticTest.f90))
USE VAR_TAPACK                !!((66-C-VAR_TAPACK.f90))
USE CCS_TAPACK                !!((68-C-CCS_TAPACK.f90))
USE TBX_QuasiDiffusion        !!((69-B-TBX_QuasiDiffusion.f90))
USE PRN_TAPACK                !!((76-C-PRN_TAPACK.f90))
USE TBX_MoCshort              !!((81-B-TBX_MoCshort.f90))
! compilation has problem with ifort 11.0 compiler due to this
! module file use
USE PRN_MoCshort              !!((77-B-PRN_MoCshort.f90))
USE VAR_FileSystem,ONLY: fsTemp,fsInput,fsData,fsOutput,fsSeparator
USE VAR_QDAnalyticTest,ONLY: AnalyticTestCase        !!((33-C-VAR_QDAnalyticTest.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

!!#### LOCAL PARAMETERS
CHARACTER(*),PARAMETER :: mod_ = "TBX_TAPACK"

!!#### PRIVATE ACCESS LIST
PRIVATE :: mod_

!!#### PUBLIC LIST
PUBLIC :: INPUT_TAPACK
PUBLIC :: SETUP_TAPACK
PUBLIC :: SOLVE_TAPACK_LowOrder
PUBLIC :: SOLVE_TAPACK_HighOrder
PUBLIC :: REFINE_TAPACK
PUBLIC :: CHECK_TAPACK
PUBLIC :: WRAPUP_TAPACK

PUBLIC :: PRINT_Solution
PUBLIC :: PRINT_SolutionAux
PUBLIC :: PRINT_InputEcho
!deprecated routine
!PUBLIC :: PRINT_TAPACK

!!#### MODULE PROCEDURES
CONTAINS




!
! !!### SUBROUTINE >>PRINT_TAPACK<<
! SUBROUTINE PRINT_TAPACK( OutputJob , fdbk )
! !!#### PURPOSE
! !! Control the output printing for TAPACK.
!
! !!#### REQUIRED INPUT
! !! * output job
! INTEGER       ,INTENT(IN)    :: OutputJob
!
! !!#### OPTIONAL INPUT/OUTPUT
! !! * feedback <fdbk>
! TYPE(TYPE_FdBk),OPTIONAL,INTENT(INOUT) :: fdbk
!
! !!--begin--
!
! !output a chunk of data according to OutputJob
! SELECT CASE( OutputJob )
!  CASE(InputEcho_) ; CALL PRINT_InputEcho( fdbk )
!  CASE(Solution_ ) ; CALL PRINT_Solution ( fdbk )
! END SELECT
!
! !!--end--
! END SUBROUTINE


!!### SUBROUTINE >>PRINT_Solution<<
SUBROUTINE PRINT_Solution(fdbk,iter)
!!#### PURPOSE
!! Controls printing of the main solution output file,
!! <tap.out>, called from <<PRINT_TAPACK>>.

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_="PRINT_Solution"

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_FdBk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### OPTIONAL INPUT
INTEGER,OPTIONAL,INTENT(IN) :: iter

!!#### LOCAL VARIABLES
INTEGER :: n,iout,Unit_,m
REAL    :: tin,tout,dt0
TYPE(varying_string) :: file

!!--begin--
!start time
CALL CPU_TIME(tin)

IF( PRESENT(iter) )THEN
 IF( .NOT.PrintEachIteration )RETURN
 file = TRIM(OutputFileBase)//"_s%s"
 file = Substitute( TRIM(STR(file)) , (/"s%s"/) , (/iter/) )
 file = TRIM(STR(file))//".out"
 Unit_ = NewFile( file )
ELSE
 Unit_ = Unit_out
END IF

!loop through all the outputs and call their respective print routines
DO n = 1 , SIZE(LIST_Outputs)

 iout = LIST_Outputs(n)

 !comment
 CALL UpdateAndDump(fdbk_comment,fdbk,s="[[TAP]] Printing output: "//&
   TRIM(KEY_Outputs(iout))//".")

 !header
 WRITE(Unit_,"(a)")"%Out"//TRIM(ADJUSTL(STR(n,"(i6.3)")))//"%"
 WRITE(Unit_,"(a)")"###"//TRIM(KEY_Outputs(iout))
 CALL PRINT_Text(s=TRIM(DESCRIPTION_Outputs(iout)),Unit=Unit_)

 !body
 SELECT CASE( LIST_Outputs(n) )

  !*** high order variable output ***
  CASE(01); CALL PRINT_AngularFluxV_gen( &
                 Mesh , Ordinates,Weights,AngularFluxV , &
                 FdBk , Unit=Unit_ )

  CASE(02); CALL PRINT_AngularFluxF_gen( &
                 Mesh , Ordinates,Weights,AngularFluxF , &
                 FdBk , Unit=Unit_ )

  CASE(03); CALL PRINT_AngularFluxC_gen( &
                 Mesh , Ordinates,Weights,AngularFluxC , &
                 FdBk , Unit=Unit_ )

  CASE(04); CALL PRINT_ScalarFluxV( &
                 Mesh , ScalarFluxV  , &
                 fdbk , Unit=Unit_ )

  CASE(05); CALL PRINT_ScalarFluxF( &
                 Mesh , ScalarFluxF  , &
                 fdbk , Unit=Unit_ )

  CASE(06); CALL PRINT_ScalarFluxC( &
                 Mesh , ScalarFluxC  , &
                 fdbk , Unit=Unit_ )

  CASE(07); CALL PRINT_CurrentV( &
                 Mesh , CurrentV , &
                 fdbk , Unit=Unit_ )

  CASE(08); CALL PRINT_CurrentF( &
                 Mesh , CurrentF  , &
                 fdbk , Unit=Unit_ )

  CASE(09); CALL PRINT_CurrentFN( &
                 Mesh , CurrentFN  , &
                 fdbk , Unit=Unit_ )

  CASE(10); CALL PRINT_CurrentC( &
                 Mesh , CurrentC  , &
                 fdbk , Unit=Unit_ )

  CASE(11); CALL PRINT_ScalarFlux_CellRegions( &
                 Mesh,label,IntegralRegions,&
                 ScalarFluxC,&
                 fdbk,Unit=Unit_,varname="HI-Phi")

  CASE(12); CALL PRINT_Current_Exiting(&
                 Mesh,label,exit_r1,exit_r2,CurrentFN,&
                 fdbk,Unit=Unit_,varname="HI-JFN")


  !*** low order variable output ***
  CASE(13); CALL PRINT_EddingtonsV( &
                 LO_Mesh , KxxV , KyyV , KxyV , &
                 EddingtonxxV , EddingtonyyV , EddingtonxyV , &
                 fdbk , Unit = Unit_ )

  CASE(14); CALL PRINT_EddingtonsF( &
                 LO_Mesh , KxxF , KyyF , KxyF , &
                 EddingtonxxF , EddingtonyyF , EddingtonxyF , &
                 fdbk , Unit = Unit_ )

  CASE(15); CALL PRINT_EddingtonsC( &
                 LO_Mesh , KxxC , KyyC , KxyC , &
                 EddingtonxxC , EddingtonyyC , EddingtonxyC , &
                 fdbk , Unit = Unit_ )

  CASE(16); CALL PRINT_ScalarFluxV( &
                 LO_Mesh , LO_ScalarFluxV , &
                 fdbk , Unit=Unit_ , varname="LO-PhiV")

  CASE(17); CALL PRINT_ScalarFluxF( &
                 LO_Mesh , LO_ScalarFluxF , &
                 fdbk , Unit=Unit_ , varname="LO-PhiF")

  CASE(18); CALL PRINT_ScalarFluxC( &
                 LO_Mesh , LO_ScalarFluxC , &
                 fdbk , Unit=Unit_ , varname="LO-PhiC")

  CASE(19); CALL PRINT_CurrentFN( &
                 LO_Mesh , LO_CurrentFN , &
                 fdbk , Unit=Unit_ , varname="LO-JFN")

  CASE(20); CALL PRINT_CurrentC( &
                 LO_Mesh , LO_CurrentC , &
                 fdbk , Unit=Unit_ , varname="LO-JC")

  CASE(21); CALL PRINT_CBoundary(  &
                 LO_Mesh , CBoundary , jbdry , &
                 ScalarFluxIN , CurrentIN , &
                 Fdbk , Unit=Unit_   )

  CASE(22); CALL PRINT_ScalarFlux_CellRegions( &
                 LO_Mesh,label,IntegralRegions,&
                 LO_ScalarFluxC,&
                 fdbk,Unit=Unit_,varname="LO-Phi")

  CASE(23); CALL PRINT_Current_Exiting(&
                 LO_Mesh,label,exit_r1,exit_r2,LO_CurrentFN,&
                 fdbk,Unit=Unit_,varname="LO-JFN")

  !!*** low-order linear systems solver output ***
  CASE(24); !CALL Stop(s="PRINT_LO_SystemInfo is disabled");
            CALL PRINT_LO_SystemInfo( A , System , Unit=Unit_)
  CASE(25); !CALL Stop(s="PRINT_IterTable is disabled");
            CALL PRINT_IterTable( IterTable , Unit=Unit_ )

  !!*** characteristics method output ***
  CASE(26); CALL PRINT_CharacteristicInfo( fdbk , Unit=Unit_ )
  CASE(27); CALL PRINT_InterpInfo( fdbk , Unit=Unit_ )

  !!*** tapack timing outputs ****
  CASE(28); CALL PRINT_TimeSummary(dt,Fdbk,Unit=Unit_)
  CASE(29); CALL PRINT_TimeTable(TAP_what,TAP_time,Fdbk,Unit=Unit_)

  CASE(30) ; CALL PRINT_KEYS( KEY_Outputs,fdbk , Unit=Unit_ )

  CASE DEFAULT ; CALL STOP( s="The solution printing key was not found! ["//mod_//"::"//proc_//"]" )

 END SELECT

 !footer
 WRITE(Unit_,"(//)")

END DO

!if we opened a new unit, close it
IF( PRESENT(iter) )THEN
 CLOSE( Unit_ )
END IF

!end time
CALL CPU_TIME(tout)
dt0 = (tout-tin)

!print status
CALL UpdateAndDump(fdbk_profile,fdbk,s="[[TAP]] solution printed in &
  &[time="//STRTIME(dt0)//"].")

!!--end--
END SUBROUTINE



!!### SUBROUTINE >>PRINT_SolutionAux<<
SUBROUTINE PRINT_SolutionAux(fdbk,iter)
!!#### PURPOSE
!! Controls printing of the auxilliary solution
!! files.

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_="PRINT_SolutionAux"

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_FdBk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### OPTIONAL INPUT
INTEGER,OPTIONAL,INTENT(IN) :: iter

!!#### LOCAL VARIABLES
INTEGER :: n,iout,Unit_,m
REAL    :: tin,tout,dt0
TYPE(varying_string) :: file
REAL(KIND_MSH) :: FC(2)
INTEGER :: j,jd

!!--begin--

!! * output a gmv file
IF( OutputGMV )THEN

 file = TRIM(OutputFileBase)//".gmv"
 IF( PRESENT(iter) )THEN
  IF( PrintEachIteration )THEN
   file = TRIM(file)//"."//TRIM(STR(iter,"(i4.4)"))
  END IF
 END IF

 Unit_ = NewFile( file )

 CALL gmvInitialize(Unit_,Mesh,l_,LABEL_Materials)

 !output external source as a tracer
! CALL gmvBegin_Tracers(Unit_,Mesh)
! CALL UPDATE_QextA(MCS_KEY_CellFunction(CellFunctionMethodA),&
!   Mesh,ExtSourceCellFunction,&
!   Omega=Ordinates(1:2,84),mIndices=(/84,SIZE(pThread)/))
! CALL gmvSet_TracerCellFunctionData(Unit_,Mesh,"QExtA(84)",&
!   ExtSourceCellFunction(:,1,:))
! CALL gmvEnd_Tracers(Unit_,Mesh)
 !-e-n-d-of-tracers

 CALL gmvBegin_Variables(Unit_,Mesh)

 IF( ASSOCIATED(ScalarFluxV) )THEN
  CALL gmvSet_NodeData(Unit_,Mesh,"HI_PhiV",ScalarFluxV(1,:))
 END IF
 IF( ASSOCIATED(ScalarFluxC) )THEN
  CALL gmvSet_CellData(Unit_,Mesh,"HI_PhiC",ScalarFluxC(1,:))
 END IF
 IF( ASSOCIATED(LO_ScalarFluxC) )THEN
  CALL gmvSet_CellData(Unit_,Mesh,"LO_PhiC",LO_ScalarFluxC(1,:))
 END IF
 IF( ASSOCIATED(EddingtonxxV) )THEN
  CALL gmvSet_NodeData(Unit_,Mesh,"ExxV",EddingtonxxV(1,:))
 END IF
 IF( ASSOCIATED(EddingtonxyV) )THEN
  CALL gmvSet_NodeData(Unit_,Mesh,"ExyV",EddingtonxyV(1,:))
 END IF
 IF( ASSOCIATED(EddingtonyyV) )THEN
  CALL gmvSet_NodeData(Unit_,Mesh,"EyyV",EddingtonyyV(1,:))
 END IF
 IF( ASSOCIATED(EddingtonxxC) )THEN
  CALL gmvSet_CellData(Unit_,Mesh,"ExxC",EddingtonxxC(1,:))
 END IF
 IF( ASSOCIATED(EddingtonxyC) )THEN
  CALL gmvSet_CellData(Unit_,Mesh,"ExyC",EddingtonxyC(1,:))
 END IF
 IF( ASSOCIATED(EddingtonyyC) )THEN
  CALL gmvSet_CellData(Unit_,Mesh,"EyyC",EddingtonyyC(1,:))
 END IF
 IF( ASSOCIATED(KxxV) )THEN
  CALL gmvSet_NodeData(Unit_,Mesh,"KxxV",KxxV(1,:))
 END IF
 IF( ASSOCIATED(KxyV) )THEN
  CALL gmvSet_NodeData(Unit_,Mesh,"KxyV",KxyV(1,:))
 END IF
 IF( ASSOCIATED(KyyV) )THEN
  CALL gmvSet_NodeData(Unit_,Mesh,"KyyV",KyyV(1,:))
 END IF
 IF( ASSOCIATED(KxxC) )THEN
  CALL gmvSet_CellData(Unit_,Mesh,"KxxC",KxxC(1,:))
 END IF
 IF( ASSOCIATED(KxyC) )THEN
  CALL gmvSet_CellData(Unit_,Mesh,"KxyC",KxyC(1,:))
 END IF
 IF( ASSOCIATED(KyyC) )THEN
  CALL gmvSet_CellData(Unit_,Mesh,"KyyC",KyyC(1,:))
 END IF

 IF( ASSOCIATED(AngularFluxC) )THEN
  !don't output angular flux
  !DO m=1,SIZE(AngularFluxC,3)
  ! CALL gmvSet_CellData(Unit_,Mesh,"PsiC"//TRIM(STR(m)),AngularFluxC(1,:,m))
  !END DO
 END IF
 IF( ASSOCIATED(AngularFluxV) )THEN
  !don't output angular flux
  DO m=1,SIZE(AngularFluxV,3)
   CALL gmvSet_NodeData(Unit_,Mesh,"PsiV"//TRIM(STR(m)),AngularFluxV(1,:,m))
  END DO
 END IF

 IF( ASSOCIATED(CurrentV) )THEN
  CALL gmvSet_NodeData(Unit_,Mesh,"CurrentxV",CurrentV(1,1,:))
  CALL gmvSet_NodeData(Unit_,Mesh,"CurrentyV",CurrentV(2,1,:))
 END IF
 IF( ASSOCIATED(CurrentC) )THEN
  CALL gmvSet_CellData(Unit_,Mesh,"CurrentxC",CurrentC(1,1,:))
  CALL gmvSet_CellData(Unit_,Mesh,"CurrentyC",CurrentC(2,1,:))
 END IF

 IF( ASSOCIATED(ExtSourceCellFunction) )THEN
  CALL gmvSet_CellData(Unit_,Mesh,"ExtSource1",ExtSourceCellFunction(1,1,:))
  IF( SIZE(ExtSourceCellFunction,1)==3 )THEN
    CALL gmvSet_CellData(Unit_,Mesh,"ExtSourcex",ExtSourceCellFunction(2,1,:))
    CALL gmvSet_CellData(Unit_,Mesh,"ExtSourcey",ExtSourceCellFunction(3,1,:))
  END IF
 END IF

 IF( ASSOCIATED(TotSourceCellFunction) )THEN
  CALL gmvSet_CellData(Unit_,Mesh,"TotSource1",TotSourceCellFunction(1,1,:))
  IF( SIZE(TotSourceCellFunction,1)==3 )THEN
    CALL gmvSet_CellData(Unit_,Mesh,"TotSourcex",TotSourceCellFunction(2,1,:))
    CALL gmvSet_CellData(Unit_,Mesh,"TotSourcey",TotSourceCellFunction(3,1,:))
  END IF
 END IF

 IF( Using_AnalyticTransportTest )THEN
  CALL gmv_AnalyticTestOutput(Mesh,LO_Mesh,&
  ScalarFluxV,ScalarFluxF,ScalarFluxC,&
  LO_ScalarFluxV,LO_ScalarFluxF,LO_ScalarFluxC,&
  EddingtonxxV,EddingtonyyV,EddingtonxyV,&
  EddingtonxxF,EddingtonyyF,EddingtonxyF,&
  EddingtonxxC,EddingtonyyC,EddingtonxyC,&
  KxxV,KyyV,KxyV,&
  KxxF,KyyF,KxyF,&
  KxxC,KyyC,KxyC,&
  CurrentFN,&
  LO_CurrentFN,&
  AngularFluxV,AngularFluxF,AngularFluxC,&
  Ordinates,OutputFileBase,Unit_)
!   CALL UPDATE_QextA(MCS_KEY_CellFunction(CellFunctionMethodA),&
!     Mesh,ExtSourceCellFunction,&
!     Omega=Ordinates(1:2,84),mIndices=(/84,SIZE(pThread)/))
!   CALL gmvSet_CellFunctionData(Unit_,Mesh,"QExtA(84)",&
!     ExtSourceCellFunction(:,1,:))
!   CALL OutputVertAFValues(Mesh,"EXACT_QExtA",EXACT_Qext,&
!     Ordinates,Unit_,mtarget=84)

!this is the low-order test but it does have a name that 
!implies it could be any test
ELSEIF( AnalyticTestCase/=0 )THEN

  CALL gmv_LOQDAnalyticTestOutput(Mesh,LO_Mesh,&
  ScalarFluxF,ScalarFluxC,&
  LO_ScalarFluxF,LO_ScalarFluxC,&
  EddingtonxxV,EddingtonyyV,EddingtonxyV,&
  EddingtonxxF,EddingtonyyF,EddingtonxyF,&
  EddingtonxxC,EddingtonyyC,EddingtonxyC,&
  CurrentFN,&
  LO_CurrentFN,&
  OutputFileBase,Unit_)
 
 END IF
 CALL gmvEnd_Variables(Unit_,Mesh)
 CALL gmvFinalize(Unit_,Mesh)
 CLOSE(Unit_)
END IF

!! * save a solution file
IF( SaveSolution )THEN

 file = TRIM(OutputFileBase) 
 CALL SAVEn(TRIM(STR(file))//"-LO_ScalarFluxF",LO_ScalarFluxF)
 CALL SAVEn(TRIM(STR(file))//"-LO_ScalarFluxC",LO_ScalarFluxC)
 CALL SAVEn(TRIM(STR(file))//"-LO_CurrentFN",LO_CurrentFN)
 CALL SAVEn(TRIM(STR(file))//"-ScalarFluxV",ScalarFluxV)
 CALL SAVEn(TRIM(STR(file))//"-ScalarFluxF",ScalarFluxF)
 CALL SAVEn(TRIM(STR(file))//"-ScalarFluxC",ScalarFluxC)
 CALL SAVEn(TRIM(STR(file))//"-AngularFluxV",AngularFluxV)
 CALL SAVEn(TRIM(STR(file))//"-AngularFluxF",AngularFluxF)
 CALL SAVEn(TRIM(STR(file))//"-AngularFluxC",AngularFluxC)
 CALL SAVEn(TRIM(STR(file))//"-TotSourceCellFunction",TotSourceCellFunction)
 CALL SAVEn(TRIM(STR(file))//"-ScalarFluxCellFunction",ScalarFluxCellFunction)
 CALL SAVEn(TRIM(STR(file))//"-Ordinates",Ordinates)
 CALL SAVEn(TRIM(STR(file))//"-Weights",Weights)
 CALL SAVE_Mesh(TRIM(STR(file))//"-Mesh.dat",Mesh)

END IF

!print boundary scalar fluxes
IF( ASSOCIATED(LO_Mesh) )THEN
 DO jd=1,4
  file = TRIM(OutputFileBase)//".lophif.jd"//TRIM(STR(jd)) 
  Unit_=NewFile(TRIM(STR(file)))
  DO j=1,NUM_Faces(LO_Mesh)
   IF( IsBoundaryFace(LO_Mesh,j,jd) )THEN
    FC = FaceCentroid(LO_Mesh,j)
    WRITE(Unit_,"(i5,3Es20.6,Es26.13)")j,FC(1),FC(2),FaceArea(LO_Mesh,j),LO_ScalarFluxF(1,j)
   END IF
  END DO
  CLOSE(Unit_)
 END DO
 DO jd=1,4
  file = TRIM(OutputFileBase)//".lojfn.jd"//TRIM(STR(jd)) 
  Unit_=NewFile(TRIM(STR(file)))
  DO j=1,NUM_Faces(LO_Mesh)
   IF( IsBoundaryFace(LO_Mesh,j,jd) )THEN
    FC = FaceCentroid(LO_Mesh,j)
    WRITE(Unit_,"(i5,3Es20.6,Es26.13)")j,FC(1),FC(2),FaceArea(LO_Mesh,j),LO_CurrentFN(1,j)
   END IF
  END DO
  CLOSE(Unit_)
 END DO
END IF
IF( ASSOCIATED(ScalarFluxF) )THEN
 DO jd=1,4
  file = TRIM(OutputFileBase)//".phif.jd"//TRIM(STR(jd)) 
  Unit_=NewFile(TRIM(STR(file)))
  DO j=1,NUM_Faces(Mesh)
   IF( IsBoundaryFace(Mesh,j,jd) )THEN
    FC = FaceCentroid(Mesh,j)
    WRITE(Unit_,"(i5,3Es20.6,Es26.13)")j,FC(1),FC(2),FaceArea(Mesh,j),ScalarFluxF(1,j)
   END IF
  END DO
  CLOSE(Unit_)
 END DO
END IF
IF( ASSOCIATED(CurrentFN) )THEN
 DO jd=1,4
  file = TRIM(OutputFileBase)//".jfn.jd"//TRIM(STR(jd)) 
  Unit_=NewFile(TRIM(STR(file)))
  DO j=1,NUM_Faces(Mesh)
   IF( IsBoundaryFace(Mesh,j,jd) )THEN
    FC = FaceCentroid(Mesh,j)
    WRITE(Unit_,"(i5,3Es20.6,Es26.13)")j,FC(1),FC(2),FaceArea(Mesh,j),CurrentFN(1,j)
   END IF
  END DO
  CLOSE(Unit_)
 END DO
END IF
!!--end--
END SUBROUTINE


!!### SUBROUTINE >>PRINT_InputEcho<<
SUBROUTINE PRINT_InputEcho(fdbk)

!!#### PURPOSE
!! Echo out the input to the main output file <tap.out>
!! for checking, called called from <<PRINT_TAPACK>>.

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_FdBk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_="PRINT_InputEcho"

!!#### LOCAL VARIABLES
INTEGER :: n,iout
REAL    :: tin,tout,dt
TYPE(varying_string) :: InputFile

!!--begin--

!start time
CALL CPU_TIME(tin)

!loop through all the inputs asked to be printed and call the correct
!printing routine
DO n = 1 , SIZE(LIST_InputEdits)

 iout = LIST_InputEdits(n)

 !comment
 CALL UpdateAndDump(fdbk_comment,fdbk,s="[[TAP]] Printing input echo "//&
   "[key="//TRIM(KEY_InputEdits(iout))//"]. ")

 !header
 WRITE(Unit_out,"(a)")"%Inp"//TRIM(ADJUSTL(STR(n,"(i6.3)")))//"%"
 WRITE(Unit_out,"(a)")"###"//TRIM(KEY_InputEdits(iout))
 CALL PRINT_Text(s=TRIM(DESCRIPTION_InputEdits(iout)),Unit=Unit_out)

 !body
 SELECT CASE( LIST_InputEdits(n) )
  CASE(01) ; CALL PRINT_QuadratureSet( &
                  Ordinates , Weights , Unit=Unit_out )

  CASE(02) ; CALL PRINT_genMomentTest( &
                  Ordinates , Weights , Unit=Unit_out )

  CASE(03) ; CALL PRINT_NuclearData( Mesh , MacS , MacT , l_ , &
                  ExtSourceCellFunction , fdbk , Unit=Unit_out )

  CASE(04) ; CALL PRINT_BC( Mesh , BC , KEY_BC , fdbk , Unit=Unit_out )

  !!*** mesh output ***
  CASE(05) ; CALL PRINT_Mesh( Mesh , Unit=Unit_out )
  CASE(06) ; CALL PRINT_Mesh( LO_Mesh , Unit=Unit_out )

  !!*** characteristics print outs ***
  CASE(07) ; CALL PRINT_StreamingInterpolants( fdbk , Unit=Unit_out )
  CASE(08) ; CALL PRINT_InteriorCells        ( fdbk , Unit=Unit_out )
  CASE(09) ; CALL PRINT_IncomingDirections   ( fdbk , Unit=Unit_out )
  CASE(10) ; CALL PRINT_SweepOrder           ( fdbk , Unit=Unit_out )

  CASE(11) ; CALL PRINT_KEYS( KEY_InputEdits,fdbk , Unit=Unit_out )

  CASE(12) ; InputFile = TRIM(MasterInputFile)
             CALL PRINT_InputFile( STR(InputFile) , fdbk , Unit=Unit_out )
 END SELECT

 !footer
 WRITE(Unit_out,"(//)")

END DO

!end time
CALL CPU_TIME(tout)
dt = (tout-tin)

!print status
CALL UpdateAndDump(fdbk_profile,fdbk,s="[[TAP]] input echo printed in &
  &[time="//STRTIME(dt)//"].")

!print options
CALL UpdateAndDump(fdbk_comment,fdbk,s="[[TAP]] Entering transport calculation with the following options: ")
CALL PRINT_Options(Unit=OutputUnit(fdbk))

!!--end--
END SUBROUTINE



!!### SUBROUTINE >>INPUT_TAPACK<<
SUBROUTINE INPUT_TAPACK(fdbk)

!!#### PURPOSE
!! This is the main routine that reads input from a file
!! defined by the single argument to TAPACK.

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_FdBk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### LOCAL VARIABLES
!! * input/output object <sio>
TYPE(TYPE_sio),POINTER :: sio
REAL                   :: tin,tout,dt
INTEGER                :: Unit_log
TYPE(varying_string)   :: file,dir,base,logfile

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_ = "INPUT_TAPACK"

!!--begin--
!
!! * initialize feedback and streaming input output
CALL ALLOCATE( fdbk )
sio => NULL()
!
!! * timing
CALL CPU_TIME(tin)
!
!! * Initialize the <sio> structure and form the the control file path
!! * Process the arguments of the command-line call
CALL GETARG( 1 , MasterInputFile )
file = MasterInputFile
CALL SplitFileName(file,dir,base)
OutputFileBase = base
CALL UpdateAndDump( fdbk_comment , fdbk , s="[[TAP]] using file=["//TRIM(STR(file))//"] &
  &assuming base=["//TRIM(STR(base))//"]")

IF( TRIM(dir) .NE. '.' )THEN
    CALL UpdateAndDump( fdbk_error , fdbk , s="[[TAP]] As of TAPACK v2.23.0, &
  &the exe must be run in the dir=["//TRIM(STR(dir))//"] where the input resides.")
END IF

!! * change output unit to the log file
logfile     = TRIM(OutputFileBase)//".log" 
CALL UpdateAndDump(fdbk_comment,fdbk,s="[[TAP]] Logging info is now being &
  &redirected to [file="//TRIM(STR(logfile))//"].")
Unit_log = NewFile(TRIM(STR(logfile)),STATUS="Replace",IfOpened="C")
CALL Modify(fdbk,OutputUnit=Unit_log)

CALL CONNECT(SIO,ACTION  = "Read"      ,&
                 FORM    = "Formatted" ,&
                 FILE    = TRIM(STR(file))  ,&
                 ACCESS  = "Sequential")
!! * Set defaults for interpretting input (acts like LaTeX).
SIO%Comment      = "!"
SIO%Continue     = "&"
SIO%Stop         = "#"
SIO%Ignore       = " "
SIO%CmdBeg       = "\"
SIO%ArgBeg       = "{"
SIO%ArgEnd       = "}"
SIO%ArgDelim     = "}{"
SIO%SubArgDelim  = ","
SIO%ArgDefine    = "="
SIO%Argdefault   = "*"
SIO%StrTag       = '"'
SIO%LEN_ArgDelim = 2
TimeTreatment=SteadyState_
EnergyDiscretization=EnergyGroups_
AngularDiscretization=DiscreteOrdinates_
SpatialDiscretization=Mesh_
TransportModule=MoCShort_
AccelerationModule=Quasidiffusion_

CALL UpdateAndDump( fdbk_comment , fdbk , s="[[TAP]] Initializing TAPACK version=["//TRIM(VERSION)//"]")

CALL UpdateAndDump(fdbk_comment,fdbk,s="[[TAP]] Setting control file path\file="//TRIM(STR(file)))

!! * Enter the input loop.
DO
 !
 !! * Read a command and arguments into .
 CALL READ_COMMAND(SIO,fdbk)
 !
 !! * Kick out when [SIO] destroys itself (or reaches end of a file) in case the
 !!    SIO is structure is kept alive.
 IF( .NOT.ASSOCIATED(SIO) )EXIT
 !
 !! * Print a message saying the command
 !!    card being read, line number, etc.
 CALL UpdateAndDump(fdbk_comment,fdbk,s="[[TAP]] Reading &
                & [card="//TRIM(STR(SIO%cmd))//"]"//&
             " on [line="//TRIM(STR(SIO%line_num))//"]"//&
             " of [file="//TRIM(STR(SIO%file))//"]..." )
 !
 !! * Enter in to the database of command cards.
 CALL SWITCHBOARD_TAPACK(SIO,fdbk)

END DO

!! * Calculate the runtime for input reading.
CALL CPU_TIME(tout)
dt = (tout-tin)
!
!! * Send a message notifying successful termination.
CALL UpdateAndDump(fdbk_profile,fdbk,s="[[TAP]] Input completed in &
  &[time="//STRTIME(dt)//"].")
!

!!--end--
CONTAINS

SUBROUTINE SplitFileName(File,Dir,Base)
USE ISO_varying_string
TYPE(varying_string) :: File
TYPE(varying_string) :: Dir,File0,Base
INTEGER :: i
Dir='.'
File0=File
DO i=LEN(File),1,-1
    IF( extract(File,i,i)==fsSeparator )THEN
        Dir=extract(File,1,i-1)
        File0=extract(File,i,LEN(File))
        EXIT
    END IF
END DO

Base=''
DO i=LEN(File0),1,-1
    IF( extract(File0,i,i)=='.' )THEN
        Base=extract(File0,1,i-1)
        exit
    END IF
END DO

IF( LEN(Base)==0 )THEN
    Base=File0
END IF

END SUBROUTINE SplitFileName

END SUBROUTINE INPUT_TAPACK


!!### SUBROUTINE >>WRAPUP_TAPACK<<
SUBROUTINE WRAPUP_TAPACK( fdbk )

!!#### PURPOSE
!! Wrap up the transport problem by deallocating
!! all variables from each package.

!!#### REQUIRED INPUT/OUTPUT
!! * feedback variable
TYPE(TYPE_fdbk),INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
REAL :: tin,tout,dt

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_ = "WRAPUP_TAPACK"

!!--begin--
CALL CPU_TIME(tin)

!1. discretizations wrapup

!1.a. spatial discretization (mesh) wrapup
SELECT CASE( SpatialDiscretization )
 CASE( Mesh_ ) ;              CALL WRAPUP_Mesh( Mesh , fdbk )
END SELECT

!1.b. energy discretization wrapup
SELECT CASE( EnergyDiscretization )
 !CASE( EneRgyGroups_ ) ;      CALL WRAPUP_EneRgyGroups( fdbk )
END SELECT

!1.c. angular discretization wrapup
SELECT CASE( AngularDiscretization )
 !CASE( DiscreteOrdinates_ ) ; CALL WRAPUP_DiscreteOrdinates( fdbk )
END SELECT

!1.d. time discretization wrapup
SELECT CASE( TimeTreatment )
 !CASE( SteadySTate_ ) ;       CALL WRAPUP_SteadySTate( fdbk )
END SELECT


!2. data wrapup

!2.a. source wrapup
SELECT CASE( SourceModule )
 !CASE( Source_ ) ;            CALL WRAPUP_Source( fdbk )
END SELECT

!2.b. cross section wrapup
SELECT CASE( XSModule )
 !CASE( XSMonkey_ ) ;          CALL WRAPUP_XSMonkey( fdbk )
END SELECT


!3. solver wrapup

!3.a. nuclide transmutation wrapup
SELECT CASE( TransmutationModule )
 !CASE( NuCSpider_ ) ;         CALL WRAPUP_NuCSpider( fdbk )
END SELECT

!3.b. transport method wrapup
SELECT CASE( TransportModule )
 CASE( MoCshort_ ) ;          CALL WRAPUP_MoCshort( fdbk )
END SELECT

!3.c. acceleration method wrapup
SELECT CASE( AccelerationModule )
 CASE( QuasiDiffusion_ ) ;    CALL WRAPUP_QuasiDiffusion( fdbk )
END SELECT

!10. transport algorithms package wrapup
CALL WRAPUP_( fdbk )

!end time
CALL CPU_TIME(tout)
dt = (tout-tin)

!update
CALL UpdateAndDump(fdbk_profile,fdbk,s="[[TAP]] wrapup completed in "//TRIM(STR(dt*1.E3,"(e8.2)"))//" milliseconds.")

WRITE(OutputUnit(fdbk),"(a)")"<<< [[TAP]] successfully shutdown."

!!--end--
END SUBROUTINE



!!### SUBROUTINE >>WRAPUP_<<
SUBROUTINE WRAPUP_( fdbk )

!!#### PURPOSE
!! The final wrapup for TAPACK, called from
!! <<WRAPUP_TAPACK>>.

!!#### REQUIRED INPUT/OUTPUT
!! * feedback variable
TYPE(TYPE_fdbk),INTENT(INOUT) :: fdbk

!!--begin--
!close the output unit
CLOSE( Unit_out )
Unit_out = 0

!close the iteration output units
CLOSE( Unit_ithi )
CLOSE( Unit_itlo )
Unit_ithi = 0
Unit_itlo = 0

CALL UpdateAndDump(fdbk_comment,fdbk,s="[[TAP]] Finished wrapup ...")

!!--end--
END SUBROUTINE


!!### SUBROUTINE >>SETUP_<<
SUBROUTINE SETUP_( fdbk )

!!#### PURPOSE
!! The initial setup for TAPACK, called from <<SETUP_TAPACK>>
!! which writes header and table of contents information to the
!! main output file  <tap.out>.

!!#### REQUIRED INPUT/OUTPUT
!! * feedback variable
TYPE(TYPE_fdbk) :: fdbk

!!#### LOCAL VARIABLES
TYPE(varying_string) :: TS,path,VS
INTEGER              :: N1,N2,n,m,i

!!--begin--
!0. get time stamp
TS = TimeStamp()


!1. print message
CALL UpdateAndDump(fdbk_comment,fdbk,s="[[TAP]] is setting up primary output file, &
  &stamped with [date="//TRIM(STR(TS))//"]")

!2.a close the old output file and open a new one
IF( Unit_out/=0 )THEN
 CLOSE(Unit_out)
END IF

!2.b get a unit not already being used
Unit_out = NewUnit()

!2.c open the file (using the correct directory character <Dir>)
IF( LEN_TRIM(OutputFileBase)==0 )THEN
 path = TRIM(STR(TS))//".out"
 OPEN( Unit=Unit_out , FILE=TRIM(STR(path)) )
ELSE
 path = TRIM(OutputFileBase)//".out"
 OPEN( UNIT=Unit_out , FILE=TRIM(STR(path)) )
END IF



!3.a write the file label
IF( LEN_TRIM(label)/=0 )THEN
 WRITE(Unit_out,"(a,//)")"#"//TRIM( label )
ELSE
 CALL UpdateAndDump(fdbk_comment,fdbk,s="use \TAPlabel{""your text""} in the control file &
 &to give this file a label.")
END IF

!3.b write the details
IF( LEN_TRIM(details)/=0 )THEN
 WRITE(Unit_out,"(a)")"##DETAILS"
 CALL Print_Text( s=SMUSH(STR(details)) , &
   Unit=Unit_out , Columns=70 , LineIndents=(/0,0/) )
 WRITE(Unit_out,"(a)")" "
END IF

!3.c write the author
IF( LEN_TRIM(Author)/=0 )THEN
 WRITE(Unit_out,"(a)")"##AUTHOR"
 WRITE(Unit_out,"(2x,a,//)")TRIM(STR(Author))
END IF

!3.d write the timestamp
WRITE(Unit_out,"(a)")"##DATE MODIFIED"
WRITE(Unit_out,"(2x,a,//)")TRIM(STR(TS))



!4. write output listing
N1 = SIZE(LIST_InputEdits)
N2 = SIZE(LIST_Outputs)
IF( N1+N2>0 )THEN

 !4.a.
 WRITE(Unit_out,"(a)"   )"##OUTPUT LISTING"

 !4.b list the input edits
 m = 0
 DO n = 1 , N1
  m = m + 1
  i = LIST_InputEdits(n)
  IF( i==0 )CYCLE
  VS = "%Inp"//TRIM(STR(n,"(i6.3)"))//"% ........ ###"//&
    TRIM(KEY_InputEdits(i))//"    ("//TRIM(DESCRIPTION_InputEdits(i))//")"
  WRITE(Unit_out,"(1x,a)")STR(VS)
 END DO

 !4.c list the outputs
 DO n = 1 , N2
  m = m + 1
  i = LIST_Outputs(n)
  IF( i==0 )CYCLE
  VS = "%Out"//TRIM(STR(n,"(i6.3)"))//"% ........ ###"//&
    TRIM(KEY_Outputs(i))//"    ("//TRIM(DESCRIPTION_Outputs(i))//")"
  WRITE(Unit_out,"(1x,a)")STR(VS)
 END DO
 WRITE(Unit_out,"(//)")

END IF
VS = ""

!5. open the runtime iteration output files
path = TRIM(OutputFileBase)//".ithi" 
Unit_ithi = NewFile( TRIM(STR(path))  , STATUS="Replace" , IfOpened="R" )

path = TRIM(OutputFileBase)//".itlo" 
Unit_itlo = NewFile( TRIM(STR(path))  , STATUS="Replace" , IfOpened="R" )

!!--end--
END SUBROUTINE



!!### SUBROUTINE <<SETUP_TAPACK>>
SUBROUTINE SETUP_TAPACK( fdbk )

!!#### PURPOSE
!! Set up the transport problem.

!!#### REQUIRED INPUT/OUTPUT
!! * feedback variable
TYPE(TYPE_fdbk),INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
REAL    :: tin,tout,dt

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_ = "SETUP_TAPACK"

!!--begin--
CALL CPU_TIME(tin)

!0. transport algorithms package setup
CALL SETUP_( fdbk )

!1. discretizations

!1.a. spatial discretization (mesh) setup
SELECT CASE( SpatialDiscretization )
 CASE( Mesh_ ) ;              CALL SETUP_Mesh( Mesh , fdbk )
END SELECT
!1.b. return number of dimensions
NDim = NUM_Dimensions_Mesh(Mesh)

!1.c. energy discretization setup
SELECT CASE( EnergyDiscretization )
 !CASE( EneRgyGroups_ ) ;      CALL SETUP_EneRgyGroups( fdbk )
END SELECT

!1.d. angular discretization setup
SELECT CASE( AngularDiscretization )
 CASE( DiscreteOrdinates_ ) ; CALL SETUP_DiscreteOrdinates( &
                                   NDim , UniformZGeometry , &
                                   Order,aOrder,pOrder,&
                                   aQuadrature,pQuadrature,Quadrature,&
                                   QuadratureType,Ordinates , Weights , fdbk )
END SELECT

!1.e. time discretization setup
SELECT CASE( TimeTreatment )
 !CASE( SteadySTate_ ) ;       CALL SETUP_SteadySTate( fdbk )
END SELECT


!2. data setup

!2.a. source setup
SELECT CASE( SourceModule )
 !CASE( Source_ ) ;            CALL SETUP_Source( fdbk )
END SELECT

!2.b. cross section setup
SELECT CASE( XSModule )
 !CASE( XSMonkey_ ) ;          CALL SETUP_XSMonkey( fdbk )
END SELECT

!3. solver setup

!3.a. nuclide transmutation setup
SELECT CASE( TransmutationModule )
 !CASE( NuCSpider_ ) ;         CALL SETUP_NuCSpider( fdbk )
END SELECT

!3.b. transport method setup
SELECT CASE( TransportModule )
 CASE( MoCshort_ ) ;          CALL SETUP_MoCshort( fdbk )
END SELECT

!3.c. acceleration method setup
SELECT CASE( AccelerationModule )
 CASE( QuasiDiffusion_ ) ;    CALL SETUP_QuasiDiffusion( fdbk )
END SELECT


!4. Setup the integral regions.
CALL SETUP_IntegralRegions( Mesh , 2 , IntegralRegions )

!end time
CALL CPU_TIME(tout)
dt = (tout-tin)

!update
CALL UpdateAndDump(fdbk_profile,fdbk,s="[[TAP]] setup completed in &
  &[time="//STRTIME(dt)//"].")

!!--end--
END SUBROUTINE



!!### SUBROUTINE >>SOLVE_TAPACK_HighOrder<<
SUBROUTINE SOLVE_TAPACK_HighOrder( dt0 , iter , fdbk )

!!#### PURPOSE
!! Solve the high-order (angularly-dependent) transport problem,
!! called from <<SOLVE_TAPACK>>.

!!#### REQUIRED INPUT/OUTPUT
!! * total elapsed time
!! * iteration count
REAL   ,INTENT(INOUT) :: dt0
INTEGER,INTENT(INOUT) :: iter

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!--begin--


!select the transport module
SELECT CASE( TransportModule )
 CASE( MOCshort_ ) ; CALL SOLVE_MOCshort( iter , dt0 , fdbk )
END SELECT

!!--end--
END SUBROUTINE


!!### SUBROUTINE >>SOLVE_TAPACK_LowOrder<<
SUBROUTINE SOLVE_TAPACK_LowOrder( dt0 , iter , fdbk )

!!#### PURPOSE
!! Solve the low-order (angularly-restricted) transport problem,
!! called from <<SOLVE_TAPACK>>.

!!#### REQUIRED INPUT/OUTPUT
!! * total elapsed time
!! * iteration count
REAL   ,INTENT(INOUT) :: dt0
INTEGER,INTENT(INOUT) :: iter

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk
INTEGER :: AcSave

!!--begin--

IF( NoLoUpdate )THEN
    AcSave=AccelerationModule
    IF( iter>0 )THEN
        AccelerationModule=0
        !this should not be here but there is some stuff that it does that 
        !we can't put elsewhere right now (i.e. TAPACK is hardwired in some
        !sense to run both problems (HI and LO)
        CALL SOLVE_QuasiDiffusion( iter , dt0 , fdbk )
    ELSEIF( iter==0 )THEN
        AccelerationModule=QuasiDiffusion_
    END IF
END IF

!select acceleration module
SELECT CASE( AccelerationModule )

  CASE( QuasiDiffusion_ ) 
    CALL SOLVE_QuasiDiffusion( iter , dt0 , fdbk )

  CASE DEFAULT
   SELECT CASE( TransportModule )
    CASE( MOCshort_ ) ; CALL UpdateAndDump(fdbk_warning,fdbk,s="[[TAP]] Bypassing LOQD source update.")
                        CALL SOURCE_UPDATE_MOCshort( iter , dt0 , fdbk )
    CASE DEFAULT      ; CALL UpdateAndDump(fdbk_error,fdbk,s="[[TAP]] No source update specified.")
   END SELECT

END SELECT

IF( NoLoUpdate )THEN
    AccelerationModule=AcSave
END IF


!!--end--
END SUBROUTINE



!!### FUNCTION >>REFINE_TAPACK<<
FUNCTION REFINE_TAPACK( dt0 , iter , FdBK ) RESULT(Refined)

!!#### PURPOSE
!! Allow the solver packages to check if they require
!! refinement of the solution and check convergence.
!! We require that each performs no refinement and then
!! and only then have convergence.

!!#### REQUIRED INPUT/OUTPUT
REAL   ,INTENT(INOUT) :: dt0
INTEGER,INTENT(INOUT) :: iter

!!#### REQUIRED OUTPUT
LOGICAL :: Refined

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_="REFINE_TAPACK"

!!#### LOCAL VARIABLES
LOGICAL :: Refined_
REAL    :: dt,tout,tin

!!--begin--

!begin time
CALL CPU_TIME(tin)

!0. start with false and let the packages decide
Refined = .FALSE.

!1. high order (transport module)
SELECT CASE( TransportModule )

 CASE( MoCshort_ )
   Refined_ = REFINE_MoCshort( iter , dt0 , fdbk , Unit=Unit_ithi )

END SELECT
Refined = (Refined .OR. Refined_)


!2. low order (acceleration module) setup
SELECT CASE( AccelerationModule )

 CASE( QuasiDiffusion_ )
   Refined_ = REFINE_QuasiDiffusion( iter , dt0 , fdbk , Unit=Unit_itlo )

END SELECT
Refined = (Refined .OR. Refined_)


!end time
CALL CPU_TIME(tout)
dt = tout - tin

!timing update
CALL UpdateAndDump(fdbk_profile,fdbk,s="[[TAP]] refinement completed in [time="//TRIM(STRTIME(dt))//"]")

!!--end--
END FUNCTION



!!### SUBROUTINE >>CHECK_TAPACK<<
SUBROUTINE CHECK_TAPACK( dt0 , iter , FdBK )

!!#### PURPOSE
!! Checks the solutions by some user-input functionality.

!!#### REQUIRED INPUT/OUTPUT
REAL   ,INTENT(INOUT) :: dt0
INTEGER,INTENT(INOUT) :: iter

!!#### REQUIRED OUTPUT
LOGICAL :: Refined

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_="CHECK_TAPACK"

!!#### LOCAL VARIABLES
REAL    :: dt,tout,tin

!!--begin--

!begin time
CALL CPU_TIME(tin)

!print for megatable
CALL PRINT_megatable(megatable)

!checking
IF( iter>=2 )THEN
 CALL PERFORM_CHECKING(megatable_checksoln,checkbnd,checkfn,checktol,fdbk)
END IF

!end time
CALL CPU_TIME(tout)
dt = tout - tin

!timing update
CALL UpdateAndDump(fdbk_profile,fdbk,s="[[TAP]] checking completed in [time="//TRIM(STRTIME(dt))//"]")

!!--end--
END SUBROUTINE



!!### SUBROUTINE >>PRINT_Options<<
SUBROUTINE PRINT_Options(Unit)

!!#### PURPOSE
!! Print the options TAPACK is currently running with.

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: Unit

!!--begin--

CALL SET_VSTROPTION(LeadSpace=4,OptionNameLength=30,OptionValLength=30)

WRITE(Unit,"(a)")" * [[TAP]] Options"
CALL PUT_line(Unit,VSTROPTION("EnergyDiscretization",STR(EnergyDiscretization)))
CALL PUT_line(Unit,VSTROPTION("AngularDiscretization",STR(AngularDiscretization)))
CALL PUT_line(Unit,VSTROPTION("SpatialDiscretization",STR(SpatialDiscretization)))
CALL PUT_line(Unit,"")
CALL PUT_line(Unit,VSTROPTION("SourceModule",STR(SourceModule)))
CALL PUT_line(Unit,VSTROPTION("XSModule",STR(XSModule)))
CALL PUT_line(Unit,VSTROPTION("TransmutationModule",STR(TransmutationModule)))
CALL PUT_line(Unit,VSTROPTION("TransportModule",STR(TransportModule)))
CALL PUT_line(Unit,VSTROPTION("AccelerationModule",STR(AccelerationModule)))
CALL PUT_line(Unit,"")
CALL PUT_line(Unit,VSTROPTION("TimeTreatment",STR(TimeTreatment)))
CALL PUT_line(Unit,VSTROPTION("NumberOfDimensions",STR(NDim)))
CALL PUT_line(Unit,VSTROPTION("PrintEachIteration",STR(PrintEachIteration)))
CALL PUT_line(Unit,VSTROPTION("MAX_iter",STR(MAX_iter)))
CALL PUT_line(Unit,VSTROPTION("MIN_iter",STR(MIN_iter)))
CALL PUT_line(Unit,VSTROPTION("tolPhiVInf",STR(tolPhiVInf)))
CALL PUT_line(Unit,"")

!allow each module to output its own options
SELECT CASE(TransportModule)
 CASE(MoCshort_) ; CALL PRINT_Options_MCS(Unit)
END SELECT

!SELECT CASE(AccelerationModule)
! CASE(QuasiDiffusion_) ; CALL PRINT_Options_QDF(Unit)
!END SELECT

!!--end--
END SUBROUTINE


!!### SUBROUTINE >>SAVE_Mesh<<
SUBROUTINE SAVE_Mesh(file,Mesh,fdbk)
!!#### PURPOSE
!! Output the mesh to a file in its most basic form.
USE CCS_Mesh,ONLY: MSHwrite

!!#### REQUIRED INPUT/OUTPUT
CHARACTER(*)   ,INTENT(IN)    :: file
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### OPTIONAL INPUT
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
INTEGER :: k,j,i
TYPE(TYPE_SIO),POINTER :: SIO=>NULL()

!!--begin--

CALL CONNECT(sio,ACTION  = "Write"     ,&
                 FORM    = "Formatted" ,&
                 FILE    = file  ,&
                 ACCESS  = "Sequential",&
                 fdbk    = fdbk )

SIO%Comment      = "!"
SIO%Continue     = "&"
SIO%Stop         = "#"
SIO%Ignore       = " "
SIO%CmdBeg       = "\"
SIO%ArgBeg       = "{"
SIO%ArgEnd       = "}"
SIO%ArgDelim     = "}{"
SIO%SubArgDelim  = ","
SIO%ArgDefine    = "="
SIO%Argdefault   = "*"
SIO%StrTag       = '"'
SIO%LEN_ArgDelim = 2


CALL MSHwrite(sio,Mesh,fdbk)

CALL DISCONNECT(sio,fdbk)

!!--end--
END SUBROUTINE



END MODULE
