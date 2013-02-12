!!# MODULE <<VAR_TAPACK>>
MODULE VAR_TAPACK

!!## PURPOSE
!! Store the essential variables of TAPACK.

!! Define all the command variables of TApack.  Setting
!! these commands correctly (and providing the right input) makes
!! TApack do what you want.

!!## CHANGES
!!## v2.23 - added DirectionIndex to megatable output (prints the current m direction)


!!## STANDARD FORTRAN MODULES
USE KND_IntrinsicTypes                                                  !!((01-A-KND_IntrinsicTypes.f90))
USE ISO_varying_string                                                  !!((03-A-ISO_varying_string.f90))

!!## EXTERNAL KINDS
USE KND_MoCshort                                                        !!((03-A-KND_MoCshort.f90))
USE KND_DiscreteOrdinates                                               !!((02-A-KND_DiscreteOrdinates.f90))

!!## EXTERNAL PARAMETERS
USE PAR_TAPACK                                                          !!((05-C-PAR_TAPACK.f90))

!!## EXTERNAL PROCEDURES
USE FUN_STR                                                             !!((05-B-FUN_STR.f90))
USE FUN_Error                                                           !!((04-A-FUN_Error.f90))
USE SUB_SAVE                                                            !!((06-B-SUB_SAVE.f90))
USE FUN_Interp1S_Linear,ONLY: Interp=>Interp1S_Linear                   !!((05-B-FUN_Interp1S_Linear.f90))
USE FUN_Default                                                         !!((04-A-FUN_Default.f90))
USE VAR_Units                                                           !!((03-A-VAR_Units.f90))
USE SUB_CLEAR                                                           !!((04-A-SUB_CLEAR.f90))
USE PRN_Table                                                           !!((11-B-PRN_Table.f90))
USE FUN_NewFile                                                         !!((05-B-FUN_NewFile.f90))
USE FUN_EXIST                                                           !!((04-B-FUN_EXIST.f90))
USE FUN_STR                                                             !!((05-B-FUN_STR.f90))
USE FUN_IsApprox                                                        !!((03-A-FUN_IsApprox.f90))
USE FUN_Upcase                                                          !!((03-A-FUN_Upcase.f90))
USE LIB_genMoments                                                      !!((13-B-LIB_genMoments.f90))
USE SUB_LOADn                                                           !!((06-B-SUB_LOADn.f90))
USE SUB_CLEARn                                                          !!((04-A-SUB_CLEARn.f90))

!!## GLOBAL TOOLBOXES
USE TBX_Mesh,ONLY: Vert,GET_Vert,GET_Direction,&                        !!((15-B-TBX_Mesh.f90))
                   CellVolume,NUM_Cells,NUM_Faces,FaceCentroid,&
                   CellCentroid,NUM_Verts,KIND_MSH,FaceNormal
USE TBX_ComputationalGeometry                                           !!((09-A-TBX_ComputationalGeometry.f90))
USE USR_IntegralRegion                                                  !!((14-B-USR_IntegralRegion.f90))
USE ISO_varying_string                                                  !!((03-A-ISO_varying_string.f90))
USE USR_FunctionParser                                                  !!((05-B-USR_FunctionParser.f90))
USE USR_TAPACK                                                          !!((48-C-USR_TAPACK.f90))
USE USR_fdbk                                                            !!((08-C-USR_fdbk.f90))

!!## GLOBAL VARIABLES
USE VAR_Mesh,ONLY: Mesh                                                 !!((46-B-VAR_Mesh.f90))
USE VAR_ScalarFluxes,ONLY: ScalarFluxV,ScalarFluxF,ScalarFluxC          !!((04-C-VAR_ScalarFluxes.f90))
USE VAR_Currents,ONLY: CurrentV,CurrentF,CurrentFN,CurrentC             !!((04-C-VAR_Currents.f90))
USE VAR_DiscreteOrdinates,ONLY: Ordinates,Weights,&                     !!((47-B-VAR_DiscreteOrdinates.f90))
  Polar_Angles,Azimuthal_Angles
USE VAR_AngularFluxes,ONLY: AngularFluxV,AngularFluxF,AngularFluxC      !!((04-C-VAR_AngularFluxes.f90))
USE VAR_EddingtonFactors,ONLY: EddingtonxxV,EddingtonyyV,EddingtonxyV,& !!((04-C-VAR_EddingtonFactors.f90))
  KxxV,KxyV,KyyV

!!## DEFAULT ACCESS
IMPLICIT NONE

PUBLIC

!!## METHODS VARIABLES
!! * time treatment
INTEGER :: TimeTreatment = 0
!! * energy discretization settings
INTEGER :: EnergyDiscretization = 0
!! * angular discretization settings
INTEGER :: AngularDiscretization = 0
!! * spatial discretization settings
INTEGER :: SpatialDiscretization = 0
!! * transport method
INTEGER :: TransportModule    = 0
!! * acceleration method
INTEGER :: AccelerationModule = 0
!! * source module
INTEGER :: SourceModule = 0
!! * xs module
INTEGER :: XSModule = 0
!! * transmutation module
INTEGER :: TransmutationModule = 0
!! * number of dimensions
INTEGER :: NDim = 0

!!## OUTPUT VARIABLES
!! * output job
INTEGER         :: OutputJob
!! * output unit
INTEGER         :: Unit_out  = 0
!! * convergence history unit for high order
INTEGER         :: Unit_ithi = 0
!! * convergence history unit for low order
INTEGER         :: Unit_itlo = 0
!! * list of input edits
INTEGER ,POINTER :: LIST_InputEdits( : )=>NULL()
!! * list of output edits
INTEGER ,POINTER :: LIST_Outputs( : )=>NULL()

!!## REQUEST VARIABLES
!! * visualization request
LOGICAL :: Requesting_Visualization = .FALSE.

!!## FILE STRUCTURE VARIABLES
!! * input file
CHARACTER(LEN_file)  :: MasterInputFile = ''


!!## INFORMATION VARIABLES
!! * output file name base (add extensions to everything) <OutputFileBase>
!! * label for the output file <label>
!! * author of the output file <author>
!! * details for the output file <details>
CHARACTER(LEN_File)  :: OutputFileBase = ''
CHARACTER(LEN_label) :: Label
TYPE(varying_string) :: Author
TYPE(varying_string) :: Details

!! * run time <dt0>
!! * iteration count <iter>
!! * whether all residual criteria have been met <ResidualsMet>
REAL    :: dt0        = 0.
REAL    :: dt(8)      = 0.
REAL,POINTER :: TAP_time(:) => NULL()
CHARACTER(32),POINTER :: TAP_what(:) => NULL()
INTEGER :: tt         = 0
INTEGER :: iter       = 0
LOGICAL :: AnyRefined = .FALSE.
INTEGER              :: MAX_iter     = 99999
INTEGER              :: MIN_iter     = 0
LOGICAL :: IterationsExceeded = .FALSE.
REAL(KIND_ScalarFlux):: tolPhiVInf   = TINY(1._KIND_ScalarFlux)
REAL(KIND_ScalarFlux):: reltolPhiVInf = 1.E-8_KIND_ScalarFlux
REAL(KIND_ScalarFlux):: tolBalRel = 1.E-8_KIND_ScalarFlux
REAL(KIND_ScalarFlux):: tolBalAbs = 1.E-8_KIND_ScalarFlux


INTEGER ,POINTER :: glist(:)=>NULL(),klist(:)=>NULL()

LOGICAL :: PrintEachIteration = .FALSE.
LOGICAL :: SaveSolution       = .TRUE.
LOGICAL :: LoadSolution       = .FALSE.
LOGICAL :: OutputGMV          = .TRUE.
LOGICAL :: NoLoUpdate         = .FALSE.
LOGICAL :: Enforce_Consistency= .FALSE.

!!## PRIVATE variables
REAL(KIND_AngularFlux),ALLOCATABLE :: ysave(:)
REAL(KIND_DOR)        ,ALLOCATABLE :: xsave(:)
PRIVATE                            :: ysave,xsave
!! * the evaluation method for boundary factors
!!   (or Psi if we are evaluating Psi first.)
!! * the evaluation method for E's (or Psi if we are
!!   evaluating Psi first.)
INTEGER :: FactorGenerationStyle = TAP_EvalFactorFirst
INTEGER :: FactorEvalMethod      = TAP_IntegrateTrapezoidal



TYPE(TYPE_IntegralRegion),POINTER :: IntegralRegions(:)=>NULL()

REAL(KIND_MSH),POINTER :: exit_r1(:)=>NULL()
REAL(KIND_MSH),POINTER :: exit_r2(:)=>NULL()

TYPE(TYPE_megatable),POINTER :: megatable(:) => NULL()
!! * solution checking variables
TYPE(TYPE_megatable),POINTER :: megatable_checksoln(:) => NULL()
INTEGER             ,POINTER :: checkfn(:)=>NULL()
INTEGER             ,POINTER :: checkbnd(:)=>NULL()
REAL(KIND_Rdp)      ,POINTER :: checktol(:)=>NULL()

PUBLIC :: NDim
PUBLIC :: TAP_ALLOCATE_P1
PUBLIC :: TAP_ALLOCATE_P2
PUBLIC :: TAP_ALLOCATE_P3
PUBLIC :: TAP_ALLOCATE_P4

!!## CONTAINED PROCEDURES
CONTAINS



SUBROUTINE TAP_ALLOCATE_P1(VarName,Var,NVar,fdbk,init,caller)

!!#### REQUIRED INPUT/OUTPUT
CHARACTER(*)  ,INTENT(IN) :: VarName
REAL(KIND_Rdp),POINTER    :: Var(:)
INTEGER                   :: Nvar(1)

!!#### OPTIONAL VARIABLES
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk
REAL(KIND_Rdp)  ,INTENT(IN)  ,OPTIONAL :: init
CHARACTER(*)    ,INTENT(IN)  ,OPTIONAL :: caller

CHARACTER(7) :: caller_
CHARACTER(100) :: File

!!--begin--
caller_ = Default( "[[TAP]]" , caller )

IF( ASSOCIATED(Var) )THEN
 CALL UpdateAndDump(fdbk_comment,fdbk,s=TRIM(caller_)//" The variable <"//VarName//"> is already&
  & associated and will not be reallocated ...")
 RETURN
END IF

File=TRIM(OutputFileBase)//"-"//VarName
IF( LoadSolution .AND. Exist(TRIM(File)//".dat") )THEN
 CALL UpdateAndDump(fdbk_comment,fdbk,s=TRIM(caller_)//" Loading <"//VarName//"> from [file="//&
  TRIM(File)//".dat] ...")
 CALL LOADn(File,Var)
ELSE
 CALL UpdateAndDump(fdbk_comment,fdbk,s=TRIM(caller_)//" Allocating <"//VarName//"> ...")
 CALL CLEARn(Var)
 ALLOCATE( Var(NVar(1)) )
 IF( PRESENT(init) )THEN
  Var = init
 ELSE
  Var = -HUGE(0._KIND_Rdp)
 END IF
END IF

!!--end--
END SUBROUTINE



SUBROUTINE TAP_ALLOCATE_P2(VarName,Var,NVar,fdbk,init,caller)

!!#### REQUIRED INPUT/OUTPUT
CHARACTER(*)  ,INTENT(IN) :: VarName
REAL(KIND_Rdp),POINTER    :: Var(:,:)
INTEGER                   :: Nvar(2)

!!#### OPTIONAL VARIABLES
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk
REAL(KIND_Rdp)  ,INTENT(IN)  ,OPTIONAL :: init
CHARACTER(*)    ,INTENT(IN)  ,OPTIONAL :: caller

CHARACTER(7) :: caller_
CHARACTER(100) :: File

!!--begin--
caller_ = Default( "[[TAP]]" , caller )

IF( ASSOCIATED(Var) )THEN
 CALL UpdateAndDump(fdbk_comment,fdbk,s=TRIM(caller_)//" The variable <"//VarName//"> is already&
  & associated and will not be reallocated ...")
 RETURN
END IF

File=TRIM(OutputFileBase)//"-"//VarName
IF( LoadSolution .AND. Exist(TRIM(File)//".dat") )THEN
 CALL UpdateAndDump(fdbk_comment,fdbk,s=TRIM(caller_)//" Loading <"//VarName//"> from [file="//&
  TRIM(File)//".dat] ...")
 CALL LOADn(File,Var)
ELSE
 CALL UpdateAndDump(fdbk_comment,fdbk,s=TRIM(caller_)//" Allocating <"//VarName//"> ...")
 CALL CLEARn(Var)
 ALLOCATE( Var(NVar(1),NVar(2)) )
 IF( PRESENT(init) )THEN
  Var = init
 ELSE
  Var = -HUGE(0._KIND_Rdp)
 END IF
END IF

!!--end--
END SUBROUTINE




SUBROUTINE TAP_ALLOCATE_P3(VarName,Var,NVar,fdbk,init,caller)

!!#### REQUIRED INPUT/OUTPUT
CHARACTER(*)  ,INTENT(IN) :: VarName
REAL(KIND_Rdp),POINTER    :: Var(:,:,:)
INTEGER                   :: Nvar(3)

!!#### OPTIONAL VARIABLES
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk
REAL(KIND_Rdp)  ,INTENT(IN)  ,OPTIONAL :: init
CHARACTER(*)    ,INTENT(IN)  ,OPTIONAL :: caller

CHARACTER(7) :: caller_
CHARACTER(100) :: File

!!--begin--
caller_ = Default( "[[TAP]]" , caller )

IF( ASSOCIATED(Var) )THEN
 CALL UpdateAndDump(fdbk_comment,fdbk,s=TRIM(caller_)//" The variable <"//VarName//"> is already&
  & associated and will not be reallocated ...")
 RETURN
END IF

File=TRIM(OutputFileBase)//"-"//VarName
IF( LoadSolution .AND. Exist(TRIM(File)//".dat") )THEN
 CALL UpdateAndDump(fdbk_comment,fdbk,s=TRIM(caller_)//" Loading <"//VarName//"> from [file="//&
  TRIM(File)//".dat] ...")
 CALL LOADn(File,Var)
ELSE
 CALL UpdateAndDump(fdbk_comment,fdbk,s=TRIM(caller_)//" Allocating <"//VarName//"> ...")
 CALL CLEARn(Var)
 ALLOCATE( Var(NVar(1),NVar(2),NVar(3)) )
 IF( PRESENT(init) )THEN
  Var = init
 ELSE
  Var = -HUGE(0._KIND_Rdp)
 END IF
END IF

!!--end--
END SUBROUTINE


SUBROUTINE TAP_ALLOCATE_P4(VarName,Var,NVar,fdbk,init,caller)

!!#### REQUIRED INPUT/OUTPUT
CHARACTER(*)  ,INTENT(IN) :: VarName
REAL(KIND_Rdp),POINTER    :: Var(:,:,:,:)
INTEGER                   :: Nvar(4)

!!#### OPTIONAL VARIABLES
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk
REAL(KIND_Rdp)  ,INTENT(IN)  ,OPTIONAL :: init
CHARACTER(*)    ,INTENT(IN)  ,OPTIONAL :: caller

CHARACTER(7) :: caller_
CHARACTER(100) :: File

!!--begin--
caller_ = Default( "[[TAP]]" , caller )

IF( ASSOCIATED(Var) )THEN
 CALL UpdateAndDump(fdbk_comment,fdbk,s=TRIM(caller_)//" The variable <"//VarName//"> is already&
  & associated and will not be reallocated ...")
 RETURN
END IF

File=TRIM(OutputFileBase)//"-"//VarName
IF( LoadSolution .AND. Exist(TRIM(File)//".dat") )THEN
 CALL UpdateAndDump(fdbk_comment,fdbk,s=TRIM(caller_)//" Loading <"//VarName//"> from [file="//&
 TRIM(File)//".dat] ...")
 CALL LOADn(File,Var)
ELSE
 CALL UpdateAndDump(fdbk_comment,fdbk,s=TRIM(caller_)//" Allocating <"//VarName//"> ...")
 CALL CLEARn(Var)
 ALLOCATE( Var(NVar(1),NVar(2),NVar(3),NVar(4)) )
 IF( PRESENT(init) )THEN
  Var = init
 ELSE
  Var = -HUGE(0._KIND_Rdp)
 END IF
END IF

!!--end--
END SUBROUTINE


SUBROUTINE PRINT_megatable(megatable)
TYPE(TYPE_megatable),POINTER :: megatable(:)

INTEGER :: b,unit_,n,nr
CHARACTER(32) :: spec
CHARACTER(32),ALLOCATABLE :: D(:,:)

!!--begin--

IF( ASSOCIATED(megatable) )THEN
 n = SIZE(megatable)
ELSE
 n = 0
END IF

IF( n==0 )RETURN

!check to see if we have the file already
IF( .NOT.Exist("megatable.txt") )THEN
 nr = 2
ELSE
 nr = 1
END IF
!get the unit to output to
unit_ = NewFile( "megatable.txt" , &
  Status="Unknown" , Position="Append" , IfOpened="Close")

!allocate the table
ALLOCATE( D(2,n) )

!clear the table
CALL CLEAR(D)

DO b=1,n
 CALL CLEAR(spec)
 spec = megatable(b)%spec

 SELECT CASE( TRIM(spec) )

  CASE("Identifier"); CALL mt_Identifier( megatable(b) , D(1,b) , D(2,b)   )

  CASE("DirectionIndex"); CALL mt_DirectionIndex( megatable(b) , D(1,b) , D(2,b)   )
  CASE("PolarAngle"); CALL mt_PolarAngle( megatable(b) , D(1,b) , D(2,b)   )
  CASE("AzimuthalAngle"); CALL mt_AzimuthalAngle( megatable(b) , D(1,b) , D(2,b)   )
  
  CASE("XV"        ); CALL mt_XV       ( megatable(b) , D(1,b) , D(2,b)   )
  CASE("YV"        ); CALL mt_YV       ( megatable(b) , D(1,b) , D(2,b)   )

  CASE("PsiV"      ); CALL mt_PsiV     ( megatable(b) , D(1,b) , D(2,b)   )
!  CASE("PsiF"      ); CALL mt_PsiF     ( megatable(b) , D(1,b) , D(2,b)   )
  CASE("PsiC"      ); CALL mt_PsiC     ( megatable(b) , D(1,b) , D(2,b)   )
!
  CASE("PhiV"      ); CALL mt_PhiV     ( megatable(b) , D(1,b) , D(2,b)   )
!  CASE("PhiF"      ); CALL mt_PhiF     ( megatable(b) , D(1,b) , D(2,b)   )
  CASE("PhiC"      ); CALL mt_PhiC     ( megatable(b) , D(1,b) , D(2,b)   )
  CASE("PhiD"      ); CALL mt_PhiD     ( megatable(b) , D(1,b) , D(2,b)   )
  CASE("PhiR"      ); CALL mt_PhiR     ( megatable(b) , D(1,b) , D(2,b)   )

  CASE("JxV"       ); CALL mt_JxV      ( megatable(b) , D(1,b) , D(2,b)   )
!  CASE("JxF"       ); CALL mt_JxF      ( megatable(b) , D(1,b) , D(2,b)   )
!  CASE("JxC"       ); CALL mt_JxC      ( megatable(b) , D(1,b) , D(2,b)   )
  CASE("JyV"       ); CALL mt_JyV      ( megatable(b) , D(1,b) , D(2,b)   )
!  CASE("JyF"       ); CALL mt_JyF      ( megatable(b) , D(1,b) , D(2,b)   )
!  CASE("JyC"       ); CALL mt_JyC      ( megatable(b) , D(1,b) , D(2,b)   )
!  CASE("JFN"       ); CALL mt_JFN      ( megatable(b) , D(1,b) , D(2,b)   )
!
  CASE("ExxV"      ); CALL mt_ExxV     ( megatable(b) , D(1,b) , D(2,b)   )
!  CASE("ExxF"      ); CALL mt_ExxF     ( megatable(b) , D(1,b) , D(2,b)   )
!  CASE("ExxC"      ); CALL mt_ExxC     ( megatable(b) , D(1,b) , D(2,b)   )
  CASE("EyyV"      ); CALL mt_EyyV     ( megatable(b) , D(1,b) , D(2,b)   )
!  CASE("EyyF"      ); CALL mt_EyyF     ( megatable(b) , D(1,b) , D(2,b)   )
!  CASE("EyyC"      ); CALL mt_EyyC     ( megatable(b) , D(1,b) , D(2,b)   )
  CASE("ExyV"      ); CALL mt_ExyV     ( megatable(b) , D(1,b) , D(2,b)   )
!  CASE("ExyF"      ); CALL mt_ExyF     ( megatable(b) , D(1,b) , D(2,b)   )
!  CASE("ExyC"      ); CALL mt_ExyC     ( megatable(b) , D(1,b) , D(2,b)   )
!
  CASE("KxxV" ); CALL mt_KxxV( megatable(b) , D(1,b) , D(2,b)   )
!  CASE("numerExxF" ); CALL mt_numerExxF( megatable(b) , D(1,b) , D(2,b)   )
!  CASE("numerExxC" ); CALL mt_numerExxC( megatable(b) , D(1,b) , D(2,b)   )
  CASE("KyyV" ); CALL mt_KyyV( megatable(b) , D(1,b) , D(2,b)   )
!  CASE("numerEyyF" ); CALL mt_numerEyyF( megatable(b) , D(1,b) , D(2,b)   )
!  CASE("numerEyyC" ); CALL mt_numerEyyC( megatable(b) , D(1,b) , D(2,b)   )
  CASE("KxyV" ); CALL mt_KxyV( megatable(b) , D(1,b) , D(2,b)   )
!  CASE("numerExyF" ); CALL mt_numerExyF( megatable(b) , D(1,b) , D(2,b)   )
!  CASE("numerExyC" ); CALL mt_numerExyC( megatable(b) , D(1,b) , D(2,b)   )
!
!  CASE("LeakageC"  ); CALL mt_LeakageC ( megatable(b) , D(1,b) , D(2,b)   )
!
!  CASE("FlowRateF" ); CALL mt_FlowRateF( megatable(b) , D(1,b) , D(2,b)   )
!
!  CASE("AbsRateC"  ); CALL mt_AbsRateC ( megatable(b) , D(1,b) , D(2,b)   )
!
!  CASE("ExitFlowRate"); CALL mt_ExitFlowRate ( megatable(b) , D(1,b) , D(2,b)   )
  CASE("NumIts" ); CALL mt_NumIts( megatable(b) , D(1,b) , D(2,b)   )

 END SELECT
 IF( TRIM(spec)=="Note" )THEN
  D(1,b) = TRIM(ADJUSTL(D(1,b)))//"-"//TRIM(STR(megatable(b)%Note))
 END IF
END DO

!print the table
IF( Nr==2 )THEN
 CALL PRINT_Table(D(1:2,:),Unit=Unit_,&
   printseparator=(/.TRUE., .TRUE.,.FALSE./),&
   usefixedwidth=SPREAD(.TRUE.,1,SIZE(D,2)),&
   CenterFirstRow=.TRUE. )
ELSE
 CALL PRINT_Table(D(2:2,:),Unit=Unit_,&
   printseparator=(/.FALSE., .FALSE.,.FALSE./),&
   usefixedwidth=SPREAD(.TRUE.,1,SIZE(D,2)),&
   CenterFirstRow=.FALSE. )
END IF

DEALLOCATE( D )

!!--end--
END SUBROUTINE


SUBROUTINE mt_NumIts( megatable , header , column )
TYPE(TYPE_megatable),INTENT(IN)  :: megatable
CHARACTER(*)        ,INTENT(OUT) :: header,column
REAL(KIND_MSH) :: R(Mesh%NDim)
INTEGER :: i,j,k,l,m,n,g
!!--begin--

header = "NumIts"
column = ADJUSTL(STR(iter,"(I5)"))

!!--end--
END SUBROUTINE

SUBROUTINE mt_DirectionIndex( megatable , header , column )
TYPE(TYPE_megatable),INTENT(IN)  :: megatable
CHARACTER(*)        ,INTENT(OUT) :: header,column
REAL(KIND_MSH) :: R(Mesh%NDim)
INTEGER :: i,j,k,l,m,n,g
!!--begin--

header = "DirectionIndex"
column = "N/A"

INCLUDE "14-C-USR_megatable_m.f90.sup"
IF( m==0 )THEN
    RETURN
END IF
column = ADJUSTL(STR(m,"(I5)"))

!!--end--
END SUBROUTINE

SUBROUTINE mt_AzimuthalAngle( megatable , header , column )
TYPE(TYPE_megatable),INTENT(IN)  :: megatable
CHARACTER(*)        ,INTENT(OUT) :: header,column
REAL(KIND_MSH) :: R(Mesh%NDim)
INTEGER :: i,j,k,l,m,n,g
!!--begin--

header = "AzimuthalAngle"
column = "N/A"

INCLUDE "14-C-USR_megatable_m.f90.sup"
IF( m==0 )THEN
    RETURN
END IF
column = ADJUSTL(STR(Azimuthal_Angles(m),"(Es16.8)"))

!!--end--
END SUBROUTINE

SUBROUTINE mt_PolarAngle( megatable , header , column )
TYPE(TYPE_megatable),INTENT(IN)  :: megatable
CHARACTER(*)        ,INTENT(OUT) :: header,column
REAL(KIND_MSH) :: R(Mesh%NDim)
INTEGER :: i,j,k,l,m,n,g
!!--begin--

header = "PolarAngle"
column = "N/A"

INCLUDE "14-C-USR_megatable_m.f90.sup"
IF( m==0 )THEN
    RETURN
END IF
column = ADJUSTL(STR(Polar_Angles(m),"(Es16.8)"))

!!--end--
END SUBROUTINE

SUBROUTINE mt_XV( megatable , header , column )
TYPE(TYPE_megatable),INTENT(IN)  :: megatable
CHARACTER(*)        ,INTENT(OUT) :: header,column
REAL(KIND_MSH) :: R(Mesh%NDim)
INTEGER :: i,j,k,l,m,n,g
!!--begin--

header = "XV"
column = "N/A"

INCLUDE "14-C-USR_megatable_k.f90.sup"
IF( k==0 )THEN
 RETURN
END IF

R = Vert(Mesh,k)
column = ADJUSTL(STR(R(1),"(Es21.13)"))

!!--end--
END SUBROUTINE


SUBROUTINE mt_YV( megatable , header , column )
TYPE(TYPE_megatable),INTENT(IN)  :: megatable
CHARACTER(*)        ,INTENT(OUT) :: header,column
REAL(KIND_MSH) :: R(Mesh%NDim)
INTEGER :: i,j,k,l,m,n,g
!!--begin--

header = "YV"
column = "N/A"

INCLUDE "14-C-USR_megatable_k.f90.sup"
IF( k==0 )THEN
 RETURN
END IF

R = Vert(Mesh,k)
column = ADJUSTL(STR(R(2),"(Es21.13)"))

!!--end--
END SUBROUTINE



SUBROUTINE mt_PsiV( megatable , header , column )
TYPE(TYPE_megatable),INTENT(IN)  :: megatable
CHARACTER(*)        ,INTENT(OUT) :: header,column
INTEGER :: i,j,k,l,m,n,g
!!--begin--

header = "PsiV"
column = "N/A"

INCLUDE "14-C-USR_megatable_g.f90.sup"
INCLUDE "14-C-USR_megatable_k.f90.sup"
INCLUDE "14-C-USR_megatable_m.f90.sup"
IF( g==0 .OR. k==0 .OR. m==0 )THEN
 RETURN
END IF

column = ADJUSTL(STR(AngularFluxV(g,k,m),"(Es21.13)"))

!!--end--
END SUBROUTINE



SUBROUTINE mt_PsiC( megatable , header , column )
TYPE(TYPE_megatable),INTENT(IN)  :: megatable
CHARACTER(*)        ,INTENT(OUT) :: header,column
INTEGER :: i,j,k,l,m,n,g
!!--begin--

header = "PsiC"
column = "N/A"

INCLUDE "14-C-USR_megatable_g.f90.sup"
INCLUDE "14-C-USR_megatable_i.f90.sup"
INCLUDE "14-C-USR_megatable_m.f90.sup"
IF( g==0 .OR. i==0 .OR. m==0 )THEN
 RETURN
END IF

column = ADJUSTL(STR(AngularFluxC(g,i,m),"(Es21.13)"))

!!--end--
END SUBROUTINE



SUBROUTINE mt_PhiV( megatable , header , column )
TYPE(TYPE_megatable),INTENT(IN)  :: megatable
CHARACTER(*)        ,INTENT(OUT) :: header,column
INTEGER :: i,j,k,l,m,n,g
!!--begin--

header = "PhiV"
column = "N/A"

INCLUDE "14-C-USR_megatable_g.f90.sup"
INCLUDE "14-C-USR_megatable_k.f90.sup"
IF( g==0 .OR. k==0 )THEN
 RETURN
END IF


column = ADJUSTL(STR(ScalarFluxV(g,k),"(Es21.13)"))

!!--end--
END SUBROUTINE


SUBROUTINE mt_PhiC( megatable , header , column )
TYPE(TYPE_megatable),INTENT(IN)  :: megatable
CHARACTER(*)        ,INTENT(OUT) :: header,column
INTEGER :: i,j,k,l,m,n,g
!!--begin--

header = "PhiC"
column = "N/A"

INCLUDE "14-C-USR_megatable_g.f90.sup"
INCLUDE "14-C-USR_megatable_i.f90.sup"
IF( g==0 .OR. i==0 )THEN
 RETURN
END IF


column = ADJUSTL(STR(ScalarFluxC(g,i),"(Es21.13)"))

!!--end--
END SUBROUTINE



SUBROUTINE mt_Identifier( megatable , header , column )
TYPE(TYPE_megatable),INTENT(IN)  :: megatable
CHARACTER(*)        ,INTENT(OUT) :: header,column
INTEGER :: i,j,k,l,m,n,g
!!--begin--

header = "Identifier"
column = ADJUSTL(STR(megatable%Note))

!!--end--
END SUBROUTINE



SUBROUTINE mt_PhiD( megatable , header , column )
TYPE(TYPE_megatable),INTENT(IN)  :: megatable
CHARACTER(*)        ,INTENT(OUT) :: header,column
REAL(KIND_Rdp) :: PhiD,Vol,TotVol
INTEGER :: i,j,k,l,m,n,g
!!--begin--

header = "PhiD"
column = "N/A"

INCLUDE "14-C-USR_megatable_g.f90.sup"
IF( g==0 )THEN
 RETURN
END IF

PhiD = 0.d0
Vol  = 0.d0
DO i=1,NUM_Cells(Mesh)
 Vol  = CellVolume(Mesh,i)
 PhiD = PhiD + ScalarFluxC(g,i)*Vol
 TotVol = TotVol + Vol
END DO
PhiD = PhiD/TotVol

column = ADJUSTL(STR(PhiD,"(Es21.13)"))

!!--end--
END SUBROUTINE


SUBROUTINE mt_PhiR( megatable , header , column )
TYPE(TYPE_megatable),INTENT(IN)  :: megatable
CHARACTER(*)        ,INTENT(OUT) :: header,column
REAL(KIND_Rdp) :: PhiR,Vol,TotVol
CHARACTER(32) :: region
INTEGER :: i,j,k,l,m,n,g
!!--begin--

header = "PhiR"
column = "N/A"

INCLUDE "14-C-USR_megatable_region.f90.sup"
IF( n==0 )THEN
 RETURN
END IF

!set the header to the region ID and use the
!region number n to evaluate the scaled average
!value in the region
header = "PhiR("//TRIM(region)//")"
PhiR = ( IntegralRegions(n) % integrals(2)/&
         IntegralRegions(n) % integrals(1) )*&
         IntegralRegions(n) % scale

column = ADJUSTL(STR(PhiR,"(Es21.13)"))

!!--end--
END SUBROUTINE


SUBROUTINE mt_JxV( megatable , header , column )
TYPE(TYPE_megatable),INTENT(IN)  :: megatable
CHARACTER(*)        ,INTENT(OUT) :: header,column
INTEGER :: i,j,k,l,m,n,g
!!--begin--

INCLUDE "14-C-USR_megatable_g.f90.sup"
INCLUDE "14-C-USR_megatable_k.f90.sup"

header = "JxV"
column = ADJUSTL(STR(CurrentV(1,g,k),"(Es21.13)"))

!!--end--
END SUBROUTINE


SUBROUTINE mt_JyV( megatable , header , column )
TYPE(TYPE_megatable),INTENT(IN)  :: megatable
CHARACTER(*)        ,INTENT(OUT) :: header,column
INTEGER :: i,j,k,l,m,n,g
!!--begin--

INCLUDE "14-C-USR_megatable_g.f90.sup"
INCLUDE "14-C-USR_megatable_k.f90.sup"

header = "JyV"
column = ADJUSTL(STR(CurrentV(2,g,k),"(Es21.13)"))

!!--end--
END SUBROUTINE



SUBROUTINE mt_ExxV( megatable , header , column )
TYPE(TYPE_megatable),INTENT(IN)  :: megatable
CHARACTER(*)        ,INTENT(OUT) :: header,column
INTEGER :: i,j,k,l,m,n,g
!!--begin--

INCLUDE "14-C-USR_megatable_g.f90.sup"
INCLUDE "14-C-USR_megatable_k.f90.sup"

header = "ExxV"
column = ADJUSTL(STR(EddingtonxxV(g,k),"(Es21.13)"))

!!--end--
END SUBROUTINE


SUBROUTINE mt_EyyV( megatable , header , column )
TYPE(TYPE_megatable),INTENT(IN)  :: megatable
CHARACTER(*)        ,INTENT(OUT) :: header,column
INTEGER :: i,j,k,l,m,n,g
!!--begin--

INCLUDE "14-C-USR_megatable_g.f90.sup"
INCLUDE "14-C-USR_megatable_k.f90.sup"

header = "EyyV"
column = ADJUSTL(STR(EddingtonyyV(g,k),"(Es21.13)"))

!!--end--
END SUBROUTINE


SUBROUTINE mt_ExyV( megatable , header , column )
TYPE(TYPE_megatable),INTENT(IN)  :: megatable
CHARACTER(*)        ,INTENT(OUT) :: header,column
INTEGER :: i,j,k,l,m,n,g
!!--begin--

INCLUDE "14-C-USR_megatable_g.f90.sup"
INCLUDE "14-C-USR_megatable_k.f90.sup"

header = "ExyV"
column = ADJUSTL(STR(EddingtonxyV(g,k),"(Es21.13)"))

!!--end--
END SUBROUTINE


SUBROUTINE mt_KxxV( megatable , header , column )
TYPE(TYPE_megatable),INTENT(IN)  :: megatable
CHARACTER(*)        ,INTENT(OUT) :: header,column
INTEGER :: i,j,k,l,m,n,g
!!--begin--

INCLUDE "14-C-USR_megatable_g.f90.sup"
INCLUDE "14-C-USR_megatable_k.f90.sup"

header = "KxxV"
column = ADJUSTL(STR(KxxV(g,k),"(Es21.13)"))

!!--end--
END SUBROUTINE


SUBROUTINE mt_KyyV( megatable , header , column )
TYPE(TYPE_megatable),INTENT(IN)  :: megatable
CHARACTER(*)        ,INTENT(OUT) :: header,column
INTEGER :: i,j,k,l,m,n,g
!!--begin--

INCLUDE "14-C-USR_megatable_g.f90.sup"
INCLUDE "14-C-USR_megatable_k.f90.sup"

header = "KyyV"
column = ADJUSTL(STR(KyyV(g,k),"(Es21.13)"))

!!--end--
END SUBROUTINE


SUBROUTINE mt_KxyV( megatable , header , column )
TYPE(TYPE_megatable),INTENT(IN)  :: megatable
CHARACTER(*)        ,INTENT(OUT) :: header,column
INTEGER :: i,j,k,l,m,n,g
!!--begin--

INCLUDE "14-C-USR_megatable_g.f90.sup"
INCLUDE "14-C-USR_megatable_k.f90.sup"

header = "KxyV"
column = ADJUSTL(STR(KxyV(g,k),"(Es21.13)"))

!!--end--
END SUBROUTINE


PURE ELEMENTAL FUNCTION Function_AngularFlux( x0 ) RESULT(y0)
!!#### ARGUMENT
REAL(KIND_DOR),INTENT(IN) :: x0
!!#### RESULT
REAL(KIND_AngularFlux) :: y0
!!--begin--
IF( x0<xsave(1) )THEN
 y0 = ysave(1)
ELSEIF( x0>xsave(SIZE(xsave)) )THEN
 y0 = ysave(SIZE(ysave))
ELSE
 y0 = Interp(x0,ysave,xsave)
END IF
!!--end--
END FUNCTION


!
!!### SUBROUTINE <<PERFORM_CHECKING>>
SUBROUTINE PERFORM_CHECKING(megatable,checkbnd,checkfn,checktol,fdbk)

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_megatable)  ,INTENT(INOUT) :: megatable(:)
INTEGER               ,POINTER       :: checkbnd(:)
INTEGER               ,POINTER       :: checkfn(:)
REAL(KIND_AngularFlux),POINTER       :: checktol(:)

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### LOCAL VARIABLES
INTEGER :: b,i,j,k,l,m,g,Ni,Nj,Nk,Nl,Nm,Ng
INTEGER,DIMENSION(2) :: ib,jb,kb,lb,mb,gb
REAL(KIND_AngularFlux) :: actualval,val,tol
LOGICAL :: Check
TYPE(varying_string) :: VS
CHARACTER(32) :: error
INTEGER :: NUM_Fails
!!--begin--

DO b=1,SIZE(megatable)
 NUM_Fails=0
 CALL mtExtract_Index_Bounds(megatable(b),ib,jb,kb,lb,mb,gb)
 tol = checktol(b)
 DO i=ib(1),ib(2)
  DO j=jb(1),jb(2)
   DO k=kb(1),kb(2)
    DO m=mb(1),mb(2)
     DO g=gb(1),gb(2)
      Check = CHECK_VARS( megatable(b)%spec, checkbnd(b),checkfn(b) , i,j,k,l,m,g, ActualVal , Val , Tol )
      IF( .NOT.Check )THEN
       NUM_Fails=NUM_Fails+1
       VS = FailMsg( megatable(b)%spec , i,j,k,l,m,g, ActualVal , Val , Tol )
       CALL UPDATE(fdbk_warning,fdbk,s=STR(VS) )
       CALL DUMP(fdbk)
      END IF
     END DO
    END DO
   END DO
  END DO
 END DO
 IF( NUM_Fails==0 )THEN
  CALL UPDATE(fdbk_comment,fdbk,s="[[TAP]] Solution checking for "//&
    TRIM(megatable(b)%spec)//" [test=PASS]")
 ELSE
  CALL UPDATE(fdbk_warning,fdbk,s="[[TAP]] Solution checking for "//&
    TRIM(megatable(b)%spec)//" [test=FAIL]")
 END IF
END DO

!!--end--
END SUBROUTINE


SUBROUTINE mtExtract_Index_Bounds(megatable,ib,jb,kb,lb,mb,gb)
TYPE(TYPE_megatable),INTENT(IN)  :: megatable
INTEGER,DIMENSION(2),INTENT(OUT) :: ib,jb,kb,lb,mb,gb
INTEGER :: i,j,k,l,m,n,g
INTEGER :: Ni,Nj,Nk,Nl,Nm,Nn,Ng
!!--begin--

Ni = NUM_Cells(Mesh)
IF( megatable%i>0 )THEN
 ib = megatable%i
ELSE IF( megatable%i<0 )THEN
 !hack
 ib=0
ELSE
 ib = (/1,Ni/)
END IF

Nj = NUM_Faces(Mesh)
IF( megatable%j>0 )THEN
 jb = megatable%j
ELSE IF( megatable%j<0 )THEN
 !hack
 jb=0
ELSE
 jb = (/1,Nj/)
END IF

Nk = NUM_Verts(Mesh)
IF( megatable%k>0 )THEN
 kb = megatable%k
ELSE IF( megatable%k<0 )THEN
 kb = GET_vert(Mesh,vert=megatable%r)
ELSE
 kb = (/1,Nk/)
END IF

Nm = NUM_Directions(AngularFluxV)
IF( megatable%m>0 )THEN
 mb = megatable%m
ELSE IF( megatable%m<0 )THEN
 mb = GET_Direction(Ordinates,megatable%Omega)
ELSE
 mb = (/1,Nm/)
END IF

Ng = NUM_EnergyGroups(AngularFluxv)
IF( megatable%g>0 )THEN
 gb = megatable%g
ELSE IF( megatable%g<0 )THEN
 !hack for one group
 gb = 0
ELSE
 gb = (/1,Ng/)
END IF

lb = 0

!!--end--
END SUBROUTINE


FUNCTION FailMsg( spec, i,j,k,l,m,g, ActualVal , Val , Tol )
!!#### PURPOSE
!! Produce a message for the failure.

!!#### REQUIRED INPUT
CHARACTER(*)          ,INTENT(IN) :: spec
INTEGER               ,INTENT(IN) :: i,j,k,l,m,g
REAL(KIND_AngularFlux),INTENT(IN) :: ActualVal,Val,Tol

!!#### REQUIRED OUTPUT
TYPE(varying_string)   :: FailMsg

!!--begin--

FailMsg = "The value of "//TRIM(STR(spec))
IF( i/=0 )THEN
 FailMsg = FailMsg//" at i="//TRIM(STR(i))
END IF
IF( j/=0 )THEN
 FailMsg = FailMsg//" at j="//TRIM(STR(j))
END IF
IF( k/=0 )THEN
 FailMsg = FailMsg//" at k="//TRIM(STR(k))
END IF
IF( l/=0 )THEN
 FailMsg = FailMsg//" at l="//TRIM(STR(l))
END IF
IF( g/=0 )THEN
 FailMsg = FailMsg//" at g="//TRIM(STR(g))
END IF
IF( m/=0 )THEN
 FailMsg = FailMsg//" at m="//TRIM(STR(m))
END IF
FailMsg = FailMsg//" was not equal to the actual value of "//TRIM(STR(ActualVal,"(E)"))//&
          " to tolerance of "//TRIM(STR(Tol,"(E)"))//". Your incorrect value was "//&
          TRIM(STR(Val,"(E)"))//"."

!!--end--
END FUNCTION




!!### FUNCTION <<CHECK_VARS>>
FUNCTION CHECK_VARS( spec , checkbnd,checkfn , i,j,k,l,m,g, ActualVal , Val , Tol ) RESULT(Check)

!!#### PURPOSE
!! Check some variables to see if they are equal to a value.

!!#### REQUIRED INPUT
CHARACTER(*)  ,INTENT(IN) :: spec
INTEGER       ,INTENT(IN) :: checkbnd,checkfn
INTEGER       ,INTENT(IN) :: i,j,k,l,m,g
REAL(KIND_Rdp),INTENT(IN) :: Tol

!!#### REQUIRED OUTPUT
REAL(KIND_Rdp),INTENT(OUT) :: ActualVal
REAL(KIND_Rdp),INTENT(OUT) :: Val

!!#### REQUIRED OUTPUT
LOGICAL :: Check

!!#### LOCAL VARIABLES
LOGICAL :: OutOfBounds

!!--begin--
SELECT CASE(TRIM(ADJUSTL(spec)))
 CASE("PsiV")
  Val       = AngularFluxV(g,k,m)
  ActualVal = EVAL_PsiV_fn( checkbnd,checkfn , g,k,m , OutOfBounds )
  IF( OutOfBounds )THEN
   Check = .TRUE.
  ELSE
   Check = IsApprox(AngularFluxV(g,k,m),ActualVal,RelTol=Tol)
  END IF

 CASE("PsiF")
  Val       = AngularFluxF(g,j,m)
  ActualVal = EVAL_PsiF_fn( checkbnd,checkfn , g,j,m , OutOfBounds )
  IF( OutOfBounds )THEN
   Check = .TRUE.
  ELSE
   Check = IsApprox(AngularFluxF(g,j,m),ActualVal,RelTol=Tol)
  END IF

 CASE("PsiC")
  Val       = AngularFluxC(g,i,m)
  ActualVal = EVAL_PsiC_fn( checkbnd,checkfn , g,i,m , OutOfBounds )
  IF( OutOfBounds )THEN
   Check = .TRUE.
  ELSE
   Check = IsApprox(AngularFluxC(g,i,m),ActualVal,RelTol=Tol)
  END IF

 CASE("PhiV")
  Val       = ScalarFluxV (g,k)
  ActualVal = EVAL_PhiV_fn( checkbnd,checkfn , g,k , OutOfBounds )
  IF( OutOfBounds )THEN
   Check = .TRUE.
  ELSE
   Check = IsApprox(   ScalarFluxV(g,k),ActualVal,RelTol=Tol)
  END IF

 CASE("PhiF")
  Val       = ScalarFluxF (g,j)
  ActualVal = EVAL_PhiF_fn( checkbnd,checkfn , g,j , OutOfBounds )
  IF( OutOfBounds )THEN
   Check = .TRUE.
  ELSE
   Check = IsApprox(   ScalarFluxF(g,j),ActualVal,RelTol=Tol)
  END IF

 CASE("PhiC")
  Val       = ScalarFluxC (g,i)
  ActualVal = EVAL_PhiC_fn( checkbnd,checkfn , g,i , OutOfBounds )
  IF( OutOfBounds )THEN
   Check = .TRUE.
  ELSE
   Check = IsApprox(   ScalarFluxC(g,i),ActualVal,RelTol=Tol)
  END IF

 CASE DEFAULT
  Check = .FALSE.
END SELECT

!!--end--
END FUNCTION


FUNCTION EVAL_PsiV_fn( checkbnd,checkfn , g,k,m , OutofBounds ) RESULT(ActualVal)
INTEGER   ,INTENT(IN) :: checkbnd,checkfn
INTEGER   ,INTENT(IN) :: g,k,m
LOGICAL   ,INTENT(OUT):: OutOfBounds
REAL(KIND_AngularFlux)  :: ActualVal

REAL(KIND_AngularFlux)  :: V(3),x,y,z,ox,oy,oz,mr

!!--begin--
V=Vert(Mesh,k)
x=V(1)
y=MERGE(V(2),0._KIND_Rdp,Mesh%NDim>=2)
z=MERGE(V(3),0._KIND_Rdp,Mesh%NDim>=3)
ox=Ordinates(1,m)
oy=Ordinates(2,m)
oz=Ordinates(3,m)
mr=REAL(m,KIND_Rdp)

CALL MyEvaluation( checkbnd,checkfn , (/x,y,z,ox,oy,oz,mr/), ActualVal , OutOfBounds )

!!--end--
END FUNCTION


SUBROUTINE MyEvaluation( checkbnd,checkfn , inparray , ReturnVal , OutofBounds )
INTEGER               ,INTENT(IN)  :: checkbnd,checkfn
REAL(KIND_Rdp)        ,INTENT(IN)  :: inparray(:)
REAL(KIND_AngularFlux),INTENT(OUT) :: ReturnVal
LOGICAL               ,INTENT(OUT) :: OutOfBounds
CHARACTER(32) :: errorstr
LOGICAL       :: Truth
REAL(KIND_AngularFlux) :: TestVal
!!--begin--

CALL s_evaluatefn( checkbnd, inparray, TestVal, errorstr)
IF( TRIM(errorstr)/="OK" )THEN
 WRITE(*,*)"[[TAP]] Error in function parser for truth test."
 STOP
END IF
![hack] a true/false test
Truth = (TestVal>0._KIND_Rdp)

OutOfBounds = .NOT.Truth

!get function evaluation
IF( OutOfBounds )THEN
 ReturnVal=Error(1._KIND_Rdp)
ELSE
 CALL s_evaluatefn( checkfn , inparray, ReturnVal, errorstr)
 IF( TRIM(errorstr)/="OK" )THEN
  WRITE(*,*)"[[TAP]] Error in function parser for function eval."
  STOP
 END IF
END IF
!!--end--
END SUBROUTINE

FUNCTION EVAL_PsiF_fn( checkbnd,checkfn , g,j,m , OutOfBounds )  RESULT(ActualVal)
LOGICAL   ,INTENT(OUT):: OutOfBounds
INTEGER         ,INTENT(IN) :: checkbnd,checkfn
INTEGER     ,INTENT(IN) :: g,j,m
REAL(KIND_AngularFlux)  :: V(3),x,y,z,ox,oy,oz,mr
REAL(KIND_AngularFlux) :: ActualVal
CHARACTER(32) :: error
!!--begin--
V=FaceCentroid(Mesh,j)
x=V(1)
y=MERGE(V(2),0._KIND_Rdp,Mesh%NDim>=2)
z=MERGE(V(3),0._KIND_Rdp,Mesh%NDim>=3)
ox=Ordinates(1,m)
oy=Ordinates(2,m)
oz=Ordinates(3,m)
mr=REAL(m,KIND_Rdp)
CALL MyEvaluation( checkbnd,checkfn , (/x,y,z,ox,oy,oz,mr/), ActualVal , OutOfBounds )
!!--end--
END FUNCTION


FUNCTION EVAL_PsiC_fn( checkbnd,checkfn , g,i,m , OutOfBounds )  RESULT(ActualVal)
LOGICAL   ,INTENT(OUT):: OutOfBounds
INTEGER         ,INTENT(IN) :: checkbnd,checkfn
INTEGER     ,INTENT(IN) :: g,i,m
REAL(KIND_AngularFlux)  :: V(3),x,y,z,ox,oy,oz,mr
REAL(KIND_AngularFlux) :: ActualVal
CHARACTER(32) :: error
!!--begin--
V=CellCentroid(Mesh,i)
x=V(1)
y=MERGE(V(2),0._KIND_Rdp,Mesh%NDim>=2)
z=MERGE(V(3),0._KIND_Rdp,Mesh%NDim>=3)
ox=Ordinates(1,m)
oy=Ordinates(2,m)
oz=Ordinates(3,m)
mr=REAL(m,KIND_Rdp)
CALL MyEvaluation( checkbnd,checkfn , (/x,y,z,ox,oy,oz,mr/), ActualVal , OutOfBounds )
!!--end--
END FUNCTION


FUNCTION EVAL_PhiV_fn( checkbnd,checkfn , g,k , OutOfBounds )  RESULT(ActualVal)
LOGICAL   ,INTENT(OUT):: OutOfBounds
INTEGER         ,INTENT(IN) :: checkbnd,checkfn
INTEGER     ,INTENT(IN) :: g,k
REAL(KIND_AngularFlux)  :: V(3),x,y,z,ox,oy,oz
REAL(KIND_AngularFlux) :: ActualVal
CHARACTER(32) :: error
!!--begin--
V=Vert(Mesh,k)
x=V(1)
y=MERGE(V(2),0._KIND_Rdp,Mesh%NDim>=2)
z=MERGE(V(3),0._KIND_Rdp,Mesh%NDim>=3)
ox=0._KIND_Rdp
oy=0._KIND_Rdp
oz=0._KIND_Rdp
CALL MyEvaluation( checkbnd,checkfn , (/x,y,z,ox,oy,oz,0._KIND_Rdp/), ActualVal , OutOfBounds )
!!--end--
END FUNCTION


FUNCTION EVAL_PhiF_fn( checkbnd,checkfn , g,j , OutOfBounds )  RESULT(ActualVal)
LOGICAL   ,INTENT(OUT):: OutOfBounds
INTEGER     ,INTENT(IN) :: checkbnd,checkfn
INTEGER     ,INTENT(IN) :: g,j
REAL(KIND_AngularFlux)  :: V(3),x,y,z,ox,oy,oz
REAL(KIND_AngularFlux) :: ActualVal
CHARACTER(32) :: error
!!--begin--
V=FaceCentroid(Mesh,j)
x=V(1)
y=MERGE(V(2),0._KIND_Rdp,Mesh%NDim>=2)
z=MERGE(V(3),0._KIND_Rdp,Mesh%NDim>=3)
ox=0._KIND_Rdp
oy=0._KIND_Rdp
oz=0._KIND_Rdp
CALL MyEvaluation( checkbnd,checkfn , (/x,y,z,ox,oy,oz,0._KIND_Rdp/), ActualVal , OutOfBounds )
!!--end--
END FUNCTION


FUNCTION EVAL_PhiC_fn( checkbnd,checkfn , g,i , OutOfBounds )  RESULT(ActualVal)
LOGICAL   ,INTENT(OUT):: OutOfBounds
INTEGER     ,INTENT(IN) :: checkbnd,checkfn
INTEGER     ,INTENT(IN) :: g,i
REAL(KIND_AngularFlux)  :: V(3),x,y,z,ox,oy,oz
REAL(KIND_AngularFlux) :: ActualVal
CHARACTER(32) :: error
!!--begin--
V=CellCentroid(Mesh,i)
x=V(1)
y=MERGE(V(2),0._KIND_Rdp,Mesh%NDim>=2)
z=MERGE(V(3),0._KIND_Rdp,Mesh%NDim>=3)
ox=0._KIND_Rdp
oy=0._KIND_Rdp
oz=0._KIND_Rdp
CALL MyEvaluation( checkbnd,checkfn , (/x,y,z,ox,oy,oz,0._KIND_Rdp/), ActualVal , OutOfBounds )
!!--end--
END FUNCTION


PURE FUNCTION NUM_EnergyGroups( AngularFluxV ) RESULT(NUM)
REAL(KIND_AngularFlux),INTENT(IN) :: AngularFluxV(:,:,:)
INTEGER :: NUM
!!--begin--
NUM = SIZE(AngularFluxV,1)
!!--end--
END FUNCTION

PURE FUNCTION NUM_Directions( AngularFluxV ) RESULT(NUM)
REAL(KIND_AngularFlux),INTENT(IN) :: AngularFluxV(:,:,:)
INTEGER :: NUM
!!--begin--
NUM = SIZE(AngularFluxV,3)
!!--end--
END FUNCTION



!!### SUBROUTINE <<UPDATE_HighOrder_Variables>>
SUBROUTINE UPDATE_HighOrder_Variables(UpdateStr)

!!#### PURPOSE
!! Provide one subroutine through which to udpate variable values.

!!#### REQUIRED INPUT
CHARACTER(*),INTENT(IN) :: UpdateStr

!!#### LOCAL VARIABLES
CHARACTER(LEN(UpdateStr)) :: UpdateStr_

!!#### LOCAL VARIABLES
INTEGER :: g,k,i,j,m
REAL(KIND_MSH),ALLOCATABLE :: FN(:)

!!--begin--
UpdateStr_ = UpCase(UpdateStr)

!! 2.1. Vertex scalar fluxes.
IF( UpdateStr_=="SCALARFLUXV" .OR. UpdateStr_=="ALL" )THEN
IF( .NOT.ASSOCIATED(ScalarFluxV) )THEN
 ALLOCATE(ScalarFluxV(SIZE(AngularFluxV,1),SIZE(AngularFluxV,2)))
END IF

ScalarFluxV=0.d0
DO m=1,SIZE(AngularFluxV,3)
FORALL(k=1:SIZE(AngularFluxV,2) )
 FORALL(g=1:SIZE(AngularFluxV,1) )
  ScalarFluxV(g,k) = ScalarFluxV(g,k)+AngularFluxV(g,k,m)*Weights(m) !v2.22 Moment0( AngularFluxV(g,k,:) , Ordinates,Weights )
 END FORALL
END FORALL
END DO
END IF

!! 2.2. Face scalar fluxes.
IF( UpdateStr_=="SCALARFLUXF" .OR. UpdateStr_=="ALL" )THEN
IF( ASSOCIATED(AngularFluxF) )THEN
 IF( .NOT.ASSOCIATED(ScalarFluxF) )THEN
  ALLOCATE(ScalarFluxF(SIZE(AngularFluxF,1),SIZE(AngularFluxF,2)))
 END IF
 ScalarFluxF=0.d0
 DO m=1,SIZE(AngularFluxF,3)
 FORALL(j=1:SIZE(AngularFluxF,2) )
  FORALL(g=1:SIZE(AngularFluxF,1) )
   ScalarFluxF(g,j) = ScalarFluxF(g,j) + AngularFluxF(g,j,m)*Weights(m) !v2.22 Moment0( AngularFluxF(g,j,:) , Ordinates,Weights )
  END FORALL
 END FORALL
 END DO
END IF
END IF

!! 2.3. Cell scalar fluxes.
IF( UpdateStr_=="SCALARFLUXC" .OR. UpdateStr_=="ALL" )THEN
IF( ASSOCIATED(AngularFluxC) )THEN
 IF( .NOT.ASSOCIATED(ScalarFluxC) )THEN
  ALLOCATE(ScalarFluxC(SIZE(AngularFluxC,1),SIZE(AngularFluxC,2)))
 END IF
 ScalarFluxC=0.d0
 DO m=1,SIZE(AngularFluxC,3)
 FORALL(i=1:SIZE(AngularFluxC,2) )
  FORALL(g=1:SIZE(AngularFluxC,1) )
   ScalarFluxC(g,i) = ScalarFluxC(g,i) + AngularFluxC(g,i,m)*Weights(m) !v2.22 Moment0( AngularFluxC(g,i,:) , Ordinates,Weights )
  END FORALL
 END FORALL
 END DO
END IF
END IF

!! 2.4. Vertex current vectors.
IF( UpdateStr_=="CURRENTV" .OR. UpdateStr_=="ALL" )THEN
IF( .NOT.ASSOCIATED(CurrentV) )THEN
 ALLOCATE(CurrentV(2,SIZE(AngularFluxV,1),SIZE(AngularFluxV,2)))
END IF
CurrentV=0.d0
DO m=1,SIZE(AngularFluxV,3)
FORALL(k=1:SIZE(AngularFluxV,2) )
 FORALL(g=1:SIZE(AngularFluxV,1) )
   CurrentV(1,g,k) = CurrentV(1,g,k)+AngularFluxV(g,k,1)*Ordinates(1,m)*Weights(m)
   CurrentV(2,g,k) = CurrentV(2,g,k)+AngularFluxV(g,k,2)*Ordinates(2,m)*Weights(m)
  !v2.22 CurrentV(1,g,k) = Moment1_Dim( AngularFluxV(g,k,:) , Ordinates,Weights , 1 )
  !v2.22 CurrentV(2,g,k) = Moment1_Dim( AngularFluxV(g,k,:) , Ordinates,Weights , 2 )
 END FORALL
END FORALL
END DO
END IF


!! 2.5. Face Current Vectors.
IF( UpdateStr=="CURRENTF" .OR. UpdateStr_=="ALL" )THEN
IF( ASSOCIATED(AngularFluxF) )THEN

 IF(.NOT.ASSOCIATED(CurrentF) )THEN
  ALLOCATE( CurrentF(2,SIZE(AngularFluxF,1),SIZE(AngularFluxF,2)) )
 END IF
 CurrentF=0.d0
 DO m=1,SIZE(AngularFluxF,3)
 FORALL(j=1:SIZE(AngularFluxF,2) )
  FORALL(g=1:SIZE(AngularFluxF,1) )
   CurrentF(1,g,j) = CurrentF(1,g,j)+AngularFluxF(g,j,m)*Ordinates(1,m)*Weights(m)
   CurrentF(2,g,j) = CurrentF(2,g,j)+AngularFluxF(g,j,m)*Ordinates(2,m)*Weights(m)
   !v2.22 CurrentF(1,g,j) = Moment1_Dim( AngularFluxF(g,j,:) , Ordinates,Weights , 1 )
   !v2.22 CurrentF(2,g,j) = Moment1_Dim( AngularFluxF(g,j,:) , Ordinates,Weights , 2 )
  END FORALL
 END FORALL
 END DO

END IF
END IF

!! 2.6. Cell Current Vectors.
IF( UpdateStr_=="CURRENTC" .OR. UpdateStr_=="ALL" )THEN
IF( ASSOCIATED(AngularFluxC) )THEN

 IF(.NOT.ASSOCIATED(CurrentC) )THEN
  ALLOCATE( CurrentC(2,SIZE(AngularFluxC,1),SIZE(AngularFluxC,2)) )
 END IF

 CurrentC=0.d0
 DO m=1,SIZE(AngularFluxC,3)
 FORALL(i=1:SIZE(AngularFluxC,2) )
  FORALL(g=1:SIZE(AngularFluxC,1) )
   CurrentC(1,g,i) = CurrentC(1,g,i)+AngularFluxC(g,i,m)*Ordinates(1,m)*Weights(m)
   CurrentC(2,g,i) = CurrentC(2,g,i)+AngularFluxC(g,i,m)*Ordinates(2,m)*Weights(m)
   !v2.22 CurrentC(1,g,i) = Moment1_Dim( AngularFluxC(g,i,:) , Ordinates,Weights , 1 )
   !v2.22 CurrentC(2,g,i) = Moment1_Dim( AngularFluxC(g,i,:) , Ordinates,Weights , 2 )
  END FORALL
 END FORALL
 END DO

END IF
END IF

!update face normal current (all others should be done)
IF( UpdateStr_=="CURRENTFN" .OR. UpdateStr_=="ALL" )THEN
IF( ASSOCIATED(CurrentF) )THEN
 ALLOCATE( FN(1:Mesh%NDim) )
 IF( .NOT.ASSOCIATED(CurrentFN) )THEN
  ALLOCATE( CurrentFN(SIZE(CurrentF,2),SIZE(CurrentF,3)) )
 END IF

 DO j=1,NUM_Faces(Mesh)
  FN = FaceNormal(Mesh,j)
  DO g=1,SIZE(CurrentFN,1)
   CurrentFN(g,j) = DOT_PRODUCT( CurrentF(:,g,j) , FN )
  END DO
 END DO
 DEALLOCATE( FN )
END IF
END IF

!!--end--
END SUBROUTINE

END MODULE
