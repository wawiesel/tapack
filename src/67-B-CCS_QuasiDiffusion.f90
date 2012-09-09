!!## COMMAND CARD SWITCHBOARD: QuasiDiffusion
MODULE CCS_QuasiDiffusion
!!### PURPOSE
!! The command card switchboard for <TAPACK>.

!!### FORTRAN STANDARDS MODULES
USE ISO_varying_string !!((03-A-ISO_varying_string.f90))

!!### USER MODULES
USE USR_fdbk           !!((08-C-USR_fdbk.f90))

!!### FUNCTION MODULES
USE SUB_Pause          !!((04-B-SUB_Pause.f90))
USE FUN_NewFile        !!((05-B-FUN_NewFile.f90))
USE FUN_STR            !!((05-B-FUN_STR.f90))
USE FUN_VSTR           !!((05-B-FUN_VSTR.f90))

!!### COMMAND CARD MODULES
USE CC1_simple,ONLY: & !!((11-A-CC1_simple.f90))
  QDFsystem            =>simple , &
  QDFinteractive       =>simple , &
  QDFautoiterfix       =>simple , &
  QDFinterfacetype     =>simple , &
  QDFhalfcellshape     =>simple , &
  QDFprintmatrix       =>simple , &
  QDFdumptransportdata =>simple , &
  QDFprintresiduals    =>simple , &
  QDFanalytictestcase  =>simple , &
  QDFcellfunction      =>simple , &
  QDFnonlinearfixup    =>simple , &
  QDFmaxit             =>simple

!!#### LIBRARY MODULES
USE LIB_Prompts        !!((06-B-LIB_Prompts.f90))
USE LIB_GenericPhrases !!((07-B-LIB_GenericPhrases.f90))

!!#### TOOLBOXES
USE TBX_SIO            !!((10-A-TBX_SIO.f90))
USE TBX_Mesh           !!((15-B-TBX_Mesh.f90))

!!#### VARIABLE MODULES
USE PAR_QuasiDiffusion !!((05-C-PAR_QuasiDiffusion.f90))
USE VAR_QuasiDiffusion !!((46-B-VAR_QuasiDiffusion.f90))
USE VAR_Mesh           !!((46-B-VAR_Mesh.f90))
USE VAR_TAPACK         !!((66-C-VAR_TAPACK.f90))
USE VAR_QDAnalyticTest !!((33-C-VAR_QDAnalyticTest.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: mod_  = "CCS_QuasiDiffusion"
CHARACTER(*),PARAMETER :: file_ = "38-B-CCS_QuasiDiffusion.f90"

!!#### PRE-CALCULATED COMMAND HASHES
INTEGER,PARAMETER :: interactive_       = 0000386226
INTEGER,PARAMETER :: noisy_             = 0000025507
INTEGER,PARAMETER :: solver_            = 0000054741
INTEGER,PARAMETER :: system_            = 0000064467
INTEGER,PARAMETER :: reorder_           = 0000080388
INTEGER,PARAMETER :: sparskit_          = 0000140398
INTEGER,PARAMETER :: maxit_             = 0000021301
INTEGER,PARAMETER :: diffusionits_      = 0000441260
INTEGER,PARAMETER :: eoverride_         = 0000187079
INTEGER,PARAMETER :: coverride_         = 0000182177
INTEGER,PARAMETER :: autoiterfix_       = 0000409178
INTEGER,PARAMETER :: interfacetype_     = 0000708223
INTEGER,PARAMETER :: halfcellshape_     = 0000514005
INTEGER,PARAMETER :: bc_                = 0000000485
INTEGER,PARAMETER :: printmatrix_       = 0000457674
INTEGER,PARAMETER :: printresiduals_    = 0001128213
INTEGER,PARAMETER :: dumptransportdata_ = 0001168857
INTEGER,PARAMETER :: analytictestcase_  = 0001424924
INTEGER,PARAMETER :: analytictestvals_  = 0001432006
INTEGER,PARAMETER :: analytictestopt_   = 0001106340
INTEGER,PARAMETER :: cellfunction_      = 0000421224
INTEGER,PARAMETER :: nonlinearfixup_    = 0001010587


!!#### ACCESS
PUBLIC :: SWITCHBOARD_QuasiDiffusion

CONTAINS

SUBROUTINE SWITCHBOARD_QuasiDiffusion( sio , FdBk )
!!#### PURPOSE
!! The command switchboard for the QuasiDiffusion package.

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object [sio]
TYPE(TYPE_sio),POINTER :: sio

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object [FdBk]
TYPE(TYPE_FdBk),OPTIONAL,INTENT(INOUT) :: FdBk

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_ = "SWITCHBOARD_QuasiDiffusion"

!!#### LOCAL VARIABLES
TYPE(varying_string) :: VS

!!--begin--
!pick routine to execute which loads some data
SELECT CASE(sio%HASH(2))
 CASE(system_)         ; CALL QDFsystem        ( sio , System , FdBk )
 CASE(solver_)         ; CALL QDFsolver        ( sio , Solver , SubSolver , FdBk )
 CASE(interactive_)    ; CALL QDFinteractive   ( sio , Interactive , FdBk )
 CASE(diffusionits_)   ; CALL QDFdiffusionits  ( sio , DiffusionOverride , FdbK )
 CASE(eoverride_)      ; CALL QDFeoverride     ( sio , EOverride , EOverrideVals , FdbK )
 CASE(coverride_)      ; CALL QDFcoverride     ( sio , COverride , COverrideVals , FdbK )
 CASE(noisy_)          ; CALL QDFnoisy         ( sio , Noisy , Unit_noisy , File_noisy , FdBk )
 CASE(reorder_)        ; CALL QDFreorder       ( sio , ReorderStrategies , FdBk )
 CASE(autoiterfix_)    ; CALL QDFautoiterfix   ( sio , Using_AutoIterFix , FdBk )
 CASE(sparskit_)       ; CALL QDFsparskit      ( sio , SPARSKIT_lfil , SPARSKIT_droptol , &
                                                       SPARSKIT_nkryss , SPARSKIT_maxmatvec , &
                                                       SPARSKIT_niwk , fdbk )
 CASE(interfacetype_)     ; CALL QDFinterfacetype   ( sio , InterfaceType , fdbk , KEYS=QDF_KEY_InterfaceType )
 CASE(halfcellshape_)     ; CALL QDFhalfcellshape   ( sio , HalfCellShape , fdbk , KEYS=QDF_KEY_HalfCellShape )
 CASE(bc_)                ; CALL QDFbc              ( sio , Mesh , LOBC , FixBdryVals , FdBk )
 CASE(printmatrix_)       ; CALL QDFprintmatrix     ( sio , OutputCoefficientMatrix , FdBk )
 CASE(printresiduals_)    ; CALL QDFprintresiduals  ( sio , PrintResiduals , FdBk )

 CASE(dumptransportdata_) ; CALL QDFdumptransportdata( sio , DumpTransportData , FdBk )

 CASE(analytictestcase_)  ; CALL QDFanalytictestcase( sio , AnalyticTestCase , FdBk )
                            Using_AnalyticSource = AnalyticTestCase/=0
                            !WRITE(*,*)'AnalyticTestCase=',AnalyticTestCase       
 CASE(analytictestvals_)  ; CALL QDFanalytictestvals( sio  , alpha,C,phi0,Exx0,Exx1,Exx2,&
                                   Eyy0,Eyy1,Eyy2,Exy0,siga,sigt , fdbk )
 CASE(analytictestopt_)   ; CALL QDFanalytictestopt( sio  , EvaluationMethod_J , &
                                   EvaluationMethod_Phi , EvaluationMethod_Q , &
                                   EvaluationMethod_E , fdbk )

 CASE(cellfunction_)      ; CALL QDFcellfunction( sio , CellFunctionMethod , fdbk , &
                                                  KEYS=QDF_KEY_CellFunction )

 CASE(nonlinearfixup_)    ; CALL QDFnonlinearfixup( sio , NonlinearFixup , fdbk , &
                                                    KEYS=QDF_KEY_NonlinearFixup )
 CASE(maxit_)    ; CALL QDFmaxit( sio , MaxLOIterations , fdbk )

 CASE DEFAULT
   VS = COMMAND_NOT_RECOGNIZED(sio,mod_,proc_,"")
   CALL UPDATE(fdbk_error,fdbk,s=STR(VS))
   VS = ""

END SELECT

!!--end--
END SUBROUTINE



SUBROUTINE QDFnoisy( sio , Noisy , Unit_noisy , File_noisy , fdbk )
!!#### PURPOSE
!! The command DataBaSe for the MoCshort package.

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <sio>
TYPE(TYPE_sio),POINTER :: sio

!!#### REQUIRED INPUT/OUTPUT
LOGICAL     ,INTENT(INOUT) :: Noisy
INTEGER     ,INTENT(INOUT) :: Unit_noisy
CHARACTER(*),INTENT(INOUT) :: File_noisy

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <FdBk>
TYPE(TYPE_FdBk),OPTIONAL,INTENT(INOUT) :: FdBk

!!--begin--
CALL BEGIN_ARGUMENTS(sio,(/"switch",&
                           "file  "/),fdbk,optional=(/.false.,.true./))
CALL ARGUMENT(sio,noisy,fdbk)
CALL ARGUMENT(sio,File_noisy,fdbk,default="")
CALL END_ARGUMENTS(sio,fdbk)

IF( Reading(sio) )THEN
 IF( sio%present(2) )THEN
  Unit_noisy = NewFile(TRIM(File_noisy),STATUS="Replace")
 ELSE
  Unit_noisy = window_unit
 END IF
END IF

!!--end--
END SUBROUTINE



SUBROUTINE QDFsolver( sio , Solver , SubSolver , fdbk )
!!#### PURPOSE
!! The command DataBaSe for the MoCshort package.

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <sio>
TYPE(TYPE_sio),POINTER :: sio

!!#### REQUIRED INPUT/OUTPUT
INTEGER     ,INTENT(INOUT) :: Solver
INTEGER     ,INTENT(INOUT) :: SubSolver

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <FdBk>
TYPE(TYPE_FdBk),OPTIONAL,INTENT(INOUT) :: FdBk

!!--begin--
CALL BEGIN_ARGUMENTS(sio,(/"Solver   ",&
                           "SubSolver"/),fdbk)
CALL ARGUMENT(sio,Solver   ,fdbk,Keys=QDF_KEY_Solver,Default=01)
CALL ARGUMENT(sio,SubSolver,fdbk,Keys=QDF_KEY_SubSolver,Default=01)
CALL END_ARGUMENTS(sio,fdbk)

!!--end--
END SUBROUTINE






SUBROUTINE QDFreorder( sio , ReorderStrategies , fdbk )
!!#### PURPOSE
!! Defines the way to reorder the low-order
!! quasi-diffusion equations.  If multiple reordering
!! strategies are desired, they will be processed sequentially.


!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <sio>
!! * list of reordering strategies <ReorderStrategies>
TYPE(TYPE_sio),POINTER :: sio
INTEGER       ,POINTER :: ReorderStrategies(:)

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
INTEGER :: m

!!--begin--

!!if writing
!IF( Writing(sio) )THEN
!
! !kick out
! IF( .NOT.ASSOCIATED(ReorderStrategies) )RETURN
!
! !current index
! m = SIZE(ReorderStrategies)
!
!!if reading
!ELSE IF( Reading(sio) )THEN
!
! !reallocate
! CALL Reallocate( ReorderStrategies , dn=1 )
!
! !current index
! m = SIZE(ReorderStrategies)
!
!END IF


!ARGUMENTS
CALL BEGIN_ARGUMENTS( sio , (/"strategy"/)    , FdBk )
CALL ARGUMENT       ( sio , ReorderStrategies , FdBk , KEYS=QDF_KEY_ReorderStrategies )
CALL END_ARGUMENTS  ( sio                     , FdBk )


!!if writing
!IF( Writing(sio) )THEN
!
! !reallocate
! CALL Reallocate( ReorderStrategies , dn=-1 )
!
!END IF

!!--end--
END SUBROUTINE


SUBROUTINE QDFsparskit( sio , SPARSKIT_lfil , SPARSKIT_droptol , &
  SPARSKIT_nkryss , SPARSKIT_maxmatvec , &
  SPARSKIT_niwk , fdbk )

!!#### PURPOSE
!! Options for the sparskit package ILUT preconditioned iterative
!! solver.


!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <sio>
TYPE(TYPE_sio),POINTER       :: sio
INTEGER       ,INTENT(INOUT) :: SPARSKIT_lfil
REAL(KIND_QDF),INTENT(INOUT) :: SPARSKIT_droptol
INTEGER       ,INTENT(INOUT) :: SPARSKIT_nkryss
INTEGER       ,INTENT(INOUT) :: SPARSKIT_maxmatvec
INTEGER       ,INTENT(INOUT) :: SPARSKIT_niwk

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!--begin--
CALL BEGIN_ARGUMENTS( sio , (/"lfil     ",&
                              "droptol  ",&
                              "nkryss   ",&
                              "maxmatvec",&
                              "niwk     ",&
                              "StdScale ",&
                              "PltSps   ",&
                              "PltPrcSps",&
                              "EvalCond ",&
                              "RowScale ",&
                              "ColScale ",&
                              "RowNorm  ",&
                              "ColNorm  ",&
                              "PrecSpec "/) , fdbk , &
                             optional=(/.true.,&
                                        .true. ,&
                                        .true. ,&
                                        .true. ,&
                                        .true. ,&
                                        .true. ,&
                                        .true. ,&
                                        .true. ,&
                                        .true. ,&
                                        .true. ,&
                                        .true. ,&
                                        .true. ,&
                                        .true. ,&
                                        .true. ,&
                                        .true. ,&
                                        .true. ,&
                                        .true. /) )
CALL ARGUMENT( sio , SPARSKIT_lfil      , FdBk , Default=ERROR(SPARSKIT_lfil) )
CALL ARGUMENT( sio , SPARSKIT_droptol   , FdBk , Default=ERROR(SPARSKIT_droptol) )
CALL ARGUMENT( sio , SPARSKIT_nkryss    , FdBk , Default=ERROR(SPARSKIT_nkryss) )
CALL ARGUMENT( sio , SPARSKIT_maxmatvec , FdBk , Default=ERROR(SPARSKIT_maxmatvec) )
CALL ARGUMENT( sio , SPARSKIT_niwk      , FdBk , Default=ERROR(SPARSKIT_niwk) )

CALL ARGUMENT( sio , StandardScaling    , FdBk , Default=StandardScaling )
CALL ARGUMENT( sio , PlotSparsityPattern, FdBk , Default=PlotSparsityPattern )
CALL ARGUMENT( sio , PlotPrecSparsityPattern , FdBk , Default=PlotPrecSparsityPattern )
CALL ARGUMENT( sio , EvaluateConditionNumber      , FdBk , Default=EvaluateConditionNumber )
CALL ARGUMENT( sio , Use_RowScaling      , FdBk , Default=Use_RowScaling )
CALL ARGUMENT( sio , Use_ColScaling      , FdBk , Default=Use_ColScaling )
CALL ARGUMENT( sio , rownorm      , FdBk , Default=rownorm )
CALL ARGUMENT( sio , colnorm      , FdBk , Default=colnorm )
CALL ARGUMENT( sio , PrecSpec      , FdBk , Default=PrecSpec )

CALL END_ARGUMENTS( sio , fdbk )

!!--end--
END SUBROUTINE




SUBROUTINE QDFdiffusionits( sio  , DiffusionOverride , fdbk )
!!#### PURPOSE
!! Allows user to choose the iterations on which to do
!! diffusion calcs.

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <sio>
!! * whether or not to do diffusion for the first iteration,
!!   and all subsequent iterations
TYPE(TYPE_sio),POINTER :: sio
LOGICAL       ,INTENT(INOUT) :: DiffusionOverride(2)


!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!--begin--

CALL BEGIN_ARGUMENTS( sio , (/"DiffOverride"/) , FdBk , Optional=(/.FALSE./) , NSubArg=(/2/) )
CALL ARGUMENT       ( sio , DiffusionOverride , FdBk )
CALL END_ARGUMENTS  ( sio , FdBk )

!!--end--
END SUBROUTINE


SUBROUTINE QDFeoverride( sio  , EOverride , EOverrideVals , fdbk )
!!#### PURPOSE
!! Allows user to choose whether to override the eddington
!! factors with some constant ones.

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <sio>
!! * whether or not to use the eddingtons for the first iteration,
!!   and all subsequent iterations
TYPE(TYPE_sio),POINTER :: sio
LOGICAL       ,INTENT(INOUT) :: EOverride    (2)
REAL(KIND_QDF),INTENT(INOUT) :: EOverrideVals(3)


!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!--begin--

CALL BEGIN_ARGUMENTS( sio , (/"EOverride    ","EOverrideVals"/) , FdBk , &
  Optional=(/.FALSE.,.FALSE./) , NSubArg=(/2,3/) )
CALL ARGUMENT       ( sio , EOverride , FdBk )
CALL ARGUMENT       ( sio , EOverrideVals , FdBk )
CALL END_ARGUMENTS  ( sio , FdBk )

!!--end--
END SUBROUTINE



SUBROUTINE QDFanalytictestvals( sio  , alpha,C,phi0,Exx0,Exx1,Exx2,&
  Eyy0,Eyy1,Eyy2,Exy0,siga,sigt , fdbk )

!!#### PURPOSE
!! Allows user to choose analytic test values.

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <sio>
TYPE(TYPE_sio),POINTER :: sio
REAL(KIND_QDF),INTENT(INOUT) :: alpha,C,phi0
REAL(KIND_QDF),INTENT(INOUT) :: Exx0,Exx1,Exx2
REAL(KIND_QDF),INTENT(INOUT) :: Eyy0,Eyy1,Eyy2
REAL(KIND_QDF),INTENT(INOUT) :: Exy0
REAL(KIND_QDF),INTENT(INOUT) :: siga,sigt

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
REAL(KIND_QDF) :: Exx(3),Eyy(3),Exy(1)

!!--begin--

!set arrays from E's
Exx(1) = Exx0 ; Exx(2) = Exx1 ; Exx(3) = Exx2
Eyy(1) = Eyy0 ; Eyy(2) = Eyy1 ; Eyy(3) = Eyy2
Exy(1) = Exy0

CALL BEGIN_ARGUMENTS( sio , (/"phi0 ","C    ","alpha",&
  "Exx  ","Eyy  ","Exy  ","siga ","sigt "/) , FdBk , &
  Optional=(/.TRUE.,.TRUE.,.TRUE.,.TRUE.,.TRUE.,.TRUE.,.TRUE.,.TRUE./) , NSubArg=(/0,0,0,&
  3,3,1,0,0/) )
CALL ARGUMENT       ( sio , phi0  , FdBk , default=phi0)
CALL ARGUMENT       ( sio , C     , FdBk , default=C)
CALL ARGUMENT       ( sio , alpha , FdBk , default=alpha)
CALL ARGUMENT       ( sio , Exx   , FdBk , default=Exx)
CALL ARGUMENT       ( sio , Eyy   , FdBk , default=Eyy)
CALL ARGUMENT       ( sio , Exy   , FdBk , default=Exy)
CALL ARGUMENT       ( sio , siga  , FdBk , default=siga)
CALL ARGUMENT       ( sio , sigt  , FdBk , default=sigt)
CALL END_ARGUMENTS  ( sio , FdBk )

!set E's from returned vectors
Exx0 = Exx(1) ; Exx1 = Exx(2) ; Exx2 = Exx(3)
Eyy0 = Eyy(1) ; Eyy1 = Eyy(2) ; Eyy2 = Eyy(3)
Exy0 = Exy(1)

!!--end--
END SUBROUTINE



SUBROUTINE QDFanalytictestopt( sio  , EvaluationMethod_J , &
  EvaluationMethod_Phi , EvaluationMethod_Q , &
  EvaluationMethod_E , fdbk )

!!#### PURPOSE
!! Allows user to choose options how face-average and cell-average values
!! are evaluated.

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <sio>
TYPE(TYPE_sio),POINTER       :: sio
INTEGER       ,INTENT(INOUT) :: EvaluationMethod_J
INTEGER       ,INTENT(INOUT) :: EvaluationMethod_Phi
INTEGER       ,INTENT(INOUT) :: EvaluationMethod_Q
INTEGER       ,INTENT(INOUT) :: EvaluationMethod_E

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk


!!--begin--

CALL BEGIN_ARGUMENTS( sio , (/"EvalJ  ","EvalPhi","EvalQ  ","EvalE  "/) , FdBk , &
  Optional=(/.TRUE.,.TRUE.,.TRUE.,.TRUE./) , NSubArg=(/0,0,0,0/) )

CALL ARGUMENT       ( sio , EvaluationMethod_J   , FdBk , &
  default=EvaluationMethod_J   , Keys=KEY_EvaluationMethod )

CALL ARGUMENT       ( sio , EvaluationMethod_Phi , FdBk , &
  default=EvaluationMethod_Phi , Keys=KEY_EvaluationMethod )

CALL ARGUMENT       ( sio , EvaluationMethod_Q   , FdBk , &
  default=EvaluationMethod_Q   , Keys=KEY_EvaluationMethod )

CALL ARGUMENT       ( sio , EvaluationMethod_E   , FdBk , &
  default=EvaluationMethod_E   , Keys=KEY_EvaluationMethod )

CALL END_ARGUMENTS  ( sio , FdBk )

!!--end--
END SUBROUTINE




SUBROUTINE QDFcoverride( sio  , COverride , COverrideVals , fdbk )
!!#### PURPOSE
!! Allows user to choose whether to override the boundary
!! factors with some constant ones.

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <sio>
!! * whether or not to use the boundary factors for the first iteration,
!!   and all subsequent iterations
TYPE(TYPE_sio),POINTER :: sio
LOGICAL       ,INTENT(INOUT) :: COverride    (2)
REAL(KIND_QDF),INTENT(INOUT) :: COverrideVals


!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!--begin--

CALL BEGIN_ARGUMENTS( sio , (/"COverride    ","COverrideVals"/) , FdBk , &
  Optional=(/.FALSE.,.FALSE./) , NSubArg=(/2,0/) )
CALL ARGUMENT       ( sio , COverride , FdBk )
CALL ARGUMENT       ( sio , COverrideVals , FdBk )
CALL END_ARGUMENTS  ( sio , FdBk )

!!--end--
END SUBROUTINE



!!### CARD SUBROUTINE: \<QDFbc\>
SUBROUTINE QDFbc( sio , Mesh , LOBC , FixBdryVals , FdBk )

!!#### PURPOSE
!! Read in boundary condition information for the domain.

!!#### REQUIRED INPUT/OUTPUT
!! * boundary conditions for low order \<LOBC\>
!! * values for boundary factors at each boundary \<FixBdryVals\>
TYPE(TYPE_sio),POINTER :: sio
INTEGER       ,POINTER :: LOBC(:)
REAL(KIND_MCs),POINTER :: FixBdryVals(:)

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_FdBk),INTENT(INOUT),OPTIONAL :: FdBk

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_ = "QDFbc"

!!#### LOCAL VARIABLES
INTEGER              :: i
TYPE(varying_string) :: VS
REAL(KIND_QDF) :: fixedval(1)

!!--begin--

IF( IsQr(Mesh%domain%DomainShape) )THEN
 !! Setup.
 IF( Reading(sio) )ALLOCATE( LOBC(1:6) )

 !! Arguments.
 CALL BEGIN_ARGUMENTS( sio , (/"b-r-t-l","u-d    "/) , FdBk ,&
   nsubarg=(/4,2/) ,&
   optional=(/.FALSE.,.TRUE./))
 CALL ARGUMENT( sio , LOBC(1:4) , FdBk , Keys=QDF_KEY_LOBC )
 CALL ARGUMENT( sio , LOBC(5:6) , FdBk , Keys=QDF_KEY_LOBC , Default=(/1,1/) )
 CALL END_ARGUMENTS( sio , FdBk )

 !! Conditional Datablock.
 ALLOCATE( FixBdryVals(4) )
 CALL CLEAR(FixBdryVals)
 DO i=1,4
  IF( LOBC(i)==QDF_fixedJ .OR. LOBC(i)==QDF_fixedPhi )THEN
   CALL DATABLOCK(sio,fixedval,FdBk)
   FixBdryVals(i) = fixedval(1)
  END IF
 END DO

 !! Wrapup.
 IF( Writing(sio) )DEALLOCATE( LOBC )

ELSE
 VS = MODPROC(mod_,proc_)//"the boundary conditions are only &
   &available for a rectangular domain!"
 CALL UPDATE(fdbk_error,fdbk,s=STR(VS))
 VS = ""
END IF

!!--end--
END SUBROUTINE



END MODULE
