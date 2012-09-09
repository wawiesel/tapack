!!# COMMAND CARD SWITCHBOARD <<CCS_MoCshort>>
MODULE CCS_MoCshort

!!## PURPOSE
!! This is the command card switchboard for the
!! <MoCshort> (<MCS>) package.

!!## CHANGES
!! * v2.23 - ability to read in restart files, detailed angular flux at a given vert

!!## PARAMETER MODULES
USE PAR_MoCshort                                                  !!((03-A-PAR_MoCshort.f90))

!!## USER MODULES
USE USR_fdbk                                                      !!((08-C-USR_fdbk.f90))
USE USR_SBCharacteristics,ONLY: CWENO_eps,CWENO_order,CWENO_cwt,& !!((26-C-USR_SBCharacteristics.f90))
                                EdgeInterpolator,KEYS_EdgeInterpolator,&
                                KIND_qmc
USE USR_FunctionParser                                            !!((05-B-USR_FunctionParser.f90))
USE USR_TransportAnalyticTest                                     !!((56-C-USR_TransportAnalyticTest.f90))
USE USR_TransportAnalyticTest_WIESEL,ONLY: &
ANALYTIC_A,ANALYTIC_C,&
ANALYTIC_D1,ANALYTIC_DX,ANALYTIC_DY,&
ANALYTIC_BX,ANALYTIC_BY


!!## PROCEDURE MODULES
USE SUB_Pause                                                     !!((04-B-SUB_Pause.f90))
USE FUN_STR                                                       !!((05-B-FUN_STR.f90))
USE FUN_VSTR                                                      !!((05-B-FUN_VSTR.f90))
USE FUN_NewFile                                                   !!((05-B-FUN_NewFile.f90))
USE SUB_CLEAR                                                     !!((04-A-SUB_CLEAR.f90))

USE VAR_TAPACK                                                    !!((66-C-VAR_TAPACK.f90))

!!## CARD MODULES
USE CC1_simple,ONLY: &                                            !!((11-A-CC1_simple.f90))
     MCSorder       =>simple , &
     MCSonlygeometry=>simple , &
     MCSinteractive =>simple , &
     MCSmonotonic   =>simple , &
     MCSmonolin     =>simple , &
     MCSsourceorder =>simple , &
     MCSsplitting   =>simple , &
     MCSinterpplane =>simple , &
     MCSlogtransform=>simple , &
     MCSjiggle      =>simple , &
     MCSnobacksies  =>simple , &
     MCSlongchar    =>simple , &
     MCSsbalance    =>simple , &
     MCSpackedcache =>simple , &
     MCSmingamma    =>simple , &
     MCSsbedgeinterp=>simple , &
     MCScellfunction=>simple , &
     MCScache       =>simple , &
     MCSrestart     =>simple , & 
     MCSdiscorner   =>simple, &
     MCSnonlinearfixup=>simple

!!## LIBRARY MODULES
USE LIB_Prompts                                                   !!((06-B-LIB_Prompts.f90))
USE LIB_GenericPhrases                                            !!((07-B-LIB_GenericPhrases.f90))


!!## TOOLBOXES
USE TBX_SIO                                                       !!((10-A-TBX_SIO.f90))
USE TBX_Mesh                                                      !!((15-B-TBX_Mesh.f90))


!!## VARIABLE MODULES
USE VAR_MoCshort                                                  !!((47-B-VAR_MoCshort.f90))
USE VAR_Mesh                                                      !!((46-B-VAR_Mesh.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE


!!## DEFAULT ACCESS
PRIVATE


!!## IDENTIFICATION
CHARACTER(*),PARAMETER :: mod_  = "CCS_MoCshort"
CHARACTER(*),PARAMETER :: file_ = "38-B-CCS_MoCshort.f90"


!!## PRE-CALCULATED COMMAND HASHES
INTEGER,PARAMETER :: bc_           = 0000000485
INTEGER,PARAMETER :: interactive_  = 0000386226
INTEGER,PARAMETER :: onlygeometry_ = 0000621844
INTEGER,PARAMETER :: order_        = 0000025877
INTEGER,PARAMETER :: sourceorder_  = 0000486944
INTEGER,PARAMETER :: monotonic_    = 0000213811
INTEGER,PARAMETER :: monolin_      = 0000083170
INTEGER,PARAMETER :: splitting_    = 0000226511
INTEGER,PARAMETER :: interpplane_  = 0000408702
INTEGER,PARAMETER :: explodefix_   = 0000293953
INTEGER,PARAMETER :: jiggle_       = 0000036443
INTEGER,PARAMETER :: logtransform_ = 0000571432
INTEGER,PARAMETER :: nobacksies_   = 0000245457
INTEGER,PARAMETER :: longchar_     = 0000122396
INTEGER,PARAMETER :: testlinsrc_   = 0000315033
INTEGER,PARAMETER :: checksym_     = 0000078274
INTEGER,PARAMETER :: sbalance_     = 0000104524
INTEGER,PARAMETER :: packedcache_  = 0000298670
INTEGER,PARAMETER :: mingamma_     = 0000113356
INTEGER,PARAMETER :: sbopts_       = 0000046721
INTEGER,PARAMETER :: sbedgeinterp_ = 0000442558
INTEGER,PARAMETER :: longpointlist_= 0000781759
INTEGER,PARAMETER :: cellfunction_ = 0000421224
INTEGER,PARAMETER :: nobalcheck_   = 0000245505
INTEGER,PARAMETER :: analyticwarsa_= 0000627403
INTEGER,PARAMETER :: rayinfo_      = 0000082732
INTEGER,PARAMETER :: analyticwiesel_=0000843363
INTEGER,PARAMETER :: cache_         =0000011111
INTEGER,PARAMETER :: options_       =0000091914
INTEGER,PARAMETER :: restart_       =0000082998
INTEGER,PARAMETER :: debugpsiv_     =0000131963
INTEGER,PARAMETER :: discorner_     =0000155473
INTEGER,PARAMETER :: nonlinearfixup_=0001010587

!!## ACCESS
PUBLIC :: SWITCHBOARD_MoCshort


!!## MODULE PROCEDURES
CONTAINS


!!### SWITCHBOARD SUBROUTINE: <SWITCHBOARD_MoCshort>
SUBROUTINE SWITCHBOARD_MoCshort( sio , Mesh , fdbk )

!!#### PURPOSE
!! The command card switchboard for the MoCshort package.

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <sio>
TYPE(TYPE_sio),POINTER :: sio

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_="SWITCHBOARD_MoCshort"

!!#### LOCAL VARIABLES
TYPE(varying_string) :: VS

!!--begin--

!pick routine to execute which loads some data
SELECTCASE(sio%HASH(2))

 CASE(interactive_)     ; CALL MCSinteractive ( sio , InteractiveAngularflux , fdbk )
 CASE(order_)           ; CALL MCSorder       ( sio , InterpOrder , fdbk )
 CASE(sourceorder_)     ; CALL MCSsourceorder ( sio , SourceOrder , fdbk )
 CASE(monolin_)         ; CALL MCSmonolin     ( sio , Using_MonoLin , fdbk )
 CASE(splitting_)       ; CALL MCSsplitting   ( sio , Using_Splitting , fdbk )
 CASE(onlygeometry_)    ; CALL MCSonlygeometry( sio , OnlyGeometry , fdbk )
 CASE(bc_)              ; CALL MCSbc          ( sio , Mesh , BC , FixedAngularFlux , fdbk )
 CASE(monotonic_)       ; CALL MCSmonotonic   ( sio , Using_Monotonization , fdbk )
 CASE(interpplane_)     ; CALL MCSinterpplane ( sio , InterpPlaneU , fdbk ,KEYS=KEY_InterpPlane)
 CASE(logtransform_)    ; CALL MCSlogtransform( sio , Using_LogTransform , fdbk )
 CASE(jiggle_)          ; CALL MCSjiggle      ( sio , Using_Jiggle , fdbk )
 CASE(nobacksies_)      ; CALL MCSnobacksies  ( sio , Using_NoBacksies , fdbk )
 CASE(explodefix_)      ; CALL MCSexplodefix  ( sio , Using_ExplodeFix , MIN_AngularFlux , MAX_AngularFlux , fdbk )
 CASE(longchar_)        ; CALL MCSlongchar    ( sio , Using_LongCharacteristics , fdbk )
 CASE(testlinsrc_)      ; CALL MCStestlinsrc  ( sio , Using_LinearSourceTest , Unit_LinearSourceTest , fdbk )
 CASE(checksym_)        ; CALL MCSchecksym    ( sio , Mesh%Ndim , P1sym , P2sym , Unit_AFSymmetryCheck , fdbk )
 CASE(sbalance_)        ; CALL MCSsbalance    ( sio , Using_SBCharacteristics , fdbk )
 CASE(packedcache_)     ; CALL MCSpackedcache ( sio , Using_PackedCaching , fdbk )
 CASE(sbedgeinterp_)    ; CALL MCSsbedgeinterp( sio , EdgeInterpolator,fdbk,KEYS=KEYS_EdgeInterpolator)
 CASE(sbopts_)          ; CALL MCSsbopts      ( sio , CWENO_cwt, CWENO_order,CWENO_eps,fdbk)
 CASE(longpointlist_)   ; CALL MCSlongpointlist( sio , Mesh%NDim , Unit_LongChar , PointList_LongChar , fdbk )
 CASE(cellfunction_)    ; CALL MCScellfunction( sio , CellFunctionMethod , fdbk , KEYS=MCS_KEY_CellFunction )
 CASE(nobalcheck_)      ; CALL MCSnobalcheck  ( sio , CHECKING_BALANCE , NO_BALANCE_MSG , fdbk )
 CASE(analyticwarsa_)   ; CALL MCSanalyticwarsa(sio , Using_AnalyticTransportTest , fdbk )
 CASE(rayinfo_)         ; CALL MCSrayinfo( sio , Print_RayEffectsInfo , rayeffectsfile , fdbk )
 CASE(analyticwiesel_)  ; CALL MCSanalyticwiesel(sio , Using_AnalyticTransportTest , fdbk )
 CASE(cache_)           ; CALL MCScache(sio,Using_Cache,fdbk)
 CASE(options_)         ; CALL MCSoptions(sio,Using_AF_ScaleFactor,fdbk)
 CASE(restart_)         ; CALL MCSrestart(sio,affile,fdbk)
 CASE(debugpsiv_)       ; CALL MCSdebugpsiv(sio,Mesh%Ndim,Unit_DebugPsiV , PointList_DebugPsiV,fdbk)
 CASE(discorner_)       ; CALL MCSdiscorner(sio,Allow_Discontinuous_Corners,fdbk)
 CASE(nonlinearfixup_)  ; CALL MCSnonlinearfixup( sio , NonlinearFixup , fdbk , &
                                                    KEYS=MCS_KEY_NonlinearFixup )
 CASE DEFAULT
   VS = COMMAND_NOT_RECOGNIZED(sio,mod_,proc_,"")
   CALL UPDATE(fdbk_error,fdbk,s=STR(VS))
   VS = ""
ENDSELECT

!!--end--
ENDSUBROUTINE


!!### CARD SUBROUTINE <<MCSnobalcheck>>
SUBROUTINE MCSnobalcheck( sio , CHECKING_BALANCE , NO_BALANCE_MSG , fdbk )
!!#### PURPOSE
!! Turn off balance equation checkings.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_sio)           ,POINTER       :: sio
LOGICAL                  ,INTENT(INOUT) :: CHECKING_BALANCE
TYPE(varying_string)               :: NO_BALANCE_MSG

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_ = "MCSnobalcheck"

!!#### LOCAL VARIABLES
TYPE(varying_string) :: file
LOGICAL :: NoBalance
!!--begin--

IF( Writing(sio) )THEN
 NoBalance = .NOT.CHECKING_BALANCE
END IF

!! Arguments.
CALL BEGIN_ARGUMENTS( sio , (/"NoBalance"   ,&
                              "MSG      "/) , fdbk ,&
  nsubarg =(/0      ,0     /) , &
  optional=(/.FALSE.,.TRUE./) )
CALL ARGUMENT( sio , NoBalance , fdbk )
CALL ARGUMENT( sio , NO_BALANCE_MSG , fdbk , Default=VSTR("") )
CALL END_ARGUMENTS( sio , fdbk )

IF( Reading(sio) )THEN
 CHECKING_BALANCE = .NOT.NoBalance
END IF

!!--end--
END SUBROUTINE


!!### CARD SUBROUTINE <<MCSoptions>>
SUBROUTINE MCSoptions( sio , &
  Using_AF_ScaleFactor , fdbk )
!!#### PURPOSE
!! General option passer

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_sio)           ,POINTER       :: sio
LOGICAL                  ,INTENT(INOUT) :: Using_AF_ScaleFactor

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_ = "MCSoptions"

!!#### LOCAL VARIABLES
TYPE(varying_string) :: file

!!--begin--

!! Arguments.
CALL BEGIN_ARGUMENTS( sio , (/"Using_AF_ScaleFactor"/) , fdbk ,&
  nsubarg =(/0/) , &
  optional=(/.TRUE./) )
CALL ARGUMENT( sio , Using_AF_ScaleFactor , fdbk, Default=Using_AF_ScaleFactor )
CALL END_ARGUMENTS( sio , fdbk )

!!--end--
END SUBROUTINE MCSoptions


!!### CARD SUBROUTINE: <MCSbc>
SUBROUTINE MCSbc( sio , Mesh , BC , FixedAngularFlux , fdbk )

!!#### PURPOSE
!! Read in boundary condition information for the domain.

!!#### REQUIRED INPUT/OUTPUT
!! * boundary conditions <BC>
!! * values for fixed angular fluxes at each boundary <FixedAngularFlux>
TYPE(TYPE_sio),POINTER :: sio
INTEGER       ,POINTER :: BC(:)
REAL(KIND_MCs),POINTER :: FixedAngularFlux(:)

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_ = "MCSbc"

!!#### LOCAL VARIABLES
INTEGER              :: i
TYPE(varying_string) :: VS
REAL(KIND_MCs)       :: fixedval(1)
CHARACTER(255)       :: fixedstr(1),error1
TYPE(varying_string) :: varstr(1)

!!--begin--

IF( IsQr(Mesh%domain%DomainShape) )THEN
 !! Setup.
 IF( Reading(sio) )ALLOCATE( BC(1:6) )

 !! Arguments.
 CALL BEGIN_ARGUMENTS( sio , (/"b-r-t-l","u-d    "/) , fdbk ,&
   nsubarg=(/4,2/) ,&
   optional=(/.FALSE.,.TRUE./))
 CALL ARGUMENT( sio , BC(1:4) , fdbk , Keys=KEY_BC )
 CALL ARGUMENT( sio , BC(5:6) , fdbk , Keys=KEY_BC , Default=(/1,1/) )
 CALL END_ARGUMENTS( sio , fdbk )

 !! Conditional Datablock.
 ALLOCATE( FixedAngularFlux(4) , FunctionAngularFlux(4) )
 CALL CLEAR(FixedAngularFlux)
 CALL CLEAR(FunctionAngularFlux)

 !! Go through the domain boundaries.
 DO i=1,4

  !boundary conditions are fixed
  IF( BC(i)==fixed_ .OR. BC(i)==planewave_ )THEN
   CALL DATABLOCK(sio,fixedval,fdbk)
   FixedAngularFlux(i) = fixedval(1)

  !a function is provided to evaluate boundary conditions
  ELSE IF( BC(i)==function_ )THEN
   CALL DATABLOCK(sio,varstr,fdbk)
   fixedstr(1)=varstr(1)
   call s_createfn(fixedstr(1), 'x y z ox oy oz m', &
     FunctionAngularFlux(i), error1)
   IF( TRIM(error1)/="OK" )THEN
    VS = MODPROC(mod_,proc_)//"the submitted function <"//&
      TRIM(fixedstr(1))//"> produced the error message "//&
      "<"//TRIM(error1)//">."
    CALL UPDATE(fdbk_error,fdbk,s=STR(VS))
   END IF
  END IF

 END DO

 !! Wrapup.
 IF( Writing(sio) )DEALLOCATE( BC )

ELSE
 VS = MODPROC(mod_,proc_)//"the boundary conditions are only &
   &available for a rectangular domain!"
 CALL UPDATE(fdbk_error,fdbk,s=STR(VS))
 VS = ""
END IF

!!--end--
END SUBROUTINE


!!### CARD SUBROUTINE: <MCSexplodefix>
SUBROUTINE MCSexplodefix( sio , Using_ExplodeFix , MIN_AngularFlux , MAX_AngularFlux , fdbk )

!!#### PURPOSE
!! Read/write angular flux limits (min/max) for any transport simulation.

!!#### REQUIRED INPUT/OUTPUT
!! * whether to use or not <Using_ExplodeFix>
!! * minimum and maximum for angular fluxes <MIN_AngularFlux,MAX_AngularFlux>
TYPE(TYPE_sio),POINTER       :: sio
LOGICAL       ,INTENT(INOUT) :: Using_ExplodeFix
REAL(KIND_MCs),INTENT(INOUT) :: MIN_AngularFlux,MAX_AngularFlux


!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_ = "MCSexplodefix"

!!#### LOCAL VARIABLES
REAL(KIND_MCs) :: l(2)

!!--begin--

!! Setup.
IF( Writing(sio) )THEN
 l(1) = MIN_AngularFlux
 l(2) = MAX_AngularFlux
END IF

!! Arguments.
CALL BEGIN_ARGUMENTS( sio , (/"Using_ExplodeFix ",&
                              "RANGE_AngularFlux"/) , fdbk ,&
  nsubarg=(/0,2/) ,&
  optional=(/.FALSE.,.FALSE./))
CALL ARGUMENT( sio , Using_ExplodeFix , fdbk )
CALL ARGUMENT( sio , l , fdbk )
CALL END_ARGUMENTS( sio , fdbk )

!! Wrapup.
IF( Reading(sio) )THEN
 MIN_AngularFlux = l(1)
 MAX_AngularFlux = l(2)
END IF

!!--end--
END SUBROUTINE



!!### CARD SUBROUTINE: <MCSsbopts>
SUBROUTINE MCSsbopts( sio , CWENO_cwt , CWENO_order , CWENO_eps , fdbk )

!!#### PURPOSE
!! Handle some options for the subcell balance routines.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_sio),POINTER       :: sio
REAL(KIND_qmc),INTENT(INOUT) :: CWENO_cwt
INTEGER       ,INTENT(INOUT) :: CWENO_order
REAL(KIND_qmc),INTENT(INOUT) :: CWENO_eps

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_ = "MCSsbopts"

!!#### LOCAL VARIABLES
REAL(KIND_MCs) :: l(2)

!!--begin--

!! Setup.

!! Arguments.
CALL BEGIN_ARGUMENTS( sio , (/"CWENO_cwt  ",&
                              "CWENO_order",&
                              "CWENO_eps  "/), fdbk ,&
                            nsubarg=(/0,0,0/) ,&
                            optional=(/.TRUE.,.TRUE.,.TRUE./) )
CALL ARGUMENT( sio , CWENO_cwt   , fdbk , Default=CWENO_cwt )
CALL ARGUMENT( sio , CWENO_order , fdbk , Default=CWENO_order )
CALL ARGUMENT( sio , CWENO_eps   , fdbk , Default=CWENO_eps )
CALL END_ARGUMENTS( sio , fdbk )

!! Wrapup.

!!--end--
END SUBROUTINE



!!### CARD SUBROUTINE: <MCStestlinsrc>
SUBROUTINE MCStestlinsrc( sio , Using_LinearSourceTest , Unit_LinearSourceTest , fdbk )

!!#### PURPOSE
!! Test a linear source (has specific requirements on cross sections, bc, etc.)
!! See <EXACT_Psi_LinSrc> for details.

!!#### REQUIRED INPUT/OUTPUT
!! * using a linear source test <Using_LinearSourceTest>
!! * the unit to write columns of data to <Unit_LinearSourceTest>
TYPE(TYPE_sio),POINTER       :: sio
LOGICAL       ,INTENT(INOUT) :: Using_LinearSourceTest
INTEGER       ,INTENT(INOUT) :: Unit_LinearSourceTest


!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_ = "MCStestlinsrc"

!!#### LOCAL VARIABLES
TYPE(varying_string) :: file

!!--begin--

!! Setup.
!IF( Writing(sio) )THEN
!END IF

!! Arguments.
CALL BEGIN_ARGUMENTS( sio , (/"UseLinSrc",&
                              "file     "/) , fdbk ,&
  nsubarg=(/0,0/) ,&
  optional=(/.FALSE.,.TRUE./))
CALL ARGUMENT( sio , Using_LinearSourceTest , fdbk )
CALL ARGUMENT( sio , file , fdbk , Default=VSTR("") )
CALL END_ARGUMENTS( sio , fdbk )

!! Wrapup.
IF( Reading(sio) )THEN
 IF( file/="" )THEN
  Unit_LinearSourceTest = NewFile(file)
 ELSE
  Unit_LinearSourceTest = 0
 END IF
END IF

!!--end--
END SUBROUTINE


!!### CARD SUBROUTINE <<MCSchecksym>>
SUBROUTINE MCSchecksym( sio , NDim , P1sym , P2sym , Unit_AFSymmetryCheck , fdbk )

!!#### PURPOSE
!! Test for symmetry in the angular flux.

!!#### REQUIRED INPUT/OUTPUT
!! * number of dimensions <NDim>
!! * first point to define symmetry plane <P1sym>
!! * second point to define symmetry plane <P2sym>
!! * the unit to write to <Unit_AFSymmetryCheck>
INTEGER       ,INTENT(IN)    :: NDim
TYPE(TYPE_sio),POINTER       :: sio
REAL(KIND_MSH),INTENT(INOUT) :: P1sym(NDim),P2sym(NDim)
INTEGER       ,INTENT(INOUT) :: Unit_AFSymmetryCheck


!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_ = "MCSchecksym"

!!#### LOCAL VARIABLES
TYPE(varying_string) :: file

!!--begin--

!! Setup.
!IF( Writing(sio) )THEN
!END IF

!! Arguments.
CALL BEGIN_ARGUMENTS( sio , (/"P1sym","P2sym","file "/) , fdbk ,&
  nsubarg=(/NDim,NDim,0/) ,&
  optional=(/.FALSE.,.FALSE.,.TRUE./))
CALL ARGUMENT( sio , P1sym , fdbk )
CALL ARGUMENT( sio , P2sym , fdbk )
CALL ARGUMENT( sio , file , fdbk , Default=VSTR("") )
CALL END_ARGUMENTS( sio , fdbk )

!! Wrapup.
IF( Reading(sio) )THEN
 Using_AFSymmetryCheck = .TRUE.
 IF( file/="" )THEN
  Unit_AFSymmetryCheck = NewFile(file)
 ELSE
  Unit_AFSymmetryCheck = 0
 END IF
END IF

!!--end--
END SUBROUTINE


!!### CARD SUBROUTINE <<MCSanalyticwarsa>>
SUBROUTINE MCSanalyticwarsa( sio , USING_Test , fdbk )

!!#### PURPOSE
!! Read in parameters for an analytic test devised by Jim Warsa in a 2008 Nuclear
!! Science and Engineering Technical Note (160, 385-400).

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_sio),POINTER       :: sio
LOGICAL       ,INTENT(INOUT) :: USING_Test

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_ = "MCSanalyticwarsa"
REAL(KIND_MCS) :: sigt,sigs

!!--begin--

!! Arguments.
CALL BEGIN_ARGUMENTS( sio , (/"Test   ",&
                              "sigmat ",&
                              "sigmas ",&
                              "SpatRep"/) , fdbk ,&
                            nsubarg=(/0,0,0,0/) ,&
                           optional=(/.FALSE.,.FALSE.,.FALSE.,.FALSE./))
CALL ARGUMENT( sio , USING_Test , fdbk )
CALL ARGUMENT( sio , sigt , fdbk )
CALL ARGUMENT( sio , sigs , fdbk )
CALL ARGUMENT( sio , CellFunctionMethodA , fdbk , KEYS=MCS_KEY_CellFunction , Default=MCS_LINEAR_GAUSS)
CALL END_ARGUMENTS( sio , fdbk )

IF( Using_Test )THEN
 CALL SET_AnalyticTest("WARSA",sigt,sigs,fdbk)
END IF

!!--end--
END SUBROUTINE


!!### CARD SUBROUTINE <<MCSanalyticwiesel>>
SUBROUTINE MCSanalyticwiesel( sio , USING_Test , &
  fdbk )

!!#### PURPOSE
!! Read in parameters for an analytic test devised by
!! William Wieselquist, William.wieselquist AT gmail.com.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_sio),POINTER       :: sio
LOGICAL       ,INTENT(INOUT) :: USING_Test


!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_ = "MCSanalyticwiesel"
REAL(KIND_MCS) :: sigt,sigs

!!--begin--

!! Arguments.
CALL BEGIN_ARGUMENTS( sio , (/"Test   ","sigmat ","sigmas ","SpatRep",&
"C","A","BX","BY","D1","DX","DY"/) , fdbk ,&
  nsubarg=(/0,0,0,0,&
            0,0,0,0,0,0,0/) ,&
  optional=(/.FALSE.,.FALSE.,.FALSE.,.FALSE.,&
.TRUE.,.TRUE.,.TRUE.,.TRUE.,.TRUE.,.TRUE.,.TRUE./))
CALL ARGUMENT( sio , USING_Test , fdbk )
CALL ARGUMENT( sio , sigt , fdbk )
CALL ARGUMENT( sio , sigs , fdbk )
CALL ARGUMENT( sio , CellFunctionMethodA , fdbk , KEYS=MCS_KEY_CellFunction , Default=MCS_LINEAR_GAUSS)
CALL ARGUMENT( sio , ANALYTIC_C, fdbk, Default=ANALYTIC_C)
CALL ARGUMENT( sio , ANALYTIC_A, fdbk, Default=ANALYTIC_A)
CALL ARGUMENT( sio , ANALYTIC_BX, fdbk, Default=ANALYTIC_BX)
CALL ARGUMENT( sio , ANALYTIC_BY, fdbk, Default=ANALYTIC_BY)
CALL ARGUMENT( sio , ANALYTIC_D1, fdbk, Default=ANALYTIC_D1)
CALL ARGUMENT( sio , ANALYTIC_DX, fdbk, Default=ANALYTIC_DX)
CALL ARGUMENT( sio , ANALYTIC_DY, fdbk, Default=ANALYTIC_DY)
CALL END_ARGUMENTS( sio , fdbk )

IF( Using_Test )THEN
 CALL SET_AnalyticTest("WIESEL",sigt,sigs,fdbk)
END IF

!!--end--
END SUBROUTINE



!!### CARD SUBROUTINE <<MCSlongpointlist>>
SUBROUTINE MCSlongpointlist( sio , NDim , Unit_LongChar , PointList_LongChar , fdbk )

!!#### PURPOSE
!! Read in a list of points for the long characteristics solver
!! to output angular fluxes for.

!!#### REQUIRED INPUT/OUTPUT
!! * number of dimensions <NDim>
!! * first point to define symmetry plane <P1sym>
!! * second point to define symmetry plane <P2sym>
!! * the unit to write to <Unit_AFSymmetryCheck>
INTEGER       ,INTENT(IN)    :: NDim
TYPE(TYPE_sio),POINTER       :: sio
REAL(KIND_MSH),POINTER       :: PointList_LongChar(:,:)
INTEGER       ,INTENT(INOUT) :: Unit_LongChar


!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_ = "MCSlongpointlist"

!!#### LOCAL VARIABLES
TYPE(varying_string) :: file
INTEGER              :: NPts

!!--begin--

!! Arguments.
CALL BEGIN_ARGUMENTS( sio , (/"Npts","file"/) , fdbk ,&
  nsubarg=(/0,0/) ,&
  optional=(/.FALSE.,.FALSE./))
CALL ARGUMENT( sio , NPts , Fdbk )
CALL ARGUMENT( sio , file , fdbk )
CALL END_ARGUMENTS( sio , fdbk )

CALL DATABLOCK( sio , (/1,Ndim/) , (/1,Npts/) , PointList_LongChar , fdbk )

!! Wrapup.
IF( Reading(sio) )THEN
 Unit_LongChar = NewFile(file)
END IF


!!--end--
END SUBROUTINE


!!### CARD SUBROUTINE <<MCSdebugpsiv>>
SUBROUTINE MCSdebugpsiv( sio , NDim , Unit_DebugPsiV , PointList_DebugPsiV , fdbk )

!!#### PURPOSE
!! Read in a list of points for the long characteristics solver
!! to output angular fluxes for.

!!#### REQUIRED INPUT/OUTPUT
!! * number of dimensions <NDim>
INTEGER       ,INTENT(IN)    :: NDim
TYPE(TYPE_sio),POINTER       :: sio
REAL(KIND_MSH),POINTER       :: PointList_DebugPsiV(:,:)
INTEGER       ,INTENT(INOUT) :: Unit_DebugPsiV


!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_ = "MCSdebugpsiv"

!!#### LOCAL VARIABLES
TYPE(varying_string) :: file
INTEGER              :: NPts

!!--begin--

!! Arguments.
CALL BEGIN_ARGUMENTS( sio , (/"Npts","file"/) , fdbk ,&
  nsubarg=(/0,0/) ,&
  optional=(/.FALSE.,.FALSE./))
CALL ARGUMENT( sio , NPts , Fdbk )
CALL ARGUMENT( sio , file , fdbk )
CALL END_ARGUMENTS( sio , fdbk )

CALL DATABLOCK( sio , (/1,Ndim/) , (/1,Npts/) , PointList_DebugPsiV , fdbk )

!! Wrapup.
IF( Reading(sio) )THEN
 Unit_DebugPsiV = NewFile(file)
END IF


!!--end--
END SUBROUTINE

!!### CARD SUBROUTINE <<MCSrayinfo>>
SUBROUTINE MCSrayinfo( sio , Print_RayEffectsInfo , rayeffectsfile , fdbk )

!!#### PURPOSE
!! Read in a list of points for the long characteristics solver
!! to output angular fluxes for.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_sio),POINTER        :: sio
LOGICAL       ,INTENT(INOUT)  :: Print_RayEffectsInfo
CHARACTER(*)  ,INTENT(INOUT)  :: rayeffectsfile

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_ = "MCSrayinfo"

!!#### LOCAL VARIABLES
TYPE(varying_string) :: file
INTEGER              :: NPts

!!--begin--

!! Arguments.
CALL BEGIN_ARGUMENTS( sio , (/"Print","file "/) , fdbk ,&
  nsubarg=(/0,0/) ,&
  optional=(/.FALSE.,.TRUE./))
CALL ARGUMENT( sio , Print_RayEffectsInfo , Fdbk )
CALL ARGUMENT( sio , rayeffectsfile , fdbk , Default="" )
CALL END_ARGUMENTS( sio , fdbk )

!!--end--
END SUBROUTINE



END MODULE

