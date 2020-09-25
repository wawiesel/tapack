!!# COMMAND CARD SWITCHBOARD: <<CCS_TAPACK>>
MODULE CCS_TAPACK

!!## PURPOSE
!! The command card switchboard for <TAPACK>.


!!## DETAILS
!! Determines the right command card routine to call based
!! on <sio%HASHa> and <sio%HASHb> which is determined
!! from a two-part hash of <sio%COM>.  The routine
!! corresponding to the command card is then called to read/write
!! the associated data.


!!## NOTE
!! This routine needs access to all variable modules
!! and all card modules.


!!## FORTRAN STANDARDS MODULES
USE ISO_varying_string                                        !!((03-A-ISO_varying_string.f90))

!!## COMMAND CARD MODULES
USE CC1_simple,ONLY: &                                        !!((11-A-CC1_simple.f90))
     TAPtolsf        =>simple,&
     TAPprinteachit  =>simple,&
     TAPvisualize    =>simple,&
     TAPoutputfile   =>simple,&
     TAPdetails      =>simple,&
     TAPauthor       =>simple,&
     TAPminit        =>simple,&
     TAPmaxit        =>simple,&
     TAPlabel        =>simple,&
     TAPtolbalrel    =>simple,&
     TAPtolbalabs    =>simple,&
     TAPsavesolution =>simple,&
     TAPloadsolution =>simple,&
     TAPnoloupdate   =>simple, &
     TAPconsistency  =>simple 

USE CC1_list,ONLY: &                                          !!((11-A-CC1_list.f90))
     TAPinputedit    =>list,&
     TAPoutput       =>list

!!## COMMAND CARD SWITCHBOARDS
USE CCS_EnergyGroups     ,ONLY: SWITCHBOARD_EnergyGroups      !!((48-B-CCS_EnergyGroups.f90))
USE CCS_Mesh             ,ONLY: SWITCHBOARD_Mesh              !!((47-B-CCS_Mesh.f90))
USE CCS_DiscreteOrdinates,ONLY: SWITCHBOARD_DiscreteOrdinates !!((48-B-CCS_DiscreteOrdinates.f90))
USE CCS_Materials        ,ONLY: SWITCHBOARD_Materials         !!((49-B-CCS_Materials.f90))
USE CCS_MoCshort         ,ONLY: SWITCHBOARD_MoCshort          !!((67-B-CCS_MoCshort.f90))
USE CCS_Source           ,ONLY: SWITCHBOARD_Source            !!((48-B-CCS_Source.f90))
USE CCS_QuasiDiffusion   ,ONLY: SWITCHBOARD_QuasiDiffusion    !!((67-B-CCS_QuasiDiffusion.f90))

!!## PARAMETERS MODULES
USE PAR_TAPACK                                                !!((05-C-PAR_TAPACK.f90))

!!## EXTERNAL PROCEDURES
USE SUB_CLEAR                                                 !!((04-A-SUB_CLEAR.f90))
USE SUB_Reallocate                                            !!((04-B-SUB_Reallocate.f90))

!!## VARIABLE MODULES
USE VAR_TAPACK                                                !!((66-C-VAR_TAPACK.f90))
USE VAR_Mesh                                                  !!((46-B-VAR_Mesh.f90))
USE VAR_Materials                                             !!((48-B-VAR_Materials.f90))

!!## USER MODULES
USE USR_fdbk                                                  !!((08-C-USR_fdbk.f90))
USE USR_Mesh                                                  !!((14-B-USR_Mesh.f90))
USE USR_IntegralRegion                                        !!((14-B-USR_IntegralRegion.f90))

!!## TOOLBOXES
USE TBX_SIO                                                   !!((10-A-TBX_SIO.f90))
USE TBX_Mesh,ONLY: NUM_Dimensions,&                           !!((15-B-TBX_Mesh.f90))
                   GET_Vert

![novis]!USE VIS_TAPACK                                                !!((68-C-VIS_TAPACK.f90))
USE PRN_Mesh,ONLY: gmvInitialize,gmvFinalize                  !!((16-C-PRN_Mesh.f90))
USE LIB_GenericPhrases                                        !!((07-B-LIB_GenericPhrases.f90))
USE FUN_STR                                                   !!((05-B-FUN_STR.f90))
USE FUN_VSTR                                                  !!((05-B-FUN_VSTR.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## DEFAULT ACCESS
PRIVATE

!!## PRECALCULATED COMMAND HASHES
!! * primary hash value using the hashing function, <HashP>
INTEGER,PARAMETER :: DOR_ = 0000002862
INTEGER,PARAMETER :: MAT_ = 0000003556
INTEGER,PARAMETER :: MCS_ = 0000003672
INTEGER,PARAMETER :: MSH_ = 0000004440
INTEGER,PARAMETER :: NRG_ = 0000004636
INTEGER,PARAMETER :: QDF_ = 0000004380
INTEGER,PARAMETER :: SRC_ = 0000005358
INTEGER,PARAMETER :: TAP_ = 0000004654
!! * secondary hash value using the hashing function, <HashP>
INTEGER,PARAMETER :: author_         = 0000040161
INTEGER,PARAMETER :: details_        = 0000052470
INTEGER,PARAMETER :: init_           = 0000009704
INTEGER,PARAMETER :: inputedit_      = 0000202797
INTEGER,PARAMETER :: label_          = 0000016641
INTEGER,PARAMETER :: outfile_        = 0000096130
INTEGER,PARAMETER :: output_         = 0000058903
INTEGER,PARAMETER :: visualize_      = 0000237365
INTEGER,PARAMETER :: visualizenow_   = 0000667068
INTEGER,PARAMETER :: psival_         = 0000053325
INTEGER,PARAMETER :: surge_          = 0000031805
INTEGER,PARAMETER :: loworder_       = 0000145436
INTEGER,PARAMETER :: highorder_      = 0000153747
INTEGER,PARAMETER :: integralregion_ = 0000936019
INTEGER,PARAMETER :: currentexiting_ = 0001048533
INTEGER,PARAMETER :: megatable_      = 0000148719
INTEGER,PARAMETER :: checksoln_      = 0000122887
INTEGER,PARAMETER :: maxit_          = 0000021301
INTEGER,PARAMETER :: minit_          = 0000022403
INTEGER,PARAMETER :: printeachit_    = 0000440826
INTEGER,PARAMETER :: tolsf_          = 0000029527
INTEGER,PARAMETER :: tolbalrel_      = 0000205433
INTEGER,PARAMETER :: tolbalabs_      = 0000202531
INTEGER,PARAMETER :: savesolution_   = 0000588576
INTEGER,PARAMETER :: loadsolution_   = 0000514474
INTEGER,PARAMETER :: noloupdate_     = 0000316389
INTEGER,PARAMETER :: consistency_       = 0000383108
CHARACTER(*),PARAMETER :: mod_="CCS_TAPACK"

!!## ACCESS
PUBLIC :: SWITCHBOARD_TAPACK


!!### MODULE PROCEDURES
CONTAINS


SUBROUTINE SWITCHBOARD_TAPACK( sio , fdbk )
!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <sio>
TYPE(TYPE_sio),POINTER :: sio

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!--begin--
SELECT CASE(sio%HASH(1))
 !!primary commands
 CASE(TAP_) ; CALL SWITCHBOARD_(sio,fdbk)

 !!secondary commands
 CASE(NRG_) ; CALL SWITCHBOARD_EnergyGroups     ( sio , fdbk )
 CASE(DOR_) ; CALL SWITCHBOARD_DiscreteOrdinates( sio , fdbk )
 CASE(MAT_) ; CALL SWITCHBOARD_Materials        ( sio , fdbk )
 CASE(MCS_) ; CALL SWITCHBOARD_MoCshort         ( sio , Mesh , fdbk )
 CASE(MSH_) ; CALL SWITCHBOARD_MeSH             ( sio , fdbk )
 CASE(SRC_) ; CALL SWITCHBOARD_Source           ( sio , fdbk )
 CASE(QDF_) ; CALL SWITCHBOARD_QuasiDiffusion   ( sio , fdbk )

END SELECT

!dump after the database
CALL DUMP(fdbk)

!!--end--
END SUBROUTINE



SUBROUTINE SWITCHBOARD_( sio , fdbk )
!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <sio>
TYPE(TYPE_sio),POINTER :: sio

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!--begin--
SELECT CASE(sio%HASH(2))

 CASE(init_)       ; CALL TAPinit( sio , TimeTreatment         ,  &
                                         EnergyDiscretization  ,  &
                                         AngularDiscretization ,  &
                                         SpatialDiscretization ,  &
                                         TransportModule       ,  &
                                         AccelerationModule    ,  fdbk )
 CASE(highorder_)  ; CALL TAPhighorder( sio , TransportModule    ,  fdbk )

 CASE(loworder_)   ; CALL TAPloworder( sio , AccelerationModule    ,  fdbk )

 CASE(author_   )  ; CALL TAPauthor(sio,author,fdbk)

 CASE(label_    )  ; CALL TAPlabel(sio,label ,fdbk)

 CASE(visualize_)  ; CALL TAPvisualize(sio,Requesting_Visualization,fdbk)

 CASE(inputedit_)  ; CALL TAPinputedit(sio,LIST_InputEdits,KEY_InputEdits,fdbk)

 CASE(output_)     ; CALL TAPoutput(sio,LIST_Outputs   ,KEY_Outputs   ,fdbk)

 CASE(outfile_)    ; CALL TAPoutputfile(sio,OutputFileBase,fdbk)

 CASE(details_)    ; CALL TAPdetails(sio,details,fdbk)

 CASE(minit_)      ; CALL TAPminit(sio,MIN_iter,fdbk)

 CASE(maxit_)      ; CALL TAPmaxit(sio,MAX_iter,fdbk)

 CASE(printeachit_); CALL TAPprinteachit(sio,PrintEachIteration,fdbk)

 CASE(psival_)     ; CALL TAPpsival(sio,glist,klist,fdbk)

 CASE(tolsf_)      ; CALL TAPtolsf( sio , tolPhiVInf , fdbk ); reltolPhiVInf=tolPhiVInf

 CASE(tolbalrel_)      ; CALL TAPtolbalrel( sio , tolBalRel , fdbk )
 CASE(tolbalabs_)      ; CALL TAPtolbalabs( sio , tolBalAbs , fdbk )

 CASE(integralregion_) ; CALL TAPintegralregion( sio , IntegralRegions , NUM_Dimensions(Mesh) , fdbk )

 CASE(currentexiting_) ; CALL TAPcurrentexiting( sio , exit_r1 , exit_r2 , NUM_Dimensions(Mesh) , fdbk )

 CASE(megatable_) ; CALL TAPmegatable( sio , megatable , fdbk )
 CASE(checksoln_) ; CALL TAPchecksoln( sio , megatable_checksoln , &
   checkfn,checkbnd,checktol,fdbk )

 CASE(visualizenow_)   ; CALL TAPvisualizenow   ( sio , Mesh , fdbk )

 CASE(savesolution_) ; CALL TAPsavesolution(sio,savesolution,fdbk)
 CASE(loadsolution_) ; CALL TAPloadsolution(sio,loadsolution,fdbk)
 CASE(noloupdate_) ; CALL TAPnoloupdate(sio,noloupdate,fdbk)
 CASE(consistency_)    ; CALL TAPconsistency( sio , Enforce_Consistency, fdbk )
 CASE DEFAULT      ; CALL UPDATE( (/fdbk_warning/) , fdbk , i=(/sio%LINE_NUM/) , &
                       s="[[TAP]] Execution has stopped on LINE_NUM=%I(1) of FILE='"//&
                       TRIM(STR(sio%FILE))//" because a 'TAP' (Transport&
                       & Algorithms Package) command was not found in the database.&
                       &  Possible reasons for this error are: 1) the command-suffix, "//&
                       TRIM(STR(sio%splitcmd(2)))//&
                       " (and thus the command 'TAP"//TRIM(STR(sio%splitcmd(2)))//&
                       "'), doesn't exist; 2) the hash value, "//&
                       TRIM(STR(sio%splitcmd(2)))//"_, is not equal to HASH('"//&
                       TRIM(STR(sio%splitcmd(2)))//"') or it doesn't exist.)" )

END SELECT

!!--end--
END SUBROUTINE


SUBROUTINE TAPinit( sio , TimeTreatment      ,  &
                      EnergyDiscretization  ,  &
                      AngularDiscretization ,  &
                      SpatialDiscretization ,  &
                      TransportModule       ,  &
                      AccelerationModule    ,  fdbk )

!!#### PARAMETERS
USE PAR_TAPACK                                                !!((05-C-PAR_TAPACK.f90))
CHARACTER(*),PARAMETER :: proc_="TAPinit"

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <sio>
!! * time treatment integer-keys and character-keys <EnergyDiscretization,KEY_EnergyDiscretization>
!! * energy discretization integer-keys and character-keys <EnergyDiscretization,KEY_EnergyDiscretization>
!! * angular discretization integer-keys and character-keys <AngularDiscretization,KEY_AngularDiscretization>
!! * spatial discretization integer-keys and character-keys <SpatialDiscretization,KEY_SpatialDiscretization>
!! * transport method integer-keys and character-keys <TransportModule,KEY_TransportModule>
!! * acceleration integer-keys and character-keys <AccelerationModule,KEY_AccelerationModule>
TYPE(TYPE_sio)  ,POINTER       :: sio
INTEGER       ,INTENT(INOUT) :: TimeTreatment
INTEGER       ,INTENT(INOUT) :: EnergyDiscretization
INTEGER       ,INTENT(INOUT) :: AngularDiscretization
INTEGER       ,INTENT(INOUT) :: SpatialDiscretization
INTEGER       ,INTENT(INOUT) :: TransportModule
INTEGER       ,INTENT(INOUT) :: AccelerationModule


!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!--begin--
CALL BEGIN_ARGUMENTS( sio , (/"TimeTreatment        ",&
                             "EnergyDiscretization ",&
                             "AngularDiscretization",&
                             "SpatialDiscretization",&
                             "TransportModule      ",&
                             "AccelerationModule   "/) , fdbk )
CALL ARGUMENT( sio , TimeTreatment        , fdbk , Keys=KEY_TimeTreatment         )
CALL ARGUMENT( sio , EnergyDiscretization , fdbk , Keys=KEY_EnergyDiscretization  )
CALL ARGUMENT( sio , AngularDiscretization, fdbk , Keys=KEY_AngularDiscretization )
CALL ARGUMENT( sio , SpatialDiscretization, fdbk , Keys=KEY_SpatialDiscretization )
CALL ARGUMENT( sio , TransportModule      , fdbk , Keys=KEY_TransportModule       )
CALL ARGUMENT( sio , AccelerationModule   , fdbk , Keys=KEY_AccelerationModule    )

CALL END_ARGUMENTS( sio , fdbk )

!!--end--
END SUBROUTINE


SUBROUTINE TAPloworder( sio , AccelerationModule , fdbk )

!!#### PARAMETERS
USE PAR_TAPACK                                                !!((05-C-PAR_TAPACK.f90))
CHARACTER(*),PARAMETER :: proc_="TAPloworder"

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <sio>
!! * acceleration integer-keys and character-keys <AccelerationModule,KEY_AccelerationModule>
TYPE(TYPE_sio),POINTER       :: sio
INTEGER       ,INTENT(INOUT) :: AccelerationModule


!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!--begin--
CALL BEGIN_ARGUMENTS( sio , (/"AccelerationModule"/) , fdbk )

CALL ARGUMENT( sio , AccelerationModule   , fdbk , Keys=KEY_AccelerationModule    )

CALL END_ARGUMENTS( sio , fdbk )

!!--end--
END SUBROUTINE



SUBROUTINE TAPhighorder( sio , TransportModule , fdbk )

!!#### PARAMETERS
USE PAR_TAPACK                                                !!((05-C-PAR_TAPACK.f90))
CHARACTER(*),PARAMETER :: proc_="TAPhighorder"

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <sio>
!! * transport integer-keys and character-keys <TransportModule,KEY_TransportModule>
TYPE(TYPE_sio),POINTER       :: sio
INTEGER       ,INTENT(INOUT) :: TransportModule


!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!--begin--
CALL BEGIN_ARGUMENTS( sio , (/"TransportModule"/) , fdbk )

CALL ARGUMENT( sio , TransportModule   , fdbk , Keys=KEY_TransportModule    )

CALL END_ARGUMENTS( sio , fdbk )

!!--end--
END SUBROUTINE



SUBROUTINE TAPsurge( sio , SurGE_Data , SurGE_Suffix , fdbk )

!!#### PARAMETERS
USE PAR_TAPACK                                                !!((05-C-PAR_TAPACK.f90))

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <sio>
!! * option value <SurGE_Data>
TYPE(TYPE_sio),POINTER       :: sio
INTEGER       ,INTENT(INOUT) :: SurGE_Data
CHARACTER     ,INTENT(INOUT) :: Surge_Suffix

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!--begin--

CALL BEGIN_ARGUMENTS( sio , (/"Option","Suffix"/) , fdbk ,&
  OPTIONAL = (/.FALSE.,.TRUE./) )
CALL ARGUMENT( sio , SurGE_Data , fdbk , KEYS=KEY_SurGE)
CALL ARGUMENT( sio , SurGE_Suffix , fdbk , Default="!")
CALL END_ARGUMENTS( sio , fdbk )

!!--end--
END SUBROUTINE



SUBROUTINE TAPpsival( sio , glist , klist , fdbk )

!!#### PARAMETERS
USE PAR_TAPACK                                                !!((05-C-PAR_TAPACK.f90))
USE SUB_Reallocate                                            !!((04-B-SUB_Reallocate.f90))
USE VAR_Mesh,ONLY: Mesh                                       !!((46-B-VAR_Mesh.f90))
USE TBX_Mesh                                                  !!((15-B-TBX_Mesh.f90))
USE FUN_IsError                                               !!((05-A-FUN_IsError.f90))

CHARACTER(*),PARAMETER :: proc_="TAPpsival"

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <sio>
!! * list of energy index values <glist>
!! * list of vertex index values <klist>
TYPE(TYPE_sio),POINTER :: sio
INTEGER       ,POINTER :: glist(:)
INTEGER       ,POINTER :: klist(:)

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
INTEGER :: g0,k0
REAL(KIND_MSH) :: R0(NUM_Dimensions(Mesh))

!!--begin--
IF( Writing(sio) )THEN
 IF( .NOT.ASSOCIATED(glist) )RETURN
 g0 = glist(SIZE(glist))
 CALL REALLOCATE( glist , dn=-1 )
 k0 = klist(SIZE(klist))
 CALL REALLOCATE( klist , dn=-1 )
END IF

CALL BEGIN_ARGUMENTS( sio , (/"g","k","R"/) , fdbk , &
  OPTIONAL=(/.TRUE.,.TRUE.,.TRUE./),&
  NSUBARG=(/0,0,SIZE(R0)/) )
CALL ARGUMENT( sio , g0 , fdbk , DEFAULT=0 )
CALL ARGUMENT( sio , k0 , fdbk , DEFAULT=0 )
CALL ARGUMENT( sio , R0 , fdbk , DEFAULT=SPREAD(ERROR(1._KIND_MSH),1,SIZE(R0)) )
CALL END_ARGUMENTS( sio , fdbk )

IF( Reading(sio) )THEN
 CALL REALLOCATE( glist , dn=+1 )
 glist(SIZE(glist)) = g0
 CALL REALLOCATE( klist , dn=+1 )
 IF( .NOT.ANY(IsError(R0)) )THEN
  k0 = GET_Vert(Mesh,vert=R0,tol=HUGE(1._KIND_MSH))
 END IF
 klist(SIZE(klist)) = k0
END IF


!!--end--
END SUBROUTINE



SUBROUTINE TAPcurrentexiting( sio  , exit_r1 , exit_r2 , nd , fdbk )
!!#### PURPOSE
!! Outputs the exiting current for a defined portion of the boundary.

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <sio>
!! * definitions of the regions
!! * number of dimensions
TYPE(TYPE_sio),POINTER :: sio
REAL(KIND_MSH),POINTER :: exit_r1(:),exit_r2(:)
INTEGER       ,INTENT(IN) :: nd


!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk
REAL(KIND_MSH) :: posinf(nd),neginf(nd)

!!--begin--

posinf = SPREAD(+HUGE(1._KIND_MSH),1,nd)
neginf = SPREAD(-HUGE(1._KIND_MSH),1,nd)

IF( Reading(SIO) )THEN
 CALL CLEARn( exit_r1 )
 CALL CLEARn( exit_r2 )
 ALLOCATE( exit_r1(nd) , exit_r2(nd) )
END IF


!A. ARGUMENTS: read/write the region label, number of points, scale-factor
CALL BEGIN_ARGUMENTS( sio , (/"exitr1",&
                              "exitr2"/) , fdbk , Optional=(/.TRUE.,&
                                                             .TRUE./) , &
                                                  NSubArg=(/nd,nd/) )
CALL ARGUMENT       ( sio , exit_r1 , fdbk , Default=posinf )
CALL ARGUMENT       ( sio , exit_r2 , fdbk , Default=neginf )
CALL END_ARGUMENTS( sio , fdbk )

IF( Writing(SIO) )THEN
 CALL CLEARn( exit_r1 )
 CALL CLEARn( exit_r2 )
END IF

!!--end--
END SUBROUTINE




!!### SUBROUTINE <<TAPmegatable>>
SUBROUTINE TAPmegatable( sio  , megatable , fdbk )

!!#### PURPOSE
!! Specifies a new column to add to the megatable.  A row of
!! the megatable is printed out to the file "megatable.txt" each
!! run.  This gives an easy way to compare some quantities across
!! multiple runs without digging through the output file.


!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <sio>
!! * the specs for the megatable <megatable>
!! * number of dimensions <nd>
TYPE(TYPE_sio)      ,POINTER    :: sio
TYPE(TYPE_megatable),POINTER    :: megatable(:)


!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk


!!#### LOCAL VARIABLES
INTEGER :: b
TYPE(varying_string) :: VSblank !need this for a default value

!!--begin--

VSblank = " "

IF( Reading(SIO) )THEN
 CALL REALLOCATE_megatable( megatable , 1 )
 b = SIZE(megatable)
END IF


CALL BEGIN_ARGUMENTS( sio , (/"spec  ",&
                              "i     ","j     ","k     ","l     ",&
                              "m     ","g     ","r     ","Omega ",&
                              "E     ","Region","Note  "/) , fdbk , &
                              Optional=(/.FALSE.,&
                                         .TRUE.,.TRUE.,.TRUE.,.TRUE.,&
                                         .TRUE.,.TRUE.,&
                                         .TRUE.,.TRUE.,.TRUE.,&
                                         .TRUE.,&
                                         .TRUE./) , &
                              NSubArg= (/0,&
                                         0,0,0,0,&
                                         0,0,&
                                         3,3,0,&
                                         0,&
                                         0/) )

CALL ARGUMENT       ( sio , megatable(b)%spec   , fdbk )
CALL ARGUMENT       ( sio , megatable(b)%i      , fdbk , Default=0)
CALL ARGUMENT       ( sio , megatable(b)%j      , fdbk , Default=0)
CALL ARGUMENT       ( sio , megatable(b)%k      , fdbk , Default=0)
CALL ARGUMENT       ( sio , megatable(b)%l      , fdbk , Default=0)
CALL ARGUMENT       ( sio , megatable(b)%m      , fdbk , Default=0)
CALL ARGUMENT       ( sio , megatable(b)%g      , fdbk , Default=0)
CALL ARGUMENT       ( sio , megatable(b)%r      , fdbk , Default=(/0.d0,0.d0,0.d0/))
CALL ARGUMENT       ( sio , megatable(b)%Omega  , fdbk , Default=(/0.d0,0.d0,0.d0/))
CALL ARGUMENT       ( sio , megatable(b)%E      , fdbk , Default=0.d0)
CALL ARGUMENT       ( sio , megatable(b)%Region , fdbk , Default=" " )
CALL ARGUMENT       ( sio , megatable(b)%Note   , fdbk , Default=" " )

CALL END_ARGUMENTS( sio , fdbk )

!!--end--
END SUBROUTINE




!!### SUBROUTINE <<TAPchecksoln>>
SUBROUTINE TAPchecksoln( sio  , megatable , &
  checkfn,checkbnd,checktol,fdbk)

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <sio>
!! * the specs for the megatable <megatable>
!! * number of dimensions <nd>
TYPE(TYPE_sio)      ,POINTER    :: sio
TYPE(TYPE_megatable),POINTER    :: megatable(:)
INTEGER             ,POINTER    :: checkfn(:)
INTEGER             ,POINTER    :: checkbnd(:)
REAL(KIND_Rdp)      ,POINTER    :: checktol(:)

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
CHARACTER(*),PARAMETER :: proc_="TAPchecksoln"
INTEGER              :: b
TYPE(varying_string) :: VS,VStrue,VS2
CHARACTER(32)        :: errormsg

!!--begin--

IF( Reading(SIO) )THEN
 CALL REALLOCATE_megatable( megatable , 1 )
 b = SIZE(megatable)
 CALL REALLOCATE( checkfn  , 1 )
 CALL REALLOCATE( checkbnd , 1 )
 CALL REALLOCATE( checktol , 1 )
 VStrue="0.d0"
END IF


CALL BEGIN_ARGUMENTS( sio , (/"spec    ","checkfn ",&
                              "i       ","j       ",&
                              "k       ","l       ",&
                              "m       ","g       ",&
                              "r       ","Omega   ","E       ",&
                              "Region  ","Note    ","checktol",&
                              "checkbnd"/) , fdbk , &
                              Optional=(/.FALSE.,.FALSE.,&
                                         .TRUE.,.TRUE.,.TRUE.,.TRUE.,&
                                         .TRUE.,.TRUE.,&
                                         .TRUE.,.TRUE.,.TRUE.,&
                                         .TRUE.,&
                                         .TRUE.,.TRUE.,.TRUE./) , &
                              NSubArg= (/0,0,&
                                         0,0,0,0,&
                                         0,0,&
                                         3,3,0,&
                                         0,&
                                         0,0,0/) )

CALL ARGUMENT       ( sio , megatable(b)%spec   , fdbk )
CALL ARGUMENT       ( sio , VS                  , fdbk )
CALL ARGUMENT       ( sio , megatable(b)%i      , fdbk , Default=-1)
CALL ARGUMENT       ( sio , megatable(b)%j      , fdbk , Default=-1)
CALL ARGUMENT       ( sio , megatable(b)%k      , fdbk , Default=-1)
CALL ARGUMENT       ( sio , megatable(b)%l      , fdbk , Default=-1)
CALL ARGUMENT       ( sio , megatable(b)%m      , fdbk , Default=-1)
CALL ARGUMENT       ( sio , megatable(b)%g      , fdbk , Default=-1)
CALL ARGUMENT       ( sio , megatable(b)%r      , fdbk , Default=(/0.d0,0.d0,0.d0/))
CALL ARGUMENT       ( sio , megatable(b)%Omega  , fdbk , Default=(/0.d0,0.d0,0.d0/))
CALL ARGUMENT       ( sio , megatable(b)%E      , fdbk , Default=0.d0)
CALL ARGUMENT       ( sio , megatable(b)%Region , fdbk , Default=" " )
CALL ARGUMENT       ( sio , megatable(b)%Note   , fdbk , Default=" " )
CALL ARGUMENT       ( sio , checktol(b)         , fdbk , Default=1.d-16 )
CALL ARGUMENT       ( sio , VS2                 , fdbk , Default=VStrue )
CALL END_ARGUMENTS( sio , fdbk )

IF( Reading(sio) )THEN
 call s_createfn(STR(VS), 'x y z ox oy oz m', checkfn(b), errormsg)
 IF( TRIM(errormsg)/="OK" )THEN
  VS = MODPROC(mod_,proc_)//"the submitted function <"//&
  TRIM(STR(VS))//"> produced the error message "//&
  "<"//TRIM(errormsg)//">."
  CALL UPDATE(fdbk_error,fdbk,s=STR(VS))
 END IF
 call s_createfn(STR(VS2),'x y z ox oy oz m', checkbnd(b),errormsg)
 IF( TRIM(errormsg)/="OK" )THEN
  VS2 = MODPROC(mod_,proc_)//"the submitted function <"//&
  TRIM(STR(VS2))//"> produced the error message "//&
  "<"//TRIM(errormsg)//">."
  CALL UPDATE(fdbk_error,fdbk,s=STR(VS2))
 END IF

END IF

!!--end--
END SUBROUTINE




!!### SUBROUTINE: <<TAPvisualizenow>>
SUBROUTINE TAPvisualizenow( sio , Mesh , fdbk )

!!#### PURPOSE
!! Immediately visualize the mesh (reading only).

USE TBX_Materials                                             !!((53-B-TBX_Materials.f90))
USE VAR_Materials,ONLY: MaterialFill                          !!((48-B-VAR_Materials.f90))

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <sio>
!! * mesh object <Mesh>
TYPE(TYPE_sio) ,POINTER       :: sio
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
LOGICAL :: TorF
INTEGER :: Unit_

!!--begin--

!! Arguments.
CALL BEGIN_ARGUMENTS( sio , (/"TorF"/) , fdbk , Optional=(/.true./),NSubArg=(/0/))
CALL ARGUMENT( sio , TorF , fdbk , default=.true. )
CALL END_ARGUMENTS( sio , fdbk )

IF( TorF )THEN
 !write out materials colors file
 CALL SETUP_MaterialFills( Mesh , MaterialFill )
 CALL FINALIZE_Mesh(Mesh)
 CALL UpdateAndDump(fdbk_comment,fdbk,s="[[TAP]] Putting mesh into [file=Mesh.gmv].")
 Unit_ = NewFile( "Mesh.gmv" )
 CALL gmvInitialize(Unit_,Mesh,l_,LABEL_Materials)
 CALL gmvFinalize(Unit_,Mesh)
 CLOSE(Unit_)
 STOP
 !CALL OUTPUT_ColorMap(Mesh)
 ![novis]!CALL Visualize_TAPACK()
END IF

!!--end--
END SUBROUTINE



!!### SUBROUTINE <<TAPintegralregion>>
SUBROUTINE TAPintegralregion( sio , IntegralRegion , ndimensions , fdbk )

!!#### PURPOSE
!! Defines a polygon to report integrals of.


!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <sio>
!! * regions to integrate over <IntegralRegion>
TYPE(TYPE_sio)           ,POINTER :: sio
TYPE(TYPE_IntegralRegion),POINTER :: IntegralRegion(:)


!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: Ndimensions


!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk


!!#### LOCAL VARIABLES
CHARACTER(LEN_Label) :: string
INTEGER :: n,m
REAL(KIND_MSH) :: scale


!!--begin--

!if writing
IF( Writing(sio) )THEN

 !current index
 m = SIZE(IntegralRegion)

 !number of points
 n = SIZE(IntegralRegion(m)%coeff)/ndimensions

 !set label string
 string = IntegralRegion(m)%label

 !set scale
 scale = IntegralRegion(m)%scale

!if reading
ELSE IF( Reading(sio) )THEN

 !reallocate
 CALL Reallocate_IntegralRegion( IntegralRegion , 1 )

 !current index
 m = SIZE(IntegralRegion)

END IF


!A. ARGUMENTS: read/write the region label, number of points, scale-factor
CALL BEGIN_ARGUMENTS( sio , (/"region-label",&
                              "num-points  " ,&
                              "scale-factor"/) , fdbk )
CALL ARGUMENT       ( sio , string           , fdbk )
CALL ARGUMENT       ( sio , n                , fdbk )
CALL ARGUMENT       ( sio , scale            , fdbk )
CALL END_ARGUMENTS  ( sio                    , fdbk )

!B. DATABLOCK: read/write the coefficients of the polygon (just points of the polygon)
CALL DATABLOCK( sio , (/1,Ndimensions*n/) , IntegralRegion(m)%coeff , fdbk )


IF( Reading(sio) )THEN
 !set label string
 IntegralRegion(m)%label = string

 !set scale
 IntegralRegion(m)%scale = scale

ELSE

 !take the material away
 CALL REALLOCATE_IntegralRegion( IntegralRegion , -1 )

END IF

!!--end--
END SUBROUTINE





END MODULE
