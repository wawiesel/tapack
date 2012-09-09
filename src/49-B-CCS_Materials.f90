!!# COMMAND CARD SWITCHBOARD <<CCS_Materials>>
MODULE CCS_Materials

!!## PURPOSE
!! The switchboard for <Materials> (<MAT>) command
!! cards.

!!## FORTRAN STANDARD MODULES
USE ISO_varying_string !!((03-A-ISO_varying_string.f90))

!!## EXTERNAL KINDS
USE KND_XSExpansion,ONLY: KIND_Mac !!((02-A-KND_XSExpansion.f90))

!!## EXTERNAL PROCEDURES
USE SUB_Pause !!((04-B-SUB_Pause.f90))
USE SUB_Reallocate !!((04-B-SUB_Reallocate.f90))
USE SUB_Find !!((05-B-SUB_Find.f90))
USE FUN_INDEXa !!((08-B-FUN_INDEXa.f90))
USE FUN_ptr_INDICESa !!((09-B-FUN_ptr_INDICESa.f90))
USE FUN_LEN_TRIMa !!((03-A-FUN_LEN_TRIMa.f90))
USE FUN_STR !!((05-B-FUN_STR.f90))
USE FUN_VSTR !!((05-B-FUN_VSTR.f90))
USE FUN_ERROR !!(())

!!## GLOBAL USER MODULES
!! * simple pointer list
!! * feeback object user module
USE USR_SimpleList !!((09-B-USR_SimpleList.f90))
USE USR_fdbk !!((08-C-USR_fdbk.f90))
!USE USR_Source                                             !!((35-B-USR_Source.f90))

!!## GLOBAL LIBRARIES
USE LIB_Prompts !!((06-B-LIB_Prompts.f90))

!!## GLOBAL TOOLBOXES
!! * input/output toolbox
USE TBX_SIO !!((10-A-TBX_SIO.f90))
USE TBX_Mesh !!((15-B-TBX_Mesh.f90))

!!## GLOBAL VARIABLES
USE VAR_Materials    ,ONLY: TYPE_MaterialFill,Reallocate,& !!((48-B-VAR_Materials.f90))
                            LABEL_Materials,l_,MaterialFill
USE VAR_Source !!((47-B-VAR_Source.f90))
USE VAR_XSMonkey !!((47-B-VAR_XSMonkey.f90))
USE VAR_Mesh        ,ONLY: Mesh !!((46-B-VAR_Mesh.f90))
USE VAR_EnergyGroups,ONLY: NG !!((47-B-VAR_EnergyGroups.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## DEFAULT ACCESS
PRIVATE

!!## PRECALCULATED COMMAND HASHES
INTEGER,PARAMETER :: assoc_   = 0000018919
INTEGER,PARAMETER :: assoc0_  = 0000037634
INTEGER,PARAMETER :: init_    = 0000009704
INTEGER,PARAMETER :: macf0_   = 0000017396
INTEGER,PARAMETER :: macnu0_  = 0000034172
INTEGER,PARAMETER :: macs0_   = 0000018068
INTEGER,PARAMETER :: mact0_   = 0000018116
INTEGER,PARAMETER :: polygon_ = 0000092670


!!## PUBLIC ACCESS LIST
PUBLIC :: SWITCHBOARD_Materials


!!## MODULE PROCEDURES
CONTAINS


!!### SUBROUTINE <<SWITCHBOARD_Materials>>
SUBROUTINE SWITCHBOARD_Materials( sio , FdBk )

!!#### PURPOSE
!! The command DataBaSe for the Materials package.

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <sio>
TYPE(TYPE_sio),POINTER :: sio

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <FdBk>
TYPE(TYPE_FdBk),OPTIONAL,INTENT(INOUT) :: FdBk

INTEGER :: j

!!--begin--
!pick routine to execute which loads some data
SELECTCASE(sio%HASH(2))

 CASE(init_)   ; CALL MATinit  ( sio , LABEL_materials , FdBk )

 CASE(assoc0_) ; CALL MATassoc0( sio , l_    , Mesh%CellLabels , LABEL_materials , FdBk )

 CASE(assoc_)  ; CALL MATassoc ( sio , l_    , Mesh%CellLabels , LABEL_materials , FdBk )

 CASE(mact0_)  ; CALL MATmac0  ( sio , MacT  , j , NG , LABEL_materials , FdBk )

 CASE(macs0_)  ; CALL MATmac0  ( sio , MacS  , j , NG , LABEL_materials , FdBk )

 CASE(macnu0_) ; CALL MATmac0  ( sio , MacNu , j , NG , LABEL_materials , FdBk )

 CASE(macf0_)  ; CALL MATmac0  ( sio , MacF  , j , NG , LABEL_materials , FdBk )

 CASE(polygon_); CALL MATpolygon( sio , LABEL_materials , MaterialFill , NUM_Dimensions(Mesh) , FdBk )

 CASE DEFAULT ; WRITE(*,"(a)")WarningPrompt//"MAT (material) command: \"//TRIM(STR(sio%cmd))&
                      //", NOT RECOGNIZED ON LINE=",TRIM(STR(sio%LINE_NUM))//" of FILE="//TRIM(STR(sio%FILE))
ENDSELECT

!!--end--
ENDSUBROUTINE


!!### SUBROUTINE <<MATinit>>
SUBROUTINE MATinit( sio , LABEL_materials , FDBK )

!!#### PURPOSE
!! Initializes a material.

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <sio>
!! * list of material labels <LABEL_materials>
TYPE(TYPE_sio),POINTER :: sio
CHARACTER(*),POINTER :: LABEL_materials(:)

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <FDBK>
TYPE(TYPE_FDBK),OPTIONAL,INTENT(INOUT) :: FDBK

!!#### LOCAL VARIABLES
CHARACTER(LEN(LABEL_materials)) :: string
!!--begin--
IF( Writing(sio) )THEN
 string = LABEL_materials(LEN_TRIMa(LABEL_materials))
 !take the material away from the list of material names
 CALL REMOVE_FROM_LIST( LABEL_materials , string )
ENDIF

!get the material name
CALL BEGIN_ARGUMENTS( sio , (/"material-label"/) )
CALL ARGUMENT( sio , string , FdBk )
CALL END_ARGUMENTS( sio , FdBk )

IF( Reading(sio) )THEN
 !add the material name to the list of material names
 CALL ADD_TO_LIST( LABEL_materials , string )
ENDIF

!!--end--
ENDSUBROUTINE


SUBROUTINE MATmac0( sio , Mac , j , NG , LABEL_materials , FDBK )
!!#### PURPOSE
!! Reads in a simple cross section for a material.

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <sio>
!! * index just inputted, or index to output <j>
!! * macroscopic cross section <Mac>
!! * list of material labels <LABEL_materials>
!! * number of energy groups <NG>
TYPE(TYPE_sio) ,POINTER    :: sio
INTEGER,INTENT(INOUT)    :: j
REAL(KIND_Mac),POINTER    :: Mac(:,:)
CHARACTER(*),INTENT(IN)  :: LABEL_materials(:)
INTEGER      ,INTENT(IN) :: NG

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <FDBK>
TYPE(TYPE_FDBK),OPTIONAL,INTENT(INOUT) :: FDBK

!!--begin--
IF( Reading(sio) )THEN
 IF( .NOT.ASSOCIATED(Mac) )THEN
  ALLOCATE( Mac(1:NG,1:SIZE(LABEL_materials)) )
  Mac = ERROR(1._KIND_Mac)
 ELSE
  CALL REALLOCATE( Mac , dn=(/0,SIZE(LABEL_materials)-SIZE(Mac,2)/) , &
    fill = ERROR(1._KIND_Mac) )
 ENDIF
ENDIF

CALL BEGIN_ARGUMENTS(sio,(/"material-label"/),FdBk)
CALL ARGUMENT( sio , j , FdBk , Keys=LABEL_materials )
CALL END_ARGUMENTS( sio , FdBk )
CALL DATABLOCK( sio , Mac(:,j) , FDBK )

IF( Writing(sio) )THEN
 IF( SIZE(Mac,2)==1 )THEN
  DEALLOCATE( Mac )
  NULLIFY( Mac )
 ELSE
  CALL REALLOCATE( Mac , dn=(/0,-1/) )
 ENDIF
ENDIF

!!--end--
ENDSUBROUTINE



SUBROUTINE MATassoc0( sio , l_ , LABEL_cells , LABEL_materials , FDBK )
!!#### PURPOSE
!! Simple association of a cell with a material.

!!#### MODULES
USE FUN_INDEXa !!((08-B-FUN_INDEXa.f90))
USE SUB_Reallocate !!((04-B-SUB_Reallocate.f90))
USE FUN_SIZEa !!((06-B-FUN_SIZEa.f90))

!!#### REQUIRED INPUT
!! * cell table
!! * material table
CHARACTER(*),POINTER :: LABEL_materials(:)
CHARACTER(*),POINTER :: LABEL_cells(:)

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <sio>
!! * mapping of cell index to material index (cross sections are defined for materials)
TYPE(TYPE_sio),POINTER :: sio
INTEGER     ,POINTER :: l_(:)

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <FDBK>
TYPE(TYPE_FDBK),OPTIONAL,INTENT(INOUT) :: FDBK

!!#### LOCAL VARIABLES
INTEGER :: i,l,Nc,Nm
!!--begin--
Nc=NUM_Cells(Mesh)
Nm=SIZEa(LABEL_Materials)

!get the cell label
CALL BEGIN_ARGUMENTS(sio,(/"cell-label    ",&
                          "material-label"/),FdBk)
CALL ARGUMENT( sio , i , FdBk , Keys=LABEL_cells(1:Nc)  )
CALL ARGUMENT( sio , l , FdBk , Keys=LABEL_materials(1:Nm) )
CALL END_ARGUMENTS( sio , FdBk )

IF( Reading(sio) )THEN
 !(re)allocate, if necessary
 IF( .NOT.ASSOCIATED(l_) )THEN
  ALLOCATE( l_(1:SIZE(LABEL_cells)) )
  l_ = 0
 ELSEIF( i>SIZE(l_) )THEN
  CALL REALLOCATE( l_ , 3 , Fill=INT(0,KIND(l_)) )
 ENDIF

 !set the mapping
 l_(i) = l
ENDIF

!!--end--
ENDSUBROUTINE


SUBROUTINE MATassoc( sio , l_ , LABEL_cells , LABEL_materials , FDBK )
!!#### PURPOSE
!! Association of a cell with a material
!! with a wild-card matching search.

!!#### MODULES
USE FUN_INDEX2 !!((04-B-FUN_INDEX2.f90))
USE SUB_Reallocate !!((04-B-SUB_Reallocate.f90))
USE FUN_SIZEa !!((06-B-FUN_SIZEa.f90))

!!#### REQUIRED INPUT
!! * cell table
!! * material table
CHARACTER(*),POINTER :: LABEL_materials(:)
CHARACTER(*),POINTER :: LABEL_cells(:)

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <sio>
!! * mapping of cell index to material index (cross sections are defined for materials)
TYPE(TYPE_sio),POINTER :: sio
INTEGER     ,POINTER :: l_(:)

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <FDBK>
TYPE(TYPE_FDBK),OPTIONAL,INTENT(INOUT) :: FDBK

!!#### LOCAL VARIABLES
CHARACTER(LEN(LABEL_cells))     :: cell_label
INTEGER :: i,l,n,Nc,Nm

INTEGER,POINTER :: ilist(:)

!!--begin--

Nc=NUM_Cells(Mesh)
Nm=SIZEa(LABEL_Materials)

!get the cell label
CALL BEGIN_ARGUMENTS(sio,(/"cell-label    ",&
                           "material-label"/),FdBk)
CALL ARGUMENT( sio , cell_label , FdBk )
CALL ARGUMENT( sio , l , FdBk , KEYS=LABEL_Materials(1:Nm) )
CALL END_ARGUMENTS( sio , FdBk )

IF( Reading(sio) )THEN

 !allocate, if necessary
 IF( .NOT.ASSOCIATED(l_) )THEN
  ALLOCATE( l_(1:Nc) )
  l_ = 0

 !reallocate if necessary
 ELSE IF( Nc>SIZE(l_) )THEN
  CALL REALLOCATE( l_ , Nc-SIZE(l_) , Fill=INT(0,KIND(l_)) )
 END IF

 !find with wildcard "*"
 NULLIFY( ilist )
 ilist => ptr_INDICESa( LABEL_Cells(1:Nc) , Cell_label , CASESEN=.FALSE. , WILDCARD="*")

 !set material numbers for those cells
 IF( ASSOCIATED(ilist) )THEN
  DO n=1,SIZE(ilist)
   i = ilist(n)
   l_(i) = l
  END DO
  DEALLOCATE(ilist)

 !print warning message if no cell label matches criteria
 ELSE
  CALL UPDATE(fdbk_warning,fdbk,s="The cell label ("//TRIM(cell_label)//") does &
     &not exist.  The material ("//TRIM(LABEL_materials(l))//") cannot be associated with &
         &any cells.")
 END IF

END IF

!!--end--
END SUBROUTINE


SUBROUTINE MATpolygon( sio , LABEL_materials , MaterialFill , ndimensions , FDBK )
!!#### PURPOSE
!! Defines a polygon of a particular material to fill the mesh with.

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <sio>
!! * list of material labels <LABEL_materials>
TYPE(TYPE_sio)           ,POINTER :: sio
CHARACTER(*)            ,POINTER :: LABEL_materials(:)
TYPE(TYPE_MaterialFill)  ,POINTER :: MaterialFill(:)

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: Ndimensions

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <FDBK>
TYPE(TYPE_FDBK),OPTIONAL,INTENT(INOUT) :: FDBK

!!#### LOCAL VARIABLES
CHARACTER(LEN(LABEL_materials)) :: string
INTEGER :: n

!!--begin--
!if writing
IF( Writing(sio) )THEN

 !set label string
 string = LABEL_Materials( MaterialFill(SIZE(MaterialFill)) % l )

 !take the material away
 CALL Reallocate( MaterialFill , -1 )

!if reading
ELSE IF( Reading(sio) )THEN

 !reallocate
 CALL Reallocate( MaterialFill , 1 )

END IF

!A. ARGUMENTS: read/write the material label, number of points
CALL BEGIN_ARGUMENTS( sio , (/"material-label   ",&
                              "num-points       "/) , FdBk )
CALL ARGUMENT( sio , string , FdBk )
CALL ARGUMENT( sio , n , FdBk )
CALL END_ARGUMENTS( sio , FdBk )

!B. DATABLOCK: read/write the coefficients of the polygon
!! (just points of the polygon)
CALL DATABLOCK( sio , (/1,Ndimensions*n/) , MaterialFill(SIZE(MaterialFill))%coeff , FdBk )

IF( Reading(sio) )THEN
 !determine material fill material number
 MaterialFill(SIZE(MaterialFill))%l = INDEXa( LABEL_materials , TRIM(string) , CASESEN=.FALSE. )
END IF

!!--end--
END SUBROUTINE

END MODULE
