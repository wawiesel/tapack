!!# COMMAND CARD SWITCHBOARD MODULE: >>CCS_Mesh<<
MODULE CCS_Mesh


!!## PURPOSE
!! Provide the command card switchboard for the meshing
!! (MSH) package.



!!## MODULE DEPENDENCIES
USE ISO_varying_string                    !!((03-A-ISO_varying_string.f90))
USE PAR_Constants_Rdp                     !!((02-A-PAR_Constants_Rdp.f90))
USE SUB_CLEAR                             !!((04-A-SUB_CLEAR.f90))
USE SUB_CLEARn                            !!((04-A-SUB_CLEARn.f90))
USE SUB_Find                              !!((05-B-SUB_Find.f90))
USE SUB_Pause                             !!((04-B-SUB_Pause.f90))
USE SUB_Reallocate                        !!((04-B-SUB_Reallocate.f90))
USE SUB_Stop                              !!((04-B-SUB_Stop.f90))
USE FUN_INDEXa                            !!((08-B-FUN_INDEXa.f90))
USE FUN_Random                            !!((03-A-FUN_Random.f90))
USE FUN_Error                             !!((04-A-FUN_Error.f90))
USE FUN_Sentence                          !!((04-B-FUN_Sentence.f90))
USE FUN_NewFile                           !!((05-B-FUN_NewFile.f90))
USE FUN_VSTR                              !!((05-B-FUN_VSTR.f90))
USE FUN_STR                               !!((05-B-FUN_STR.f90))
USE FUN_xyPLANE                           !!((05-B-FUN_xyPLANE.f90))
USE FUN_xyzPLANE                          !!((06-B-FUN_xyzPLANE.f90))
USE FUN_Upcase                            !!((03-A-FUN_Upcase.f90))
USE FUN_xyPERPCCW                         !!((03-A-FUN_xyPERPCCW.f90))
USE USR_fdbk                              !!((08-C-USR_fdbk.f90))
USE USR_SimpleSet                         !!((12-B-USR_SimpleSet.f90))
USE LIB_GenericPhrases                    !!((07-B-LIB_GenericPhrases.f90))
USE LIB_Norm                              !!((04-B-LIB_Norm.f90))
USE LIB_Prompts                           !!((06-B-LIB_Prompts.f90))

USE TBX_SIO                               !!((10-A-TBX_SIO.f90))

USE PAR_ComputationalGeometry
USE USR_IntegralRegion
USE KND_Mesh
USE PAR_Mesh
USE USR_Mesh
! I don't know what is causing the error in this routine, but
! using everything from <TBX_Mesh> causes a strange error so I just
! add the long list of all I need.
USE TBX_Mesh,ONLY: ModifyInterpGrid,&
  IsCellBased,ENSURE_Mesh_Cellbased,KEY_MeshType,&
  UPDATE_Mesh,&
  GET_Vert,GET_Face,GET_Cell,SPLIT_Face,&
  Vert,&
  ADD_Face_Ps,ADD_Cell_Jx,ADD_blockVerts,&
  ADD_blockFaces,ADD_blockCells,&
  ptr_CellSet_ALL,ptr_CellSet_POLYREGION,ptr_CellSet_GENLABEL,&
  ptr_FaceSet_ALL,ptr_FaceSet_PLANEINTERSECT,ptr_FaceSet_POLYREGION,ptr_FaceSet_GENLABEL,&
  ptr_VertSet_ALL,ptr_VertSet_POLYREGION,ptr_VertSet_GENLABEL,&
  SPLIT_Cells,SPLIT_Mesh_Pn,OrientBoundary,ADD_CellCentroidVerts,&
  CREATE,DESTROY,SCALE_Mesh,TRANSLATE_Mesh,PERTURB_Verts,&
  GRAVITATE_Verts,RENAME_Cells,RENAME_Faces,RENAME_Verts,ROTATE_Verts,&
  CREATE_Domain_Qr,SETUP_Mesh_Adap3,CRACK_Cell,CellCentroid,GET_CellVertBoundarySet,&
  PRINT_Mesh


USE VAR_Mesh,ONLY: Mesh,FrozenVertSet                     


!!## DEFAULT IMPLICIT
IMPLICIT NONE



!!## DEFAULT ACCESS
PRIVATE



!!## PRE-CALCULATED COMMAND HASHES
INTEGER,PARAMETER :: block_          = 0000016293
INTEGER,PARAMETER :: cell_           = 0000006128
INTEGER,PARAMETER :: cellbased_      = 0000125281
INTEGER,PARAMETER :: domain_         = 0000034695
INTEGER,PARAMETER :: face_           = 0000006032
INTEGER,PARAMETER :: init_           = 0000009704
INTEGER,PARAMETER :: test_           = 0000012426
INTEGER,PARAMETER :: vert_           = 0000013878
INTEGER,PARAMETER :: split_          = 0000028799
INTEGER,PARAMETER :: interpgrid_     = 0000283863
INTEGER,PARAMETER :: movevert_       = 0000141420
INTEGER,PARAMETER :: moveface_       = 0000133574
INTEGER,PARAMETER :: movecell_       = 0000133670
INTEGER,PARAMETER :: update_         = 0000054041
INTEGER,PARAMETER :: pertvert_       = 0000136000
INTEGER,PARAMETER :: rotvert_        = 0000098478
INTEGER,PARAMETER :: scale_          = 0000022181
INTEGER,PARAMETER :: addccverts_     = 0000150599
INTEGER,PARAMETER :: randseed_       = 0000115228
INTEGER,PARAMETER :: splitface_      = 0000219671
INTEGER,PARAMETER :: finalize_       = 0000095788
INTEGER,PARAMETER :: orientboundary_ = 0001038655
INTEGER,PARAMETER :: rename_         = 0000044071
INTEGER,PARAMETER :: freezevert_     = 0000251881
INTEGER,PARAMETER :: splitfaces_     = 0000317635
INTEGER,PARAMETER :: splitplane_     = 0000328987
INTEGER,PARAMETER :: move_           = 0000012014
INTEGER,PARAMETER :: gravitate_      = 0000178387
INTEGER,PARAMETER :: crackle_        = 0000055470
INTEGER,PARAMETER :: write_          = 0000033617

!!## IDENTIFICATION
CHARACTER(*),PARAMETER :: file_="47-B-CCS_Mesh.f90"
CHARACTER(*),PARAMETER :: mod_ ="CCS_Mesh"



!!## ACCESS
PUBLIC :: SWITCHBOARD_Mesh
PUBLIC :: MSHvert,MSHface,MSHcell,MSHwrite



!!## MODULE PROCEDURES
CONTAINS


!!### SWITCHBOARD SUBROUTINE: <<SWITCHBOARD_Mesh>>
SUBROUTINE SWITCHBOARD_Mesh( SIO , fdbk )

!!#### PURPOSE
!! The switchboard subroutine for the Mesh (MSH) package.

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <SIO>
TYPE(TYPE_SIO),POINTER :: SIO

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_ = "SWITCHBOARD_Mesh"

!!#### LOCAL VARIABLES
TYPE(varying_string) :: VS

!!--begin--

!select and execute appropriate data-import/export routine
SELECT CASE( SIO%HASH(2) )

 CASE(init_)            ; CALL MSHinit           ( SIO , Mesh , fdbk )
 CASE(vert_)            ; CALL MSHvert           ( SIO , Mesh , fdbk )
 CASE(face_)            ; CALL MSHface           ( SIO , Mesh , fdbk )
 CASE(cell_)            ; CALL MSHcell           ( SIO , Mesh , fdbk )
 CASE(block_)           ; CALL MSHblock          ( SIO , Mesh , fdbk )
 CASE(domain_)          ; CALL MSHdomain         ( SIO , Mesh , fdbk )
 CASE(test_)            ; CALL MSHtest           ( SIO , Mesh , fdbk )
 CASE(cellbased_)       ; CALL MSHcellbased      ( SIO , Mesh , fdbk )
 CASE(split_)           ; CALL MSHsplit          ( SIO , Mesh , fdbk )
 CASE(interpgrid_)      ; CALL MSHinterpgrid     ( SIO , Mesh , fdbk )
 CASE(movevert_)        ; CALL MSHmovevert       ( SIO , Mesh , fdbk )
 CASE(pertvert_)        ; CALL MSHpertvert       ( SIO , Mesh , fdbk )
 CASE(rotvert_)         ; CALL MSHrotvert        ( SIO , Mesh , fdbk )
 CASE(freezevert_)      ; CALL MSHfreezevert     ( SIO , Mesh , fdbk )
 CASE(gravitate_)       ; CALL MSHgravitate      ( SIO , Mesh , fdbk )
 CASE(scale_)           ; CALL MSHscale          ( SIO , Mesh , fdbk )
 CASE(addccverts_)      ; CALL MSHaddccverts     ( SIO , Mesh , fdbk )
 CASE(randseed_)        ; CALL MSHrandseed       ( SIO , Mesh , fdbk )
 CASE(splitface_)       ; CALL MSHsplitface      ( SIO , Mesh , fdbk )
 CASE(splitfaces_)      ; CALL MSHsplitfaces     ( SIO , Mesh , fdbk )
 CASE(orientboundary_)  ; CALL MSHorientboundary ( SIO , Mesh , fdbk )
 CASE(finalize_)        ; CALL MSHfinalize       ( SIO , Mesh , fdbk )
 CASE(rename_)          ; CALL MSHrename         ( SIO , Mesh , fdbk )
 CASE(splitplane_)      ; CALL MSHsplitplane     ( SIO , Mesh , fdbk )
 CASE(move_)            ; CALL MSHmove           ( SIO , Mesh , fdbk )
 CASE(crackle_)         ; CALL MSHcrackle        ( SIO , Mesh , fdbk )
 CASE(write_)           ; CALL MSHwrite          ( SIO , Mesh , fdbk )
 CASE DEFAULT
   VS = COMMAND_NOT_RECOGNIZED(SIO,mod_,proc_,"")
   CALL UPDATE(fdbk_error,fdbk,s=STR(VS))
   VS = ""

END SELECT

!!--end--
END SUBROUTINE


SUBROUTINE MSHsplitplane(SIO , Mesh ,fdbk)
!!#### PURPOSE
!! Split the whole mesh by a plane.

USE FUN_xyPLANE                           !!((05-B-FUN_xyPLANE.f90))

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <SIO>
!! * mesh object <Mesh>
TYPE(TYPE_SIO) ,POINTER       :: SIO
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
REAL(KIND_MSH) :: P2(Mesh%Ndim,2),Pn(Mesh%Ndim+1)

!!--begin--

!! Writing action.
!IF( Writing(SIO) )THEN
!
!END IF

!! Arguments.
CALL BEGIN_ARGUMENTS( SIO , (/"P1","P2"/) , fdbk , Optional=(/.false.,.false./),NSubArg=(/Mesh%NDim,Mesh%NDim/))
CALL ARGUMENT( SIO , P2(:,1) , fdbk )
CALL ARGUMENT( SIO , P2(:,2) , fdbk )
CALL END_ARGUMENTS( SIO , fdbk )

!! Reading action.
IF( Reading(SIO) )THEN
 Pn = xyPLANE_P2( P2 )
 CALL SPLIT_Mesh_Pn( Mesh , Pn )
END IF
!!--end--
END SUBROUTINE


!!### SUBROUTINE >>MSHwrite<<
SUBROUTINE MSHwrite(SIO,Mesh,fdbk)
!!#### PURPOSE
!! Write the mesh to a file in its most basic form.

!!#### REQUIRED INPUT/OUTPUT
!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_SIO) ,POINTER       :: SIO
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### OPTIONAL INPUT
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
INTEGER :: k,j,i
TYPE(varying_string)  :: file
TYPE(TYPE_SIO) ,POINTER :: sio_write

!!--begin--
IF( Reading(SIO) )THEN

    !! Arguments.
    CALL BEGIN_ARGUMENTS( SIO , (/"file"/) , fdbk , Optional=(/.false./) , &
    NSubArg=(/0/) )
    CALL ARGUMENT( SIO , file , fdbk )
    CALL END_ARGUMENTS( SIO , fdbk )
    CALL CONNECT(sio_write,&
                 FILE    = STR(file)   ,&
                 ACTION  = "Write"     ,&
                 FORM    = "Formatted" ,&
                 ACCESS  = "Sequential",&
                 fdbk    = fdbk )
    sio_write%Comment      = "!"
    sio_write%Continue     = "&"
    sio_write%Stop         = "#"
    sio_write%Ignore       = " "
    sio_write%CmdBeg       = "\"
    sio_write%ArgBeg       = "{"
    sio_write%ArgEnd       = "}"
    sio_write%ArgDelim     = "}{"
    sio_write%SubArgDelim  = ","
    sio_write%ArgDefine    = "="
    sio_write%Argdefault   = "*"
    sio_write%StrTag       = '"'
    sio_write%LEN_ArgDelim = 2

ELSE IF( Writing(SIO) )THEN
    sio_write=>SIO
END IF

DO k=1,Mesh%NVerts
    sio_write%cmd = "MSHvert"
    CALL WRITE_COMMAND(sio_write,fdbk)
    CALL MSHvert(sio_write,Mesh,fdbk,kCopy=k)
END DO

DO j=1,Mesh%NFaces
    sio_write%cmd = "MSHface"
    CALL WRITE_COMMAND(sio_write,fdbk)
    CALL MSHface(sio_write,Mesh,fdbk,jCopy=j)
END DO

DO i=1,Mesh%NCells
    sio_write%cmd = "MSHcell"
    CALL WRITE_COMMAND(sio_write,fdbk)
    CALL MSHcell(sio_write,Mesh,fdbk,iCopy=i)
END DO

IF( Reading(SIO) )THEN
    CALL DISCONNECT(sio_write,fdbk)
ELSE IF( Writing(SIO) )THEN
    sio_write=>null()
END IF

!!--end--
END SUBROUTINE MSHwrite

SUBROUTINE MSHinterpgrid( SIO , Mesh , fdbk )
!!#### PURPOSE
!! Setup a grid (an orthogonal structured mesh) to use
!! with unstructured meshes to interpolate scattered
!! values to.  Used exclusively for plotting solutions
!! and outputting "Grid" versions of solutions.

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <SIO>
!! * mesh object <Mesh>
TYPE(TYPE_SIO) ,POINTER       :: SIO
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
INTEGER :: N(NUM_Dimensions_Mesh(Mesh))

!!--begin--
!reading action: initialize before reading
IF( Reading(SIO) )THEN
 CALL ModifyInterpGrid(Mesh,Init=.TRUE.)
END IF

!! Arguments.
CALL BEGIN_ARGUMENTS( SIO , (/"N"/) , fdbk , Optional=(/.false./) , &
  NSubArg=(/NUM_Dimensions_Mesh(Mesh)/) )
CALL ARGUMENT( SIO , N , fdbk )
CALL END_ARGUMENTS( SIO , fdbk )

!reading action: setup the grid
IF( Reading(SIO) )THEN
 CALL ModifyInterpGrid(Mesh,Nx=N(1),Ny=N(2))
END IF

!writing action: initialize after writing
IF( Writing(SIO) )THEN
 CALL ModifyInterpGrid(Mesh,Init=.TRUE.)
END IF

!!--end--
END SUBROUTINE






SUBROUTINE MSHcellbased( SIO , Mesh , fdbk )
!!#### PURPOSE
!! Make a mesh cell-based.

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <SIO>
!! * mesh object <Mesh>
TYPE(TYPE_SIO) ,POINTER       :: SIO
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
LOGICAL :: TorF
LOGICAL :: Reorder

!!--begin--
!! Writing action.
IF( Writing(SIO) )THEN
 TorF = IsCellBased(Mesh)
END IF

!! Arguments.
CALL BEGIN_ARGUMENTS( SIO , (/"TorF   ",&
                              "Reorder"/) , fdbk , &
  Optional=(/.true.,.true./),NSubArg=(/0,0/))
CALL ARGUMENT( SIO , TorF , fdbk , default=.true. )
CALL ARGUMENT( SIO , Reorder , fdbk , default=.true. )
CALL END_ARGUMENTS( SIO , fdbk )

!! Reading action.
IF( Reading(SIO) )THEN
 IF( TorF )THEN
  CALL ENSURE_Mesh_Cellbased(Mesh,Reorder=Reorder)
 END IF
END IF

!!--end--
END SUBROUTINE



SUBROUTINE MSHfinalize( SIO , Mesh , fdbk )
!!#### PURPOSE
!! Finalize a mesh.

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <SIO>
!! * mesh object <Mesh>
TYPE(TYPE_SIO) ,POINTER       :: SIO
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
LOGICAL :: TorF

!!--begin--

!! Arguments.
CALL BEGIN_ARGUMENTS( SIO , (/"TorF"/) , fdbk , Optional=(/.true./),NSubArg=(/0/))
CALL ARGUMENT( SIO , TorF , fdbk , default=.true. )
CALL END_ARGUMENTS( SIO , fdbk )

!! Reading action.
IF( Reading(SIO) )THEN
 IF( TorF )THEN
  CALL FINALIZE_Mesh(Mesh)
 END IF
END IF

!WRITE(*,*)'after MSHfinalize'
!CALL PRINT_Mesh(Mesh)

!!--end--
END SUBROUTINE


SUBROUTINE MSHorientboundary( SIO , Mesh , fdbk )
!!#### PURPOSE
!! Orient mesh boundary.

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <SIO>
!! * mesh object <Mesh>
TYPE(TYPE_SIO) ,POINTER       :: SIO
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
LOGICAL :: TorF

!!--begin--

!! Arguments.
CALL BEGIN_ARGUMENTS( SIO , (/"TorF"/) , fdbk , Optional=(/.true./),NSubArg=(/0/))
CALL ARGUMENT( SIO , TorF , fdbk , default=.true. )
CALL END_ARGUMENTS( SIO , fdbk )

!! Reading action.
IF( Reading(SIO) )THEN
 IF( TorF )THEN
  CALL OrientBoundary(Mesh)
 END IF
END IF

!!--end--
END SUBROUTINE



SUBROUTINE MSHaddccverts( SIO , Mesh , fdbk )
!!#### PURPOSE
!! Add cell centroids to the list of Vertices.

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <SIO>
!! * mesh object <Mesh>
TYPE(TYPE_SIO) ,POINTER       :: SIO
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
LOGICAL :: TorF
INTEGER :: j,Nd
INTEGER,POINTER :: j_(:),k_(:)
REAL(KIND_MSH),ALLOCATABLE :: Pn(:),P(:),U(:)

!!--begin--

!! Arguments.
CALL BEGIN_ARGUMENTS( SIO , (/"TorF"/) , fdbk , Optional=(/.true./) , NSubArg=(/0/) )
CALL ARGUMENT( SIO , TorF , fdbk , default=.false. )
CALL END_ARGUMENTS( SIO , fdbk )

!! Reading action.
IF( Reading(SIO) )THEN
 IF( TorF )THEN
  CALL ADD_CellCentroidVerts(Mesh)
  !Nd = NUM_Dimensions_Mesh(Mesh)
  !ALLOCATE( P(Nd) , U(Nd) , Pn(Nd+1) )
  !DO j=1,NUM_Faces_Mesh(Mesh)
  ! U = FaceNormal(Mesh,j)
  ! U = xyPERPCCW_U( U )
  ! P = FaceCentroid(Mesh,j)
  ! Pn = xyPLANE_PV( P , U )

  ! NULLIFY( j_,k_)
  ! TorF = SPLIT_Face( Mesh , j , Pn , j_ , k_ )
  ! DEALLOCATE( j_,k_ )
  !END DO
  !DEALLOCATE( Pn , U , P )
 END IF
END IF

!!--end--
END SUBROUTINE



SUBROUTINE MSHinit( SIO , Mesh , fdbk )
!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <SIO>
!! * mesh object <Mesh>
TYPE(TYPE_SIO) ,POINTER       :: SIO
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
INTEGER :: NDim,MeshType

!!--begin--
!! Setup.
IF( Writing(SIO) )THEN
 NDim = NUM_Dimensions_Mesh(Mesh)
 CALL DESTROY( Mesh )
END IF

!! Arguments.
CALL BEGIN_ARGUMENTS( SIO , (/"NDim"/) , fdbk )
CALL ARGUMENT( SIO , NDim , fdbk )
CALL END_ARGUMENTS( SIO , fdbk )

!! Wrapup.
IF( Reading(SIO) )THEN
 MeshType = INDEXa( KEY_MeshType , "Unstructured" )
 CALL CREATE( Mesh , NDim , MeshType )
END IF

!!--end--
END SUBROUTINE


SUBROUTINE MSHvert( SIO , Mesh , fdbk , kCopy )
!!#### PURPOSE
!! Defines the \MSHvert card which adds a Vertex to the mesh
!! object and adds an entry in the <%VertLabels> component of <Mesh>.

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <SIO>
!! * mesh object <Mesh>
TYPE(TYPE_SIO) ,POINTER       :: SIO
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk
INTEGER        ,OPTIONAL,INTENT(IN)    :: kCopy

!!#### LOCAL VARIABLES
CHARACTER(LEN_MESH_LABEL) :: VertLabel
REAL(KIND_MSH)       :: Coords(1:NUM_Dimensions_Mesh(Mesh))
INTEGER              :: k

!!--begin--
!! Writing setup.
IF( Writing(SIO) )THEN
 IF( PRESENT(kCopy) )THEN
  Coords = Vert(Mesh,kCopy)
  VertLabel = Mesh%VertLabels(kCopy)
 ELSE
  Coords = REMOVE_vert_Mesh( Mesh , NUM_Verts_Mesh(Mesh) , VertLabel )
 END IF
END IF

!! Arguments.
CALL BEGIN_ARGUMENTS( SIO , (/"VertLabel",&
                              "Coords   " /) , fdbk , NSubArg=(/0,NUM_Dimensions_Mesh(Mesh)/) )
CALL ARGUMENT( SIO , VertLabel , fdbk )
CALL ARGUMENT( SIO , Coords    , fdbk )
CALL END_ARGUMENTS( SIO , fdbk )

!! Reading wrapup.
IF( Reading(SIO) )THEN
 k = ADD_vert_Mesh( Mesh , Coords , VertLabel )
END IF

!!--end--
END SUBROUTINE



SUBROUTINE MSHscale( SIO , Mesh , fdbk )
!!#### PURPOSE
!! Defines the \MSHscale card which scales the
!! mesh by a factor.

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <SIO>
!! * mesh object <Mesh>
TYPE(TYPE_SIO) ,POINTER       :: SIO
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
REAL(KIND_MSH) :: scale(Mesh%NDim)

!!--begin--
!! Writing setup.
IF( Writing(SIO) )THEN
 CALL STOP(s="There is no way to output the MSHscale card!")
END IF

!! Arguments.
CALL BEGIN_ARGUMENTS( SIO , (/"scale"/) , fdbk , NSubArg=(/Mesh%Ndim/))
CALL ARGUMENT( SIO , scale , fdbk )
CALL END_ARGUMENTS( SIO , fdbk )

!! Reading wrapup.
IF( Reading(SIO) )THEN
 CALL SCALE_Mesh(Mesh,scale)
END IF

!!--end--
END SUBROUTINE


SUBROUTINE MSHmovevert( SIO , Mesh , fdbk )
!!#### PURPOSE
!! Defines the \MSHmovevert card which moves a Vertex in the mesh
!! object.

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <SIO>
!! * mesh object <Mesh>
TYPE(TYPE_SIO) ,POINTER       :: SIO
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
CHARACTER(LEN_MESH_LABEL) :: VertLabel
REAL(KIND_MSH)       :: Vector(1:NUM_Dimensions_Mesh(Mesh))
INTEGER              :: k

!!--begin--
!! Writing setup.
IF( Writing(SIO) )THEN
 CALL Stop(s="MSHmovevert has no output equivalent!")
END IF

!! Arguments.
CALL BEGIN_ARGUMENTS( SIO , (/"VertLabel",&
                              "Vector   " /) , fdbk , NSubArg=(/0,NUM_Dimensions_Mesh(Mesh)/) )
CALL ARGUMENT( SIO , VertLabel , fdbk )
CALL ARGUMENT( SIO , Vector    , fdbk )
CALL END_ARGUMENTS( SIO , fdbk )

!! Reading wrapup.
IF( Reading(SIO) )THEN
 !search for the VertLabel
 k = INDEXa( Mesh%VertLabels , VertLabel , CASESEN=.FALSE. )

 !print a warning message
 IF( k==0 )THEN
  CALL Pause(s="in command MSHmovevert, VertLabel="//TRIM(VertLabel)//" was not found&
    &in the mesh structure.  <PRESS ENTER>")
 !move the Vert---caution, no check is made to see if this results
 !in a garbage mesh
 ELSE

  !only move the Vert if it is NOT in the frozen Vert set
  IF( FIND_IN_SET(FrozenVertSet,k)==0 )THEN
   Mesh%Verts(:,k) = Mesh%Verts(:,k) + Vector
   CALL UPDATE_Mesh(Mesh,klist=(/k/))
  END IF

 END IF
END IF

!!--end--
END SUBROUTINE



SUBROUTINE MSHmove( SIO , Mesh , fdbk )
!!#### PURPOSE
!! Defines the \MSHmove card which moves a Vertex in the mesh
!! object.

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <SIO>
!! * mesh object <Mesh>
TYPE(TYPE_SIO) ,POINTER       :: SIO
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
CHARACTER(LEN_MESH_LABEL) :: VertLabel
REAL(KIND_MSH)       :: Vector(1:NUM_Dimensions_Mesh(Mesh))
INTEGER              :: k

!!--begin--
!! Writing setup.
IF( Writing(SIO) )THEN
 CALL Stop(s="MSHmove has no output equivalent!")
END IF

!! Arguments.
CALL BEGIN_ARGUMENTS( SIO , (/"Vector"/) , fdbk , NSubArg=(/NUM_Dimensions_Mesh(Mesh)/) )
CALL ARGUMENT( SIO , Vector    , fdbk )
CALL END_ARGUMENTS( SIO , fdbk )

!! Reading wrapup.
IF( Reading(SIO) )THEN
 CALL TRANSLATE_Mesh(Mesh,Vector)
END IF

!!--end--
END SUBROUTINE




SUBROUTINE MSHfreezevert( SIO , Mesh , fdbk )
!!#### PURPOSE
!! Defines the \MSHfreezevert card which freezes a Vertex in the mesh
!! object (cannot be perturbed or moved by later operations).

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <SIO>
!! * mesh object <Mesh>
TYPE(TYPE_SIO) ,POINTER       :: SIO
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
REAL(KIND_MSH)       :: Pos(1:NUM_Dimensions_Mesh(Mesh))
INTEGER              :: k,n
INTEGER,POINTER      :: VertSet(:)
CHARACTER(32)        :: VertSpec

!!--begin--

!! Writing setup.
IF( Writing(SIO) )THEN
 CALL Stop(s="MSHfreezevert has no output equivalent---but should!")
END IF

!! Arguments.
CALL BEGIN_ARGUMENTS( SIO , (/"Pos","VertSpec"/) , fdbk , NSubArg=(/NUM_Dimensions_Mesh(Mesh),0/),&
  Optional=(/.TRUE.,.TRUE./) )
CALL ARGUMENT( SIO , Pos    , fdbk, Default=(/0.d0,0.d0/) )
CALL ARGUMENT( SIO , VertSpec  , fdbk , Default='')
CALL END_ARGUMENTS( SIO , fdbk )

!! Datablock.

!! * get the set of Verts described
NULLIFY( VertSet )
IF( LEN_TRIM(VertSpec)/=0 )THEN
  CALL DATABLOCK_Vertspec( SIO , Mesh , VertSpec , VertSet , fdbk )
ELSE
 !search for the Vert by position [hack] the tolerance for now
 k = GET_Vert(Mesh,Vert=Pos,tol=1.d-12)
 IF( k/=0 )THEN
    ALLOCATE(VertSet(1))
    VertSet(1)=k
 END IF
END IF

!! Reading wrapup.
IF( Reading(SIO) )THEN
    !! * freeze the Verts
    IF( .NOT.ASSOCIATED(VertSet) )THEN
        CALL UPDATE(fdbk_comment,fdbk,s="[[MSH]] Processing command MSHfreezevert, &
        &the VertSpec="//TRIM(VertSpec)//" led to an empty set of Verts to freeze.")
        CALL ADD_TO_SET( FrozenVertSet , k )
    ELSE
      DO n=1,SIZE(VertSet)
          CALL ADD_TO_SET( FrozenVertSet , VertSet(n) )
      END DO
      DEALLOCATE( VertSet )
    END IF
END IF

!!--end--
END SUBROUTINE



SUBROUTINE MSHrandseed( SIO , Mesh , fdbk )
!!#### PURPOSE
!! Allow the user to change/set the random number seed
!! in order to get the same random mesh every time.

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <SIO>
!! * mesh object <Mesh>
TYPE(TYPE_SIO) ,POINTER       :: SIO
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
INTEGER :: k
INTEGER,ALLOCATABLE :: randseed(:)
INTEGER :: randseed0

!!--begin--
CALL RANDOM_SEED(size=k)
ALLOCATE( randseed(k) )

!! Writing setup.
IF( Writing(SIO) )THEN
 CALL RANDOM_SEED( get=randseed )
 randseed0 = randseed(1)
END IF

!! Arguments.
CALL BEGIN_ARGUMENTS( SIO , (/"randseed"/) , fdbk , &
                            NSubArg=(/0/) , &
                            Optional=(/.FALSE./) )
CALL ARGUMENT( SIO , randseed0 , fdbk )
CALL END_ARGUMENTS( SIO , fdbk )

IF( Reading(SIO) )THEN
 randseed = HUGE(randseed0)-1
 randseed(1) = randseed0
 CALL RANDOM_SEED( put=randseed )
END IF

DEALLOCATE( randseed )

!!--end--
END SUBROUTINE


SUBROUTINE MSHpertvert( SIO , Mesh , fdbk )
!!#### PURPOSE
!! Defines the \MSHpertvert card which perturbs a Vertex in the mesh
!! object.

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <SIO>
!! * mesh object <Mesh>
TYPE(TYPE_SIO) ,POINTER       :: SIO
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
CHARACTER(LEN_MESH_LABEL) :: VertSpec
REAL(KIND_MSH)        :: Radius(1:2),Polar(1:2),Azimuthal(1:2)
INTEGER,POINTER      :: VertSet(:)

!!--begin--
!! Writing setup.
IF( Writing(SIO) )THEN
 CALL Stop(s="MSHpertvert has no output equivalent!")
END IF

!! Arguments.
CALL BEGIN_ARGUMENTS( SIO , (/"VertSpec         "   , &
                              "l_Radius(len)    "   , &
                              "l_Polar(deg)     "   , &
                              "l_Azimuthal(deg) "/) , fdbk , &
                            NSubArg=(/0,2,2,2/) , &
                            Optional=(/.FALSE.,.FALSE.,.TRUE.,.TRUE./) )
CALL ARGUMENT( SIO , VertSpec  , fdbk )
CALL ARGUMENT( SIO , Radius     , fdbk )
CALL ARGUMENT( SIO , Polar      , fdbk , Default= (/ c_0      ,c_2_times_PI/) )
CALL ARGUMENT( SIO , Azimuthal  , fdbk , Default= (/-c_PI_by_2,c_PI_by_2   /) )
CALL END_ARGUMENTS( SIO , fdbk )

!! Datablock.

!! * get the set of Verts described
NULLIFY( VertSet )
CALL DATABLOCK_Vertspec( SIO , Mesh , VertSpec , VertSet , fdbk )
!! * perturb the Verts
IF( .NOT.ASSOCIATED(VertSet) )THEN
 CALL UPDATE(fdbk_comment,fdbk,s="[[MSH]] Processing command MSHpertvert, &
   &the VertSpec="//TRIM(VertSpec)//" led to an empty set of Verts to perturb.")
ELSE
 CALL PERTURB_Verts( Mesh , VertSet , Radius , Polar , Azimuthal , FrozenVertSet )
 DEALLOCATE( VertSet )
END IF

!!--end--
END SUBROUTINE


SUBROUTINE MSHcrackle( SIO , Mesh , fdbk )
!!#### PURPOSE
!! Defines the \MSHcrackle card which uses gravitate
!! in specific cells.

USE FUN_Random

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <SIO>
!! * mesh object <Mesh>
TYPE(TYPE_SIO) ,POINTER       :: SIO
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
CHARACTER(LEN_MESH_LABEL) :: CellSpec
INTEGER,POINTER      :: CellSet(:),VertSet(:)
REAL(KIND_MSH) :: GravConst,Center(2),x
INTEGER :: n,i,m,iRepeat,NRepeat
LOGICAL,POINTER :: VisitedCell(:)

!!--begin--

!! Writing setup.
IF( Writing(SIO) )THEN
 CALL Stop(s="The <MSHcrackle> card has no meaning on output!")
END IF

!! Arguments.
CALL BEGIN_ARGUMENTS( SIO , (/"CellSpec      ",&
                              "GravConst     ",&
                              "NRepeat       "/),&
                             fdbk , &
                             NSubArg=(/0,0,0/),&
                             Optional=(/.FALSE.,.FALSE.,.TRUE./) )
CALL ARGUMENT( SIO , CellSpec   , fdbk )
CALL ARGUMENT( SIO , GravConst , fdbk )
CALL ARGUMENT( SIO , NRepeat , fdbk, Default=1 )
CALL END_ARGUMENTS( SIO , fdbk )

NULLIFY( CellSet )
CALL DATABLOCK_CellSpec  ( SIO , Mesh , CellSpec , CellSet , fdbk )

NULLIFY(VisitedCell)
ALLOCATE(VisitedCell(SIZE(CellSet)))

DO IRepeat=1,NRepeat

    VisitedCell=.FALSE.

    DO n=1,SIZE(CellSet)

        !visit cells in random order
        m=Random( (/1,SIZE(CellSet)+1/) )
        DO WHILE(VisitedCell(m))
            m=m+1
            IF( m>SIZE(CellSet) )m=1
        END DO
        VisitedCell(m)=.TRUE.
        i=CellSet(m)
        
        !get the verts
        NULLIFY( VertSet )
        CALL GET_CellVertBoundarySet(Mesh,i,VertSet)
        IF( .NOT.ASSOCIATED(VertSet) )CYCLE
        Center=CellCentroid(Mesh,i)

        !perturb
        CALL GRAVITATE_Verts( Mesh , VertSet , Center , GravConst , FrozenVertSet, MoveType='Linear' )
        
        DEALLOCATE( VertSet )

    END DO

END DO

DEALLOCATE( CellSet,VisitedCell )

!!--end--
END SUBROUTINE


SUBROUTINE MSHgravitate( SIO , Mesh , fdbk )
!!#### PURPOSE
!! Defines the \MSHgravitate card which sucks.

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <SIO>
!! * mesh object <Mesh>
TYPE(TYPE_SIO) ,POINTER       :: SIO
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
CHARACTER(LEN_MESH_LABEL) :: VertSpec
REAL(KIND_MSH)       :: Center(2),GravConst
INTEGER,POINTER      :: VertSet(:)

!!--begin--
!! Writing setup.
IF( Writing(SIO) )THEN
 CALL Stop(s="MSHgravitate has no output equivalent!")
END IF

!! Arguments.
CALL BEGIN_ARGUMENTS( SIO , (/"VertSpec  "   , &
                              "Center    "   , &
                              "GravConst "   /) , fdbk , &
                            NSubArg=(/0,Mesh%NDim,0/) , &
                            Optional=(/.FALSE.,.FALSE.,.FALSE./) )
CALL ARGUMENT( SIO , VertSpec  , fdbk )
CALL ARGUMENT( SIO , Center     , fdbk )
CALL ARGUMENT( SIO , GravConst  , fdbk )
CALL END_ARGUMENTS( SIO , fdbk )

!! Datablock.

!! * get the set of Verts described
NULLIFY( VertSet )
CALL DATABLOCK_Vertspec( SIO , Mesh , VertSpec , VertSet , fdbk )
!! * perturb the Verts
IF( .NOT.ASSOCIATED(VertSet) )THEN
 CALL UPDATE(fdbk_comment,fdbk,s="[[MSH]] Processing command MSHpertvert, &
   &the VertSpec="//TRIM(VertSpec)//" led to an empty set of Verts to perturb.")
ELSE
 CALL GRAVITATE_Verts( Mesh , VertSet , Center , GravConst , FrozenVertSet )
 DEALLOCATE( VertSet )
END IF

!!--end--
END SUBROUTINE




SUBROUTINE MSHrename( SIO , Mesh , fdbk )
!!#### PURPOSE
!! Renames the Verts/faces/cells of a mesh.

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <SIO>
!! * mesh object <Mesh>
TYPE(TYPE_SIO) ,POINTER       :: SIO
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
LOGICAL :: RenameC,RenameF,RenameV

!!--begin--

!! Writing setup.
IF( Writing(SIO) )THEN
 CALL Stop(s="MSHrename has no output equivalent!")
END IF

!! Arguments.
CALL BEGIN_ARGUMENTS( SIO , (/"Cells"   , &
                              "Faces"   , &
                              "Verts"   /) , fdbk , &
                            NSubArg=(/0,0,0/) , &
                            Optional=(/.TRUE.,.TRUE.,.TRUE./) )
CALL ARGUMENT( SIO , RenameC  , fdbk , Default=.FALSE. )
CALL ARGUMENT( SIO , RenameF  , fdbk , Default=.FALSE. )
CALL ARGUMENT( SIO , RenameV  , fdbk , Default=.FALSE. )
CALL END_ARGUMENTS( SIO , fdbk )


!! Reading wrapup.
IF( Reading(SIO) )THEN
 IF( RenameC )THEN
  CALL RENAME_Cells(Mesh)
 END IF
 IF( RenameF )THEN
  CALL RENAME_Faces(Mesh)
 END IF
 IF( RenameV )THEN
  CALL RENAME_Verts(Mesh)
 END IF
END IF

!!--end--
END SUBROUTINE


SUBROUTINE MSHrotvert( SIO , Mesh , fdbk )
!!#### PURPOSE
!! Defines the \MSHrotvert card which rotates a Vertex in the mesh
!! object.

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <SIO>
!! * mesh object <Mesh>
TYPE(TYPE_SIO) ,POINTER       :: SIO
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
CHARACTER(LEN_MESH_LABEL) :: VertSpec
REAL(KIND_MSH)       :: Center(1:2),Polar(1:2),Azimuthal(1:2),spiral
INTEGER,POINTER      :: VertSet(:)

!!--begin--
!! Writing setup.
IF( Writing(SIO) )THEN
 CALL Stop(s="MSHpertvert has no output equivalent!")
END IF

!! Arguments.
CALL BEGIN_ARGUMENTS( SIO , (/"VertSpec         "   , &
                              "Center           "   , &
                              "l_Polar(deg)     "   , &
                              "l_Azimuthal(deg) "   , &
                                                          "spiral           "/) , fdbk , &
                            NSubArg=(/0,2,2,2,0/) , &
                            Optional=(/.FALSE.,.FALSE.,.TRUE.,.TRUE.,.TRUE./) )
CALL ARGUMENT( SIO , VertSpec   , fdbk )
CALL ARGUMENT( SIO , Center     , fdbk )
CALL ARGUMENT( SIO , Polar      , fdbk , Default= (/ c_0      ,c_2_times_PI/) )
CALL ARGUMENT( SIO , Azimuthal  , fdbk , Default= (/-c_PI_by_2,c_PI_by_2   /) )
CALL ARGUMENT( SIO , Spiral     , fdbk , Default= 0._KIND_MSH )
CALL END_ARGUMENTS( SIO , fdbk )

!! Datablock.

!! * get the set of Verts described
NULLIFY( VertSet )
CALL DATABLOCK_Vertspec( SIO , Mesh , VertSpec , VertSet , fdbk )
!! * perturb the Verts
IF( .NOT.ASSOCIATED(VertSet) )THEN
 CALL UPDATE(fdbk_comment,fdbk,s="[[MSH]] Processing command MSHrotvert, &
   &the VertSpec="//TRIM(VertSpec)//" led to an empty set of Verts to perturb.")
ELSE
 CALL ROTATE_Verts( Mesh , VertSet , Center , Polar , Azimuthal , spiral )
 DEALLOCATE( VertSet )
END IF

!!--end--
END SUBROUTINE



SUBROUTINE MSHsplit( SIO , Mesh , fdbk )
!!#### PURPOSE
!! Split the mesh according to various elemental shapes.

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <SIO>
!! * mesh object <Mesh>
TYPE(TYPE_SIO) ,POINTER       :: SIO
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
CHARACTER(LEN_MESH_LABEL) :: CellSpec
CHARACTER(32)        :: SplitShape
INTEGER,POINTER      :: CellSet(:)
LOGICAL              :: ShiftToCenter
REAL(KIND_MSH)       :: Radius(2),Polar(2),Azimuthal(2)
REAL(KIND_MSH)       :: VertTol

!!--begin--
!! Writing setup.
IF( Writing(SIO) )THEN
 CALL Stop(s="The <MSHsplit> card has no meaning on output!")
END IF

!! Arguments.
CALL BEGIN_ARGUMENTS( SIO , (/"CellSpec      ",&
                              "SplitShape    ",&
                              "ShiftToCenter ",&
                              "l_Radius      ",&
                              "l_degPolar    ",&
                              "l_degAzimuthal",&
                              "VertTol       "/),&
                             fdbk , &
                             NSubArg=(/0,0,0,2,2,2,0/),&
                             Optional=(/.FALSE.,.FALSE.,.TRUE.,.TRUE.,.TRUE.,.TRUE.,.TRUE./) )
CALL ARGUMENT( SIO , CellSpec   , fdbk )
CALL ARGUMENT( SIO , SplitShape , fdbk )
CALL ARGUMENT( SIO , ShiftToCenter , fdbk , Default=.FALSE.)
CALL ARGUMENT( SIO , Radius     , fdbk , Default= (/ REAL( 0,KIND_MSH), REAL(0  ,KIND_MSH)/) )
CALL ARGUMENT( SIO , Polar      , fdbk , Default= (/ REAL( 0,KIND_MSH), REAL(360,KIND_MSH)/) )
CALL ARGUMENT( SIO , Azimuthal  , fdbk , Default= (/-REAL(90,KIND_MSH),+REAL(90 ,KIND_MSH)/) )
CALL ARGUMENT( SIO , VertTol , fdbk , Default=EPSILON(1._KIND_MSH))
CALL END_ARGUMENTS( SIO , fdbk )


!! Datablock.
!!  * determine the set of cells to split
NULLIFY( CellSet )
CALL DATABLOCK_CellSpec  ( SIO , Mesh , CellSpec , CellSet , fdbk )
!WRITE(*,*)CellSet
!!  * split those cells according to the shape
IF( Radius(1)/=REAL(0,KIND_MSH) .AND. Radius(2)/=REAL(0,KIND_MSH) )THEN
 CALL DATABLOCK_CellSplitShape( SIO , Mesh , SplitShape , &
   CellSet , ShiftToCenter, fdbk  , &
   Radius , Polar , Azimuthal , VertTol=VertTol )
ELSE
 CALL DATABLOCK_CellSplitShape( SIO , Mesh , SplitShape , &
   CellSet , ShiftToCenter , fdbk , VertTol=VertTol)
END IF

IF( ASSOCIATED(CellSet) )DEALLOCATE( CellSet )

!!--end--
END SUBROUTINE




SUBROUTINE MSHsplitfaces( SIO , Mesh , fdbk )
!!#### PURPOSE
!! Split the faces of the mesh according to various elemental shapes.

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <SIO>
!! * mesh object <Mesh>
TYPE(TYPE_SIO) ,POINTER       :: SIO
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
CHARACTER(LEN_MESH_LABEL) :: FaceSpec
CHARACTER(32)        :: SplitShape
INTEGER,POINTER      :: FaceSet(:)
LOGICAL              :: ShiftToCenter
REAL(KIND_MSH)       :: Radius(2),Polar(2),Azimuthal(2)

!!--begin--
!! Writing setup.
IF( Writing(SIO) )THEN
 CALL Stop(s="The <MSHsplitfaces> card has no meaning on output!")
END IF

!! Arguments.
CALL BEGIN_ARGUMENTS( SIO , (/"FaceSpec      ",&
                              "SplitShape    ",&
                              "ShiftToCenter ",&
                              "l_Radius      ",&
                              "l_degPolar    ",&
                              "l_degAzimuthal"/),&
                             fdbk , &
                             NSubArg=(/0,0,0,2,2,2/),&
                             Optional=(/.FALSE.,.FALSE.,.TRUE.,.TRUE.,.TRUE.,.TRUE./) )
CALL ARGUMENT( SIO , FaceSpec   , fdbk )
CALL ARGUMENT( SIO , SplitShape , fdbk )
CALL ARGUMENT( SIO , ShiftToCenter , fdbk , Default=.FALSE.)
CALL ARGUMENT( SIO , Radius     , fdbk , Default= (/ REAL( 0,KIND_MSH), REAL(0  ,KIND_MSH)/) )
CALL ARGUMENT( SIO , Polar      , fdbk , Default= (/ REAL( 0,KIND_MSH), REAL(360,KIND_MSH)/) )
CALL ARGUMENT( SIO , Azimuthal  , fdbk , Default= (/-REAL(90,KIND_MSH),+REAL(90 ,KIND_MSH)/) )
CALL END_ARGUMENTS( SIO , fdbk )


!! Datablock.
!!  * determine the set of cells to split
NULLIFY( FaceSet )
CALL DATABLOCK_FaceSpec  ( SIO , Mesh , FaceSpec   , FaceSet , fdbk )
!!  * split those cells according to the shape
IF( Radius(1)/=REAL(0,KIND_MSH) .AND. Radius(2)/=REAL(0,KIND_MSH) )THEN
 CALL DATABLOCK_FaceSplitShape( SIO , Mesh , SplitShape , FaceSet , ShiftToCenter, fdbk  , &
   Radius , Polar , Azimuthal )
ELSE
 CALL DATABLOCK_FaceSplitShape( SIO , Mesh , SplitShape , FaceSet , ShiftToCenter , fdbk )
END IF

IF( ASSOCIATED(FaceSet) )DEALLOCATE( FaceSet )

!!--end--
END SUBROUTINE



SUBROUTINE MSHsplitface( SIO , Mesh , fdbk )
!!#### PURPOSE
!! Split a face in the mesh according to various elemental shapes.

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <SIO>
!! * mesh object <Mesh>
TYPE(TYPE_SIO) ,POINTER       :: SIO
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
CHARACTER(LEN_MESH_LABEL) :: FaceLabel
INTEGER,POINTER      :: j_(:),k_(:)
REAL(KIND_MSH)       :: FaceCentroid(Mesh%Ndim),tol
INTEGER              :: j
LOGICAL              :: Split
REAL(KIND_MSH)       :: Pn_1(Mesh%NDim+1)
REAL(KIND_MSH)       :: Ps_1(Mesh%NDim,2)
REAL(KIND_MSH)       :: Default_FaceCentroid(Mesh%NDim)
!!--begin--

! initialize
NULLIFY( j_,k_)

Default_FaceCentroid = 0._KIND_MSH

!! Writing setup.
IF( Writing(SIO) )THEN
 CALL Stop(s="The <MSHsplitface> card has no meaning on output!")
END IF

!! Arguments.
CALL BEGIN_ARGUMENTS( SIO , (/"FaceLabel   ",&
                              "FaceCentroid",&
                              "tol         "/) , fdbk , NSubArg=(/0,Mesh%NDim,0/),&
                             Optional=(/.TRUE.,.TRUE.,.TRUE./) )
CALL ARGUMENT( SIO , FaceLabel   , fdbk , Default=" ")
CALL ARGUMENT( SIO , FaceCentroid , fdbk , Default=Default_FaceCentroid)
CALL ARGUMENT( SIO , tol , fdbk , Default=0.0_KIND_MSH)
CALL END_ARGUMENTS( SIO , fdbk )

!get data for the plane
CALL DATABLOCK( SIO , Ps_1 , fdbk )
Pn_1 = xyPLANE_P2( Ps_1 )

!if label is present
IF( SIO%present(1) )THEN
 j = GET_Face( Mesh , FaceLabel )
ELSE IF( SIO%present(2) )THEN
 j = GET_Face( Mesh , FaceCentroid=FaceCentroid , tol=tol )
ELSE
 j = 0
END IF

IF( j/=0 )THEN
 Split = SPLIT_Face( Mesh , j , Pn_1 , j_ , k_ )
 IF( ASSOCIATED(j_) )DEALLOCATE(j_)
 IF( ASSOCIATED(k_) )DEALLOCATE(k_)
 CALL FINALIZE_Mesh(Mesh)
END IF

!!--end--
END SUBROUTINE



SUBROUTINE MSHface( SIO , Mesh , fdbk , jCopy )
!!#### PURPOSE
!! Defines the \MSHface card which adds a face to
!! the mesh object and adds an entry to the <%FaceLabels>
!! component of <Mesh>.

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <SIO>
!! * mesh object <Mesh>
TYPE(TYPE_SIO) ,POINTER       :: SIO
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk
INTEGER        ,OPTIONAL,INTENT(IN)    :: jCopy

!!#### LOCAL VARIABLES
CHARACTER(LEN_MESH_LABEL) :: FaceLabel
CHARACTER(LEN_MESH_LABEL) :: VertLabel
INTEGER,POINTER      :: VertList(:)
INTEGER              :: k,n,j,jerr,j0,Nl,NVerts
INTEGER              :: FaceShape_
INTEGER,POINTER      :: EdgeShape(:)
INTEGER,POINTER      :: VL(:)
!!--begin--
NULLIFY( VertList, EdgeShape )

!! Writing setup.
IF( Writing(SIO) )THEN
 IF( PRESENT(jCopy) )THEN
  j = jCopy
 ELSE
  j =  NUM_Faces_Mesh(Mesh)
 END IF
 FaceShape_=  FaceShape_Mesh(Mesh,j) !FaceShape
 VL => ptr_VertList_Mesh(Mesh,j)
 NVerts = SIZE(VL)
 ALLOCATE( VertList(1:NVerts) )
 VertList  = VL
 VL => NULL()

 Nl = NUM_Edges_Mesh(Mesh,j)
 ALLOCATE(EdgeShape(1:Nl))
 EdgeShape =  EdgeShapes_Mesh(Mesh,j,Nl)
 IF( PRESENT(jCopy) )THEN
  FaceLabel = Mesh%FaceLabels(j)
 ELSE
  j0 = REMOVE_Face_Mesh(Mesh,j,FaceLabel) !FaceLabel
 END IF

END IF

!! Arguments.
CALL BEGIN_ARGUMENTS( SIO , (/"FaceLabel",&
                              "FaceShape",&
                              "NumVerts "/) ,fdbk)
CALL ARGUMENT( SIO , FaceLabel  , fdbk )
CALL ARGUMENT( SIO , FaceShape_ , fdbk , keys=KEY_FaceShape )
CALL ARGUMENT( SIO , NVerts     , fdbk )
CALL END_ARGUMENTS( SIO , fdbk )


!! Datablock.
CALL DATABLOCK( SIO , (/1,NVerts  /) , VertList  , fdbk , Keys=Mesh%VertLabels )
CALL DATABLOCK( SIO , (/1,NVerts-1/) , EdgeShape , fdbk , Keys=KEY_EdgeShape )

!! Reading wrapup.
IF( Reading(SIO) )THEN
 !![hack]
 !!using planar segment face always
 j =  ADD_Face_Ps(Mesh,VertList,EdgeShape,FaceLabel)
 DEALLOCATE( VertList,EdgeShape )
END IF

!!--end--
END SUBROUTINE



SUBROUTINE MSHcell( SIO , Mesh , fdbk , iCopy )
!!#### PURPOSE
!! Defines the \MSHcell card which adds a cell to the mesh
!! object and adds an entry in the <%CellLabels>.

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <SIO>
!! * mesh object <Mesh>
TYPE(TYPE_SIO)  ,POINTER       :: SIO
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk
INTEGER        ,OPTIONAL,INTENT(IN)    :: iCopy

!!#### LOCAL VARIABLES
CHARACTER(LEN_MESH_LABEL) :: CellLabel
INTEGER,POINTER      :: FaceList(:)
INTEGER,POINTER      :: FL(:)
INTEGER              :: i,NFaces,i0
INTEGER              :: CellShape_

!!--begin--
NULLIFY(FaceList,FL)

!! Writing setup.
IF( Writing(SIO) )THEN
 !either get the last one or the the specified one
 IF( PRESENT(iCopy) )THEN
  i = iCopy
 ELSE
  i =  NUM_Cells_Mesh(Mesh)
 END IF
 !get aux variables
 CellShape_=  CellShape_Mesh(Mesh,i)
 FL        => ptr_FaceList_Mesh(Mesh,i)
 NFaces    =  SIZE(FL)
 ALLOCATE( FaceList(NFaces) )
 FaceList  =  ABS( FL ) !because a negative index indicates opposite normals
 !do not remove the cell if we are just copying the mesh
 IF( PRESENT(iCopy) )THEN
  CellLabel = Mesh%CellLabels(i)
 ELSE
  i0 =  REMOVE_Cell_Mesh(Mesh,i,CellLabel)
 END IF
END IF

!! Arguments.
CALL BEGIN_ARGUMENTS( SIO , (/"CellLabel",&
                              "CellShape",&
                              "NFaces   " /) , fdbk )
CALL ARGUMENT( SIO , CellLabel , fdbk )
CALL ARGUMENT( SIO , CellShape_, fdbk , Keys=KEY_CellShape )
CALL ARGUMENT( SIO , NFaces    , fdbk )
CALL END_ARGUMENTS( SIO , fdbk )

!! Datablock.
CALL DATABLOCK( SIO , (/1,NFaces/) , FaceList , fdbk , Keys=Mesh%FaceLabels )

!! Wrapup.
IF( Reading(SIO) )THEN
 !![hack]
 !assume face assembly
 i = ADD_Cell_Jx( Mesh , FaceList , CellLabel )
 DEALLOCATE( FaceList  )
END IF

!!--end--
END SUBROUTINE



SUBROUTINE MSHdomain( SIO , Mesh , fdbk )
!!#### PURPOSE
!! Reads in a domain of a mesh.

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <SIO>
!! * mesh object <Mesh>
TYPE(TYPE_SIO) ,POINTER       :: SIO
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_="MSHdomain"

!!#### LOCAL VARIABLES
INTEGER                                         :: CellShape
REAL(KIND_MSH),DIMENSION(1:NUM_Dimensions_Mesh(Mesh)) :: P0,V1,V2,V3
REAL(KIND_MSH),DIMENSION(1:NUM_Dimensions_Mesh(Mesh)) :: V1_,V2_,V3_
REAL(KIND_MSH),DIMENSION(1:NUM_Dimensions_Mesh(Mesh)) :: scale
INTEGER                                         :: NDim
TYPE(varying_string) :: VS

!!--begin--
!! Setup.
NDim = NUM_Dimensions_Mesh(Mesh)

!! Arguments.
CALL BEGIN_ARGUMENTS(SIO,(/"CellShape",&
                          "P0       ",&
                          "V1       ",&
                          "V2       ",&
                          "V3       ",&
                          "scale    " /) , fdbk , &
                          NSubArg = (/0,&
                                     NDim,&
                                     NDim,&
                                     NDim,&
                                     NDim,&
                                     NDim /) , &
                          optional = (/.FALSE.,&
                                      .FALSE.,&
                                      .FALSE.,&
                                      .FALSE.,&
                                      .TRUE. ,&
                                      .TRUE.  /) )

CALL ARGUMENT( SIO , CellShape , fdbk , Keys=KEY_CellShape )

SELECT CASE( CellShape )
 CASE(Qs_,Qr_)
   CALL ARGUMENT( SIO , P0    , fdbk )
   CALL ARGUMENT( SIO , V1    , fdbk )
   CALL ARGUMENT( SIO , V2    , fdbk , default=SPREAD(0._KIND_MSH,1,Mesh%NDim) )
   CALL ARGUMENT( SIO , V3    , fdbk , default=SPREAD(0._KIND_MSH,1,Mesh%NDim) )
   CALL ARGUMENT( SIO , scale , fdbk , default=SPREAD(1._KIND_MSH,1,Mesh%NDim) )

   !! Apply scale.
   IF( Mesh%NDim>=1 )V1_= V1*scale(1)
   IF( Mesh%NDim>=2 )V2_= V2*scale(2)
   IF( Mesh%NDim>=3 )V3_= V3*scale(3)
   CALL CREATE_Domain_Qr( Mesh , P0,V1_,V2_,V3_)

 CASE DEFAULT
   VS = MODPROC(mod_,proc_)
   CALL UPDATE( fdbk_error , fdbk , s=STR(VS)//&
     "the CellShape specifier is not allowed.")
   VS = ""
END SELECT

CALL END_ARGUMENTS( SIO , fdbk )

!!--end--
END SUBROUTINE



SUBROUTINE MSHblock( SIO , Mesh , fdbk )
!!#### PURPOSE
!! Reads in a block of mesh.

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <SIO>
!! * mesh object <Mesh>
TYPE(TYPE_SIO) ,POINTER       :: SIO
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
INTEGER                                         :: MeshType
CHARACTER(LEN_MESH_LABEL)                            :: BlockLabel
REAL(KIND_MSH),DIMENSION(1:NUM_Dimensions_Mesh(Mesh)) :: P0,V1,V2,V3
REAL(KIND_MSH),DIMENSION(1:NUM_Dimensions_Mesh(Mesh)) :: V1_,V2_,V3_
REAL(KIND_MSH),DIMENSION(1:NUM_Dimensions_Mesh(Mesh)) :: scale
INTEGER                                         :: NDim

!!--begin--
!! Setup.
NDim = NUM_Dimensions_Mesh(Mesh)

!! Arguments.
CALL BEGIN_ARGUMENTS(SIO,(/"MeshType  ",&
                          "BlockLabel",&
                          "P0        ",&
                          "V1        ",&
                          "V2        ",&
                          "V3        ",&
                          "scale     " /) , fdbk , &
                          NSubArg = (/0,&
                                     0,&
                                     NDim,&
                                     NDim,&
                                     NDim,&
                                     NDim,&
                                     NDim /) , &
                          optional = (/.FALSE.,&
                                      .FALSE.,&
                                      .FALSE.,&
                                      .FALSE.,&
                                      .TRUE.,&
                                      .TRUE.,&
                                      .TRUE.  /) )
CALL ARGUMENT( SIO , MeshType  , fdbk , Keys=KEY_MeshType )
CALL ARGUMENT( SIO , BlockLabel, fdbk )
CALL ARGUMENT( SIO , P0        , fdbk )
CALL ARGUMENT( SIO , V1        , fdbk )
CALL ARGUMENT( SIO , V2        , fdbk , default=SPREAD(0._KIND_MSH,1,Mesh%NDim) )
CALL ARGUMENT( SIO , V3        , fdbk , default=SPREAD(0._KIND_MSH,1,Mesh%NDim) )
CALL ARGUMENT( SIO , scale     , fdbk , default=SPREAD(1._KIND_MSH,1,Mesh%NDim) )
CALL END_ARGUMENTS( SIO , fdbk )

!! Apply scale.
IF(NDim>=1)V1_ = V1*scale(1)
IF(NDim>=2)V2_ = V2*scale(2)
IF(NDim>=3)V3_ = V3*scale(3)

!! Datablock.
SELECT CASE( MeshType )
 CASE( MSH_Uniform    ) ; CALL DATABLOCK_MSHblock_Uniform   ( SIO , Mesh , BlockLabel , P0,V1_,V2_,V3_ , fdbk )
 CASE( MSH_Regular    ) ; CALL DATABLOCK_MSHblock_Regular   ( SIO , Mesh , BlockLabel , P0,V1_,V2_,V3_ , fdbk )
 CASE( MSH_Structured ) ; CALL DATABLOCK_MSHblock_Structured( SIO , Mesh , BlockLabel , P0,V1_,V2_,V3_ , fdbk )
 CASE( MSH_Randomized ) ; CALL DATABLOCK_MSHblock_Structured( SIO , Mesh , BlockLabel , P0,V1_,V2_,V3_ , fdbk , Randomized=.TRUE.)
 CASE( MSH_Saw        ) ; CALL DATABLOCK_MSHblock_Structured( SIO , Mesh , BlockLabel , P0,V1_,V2_,V3_ , fdbk , Saw=.TRUE.)
 !CASE( ZMesh_      ) ; CALL DATABLOCK_MSHblock_ZMesh     ( SIO , Mesh , BlockLabel , P0,V1_,V2_,V3_ , fdbk )
END SELECT

!!--end--
END SUBROUTINE



SUBROUTINE DATABLOCK_MSHblock_Uniform( SIO , Mesh , BlockLabel , &
  P0,V1,V2,V3 , fdbk )
!!#### PURPOSE
!! Reads in a uniform mesh block.

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <SIO>
!! * mesh object <Mesh>
TYPE(TYPE_SIO) ,POINTER       :: SIO
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
!! * label for the components of the mesh added in this procedure
!! * origin for mesh block <P0>
!! * the vector tracing the first primary edge of the block <V1>
!! * the vector tracing the second primary edge of the block <V2>
!! * the vector tracing the third primary edge of the block <V3>
CHARACTER(LEN=LEN_MESH_LABEL,KIND=KIND_S)          ,INTENT(IN) :: BlockLabel
REAL(KIND_MSH),DIMENSION(NUM_Dimensions_Mesh(Mesh)),INTENT(IN) :: P0,V1,V2,V3

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
INTEGER      ,DIMENSION(1:NUM_Dimensions_Mesh(Mesh)) :: Div
REAL(KIND_MSH),POINTER                           :: delx(:),dely(:),delz(:)
REAL(KIND_MSH),DIMENSION(1:NUM_Dimensions_Mesh(Mesh)) :: U1,U2,U3

!!--begin--
!get number of divisions (part1--simple don't need an auxiliary routine)
CALL DATABLOCK( SIO , Div , fdbk )

!get <delx,dely,delz> and <U1,U2,U3> from <Div>,<V1>,<V2>,<V3> (part2)
CALL DATABLOCK_MSHblock_Uni2(SIO,Div,P0,V1,V2,V3,&
  delx,dely,delz,U1,U2,U3,fdbk)

!setup the mesh block (part3)
CALL DATABLOCK_MSHblock_part3(Mesh,BlockLabel,Div,P0,U1,U2,U3 , &
  delx,dely,delz,fdbk )

!deallocate stuff
IF( ASSOCIATED(delx) )DEALLOCATE(delx)
IF( ASSOCIATED(dely) )DEALLOCATE(dely)
IF( ASSOCIATED(delz) )DEALLOCATE(delz)

!setup the grid representation
IF( SIZE(Div)==2 )THEN
 CALL ModifyInterpGrid(Mesh,NX=Div(1)+1,NY=Div(2)+1)
END IF

!!--end--
END SUBROUTINE



SUBROUTINE DATABLOCK_MSHblock_Regular( SIO , Mesh , BlockLabel , &
  P0,V1,V2,V3 , fdbk  )
!!#### PURPOSE
!! Reads in a regular mesh block.

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <SIO>
!! * mesh object <Mesh>
TYPE(TYPE_SIO)                      ,POINTER       :: SIO
TYPE(TYPE_Mesh)                     ,INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
CHARACTER(LEN=LEN_MESH_LABEL,KIND=KIND_S)            ,INTENT(IN)  :: BlockLabel
REAL(KIND_MSH),DIMENSION(1:NUM_Dimensions_Mesh(Mesh)),INTENT(IN) :: P0,V1,V2,V3

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
INTEGER      ,DIMENSION(1:NUM_Dimensions_Mesh(Mesh)) :: Div
REAL(KIND_MSH),DIMENSION(1:NUM_Dimensions_Mesh(Mesh)) :: U1,U2,U3
REAL(KIND_MSH),POINTER :: delx(:),dely(:),delz(:)

!!--begin--
!! Datablock.

!get number of divisions (part1--simple don't need an auxiliary routine)
CALL DATABLOCK( SIO , Div , fdbk )

!get <delx,dely,delz> and <U1,U2,U3> from <Div> (part2)
CALL DATABLOCK_MSHblock_Reg2(SIO,Div,P0,V1,V2,V3,&
  delx,dely,delz,U1,U2,U3,fdbk)

!setup the mesh block (part3)
CALL DATABLOCK_MSHblock_part3(Mesh,BlockLabel,Div,P0,U1,U2,U3 , &
  delx,dely,delz,fdbk )

!deallocate stuff
IF( ASSOCIATED(delx) )DEALLOCATE(delx)
IF( ASSOCIATED(dely) )DEALLOCATE(dely)
IF( ASSOCIATED(delz) )DEALLOCATE(delz)

!setup the grid representation
IF( SIZE(Div)==2 )THEN
 CALL ModifyInterpGrid(Mesh,NX=Div(1)+1,NY=Div(2)+1)
END IF

!!--end--
END SUBROUTINE


SUBROUTINE DATABLOCK_MSHblock_Structured( SIO , Mesh , BlockLabel , &
  P0,V1,V2,V3 , fdbk , Randomized , Saw )
!!#### PURPOSE
!! Reads in a structured mesh block.

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <SIO>
!! * mesh object <Mesh>
TYPE(TYPE_SIO)                      ,POINTER       :: SIO
TYPE(TYPE_Mesh)                     ,INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
CHARACTER(LEN=LEN_MESH_LABEL,KIND=KIND_S)            ,INTENT(IN) :: BlockLabel
REAL(KIND_MSH),DIMENSION(1:NUM_Dimensions_Mesh(Mesh)),INTENT(IN) :: P0,V1,V2,V3

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### OPTIONAL INPUT
LOGICAL,OPTIONAL :: Randomized,Saw

!!#### LOCAL VARIABLES
LOGICAL :: MSH_Randomized,MSH_Saw
INTEGER      ,DIMENSION(1:NUM_Dimensions_Mesh(Mesh)) :: Div
REAL(KIND_MSH),DIMENSION(1:NUM_Dimensions_Mesh(Mesh)) :: U1,U2,U3
REAL(KIND_MSH),POINTER :: delx(:),dely(:),delz(:),perts(:,:,:,:)

!!--begin--
!! Setup.
IF( PRESENT(Randomized) )THEN
 MSH_Randomized = Randomized
ELSE
 MSH_Randomized = .FALSE.
END IF

IF( PRESENT(Saw) )THEN
 MSH_Saw = Saw
ELSE
 MSH_Saw = .FALSE.
END IF


!get number of divisions (part1--simple don't need an auxiliary routine)
CALL DATABLOCK( SIO , Div , fdbk )
CALL DUMP(fdbk)

!get <delx,dely,delz> and <U1,U2,U3> from <Div> (part2)
CALL DATABLOCK_MSHblock_Reg2(SIO,Div,P0,V1,V2,V3,&
  delx,dely,delz,U1,U2,U3,fdbk)
CALL DUMP(fdbk)

!get the perturbations
IF( MSH_Randomized )THEN
 CALL DATABLOCK_MSHblock_randperts(SIO,Div,delx,dely,delz,perts,fdbk)
ELSEIF( MSH_Saw )THEN
 CALL DATABLOCK_MSHblock_sawperts(SIO,Div,delx,dely,delz,perts,fdbk)
ELSE
 CALL DATABLOCK_MSHblock_readperts(SIO,Div,perts,fdbk)
END IF
CALL DUMP(fdbk)

!setup the mesh block (part3)
CALL DATABLOCK_MSHblock_part3(Mesh,BlockLabel,Div,P0,U1,U2,U3 , &
  delx,dely,delz,fdbk,perts)
CALL DUMP(fdbk)

!deallocate stuff
IF( ASSOCIATED(delx) )DEALLOCATE(delx)
IF( ASSOCIATED(dely) )DEALLOCATE(dely)
IF( ASSOCIATED(delz) )DEALLOCATE(delz)

!setup the grid representation
IF( SIZE(Div)==2 )THEN
 CALL ModifyInterpGrid(Mesh,NX=Div(1)+1,NY=Div(2)+1)
END IF

!!--end--
END SUBROUTINE



SUBROUTINE DATABLOCK_MSHblock_readperts(SIO,Div,perts,fdbk)
!!#### PURPOSE
!! Gets perturbations in the mesh block (from a file, or
!! randomly).

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <SIO>
TYPE(TYPE_SIO),POINTER       :: SIO

!!#### REQUIRED OUTPUT
REAL(KIND_MSH),POINTER :: perts(:,:,:,:)

!!#### REQUIRED INPUT
INTEGER      ,INTENT(IN) :: Div(:)

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
INTEGER               :: kx,ky,kz

!!--begin--
!allocate
SELECT CASE( SIZE(Div) )
 CASE(1) ; ALLOCATE( perts(1,Div(1)+1,       1,       1) )
 CASE(2) ; ALLOCATE( perts(2,Div(1)+1,Div(2)+1,       1) )
 CASE(3) ; ALLOCATE( perts(3,Div(1)+1,Div(2)+1,Div(3)+1) )
END SELECT

!get perts
DO ky=1,SIZE(perts,3)
 DO kx=1,SIZE(perts,2)
  CALL DATABLOCK( SIO , perts(:,kx,ky,1) , fdbk )
 END DO
END DO

!!--end--
END SUBROUTINE


SUBROUTINE DATABLOCK_MSHblock_sawperts(SIO,Div,delx,dely,delz,perts,fdbk)
!!#### PURPOSE
!! Intelligently determine saw-tooth perturbations for a
!! mesh block.

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <SIO>
TYPE(TYPE_SIO),POINTER       :: SIO

!!#### REQUIRED INPUT
INTEGER       ,INTENT(IN) :: Div(:)
REAL(KIND_MSH),INTENT(IN) :: delx(:),dely(:),delz(:)

!!#### REQUIRED OUTPUT
REAL(KIND_MSH),POINTER :: perts(:,:,:,:)

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
INTEGER :: kz,ky,kx,Nx,Ny,Nz,NDim
LOGICAL :: Boundary_x,Boundary_y,Boundary_z
REAL(KIND_MSH) :: fp(1:SIZE(Div))

!!--begin--
!! Initialize numbers.
NDim = SIZE(Div)
Nx=1;Ny=1;Nz=1
IF(NDim>=1)Nx=Div(1)+1
IF(NDim>=2)Ny=Div(2)+1
IF(NDim>=3)Nz=Div(3)+1

!get the fraction for each direction
CALL DATABLOCK(SIO,fp,fdbk)

!allocate
ALLOCATE( perts(NDim,Nx,Ny,Nz) )
perts = 0._KIND_MSH


!! Initialize the random seed.
CALL RANDOM_SEED()

!! Enter loop.
DO kz=1,Nz
 DO ky=1,Ny
  DO kx=1,Nx

   !! Determine which boundaries the point is on
   Boundary_x = (kx==1 .OR. kx==Nx)
   Boundary_y = (ky==1 .OR. ky==Ny)
   Boundary_z = (kz==1 .OR. kz==Nz)

   IF( .NOT.(Boundary_x .OR. Boundary_y) )THEN
    perts(2,kx,ky,kz) = Fp(1)*(MERGE(-SUM(dely(:ky-1)),SUM(dely(ky:)),MOD(kx,2)==0))
   END IF

   IF( .NOT.(Boundary_y .OR. Boundary_z) )THEN
    perts(3,kx,ky,kz) = Fp(2)*(MERGE(-SUM(delz(:kz-1)),SUM(delz(kz:)),MOD(ky,2)==0))
   END IF

   IF( .NOT.(Boundary_z .OR. Boundary_x) )THEN
    perts(1,kx,ky,kz) = Fp(3)*(MERGE(-SUM(delx(:kx-1)),SUM(delx(kx:)),MOD(kz,2)==0))
   END IF

  END DO
 END DO
END DO

!!--end--
END SUBROUTINE


SUBROUTINE DATABLOCK_MSHblock_randperts(SIO,Div,delx,dely,delz,perts,fdbk)
!!#### PURPOSE
!! Intelligently determine random perturbations
!! for a regularly-spaced mesh.

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <SIO>
TYPE(TYPE_SIO),POINTER       :: SIO

!!#### REQUIRED INPUT
INTEGER      ,INTENT(IN) :: Div(:)
REAL(KIND_MSH),INTENT(IN) :: delx(:),dely(:),delz(:)

!!#### REQUIRED OUTPUT
REAL(KIND_MSH),POINTER :: perts(:,:,:,:)

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
INTEGER :: kz,ky,kx,Nx,Ny,Nz,NDim
LOGICAL :: Boundary_x,Boundary_y,Boundary_z
REAL(KIND_MSH) :: fp(1:SIZE(Div))

!!--begin--
!! Initialize numbers.
NDim = SIZE(Div)
Nx=1;Ny=1;Nz=1
IF(NDim>=1)Nx=Div(1)+1
IF(NDim>=2)Ny=Div(2)+1
IF(NDim>=3)Nz=Div(3)+1

!allocate
ALLOCATE( perts(NDim,Nx,Ny,Nz) )
perts = 0._KIND_MSH


!get the fraction for each direction
CALL DATABLOCK(SIO,fp,fdbk)
CALL DUMP(fdbk)

!! Initialize the random seed.
CALL RANDOM_SEED()

!! Enter loop.
DO kz=1,Nz
 DO ky=1,Ny
  DO kx=1,Nx

   !! Determine which boundaries the point is on
   Boundary_x = (kx==1 .OR. kx==Nx)
   Boundary_y = (ky==1 .OR. ky==Ny)
   Boundary_z = (kz==1 .OR. kz==Nz)

   !! Perturb only along x-boundary.
   IF( .NOT.Boundary_x )THEN
    perts(1,kx,ky,kz) = Fp(1)*Random( (/-delx(kx-1),+delx(kx)/) )
   END IF

   IF( .NOT.Boundary_y )THEN
    perts(2,kx,ky,kz) = Fp(2)*Random( (/-dely(ky-1),+dely(ky)/) )
   END IF

   IF( .NOT.Boundary_z )THEN
    perts(3,kx,ky,kz) = Fp(3)*Random( (/-delz(kz-1),+delz(kz)/) )
   END IF

  END DO
 END DO
END DO

!!--end--
END SUBROUTINE


SUBROUTINE DATABLOCK_MSHblock_Reg2(SIO,Div,P0,V1,V2,V3,&
  delx,dely,delz,U1,U2,U3,fdbk)
!!#### PURPOSE
!! Reads in a regular mesh block.

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <SIO>
TYPE(TYPE_SIO)                      ,POINTER       :: SIO

!!#### REQUIRED INPUT
INTEGER      ,INTENT(IN) :: Div(:)
REAL(KIND_MSH),INTENT(IN) :: P0(:),V1(:),V2(:),V3(:)

!!#### REQUIRED OUTPUT
REAL(KIND_MSH),POINTER     :: delx(:),dely(:),delz(:)
REAL(KIND_MSH),INTENT(OUT) :: U1(:),U2(:),U3(:)

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
INTEGER       :: NDim
REAL(KIND_MSH) :: Norm_V1,Norm_V2,Norm_V3

!!--begin--
!! Setup.
NDim = SIZE(Div)
delx => NULL()
dely => NULL()
delz => NULL()


!! Datablock.

IF( NDim>=1 )THEN
 !get intervals
 CALL DATABLOCK( SIO , (/1,Div(1)/) , delx , fdbk )
 !calculat norm of <V1> block vector
 Norm_V1 = NormEll2( V1 )
 !normalize <delx> to 1 because it represents fractions of <V1>
 delx = delx/SUM(delx)
 !now calculate true <delx>
 delx = delx*Norm_V1
 !get <V1> direction for block vector
 U1 = V1/Norm_V1
END IF

IF( NDim>=2 )THEN
 !get intervals
 CALL DATABLOCK( SIO , (/1,Div(2)/) , dely , fdbk )
 !calculat norm of <V2> block vector
 Norm_V2 = NormEll2( V2 )
 !normalize <dely> to 1 because it represents fractions of <V2>
 dely = dely/SUM(dely)
 !now calculate true <dely>
 dely = dely*Norm_V2
 !get <V2> direction for block vector
 U2 = V2/Norm_V2
END IF

IF( NDim>=3 )THEN
 !get intervals
 CALL DATABLOCK( SIO , (/1,Div(3)/) , delz , fdbk )
 !calculat norm of <V3> block vector
 Norm_V3 = NormEll2( V3 )
 !normalize <delz> to 1 because it represents fractions of <V3>
 delz = delz/SUM(delz)
 !now calculate true <delz>
 delz = delz*Norm_V3
 !get <V3> direction for block vector
 U3 = V3/Norm_V3
END IF

!!--end--
END SUBROUTINE


SUBROUTINE DATABLOCK_MSHblock_Uni2(SIO,Div,P0,V1,V2,V3,&
  delx,dely,delz,U1,U2,U3,fdbk)
!!#### PURPOSE
!! Reads in a uniform mesh block.

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <SIO>
TYPE(TYPE_SIO)                      ,POINTER       :: SIO

!!#### REQUIRED INPUT
INTEGER      ,INTENT(IN) :: Div(:)
REAL(KIND_MSH),INTENT(IN) :: P0(:),V1(:),V2(:),V3(:)

!!#### REQUIRED OUTPUT
REAL(KIND_MSH),POINTER     :: delx(:),dely(:),delz(:)
REAL(KIND_MSH),INTENT(OUT) :: U1(:),U2(:),U3(:)

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
INTEGER       :: NDim
REAL(KIND_MSH) :: Norm_V1,Norm_V2,Norm_V3

!!--begin--
!! Setup.
NDim = SIZE(Div)
delx => NULL()
dely => NULL()
delz => NULL()

IF( NDim>=1 )THEN
 ALLOCATE( delx( 1:Div(1)) )
 !calculate length of x-side
 Norm_V1 = NormEll2( V1 )
 !make uniform <delx>
 delx = Norm_V1/REAL(Div(1),KIND_MSH)
 !get direction of x-side of block
 U1 = V1/Norm_V1
END IF

IF( NDim>=2 )THEN
 ALLOCATE( dely( 1:Div(2)) )
 !calculate length of y-side
 Norm_V2 = NormEll2( V2 )
 !make uniform <dely>
 dely = Norm_V2/REAL(Div(2),KIND_MSH)
 !get direction of y-side of block
 U2 = V2/Norm_V2
END IF

IF( NDim>=3 )THEN
 ALLOCATE( delz( 1:Div(3)) )
 !calculate length of z-side
 Norm_V3 = NormEll2( V3 )
 !make uniform <delz>
 delz = Norm_V3/REAL(Div(3),KIND_MSH)
 !get direction of z-side of block
 U3 = V3/Norm_V3
END IF

!!--end--
END SUBROUTINE


SUBROUTINE DATABLOCK_MSHblock_part3(Mesh,BlockLabel,Div,P0,U1,U2,U3 , &
  delx,dely,delz,fdbk,perts)
!!#### PURPOSE
!! Reads in a regular mesh block.

!!#### REQUIRED INPUT/OUTPUT
!! * input/output object <SIO>
!! * mesh object <Mesh>
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
CHARACTER(LEN=LEN_MESH_LABEL,KIND=KIND_S)            ,INTENT(IN) :: BlockLabel
REAL(KIND_MSH),DIMENSION(1:NUM_Dimensions_Mesh(Mesh)),INTENT(IN) :: P0,U1,U2,U3
INTEGER      ,DIMENSION(1:NUM_Dimensions_Mesh(Mesh)),INTENT(IN) :: Div
REAL(KIND_MSH)                                  ,INTENT(IN) :: delx(:)
REAL(KIND_MSH)                                  ,INTENT(IN) :: dely(:)
REAL(KIND_MSH)                                  ,INTENT(IN) :: delz(:)

!!#### OPTIONAL INPUT
REAL(KIND_MSH),OPTIONAL,INTENT(IN) :: perts(:,:,:,:)

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
INTEGER               :: IVerts,NVerts,IFaces,NFaces,NCells

!!--begin--
!! Send off to construction routines.

IVerts = Mesh%NVerts
NVerts = ADD_blockVerts( Mesh , &
  BlockLabel , P0,U1,U2,U3 , delx,dely,delz , Div , fdbk , perts )

IFaces = Mesh%NFaces
NFaces = ADD_blockfaces( Mesh , &
  BlockLabel , (/IVerts+1,IVerts+NVerts/) , Div , fdbk )

NCells = ADD_blockcells( Mesh , &
  BlockLabel , (/IFaces+1,IFaces+NFaces/) , Div , fdbk )

!WRITE(*,*)'going to update facetocelllink'
CALL UPDATE_FaceToCellLink(Mesh)

!CALL PRINT_Mesh(Mesh)

!!--end--
END SUBROUTINE



SUBROUTINE MSHtest(SIO,Mesh,fdbk)
!!#### PURPOSE
!! Setup a test mesh.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_SIO) ,POINTER       :: SIO
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL PARAMETERS
CHARACTER(*),PARAMETER :: KEY_TestName(1:1) = (/"Adap3"/)
INTEGER     ,PARAMETER :: MSH_Adap3 = 1

!!#### LOCAL VARIABLES
INTEGER       :: MeshType,Test
REAL(KIND_MSH) :: VecSpec(NUM_Dimensions_Mesh(Mesh))
REAL(KIND_MSH) :: SclSpec1,SclSpec2
INTEGER       :: NDim

!!--begin--
!! Setup.
NDim = NUM_Dimensions_Mesh(Mesh)

!! Arguments.
CALL BEGIN_ARGUMENTS(SIO,(/"MeshType",&
                           "TestName",&
                           "VecSpec ",&
                           "SclSpec1",&
                           "SclSpec2" /) , &
                           fdbk , &
                           NSubArg  = (/   0,&
                                           0,&
                                        NDim,&
                                           0,&
                                           0 /) , &
                         optional = (/.FALSE.,&
                                      .FALSE.,&
                                      .TRUE. ,&
                                      .TRUE. ,&
                                      .TRUE.  /) )
CALL ARGUMENT( SIO , MeshType  , fdbk , Keys=KEY_MeshType )
CALL ARGUMENT( SIO , Test      , fdbk , Keys=KEY_TestName )
CALL ARGUMENT( SIO , VecSpec   , fdbk , default=SPREAD(0.5_KIND_MSH,1,NDim))
CALL ARGUMENT( SIO , SclSpec1  , fdbk , default=90.0_KIND_MSH)
CALL ARGUMENT( SIO , SclSpec2  , fdbk , default=90.0_KIND_MSH)
CALL END_ARGUMENTS( SIO , fdbk )

!! Select setup procedure.
SELECT CASE( MeshType )

  !unstructured
  CASE( MSH_Unstructured )
    SELECT CASE( Test )
      CASE( MSH_Adap3 ) ; CALL SETUP_Mesh_Adap3(Mesh,VecSpec,SclSpec1,SclSpec2)
      CASE DEFAULT  ; CALL STOP(s="The test mesh is unknown or not unstructured! (/CCS_Mesh::MSHtest/)")
    END SELECT

  !default
  CASE DEFAULT ; CALL STOP(s="The mesh type is unknown. (/CCS_Mesh::MSHtest/)")

END SELECT

!!--end--
END SUBROUTINE


SUBROUTINE DATABLOCK_CellSpec( SIO , Mesh , CellSpec , CellSet , fdbk)
!!#### PURPOSE
!! Read in a datablock based on a <CellSpec> and return
!! the appropriate set of cells specified, <CellSet>.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_SIO),POINTER :: SIO

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
CHARACTER(*)  ,INTENT(IN) :: CellSpec

!!#### REQUIRED OUTPUT
INTEGER,POINTER :: CellSet(:)

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
REAL(KIND_MSH),ALLOCATABLE :: Pg(:,:)
INTEGER :: N(1)
CHARACTER(LEN=LEN_TRIM(CellSpec)) :: CellSpec_

!!--begin--
!initialize
CellSet => NULL()

!get upcased and trimmed cellspec
CellSpec_ = UPCASE(TRIM(CellSpec))

!select action
SELECT CASE( CellSpec_ )

 CASE( "ALL","ALL_CELLS" )
   CellSet => ptr_CellSet_ALL( Mesh )

 CASE( "POLYREGION" )
   CALL DATABLOCK( SIO , N , fdbk )
   ALLOCATE( Pg(Mesh%NDim,N(1)) )
   CALL DATABLOCK( SIO , Pg , fdbk )
   CellSet => ptr_CellSet_POLYREGION( Mesh , Pg )

 CASE DEFAULT
   CellSet => ptr_CellSet_GENLABEL( Mesh , CellSpec_ )

END SELECT

!!--end--
END SUBROUTINE





SUBROUTINE DATABLOCK_FaceSpec( SIO , Mesh , FaceSpec , FaceSet , fdbk)
!!#### PURPOSE
!! Read in a datablock based on a <FaceSpec> and return
!! the appropriate set of cells specified, <FaceSet>.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_SIO),POINTER :: SIO

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
CHARACTER(*)  ,INTENT(IN) :: FaceSpec

!!#### REQUIRED OUTPUT
INTEGER,POINTER :: FaceSet(:)

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
REAL(KIND_MSH),ALLOCATABLE :: Pg(:,:),P2(:,:),Pn(:)
INTEGER :: N(1)
CHARACTER(LEN=LEN_TRIM(FaceSpec)) :: FaceSpec_

!!--begin--
!initialize
FaceSet => NULL()

!get upcased and trimmed cellspec
FaceSpec_ = UPCASE(TRIM(FaceSpec))

!select action
SELECT CASE( FaceSpec_ )

 CASE( "ALL","ALL_FACES" )
   FaceSet => ptr_FaceSet_ALL( Mesh )

 CASE( "PLANEINTERSECT" )
   ALLOCATE( P2(Mesh%NDim,2) , Pn(Mesh%NDim+1) )
   CALL DATABLOCK( SIO , P2 , fdbk )
   Pn = xyPLANE_P2( P2 )
   FaceSet => ptr_FaceSet_PLANEINTERSECT( Mesh , Pn )
   DEALLOCATE( P2,Pn )

 CASE( "POLYREGION" )
   CALL DATABLOCK( SIO , N , fdbk )
   ALLOCATE( Pg(Mesh%NDim,N(1)) )
   CALL DATABLOCK( SIO , Pg , fdbk )
   FaceSet => ptr_FaceSet_POLYREGION( Mesh , Pg )

 CASE DEFAULT
   FaceSet => ptr_FaceSet_GENLABEL( Mesh , FaceSpec_ )

END SELECT

!!--end--
END SUBROUTINE



SUBROUTINE DATABLOCK_VertSpec( SIO , Mesh , VertSpec , VertSet , fdbk)
!!#### PURPOSE
!! Read in a datablock based on a <VertSpec> and return
!! the appropriate set of Verts specified, <VertSet>.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_SIO),POINTER :: SIO

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
CHARACTER(*)  ,INTENT(IN) :: VertSpec

!!#### REQUIRED OUTPUT
INTEGER,POINTER :: VertSet(:)

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
REAL(KIND_MSH),ALLOCATABLE :: Pg(:,:)
INTEGER :: N(1)
CHARACTER(LEN=LEN_TRIM(VertSpec)) :: VertSpec_

!!--begin--
!initialize
VertSet => NULL()

!get upcased and trimmed Vertspec
VertSpec_ = UPCASE(TRIM(VertSpec))

!select action
SELECT CASE( VertSpec_ )

 CASE( "ALL","ALL_VERTS" )
   VertSet => ptr_VertSet_ALL( Mesh )

 CASE( "POLYREGION" )
   CALL DATABLOCK( SIO , N , fdbk )
   ALLOCATE( Pg(Mesh%NDim,N(1)) )
   CALL DATABLOCK( SIO , Pg , fdbk )
   VertSet => ptr_VertSet_POLYREGION( Mesh , Pg )

 CASE DEFAULT
   VertSet => ptr_VertSet_GENLABEL( Mesh , VertSpec_ )

END SELECT

!!--end--
END SUBROUTINE



SUBROUTINE DATABLOCK_CellSplitShape( SIO , Mesh , SplitShape , CellSet , &
 ShiftToCenter , fdbk , CenterRadius , CenterPolar , CenterAzimuthal , VertTol )
!!#### PURPOSE
!! Read in a datablock based on a <SplitShape> and carry out the
!! appropriate splitting operations on the <CellSet>.

USE SUB_Sort_quick,ONLY: Sort=>Sort_quick !!((03-A-SUB_Sort_quick.f90))

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_SIO) ,POINTER       :: SIO
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
CHARACTER(*)  ,INTENT(IN) :: SplitShape
LOGICAL       ,INTENT(IN) :: ShiftToCenter

!!#### REQUIRED OUTPUT
INTEGER,POINTER :: CellSet(:)

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),INTENT(INOUT) :: fdbk
!! * optional arguments to perturb the resultant center Vertex from a split
REAL(KIND_MSH) ,INTENT(IN),OPTIONAL :: CenterRadius(2),CenterPolar(2),CenterAzimuthal(2),VertTol

!!#### LOCAL VARIABLES
REAL(KIND_MSH) :: Ps_1(Mesh%NDim,2)
REAL(KIND_MSH) :: Ps_2(Mesh%NDim,2)
REAL(KIND_MSH) :: Pn_1(Mesh%NDim+1)
REAL(KIND_MSH) :: Pn_2(Mesh%NDim+1)
INTEGER :: N
LOGICAL,ALLOCATABLE :: Splits(:)
CHARACTER(LEN=LEN_TRIM(SplitShape)) :: SplitShape_
INTEGER,POINTER :: CellSetOut(:)

!!--begin--
!initialize
CellSetOut => NULL()

!get upcased and trimmed SplitShape
SplitShape_ = UPCASE(TRIM(SplitShape))

!select action
SELECT CASE( SplitShape_ )

 CASE( "PLANESEGMENT","PLANESEGMENT1" )
   !get data
   CALL DATABLOCK( SIO , Ps_1 , fdbk )

   !split first way
   IF( ASSOCIATED(CellSet) )THEN
    ALLOCATE( Splits(SIZE(CellSet)) )
    Pn_1 = xyPLANE_P2( Ps_1 )
    Splits = Split_Cells( Mesh , CellSet , Pn_1 , Ps_interior=Ps_1 , &
      ShiftToCenter=ShiftToCenter , VertTol=VertTol )
    DEALLOCATE( Splits )
   END IF

 CASE( "PLANESEGMENT2" )
   !get data
   CALL DATABLOCK( SIO , Ps_1 , fdbk )
   CALL DATABLOCK( SIO , Ps_2 , fdbk )

   !split first way
   IF( ASSOCIATED(CellSet) )THEN
    ALLOCATE( Splits(SIZE(CellSet)) )
    Pn_1 = xyPLANE_P2( Ps_1 )
    Splits = Split_Cells( Mesh , CellSet    , Pn_1 , Ps_interior=Ps_1 , CellSetOut=CellSetOut , &
      ShiftToCenter=ShiftToCenter , VertTol=VertTol )
    DEALLOCATE( Splits )

    !split second way
    ALLOCATE( Splits(SIZE(CellSet)) )
    Pn_2 = xyPLANE_P2( Ps_2 )
    Ps_1(:,2) = Ps_1(:,1)
    Ps_2(:,2) = Ps_1(:,1) + xyPERPCCW_V( Ps_1(:,2)-Ps_1(:,1) )
    Splits = Split_Cells( Mesh , CellSetOut , Pn_2 , Ps_interior=Ps_2 , &
      ShiftToCenter=ShiftToCenter , VertTol=VertTol )
    DEALLOCATE( Splits )
   END IF

 CASE( "QUAD" )
  !WRITE(*,*)'before CRACK_CELL'
  CALL CRACK_Cell( Mesh , CellSet, VertTol=VertTol )
  !WRITE(*,*)'after CRACK_CELL'

 CASE( "PLANESEGMENT3" )
   CALL Stop(s="The splitting option is not installed yet")

 CASE DEFAULT
   CALL Stop(s="The splitting option was not understood")

END SELECT

!WRITE(*,*)'before finalize'
!CALL FINALIZE_Mesh(Mesh)
!RITE(*,*)'after finalize'

!!--end--
END SUBROUTINE




SUBROUTINE DATABLOCK_FaceSplitShape( SIO , Mesh , SplitShape , FaceSet , &
 ShiftToCenter , fdbk , CenterRadius , CenterPolar , CenterAzimuthal )
!!#### PURPOSE
!! Read in a datablock based on a <SplitShape> and carry out
!! the appropriate splitting operations on <FaceSet>.
USE SUB_Sort_quick,ONLY: Sort=>Sort_quick !!((03-A-SUB_Sort_quick.f90))

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_SIO) ,POINTER       :: SIO
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
CHARACTER(*)  ,INTENT(IN) :: SplitShape
LOGICAL       ,INTENT(IN) :: ShiftToCenter

!!#### REQUIRED OUTPUT
INTEGER,POINTER :: FaceSet(:)

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),INTENT(INOUT) :: fdbk
!! * optional arguments to perturb the resultant center Vertex from a split
REAL(KIND_MSH) ,INTENT(IN),OPTIONAL :: CenterRadius(2),CenterPolar(2),CenterAzimuthal(2)

!!#### LOCAL VARIABLES
REAL(KIND_MSH) :: Ps_1(Mesh%NDim,2)
REAL(KIND_MSH) :: Pn_1(Mesh%NDim+1)
INTEGER :: N
LOGICAL,ALLOCATABLE :: Splits(:)
CHARACTER(LEN=LEN_TRIM(SplitShape)) :: SplitShape_
INTEGER,POINTER :: k_(:),j_(:)

!!--begin--

!get upcased and trimmed SplitShape
SplitShape_ = UPCASE(TRIM(SplitShape))

!select action
SELECT CASE( SplitShape_ )

 CASE( "PLANESEGMENT","PLANESEGMENT1" )
   !get data
   CALL DATABLOCK( SIO , Ps_1 , fdbk )


   !split first way
   IF( ASSOCIATED(FaceSet) )THEN
    ALLOCATE( Splits(SIZE(FaceSet)) )
    Pn_1 = xyPLANE_P2( Ps_1 )
    DO n=1,SIZE(FaceSet)
     Splits(n) = Split_Face( Mesh , FaceSet(n) , Pn_1 , k_ , j_ )
     DEALLOCATE( k_,j_)
    END DO
   END IF

 CASE( "PLANESEGMENT3" )
   CALL Stop(s="The splitting option is not installed yet")

 CASE DEFAULT
   CALL Stop(s="The splitting option was not understood")

END SELECT

CALL FINALIZE_Mesh(Mesh)

!!--end--
END SUBROUTINE



END MODULE

