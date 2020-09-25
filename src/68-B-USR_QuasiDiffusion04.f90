!!# USER MODULE: <USR_Quasidiffusion04>
MODULE USR_QuasiDiffusion04

!!## PURPOSE
!! The user-module for the fourth (Morel) variant
!! of the quasidiffusion routines.

!!## METHOD
!! The ordering of the equations is important.  We solve
!!        $$   A \vec{x} = \vec{b}  .$$
!! The number of cells is $N_i$.
!! The number of faces is $N_j$.
!! With quadrilaterals, each cell has 5 equations and a boundary
!! condition at each face on the boundary, and an interface condition
!! for ever two interior faces.

!! The following ordering applies:
!!  * The first $N_i$ coeff are for cell-average
!!    scalar Fluxes.
!!  * The next $N_j$ coeff are for face-average
!!    scalar Fluxes.
!!  * The final $N_j$ coeff are for face-average
!!    Currents.


!!## EXTERNAL KINDS
USE KND_XSExpansion                                      !!((02-A-KND_XSExpansion.f90))
USE KND_MoCshort                                         !!((03-A-KND_MoCshort.f90))
USE KND_QuasiDiffusion                                   !!((02-A-KND_QuasiDiffusion.f90))

!!## PARAMETERS
USE PAR_Constants_Rdp,ONLY: c_4_times_PI,c_1             !!((02-A-PAR_Constants_Rdp.f90))
USE PAR_MoCshort     ,ONLY: KEY_BC                       !!((03-A-PAR_MoCshort.f90))
USE PAR_QuasiDiffusion                                   !!((05-C-PAR_QuasiDiffusion.f90))
USE ISO_varying_string                                   !!((03-A-ISO_varying_string.f90))

!!## EXTERNAL PROCEDURES
USE SUB_Reallocate                                       !!((04-B-SUB_Reallocate.f90))
USE SUB_Pause                                            !!((04-B-SUB_Pause.f90))
USE SUB_Stop                                             !!((04-B-SUB_Stop.f90))
USE FUN_EQUILOC                                          !!((03-A-FUN_EQUILOC.f90))
USE FUN_Default                                          !!((04-A-FUN_Default.f90))
USE FUN_SIZEa                                            !!((06-B-FUN_SIZEa.f90))
USE FUN_Sequence                                         !!((03-A-FUN_Sequence.f90))
USE SUB_Shake                                            !!((06-B-SUB_Shake.f90))
USE FUN_NewFile                                          !!((05-B-FUN_NewFile.f90))
USE FUN_Error                                            !!((04-A-FUN_Error.f90))

!!## GLOBAL PRINTING SUBROUTINES
USE PRN_Text                                             !!((07-B-PRN_Text.f90))

!!## USER MODULES
!! * feedback user module
USE USR_fdbk                                             !!((08-C-USR_fdbk.f90))
USE USR_QuasiDiffusion                                   !!((67-B-USR_QuasiDiffusion.f90))

!!## GLOBAL TOOLBOXES
!! * SMLib sparse matrix computation
USE USR_SMlib_Matrix_Arithmetic                          !!((19-B-USR_SMlib_Matrix_Arithmetic.f90))
USE USR_SMlib_ILU                                        !!((20-B-USR_SMlib_ILU.f90))
USE USR_SMlib_CGS                                        !!((20-B-USR_SMlib_CGS.f90))
USE USR_SMlib_Band_LU                                    !!((17-B-USR_SMlib_Band_LU.f90))
USE USR_SMlib_Band_Gauss_Solver                          !!((20-B-USR_SMlib_Band_Gauss_Solver.f90))

!!## GLOBAL LIBRARIES
USE LIB_Prompts                                          !!((06-B-LIB_Prompts.f90))
USE LIB_GenericPhrases                                   !!((07-B-LIB_GenericPhrases.f90))
USE LIB_Norm                                             !!((04-B-LIB_Norm.f90))

!!## USER MODULES
USE USR_DiscreteOrdinates                                !!((34-B-USR_DiscreteOrdinates.f90))

!!## GLOBAL TOOLBOXES
USE TBX_Mesh,ONLY: &                                     !!((15-B-TBX_Mesh.f90))
 TYPE_Mesh,NUM_Cells,NUM_Faces,NUM_Verts,KIND_MSH,&
 NUM_Dimensions,FaceCentroid,FaceNormal,CellVolume,FaceArea,&
 UPDATE_BoundaryFaces,TYPE_Interfaces,SETUP_Interfaces,CellCentroid,&
 Vert,ENSURE_Mesh_CellBased
USE TBX_ComputationalGeometry                            !!((09-A-TBX_ComputationalGeometry.f90))
USE USR_MoCshort,ONLY: IsReflective                      !!((48-C-USR_MoCshort.f90))
USE VAR_QuasiDiffusion,ONLY: InterfaceType,HalfCellShape !!((46-B-VAR_QuasiDiffusion.f90))
                                         !!((04-B-LIB_Norm.f90))


!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## DEFAULT ACCESS
PRIVATE

!!## ACCESS
PUBLIC :: SETUP_QDF04,UPDATE_QDF04,RESIDUAL_QDF04,RECALL_QDF04
PUBLIC :: PUT_QDF04,COL_QDF04_Cell,COL_QDF04_Face
!!## IDENTIFICATION
CHARACTER(*),PARAMETER :: mod_="USR_QuasiDiffusion04"

!!## LOCAL PARAMETERS
INTEGER,PARAMETER :: QDF_Moment0Center = 01
INTEGER,PARAMETER :: QDF_Moment1Bottom = 02
INTEGER,PARAMETER :: QDF_Moment1Right  = 03
INTEGER,PARAMETER :: QDF_Moment1Top    = 04
INTEGER,PARAMETER :: QDF_Moment1Left   = 05
INTEGER,PARAMETER :: QDF_Flux          = 06
INTEGER,PARAMETER :: QDF_Current       = 07
INTEGER,PARAMETER :: QDF_Center        = 08
INTEGER,PARAMETER :: QDF_Bottom        = 09
INTEGER,PARAMETER :: QDF_Right         = 10
INTEGER,PARAMETER :: QDF_Left          = 11
INTEGER,PARAMETER :: QDF_Top           = 12
INTEGER,PARAMETER :: QDF_Weak          = 13
INTEGER,PARAMETER :: QDF_Strong        = 14

CHARACTER(17),PARAMETER :: LABEL_Equation(1:8)=(/"Moment0Center    ",&
                                                 "Moment1Bottom    ",&
                                                 "Moment1Right     ",&
                                                 "Moment1Top       ",&
                                                 "Moment1Left      ",&
                                                 "FluxInterface    ",&
                                                 "CurrentInterface ",&
                                                 "BoundaryCondition" /)
!!## LOCAL VARIABLES
REAL(KIND_QDF) :: coeff(1:6),FixBdryVals0
INTEGER       :: row,cols(1:6),i0,g0,j0,jb0,jd0,jn0,js0
REAL(KIND_MSH),ALLOCATABLE,DIMENSION(:) :: lB,lR,lT,lL,lh,lv
REAL(KIND_MSH),ALLOCATABLE,DIMENSION(:) :: nB,nR,nT,nL
REAL(KIND_MSH) :: hB,hR,hT,hL
REAL(KIND_MSH) :: Vi,VB,VR,VT,VL
INTEGER       :: kBL,kBR,kTR,kTL
INTEGER       :: jB,jR,jT,jL
REAL(KIND_MAC) :: macs0
REAL(KIND_MAC) :: mact0
INTEGER               :: BC0,LOBC0
INTEGER          :: save_row = 0
REAL(KIND_ExtSource)  :: ExtSource0
REAL(KIND_ExtSource)  :: ExtSourceR
REAL(KIND_ExtSource)  :: ExtSourceL
REAL(KIND_ExtSource)  :: ExtSourceT
REAL(KIND_ExtSource)  :: ExtSourceB
REAL(KIND_ScalarFlux) :: ECxx0,ECyy0,ECxy0
REAL(KIND_ScalarFlux) :: EBxx0,EByy0,EBxy0
REAL(KIND_ScalarFlux) :: ETxx0,ETyy0,ETxy0
REAL(KIND_ScalarFlux) :: ELxx0,ELyy0,ELxy0
REAL(KIND_ScalarFlux) :: ERxx0,ERyy0,ERxy0
REAL(KIND_ScalarFlux) :: ScalarFluxIN0
REAL(KIND_Current)    :: CurrentIN0
REAL(KIND_ScalarFlux) :: C0
REAL(KIND_QDF)   :: C_C  = 1.0_KIND_QDF
REAL(KIND_QDF)   :: C_SF = 0.5_KIND_QDF
TYPE(TYPE_Mesh),POINTER :: Mesh_=>NULL()

!! * the list of offsets <offset(1)> is where the first face
!!   scalar flux is listed, <offset(2)> is where the first
!!   face current is listed, <offset(3)> is the total number
!!   of unknowns for one energy group.
INTEGER,ALLOCATABLE  :: offsets(:)
INTEGER              :: NQDF,NZMAX,Ni0,Nj0,Njb0,Njn0,Nk0,NDim0,Ng0
INTEGER :: Njn1,MAX_Njs0
INTEGER,ALLOCATABLE  :: row_reorder(:)
INTEGER,ALLOCATABLE  :: col_reorder(:)
LOGICAL :: RandomShift = .FALSE.,RowAsTheyCome=.FALSE.
TYPE(TYPE_Interfaces),POINTER :: Interfaces(:)
REAL(KIND_MSH) :: h0
REAL(KIND_MSH),POINTER :: hsubs0(:)=>NULL()
LOGICAL,ALLOCATABLE :: IsMaster(:)
INTEGER,ALLOCATABLE :: row_used(:)
LOGICAL :: Using_FirstMomentSource = .FALSE.


!!## MODULE PROCEDURES
CONTAINS


SUBROUTINE SETUP_QDF04( Ng , Mesh , A , b , x , Jbdry , Jdomn , FdBk , &
 Return_Interfaces )
!!#### PURPOSE
!! Set up the system for the solution of the simplest system
!! of QDF equations where the mesh is a structured quadrilateral
!! grid and interface conditions are internal.

!!#### REQUIRED INPUT
INTEGER       ,INTENT(IN)    :: Ng

!!#### REQUIRED OUTPUT
TYPE(TYPE_Mesh),INTENT(INOUT),TARGET :: Mesh
TYPE(MSR)      ,INTENT(INOUT) :: A
REAL(KIND_QDF) ,POINTER       :: x(:),b(:)
INTEGER        ,POINTER       :: Jbdry(:),Jdomn(:)

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback <FdBk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk
TYPE(TYPE_Interfaces),OPTIONAL,POINTER :: Return_Interfaces(:)

!!#### LOCAL VARIABLES
INTEGER,POINTER :: Jinte(:)
INTEGER :: jabs,j,i,j_

!!--begin--
!! first make this mesh cellbased
CALL ENSURE_Mesh_CellBased(Mesh)


!! Set values of numbers for things.
Ni0    = NUM_Cells(Mesh)
Nj0    = NUM_Faces(Mesh)
Nk0    = NUM_Verts(Mesh)
NDim0  = NUM_Dimensions(Mesh)
Ng0    = Ng

!setup pointer to mesh
Mesh_ => Mesh

IF( ALLOCATED(offsets) )THEN
 DEALLOCATE(offsets)
END IF
ALLOCATE(offsets(1:3))

!! Set offset of the face-average Fluxes.
offsets(1) = Ni0

!! Set offset of the face-average Currents.
offsets(2) = Ni0 + Nj0

!! Set offset for one energy group.
offsets(3) = Ni0 + 2*Nj0

!! Update the boundary face list and interior face list.
Jinte => NULL()
CALL UPDATE_BoundaryFaces(Mesh,Jbdry=Jbdry,Jdomn=Jdomn,Jinte=Jinte)

!! Calculate number of boundary faces.
Njb0 = SIZE(Jbdry)

!! Calculate number of interfaces.
CALL SETUP_Interfaces(Mesh,Jinte,Interfaces)
IF( PRESENT(Return_Interfaces) )THEN
 Return_Interfaces => Interfaces
END IF

!! Don't need interior face list anymore. (Just needed it for <SETUP_Interfaces>.)
IF( ASSOCIATED(Jinte) )DEALLOCATE( Jinte )


!! Determine number of interfaces.
Njn0 = SIZE(Interfaces)

!! Determine number of interface equations.
Njn1 = NUM_InterfaceEquations(Interfaces)

!! Determine maximum number of subfaces
MAX_Njs0 = MAX_Subfaces(Interfaces)

!! Determine total number of equations.
NQDF  = Ng0*( 5*Ni0 +   Njb0 + Njn1 )

!! Determine number of nonzero entries in sparse matrix (max).
NZMAX = Ng0*(25*Ni0 + 2*Njb0 + (MAX_Njs0+1)*Njn1 )

!! Allocate and initialize.
CALL SETUP_QDF_MSR(NQDF,NZMAX,A,x,b)

!! Allocate locals.
IF( ALLOCATED(lB) )THEN
 !!WARNING!!
 !!the allocation status of lB is used to check if all these are
 !!allocated or not---as long as the only way they are allocated
 !!is through this routine, everything is fine.
 DEALLOCATE(lB,lR,lT,lL,lh,lv)
 DEALLOCATE(nB,nR,nT,nL)
 DEALLOCATE(hsubs0)
END IF
ALLOCATE(lB(NDim0),lR(NDim0),lT(NDim0),lL(NDim0),lh(NDim0),lv(NDim0))
ALLOCATE(nB(NDim0),nR(NDim0),nT(NDim0),nL(NDim0))
ALLOCATE(hsubs0(MAX_Njs0))

!! Allocate reordering arrays.
ALLOCATE( row_reorder(1:NQDF) )
ALLOCATE( col_reorder(1:NQDF) )
!! Initialize.
col_reorder = Sequence(1,1,NQDF)
row_reorder = Sequence(1,1,NQDF)

!get <IsMaster> mask
ALLOCATE( IsMaster(1:Nj0) )
CALL GET_IsMaster(IsMaster,Interfaces)

CALL UPDATE(fdbk_comment,fdbk,s="[[QDF]] Setting up QuasiDiffusion&
  & system of rank="//TRIM(STR(NQDF))//", number of unknowns="//&
  TRIM(STR(offsets(3)*Ng0))//".")

IF( NQDF>offsets(3)*Ng0 )THEN
 CALL UPDATE(fdbk_error,fdbk,s="[[QDF]] Halting execution: &
   & too many equations.")
ELSE IF( NQDF<offsets(3)*Ng0 )THEN
 CALL UPDATE(fdbk_error,fdbk,s="[[QDF]] Halting execution: &
   & too few equations.")
END IF
CALL DUMP(fdbk)

!! Randomly shift ordering.
IF( RandomShift )THEN
 CALL Shake(col_reorder)
 CALL Shake(row_reorder)
ELSE
 ![work]
 CALL UPDATE(fdbk_comment,fdbk,s="[[QDF]] Reordering rows according to the &
   &consecutive cells/faces scheme.")
 CALL DUMP(fdbk)
 CALL BlockOrdering(Mesh,Ng0,row_reorder,col_reorder) ![work]
 !col_reorder = CSHIFT(col_reorder,Njb0)
 !row_reorder = CSHIFT(row_reorder,Njb0)
END IF

!!--end--
END SUBROUTINE





SUBROUTINE SETUP_QDF04_INTERIOR(g,i,Mesh,mact,macs,l_,ExtSource,&
  ECxx , ECyy , ECxy , Exx , Eyy , Exy , Noisy,Unit,Interactive,ExtSourceF1)

!!#### PURPOSE
!! Return the basic values for a cell.

!!#### REQUIRED INPUT
!! * mesh structure
INTEGER,INTENT(IN)        :: g,i
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
REAL(KIND_MAC) ,INTENT(IN) :: macs(:,:)
REAL(KIND_MAC) ,INTENT(IN) :: mact(:,:)
INTEGER       ,INTENT(IN) :: l_(:)
REAL(KIND_ExtSource) ,INTENT(IN) :: ExtSource(:,:)
REAL(KIND_ScalarFlux),INTENT(IN) :: ECxx(:,:),ECyy(:,:),ECxy(:,:)
REAL(KIND_ScalarFlux),INTENT(IN) :: Exx(:,:),Eyy(:,:),Exy(:,:)
REAL(KIND_ExtSource) ,POINTER,OPTIONAL :: ExtSourceF1(:,:,:)

!!#### OPTIONAL INPUT
LOGICAL,OPTIONAL,INTENT(IN) :: Noisy
INTEGER,OPTIONAL,INTENT(IN) :: Unit
LOGICAL,OPTIONAL,INTENT(IN) :: Interactive

!!#### LOCAL PARAMETERS
LOGICAL,PARAMETER :: DEFAULT_Noisy=.TRUE.
INTEGER,PARAMETER :: DEFAULT_Unit=window_unit
LOGICAL,PARAMETER :: DEFAULT_Interactive=.FALSE.

!!#### LOCAL VARIABLES
REAL(KIND_MSH) :: CC(Mesh%NDim)
LOGICAL :: Noisy_
INTEGER :: Unit_
LOGICAL :: Interactive_
REAL(KIND_MSH),DIMENSION(NUM_Dimensions(Mesh)) :: &
  FCB,FCR,FCT,FCL
!!--begin--
!formats
100 FORMAT(a,i0)
101 FORMAT(a,1e21.10)
102 FORMAT(a,2e21.10)

!handle optionals
Noisy_       = DEFAULT(DEFAULT_Noisy,Noisy,override=.FALSE.)
Unit_        = DEFAULT(DEFAULT_Unit,Unit)
Interactive_ = DEFAULT(DEFAULT_Interactive,Interactive)

!update indices
g0 = g
i0 = i

!get column indices for cell i
jB = Mesh%cells(i0)%facelist(1)
jR = Mesh%cells(i0)%facelist(2)
jT = Mesh%cells(i0)%facelist(3)
jL = Mesh%cells(i0)%facelist(4)

IF( Noisy_ )THEN
 WRITE(Unit_,100)" * cell index, i=",i0
 WRITE(Unit_,100)"   * bottom face, jB=",jB
 WRITE(Unit_,100)"   * right  face, jR=",jR
 WRITE(Unit_,100)"   * top    face, jT=",jT
 WRITE(Unit_,100)"   * left   face, jL=",jL
END IF

!get bottom left vertex
kBL = Mesh%faces(jB)%vertlist(1)

!get bottom right vertex
kBR = Mesh%faces(jR)%vertlist(1)

!get top right vertex
kTR = Mesh%faces(jT)%vertlist(1)

!get top left vertex
kTL = Mesh%faces(jL)%vertlist(1)

IF( Noisy_ )THEN
 WRITE(Unit_,100)"   * bottom-left  vertex, kBL=",kBL
 WRITE(Unit_,100)"   * bottom-right vertex, kBR=",kBR
 WRITE(Unit_,100)"   * top-right    vertex, kTR=",kTR
 WRITE(Unit_,100)"   * top-left     vertex, kTL=",kTL
END IF

!get bottom distance
lB = Vert(Mesh,kBR) - Vert(Mesh,kBL)

!get right distance
lR = Vert(Mesh,kTR) - Vert(Mesh,kBR)

!get top distance
lT = Vert(Mesh,kTL) - Vert(Mesh,kTR)

!get left distance
lL = Vert(Mesh,kBL) - Vert(Mesh,kTL)

!get vertical distance
lv = FaceCentroid(Mesh,jT) - FaceCentroid(Mesh,jB)

!get horizontal distance
lh = FaceCentroid(Mesh,jL) - FaceCentroid(Mesh,jR)

IF( Noisy_ )THEN
 WRITE(Unit_,102)"   * bottom    distance, lB=",lB
 WRITE(Unit_,102)"   * right     distance, lR=",lR
 WRITE(Unit_,102)"   * top       distance, lT=",lT
 WRITE(Unit_,102)"   * left      distance, lL=",lL
 WRITE(Unit_,102)"   * central-v distance, lv=",lv
 WRITE(Unit_,102)"   * central-h distance, lh=",lh
END IF


!Determine half cell volumes
CALL EVAL_HalfCellVolumes_Qd(Mesh,kBL,kBR,kTR,kTL,jB,jR,jT,jL,i0,&
  VB,VR,VT,VL,Vi,Method=HalfCellShape)

IF( Noisy_ )THEN
 WRITE(Unit_,101)"   * full        cell volume, Vi=",Vi
 WRITE(Unit_,101)"   * bottom half-cell volume, VB=",VB
 WRITE(Unit_,101)"   * right  half-cell volume, VR=",VR
 WRITE(Unit_,101)"   * top    half-cell volume, VT=",VT
 WRITE(Unit_,101)"   * left   half-cell volume, VL=",VL
END IF

!! Get areas of faces.
hB = ABS(FaceArea(Mesh,jB))
hR = ABS(FaceArea(Mesh,jR))
hT = ABS(FaceArea(Mesh,jT))
hL = ABS(FaceArea(Mesh,jL))
IF( Noisy_ )THEN
 WRITE(Unit_,101)"   * bottom face area, hB=",hB
 WRITE(Unit_,101)"   * right  face area, hR=",hR
 WRITE(Unit_,101)"   * top    face area, hT=",hT
 WRITE(Unit_,101)"   * left   face area, hL=",hL
END IF

!! Get outward normals.
nB = FaceNormal(Mesh,jB)
nR = FaceNormal(Mesh,jR)
nT = FaceNormal(Mesh,jT)
nL = FaceNormal(Mesh,jL)
IF( Noisy_ )THEN
 WRITE(Unit_,102)"   * bottom face out. normal, nB=",nB
 WRITE(Unit_,102)"   * right  face out. normal, nR=",nR
 WRITE(Unit_,102)"   * top    face out. normal, nT=",nT
 WRITE(Unit_,102)"   * left   face out. normal, nL=",nL
END IF

!! Get cross sections.
mact0 = mact(g0,l_(i0))
macs0 = macs(g0,l_(i0))
IF( Noisy_ )THEN
 WRITE(Unit_,100)"   * energy group, g=",g0
 WRITE(Unit_,101)"     * macro transport xs,  mact=",mact0
 WRITE(Unit_,101)"     * macro scattering xs, macs=",macs0
END IF

!! Get external source.
ExtSource0 = ExtSource(g0,i0)

IF( Noisy_ )THEN
 WRITE(Unit_,101)"     * external source, extsource=",ExtSource0
END IF

Using_FirstMomentSource = .FALSE.
IF( PRESENT(ExtSourceF1) )THEN
 IF( ASSOCIATED(ExtSourceF1) )THEN
  Using_FirstMomentSource = .TRUE.
 END IF
END IF

IF( Using_FirstMomentSource )THEN
 ExtSourceR = (nR(1)*ExtSourceF1(1,g0,jR) + nR(2)*ExtSourceF1(2,g0,jR))
 ExtSourceL = (nL(1)*ExtSourceF1(1,g0,jL) + nL(2)*ExtSourceF1(2,g0,jL))
 ExtSourceT = (nT(1)*ExtSourceF1(1,g0,jT) + nT(2)*ExtSourceF1(2,g0,jT))
 ExtSourceB = (nB(1)*ExtSourceF1(1,g0,jB) + nB(2)*ExtSourceF1(2,g0,jB))
ELSE
 ExtSourceR = 0._KIND_QDF
 ExtSourceL = 0._KIND_QDF
 ExtSourceT = 0._KIND_QDF
 ExtSourceB = 0._KIND_QDF
END IF

IF( Noisy_ )THEN
 WRITE(Unit_,101)"     * external source, extsourceR=",ExtSourceR
 WRITE(Unit_,101)"     * external source, extsourceL=",ExtSourceL
 WRITE(Unit_,101)"     * external source, extsourceT=",ExtSourceT
 WRITE(Unit_,101)"     * external source, extsourceB=",ExtSourceB
END IF

!! Get cell Eddington factors.
ECxx0 = ECxx(g0,i0)
ECxy0 = ECxy(g0,i0)
ECyy0 = ECyy(g0,i0)


IF( Noisy_ )THEN
 WRITE(Unit_,101)"     * cell ECxx=",ECxx0
 WRITE(Unit_,101)"     * cell ECxy=",ECxy0
 WRITE(Unit_,101)"     * cell ECyy=",ECyy0
END IF

!! Get face Eddington factors.
EBxx0 = Exx(g0,jB)
ERxx0 = Exx(g0,jR)
ETxx0 = Exx(g0,jT)
ELxx0 = Exx(g0,jL)


IF( Noisy_ )THEN
 WRITE(Unit_,101)"     * bottom face EBxx=",EBxx0
 WRITE(Unit_,101)"     * right  face ERxx=",ERxx0
 WRITE(Unit_,101)"     * top    face ETxx=",ETxx0
 WRITE(Unit_,101)"     * left   face ELxx=",ELxx0
END IF

EBxy0 = Exy(g0,jB)
ERxy0 = Exy(g0,jR)
ETxy0 = Exy(g0,jT)
ELxy0 = Exy(g0,jL)

IF( Noisy_ )THEN
 WRITE(Unit_,101)"     * bottom face EBxy=",EBxy0
 WRITE(Unit_,101)"     * right  face ERxy=",ERxy0
 WRITE(Unit_,101)"     * top    face ETxy=",ETxy0
 WRITE(Unit_,101)"     * left   face ELxy=",ELxy0
END IF

EByy0 = Eyy(g0,jB)
ERyy0 = Eyy(g0,jR)
ETyy0 = Eyy(g0,jT)
ELyy0 = Eyy(g0,jL)

IF( Noisy_ )THEN
 WRITE(Unit_,101)"     * bottom face EByy=",EByy0
 WRITE(Unit_,101)"     * right  face ERyy=",ERyy0
 WRITE(Unit_,101)"     * top    face ETyy=",ETyy0
 WRITE(Unit_,101)"     * left   face ELyy=",ELyy0
END IF

IF( .FALSE. )THEN
 CALL Esmooth( 1.0_KIND_AngularFlux , &
   ECxy0 , EBxy0 , ERxy0 , ETxy0 , ELxy0 , &
   ECxx0 , EBxx0 , ERxx0 , ETxx0 , ELxx0 , &
   ECyy0 , EByy0 , ERyy0 , ETyy0 , ELyy0 )
END IF

IF( Noisy_ )THEN
 IF( Interactive_.AND.Unit_==window_unit )THEN
  CALL Pause(s="Press <ENTER> to continue...")
 END IF
END IF

!!--end--
END SUBROUTINE

SUBROUTINE PUT_QDF04(g,i,jb,jd,j)
!!#### OPTIONAL INPUT
INTEGER,INTENT(IN),OPTIONAL :: g,i,jb,jd,j
!!--begin--
IF( PRESENT(g ) )g0  = g
IF( PRESENT(i ) )i0  = i
IF( PRESENT(jb) )jb0 = jb
IF( PRESENT(jd) )jd0 = jd
IF( PRESENT(j ) )j0  = j

!!--end--
END SUBROUTINE



SUBROUTINE RESIDUAL_QDF04( res , relres , x , iter , Mesh , &
 mact , macs , l_ , ExtSource , ECxx , ECyy , ECxy , Exx , &
 Eyy , Exy , Jbdry , Jdomn , C , ScalarFluxIN , CurrentIN , &
 BC , LOBC , FixBdryVals , FdBk , Noisy,Unit,Interactive,Unit_resid,&
 ExtSourceF1)

!!#### PURPOSE
!! Calculate the residuals.

!!#### REQUIRED INPUT/OUTPUT
REAL(KIND_QDF),INTENT(INOUT) :: relres(:),res(:)

!!#### REQUIRED INPUT
REAL(KIND_QDF)       ,INTENT(IN) :: x(:)
INTEGER             ,INTENT(IN) :: iter
TYPE(TYPE_Mesh)      ,INTENT(IN) :: Mesh
REAL(KIND_MAC)       ,INTENT(IN) :: mact(:,:)
REAL(KIND_MAC)       ,INTENT(IN) :: macs(:,:)
INTEGER             ,INTENT(IN) :: Jbdry(:),Jdomn(:)
REAL(KIND_ScalarFlux),INTENT(IN) :: ScalarFluxIN(:,:)
REAL(KIND_ScalarFlux),INTENT(IN) :: CurrentIN(:,:)
REAL(KIND_ScalarFlux),INTENT(IN) :: C(:,:)
REAL(KIND_ScalarFlux),INTENT(IN) :: ECxx(:,:),ECxy(:,:),ECyy(:,:)
REAL(KIND_ScalarFlux),INTENT(IN) :: Exx(:,:),Exy(:,:),Eyy(:,:)
INTEGER             ,INTENT(IN) :: BC(:)
INTEGER             ,INTENT(IN) :: l_(:)
REAL(KIND_ExtSource) ,INTENT(IN) :: Extsource(:,:)
INTEGER             ,POINTER    :: LOBC(:)
REAL(KIND_QDF)      ,INTENT(IN) :: FixBdryVals(:)

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback <FdBk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### OPTIONAL INPUT
LOGICAL,OPTIONAL,INTENT(IN) :: Noisy
INTEGER,OPTIONAL,INTENT(IN) :: Unit
LOGICAL,OPTIONAL,INTENT(IN) :: Interactive
INTEGER,OPTIONAL,INTENT(IN) :: Unit_resid
REAL(KIND_ExtSource),OPTIONAL,POINTER :: ExtsourceF1(:,:,:)

!!#### LOCAL PARAMETERS
LOGICAL,PARAMETER :: DEFAULT_Noisy=.FALSE.
INTEGER,PARAMETER :: DEFAULT_Unit=window_unit
LOGICAL,PARAMETER :: DEFAULT_Interactive=.FALSE.
INTEGER,PARAMETER :: DEFAULT_Unit_resid=0

!!#### LOCAL VARIABLES
INTEGER :: i,g,n,jn
LOGICAL :: Noisy_,Interactive_
INTEGER :: Unit_,Unit_resid_
CHARACTER(80),ALLOCATABLE :: Label(:)
CHARACTER(105) :: Header

!!--begin--
!handle optionals
Noisy_       = DEFAULT(DEFAULT_Noisy,Noisy)
Unit_        = DEFAULT(DEFAULT_Unit,Unit)
Interactive_ = DEFAULT(DEFAULT_Interactive,Interactive)
Unit_resid_  = DEFAULT(DEFAULT_Unit_resid,Unit_resid)

IF( Unit_resid_/=0 )THEN
 ALLOCATE(Label(SIZE(x)))
 !header
 100 FORMAT(a7,2x,a3,2x,a5,2x,a17,2x,a5,2x,a5,2x,a10,2x,a16,2x,a9,2x,a9)
 WRITE(Header,100)"row","g","i","eqn type","j","jdmn","bc type","x","res","relres"
END IF

!! row number
save_row = 0
row   = 0

!loop through the cell indices setting 5 equations per cell
DO g=1,Ng0

 DO i=1,Ni0

  !0. setup interior locals
  CALL SETUP_QDF04_INTERIOR(g,i,Mesh,mact,macs,l_,ExtSource,&
    ECxx , ECyy , ECxy , Exx , Eyy , Exy , &
    Noisy=.FALSE.,Unit=0,Interactive=.FALSE.,ExtSourceF1=ExtSourceF1)

  !1. for each of 5 equations
  DO n=1,5
   IF( Unit_resid_/=0 )THEN
    CALL RESIDUAL_QDF04_INTERIOR(n,res,relres,x,label)
   ELSE
    CALL RESIDUAL_QDF04_INTERIOR(n,res,relres,x)
   END IF
  END DO

 END DO

 !5. set the interface conditions
 DO jn=1,Njn0

  CALL SETUP_QDF04_INTERFACE(g,jn,Mesh,Interfaces, &
    Noisy=.FALSE.,Unit=0,Interactive=.FALSE. )

  IF( Unit_resid_/=0 )THEN
   CALL RESIDUAL_QDF04_IC(res,relres,x,label)
  ELSE
   CALL RESIDUAL_QDF04_IC(res,relres,x)
  END IF

 END DO

 !6. set the boundary conditions equations
 DO jb=1,Njb0

  !! 6.a setup boundary locals
  CALL SETUP_QDF04_BOUNDARY(g,jb,Mesh,Jbdry,Jdomn, C , &
    ScalarFluxIN , CurrentIN , BC , LOBC , FixBdryVals , &
    Noisy=.FALSE.,Unit=0,Interactive=.FALSE.)

  !! 6.b Get the boundary conditions.
  IF( Unit_resid_/=0 )THEN
   CALL RESIDUAL_QDF04_BC(res,relres,x,label)
  ELSE
   CALL RESIDUAL_QDF04_BC(res,relres,x)
  END IF

 END DO
END DO

IF( Unit_resid_/=0 )THEN
 WRITE(Unit_resid_,"(a,i6.3,a)")"#QDF04 RESIDUALS (iter=",iter,")#"
 WRITE(Unit_resid_,"(a)")Header
 DO n=1,NQDF
  WRITE(Unit_resid_,"(a,2x,e16.9,2x,e9.2,2x,e9.2,'%')")Label(n),x(n),res(n),relres(n)*100._KIND_QDF
 END DO
 DEALLOCATE(Label)
 IF( Interactive_ .AND.Unit_resid_==window_unit )THEN
  CALL Pause(s="Press <ENTER> to continue...")
 END IF
 WRITE(Unit_resid_,"(//)")
END IF

!!--end--
END SUBROUTINE


SUBROUTINE RECALL_QDF04( x , ScalarFluxC , ScalarFluxF , CurrentF )
!!#### PURPOSE
!! Recall the values for the cell scalar flux, face scalar flux, and
!! face current from the solution vector x.

!!#### REQUIRED INPUT
REAL(KIND_QDF) ,INTENT(IN) :: x(:)

!!#### REQUIRED OUTPUT
REAL(KIND_ScalarFlux),POINTER :: ScalarFluxC(:,:)
REAL(KIND_ScalarFlux),POINTER :: ScalarFluxF(:,:)
REAL(KIND_Current)   ,POINTER :: CurrentF(:,:)

!!#### LOCAL VARIABLES
INTEGER :: i,g,j,n

!!--begin--
IF( ASSOCIATED(ScalarFluxC) )DEALLOCATE( ScalarFluxC )
IF( ASSOCIATED(ScalarFluxF) )DEALLOCATE( ScalarFluxF )
IF( ASSOCIATED(CurrentF) )DEALLOCATE( CurrentF )
ALLOCATE( ScalarFluxC(1:Ng0,1:Ni0) )
ALLOCATE( ScalarFluxF(1:Ng0,1:Nj0) )
ALLOCATE( CurrentF(1:Ng0,1:Nj0) )

DO g=1,Ng0
 !put the energy group index
 CALL PUT_QDF04(g=g)

 DO i=1,Ni0
  !put the cell index
  CALL PUT_QDF04(i=i)

  !cell flux
  n = COL_QDF04_Cell(QDF_Flux,QDF_Center)
  ScalarFluxC(g,i) = x(n)
 END DO

 DO j=1,Nj0
  !put the face index
  CALL PUT_QDF04(j=j)

  !face flux
  n = COL_QDF04_Face(QDF_Flux)
  ScalarFluxF(g,j) = x(n)

  !face current
  n = COL_QDF04_Face(QDF_Current)
  CurrentF(g,j) = x(n)
 END DO
END DO

!!--end--
END SUBROUTINE


SUBROUTINE UPDATE_QDF04( A , b , Mesh , mact , macs , l_ , &
 ExtSource , ECxx , ECyy , ECxy , Exx , Eyy , Exy , Jbdry , Jdomn , C , &
 ScalarFluxIN , CurrentIN , BC , LOBC , FixBdryVals , &
 FdBk , Noisy,Unit,Interactive,ExtSourceF1)
!!#### PURPOSE
!! Update the QDF matrix and right hand sides.

!!#### REQUIRED INPUT/OUTPUT
TYPE(MSR)    ,INTENT(INOUT) :: A
REAL(KIND_QDF),INTENT(INOUT) :: b(:)

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh)      ,INTENT(IN) :: Mesh
REAL(KIND_MAC)       ,INTENT(IN) :: mact(:,:)
REAL(KIND_MAC)       ,INTENT(IN) :: macs(:,:)
INTEGER             ,INTENT(IN) :: Jbdry(:),Jdomn(:)
REAL(KIND_ScalarFlux),INTENT(IN) :: ScalarFluxIN(:,:)
REAL(KIND_Current)   ,INTENT(IN) :: CurrentIN(:,:)
REAL(KIND_ScalarFlux),INTENT(IN) :: C(:,:)
REAL(KIND_ScalarFlux),INTENT(IN) :: ECxx(:,:),ECyy(:,:),ECxy(:,:)
REAL(KIND_ScalarFlux),INTENT(IN) :: Exx(:,:),Eyy(:,:),Exy(:,:)
INTEGER             ,INTENT(IN) :: BC(:)
INTEGER             ,INTENT(IN) :: l_(:)
REAL(KIND_ExtSource) ,INTENT(IN) :: Extsource(:,:)
INTEGER             ,POINTER    :: LOBC(:)
REAL(KIND_QDF)      ,INTENT(IN) :: FixBdryVals(:)

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback <FdBk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### OPTIONAL INPUT
LOGICAL,OPTIONAL,INTENT(IN) :: Noisy
INTEGER,OPTIONAL,INTENT(IN) :: Unit
LOGICAL,OPTIONAL,INTENT(IN) :: Interactive
REAL(KIND_ExtSource),OPTIONAL,POINTER :: ExtsourceF1(:,:,:)

!!#### LOCAL VARIABLES
INTEGER :: i,g,n,jn,jb

!!--begin--
CALL UPDATE(fdbk_comment,fdbk,s="[[QDF]] forming the sparse coefficient &
  &matrix <A> and right side <b> for the QDF04 system variant")

!! initialize RHS and row number
b     = Error(b)
row   = 0
save_row = 0

!! Print header for noisy output.
IF( DEFAULT(.FALSE.,Noisy) )THEN
 WRITE(DEFAULT(window_unit,Unit),"(//,a)")"#QDF04 SETUP"
END IF

!! Set up the row used array.
IF( .NOT.ALLOCATED(row_used) )THEN
 ALLOCATE(row_used(1:NQDF))
END IF
row_used = 0

!loop through the cell indices setting 5 equations per cell
DO g=1,Ng0

 DO i=1,Ni0

  !0. setup interior locals
  CALL SETUP_QDF04_INTERIOR(g,i,Mesh,mact,macs,l_,ExtSource,&
    ECxx , ECyy , ECxy , Exx , Eyy , Exy , &
    Noisy,Unit,Interactive,ExtSourceF1=ExtSourceF1)

  !1. for each of 5 equations
  DO n=1,5
   CALL UPDATE_QDF04_INTERIOR(n,A,b)
  END DO

 END DO

 !5. set the interface conditions
 DO jn=1,Njn0
  CALL SETUP_QDF04_INTERFACE(g,jn,Mesh,Interfaces, &
    Noisy,Unit,Interactive )
  CALL UPDATE_QDF04_InterfaceCondition(A,b)
 END DO

 !6. set the boundary conditions equations
 DO jb=1,Njb0

  !! 6.a setup boundary locals
  CALL SETUP_QDF04_BOUNDARY(g,jb,Mesh,Jbdry,Jdomn, C , &
    ScalarFluxIN , CurrentIN , BC , LOBC , FixBdryVals , &
    Noisy,Unit,Interactive)

  !! 6.b Get the boundary conditions.
  CALL UPDATE_QDF04_BoundaryCondition(A,b)
 END DO
END DO
!(/hack/)
!write(*,*)row_used
!CALL Pause()

!! Reallocate A to fit just right.
CALL REALLOCATE_Matrix(A)

!7. print completion information.
CALL UPDATE(fdbk_comment,fdbk,s="[[QDF]] equation/unknown count for QDF04 system: generated <"&
  //TRIM(STR(COUNT(b/=ERROR(b))))//"> equations to solve for <"//TRIM(STR(SIZE(b)))//&
  "> unknowns." )

CALL DUMP(fdbk)

!!--end--
END SUBROUTINE


SUBROUTINE SETUP_QDF04_INTERFACE(g,jn,Mesh,Interfaces, &
  Noisy,Unit,Interactive )
!!#### PURPOSE
!! Setup the interface local variables.

!USE VAR_EddingtonFactors,ONLY: Exx=>EddingtonxxF,Eyy=>EddingtonyyF !!((46-B-VAR_QuasiDiffusion.f90))

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: g,jn
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
TYPE(TYPE_Interfaces),INTENT(IN) :: Interfaces(:)

!!#### OPTIONAL INPUT
LOGICAL,INTENT(IN),OPTIONAL :: Noisy,Interactive
INTEGER,INTENT(IN),OPTIONAL :: Unit

!!#### LOCAL PARAMETERS
LOGICAL,PARAMETER :: DEFAULT_Noisy=.FALSE.
INTEGER,PARAMETER :: DEFAULT_Unit=window_unit
LOGICAL,PARAMETER :: DEFAULT_Interactive=.FALSE.
LOGICAL,PARAMETER :: OverrideWithDefaults=.FALSE.


!!#### LOCAL VARIABLES
LOGICAL :: Noisy_
INTEGER :: Unit_
LOGICAL :: Interactive_
INTEGER :: Nsubs,n,js0
REAL(KIND_MSH) :: fn(2)

!!--begin--
!handle optionals
IF( OverrideWithDefaults )THEN
 Noisy_       = DEFAULT_Noisy
 Unit_        = DEFAULT_Unit
 Interactive_ = DEFAULT_Interactive
ELSE
 Noisy_       = DEFAULT(DEFAULT_Noisy,Noisy)
 Unit_        = DEFAULT(DEFAULT_Unit,Unit)
 Interactive_ = DEFAULT(DEFAULT_Interactive,Interactive)
END IF

!formats
100 FORMAT(a,i0)
101 FORMAT(a,1e21.10)
102 FORMAT(a,2e21.10)

!set current indices: group, interface, face
g0  = g
jn0 = jn
j0  = Interfaces(jn0)%master


!determine the face area of the master face
h0 = FaceArea(Mesh,j0)

IF( InterfaceType==QDF_SC_plus_WEF )THEN
 WRITE(*,*)"InterfaceType=QDF_SC_plus_WEF is not supported now."
 STOP
 !fn = FaceNormal(Mesh,j0)
 !h0 = h0*( Exx(g0,j0)*ABS(fn(1)) + &
 !          Eyy(g0,j0)*ABS(fn(2)) )
END IF


IF( Noisy_ )THEN
 WRITE(Unit_,100)" * energy group, g=",g0
 WRITE(Unit_,100)"   * interface, jn=",jn0
 WRITE(Unit_,100)"     * master face index, j=",j0
 WRITE(Unit_,101)"     * master face area=",h0
END IF

!determine number of subs for this interface
Nsubs = SIZE(Interfaces(jn0)%subs)

!! Get face areas.
DO n=1,Nsubs
 js0 = Interfaces(jn0)%subs(n)

 hsubs0(n) = FaceArea(Mesh,js0)

 IF( InterfaceType==QDF_SC_plus_WEF )THEN
  WRITE(*,*)"InterfaceType=QDF_SC_plus_WEF is not supported now."
  STOP
  !fn = FaceNormal(Mesh,js0)
  !hsubs0(n) = hsubs0(n)*( Exx(g0,js0)*ABS(fn(1)) + &
  !                        Eyy(g0,js0)*ABS(fn(2)) )
 END IF

 IF( Noisy_ )THEN
  WRITE(Unit_,100)"     * interface sub face ",n
  WRITE(Unit_,100)"       * sub face index, jsub=",Interfaces(jn0)%subs(n)
  WRITE(Unit_,101)"       * sub face area=",hsubs0(n)
 END IF
END DO

IF( Noisy_ )THEN
 IF( Interactive_.AND.Unit_==window_unit )THEN
  CALL Pause(s="Press <ENTER> to continue...")
 END IF
END IF

!!--end--
END SUBROUTINE



SUBROUTINE SETUP_QDF04_BOUNDARY(g,jb,Mesh,Jbdry,Jdomn, C , &
    ScalarFluxIN , CurrentIN , BC , LOBC , FixBdryVals , Noisy,Unit,Interactive )
!!#### PURPOSE
!! Setup the boundary local variables.

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: g,jb
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER,INTENT(IN) :: JBdry(:),Jdomn(:),BC(:)
INTEGER,POINTER    :: LOBC(:)
REAL(KIND_QDF),INTENT(IN) :: C(:,:)
REAL(KIND_QDF),INTENT(IN) :: ScalarFluxIN(:,:)
REAL(KIND_QDF),INTENT(IN) :: CurrentIN(:,:)
REAL(KIND_QDF),INTENT(IN) :: FixBdryVals(:)

!!#### OPTIONAL INPUT
LOGICAL,INTENT(IN),OPTIONAL :: Noisy,Interactive
INTEGER,INTENT(IN),OPTIONAL :: Unit

!!#### LOCAL PARAMETERS
LOGICAL,PARAMETER :: DEFAULT_Noisy=.TRUE.
INTEGER,PARAMETER :: DEFAULT_Unit=window_unit
LOGICAL,PARAMETER :: DEFAULT_Interactive=.FALSE.

!!#### LOCAL VARIABLES
LOGICAL :: Noisy_
INTEGER :: Unit_
LOGICAL :: Interactive_

!!--begin--
!handle optionals
Noisy_       = DEFAULT(DEFAULT_Noisy,Noisy,override=.FALSE.)
Unit_        = DEFAULT(DEFAULT_Unit,Unit)
Interactive_ = DEFAULT(DEFAULT_Interactive,Interactive)

!formats
100 FORMAT(a,i0)
101 FORMAT(a,1e21.10)
102 FORMAT(a,2e21.10)

!set current indices
g0  = g
jb0 = jb
j0  = Jbdry(jb0)
jd0 = Jdomn(jb0)

BC0 = BC(jd0)

!low order boundary conditions
IF( ASSOCIATED(LOBC) )THEN
 LOBC0 = LOBC(jd0)
 FixBdryVals0 = FixBdryVals(jd0)
ELSE
 LOBC0 = 0
END IF

IF( Noisy_ )THEN
 WRITE(Unit_,100)" * face index, j=",j0
 WRITE(Unit_,100)"   * aka boundry-face, jb=",jb0
 WRITE(Unit_,100)"   * on domain-face, jd=",jd0
 WRITE(Unit_,100)"   * energy group, g=",g0
 WRITE(Unit_,"(2a)")"   * high order boundary condition=",KEY_BC(BC0)
 !WRITE(Unit_,"(a)")"   * low  order boundary condition=",QDF_KEY_LOBC(LOBC0)
END IF

!! variables
C0 = C(g0,jb0)
IF( Noisy_ )THEN
 WRITE(Unit_,101)"   * boundary functional C=",C0
END IF

ScalarFluxIN0 = ScalarFluxIN(g0,jb0)
IF( Noisy_ )THEN
 WRITE(Unit_,101)"   * incoming scalar flux, ScalarFluxIN=",ScalarFluxIN0
END IF

CurrentIN0    = CurrentIN(g0,jb0)
IF( Noisy_ )THEN
 WRITE(Unit_,101)"   * incoming current, CurrentIN=",CurrentIN0
END IF

IF( Noisy_ )THEN
 IF( Interactive_.AND.Unit_==window_unit )THEN
  CALL Pause(s="Press <ENTER> to continue...")
 END IF
END IF

!!--end--
END SUBROUTINE


SUBROUTINE UPDATE_QDF04_INTERIOR(n,A,b)
!!#### PURPOSE
!! Determine the balance equation values into <coeff> and
!! the corresponding column indices in <cols>.

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: n

!!#### REQUIRED INPUT/OUTPUT
TYPE(MSR)    ,INTENT(INOUT) :: A
REAL(KIND_QDF),INTENT(INOUT) :: b(:)
!!#### LOCAL VARIABLES
LOGICAL,PARAMETER :: Noisy_=.FALSE.
LOGICAL :: mask(6)

!!--begin--
!get row
row    =   ROW_QDF04_Cell(n)
!get columns
cols = COLS_QDF04_Cell(n)
!get coefficients
coeff = COEFF_QDF04_Cell(n)
mask = coeff/=0._KIND_QDF

!set values in sparse matrix
CALL SetRow (A, row, PACK(cols,mask) , PACK(coeff,mask) )

!set right side
b(row) =   RHS_QDF04_Cell(n)

IF( Noisy_ )THEN
 write(*,*)
 write(*,"(a)")"CELL EQUATIONS:"
 write(*,*)"i0=",i0
 write(*,*)"equation: ",LABEL_equation(n)
 write(*,*)"j0=",j0
 write(*,*)"n=",n
 write(*,*)"row=",row
 write(*,*)"cols=",cols
 write(*,*)"coeff=",coeff
 write(*,*)"rhs=",b(row)
END IF

!!--end--
END SUBROUTINE



SUBROUTINE RESIDUAL_QDF04_IC(res,relres,x,label)
!!#### PURPOSE
!! For the <QDF_System02> method, calculate the
!! residuals of the interface conditions.

!!#### REQUIRED INPUT/OUTPUT
REAL(KIND_QDF),INTENT(INOUT) :: res(:),relres(:)

!!#### REQUIRED INPUT
REAL(KIND_QDF),INTENT(IN) :: x(:)

!!#### OPTIONAL INPUT/OUTPUT
CHARACTER(*),INTENT(INOUT),OPTIONAL :: label(:)

!!--begin--
!get the weak flux interface conditions: 1 for the whole interface
CALL GetInterfaceResidual(QDF_Flux,QDF_Weak,res,relres,x,label)

!get the strong current interface conditions: 1 for each subface
DO js0=1,SIZE(Interfaces(jn0)%subs)
 CALL GetInterfaceResidual(QDF_Current,QDF_Strong,res,relres,x,label)
END DO

!!--end--
END SUBROUTINE


SUBROUTINE GetInterfaceResidual(spec2,spec5,res,relres,x,label)
!!#### REQUIRED INPUT/OUTPUT
REAL(KIND_QDF),INTENT(INOUT) :: res(:),relres(:)

!!#### REQUIRED INPUT
INTEGER      ,INTENT(IN) :: spec2,spec5
REAL(KIND_QDF),INTENT(IN) :: x(:)

!!#### OPTIONAL INPUT/OUTPUT
CHARACTER(*),INTENT(INOUT),OPTIONAL :: label(:)

!!#### LOCAL VARIABLES
INTEGER       :: m,Nm
REAL(KIND_QDF) :: rhs,avgterm

!! get row
row    =   ROW_QDF04_Interface(spec2,spec5)
!! get cols
cols   =  COLS_QDF04_Interface(spec2,spec5)
!! get coeff
coeff  = COEFF_QDF04_Interface(spec2,spec5)
!get right side
rhs    =   RHS_QDF04_Interface(spec2,spec5)

!! multiply coefficients by solution subvector
Nm = SIZEa(cols)
FORALL(m=1:Nm)coeff(m)=coeff(m)*x(cols(m))

!! subtract right side to get residual
res(row)    = SUM(coeff(1:Nm)) - rhs

!! calculate average term in residual
avgterm = (SUM(ABS(coeff(1:Nm)))+ABS(rhs))/REAL(Nm+1,KIND_QDF)

!! calculate relative residual by dividing by average
relres(row) = res(row)/avgterm

IF( PRESENT(label) )THEN
 WRITE(label(row),"(i10.7,2x,i3,2x,a5,2x,a17,2x,i8.5,2x,a5,2x,a10)")row,g0," N/A ",LABEL_Equation(spec2),j0," N/A "," N/A "
END IF

!!--end--
END SUBROUTINE




SUBROUTINE UPDATE_QDF04_InterfaceCondition(A,b)
!!#### PURPOSE
!! For the <QDF_System02> method, include the interface conditions.

!!#### REQUIRED INPUT/OUTPUT
TYPE(MSR)    ,INTENT(INOUT) :: A
REAL(KIND_QDF),INTENT(INOUT) :: b(:)

!!#### LOCAL VARIABLES
INTEGER :: N
LOGICAL,PARAMETER :: Noisy_=.FALSE.

!!--begin--

!!GET WEAK INTERFACE CONDITIONS

!weak flux
IF( InterfaceType==QDF_SC_plus_WF .OR. InterfaceType==QDF_SC_plus_WEF )THEN
 !! get row
 row    =   ROW_QDF04_Interface(QDF_Flux,QDF_Weak)
 !! get cols
 cols   =  COLS_QDF04_Interface(QDF_Flux,QDF_Weak)
 !! get coeff
 coeff  = COEFF_QDF04_Interface(QDF_Flux,QDF_Weak)
 !! get right side
 b(row) = RHS_QDF04_Interface(QDF_Flux,QDF_Weak)

!weak current
ELSE
 !! get row
 row    =   ROW_QDF04_Interface(QDF_Current,QDF_Weak)
 !! get cols
 cols   =  COLS_QDF04_Interface(QDF_Current,QDF_Weak)
 !! get coeff
 coeff  = COEFF_QDF04_Interface(QDF_Current,QDF_Weak)
 !! get right side
 b(row) = RHS_QDF04_Interface(QDF_Current,QDF_Weak)
END IF

!get number of columns actually used.
N  = SIZEa(cols)

!print noisy info
IF( Noisy_ )THEN
 write(*,*)"i0=",i0
 write(*,*)"j0=",j0
 write(*,*)"equation: ",LABEL_equation(6)
 write(*,*)"row=",row
 write(*,*)"cols=",cols(1:N)
 write(*,*)"coeff=",coeff(1:N)
 write(*,*)"rhs=",b(row)
END IF

!include row in sparse matrix
CALL SetRow (A, row, cols(1:N) , coeff(1:N) )


!!GET STRONG INTERFACE CONDITIONS

DO js0=1,SIZE(Interfaces(jn0)%subs)

 !strong current
 IF( InterfaceType==QDF_SC_plus_WF .OR. InterfaceType==QDF_SC_plus_WEF )THEN
  !! get row
  row    =   ROW_QDF04_Interface(QDF_Current,QDF_Strong)
  !! get cols
  cols   =  COLS_QDF04_Interface(QDF_Current,QDF_Strong)
  !! get coeff
  coeff  = COEFF_QDF04_Interface(QDF_Current,QDF_Strong)
  !! get right side
  b(row) = RHS_QDF04_Interface(QDF_Current,QDF_Strong)

 !strong flux
 ELSE
  !! get row
  row    =   ROW_QDF04_Interface(QDF_Flux,QDF_Strong)
  !! get cols
  cols   =  COLS_QDF04_Interface(QDF_Flux,QDF_Strong)
  !! get coeff
  coeff  = COEFF_QDF04_Interface(QDF_Flux,QDF_Strong)
  !! get right side
  b(row) = RHS_QDF04_Interface(QDF_Flux,QDF_Strong)
 END IF

 !get number of columns actually used.
 N  = SIZEa(cols)

 !print noisy info
 IF( Noisy_ )THEN
  write(*,*)"j0=",j0
  write(*,*)"equation: ",LABEL_equation(7)
  write(*,*)"row=",row
  write(*,*)"cols=",cols(1:N)
  write(*,*)"coeff=",coeff(1:N)
 END IF

 !! include row in sparse matrix
 CALL SetRow (A, row, cols(1:N) , coeff(1:N) )


END DO

!!--end--
END SUBROUTINE


SUBROUTINE UPDATE_QDF04_BoundaryCondition(A,b)
!!#### PURPOSE
!! For the <QDF_System02> method, include the boundary conditions.

!!#### REQUIRED INPUT/OUTPUT
TYPE(MSR)    ,INTENT(INOUT) :: A
REAL(KIND_QDF),INTENT(INOUT) :: b(:)

!!#### LOCAL VARIABLES
INTEGER :: N
LOGICAL,PARAMETER :: Noisy_=.FALSE.

!!--begin--
!! get row
row    =   ROW_QDF04_BoundaryFace(BC0,LOBC0)
!! get cols
cols   =  COLS_QDF04_BoundaryFace(BC0,LOBC0)
!! get coeff
coeff  = COEFF_QDF04_BoundaryFace(BC0,LOBC0)

!get number of columns actually used.
N  = SIZEa(cols)

!include in sparse matrix
CALL SetRow (A, row, cols(1:N) , coeff(1:N) )
!get right side
b(row) = RHS_QDF04_BoundaryFace(BC0,LOBC0)


!print noisy info
IF( Noisy_ )THEN
 write(*,*)"j0=",j0
 write(*,*)"equation: ",LABEL_Equation(8)
 write(*,*)"row=",row
 write(*,*)"cols=",cols(1:N)
 write(*,*)"coeff=",coeff(1:N)
 write(*,*)"rhs=",b(row)
END IF

!!--end--
END SUBROUTINE





SUBROUTINE RESIDUAL_QDF04_INTERIOR(n,res,relres,x,label)
!!#### PURPOSE
!! Determine the the residual of the solution <x>.

!!#### REQUIRED INPUT
INTEGER      ,INTENT(IN) :: n
REAL(KIND_QDF),INTENT(IN) :: x(:)

!!#### REQUIRED INPUT/OUTPUT
REAL(KIND_QDF),INTENT(INOUT) :: res(:),relres(:)

!!#### OPTIONAL INPUT
CHARACTER(*),OPTIONAL,INTENT(INOUT) :: label(:)

!!#### LOCAL VARIABLES
INTEGER       :: m,Nm
REAL(KIND_QDF) :: rhs,avgterm

!!--begin--
!get row
row    =   ROW_QDF04_Cell(n)
!get columns
cols   =  COLS_QDF04_Cell(n)
!get coefficients
coeff  = COEFF_QDF04_Cell(n)
!! get right side
rhs    =   RHS_QDF04_Cell(n)

!! multiply coefficients by solution subvector
Nm=SIZEa(cols)
FORALL(m=1:Nm)coeff(m)=coeff(m)*x(cols(m))

!! subtract right side to get residual
res(row) = SUM(coeff(1:6)) - rhs

!! calculate average term
avgterm = (SUM(ABS(coeff))+ABS(rhs))/5._KIND_QDF

!! divide to get relative residual
relres(row) = res(row)/avgterm

IF( PRESENT(label) )THEN
 WRITE(label(row),"(i10.7,2x,i3,2x,i8.5,2x,a17,2x,a5,2x,a5,2x,a10)")row,g0,i0,LABEL_Equation(n),&
   MERGE(" N/A ",TRIM(STR(j0,"(i8.5)")),n==QDF_Moment0Center)," N/A "," N/A "
END IF
!!--end--
END SUBROUTINE


SUBROUTINE RESIDUAL_QDF04_BC(res,relres,x,label)
!!#### PURPOSE
!! For the <QDF_System02> method,
!! calculate the residuals of the boundary conditions.

!!#### REQUIRED INPUT/OUTPUT
REAL(KIND_QDF),INTENT(INOUT) :: res(:),relres(:)

!!#### REQUIRED INPUT
REAL(KIND_QDF),INTENT(IN) :: x(:)

!!#### OPTIONAL INPUT/OUTPUT
CHARACTER(*),INTENT(INOUT),OPTIONAL :: label(:)

!!#### LOCAL VARIABLES
INTEGER       :: m,Nm
REAL(KIND_QDF) :: rhs,avgterm

!!--begin--
!! get row
row    =   ROW_QDF04_BoundaryFace(BC0,LOBC0)
!! get cols
cols   =  COLS_QDF04_BoundaryFace(BC0,LOBC0)
!! get coeff
coeff  = COEFF_QDF04_BoundaryFace(BC0,LOBC0)
!get right side
rhs = RHS_QDF04_BoundaryFace(BC0,LOBC0)


!! multiply coefficients by solution subvector
Nm = SIZEa(cols)
FORALL(m=1:Nm)coeff(m)=coeff(m)*x(cols(m))

!! subtract right side to get residual
res(row)    = SUM(coeff(1:Nm)) - rhs

!! calculate average term in residual
avgterm = (SUM(ABS(coeff(1:Nm)))+ABS(rhs))/REAL(Nm+1,KIND_QDF)

!! calculate relative residual by dividing by average
relres(row) = res(row)/avgterm

IF( PRESENT(label) )THEN
 WRITE(label(row),"(i10.7,1x,i3,2x,a5,2x,a17,2x,i8.5,2x,i8.5,2x,a10)")row,g0," N/A ",LABEL_Equation(8),j0,jd0,KEY_BC(BC0)
END IF

!!--end--
END SUBROUTINE


FUNCTION RHS_QDF04_Cell(spec2) RESULT(RHS)
!!#### PURPOSE
!! Return cell-based right sides of equations.

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: spec2

!!#### REQUIRED OUTPUT
REAL(KIND_QDF) :: RHS

!!--begin--
SELECT CASE(spec2)
 CASE(QDF_Moment0Center) ; RHS = ExtSource0*Vi*c_4_times_Pi
 CASE(QDF_Moment1Left  ) ; RHS = ExtSourceL*c_4_times_Pi
 CASE(QDF_Moment1Right ) ; RHS = ExtSourceR*c_4_times_Pi
 CASE(QDF_Moment1Top   ) ; RHS = ExtSourceT*c_4_times_Pi
 CASE(QDF_Moment1Bottom) ; RHS = ExtSourceB*c_4_times_Pi
 CASE DEFAULT            ; RHS = ERROR(RHS)
END SELECT

!!--end--
END FUNCTION


FUNCTION RHS_QDF04_BoundaryFace(BC0,LOBC0) RESULT(RHS)
!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: BC0,LOBC0

!!#### REQUIRED OUTPUT
REAL(KIND_QDF) :: RHS

!!--begin--

SELECT CASE( LOBC0 )

 !get RHS from special low order values
 CASE(QDF_fixedJ)        ; RHS = FixBdryVals0
 CASE(QDF_fixedPhi)      ; RHS = FixBdryVals0
 CASE(QDF_functionJ)     ; RHS = CurrentIN0
 CASE(QDF_functionPhi)   ; RHS = ScalarFluxIN0
 CASE(QDF_functionMixed) ; RHS = C_C*CurrentIN0 - C_SF*ScalarFluxIN0

 !get usual RHS values
 CASE DEFAULT
  IF( IsReflective(BC0) )THEN
   RHS = 0._KIND_QDF
  ELSE
   RHS = CurrentIN0 - C0*ScalarFluxIN0
  END IF

END SELECT
!!--end--
END FUNCTION


FUNCTION COEFF_QDF04_BoundaryFace(BC0,LOBC0) RESULT(Coeff)
!!#### PURPOSE
!! Return coefficients for boundary faces.

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: BC0,LOBC0

!!#### REQUIRED OUTPUT
REAL(KIND_QDF) :: Coeff(1:6)

!!--begin--

Coeff = Error(Coeff)

SELECT CASE(LOBC0)

 !get Coeff from special low order values
 CASE(QDF_fixedJ       ) ; Coeff(1) = 1._KIND_QDF
 CASE(QDF_fixedPhi     ) ; Coeff(1) = 1._KIND_QDF
 CASE(QDF_functionPhi  ) ; Coeff(1) = 1._KIND_QDF
 CASE(QDF_functionJ    ) ; Coeff(1) = 1._KIND_QDF
 CASE(QDF_functionMixed) ; Coeff(1) = +C_C
                           Coeff(2) = -C_SF

 !normal handling
 CASE DEFAULT
  IF( IsReflective(BC0) )THEN
   Coeff(1) = 1._KIND_QDF
  ELSE
   Coeff(1) = 1._KIND_QDF
   Coeff(2) = -C0
  END IF

END SELECT


!!--end--
END FUNCTION


FUNCTION COLS_QDF04_BoundaryFace(BC0,LOBC0) RESULT(COLS)
!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: BC0,LOBC0

!!#### REQUIRED OUTPUT
INTEGER :: COLS(1:6)

!!--begin--

cols = ERROR(cols)

SELECT CASE( LOBC0 )

 !get Cols from special low order values
 CASE(QDF_fixedJ)      ; cols(1) = COL_QDF04_Face(QDF_Current)
 CASE(QDF_fixedPhi)    ; cols(1) = COL_QDF04_Face(QDF_Flux)
 CASE(QDF_functionJ  ) ; cols(1) = COL_QDF04_Face(QDF_Current)
 CASE(QDF_functionPhi) ; cols(1) = COL_QDF04_Face(QDF_Flux)

 CASE(QDF_functionMixed) ; cols(1)=COL_QDF04_Face(QDF_Current)
                           cols(2)=COL_QDF04_Face(QDF_Flux)

 !normal handling
 CASE DEFAULT

   IF( IsReflective(BC0) )THEN
    cols(1) = COL_QDF04_Face(QDF_Current)
   ELSE
    cols(1) = COL_QDF04_Face(QDF_Current)
    cols(2) = COL_QDF04_Face(QDF_Flux   )
   END IF
END SELECT

!!--end--
END FUNCTION



FUNCTION COLS_QDF04_Cell( spec3 ) RESULT(cols)
!!#### PURPOSE
!! Determines the columns for a specific equation of a cell.

!!#### REQUIRED INPUT
!! * <spec3> must be one of:
!!     <QDF_Moment0Center>
!!     <QDF_Moment1Bottom>
!!     <QDF_Moment1Right>
!!     <QDF_Moment1Top>
!!     <QDF_Moment1Left>
INTEGER,INTENT(IN) :: spec3

!!#### REQUIRED OUTPUT
!! * columns for the specified equation,
!!   assuming <QDF_System02> method's ordering of unknowns
INTEGER :: cols(1:6)

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_ = "COLS_QDF04plus_Cell"

!!#### LOCAL VARIABLES
TYPE(varying_string) :: VS

!!--begin--
!initialize
cols = ERROR(cols)

!set column indices
SELECT CASE( spec3 )
  CASE(QDF_Moment0Center)
    cols(1) = COL_QDF04_Cell(QDF_Current,QDF_Bottom)
    cols(2) = COL_QDF04_Cell(QDF_Current,QDF_Right )
    cols(3) = COL_QDF04_Cell(QDF_Current,QDF_Top   )
    cols(4) = COL_QDF04_Cell(QDF_Current,QDF_Left  )
    cols(5) = COL_QDF04_Cell(QDF_Flux   ,QDF_Center)

  CASE(QDF_Moment1Bottom)
    cols(1) = COL_QDF04_Cell(QDF_Flux   ,QDF_Bottom)
    cols(2) = COL_QDF04_Cell(QDF_Flux   ,QDF_Right )
    cols(3) = COL_QDF04_Cell(QDF_Flux   ,QDF_Center)
    cols(4) = COL_QDF04_Cell(QDF_Flux   ,QDF_Left  )
    cols(5) = COL_QDF04_Cell(QDF_Current,QDF_Bottom)
    cols(6) = COL_QDF04_Cell(QDF_Flux   ,QDF_Top   )

  CASE(QDF_Moment1Right )
    cols(1) = COL_QDF04_Cell(QDF_Flux   ,QDF_Bottom)
    cols(2) = COL_QDF04_Cell(QDF_Flux   ,QDF_Right )
    cols(3) = COL_QDF04_Cell(QDF_Flux   ,QDF_Top   )
    cols(4) = COL_QDF04_Cell(QDF_Flux   ,QDF_Center)
    cols(5) = COL_QDF04_Cell(QDF_Current,QDF_Right )
    cols(6) = COL_QDF04_Cell(QDF_Flux   ,QDF_Left  )

  CASE(QDF_Moment1Top   )
    cols(1) = COL_QDF04_Cell(QDF_Flux   ,QDF_Center)
    cols(2) = COL_QDF04_Cell(QDF_Flux   ,QDF_Right )
    cols(3) = COL_QDF04_Cell(QDF_Flux   ,QDF_Top   )
    cols(4) = COL_QDF04_Cell(QDF_Flux   ,QDF_Left  )
    cols(5) = COL_QDF04_Cell(QDF_Current,QDF_Top   )
    cols(6) = COL_QDF04_Cell(QDF_Flux   ,QDF_Bottom)

  CASE(QDF_Moment1Left  )
    cols(1) = COL_QDF04_Cell(QDF_Flux   ,QDF_Bottom)
    cols(2) = COL_QDF04_Cell(QDF_Flux   ,QDF_Center)
    cols(3) = COL_QDF04_Cell(QDF_Flux   ,QDF_Top   )
    cols(4) = COL_QDF04_Cell(QDF_Flux   ,QDF_Left  )
    cols(5) = COL_QDF04_Cell(QDF_Current,QDF_Left  )
    cols(6) = COL_QDF04_Cell(QDF_Flux   ,QDF_Right )

  CASE DEFAULT
    VS = MODPROC(mod_,proc_)//"this equation specification does not exist."
        CALL Stop(s=STR(VS))
    VS = ""

END SELECT

!shift all for energy groups
cols = cols + (g0-1)*offsets(3)

!!--end--
END FUNCTION




FUNCTION COEFF_QDF04_Cell( spec3 ) RESULT(coeff)
!!#### PURPOSE
!! Determines the coefficients for a specific equation of a cell.
!! This is a substitute procedure which uses a modified algorithm.

!!#### REQUIRED INPUT
!! * <spec3> must be one of:
!!     <QDF_Moment0Center>
!!     <QDF_Moment1Bottom>
!!     <QDF_Moment1Right>
!!     <QDF_Moment1Top>
!!     <QDF_Moment1Left>
INTEGER,INTENT(IN) :: spec3

!!#### REQUIRED OUTPUT
!! * coefficients for the specified equation
REAL(KIND_QDF) :: coeff(1:6)

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_ = "COEFF_QDF04plus_Cell"

!!#### LOCAL VARIABLES
TYPE(varying_string) :: VS
REAL(KIND_QDF) :: ci(3),cj0(3),cjs(2,3)

!!--begin--

coeff = 0._KIND_QDF

SELECT CASE( spec3 )
  CASE(QDF_Moment0Center)
    coeff(1) = hB
    coeff(2) = hR
    coeff(3) = hT
    coeff(4) = hL
    coeff(5) = (mact0 - macs0)*Vi

  CASE(QDF_Moment1Bottom)
    CALL MorelGradient(i0,jB,(/jR,jL,jT/),ci,cj0,cjs)
    coeff(1) = nB(1)*(cj0(1)  *EBxx0 + cj0(2)  *EBxy0) + nB(2)*(cj0(1)  *EBxy0 + cj0(2)  *EByy0)
    coeff(2) = nB(1)*(cjs(1,1)*ERxx0 + cjs(2,1)*ERxy0) + nB(2)*(cjs(1,1)*ERxy0 + cjs(2,1)*ERyy0)
    coeff(3) = nB(1)*( ci(1  )*ECxx0 +  ci(2  )*ECxy0) + nB(2)*( ci(1  )*ECxy0 +  ci(2  )*ECyy0)
    coeff(4) = nB(1)*(cjs(1,2)*ELxx0 + cjs(2,2)*ELxy0) + nB(2)*(cjs(1,2)*ELxy0 + cjs(2,2)*ELyy0)
    coeff(5) = mact0
    coeff(6) = nB(1)*(cjs(1,3)*ETxx0 + cjs(2,3)*ETxy0) + nB(2)*(cjs(1,3)*ETxy0 + cjs(2,3)*ETyy0)

  CASE(QDF_Moment1Right )
    CALL MorelGradient(i0,jR,(/jB,jT,jL/),ci,cj0,cjs)
    coeff(1) = nR(1)*(cjs(1,1)*EBxx0 + cjs(2,1)*EBxy0) + nR(2)*(cjs(1,1)*EBxy0 + cjs(2,1)*EByy0)
    coeff(2) = nR(1)*(cj0(1  )*ERxx0 + cj0(2  )*ERxy0) + nR(2)*(cj0(1  )*ERxy0 + cj0(2  )*ERyy0)
    coeff(3) = nR(1)*(cjs(1,2)*ETxx0 + cjs(2,2)*ETxy0) + nR(2)*(cjs(1,2)*ETxy0 + cjs(2,2)*ETyy0)
    coeff(4) = nR(1)*( ci(1  )*ECxx0 +  ci(2  )*ECxy0) + nR(2)*( ci(1  )*ECxy0 +  ci(2  )*ECyy0)
    coeff(5) = mact0
    coeff(6) = nR(1)*(cjs(1,3)*ELxx0 + cjs(2,3)*ELxy0) + nR(2)*(cjs(1,3)*ELxy0 + cjs(2,3)*ELyy0)

  CASE(QDF_Moment1Top   )
    CALL MorelGradient(i0,jT,(/jL,jR,jB/),ci,cj0,cjs)
    coeff(1) = nT(1)*( ci(1  )*ECxx0 +  ci(2  )*ECxy0) + nT(2)*( ci(1  )*ECxy0 +  ci(2  )*ECyy0)
    coeff(2) = nT(1)*(cjs(1,2)*ERxx0 + cjs(2,2)*ERxy0) + nT(2)*(cjs(1,2)*ERxy0 + cjs(2,2)*ERyy0)
    coeff(3) = nT(1)*(cj0(1  )*ETxx0 + cj0(2  )*ETxy0) + nT(2)*(cj0(1  )*ETxy0 + cj0(2  )*ETyy0)
    coeff(4) = nT(1)*(cjs(1,1)*ELxx0 + cjs(2,1)*ELxy0) + nT(2)*(cjs(1,1)*ELxy0 + cjs(2,1)*ELyy0)
    coeff(5) = mact0
    coeff(6) = nT(1)*(cjs(1,3)*EBxx0 + cjs(2,3)*EBxy0) + nT(2)*(cjs(1,3)*EBxy0 + cjs(2,3)*EByy0)

  CASE(QDF_Moment1Left  )
    CALL MorelGradient(i0,jL,(/jB,jT,jR/),ci,cj0,cjs)
    coeff(1) = nL(1)*(cjs(1,1)*EBxx0 + cjs(2,1)*EBxy0) + nL(2)*(cjs(1,1)*EBxy0 + cjs(2,1)*EByy0)
    coeff(2) = nL(1)*( ci(1  )*ECxx0 +  ci(2  )*ECxy0) + nL(2)*( ci(1  )*ECxy0 +  ci(2  )*ECyy0)
    coeff(3) = nL(1)*(cjs(1,2)*ETxx0 + cjs(2,2)*ETxy0) + nL(2)*(cjs(1,2)*ETxy0 + cjs(2,2)*ETyy0)
    coeff(4) = nL(1)*(cj0(1  )*ELxx0 + cj0(2  )*ELxy0) + nL(2)*(cj0(1  )*ELxy0 + cj0(2  )*ELyy0)
    coeff(5) = mact0
    coeff(6) = nL(1)*(cjs(1,3)*ERxx0 + cjs(2,3)*ERxy0) + nL(2)*(cjs(1,3)*ERxy0 + cjs(2,3)*ERyy0)

  CASE DEFAULT
    VS = MODPROC(mod_,proc_)//"this equation specification does not exist."
    CALL Stop(s=STR(VS))
    VS = ""
END SELECT

!write(33,*)LABEL_equation(spec3)
!write(33,*)"coeff="
!wrIte(33,*)coeff
!write(33,*)" "

!!--end--
END FUNCTION






SUBROUTINE MorelGradientDotNormal(i,j0,js,ci,cj0,cjs)
INTEGER :: i,js(:),j0
REAL(KIND_QDF) :: ci,cj0
REAL(KIND_QDF) :: cjs(SIZE(js))
REAL(KIND_QDF) :: cc(2),cvol,fc0(2),fn0(2),r0(2),a,farea,fc(2),fn(2),d,b
INTEGER :: n,j
!!--begin--

cc   = CellCentroid(Mesh_,i)
cvol = CellVolume  (Mesh_,i)
fc0  = FaceCentroid(Mesh_,j0)
fn0  = FaceNormal  (Mesh_,j0)
r0   = fc0-cc
a    = DOT_PRODUCT(r0,fn0)

ci  = -c_1/a
cj0 =  c_1/a

DO n=1,SIZE(js)
 j = js(n)
 farea = FaceArea    (Mesh_,j)
 fc    = FaceCentroid(Mesh_,j)
 fn    = FaceNormal  (Mesh_,j)
 d     = DOT_PRODUCT(fn,fn0)
 b     = DOT_PRODUCT(r0,fn)
 cjs(j) = (farea/cvol)*(d-b/a)
END DO

!!--end--
END SUBROUTINE



SUBROUTINE MorelGradient(i,j0,js,ci,cj0,cjs)
INTEGER :: i,js(:),j0
REAL(KIND_QDF) :: ci(2),cj0(2)
REAL(KIND_QDF) :: cjs(2,SIZE(js))
REAL(KIND_QDF) :: cc(2),cvol,fc0(2),fn0(2),r0(2),a,farea,fc(2),fn(2),b
INTEGER :: n,j
!!--begin--
cc   = CellCentroid(Mesh_,i)
cvol = CellVolume  (Mesh_,i)
fc0  = FaceCentroid(Mesh_,j0)
fn0  = FaceNormal  (Mesh_,j0)
r0   = fc0-cc
a    = DOT_PRODUCT(r0,fn0)


ci  = -fn0/a
cj0 =  fn0/a

DO n=1,SIZE(js)
 j = js(n)
 farea = FaceArea    (Mesh_,j)
 fc    = FaceCentroid(Mesh_,j)
 fn    = FaceNormal  (Mesh_,j)
 b     = DOT_PRODUCT(r0,fn)
 cjs(:,n) = (farea/cvol)*(fn-fn0*b/a)
END DO

!!--end--
END SUBROUTINE



FUNCTION COL_QDF04_Cell( spec1 , spec2 ) RESULT(col)
!!#### PURPOSE
!! Determines the column of a cell-specified unknown.

!!#### REQUIRED INPUT
!! * <spec1> must be one of:
!!     <QDF_Flux>
!!     <QDF_Current>
!! * <spec2> must be one of:
!!     <QDF_Center> (for QDF_Flux only)
!!     <QDF_Bottom>
!!     <QDF_Right>
!!     <QDF_Top>
!!     <QDF_Left>
INTEGER,INTENT(IN) :: spec1
INTEGER,INTENT(IN) :: spec2

!!#### REQUIRED OUTPUT
!! * column number for the specified unknown,
!!   assuming <QDF_System02> method's ordering of unknowns
INTEGER :: col

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_ = "COL_QDF04_Cell"

!!#### LOCAL VARIABLES
TYPE(varying_string) :: VS


!!--begin--
SELECT CASE( spec1 )

  CASE(QDF_Flux)
   SELECT CASE(spec2)
    CASE(QDF_Center) ; col = i0
    CASE(QDF_Bottom) ; col = jB+offsets(1)
    CASE(QDF_Right ) ; col = jR+offsets(1)
    CASE(QDF_Left  ) ; col = jL+offsets(1)
    CASE(QDF_Top   ) ; col = jT+offsets(1)
    CASE DEFAULT
          VS = MODPROC(mod_,proc_)//"this flux specification does not exist."
          CALL Stop(s=STR(VS))
          VS = ""
   END SELECT

  CASE(QDF_Current)
   SELECT CASE(spec2)
    CASE(QDF_Bottom) ; col = jB+offsets(2)
    CASE(QDF_Right ) ; col = jR+offsets(2)
    CASE(QDF_Left  ) ; col = jL+offsets(2)
    CASE(QDF_Top   ) ; col = jT+offsets(2)
    CASE DEFAULT
          VS = MODPROC(mod_,proc_)//"this current specification does not exist."
          CALL Stop(s=STR(VS))
          VS = ""
   END SELECT

  CASE DEFAULT
    VS = MODPROC(mod_,proc_)//"this combination of unknown specifications does not exist."
        CALL Stop(s=STR(VS))
        VS = ""

END SELECT

!shift all for energy groups
col = col + (g0-1)*offsets(3)

!! Apply reordering.
col = col_reorder(col)

!!--end--
END FUNCTION


FUNCTION COL_QDF04_Face( spec1 ) RESULT(col)
!!#### PURPOSE
!! Determines the column of the current face
!! unknown specified by <spec1>.

!!#### REQUIRED INPUT
!! * specificiations for the column
!!   <spec1> may be one of:
!!     <QDF_Flux>
!!     <QDF_Current>
INTEGER,INTENT(IN) :: spec1

!!#### REQUIRED OUTPUT
!! * column number for the specified unknown, assuming
!!   <QDF_System02> method's ordering of unknowns
INTEGER :: col

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_ = "COL_QDF04_Face"

!!#### LOCAL VARIABLES
TYPE(varying_string) :: VS

!!--begin--
SELECT CASE( spec1 )
  CASE( QDF_Flux   ) ; col = j0+offsets(1)
  CASE( QDF_Current) ; col = j0+offsets(2)

  CASE DEFAULT
    VS = MODPROC(mod_,proc_)//&
      "this combination of unknown specifications does not exist."
        CALL Stop(s=STR(VS))
        VS = ""
END SELECT

!shift for energy group
col = col + (g0-1)*offsets(3)

!! Apply reordering.
col = col_reorder(col)

!!--end--
END FUNCTION


FUNCTION ROW_QDF04_Cell( spec3 ) RESULT(row)
!!#### PURPOSE
!! Determines the row for a specific equation of a cell.

!!#### REQUIRED INPUT
!! * <spec3> must be one of:
!!     <QDF_Moment0Center>
!!     <QDF_Moment1Bottom>
!!     <QDF_Moment1Right>
!!     <QDF_Moment1Top>
!!     <QDF_Moment1Left>
INTEGER,INTENT(IN) :: spec3

!!#### REQUIRED OUTPUT
!! * row number for the specified equation,
!!   assuming <QDF_System02> method's ordering of unknowns
INTEGER :: row

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_ = "ROW_QDF04_Cell"

!!#### LOCAL VARIABLES
TYPE(varying_string) :: VS

!!--begin--
IF( RowAsTheyCome )THEN
 save_row = save_row + 1
 row = save_row
ELSE

 SELECT CASE( spec3 )
   CASE(QDF_Moment0Center) ;         ; row = COL_QDF04_Cell(QDF_Flux,QDF_Center) ! i0
   CASE(QDF_Moment1Bottom) ; j0 = jB ; row = COL_QDF04_Face(MERGE(QDF_Current,QDF_Flux,IsMaster(j0)))
   CASE(QDF_Moment1Right ) ; j0 = jR ; row = COL_QDF04_Face(MERGE(QDF_Current,QDF_Flux,IsMaster(j0)))
   CASE(QDF_Moment1Top   ) ; j0 = jT ; row = COL_QDF04_Face(MERGE(QDF_Current,QDF_Flux,IsMaster(j0)))
   CASE(QDF_Moment1Left  ) ; j0 = jL ; row = COL_QDF04_Face(MERGE(QDF_Current,QDF_Flux,IsMaster(j0)))
   CASE DEFAULT
     VS = MODPROC(mod_,proc_)//"this equation specification does not exist."
         CALL Stop(s=STR(VS))
         VS = ""
 END SELECT

 !shift for energy group
 row = row + (g0-1)*offsets(3)
 !! Apply reordering.
 row = row_reorder(row)
END IF

!increment debugging row_used array
IF( ALLOCATED(row_used) )THEN
 row_used(row) = row_used(row) + 1
END IF

!!--end--
END FUNCTION


FUNCTION ROW_QDF04_BoundaryFace(BC0,LOBC0) RESULT(ROW)
!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: BC0,LOBC0

!!#### REQUIRED OUTPUT
INTEGER :: ROW

!!--begin--
IF( RowAsTheyCome )THEN
 save_row = save_row + 1
 row = save_row
ELSE

 row = COL_QDF04_Face(QDF_Current)
 row = row + (g0-1)*offsets(3)

 !! Apply reordering.
 row = row_reorder(row)

END IF

!debugging row_used
IF( ALLOCATED(row_used) )THEN
 row_used(row) = row_used(row) + 1
END IF

!!--end--
END FUNCTION


FUNCTION ROW_QDF04_InterFace(spec2,spec5) RESULT(ROW)
!!#### PURPOSE
!! Return the appropriate row for an unknown spec
!!   <spec2> = QDF_Flux,QDF_Current
!! and a condition spec
!!   <spec5> = QDF_Strong,QDF_Weak

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: spec2,spec5

!!#### REQUIRED OUTPUT
INTEGER :: ROW

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_="ROW_QDF04_InterFace"

!!#### LOCAL VARIABLES
TYPE(varying_string) :: VS

!!--begin--
IF( RowAsTheyCome )THEN
 save_row = save_row + 1
 row = save_row
ELSE

 SELECT CASE(spec5)

   !strong condition
   CASE(QDF_Strong)
     j0  = Interfaces(jn0)%subs(js0)
     row = COL_QDF04_Face(QDF_Current)

   !weak condition
   CASE(QDF_Weak)
     j0  = Interfaces(jn0)%master
     row = COL_QDF04_Face(QDF_Flux)

   CASE DEFAULT
     VS = MODPROC(mod_,proc_)//"this interface specification does not exist."
         CALL Stop(s=STR(VS))
         VS = ""

 END SELECT

 !! Incorporate energy groups.
 row = row + (g0-1)*offsets(3)

 !! Apply reordering.
 row = row_reorder(row)

END IF

!debugging row_used
IF( ALLOCATED(row_used) )THEN
 row_used(row) = row_used(row) + 1
END IF

!!--end--
END FUNCTION



FUNCTION COLS_QDF04_InterFace(spec2,spec5) RESULT(COLS)
!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: spec2,spec5

!!#### REQUIRED OUTPUT
INTEGER :: COLS(1:6)

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_="COLS_QDF04_InterFace"

!!#### LOCAL VARIABLES
TYPE(varying_string) :: VS

!!--begin--
!init
cols = ERROR(cols)

SELECT CASE(spec5)

  !weak condition
  CASE(QDF_Weak)
    j0      = Interfaces(jn0)%master
    cols(1) = COL_QDF04_Face(spec2)
    DO js0 = 1,SIZE(Interfaces(jn0)%subs)
     j0          = Interfaces(jn0)%subs(js0)
     cols(1+js0) = COL_QDF04_Face(spec2)
    END DO

  !strong condition
  CASE(QDF_Strong)
    j0      = Interfaces(jn0)%master
    cols(1) = COL_QDF04_Face(spec2)
    j0      = Interfaces(jn0)%subs(js0)
    cols(2) = COL_QDF04_Face(spec2)

  CASE DEFAULT
    VS = MODPROC(mod_,proc_)
        CALL Stop(s=STR(VS)//&
      "this interface specification does not exist.")
    VS = ""

END SELECT

!!--end--
END FUNCTION


FUNCTION COEFF_QDF04_Interface(spec2,spec5) RESULT(Coeff)
!!#### PURPOSE
!! Return coefficients for interfaces.

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: spec2,spec5

!!#### REQUIRED OUTPUT
REAL(KIND_QDF) :: Coeff(1:6)

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_="COEFF_QDF04_InterFace"

!!#### LOCAL VARIABLES
TYPE(varying_string) :: VS


!!--begin--
!init
coeff = ERROR(coeff)

!select variable type
SELECT CASE(spec2)

 !flux interface condition (differs only in sign
 !from the current interface condition)
 CASE(QDF_Flux)

   SELECT CASE(spec5)
    !weak condition
    CASE(QDF_Weak)
      coeff(1) = h0
      DO js0 = 1,SIZE(Interfaces(jn0)%subs)
       coeff(1+js0) = -hsubs0(js0)
      END DO

    !strong condition
    CASE(QDF_Strong)
      coeff(1) = 1.0_KIND_QDF
      coeff(2) = -1.0_KIND_QDF

    !default condition
    CASE DEFAULT
      VS = MODPROC(mod_,proc_)
          CALL Stop(s=STR(VS)//&
        "this flux interface specification (spec5) does not exist.")
      VS = ""

   END SELECT

 !current interface condition (differs only in sign
 !from the flux interface condition)
 CASE(QDF_Current)

   SELECT CASE(spec5)
    !weak condition
    CASE(QDF_Weak)
      coeff(1) = h0
      DO js0 = 1,SIZE(Interfaces(jn0)%subs)
       coeff(1+js0) = hsubs0(js0)
      END DO

    !strong condition
    CASE(QDF_Strong)
      coeff(1) = 1.0_KIND_QDF
      coeff(2) = 1.0_KIND_QDF

    !default condition
    CASE DEFAULT
      VS = MODPROC(mod_,proc_)
          CALL Stop(s=STR(VS)//&
        "this current interface specification (spec5) does not exist.")
      VS = ""
   END SELECT

 CASE DEFAULT
   VS = MODPROC(mod_,proc_)
   CALL Stop(s=STR(VS)//&
     "this interface specification (spec2) does not exist.")
   VS = ""
END SELECT

!!--end--
END FUNCTION


FUNCTION RHS_QDF04_Interface(spec2,spec5) RESULT(rhs)
!!#### PURPOSE
!! Return right sides for interfaces.

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: spec2,spec5

!!#### REQUIRED OUTPUT
REAL(KIND_QDF) :: rhs

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_="rhs_QDF04_InterFace"

!!#### LOCAL VARIABLES
TYPE(varying_string) :: VS

!!--begin--
!init
rhs = error(rhs)

SELECT CASE(spec2)
 CASE(QDF_Flux)    ; rhs = 0._KIND_QDF
 CASE(QDF_Current) ; rhs = 0._KIND_QDF
 CASE DEFAULT
   VS = MODPROC(mod_,proc_)
   CALL Stop(s=STR(VS)//&
     "this interface specification does not exist.")
   VS = ""
END SELECT
!!--end--
END FUNCTION



END MODULE
