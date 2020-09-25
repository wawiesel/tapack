!!# MODULE <<USR_QuasiDiffusion>>
MODULE USR_QuasiDiffusion01
!!## PURPOSE
!! The user-module for the first variant
!! of the quasidiffusion routines.

!!## METHOD
!! The ordering of the equations is important.  We solve
!!        $$   A \vec{x} = \vec{b}  .$$
!! The number of cells is $N_i$.
!! The number of faces is $N_j$.
!! With quadrilaterals, each cell has 5 equations and a boundary
!! condition at each face on the boundary.  For faces not on
!! the boundary, every face is shared by exactly two cells.
!! Thus there are $M = 2 N_j + N_i$ unknowns and we must
!! assemble an $M \times M$ matrix and a RHS vector of
!! length $M$.  The following ordering applies:
!!  * The first $N_i$ coeff are for cell-average
!!    scalar Fluxes.
!!  * The next $N_j$ coeff are for face-average
!!    scalar Fluxes.
!!  * The final $N_j$ coeff are for face-average
!!    Currents.


!!## EXTERNAL KINDS
USE KND_XSExpansion                      !!((02-A-KND_XSExpansion.f90))
USE KND_MoCshort                         !!((03-A-KND_MoCshort.f90))
USE KND_QuasiDiffusion                   !!((02-A-KND_QuasiDiffusion.f90))

!!## PARAMETERS
USE PAR_Constants_Rdp,ONLY: c_4_times_PI !!((02-A-PAR_Constants_Rdp.f90))
USE PAR_MoCshort     ,ONLY: KEY_BC       !!((03-A-PAR_MoCshort.f90))
USE PAR_QuasiDiffusion                   !!((05-C-PAR_QuasiDiffusion.f90))
USE ISO_varying_string                   !!((03-A-ISO_varying_string.f90))

!!## EXTERNAL PROCEDURES
USE SUB_Reallocate                       !!((04-B-SUB_Reallocate.f90))
USE SUB_Pause                            !!((04-B-SUB_Pause.f90))
USE SUB_Stop                             !!((04-B-SUB_Stop.f90))
USE FUN_EQUILOC                          !!((03-A-FUN_EQUILOC.f90))
USE FUN_Default                          !!((04-A-FUN_Default.f90))
USE FUN_SIZEa                            !!((06-B-FUN_SIZEa.f90))
USE FUN_Sequence                         !!((03-A-FUN_Sequence.f90))
USE SUB_Shake                            !!((06-B-SUB_Shake.f90))
USE FUN_Error                            !!((04-A-FUN_Error.f90))

!!## GLOBAL PRINTING SUBROUTINES
USE PRN_Text                             !!((07-B-PRN_Text.f90))
USE PRN_Matrix_dmat                      !!((09-B-PRN_Matrix_dmat.f90))

!!## USER MODULES
!! * feedback user module
USE USR_fdbk                             !!((08-C-USR_fdbk.f90))
USE USR_QuasiDiffusion                   !!((67-B-USR_QuasiDiffusion.f90))

!!## GLOBAL TOOLBOXES
!! * SMLib sparse matrix computation
USE USR_SMlib_Matrix_Arithmetic          !!((19-B-USR_SMlib_Matrix_Arithmetic.f90))
USE USR_SMlib_ILU                        !!((20-B-USR_SMlib_ILU.f90))
USE USR_SMlib_CGS                        !!((20-B-USR_SMlib_CGS.f90))
USE USR_SMlib_Band_LU                    !!((17-B-USR_SMlib_Band_LU.f90))
USE USR_SMlib_Band_Gauss_Solver          !!((20-B-USR_SMlib_Band_Gauss_Solver.f90))

!!## GLOBAL LIBRARIES
USE LIB_Prompts                          !!((06-B-LIB_Prompts.f90))
USE LIB_GenericPhrases                   !!((07-B-LIB_GenericPhrases.f90))
USE LIB_Norm                             !!((04-B-LIB_Norm.f90))

!!## USER MODULES
USE USR_DiscreteOrdinates                !!((34-B-USR_DiscreteOrdinates.f90))

!!## GLOBAL TOOLBOXES
USE TBX_Mesh,ONLY: &                     !!((15-B-TBX_Mesh.f90))
 TYPE_Mesh,NUM_Cells,NUM_Faces,NUM_Verts,KIND_MSH,&
 NUM_Dimensions,FaceCentroid,FaceNormal,CellVolume,FaceArea,&
 UPDATE_BoundaryFaces
USE TBX_ComputationalGeometry            !!((09-A-TBX_ComputationalGeometry.f90))
USE USR_MoCshort,ONLY: IsReflective      !!((48-C-USR_MoCshort.f90))


!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## DEFAULT ACCESS
PRIVATE

!!## ACCESS
PUBLIC :: SETUP_QDF01,UPDATE_QDF01,RESIDUAL_QDF01,RECALL_QDF01

!!## IDENTIFICATION
CHARACTER(*),PARAMETER :: mod_="USR_QuasiDiffusion01"

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

CHARACTER(17),PARAMETER :: LABEL_Equation(1:6)=(/"Moment0Center    ",&
                                                 "Moment1Bottom    ",&
                                                 "Moment1Right     ",&
                                                 "Moment1Top       ",&
                                                 "Moment1Left      ",&
                                                 "BoundaryCondition" /)
!!#### LOCAL VARIABLES
REAL(KIND_QDF) :: coeff(1:5),FixBdryVals0
INTEGER       :: row,cols(1:5),i0,g0,j0,jb0,jd0
REAL(KIND_MSH),ALLOCATABLE,DIMENSION(:) :: lB,lR,lT,lL,lh,lv
REAL(KIND_MSH),ALLOCATABLE,DIMENSION(:) :: nB,nR,nT,nL
REAL(KIND_MSH) :: hB,hR,hT,hL
REAL(KIND_MSH) :: Vi,VB,VR,VT,VL
INTEGER       :: kBL,kBR,kTR,kTL
INTEGER       :: jB,jR,jT,jL
REAL(KIND_MAC) :: macs0
REAL(KIND_MAC) :: mact0
INTEGER       :: BC0,LOBC0
INTEGER :: save_row = 0
REAL(KIND_ExtSource)  :: extsource0
REAL(KIND_ScalarFlux) :: ECxx0,ECyy0,ECxy0
REAL(KIND_ScalarFlux) :: EBxx0,EByy0,EBxy0
REAL(KIND_ScalarFlux) :: ETxx0,ETyy0,ETxy0
REAL(KIND_ScalarFlux) :: ELxx0,ELyy0,ELxy0
REAL(KIND_ScalarFlux) :: ERxx0,ERyy0,ERxy0
REAL(KIND_ScalarFlux) :: ScalarFluxIN0
REAL(KIND_Current)    :: CurrentIN0
REAL(KIND_ScalarFlux) :: C0

!! * the list of offsets <offset(1)> is where the first face
!!   scalar flux is listed, <offset(2)> is where the first
!!   face current is listed, <offset(3)> is the total number
!!   of unknowns for one energy group.
INTEGER,ALLOCATABLE  :: offsets(:)
INTEGER              :: NQDF,NZMAX,Ni0,Nj0,Njb0,Nk0,NDim0,Ng0
INTEGER,ALLOCATABLE  :: row_reorder(:)
INTEGER,ALLOCATABLE  :: col_reorder(:)
LOGICAL :: RandomShift = .FALSE.,RowAsTheyCome=.FALSE.

CONTAINS


SUBROUTINE SETUP_QDF01( Ng , Mesh , A , b , x , Jbdry , Jdomn , FdBk )
!!#### PURPOSE
!! Set up the system for the solution of the simplest system
!! of QDF equations where the mesh is a structured quadrilateral
!! grid and interface conditions are internal.


!!#### REQUIRED INPUT
INTEGER       ,INTENT(IN)    :: Ng

!!#### REQUIRED OUTPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh
TYPE(MSR)     ,INTENT(INOUT) :: A
REAL(KIND_QDF) ,POINTER       :: x(:),b(:)
INTEGER       ,POINTER       :: Jbdry(:),Jdomn(:)

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback <FdBk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!--begin--
!! Set values of numbers for things.
Ni0    = NUM_Cells(Mesh)
Nj0    = NUM_Faces(Mesh)
Nk0    = NUM_Verts(Mesh)
NDim0  = NUM_Dimensions(Mesh)
Ng0    = Ng

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
CALL UPDATE_BoundaryFaces(Mesh,Jbdry=Jbdry,Jdomn=Jdomn)

!! Calculate number of boundary faces.
Njb0 = SIZE(Jbdry)

!! Determine total number of equations.
NQDF  = Ng0*( 5*Ni0 +   Njb0)
!! Determine number of nonzero entries in sparse matrix (max).
NZMAX = Ng0*(25*Ni0 + 2*Njb0)

!! Allocate and initialize.
CALL SETUP_QDF_MSR(NQDF,NZMAX,A,x,b)

!! Allocate locals.
IF( ALLOCATED(lB) )THEN
 !!WARNING!!
 !!the allocation status of lB is USEd to check if all these are
 !!allocated or not---as long as the only way they are allocated
 !!is through this routine, everything is fine.
 DEALLOCATE(lB,lR,lT,lL,lh,lv)
 DEALLOCATE(nB,nR,nT,nL)
END IF
ALLOCATE(lB(NDim0),lR(NDim0),lT(NDim0),lL(NDim0),lh(NDim0),lv(NDim0))
ALLOCATE(nB(NDim0),nR(NDim0),nT(NDim0),nL(NDim0))

!! Allocate reordering arrays.
ALLOCATE( row_reorder(1:NQDF) )
ALLOCATE( col_reorder(1:NQDF) )
!! Initialize.
col_reorder = Sequence(1,1,NQDF)
row_reorder = Sequence(1,1,NQDF)

!! Randomly shift ordering.
IF( RandomShift )THEN
 CALL Shake(col_reorder)
 CALL Shake(row_reorder)
ELSE
 !col_reorder = CSHIFT(col_reorder,Njb0)
 !row_reorder = CSHIFT(row_reorder,Njb0)
END IF
!!--end--
END SUBROUTINE



SUBROUTINE SETUP_QDF01_INTERIOR(g,i,Mesh,mact,macs,l_,ExtSource,&
  ECxx , ECyy , ECxy , Exx , Eyy , Exy , Noisy,Unit,Interactive)

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

!!#### OPTIONAL INPUT
LOGICAL,OPTIONAL,INTENT(IN) :: Noisy
INTEGER,OPTIONAL,INTENT(IN) :: Unit
LOGICAL,OPTIONAL,INTENT(IN) :: Interactive

!!#### LOCAL PARAMETERS
LOGICAL,PARAMETER :: DEFAULT_Noisy=.FALSE.
INTEGER,PARAMETER :: DEFAULT_Unit=window_unit
LOGICAL,PARAMETER :: DEFAULT_Interactive=.FALSE.

!!#### LOCAL VARIABLES
LOGICAL :: Noisy_
INTEGER :: Unit_
LOGICAL :: Interactive_

!!--begin--
!formats
100 FORMAT(a,i0)
101 FORMAT(a,1e21.10)
102 FORMAT(a,2e21.10)

!handle optionals
Noisy_       = DEFAULT(DEFAULT_Noisy,Noisy)
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
IF( jB>0 )THEN
 kBL = Mesh%faces(ABS(jB))%vertlist(1)
ELSE
 kBL = Mesh%faces(ABS(jB))%vertlist(SIZE(Mesh%faces(ABS(jB))%vertlist))
END IF

!get bottom right vertex
IF( jR>0 )THEN
 kBR = Mesh%faces(ABS(jR))%vertlist(1)
ELSE
 kBR = Mesh%faces(ABS(jR))%vertlist(SIZE(Mesh%faces(ABS(jR))%vertlist))
END IF

!get top right vertex
IF( jT>0 )THEN
 kTR = Mesh%faces(ABS(jT))%vertlist(1)
ELSE
 kTR = Mesh%faces(ABS(jT))%vertlist(SIZE(Mesh%faces(ABS(jT))%vertlist))
END IF

!get top left vertex
IF( jL>0 )THEN
 kTL = Mesh%faces(ABS(jL))%vertlist(1)
ELSE
 kTL = Mesh%faces(ABS(jL))%vertlist(SIZE(Mesh%faces(ABS(jL))%vertlist))
END IF

IF( Noisy_ )THEN
 WRITE(Unit_,100)"   * bottom-left  vertex, kBL=",kBL
 WRITE(Unit_,100)"   * bottom-right vertex, kBR=",kBR
 WRITE(Unit_,100)"   * top-right    vertex, kTR=",kTR
 WRITE(Unit_,100)"   * top-left     vertex, kTL=",kTL
END IF

!get bottom distance
lB = Mesh%verts(:,kBR) - Mesh%verts(:,kBL)

!get right distance
lR = Mesh%verts(:,kTR) - Mesh%verts(:,kBR)

!get top distance
lT = Mesh%verts(:,kTL) - Mesh%verts(:,kTR)

!get left distance
lL = Mesh%verts(:,kBL) - Mesh%verts(:,kTL)

!get vertical distance
lv = FaceCentroid(Mesh,ABS(jT)) - FaceCentroid(Mesh,ABS(jB))

!get horizontal distance
lh = FaceCentroid(Mesh,ABS(jL)) - FaceCentroid(Mesh,ABS(jR))

IF( Noisy_ )THEN
 WRITE(Unit_,102)"   * bottom    distance, lB=",lB
 WRITE(Unit_,102)"   * right     distance, lR=",lR
 WRITE(Unit_,102)"   * top       distance, lT=",lT
 WRITE(Unit_,102)"   * left      distance, lL=",lL
 WRITE(Unit_,102)"   * central-v distance, lv=",lv
 WRITE(Unit_,102)"   * central-h distance, lh=",lh
END IF

!get the volume of a cell
Vi = CellVolume(Mesh,i0)

!get the volume of the bottom half-cell
VB = xySAREA_Qg( RESHAPE((/Mesh%Verts(:,kBL),&
                    Mesh%Verts(:,kBR),&
                    FaceCentroid(Mesh,ABS(jR)),&
                    FaceCentroid(Mesh,ABS(jL)) /),(/2,4/)) )

!get the volume of the right half-cell
VR = xySAREA_Qg( RESHAPE((/FaceCentroid(Mesh,ABS(jB)),&
                    Mesh%Verts(:,kBR),&
                    Mesh%Verts(:,kTR),&
                    FaceCentroid(Mesh,ABS(jT)) /),(/2,4/)) )

!get the volume of the top half-cell
VT = xySAREA_Qg( RESHAPE((/FaceCentroid(Mesh,ABS(jL)),&
                    FaceCentroid(Mesh,ABS(jR)),&
                    Mesh%Verts(:,kTR),&
                    Mesh%Verts(:,kTL) /),(/2,4/)) )

!get the volume of the left half-cell
VL = xySAREA_Qg( RESHAPE((/Mesh%Verts(:,kBL),&
                    FaceCentroid(Mesh,ABS(jB)),&
                    FaceCentroid(Mesh,ABS(jT)),&
                    Mesh%Verts(:,kTL) /),(/2,4/)) )

IF( Noisy_ )THEN
 WRITE(Unit_,101)"   * full        cell volume, Vi=",Vi
 WRITE(Unit_,101)"   * bottom half-cell volume, VB=",VB
 WRITE(Unit_,101)"   * right  half-cell volume, VR=",VR
 WRITE(Unit_,101)"   * top    half-cell volume, VT=",VT
 WRITE(Unit_,101)"   * left   half-cell volume, VL=",VL
END IF

!! Get areas of faces.
hB = FaceArea(Mesh,jB)
hR = FaceArea(Mesh,jR)
hT = FaceArea(Mesh,jT)
hL = FaceArea(Mesh,jL)
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
extsource0 = ExtSource(g0,i0)
IF( Noisy_ )THEN
 WRITE(Unit_,101)"     * external source, extsource=",extsource0
END IF

!! Get cell Eddington factors.
ECxx0 = ECxx(g0,i0)
ECxy0 = ECxy(g0,i0)
ECyy0 = ECyy(g0,i0)
IF( Noisy_ )THEN
 WRITE(Unit_,101)"     * cell ECxx=",ECxx0
 WRITE(Unit_,101)"     * cell ECyy=",ECyy0
 WRITE(Unit_,101)"     * cell ECxy=",ECxy0
END IF

!! Get face Eddington factors.
EBxx0 = Exx(g0,ABS(jB))
ERxx0 = Exx(g0,ABS(jR))
ETxx0 = Exx(g0,ABS(jT))
ELxx0 = Exx(g0,ABS(jL))
IF( Noisy_ )THEN
 WRITE(Unit_,101)"     * bottom face EBxx=",EBxx0
 WRITE(Unit_,101)"     * right  face ERxx=",ERxx0
 WRITE(Unit_,101)"     * top    face ETxx=",ETxx0
 WRITE(Unit_,101)"     * left   face ELxx=",ELxx0
END IF

EByy0 = Eyy(g0,ABS(jB))
ERyy0 = Eyy(g0,ABS(jR))
ETyy0 = Eyy(g0,ABS(jT))
ELyy0 = Eyy(g0,ABS(jL))
IF( Noisy_ )THEN
 WRITE(Unit_,101)"     * bottom face EByy=",EByy0
 WRITE(Unit_,101)"     * right  face ERyy=",ERyy0
 WRITE(Unit_,101)"     * top    face ETyy=",ETyy0
 WRITE(Unit_,101)"     * left   face ELyy=",ELyy0
END IF

EBxy0 = Exy(g0,ABS(jB))
ERxy0 = Exy(g0,ABS(jR))
ETxy0 = Exy(g0,ABS(jT))
ELxy0 = Exy(g0,ABS(jL))
IF( Noisy_ )THEN
 WRITE(Unit_,101)"     * bottom face EBxy=",EBxy0
 WRITE(Unit_,101)"     * right  face ERxy=",ERxy0
 WRITE(Unit_,101)"     * top    face ETxy=",ETxy0
 WRITE(Unit_,101)"     * left   face ELxy=",ELxy0
END IF

IF( .FALSE. )THEN
 CALL Esmooth( 0.5_KIND_AngularFlux , &
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

SUBROUTINE PUT_QDF01(g,i,jb,jd,j)
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



SUBROUTINE RESIDUAL_QDF01( res , relres , x , iter , Mesh , mact , macs , l_ , &
 ExtSource , ECxx , ECyy , ECxy , Exx , Eyy , Exy , Jbdry , Jdomn , C , &
 ScalarFluxIN , CurrentIN , BC , LOBC , FixBdryVals , FdBk , Noisy,Unit,Interactive,Unit_resid)
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
REAL(KIND_ScalarFlux),INTENT(IN) :: ECxx(:,:),ECyy(:,:),ECxy(:,:)
REAL(KIND_ScalarFlux),INTENT(IN) :: Exx(:,:),Eyy(:,:),Exy(:,:)
INTEGER             ,INTENT(IN)  :: BC(:)
INTEGER             ,POINTER     :: LOBC(:)
REAL(KIND_QDF)       ,INTENT(IN) :: FixBdryVals(:)
INTEGER             ,INTENT(IN)  :: l_(:)
REAL(KIND_ExtSource) ,INTENT(IN) :: Extsource(:,:)

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback <FdBk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### OPTIONAL INPUT
LOGICAL,OPTIONAL,INTENT(IN) :: Noisy
INTEGER,OPTIONAL,INTENT(IN) :: Unit
LOGICAL,OPTIONAL,INTENT(IN) :: Interactive
INTEGER,OPTIONAL,INTENT(IN) :: Unit_resid

!!#### LOCAL PARAMETERS
LOGICAL,PARAMETER :: DEFAULT_Noisy=.FALSE.
INTEGER,PARAMETER :: DEFAULT_Unit=window_unit
LOGICAL,PARAMETER :: DEFAULT_Interactive=.FALSE.
INTEGER,PARAMETER :: DEFAULT_Unit_resid=0

!!#### LOCAL VARIABLES
INTEGER :: i,g,n,jb
LOGICAL :: Noisy_,Interactive_
CHARACTER(80),ALLOCATABLE :: Label(:)
CHARACTER(120) :: Header
INTEGER :: Unit_resid_,Unit_

!!--begin--
!handle optionals
Noisy_       = DEFAULT(DEFAULT_Noisy,Noisy)
Unit_        = DEFAULT(DEFAULT_Unit,Unit)
Interactive_ = DEFAULT(DEFAULT_Interactive,Interactive)
Unit_resid_  = DEFAULT(DEFAULT_Unit_resid,Unit_resid)

IF( Unit_resid_/=0 )THEN
 ALLOCATE(Label(SIZE(x)))
 !header
 WRITE(Header,"(a7,2x,a3,2x,a5,2x,a17,2x,a5,2x,a5,2x,a10,2x,a16,2x,a16)")&
   "row","g","i","eqn type","j","jdmn","bc type","res"," relres"
END IF

!! row number
save_row = 0
row   = 0

!loop through the cell indices setting 5 equations per cell
DO g=1,Ng0

 DO i=1,Ni0
  !0. setup interior locals
  CALL SETUP_QDF01_INTERIOR(g,i,Mesh,mact,macs,l_,ExtSource,&
   ECxx , ECyy , ECxy , Exx , Eyy , Exy )

  !1. for each of 5 equations
  DO n=1,5
   IF( Unit_resid_/=0 )THEN
    CALL RESIDUAL_QDF01_INTERIOR(n,res,relres,x,label)
   ELSE
    CALL RESIDUAL_QDF01_INTERIOR(n,res,relres,x)
   END IF
  END DO

 END DO

 !6. set the boundary conditions equations
 DO jb=1,Njb0

  !! 6.a setup boundary locals
  CALL SETUP_QDF01_BOUNDARY(g,jb,Mesh,Jbdry,Jdomn, C , &
    ScalarFluxIN , CurrentIN , BC , LOBC , FixBdryVals )

  !! 6.b Get the boundary conditions.
  IF( Unit_resid_/=0 )THEN
   CALL RESIDUAL_QDF01_BC(res,relres,x,label)
  ELSE
   CALL RESIDUAL_QDF01_BC(res,relres,x)
  END IF

 END DO
END DO

!! output residuals
IF( Unit_resid_/=0 )THEN
 WRITE(Unit_resid_,"(a,i6.3,a)")"#QDF01 RESIDUALS (iter=",iter,")"
 WRITE(Unit_resid_,"(a)")Header
 DO n=1,NQDF
  WRITE(Unit_resid_,"(a,2x,e16.7,2x,e16.7,'%')")Label(n),res(n),relres(n)*100._KIND_QDF
 END DO
 DEALLOCATE(Label)
 IF( Interactive_ .AND.Unit_resid_==window_unit )THEN
  CALL Pause(s="Press <ENTER> to continue...")
 END IF
 WRITE(Unit_resid_,"(/)")
END IF

!!--end--
END SUBROUTINE


SUBROUTINE RECALL_QDF01( x , ScalarFluxC , ScalarFluxF , CurrentF )
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
 CALL PUT_QDF01(g=g)

 DO i=1,Ni0
  !put the cell index
  CALL PUT_QDF01(i=i)

  !cell flux
  n = COL_QDF01_Cell(QDF_Flux,QDF_Center)
  ScalarFluxC(g,i) = x(n)
 END DO

 DO j=1,Nj0
  !put the face index
  CALL PUT_QDF01(j=j)

  !face flux
  n = COL_QDF01_Face(QDF_Flux)
  ScalarFluxF(g,j) = x(n)

  !face current
  n = COL_QDF01_Face(QDF_Current)
  CurrentF(g,j) = x(n)
 END DO
END DO

!!--end--
END SUBROUTINE


SUBROUTINE UPDATE_QDF01( A , b , Mesh , mact , macs , l_ , &
 ExtSource , ECxx , ECyy , ECxy , Exx , Eyy , Exy , Jbdry , Jdomn , C , &
 ScalarFluxIN , CurrentIN , BC , LOBC , FixBdryVals , FdBk , Noisy,Unit,Interactive)
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
INTEGER             ,POINTER     :: LOBC(:)
INTEGER             ,INTENT(IN) :: l_(:)
REAL(KIND_QDF)      ,INTENT(IN) :: FixBdryVals(:)
REAL(KIND_ExtSource),INTENT(IN) :: Extsource(:,:)

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback <FdBk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### OPTIONAL INPUT
LOGICAL,OPTIONAL,INTENT(IN) :: Noisy
INTEGER,OPTIONAL,INTENT(IN) :: Unit
LOGICAL,OPTIONAL,INTENT(IN) :: Interactive

!!#### LOCAL VARIABLES
INTEGER :: i,g,n,jb

!!--begin--
CALL UPDATE(fdbk_comment,fdbk,s="[[QDF]] forming the sparse coefficient &
  &matrix <A> and right side <b> for the QDF01 system variant.")

!! initialize RHS and row number
b     = Error(b)
row   = 0
save_row = 0

!! Print header for noisy output.
IF( DEFAULT(.FALSE.,Noisy) )THEN
 WRITE(DEFAULT(window_unit,Unit),"(//,a)")"#QDF01 SETUP"
END IF

!loop through the cell indices setting 5 equations per cell
DO g=1,Ng0

  DO n=1,5
 DO i=1,Ni0

  !0. setup interior locals
  CALL SETUP_QDF01_INTERIOR(g,i,Mesh,mact,macs,l_,ExtSource,&
   ECxx , ECyy , ECxy , Exx , Eyy , Exy , Noisy,Unit,Interactive)

  !1. for each of 5 equations
   CALL UPDATE_QDF01_INTERIOR(n,A,b)


 END DO
  END DO

 !6. set the boundary conditions equations
 DO jb=1,Njb0

  !! 6.a setup boundary locals
  CALL SETUP_QDF01_BOUNDARY(g,jb,Mesh,Jbdry,Jdomn, C , &
    ScalarFluxIN , CurrentIN , BC , LOBC , FixBdryVals , Noisy,Unit,Interactive)

  !! 6.b Get the boundary conditions.
  CALL UPDATE_QDF01_BC(A,b)

 END DO
END DO

!! Reallocate A to fit just right.
CALL REALLOCATE_Matrix(A)

!7. print completion information.
CALL UPDATE(fdbk_comment,fdbk,s="[[QDF]] equation/unknown count for QDF01 system: generated <"&
  //TRIM(STR(COUNT(b/=ERROR(b))))//"> equations to solve for <"//TRIM(STR(SIZE(b)))//&
  "> unknowns." )

CALL DUMP(fdbk)


!!--end--
END SUBROUTINE


SUBROUTINE SETUP_QDF01_BOUNDARY(g,jb,Mesh,Jbdry,Jdomn, C , &
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
LOGICAL,PARAMETER :: DEFAULT_Noisy=.FALSE.
INTEGER,PARAMETER :: DEFAULT_Unit=window_unit
LOGICAL,PARAMETER :: DEFAULT_Interactive=.FALSE.

!!#### LOCAL VARIABLES
LOGICAL :: Noisy_
INTEGER :: Unit_
LOGICAL :: Interactive_

!!--begin--
!handle optionals
Noisy_       = DEFAULT(DEFAULT_Noisy,Noisy)
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

!high order boundary conditions
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
 WRITE(Unit_,"(a)")"   * high order boundary condition=",KEY_BC(BC0)
 WRITE(Unit_,"(a)")"   * low  order boundary condition=",QDF_KEY_LOBC(LOBC0)
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


SUBROUTINE UPDATE_QDF01_INTERIOR(n,A,b)
!!#### PURPOSE
!! Determine the balance equation values into <coeff> and
!! the corresponding column indices in <cols>.

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: n

!!#### REQUIRED INPUT/OUTPUT
TYPE(MSR)    ,INTENT(INOUT) :: A
REAL(KIND_QDF),INTENT(INOUT) :: b(:)

!!--begin--
!get row
row    =   ROW_QDF01_Cell(n)
!get columns
cols   =  COLS_QDF01_Cell(n)
!get coefficients
coeff  = COEFF_QDF01_Cell(n)

!set values in sparse matrix
CALL SetRow (A, row, cols , coeff )
!set right side
b(row) =   RHS_QDF01_Cell(n)

!!--end--
END SUBROUTINE


SUBROUTINE UPDATE_QDF01_BC(A,b)
!!#### PURPOSE
!! For the <QDF_System01> method, include the boundary conditions.

!!#### REQUIRED INPUT/OUTPUT
TYPE(MSR)     ,INTENT(INOUT) :: A
REAL(KIND_QDF),INTENT(INOUT) :: b(:)

!!#### LOCAL VARIABLES
INTEGER :: N

!!--begin--
!! get row
row    =   ROW_QDF01_BoundaryFace(BC0)
![debug]write(*,*)"row=",row
!! get cols
cols   =  COLS_QDF01_BoundaryFace(BC0,LOBC0)
![debug]write(*,*)"cols=",cols
!! get coeff
coeff  = COEFF_QDF01_BoundaryFace(BC0,LOBC0)
![debug]write(*,*)"coeff=",coeff
!get right side
b(row) =   RHS_QDF01_BoundaryFace(BC0,LOBC0)
![debug]write(*,*)"b(row)=",b(row)

!get number of columns actually used.
N  = SIZEa(cols)
![debug]write(*,*)"N=",N
!include in sparse matrix
CALL SetRow (A, row, cols(1:N) , coeff(1:N) )

!!--end--
END SUBROUTINE





SUBROUTINE RESIDUAL_QDF01_INTERIOR(n,res,relres,x,label)
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
INTEGER       :: m
REAL(KIND_QDF) :: rhs,avgterm

!!--begin--
!get row
row    =   ROW_QDF01_Cell(n)
!get columns
cols   =  COLS_QDF01_Cell(n)
!get coefficients
coeff  = COEFF_QDF01_Cell(n)
!! get right side
rhs    =   RHS_QDF01_Cell(n)

!! multiply coefficients by solution subvector
FORALL(m=1:5)coeff(m)=coeff(m)*x(cols(m))

!! subtract right side to get residual
res(row) = SUM(coeff(1:5)) - rhs

!! calculate average term
avgterm = (SUM(ABS(coeff))+ABS(rhs))/5._KIND_QDF

!! divide to get relative residual
relres(row) = res(row)/avgterm

IF( PRESENT(label) )THEN
 WRITE(label(row),"(i10.7,2x,i3,2x,i8.5,2x,a17,2x,a5,2x,a5,2x,a10)")&
   row,g0,i0,LABEL_Equation(n)," N/A "," N/A "," N/A "
END IF
!!--end--
END SUBROUTINE


SUBROUTINE RESIDUAL_QDF01_BC(res,relres,x,label)
!!#### PURPOSE
!! For the <QDF_System01> method,
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
row    =   ROW_QDF01_BoundaryFace(BC0)
!! get cols
cols   =  COLS_QDF01_BoundaryFace(BC0,LOBC0)
!! get coeff
coeff  = COEFF_QDF01_BoundaryFace(BC0,LOBC0)
!get right side
rhs    =   RHS_QDF01_BoundaryFace(BC0,LOBC0)


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
 WRITE(label(row),"(i10.7,2x,i3,2x,a5,2x,a17,2x,i8.5,2x,i8.5,2x,a10)")row,g0," N/A ",LABEL_Equation(6),j0,jd0,KEY_BC(BC0)
END IF

!!--end--
END SUBROUTINE


FUNCTION RHS_QDF01_Cell(spec2) RESULT(RHS)
!!#### PURPOSE
!! Return cell-based right sides of equations.

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: spec2

!!#### REQUIRED OUTPUT
REAL(KIND_QDF) :: RHS

!!--begin--
SELECT CASE(spec2)
 CASE(QDF_Moment0Center) ; RHS = ExtSource0*Vi*c_4_times_PI
 CASE(QDF_Moment1Left  ) ; RHS = 0._KIND_QDF
 CASE(QDF_Moment1Right ) ; RHS = 0._KIND_QDF
 CASE(QDF_Moment1Top   ) ; RHS = 0._KIND_QDF
 CASE(QDF_Moment1Bottom) ; RHS = 0._KIND_QDF
 CASE DEFAULT            ; RHS = ERROR(RHS)
END SELECT

!!--end--
END FUNCTION


FUNCTION RHS_QDF01_BoundaryFace(BC0,LOBC0) RESULT(RHS)
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
 CASE(QDF_functionMixed) ; RHS = CurrentIN0 - 0.5_KIND_QDF*ScalarFluxIN0

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


FUNCTION COEFF_QDF01_BoundaryFace(BC0,LOBC0) RESULT(Coeff)
!!#### PURPOSE
!! Return coefficients for boundary faces.

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: BC0,LOBC0

!!#### REQUIRED OUTPUT
REAL(KIND_QDF) :: Coeff(1:5)

!!--begin--

Coeff = Error(Coeff)


SELECT CASE(LOBC0)

 !get Coeff from special low order values
 CASE(QDF_fixedJ       ) ; Coeff(1) = 1._KIND_QDF
 CASE(QDF_fixedPhi     ) ; Coeff(1) = 1._KIND_QDF
 CASE(QDF_functionPhi  ) ; Coeff(1) = 1._KIND_QDF
 CASE(QDF_functionJ    ) ; Coeff(1) = 1._KIND_QDF
 CASE(QDF_functionMixed) ; Coeff(1) = 1._KIND_QDF
                           Coeff(2) = -0.5_KIND_QDF

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


FUNCTION COLS_QDF01_BoundaryFace(BC0,LOBC0) RESULT(COLS)
!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: BC0,LOBC0

!!#### REQUIRED OUTPUT
INTEGER :: COLS(1:5)

!!--begin--

cols = ERROR(cols)

SELECT CASE( LOBC0 )

 !get Cols from special low order values
 CASE(QDF_fixedJ)      ; cols(1) = COL_QDF01_Face(QDF_Current)
 CASE(QDF_fixedPhi)    ; cols(1) = COL_QDF01_Face(QDF_Flux)
 CASE(QDF_functionJ  ) ; cols(1) = COL_QDF01_Face(QDF_Current)
 CASE(QDF_functionPhi) ; cols(1) = COL_QDF01_Face(QDF_Flux)

 CASE(QDF_functionMixed) ; cols(1)=COL_QDF01_Face(QDF_Current)
                           cols(2)=COL_QDF01_Face(QDF_Flux)

 !normal handling
 CASE DEFAULT

   IF( IsReflective(BC0) )THEN
    cols(1) = COL_QDF01_Face(QDF_Current)
   ELSE
    cols(1) = COL_QDF01_Face(QDF_Current)
    cols(2) = COL_QDF01_Face(QDF_Flux   )
   END IF
END SELECT

!!--end--
END FUNCTION


FUNCTION COLS_QDF01_Cell( spec3 ) RESULT(cols)
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
!!   assuming <QDF_System01> method's ordering of unknowns
INTEGER :: cols(1:5)

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_ = "COLS_QDF01_Cell"

!!#### LOCAL VARIABLES
TYPE(varying_string) :: VS

!!--begin--
!initialize
cols = ERROR(cols)

!set column indices
SELECT CASE( spec3 )
  CASE(QDF_Moment0Center)
    cols(1) = COL_QDF01_Cell(QDF_Current,QDF_Bottom)
    cols(2) = COL_QDF01_Cell(QDF_Current,QDF_Right )
    cols(3) = COL_QDF01_Cell(QDF_Current,QDF_Top   )
    cols(4) = COL_QDF01_Cell(QDF_Current,QDF_Left  )
    cols(5) = COL_QDF01_Cell(QDF_Flux   ,QDF_Center)

  CASE(QDF_Moment1Bottom)
    cols(1) = COL_QDF01_Cell(QDF_Flux   ,QDF_Bottom)
    cols(2) = COL_QDF01_Cell(QDF_Flux   ,QDF_Right )
    cols(3) = COL_QDF01_Cell(QDF_Flux   ,QDF_Center)
    cols(4) = COL_QDF01_Cell(QDF_Flux   ,QDF_Left  )
    cols(5) = COL_QDF01_Cell(QDF_Current,QDF_Bottom)

  CASE(QDF_Moment1Right )
    cols(1) = COL_QDF01_Cell(QDF_Flux   ,QDF_Bottom)
    cols(2) = COL_QDF01_Cell(QDF_Flux   ,QDF_Right )
    cols(3) = COL_QDF01_Cell(QDF_Flux   ,QDF_Top   )
    cols(4) = COL_QDF01_Cell(QDF_Flux   ,QDF_Center)
    cols(5) = COL_QDF01_Cell(QDF_Current,QDF_Right )

  CASE(QDF_Moment1Top   )
    cols(1) = COL_QDF01_Cell(QDF_Flux   ,QDF_Center)
    cols(2) = COL_QDF01_Cell(QDF_Flux   ,QDF_Right )
    cols(3) = COL_QDF01_Cell(QDF_Flux   ,QDF_Top   )
    cols(4) = COL_QDF01_Cell(QDF_Flux   ,QDF_Left  )
    cols(5) = COL_QDF01_Cell(QDF_Current,QDF_Top   )

  CASE(QDF_Moment1Left  )
    cols(1) = COL_QDF01_Cell(QDF_Flux   ,QDF_Bottom)
    cols(2) = COL_QDF01_Cell(QDF_Flux   ,QDF_Center)
    cols(3) = COL_QDF01_Cell(QDF_Flux   ,QDF_Top   )
    cols(4) = COL_QDF01_Cell(QDF_Flux   ,QDF_Left  )
    cols(5) = COL_QDF01_Cell(QDF_Current,QDF_Left  )

  CASE DEFAULT
    VS = MODPROC(mod_,proc_)//"this equation specification does not exist."
    CALL Stop(s=STR(VS))
    VS = ""

END SELECT

!shift all for energy groups
cols = cols + (g0-1)*offsets(3)

!!--end--
END FUNCTION



FUNCTION COEFF_QDF01_Cell( spec3 ) RESULT(coeff)
!!#### PURPOSE
!! Determines the coefficients for a specific equation of a cell.

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
REAL(KIND_QDF) :: coeff(1:5)

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_ = "COEFF_QDF01_Cell"

!!#### LOCAL VARIABLES
TYPE(varying_string) :: VS

!!--begin--
!set column indices
SELECT CASE( spec3 )
  CASE(QDF_Moment0Center)
    coeff(1) = hB*SIGN(1,jB)
    coeff(2) = hR*SIGN(1,jR)
    coeff(3) = hT*SIGN(1,jT)
    coeff(4) = hL*SIGN(1,jL)
    coeff(5) = (mact0 - macs0)*Vi

  CASE(QDF_Moment1Bottom)
    coeff(1) =  (1.0_KIND_QDF*lB(2)*( nB(1)*EBxx0 + nB(2)*EBxy0 ) - &
                 1.0_KIND_QDF*lB(1)*( nB(1)*EBxy0 + nB(2)*EByy0 )   )
    coeff(2) =  (0.5_KIND_QDF*lR(2)*( nB(1)*ERxx0 + nB(2)*ERxy0 ) - &
                 0.5_KIND_QDF*lR(1)*( nB(1)*ERxy0 + nB(2)*ERyy0 )   )
    coeff(3) =  (1.0_KIND_QDF*lh(2)*( nB(1)*ECxx0 + nB(2)*ECxy0 ) - &
                 1.0_KIND_QDF*lh(1)*( nB(1)*ECxy0 + nB(2)*ECyy0 )   )
    coeff(4) =  (0.5_KIND_QDF*lL(2)*( nB(1)*ELxx0 + nB(2)*ELxy0 ) - &
                 0.5_KIND_QDF*lL(1)*( nB(1)*ELxy0 + nB(2)*ELyy0 )   )
    coeff(5) =  mact0*VB*SIGN(1,jB)


  CASE(QDF_Moment1Right )
    coeff(1) =  (0.5_KIND_QDF*lB(2)*( nR(1)*EBxx0 + nR(2)*EBxy0 ) - &
                 0.5_KIND_QDF*lB(1)*( nR(1)*EBxy0 + nR(2)*EByy0 )   )
    coeff(2) =  (1.0_KIND_QDF*lR(2)*( nR(1)*ERxx0 + nR(2)*ERxy0 ) - &
                 1.0_KIND_QDF*lR(1)*( nR(1)*ERxy0 + nR(2)*ERyy0 )   )
    coeff(3) =  (0.5_KIND_QDF*lT(2)*( nR(1)*ETxx0 + nR(2)*ETxy0 ) - &
                 0.5_KIND_QDF*lT(1)*( nR(1)*ETxy0 + nR(2)*ETyy0 )   )
    coeff(4) = -(1.0_KIND_QDF*lv(2)*( nR(1)*ECxx0 + nR(2)*ECxy0 ) - &
                 1.0_KIND_QDF*lv(1)*( nR(1)*ECxy0 + nR(2)*ECyy0 )   )
    coeff(5) = mact0*VR*SIGN(1,jR)


  CASE(QDF_Moment1Top   )
    coeff(1) = -(1.0_KIND_QDF*lh(2)*( nT(1)*ECxx0 + nT(2)*ECxy0 ) - &
                 1.0_KIND_QDF*lh(1)*( nT(1)*ECxy0 + nT(2)*ECyy0 )   )
    coeff(2) =  (0.5_KIND_QDF*lR(2)*( nT(1)*ERxx0 + nT(2)*ERxy0 ) - &
                 0.5_KIND_QDF*lR(1)*( nT(1)*ERxy0 + nT(2)*ERyy0 )   )
    coeff(3) =  (1.0_KIND_QDF*lT(2)*( nT(1)*ETxx0 + nT(2)*ETxy0 ) - &
                 1.0_KIND_QDF*lT(1)*( nT(1)*ETxy0 + nT(2)*ETyy0 )   )
    coeff(4) =  (0.5_KIND_QDF*lL(2)*( nT(1)*ELxx0 + nT(2)*ELxy0 ) - &
                 0.5_KIND_QDF*lL(1)*( nT(1)*ELxy0 + nT(2)*ELyy0 )   )
    coeff(5) =  mact0*VT*SIGN(1,jT)


  CASE(QDF_Moment1Left  )
    coeff(1) =  (0.5_KIND_QDF*lB(2)*( nL(1)*EBxx0 + nL(2)*EBxy0 ) - &
                 0.5_KIND_QDF*lB(1)*( nL(1)*EBxy0 + nL(2)*EByy0 )   )
    coeff(2) =  (1.0_KIND_QDF*lv(2)*( nL(1)*ECxx0 + nL(2)*ECxy0 ) - &
                 1.0_KIND_QDF*lv(1)*( nL(1)*ECxy0 + nL(2)*ECyy0 )   )
    coeff(3) =  (0.5_KIND_QDF*lT(2)*( nL(1)*ETxx0 + nL(2)*ETxy0 ) - &
                 0.5_KIND_QDF*lT(1)*( nL(1)*ETxy0 + nL(2)*ETyy0 )   )
    coeff(4) =  (1.0_KIND_QDF*lL(2)*( nL(1)*ELxx0 + nL(2)*ELxy0 ) - &
                 1.0_KIND_QDF*lL(1)*( nL(1)*ELxy0 + nL(2)*ELyy0 )   )
    coeff(5) =  mact0*VL*SIGN(1,jL)


  CASE DEFAULT
    VS = MODPROC(mod_,proc_)//"this equation specification does not exist."
    CALL Stop(s=STR(VS))
    VS = ""

END SELECT

IF( ANY(coeff/=coeff) )THEN
 WRITE(*,*)"Something is NaN."
END IF
!!--end--
END FUNCTION


FUNCTION COL_QDF01_Cell( spec1 , spec2 ) RESULT(col)
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
!!   assuming <QDF_System01> method's ordering of unknowns
INTEGER :: col

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_ = "COL_QDF01_Cell"

!!#### LOCAL VARIABLES
TYPE(varying_string) :: VS

!!--begin--
SELECT CASE( spec1 )

  CASE(QDF_Flux)
   SELECT CASE(spec2)
    CASE(QDF_Center) ; col = i0
    CASE(QDF_Bottom) ; col = abs(jB)+offsets(1)
    CASE(QDF_Right ) ; col = abs(jR)+offsets(1)
    CASE(QDF_Left  ) ; col = abs(jL)+offsets(1)
    CASE(QDF_Top   ) ; col = abs(jT)+offsets(1)
    CASE DEFAULT
      VS = MODPROC(mod_,proc_)//&
      "this flux specification does not exist."
      CALL Stop(s=STR(VS))
      VS = ""

   END SELECT

  CASE(QDF_Current)
   SELECT CASE(spec2)
    CASE(QDF_Bottom) ; col = abs(jB)+offsets(2)
    CASE(QDF_Right ) ; col = abs(jR)+offsets(2)
    CASE(QDF_Left  ) ; col = abs(jL)+offsets(2)
    CASE(QDF_Top   ) ; col = abs(jT)+offsets(2)
    CASE DEFAULT
      VS = MODPROC(mod_,proc_)//&
      "this current specification does not exist."
      CALL Stop(s=STR(VS))
      VS = ""

   END SELECT

  CASE DEFAULT
    VS = MODPROC(mod_,proc_)//&
      "this combination of unknown specifications does not exist."
    CALL Stop(s=STR(VS))
    VS = ""

END SELECT

!shift all for energy groups
col = col + (g0-1)*offsets(3)

!! Apply reordering.
col = col_reorder(col)

!!--end--
END FUNCTION


FUNCTION COL_QDF01_Face( spec1 ) RESULT(col)
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
!!   <QDF_System01> method's ordering of unknowns
INTEGER :: col

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_ = "COL_QDF01_Face"

!!#### LOCAL VARIABLES
TYPE(varying_string) :: VS

!!--begin--
SELECT CASE( spec1 )
  CASE( QDF_Flux   ) ; col = ABS(j0)+offsets(1)
  CASE( QDF_Current) ; col = ABS(j0)+offsets(2)

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


FUNCTION ROW_QDF01_Cell( spec3 ) RESULT(row)
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
!!   assuming <QDF_System01> method's ordering of unknowns
INTEGER :: row

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_ = "ROW_QDF01_Cell"

!!#### LOCAL VARIABLES
TYPE(varying_string) :: VS

!!--begin--
IF( RowAsTheyCome )THEN
 save_row = save_row + 1
 row = save_row
ELSE

 SELECT CASE( spec3 )
   CASE(QDF_Moment0Center) ;           row = COL_QDF01_Cell(QDF_Flux,QDF_Center) ! i0
   CASE(QDF_Moment1Bottom) ; j0 = jB ; row = COL_QDF01_Face(MERGE(QDF_Current,QDF_Flux,jB<0)) ! MERGE( abs(jB)+offsets(2) , jB+offsets(1) , jB<0 )
   CASE(QDF_Moment1Right ) ; j0 = jR ; row = COL_QDF01_Face(MERGE(QDF_Current,QDF_Flux,jR<0)) ! MERGE( abs(jR)+offsets(2) , jR+offsets(1) , jR<0 )
   CASE(QDF_Moment1Top   ) ; j0 = jT ; row = COL_QDF01_Face(MERGE(QDF_Current,QDF_Flux,jT<0)) ! MERGE( abs(jT)+offsets(2) , jT+offsets(1) , jT<0 )
   CASE(QDF_Moment1Left  ) ; j0 = jL ; row = COL_QDF01_Face(MERGE(QDF_Current,QDF_Flux,jL<0)) ! MERGE( abs(jL)+offsets(2) , jL+offsets(1) , jL<0 )
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

!!--end--
END FUNCTION


FUNCTION ROW_QDF01_BoundaryFace(BC0) RESULT(ROW)
!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: BC0

!!#### REQUIRED OUTPUT
INTEGER :: ROW

!!--begin--
IF( RowAsTheyCome )THEN
 save_row = save_row + 1
 row = save_row
ELSE

 row = COL_QDF01_Face(QDF_Current) !ABS(j0) + offsets(2)
 row = row + (g0-1)*offsets(3)

 !! Apply reordering.
 row = row_reorder(row)

END IF
!!--end--
END FUNCTION


END MODULE
