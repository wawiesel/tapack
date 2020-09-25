!!# MODULE: <<PRN_Mesh>>
MODULE PRN_Mesh

!!## PURPOSE
!! Provides printing routines for Meshes.

!!## GLOBAL TOOLBOXES
USE USR_Mesh                 !!((14-B-USR_Mesh.f90))
USE TBX_Mesh                 !!((15-B-TBX_Mesh.f90))
USE FUN_InterpGrid2S_Nearest !!((07-B-FUN_InterpGrid2S_Nearest.f90))
USE FUN_InterpGrid2S_BiVar   !!((06-B-FUN_InterpGrid2S_BiVar.f90))
USE FUN_InterpGrid2S_CShep   !!((06-B-FUN_InterpGrid2S_CShep.f90))
USE FUN_INDEXa               !!((08-B-FUN_INDEXa.f90))
USE TBX_MeshInterp           !!((15-B-TBX_MeshInterp.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE
PRIVATE

!!## INTERFACES
INTERFACE PRINT_Grid
 MODULE PROCEDURE PRINT_Grid_Mesh
END INTERFACE

!!## DEFAULT ACCESS
PUBLIC :: PRINT_Grid
PUBLIC :: PRINT_Grid_Mesh
PUBLIC :: gmvInitialize,gmvFinalize
PUBLIC :: gmvSet_CellData
PUBLIC :: gmvSet_NodeData
PUBLIC :: gmvSet_TracerCellFunctionData
PUBLIC :: gmvEnd_Tracers
PUBLIC :: gmvBegin_Tracers
PUBLIC :: gmvEnd_Variables
PUBLIC :: gmvBegin_Variables
PUBLIC :: gmvSet_CellFUnctionData

CHARACTER(*),PARAMETER :: KEYS_PlotType(1:4) = &
  (/"Surface      ",&
    "Color        ",&
    "Contour      ",&
    "FilledContour" /)


CONTAINS


SUBROUTINE Print_GRID_Mesh(Mesh,Unit,f,r,FType,InterpType)
!!#### PURPOSE
!! Output the stuff on a grid.

!!#### DEPENDENCIES
USE PRN_Grid2                !!((09-B-PRN_Grid2.f90))

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER       ,INTENT(IN) :: Unit
REAL(KIND_MSH) ,INTENT(IN) :: f(:,:)
REAL(KIND_MSH) ,INTENT(IN) :: r(:,:)

!!#### OPTIONAL INPUT
!! * the function type
CHARACTER(*),INTENT(IN),OPTIONAL :: FType
!! * the interpolation type desired
CHARACTER(*),INTENT(IN),OPTIONAL :: InterpType

!!#### LOCAL VARIABLES
REAL(KIND_MSH),POINTER :: x(:),y(:)
REAL(KIND_MSH),POINTER :: x_(:),y_(:)
REAL(KIND_MSH),POINTER :: f2(:,:)
REAL(KIND_MSH),POINTER :: r2(:,:)
REAL(KIND_MSH),POINTER :: Z(:,:)
INTEGER               :: g
CHARACTER(200)        :: errmsg
INTEGER               :: ftype_,interptype_,plottype_

!!--begin--
!Get specs for optional stuff (use dummy plot type)
CALL GetOptionalSpecs(ftype_,interptype_,plottype_,&
                      ftype,interptype," ")

!get x,y
CALL ModifyInterpGrid(Mesh,x,y)

!! SETUP local Grid variables.
CALL SETUP_GRID(InterpType_,f,r,f2,r2,x,y,x_,y_,z)

!! Enter loop over groups.
DO g=1,SIZE(f,1)

 !get gridded values
 SELECT CASE(InterpType_)
  CASE(1) ; Z = InterpGrid2S_ConstCell(f(g,:),Mesh,x_,y_,x,y,errmsg)
  CASE(2) ; Z = InterpGrid2S_Nearest  (f2(g,:),r2,x,y,errmsg)
  CASE(3) ; Z = InterpGrid2S_Bivar    (f2(g,:),r2,x,y,errmsg)
  CASE(4) ; Z = InterpGrid2S_CShep    (f2(g,:),r2,x,y,errmsg)
 END SELECT

 !print them
 CALL PRINT_Grid2( z , Unit , x , y )

 !print an error message if we had an error
 IF( LEN_TRIM(errmsg)/=0 )THEN
  WRITE(Unit,"(a)")"      *"//TRIM(errmsg)
 ELSE
  WRITE(Unit,"(a)")" "
 END IF

END DO

!! WRAPUP local Grid variables.
CALL WRAPUP_GRID(InterpType_,f,r,f2,r2,x,y,x_,y_,z)

!!--end--
END SUBROUTINE



SUBROUTINE GetOptionalSpecs(ftype_,interptype_,plottype_,&
                      ftype,interptype,plottype)
!!#### REQUIRED OUTPUT
INTEGER,INTENT(OUT) :: FType_,PlotType_,InterpType_

!!#### OPTIONAL INPUT
!! * the function type
CHARACTER(*),INTENT(IN),OPTIONAL :: FType
!! * the interpolation type desired
CHARACTER(*),INTENT(IN),OPTIONAL :: InterpType
!! * the plot type desired
CHARACTER(*),INTENT(IN),OPTIONAL :: PlotType

!!--begin--
!! handle local defaults
IF( PRESENT(FType) )THEN
 FType_ = INDEXa(KEYS_FType,FType)
 !(/waw/)write(*,*)ftype_,"=","INDEXa(",Sentence(KEYS_Ftype,delim=",",beg="(/",end="/)"),",",ftype,")"
ELSE
 FType_ = 0
END IF
IF( FType_==0 )FType_=4

IF( PRESENT(InterpType) )THEN
 InterpType_ = INDEXa(KEYS_InterpType,InterpType)
ELSE
 InterpType_ = 0
END IF
IF( InterpType_==0 )InterpType_=1

IF( PRESENT(PlotType) )THEN
 PlotType_ = INDEXa(KEYS_PlotType,PlotType)
ELSE
 PlotType_ = 0
END IF
IF( PlotType_==0 )PlotType_=1

!!--end--
END SUBROUTINE





SUBROUTINE gmvInitialize(Unit,Mesh,l_,LABEL_Materials)
INTEGER        ,INTENT(IN) :: Unit
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER        ,INTENT(IN) :: l_(:)
CHARACTER(*)   ,INTENT(IN) :: LABEL_Materials(:)

!!#### LOCAL VARIABLES
INTEGER :: i,j,k,l,Nl
INTEGER,POINTER :: VertList(:)

!!--begin--

WRITE(Unit,"(a)")"gmvinput ascii"

!!*** NODE SPECS
WRITE(Unit,"(a,i0)")"nodev ",NUM_Verts(Mesh)
DO k=1,NUM_Verts(Mesh)
 SELECT CASE(NUM_Dimensions(Mesh))
  CASE(1); WRITE(Unit,"(3Es26.19)")Vert(Mesh,k),0.0,0.0
  CASE(2); WRITE(Unit,"(3Es26.19)")Vert(Mesh,k),0.0
  CASE(3); WRITE(Unit,"(3Es26.19)")Vert(Mesh,k)
 END SELECT
END DO
!!****************

!!*** CELL SPECIFICATIONS
WRITE(Unit,"(a,i0)")"cells ",NUM_Cells(Mesh)
DO i=1,NUM_Cells(Mesh)
 WRITE(Unit,"(a)")"general 1"
 NULLIFY(VertList)
 CALL GET_CellVertBoundaryList(Mesh,i,VertList)

 WRITE(Unit,"(2x,i0)")SIZE(VertList)

 WRITE(Unit,"(1x)",ADVANCE="no")
 DO j=1,SIZE(VertList)
  WRITE(Unit,"(1x,i0)",ADVANCE="no")VertList(j)
 END DO
 WRITE(Unit,"(1x)",ADVANCE="yes")
 DEALLOCATE(VertList)

END DO
!!***********************


!!*** MATERIALS SPECS ***
Nl = MAXVAL(l_)
WRITE(Unit,"(a,2i0)")"material ",Nl,0 !!0 = belongs to cells?
DO l=1,Nl
 WRITE(Unit,"(1x,a)",ADVANCE="no")LABEL_Materials(l)
END DO
WRITE(Unit,"(1x)",ADVANCE="yes")

DO i=1,NUM_Cells(Mesh)
 WRITE(Unit,"(1x,i0)",ADVANCE="no")l_(i)
END DO
WRITE(Unit,"(1x)",ADVANCE="yes")
!!*************************

!!--end--
END SUBROUTINE


SUBROUTINE gmvSet_CellData(Unit,Mesh,Name,CellData)
INTEGER        ,INTENT(IN) :: Unit
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
CHARACTER(*)   ,INTENT(IN) :: Name
REAL(KIND_MSH) ,INTENT(IN) :: CellData(:)

!!#### LOCAL VARIABLES
INTEGER :: i

!!--begin--

WRITE(Unit,"(1x,a,i0)")Name,0
DO i=1,NUM_Cells(Mesh)
 IF( ABS(CellData(i))>1.d99 )THEN
  WRITE(Unit,"(1x,Es26.19)",ADVANCE="no")SIGN(1.d99,CellData(i))
 ELSE IF( ABS(CellData(i))<1.d-99 )THEN
  WRITE(Unit,"(1x,Es26.19)",ADVANCE="no")SIGN(1.d-99,CellData(i))
 ELSE
  WRITE(Unit,"(1x,Es26.19)",ADVANCE="no")CellData(i)
 END IF
END DO
WRITE(Unit,"(1x)",ADVANCE="yes")

!!--end--
END SUBROUTINE


SUBROUTINE gmvSet_CellFUnctionData(Unit,Mesh,Name,CellFunctionData)
INTEGER        ,INTENT(IN) :: Unit
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
CHARACTER(*)   ,INTENT(IN) :: Name
REAL(KIND_MSH) ,INTENT(IN) :: CellFunctionData(:,:)

!!#### LOCAL VARIABLES
INTEGER :: i,j,k,j_,k_,d
REAL(KIND_MSH) :: f1,fx,fy,r(Mesh%NDim),f0,CC(Mesh%NDim)
REAL(KIND_MSH) :: f(NUM_Verts(Mesh))

!!--begin--

!begin of the min data
f=HUGE(1.d0)
WRITE(Unit,"(1x,a,i0)")Name//"-MIN",1
 DO i=1,NUM_Cells(Mesh)
  CC = CellCentroid(Mesh,i)
  f1 = CellFunctionData(1,i)
  fx = CellFunctionData(2,i)
  fy = CellFunctionData(3,i)

  !now do faces and verts
  DO j_=1,SIZE(Mesh%Cells(i)%FaceList)
   j=ABS(Mesh%Cells(i)%FaceList(j_))
   DO k_=1,SIZE(Mesh%Faces(j)%VertList)
    k=Mesh%Faces(j)%VertList(k_)
    r=Vert(Mesh,k)
    f0 = f1 + fx*(r(1)-CC(1)) + fy*(r(2)-CC(2))
    f(k) = MIN(f0,f(k))
   END DO
  END DO

 END DO
DO k=1,NUM_Verts(Mesh)
 WRITE(Unit,"(1x,Es26.19)",ADVANCE="no")f(k)
END DO
WRITE(Unit,"(1x)",ADVANCE="yes")
!end of the min data


!begin of the min data
f=-HUGE(1.d0)
WRITE(Unit,"(1x,a,i0)")Name//"-MAX",1
 DO i=1,NUM_Cells(Mesh)
  CC = CellCentroid(Mesh,i)
  f1 = CellFunctionData(1,i)
  fx = CellFunctionData(2,i)
  fy = CellFunctionData(3,i)

  !now do faces and verts
  DO j_=1,SIZE(Mesh%Cells(i)%FaceList)
   j=ABS(Mesh%Cells(i)%FaceList(j_))
   DO k_=1,SIZE(Mesh%Faces(j)%VertList)
    k=Mesh%Faces(j)%VertList(k_)
    r=Vert(Mesh,k)
    f0 = f1 + fx*(r(1)-CC(1)) + fy*(r(2)-CC(2))
    f(k) = MAX(f0,f(k))
   END DO
  END DO

 END DO
DO k=1,NUM_Verts(Mesh)
 WRITE(Unit,"(1x,Es26.19)",ADVANCE="no")f(k)
END DO
WRITE(Unit,"(1x)",ADVANCE="yes")
!end of the max data

!!--end--
END SUBROUTINE


SUBROUTINE gmvSet_NodeData(Unit,Mesh,Name,NodeData)
INTEGER        ,INTENT(IN) :: Unit
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
CHARACTER(*)   ,INTENT(IN) :: Name
REAL(KIND_MSH) ,INTENT(IN) :: NodeData(:)

!!#### LOCAL VARIABLES
INTEGER :: k

!!--begin--

WRITE(Unit,"(1x,a,i0)")Name,1
DO k=1,NUM_Verts(Mesh)
 IF( ABS(NodeData(k))>1.d99 )THEN
  WRITE(Unit,"(1x,Es26.19)",ADVANCE="no")SIGN(1.d99,NodeData(k))
 ELSE IF( ABS(NodeData(k))<1.d-99 )THEN
  WRITE(Unit,"(1x,Es26.19)",ADVANCE="no")SIGN(1.d-99,NodeData(k))
 ELSE
  WRITE(Unit,"(1x,Es26.19)",ADVANCE="no")NodeData(k)
 END IF
END DO
WRITE(Unit,"(1x)",ADVANCE="yes")

!!--end--
END SUBROUTINE


SUBROUTINE gmvSet_TracerCellFunctionData(Unit,Mesh,Name,CellFunctionData)
INTEGER        ,INTENT(IN) :: Unit
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
CHARACTER(*)   ,INTENT(IN) :: Name
REAL(KIND_MSH) ,INTENT(IN) :: CellFunctionData(:,:)

!!#### LOCAL VARIABLES
INTEGER :: i,j,k,j_,k_,d
REAL(KIND_MSH) :: CC(Mesh%NDim),V(Mesh%NDim),FC(Mesh%NDim)
REAL(KIND_MSH) :: f1,fx,fy,r(Mesh%NDim),f

!!--begin--

!put a tracer point at each vertex inside each cell
!each face, and the cell so we can look at discontinous data
WRITE(Unit,"(a)",ADVANCE="yes")Name
 DO i=1,NUM_Cells(Mesh)
  f1 = CellFunctionData(1,i)
  fx = CellFunctionData(2,i)
  fy = CellFunctionData(3,i)

  CC = CellCentroid(Mesh,i)

  !do the cell
  r = CC
  f = f1 + fx*r(1) + fy*r(2)
  WRITE(Unit,"(1x,Es26.19)",ADVANCE="no")f

  !now do faces and verts
  DO j_=1,SIZE(Mesh%Cells(i)%FaceList)
   j=ABS(Mesh%Cells(i)%FaceList(j_))
   FC=FaceCentroid(Mesh,j)
   r = 0.99*(FC-CC)
   f = f1 + fx*r(1) + fy*r(2)
   WRITE(Unit,"(1x,Es26.19)",ADVANCE="no")f
   DO k_=1,SIZE(Mesh%Faces(j)%VertList)
    k=Mesh%Faces(j)%VertList(k_)
    V=Vert(Mesh,k)
    r = 0.99*(V-CC)
    f = f1 + fx*r(1) + fy*r(2)
    WRITE(Unit,"(1x,Es26.19)",ADVANCE="no")f
   END DO
  END DO

 END DO
 WRITE(Unit,"(1x)",ADVANCE="yes")

!!--end--
END SUBROUTINE

SUBROUTINE gmvBegin_Tracers(Unit,Mesh)
INTEGER        ,INTENT(IN) :: Unit
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh

!!#### LOCAL VARIABLES
INTEGER :: i,j,k,j_,k_,d,npts
REAL(KIND_MSH) :: CC(Mesh%NDim),V(Mesh%NDim),FC(Mesh%NDim)
REAL(KIND_MSH) :: f1,fx,fy,r(Mesh%NDim),f
!!--begin--

!first pass to get number of tracer points
npts=1
DO i=1,NUM_Cells(Mesh)
 npts=npts+1
 DO j_=1,SIZE(Mesh%Cells(i)%FaceList)
  j=ABS(Mesh%Cells(i)%FaceList(j_))
  npts=npts+1
  DO k_=1,SIZE(Mesh%Faces(j)%VertList)
   npts=npts+1
  END DO
 END DO
END DO
WRITE(Unit,"(a,i0)")"tracers",npts

!put a tracer point at each vertex inside each cell
!each face, and the cell so we can look at discontinous data
DO d=1,Mesh%NDim
 DO i=1,NUM_Cells(Mesh)
  CC = CellCentroid(Mesh,i)

  !do the cell
  WRITE(Unit,"(1x,Es26.19)",ADVANCE="no")CC(d)

  !now do faces and verts
  DO j_=1,SIZE(Mesh%Cells(i)%FaceList)
   j=ABS(Mesh%Cells(i)%FaceList(j_))
   FC=FaceCentroid(Mesh,j)
   WRITE(Unit,"(1x,Es26.19)",ADVANCE="no")CC(d)+0.99*(FC(d)-CC(d))
   DO k_=1,SIZE(Mesh%Faces(j)%VertList)
    k=Mesh%Faces(j)%VertList(k_)
    V=Vert(Mesh,k)
    WRITE(Unit,"(1x,Es26.19)",ADVANCE="no")CC(d)+0.99*(V(d)-CC(d))
   END DO
  END DO

 END DO
 WRITE(Unit,"(1x)",ADVANCE="yes")
END DO

DO d=Mesh%NDim+1,3
 DO i=1,npts
  WRITE(Unit,"(1x,Es26.19)",ADVANCE="no")0.d0
 END DO
 WRITE(Unit,"(1x)",ADVANCE="yes")
END DO

!!--end--
END SUBROUTINE

SUBROUTINE gmvEnd_Tracers(Unit,Mesh)
INTEGER        ,INTENT(IN) :: Unit
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh

!!--begin--

WRITE(Unit,"(a)")"endtrace"

!!--end--
END SUBROUTINE


SUBROUTINE gmvBegin_Variables(Unit,Mesh)
INTEGER        ,INTENT(IN) :: Unit
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER :: i
!!--begin--

!!*** VARIABLE SPECS ***
WRITE(Unit,"(a)")"variable"

!cell volumes
WRITE(Unit,"(1x,a,i0)")"CellVol",0
DO i=1,NUM_Cells(Mesh)
 WRITE(Unit,"(1x,Es26.19)",ADVANCE="no")CellVolume(Mesh,i)
END DO
WRITE(Unit,"(1x)",ADVANCE="yes")

END SUBROUTINE

SUBROUTINE gmvEnd_Variables(Unit,Mesh)
INTEGER        ,INTENT(IN) :: Unit
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh

!!--begin--

WRITE(Unit,"(a)")"endvars"

!!--end--
END SUBROUTINE

SUBROUTINE gmvFinalize(Unit,Mesh)
INTEGER        ,INTENT(IN) :: Unit
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh

!!--begin--

WRITE(Unit,"(a)")"endgmv"

!!--end--
END SUBROUTINE

END MODULE
