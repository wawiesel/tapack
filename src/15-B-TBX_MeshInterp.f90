!!# TOOLBOX MODULE >>TBX_MeshInterp<<
MODULE TBX_MeshInterp

!!## PURPOSE
!! Provide interpolation routines on a <Mesh>.

!!## MODULES
USE ISO_varying_string
USE USR_Mesh
USE USR_ExplicitCell
USE FUN_NewFile
USE PRN_Table
USE FUN_STR
USE SUB_CLEAR

!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## DEFAULT ACCESS
PRIVATE

!!## INTERPOLATION PARAMETERS
CHARACTER(*),PARAMETER :: KEYS_FType(1:4)=(/"Vert","Edge","Face","Cell"/)
CHARACTER(*),PARAMETER :: KEYS_InterpType(1:4) = &
  (/"Const     ",&
    "Nearest   ",&
    "Bivar     ",&
    "CShep     "/)


!!## LOCAL VARIABLES
REAL(KIND_MSH),POINTER :: F_(:,:)=>NULL()
INTEGER :: g_=0
TYPE(TYPE_Mesh) :: Mesh_

!!## PUBLIC ACCESS LIST
!! * interpolation of 2D scattered data
PUBLIC :: InterpGrid2S_ConstCell
PUBLIC :: SETUP_InterpGrid2S_ConstCell
PUBLIC :: EVAL_InterpGrid2S_ConstCell
PUBLIC :: KEYS_FType
PUBLIC :: KEYS_InterpType

!!## CONTAINED PROCEDURES
CONTAINS


FUNCTION EVAL_InterpGrid2S_ConstCell( x , y )  RESULT(Fout)

!#REQUIRED INPUT
REAL(KIND_MSH) ,INTENT(IN) :: x
REAL(KIND_MSH) ,INTENT(IN) :: y

!!#### REQUIRED OUTPUT
REAL(KIND_MSH) :: Fout

!!#### LOCAL VARIABLES
INTEGER :: i
REAL(KIND_MSH) :: p2(1:2)
TYPE(TYPE_ExplicitCell) :: Xc
!!--begin--
!init
DO i=1,NUM_Cells_Mesh(Mesh_)
 Xc = ExplicitCell_Mesh(Mesh_,i)
 p2 = (/x,y/)
 IF( INTERIOR_XcP(Xc,P2,Mesh_%Cells(i)%CellCentroid) )THEN
  Fout = F_(g_,i)
 END IF
 CALL CLEAN( Xc )
END DO
!!--end--
END FUNCTION


SUBROUTINE SETUP_InterpGrid2S_ConstCell( F , Mesh , g )

!#REQUIRED INPUT
REAL(KIND_MSH) ,POINTER    :: F(:,:)
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER       ,INTENT(IN) :: g

!!--begin--
!save F
IF( ASSOCIATED( F_ ) )NULLIFY( F_ )
F_ => F

!save g
g_ = g

!save whole mesh (UGH!) (all pointer references though)
Mesh_ = Mesh

!!--end--
END SUBROUTINE



FUNCTION InterpGrid2S_ConstCell( F , Mesh , xout , yout , x_ , y_ , errmsg , &
  errint )  RESULT(Fout)
!#REQUIRED INPUT
REAL(KIND_MSH) ,INTENT(IN) :: F(:)
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
REAL(KIND_MSH) ,POINTER :: xout(:)
REAL(KIND_MSH) ,POINTER :: yout(:)
REAL(KIND_MSH) ,POINTER :: x_(:)
REAL(KIND_MSH) ,POINTER :: y_(:)
!!#### REQUIRED OUTPUT
REAL(KIND_MSH) :: Fout(1:2*SIZE(xout)-2,1:2*SIZE(yout)-2)

!!#### OPTIONAL OUTPUT
CHARACTER(*),OPTIONAL,INTENT(OUT) :: errmsg
INTEGER     ,OPTIONAL,INTENT(OUT) :: errint

!!#### LOCAL VARIABLES
INTEGER :: ix,iy,i,Unit,Nx,Ny,Nx_,Ny_
LOGICAL :: ResetSmap
REAL(KIND_MSH) :: p2(1:2)
TYPE(TYPE_ExplicitCell) :: Xc
TYPE(varying_string) :: FMT_smap
INTEGER :: digs
INTEGER,ALLOCATABLE,SAVE :: Smap(:,:)
!!--begin--
!init
Nx = SIZE(xout)
Ny = SIZE(yout)
Fout = 0._KIND_MSH

!! skip if we already have stuff saved
ResetSmap = .TRUE.
!IF( ASSOCIATED(xout_,xout) .AND. ASSOCIATED(yout_,yout) )THEN
! ResetSmap = .FALSE.
!END IF

!! Reset the mapping of unstructured cells to structured.
IF( ResetSmap )THEN

 CALL CONSTRUCT_LOCAL_LINES(x_,xout)
 CALL CONSTRUCT_LOCAL_LINES(y_,yout)

 Nx_ = SIZE(x_)
 Ny_ = SIZE(y_)

 IF( ALLOCATED(Smap) )DEALLOCATE( Smap )
 ALLOCATE( Smap(Nx_,Ny_) )
 Smap = 0

 DO i=1,NUM_Cells_Mesh(Mesh)
  Xc = ExplicitCell_Mesh(Mesh,i)
  DO iy=1,Ny_
   DO ix=1,Nx_
    p2 = (/x_(ix),y_(iy)/)
    IF( INTERIOR_XcP(Xc,P2,Mesh%Cells(i)%CellCentroid) )THEN
     Smap(ix,iy) = i
    END IF
   END DO
  END DO
  CALL CLEAN( Xc )
 END DO

 Unit = NewFile("Smap.dat",STATUS="Replace")
 digs = MAXVAL(LOG10(REAL(Smap))+1)
 FMT_Smap = "(i"//TRIM(STR(digs))//"."//TRIM(STR(digs))//")"

 CALL PRINT_Table( STR(TRANSPOSE(Smap),STR(FMT_Smap)) , unit=Unit ,delim=" ",&
  PrintSeparator=(/.FALSE.,.FALSE.,.FALSE./))
 CLOSE(Unit)

END IF

!! Set interpolated stuff.
DO iy=1,Ny_
 DO ix=1,Nx_
  i = Smap(ix,iy)
  Fout(ix,iy) = F(i)
 END DO
END DO

Unit = NewFile("Fout.dat",STATUS="Replace")
CALL PRINT_Table( STR(TRANSPOSE(Fout)) , unit=Unit ,delim=" ",&
  PrintSeparator=(/.FALSE.,.FALSE.,.FALSE./))
CLOSE(Unit)

!! Impossible to have error with this algorithm so just
!! always return clear and 0.
IF( PRESENT(errmsg) )THEN
 CALL CLEAR(errmsg)
END IF
IF( PRESENT(errint) )THEN
 errint = 0
END IF

!set pointers for later uses
!xout_ => xout
!yout_ => yout

!!--end--
END FUNCTION


SUBROUTINE CONSTRUCT_LOCAL_LINES(x_,x)
REAL(KIND_MSH),INTENT(IN) :: x(:)
REAL(KIND_MSH),POINTER :: x_(:)
INTEGER :: Nx,Nx_
INTEGER :: ix,ix_
REAL(KIND_MSH) :: dx
!!--begin--
Nx = SIZE(x)
Nx_ = 2*SIZE(x)-2

IF( ASSOCIATED(x_) )THEN
 DEALLOCATE(x_)
 NULLIFY( x_ )
END IF

!construct local lines
ALLOCATE( x_(1:Nx_) )
ix_ = 0

!special first line only has after
ix = 1
ix_ = ix_ + 1
dx = x(2) - x(1)
x_(ix_) = x(ix)+dx/100._KIND_MSH

DO ix=2,Nx-1

 !line before
 ix_ = ix_ + 1
 dx = x(ix)-x(ix-1)
 x_(ix_) = x(ix)-dx/100._KIND_MSH

 !line after
 ix_ = ix_ + 1
 dx = x(ix+1)-x(ix)
 x_(ix_) = x(ix)+dx/100._KIND_MSH

END DO

!special last line only has before
ix_ = ix_ + 1
dx = x(Nx) - x(Nx-1)
x_(ix_) = x(ix)-dx/100._KIND_MSH

!!--end--
END SUBROUTINE



END MODULE
