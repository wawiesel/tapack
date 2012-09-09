!!# PRINTING MODULE <<PRN_MoCshort>>
MODULE PRN_MoCshort

!!## PURPOSE
!! The printing of routines for the Method of Characteristics with
!! short (intra-cell) interpolation.

USE ISO_varying_string                                             !!((03-A-ISO_varying_string.f90))
USE USR_fdbk                                                       !!((08-C-USR_fdbk.f90))
USE PRN_Table                                                      !!((11-B-PRN_Table.f90))
USE FUN_STR                                                        !!((05-B-FUN_STR.f90))
USE KND_Mesh                                                       !!((05-B-KND_Mesh.f90))
USE TBX_Mesh,ONLY: &                                               !!((15-B-TBX_Mesh.f90))
  NUM_Cells,NUM_Faces,NUM_Verts,&
  CellCentroid,DomainFaceCentroid,FaceCentroid,&
  Vert,FaceNormal,SymmetryFace,&
  MeshScale,SymmetryDirection,SymmetryPoint
USE KND_ScalarFluxes                                               !!((02-A-KND_ScalarFluxes.f90))
USE FUN_Substitute                                                 !!((06-C-FUN_Substitute.f90))
USE KND_AngularFluxes                                              !!((02-A-KND_AngularFluxes.f90))
USE FUN_VSTROPTION                                                 !!((16-B-FUN_VSTROPTION.f90))
USE FUN_NewFile                                                    !!((05-B-FUN_NewFile.f90))
USE FUN_Default                                                    !!((04-A-FUN_Default.f90))
USE SUB_CLEAR                                                      !!((04-A-SUB_CLEAR.f90))
USE FUN_NewFile                                                    !!((05-B-FUN_NewFile.f90))
USE FUN_qmcfull                                                    !!((76-C-FUN_qmcfull.f90))
USE SUB_SimpSurf                                                   !!((08-C-SUB_SimpSurf.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## DEFAULT ACCESS
PRIVATE


!!## ACCESS
PUBLIC :: PRINT_InteriorCells
PUBLIC :: PRINT_IncomingDirections
PUBLIC :: PRINT_StreamingInterpolants
PUBLIC :: PRINT_SweepOrder
PUBLIC :: PRINT_CharacteristicInfo
PUBLIC :: PRINT_InterpInfo
PUBLIC :: PRINT_AFSymmetryCheck
PUBLIC :: PRINT_AFSymmetryCheckF
PUBLIC :: PRINT_Options_MCS

PUBLIC :: GNUPLOT_AngularFlux
PUBLIC :: GNUPLOT_Qfull2D

!!## MODULE PROCEDURES
CONTAINS



!!### GNUPLOT PLOT SUBROUTINE: <GNUPLOT_LO_ScalarFluxFandC>
SUBROUTINE GNUPLOT_LO_ScalarFluxFandC()

!!#### PURPOSE
!! Print out both low-order face-average and cell-average
!! scalar flux.

!!#### GLOBAL VARIABLES
USE VAR_ScalarFluxes                                               !!((04-C-VAR_ScalarFluxes.f90))
USE VAR_Mesh,ONLY: Mesh                                            !!((46-B-VAR_Mesh.f90))

!!#### LOCAL VARIABLES
INTEGER        :: i,j,Unit
REAL(KIND_MSH) :: CC(2),FC(2)

!!--begin--

Unit = NewFile("LO_SFFandC")

DO i=1,NUM_Cells(Mesh)
 CC = CellCentroid(Mesh,i)
 WRITE(Unit,"(f12.7,1x,f12.7,1x,e21.13,1x)")&
   CC(1),CC(2),ScalarFluxC(1,i)
END DO

DO j=1,NUM_Faces(Mesh)
 FC = FaceCentroid(Mesh,j)
 WRITE(Unit,"(f12.7,1x,f12.7,1x,e21.13,1x)")&
   FC(1),FC(2),ScalarFluxF(1,j)
END DO

CLOSE( Unit )

!!--end--
END SUBROUTINE



!!### GNUPLOT SUBROUTINE: <GNUPLOT_Qfull2D>
SUBROUTINE GNUPLOT_Qfull2D()

!!#### EXTERNAL VARIABLES
USE VAR_Mesh,ONLY: Mesh                                            !!((46-B-VAR_Mesh.f90))

!!#### LOCAL VARIABLES
INTEGER :: Unit
REAL(KIND_MSH) :: xseq(3),yseq(3)
REAL(KIND_MSH) :: DFC(2)

!!--begin--

!get y-extents from domain
DFC = DomainFaceCentroid(Mesh,1)
yseq(1) = DFC(2)
DFC = DomainFaceCentroid(Mesh,3)
yseq(3) = DFC(2)

!get x-extents from domain
DFC = DomainFaceCentroid(Mesh,4)
xseq(1) = DFC(1)
DFC = DomainFaceCentroid(Mesh,2)
xseq(3) = DFC(1)

!get 50 increments for each
xseq(2) = (xseq(3)-xseq(1))/50.d0
yseq(2) = (yseq(3)-yseq(1))/50.d0

!plot the simple surface
Unit = Newfile("Qfull2D")
CALL SimpSurf( Unit , Qfull2D , xseq , yseq )
CLOSE(Unit)

!!--end--
END SUBROUTINE



!!### GNUPLOT SUBROUTINE: <GNUPLOT_AngularFlux>
SUBROUTINE GNUPLOT_AngularFlux()

!!#### PURPOSE
!! Print out the angular flux by directions to files
!! <AF_m> for each direction <m>.

!!#### MODULES
USE VAR_Mesh,ONLY: Mesh                                            !!((46-B-VAR_Mesh.f90))
USE VAR_AngularFluxes                                              !!((04-C-VAR_AngularFluxes.f90))

!!#### LOCAL VARIABLES
INTEGER :: Unit,k,m,i,j
REAL(KIND_MSH) :: V(2)

!!--begin--

DO m=1,SIZE(AngularFluxV,3)
 Unit = Newfile("AFV_"//TRIM(STR(m)))
 DO k=1,NUM_Verts(Mesh)
  V = Vert(Mesh,k)
  WRITE(Unit,"(3e20.13)")V(1),V(2),AngularFluxV(1,k,m)
 END DO
 CLOSE(Unit)
 IF( ASSOCIATED(AngularFluxF) )THEN
    Unit = Newfile("AFF_"//TRIM(STR(m)))
    DO j=1,NUM_Faces(Mesh)
    V = FaceCentroid(Mesh,j)
    WRITE(Unit,"(3e20.13)")V(1),V(2),AngularFluxF(1,j,m)
    END DO
    CLOSE(Unit)
 END IF
 IF( ASSOCIATED(AngularFluxC) )THEN
    Unit = Newfile("AFC_"//TRIM(STR(m)))
    DO i=1,NUM_Cells(Mesh)
    V = CellCentroid(Mesh,i)
    WRITE(Unit,"(3e20.13)")V(1),V(2),AngularFluxC(1,i,m)
    END DO
    CLOSE(Unit)
 END IF
END DO

!!--end--
END SUBROUTINE


!!### PRINTING SUBROUTINE: <PRINT_AFSymmetryCheckF>
SUBROUTINE PRINT_AFSymmetryCheckF(Unit,Pn_sym,tol,relerr)

!!#### PURPOSE
!! Print a check of symmetry of the angular
!! fluxes of a problem, about a given line.

USE VAR_AngularFluxes                                              !!((04-C-VAR_AngularFluxes.f90))
USE VAR_DiscreteOrdinates,ONLY: Ordinates                          !!((47-B-VAR_DiscreteOrdinates.f90))
USE VAR_Mesh,ONLY: Mesh                                            !!((46-B-VAR_Mesh.f90))

!!#### REQUIRED INPUT
INTEGER       ,INTENT(IN) :: Unit
REAL(KIND_MSH),INTENT(IN) :: Pn_sym(3)

!!#### OPTIONAL OUTPUT
REAL(KIND_MSH),OPTIONAL,INTENT(OUT) :: relerr
REAL(KIND_MSH),OPTIONAL,INTENT(IN)  :: tol

!!#### LOCAL VARIABLES
INTEGER :: Nr,Nj,Nm,g,j,m,r
INTEGER :: m1,m2,j1,j2
CHARACTER(32),ALLOCATABLE :: D(:,:)
REAL(KIND_MSH) :: max_reldiff,AF1,AF2,DAF,tol_,AVG,max_diff

!!--begin--

tol_ = Default(MeshScale(Mesh)*EPSILON(1._KIND_MSH),tol)

g = 1
Nj = SIZE(AngularFluxF,2)
Nm = SIZE(AngularFluxF,3)

Nr = (Nj)*(Nm)
ALLOCATE( D(Nr+1,7) )
CALL CLEAR(D)

r = 1
D(1,:) = (/"m1 ","m2 ","j1 ","j2 ","AF1","AF2","DAF"/)

max_diff=0.d0
max_reldiff = 0.d0

DO j=1,Nj
 DO m=1,Nm

  m1 = m
  m2 = SymmetryDirection(Ordinates,Pn_sym,Ordinates(:,m1))

  j1 = j
  j2 = SymmetryFace(Mesh,Pn_sym,FaceCentroid(Mesh,j1),FaceNormal(Mesh,j1),tol=tol_)

  r = r+1
  D(r,1) = STR(m1)
  D(r,2) = STR(m2)
  D(r,3) = STR(j1)
  D(r,4) = STR(j2)
  AF1 = AngularFluxF(g,j1,m1)
  D(r,5) = STR(AF1,"(Es16.8)")
  IF( j2/=0 .AND. m2/=0 )THEN

   AF2 = AngularFluxF(g,j2,m2)
   DAF = AF1-AF2
   AVG = 0.5_KIND_AngularFlux*(AF1+AF2)
   max_diff=MAX(max_diff,ABS(DAF))   
   max_reldiff = MAX(max_reldiff,ABS(DAF)/AVG)

   D(r,6) = STR(AF2,"(Es16.8)")
   D(r,7) = STR(DAF,"(Es12.4)")
  ELSE
   D(r,6) = "No Symmetric Point"
   D(r,7) = "N/A"
  END IF

 END DO
END DO

IF( Unit/=0 )THEN
 CALL PRINT_Table(D,Unit=Unit)
 WRITE(Unit,"(a)")" maximum relative difference: "//TRIM(STR(max_reldiff,"(Es12.4)"))
 WRITE(Unit,"(a)")" maximum difference: "//TRIM(STR(max_diff,"(Es12.4)"))
END IF

IF( PRESENT(relerr) )THEN
 relerr = max_reldiff
END IF

DEALLOCATE(D)

!!--end--
END SUBROUTINE

!!### PRINTING SUBROUTINE: <PRINT_AFSymmetryCheck>
SUBROUTINE PRINT_AFSymmetryCheck(Unit,Pn_sym,tol,relerr)

!!#### PURPOSE
!! Print a check of symmetry of the angular
!! fluxes of a problem, about a given line.

USE VAR_AngularFluxes                                              !!((04-C-VAR_AngularFluxes.f90))
USE VAR_DiscreteOrdinates,ONLY: Ordinates                          !!((47-B-VAR_DiscreteOrdinates.f90))
USE VAR_Mesh,ONLY: Mesh                                            !!((46-B-VAR_Mesh.f90))

!!#### REQUIRED INPUT
INTEGER       ,INTENT(IN) :: Unit
REAL(KIND_MSH),INTENT(IN) :: Pn_sym(3)

!!#### OPTIONAL OUTPUT
REAL(KIND_MSH),OPTIONAL,INTENT(OUT) :: relerr
REAL(KIND_MSH),OPTIONAL,INTENT(IN)  :: tol

!!#### LOCAL VARIABLES
INTEGER :: Nr,Nk,Nm,g,k,m,r
INTEGER :: m1,m2,k1,k2
CHARACTER(32),ALLOCATABLE :: D(:,:)
REAL(KIND_MSH) :: max_reldiff,AF1,AF2,DAF,tol_,AVG,max_diff

!!--begin--

tol_ = Default(MeshScale(Mesh)*EPSILON(1._KIND_MSH),tol)

g = 1
Nk = SIZE(AngularFluxV,2)
Nm = SIZE(AngularFluxV,3)

Nr = (Nk)*(Nm)
ALLOCATE( D(Nr+1,7) )
CALL CLEAR(D)

r = 1
D(1,:) = (/"m1 ","m2 ","k1 ","k2 ","AF1","AF2","DAF"/)

max_reldiff = 0.d0
max_diff=0.d0

DO k=1,Nk
 DO m=1,Nm

  m1 = m
  m2 = SymmetryDirection(Ordinates,Pn_sym,Ordinates(:,m1))

  k1 = k
  k2 = SymmetryPoint(Mesh,Pn_sym,Vert(Mesh,k1),tol=tol_)

  r = r+1
  D(r,1) = STR(m1)
  D(r,2) = STR(m2)
  D(r,3) = STR(k1)
  D(r,4) = STR(k2)
  AF1 = AngularFluxV(g,k1,m1)
  D(r,5) = STR(AF1,"(Es16.8)")
  IF( k2/=0 .AND. m2/=0 )THEN

   AF2 = AngularFluxV(g,k2,m2)
   DAF = AF1-AF2
   AVG = 0.5_KIND_AngularFlux*(AF1+AF2)
   max_diff=MAX(max_diff,ABS(DAF))   
   max_reldiff = MAX(max_reldiff,ABS(DAF)/AVG)

   D(r,6) = STR(AF2,"(Es16.8)")
   D(r,7) = STR(DAF,"(Es12.4)")
  ELSE
   D(r,6) = "No Symmetric Point"
   D(r,7) = "N/A"
  END IF

 END DO
END DO

IF( Unit/=0 )THEN
 CALL PRINT_Table(D,Unit=Unit)
 WRITE(Unit,"(a)")" maximum relative difference: "//TRIM(STR(max_reldiff,"(Es12.4)"))
WRITE(Unit,"(a)")" maximum difference: "//TRIM(STR(max_diff,"(Es12.4)"))
END IF

IF( PRESENT(relerr) )THEN
 relerr = max_reldiff
END IF

DEALLOCATE(D)

!!--end--
END SUBROUTINE



!!### PRINTING SUBROUTINE: <PRINT_SweepOrder>
SUBROUTINE PRINT_SweepOrder( FdBk , Unit )

!!#### PURPOSE
!! Print the sweeping order.

!!#### MODULES
USE VAR_MoCshort         ,ONLY: pThread,WithinCell                 !!((47-B-VAR_MoCshort.f90))
USE VAR_DiscreteOrdinates,ONLY: Ordinates                          !!((47-B-VAR_DiscreteOrdinates.f90))

!!#### OPTIONAL INPUT
INTEGER,OPTIONAL,INTENT(IN) :: Unit

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_FdBk),OPTIONAL,INTENT(INOUT) :: FdBk

!!#### LOCAL VARIABLES
INTEGER :: Nk,Nm,Nc,Nr,o,m,r,c,kdig,mpdig
CHARACTER(64),ALLOCATABLE :: Tab(:,:)
TYPE(varying_string) :: fmtk,fmtmp
!!--begin--

Nk = SIZE(WithinCell,1)
Nm = SIZE(Ordinates,2)
Nr = Nk  + 1
Nc = Nm + 1
ALLOCATE( Tab(Nr,Nc) )
CALL CLEAR(Tab)

!calculate number of digits
kdig = int(log10(real(abs(Nk))))+1
mpdig = int(log10(real(abs(Nm))))+1
fmtk="" ; fmtk = "(I"//TRIM(STR(kdig+1))//"."//TRIM(STR(kdig))//")"
fmtmp="" ; fmtmp = "(I"//TRIM(STR(mpdig+1))//"."//TRIM(STR(mpdig))//")"

!initialize row number
r = 1

!top left corner
Tab(r,1) = "o\m"
!set m from left to right
DO c=2,Nc
 Tab(1,c) = STR(c-1,STR(fmtmp))
END DO
!set k from top to bottom
DO r=2,Nr
 Tab(r,1) = STR(r-1,STR(fmtk))
END DO

fmtk=""
fmtmp=""


!table body
DO m=1,Nm
 DO o=1,SIZE(pthread(m)%path(1)%order)
  r = o+1
  c = m+1
  Tab(r,c) = STR( pthread(m)%path(1)%order(o) )
 END DO
END DO


!print table
CALL PRINT_Table( Tab(1:r,:) , Unit=Unit )
DEALLOCATE( Tab )

!!--end--
END SUBROUTINE



!!### PRINTING SUBROUTINE: <PRINT_IncomingDirections>
SUBROUTINE PRINT_IncomingDirections( FdBk , Unit )

!!#### PURPOSE
!! For each vertex/direction combination, those which are incoming to the
!! domain.

!!#### MODULES
USE VAR_MoCshort         ,ONLY: WithinCell                         !!((47-B-VAR_MoCshort.f90))

!!#### OPTIONAL INPUT
INTEGER,OPTIONAL,INTENT(IN) :: Unit

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_FdBk),OPTIONAL,INTENT(INOUT) :: FdBk

!!#### LOCAL VARIABLES
INTEGER :: Nk,Nm,Nc,Nr,k,m,r,c,kdig,mdig
CHARACTER(64),ALLOCATABLE :: Tab(:,:)
TYPE(varying_string) :: fmtk,fmtm
!!--begin--

Nk = SIZE(WithinCell,1)
Nm = SIZE(WithinCell,2)

Nr  = Nk + 1
Nc  = Nm + 1
ALLOCATE( Tab(Nr,Nc) )
CALL CLEAR(Tab)

!calculate number of digits
kdig = int(log10(real(abs(Nk))))+1
mdig = int(log10(real(abs(Nm))))+1
fmtk="" ; fmtk = "(I"//TRIM(STR(kdig+1))//"."//TRIM(STR(kdig))//")"
fmtm="" ; fmtm = "(I"//TRIM(STR(mdig+1))//"."//TRIM(STR(mdig))//")"

!initialize row number
r = 1

!top left corner
Tab(r,1) = "k\m"
!set ma from left to right
DO c=2,Nc
 Tab(1,c) = STR(c-1,STR(fmtm))
END DO
!set k from top to bottom
DO r=2,Nr
 Tab(r,1) = STR(r-1,STR(fmtk))
END DO

fmtk=""
fmtm=""


!table body
DO k=1,Nk
 DO m=1,Nm
  r = k+1
  c = m+1
  Tab(r,c) = STR(WithinCell(k,m)<=0)
 END DO
END DO


!print table
CALL PRINT_Table( Tab(1:r,:) , Unit=Unit )
DEALLOCATE( Tab )

!!--end--
END SUBROUTINE



!!### PRINTING SUBROUTINE: <PRINT_InteriorCells>
SUBROUTINE PRINT_InteriorCells( FdBk , Unit )

!!#### PURPOSE
!! Output interior cell for each vertex, direction.

!!#### MODULES
USE VAR_MoCshort         ,ONLY: WithinCell                         !!((47-B-VAR_MoCshort.f90))
USE VAR_DiscreteOrdinates,ONLY: Ordinates                          !!((47-B-VAR_DiscreteOrdinates.f90))

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_FdBk),OPTIONAL,INTENT(INOUT) :: FdBk

!!#### OPTIONAL INPUT
INTEGER,OPTIONAL,INTENT(IN) :: Unit

!!#### LOCAL VARIABLES
INTEGER :: Nk,Nm,Nc,Nr,k,m,r,c,kdig,mdig
CHARACTER(64),ALLOCATABLE :: Tab(:,:)
TYPE(varying_string) :: fmtk,fmtm
!!--begin--

Nk  = SIZE(WithinCell,1)
Nm  = SIZE(WithinCell,2)
Nr  = Nk + 1
Nc  = Nm + 1
ALLOCATE( Tab(Nr,Nc) )
CALL CLEAR(Tab)

!calculate number of digits
kdig = int(log10(real(abs(Nk))))+1
mdig = int(log10(real(abs(Nm))))+1
fmtk="" ; fmtk = "(I"//TRIM(STR(kdig+1))//"."//TRIM(STR(kdig))//")"
fmtm="" ; fmtm = "(I"//TRIM(STR(mdig+1))//"."//TRIM(STR(mdig))//")"

!initialize row number
r = 1

!top left corner
Tab(r,1) = "k\m"
!set ma from left to right
DO c=2,Nc
 Tab(1,c) = STR(c-1,STR(fmtm))
END DO
!set k from top to bottom
DO r=2,Nr
 Tab(r,1) = STR(r-1,STR(fmtk))
END DO

fmtk=""
fmtm=""


!table body
DO k=1,Nk
 DO m=1,Nm
  r = k+1
  c = m+1
  Tab(r,c) = STR(WithinCell(k,m))
 END DO
END DO


!print table
CALL PRINT_Table( Tab(1:r,:) , Unit=Unit )
DEALLOCATE( Tab )

!!--end--
END SUBROUTINE




!!### PRINTING SUBROUTINE: <PRINT_StreamingInterpolants>
SUBROUTINE PRINT_StreamingInterpolants( FdBk , Unit )

!!#### PURPOSE
!! Output the streaming interpolants (vertices used in
!! the streaming interpolation) for each vertex
!! and directions.

!!#### MODULES
USE VAR_Mesh    ,ONLY: Mesh                                        !!((46-B-VAR_Mesh.f90))
USE VAR_MoCshort,ONLY: InterpOrder,FrontPos,k_                     !!((47-B-VAR_MoCshort.f90))



!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_FdBk),OPTIONAL,INTENT(INOUT) :: FdBk

!!#### REQUIRED INPUT
INTEGER,OPTIONAL,INTENT(IN) :: Unit

!!#### LOCAL VARIABLES
INTEGER :: k,m,r,s,Nr,Nc,Nk,Nm
CHARACTER(32),ALLOCATABLE :: Tab(:,:)

!!--begin--

Nc  = 2+2*(InterpOrder+1)
Nk  = SIZE(k_,2)
Nm  = SIZE(k_,3)
Nr  = Nk*Nm + 1
ALLOCATE( Tab(Nr,Nc) )
CALL CLEAR(Tab)

!initialize row number
r = 1

!header
Tab(r,1) = "k"
Tab(r,2) = "m"
DO s=1,InterpOrder+1
 Tab(r,s+2            ) = "k"//TRIM(STR(s))
END DO
DO s=1,InterpOrder+1
 Tab(r,s+3+InterpOrder) = "P"//TRIM(STR(s))
END DO


!table body
DO k=1,Nk

 !loop through directions
 DO m=1,Nm

  !cycle if there is no interpolation for this vert/direction combo
  IF( k_(1,k,m)==0 )CYCLE

  !update the row number
  r = r + 1

  !assemble the table body
  Tab(r,1) = STR(k)
  Tab(r,2) = STR(m)
  DO s=1,InterpOrder+1
   Tab(r,s+2            ) = STR(k_(s,k,m))
  END DO
  DO s=1,InterpOrder+1
   IF( k_(s,k,m)/=0 )THEN
    Tab(r,s+3+InterpOrder) = STR(FrontPos(s,k,m),"(Es22.15)")
   END IF
  END DO

 END DO

END DO


!print table
CALL PRINT_Table( Tab(1:r,:) , Unit=Unit )

DEALLOCATE( Tab )

!!--end--
END SUBROUTINE



!!### PRINTING SUBROUTINE: <PRINT_InterpInfo>
SUBROUTINE PRINT_InterpInfo(fdbk,Unit)

!!#### PURPOSE
!! Print information about the interpolation.

USE VAR_Mesh,ONLY: Mesh                                            !!((46-B-VAR_Mesh.f90))
USE VAR_DiscreteOrdinates,ONLY: Ordinates                          !!((47-B-VAR_DiscreteOrdinates.f90))
USE VAR_MoCshort,ONLY: pThread,k_,WithinCell,FrontPos              !!((47-B-VAR_MoCshort.f90))
USE VAR_EnergyGroups,ONLY: Ng                                      !!((47-B-VAR_EnergyGroups.f90))

INTEGER,OPTIONAL,INTENT(IN) :: Unit
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk


INTEGER :: Nm,Nk,Nr,Nc,r,m,o,k,i,k1,k2,k3,p
REAL(KIND_MSH) :: x1,x2,x3,f1,f2,f3,c12,c23,c31
CHARACTER(32),ALLOCATABLE :: Tab(:,:)

!!--begin--

Nm = SIZE(Ordinates,2)
Nk = NUM_Verts(Mesh)
Nr = Nk*Nm+1
Nc = 13
WRITE(*,*)Nm,Nk,Nr,Nc
ALLOCATE( Tab(Nr,Nc) )
CALL CLEAR(Tab)
Tab(1,:) = (/"o ","k ","m ","i ","k1","k2","k3","x1","x2","x3","f1","f2","f3"/)

r = 1
!enter main loop over polar angles
m_loop: DO m = 1,SIZE(pthread)


 !proceed down each path
 path_loop: DO p = 1,SIZE(pthread(m)%path)

  !in the proper order of verts
  order_loop: DO o = 1,SIZE(pthread(m)%path(p)%order)

   !get the vert index
   k = pthread(m)%path(p)%order(o)

   !get the cell index
   i = WithinCell(k,m)

   r = r + 1

   Tab(r,1) = STR(o,"(I)")
   Tab(r,2) = STR(k,"(I)")
   Tab(r,3) = STR(m,"(I)")
   Tab(r,4) = STR(i,"(I)")

   !cycle if there is no cell index
   IF( i<=0 )CYCLE

   Tab(r,4) = STR(r-1,"(I)")

   !get the two vertices for the interpolation of the scalar flux
   k1 = k_(1,k,m) ; Tab(r,5) = STR(k1)
   k2 = k_(2,k,m) ; Tab(r,6) = STR(k2)
   IF( SIZE(k_,1)>2 )THEN
    k3 = k_(3,k,m) ; Tab(r,7) = STR(k3)
   ELSE
    k3 = 0
   END IF

   !get front positions
   IF( ASSOCIATED(FrontPos) )THEN

    x1 = FrontPos( 1 , k , m ) ; Tab(r,08) = STR(x1,"(Es22.15)")
    x2 = FrontPos( 2 , k , m ) ; Tab(r,09) = STR(x2,"(Es22.15)")
    IF( SIZE(k_,1)>2 )THEN
         x3 = FrontPos( 3 , k , m ) ; Tab(r,10) = STR(x3,"(Es22.15)")
    ELSE
         x3 = 0.d0
        END IF

    !calculate streaming angular fluxes
    IF( k1==0 )THEN
     f1 = 0.d0
     f2 = - x2/( x3 - x2 )
     f3 = + x3/( x3 - x2 )

    ELSE IF( k2==0 )THEN
     f1 = - x1/( x3 - x1 )
     f2 = 0.d0
     f3 = + x3/( x3 - x1 )

    ELSE IF( k3==0 )THEN
     f1 = + x2/( x2 - x1 )
     f2 = - x1/( x2 - x1 )
     f3 = 0.d0
    ELSE

     !(all this comes from symbolic evalutation in maple)
     c12 = x1*x2
     c23 = x2*x3
     c31 = x3*x1
     f1 = + c23/(-c12+c23-c31+x1**2)
     f2 = - c31/(+c12+c23-c31-x2**2)
     f3 = + c12/(+c12-c23-c31+x3**2)
    END IF

    Tab(r,11) = STR(f1,"(F)")
    Tab(r,12) = STR(f2,"(F)")
    Tab(r,13) = STR(f3,"(F)")

   END IF

  END DO order_loop

 END DO path_loop

END DO m_loop

CALL PRINT_Table(Tab(1:r,:),Unit=Unit)
DEALLOCATE( Tab )

!!--end--
END SUBROUTINE





!!### PRINTING SUBROUTINE: <PRINT_CharacteristicInfo_>
SUBROUTINE PRINT_CharacteristicInfo(fdbk,Unit)

!!#### PURPOSE
!! Print information about the characteristics.

USE VAR_Mesh,ONLY: Mesh                                            !!((46-B-VAR_Mesh.f90))
USE VAR_EnergyGroups,ONLY: Ng                                      !!((47-B-VAR_EnergyGroups.f90))
USE VAR_DiscreteOrdinates,ONLY: Ordinates                          !!((47-B-VAR_DiscreteOrdinates.f90))
USE VAR_MoCshort,ONLY: pthread,WithinCell,k_,SourceDist,StreamDist !!((47-B-VAR_MoCshort.f90))

INTEGER,OPTIONAL,INTENT(IN) :: Unit
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk


INTEGER :: Nm,Nk,Nr,Nc,r,k,m,k1,k2,k3,p,o,i
REAL(KIND_MSH) :: s1,s2,s3,s0
CHARACTER(32),ALLOCATABLE :: Tab(:,:)

!!--begin--

Nm = SIZE(Ordinates,2)
Nk = NUM_Verts(Mesh)
Nr = Nk*Nm+1
Nc = 9
ALLOCATE( Tab(Nr,Nc) )
CALL CLEAR(Tab)
Tab(1,:) = (/"k ","m ","k1","k2","k3","s1","s2","s3","s0"/)

r = 1
!enter main loop over polar angles
m_loop: DO m = 1,SIZE(pthread)


 !proceed down each path
 path_loop: DO p   = 1,SIZE(pthread(m)%path)

  !in the proper order of verts
  order_loop: DO o = 1,SIZE(pthread(m)%path(p)%order)

   !get the vert index
   k = pthread(m)%path(p)%order(o)

   i = WithinCell(k,m)

   r = r + 1

   Tab(r,1) = STR(k,"(I)")
   Tab(r,2) = STR(m,"(I)")

   !cycle if there is no cell index
   IF( i<=0 )CYCLE

   !get the two vertices for the interpolation of the scalar flux
   k1 = k_(1,k,m) ; Tab(r,3) = STR(k1)
   k2 = k_(2,k,m) ; Tab(r,4) = STR(k2)
   IF( SIZE(k_,1)>2 )THEN
    k3 = k_(3,k,m) ; Tab(r,5) = STR(k3)
   ELSE
    k3 = 0
   END IF

   !get streaming distances
   IF( ASSOCIATED(StreamDist) )THEN

    s1 = StreamDist( 1 , k , m ) ; Tab(r,6) = STR(s1,"(Es22.15)")
    s2 = StreamDist( 2 , k , m ) ; Tab(r,7) = STR(s2,"(Es22.15)")
    IF( SIZE(k_,1)>2 )THEN
         s3 = StreamDist( 3 , k , m ) ; Tab(r,8) = STR(s3,"(Es22.15)")
    ELSE
         s3 = 0.d0
        END IF

        s0 = SourceDist(k,m) ; Tab(r,9) = STR(s0,"(Es22.15)")

   END IF

  END DO order_loop

 END DO path_loop

END DO m_loop

CALL PRINT_Table(Tab(1:r,:),Unit=Unit)
DEALLOCATE( Tab )

!!--end--
END SUBROUTINE



SUBROUTINE PrintAngularFlux()
USE VAR_Mesh,ONLY: Mesh                                            !!((46-B-VAR_Mesh.f90))
USE VAR_AngularFluxes                                              !!((04-C-VAR_AngularFluxes.f90))

!!#### LOCAL VARIABLES
INTEGER        :: Unit,k,m
REAL(KIND_MSH) :: V(2)

!!--begin--

DO m=1,SIZE(AngularFluxV,3)
 Unit = Newfile("AF_"//TRIM(STR(m)))
 DO k=1,NUM_Verts(Mesh)
  V = Vert(Mesh,k)
  WRITE(Unit,"(3e16.5)")V(1),V(2),AngularFluxV(1,k,m)
 END DO
 CLOSE(Unit)
END DO

!!--end--
END SUBROUTINE



SUBROUTINE PrintQTracks()
INTEGER :: Unit
!!--begin--
Unit = Newfile("Qtracks")
CALL SimpSurf( Unit , Qfull2D , (/0.d0,0.1d0,5.d0/) , (/0.d0,0.1d0,3.d0/) )
CLOSE(Unit)
!!--end--
END SUBROUTINE



SUBROUTINE PRINT_Options_MCS(Unit)
USE VAR_MoCshort                                                   !!((47-B-VAR_MoCshort.f90))
INTEGER,INTENT(IN) :: Unit
!!--begin--

WRITE(Unit,"(a)")" * [[MCS]] Options"
CALL PUT_line(Unit,VSTROPTION("InterpOrder",STR(InterpOrder)))
CALL PUT_line(Unit,VSTROPTION("SourceOrder",STR(SourceOrder)))
CALL PUT_line(Unit,VSTROPTION("OnlyGeometry",STR(OnlyGeometry)))
CALL PUT_line(Unit,VSTROPTION("InterpPlaneU",STR(InterpPlaneU)))
CALL PUT_line(Unit,VSTROPTION("Using_LinearSourceTest",STR(Using_LinearSourceTest)))
CALL PUT_line(Unit,VSTROPTION("Using_Monotonization",STR(Using_Monotonization)))
CALL PUT_line(Unit,VSTROPTION("Using_MonoLin",STR(Using_MonoLin)))
CALL PUT_line(Unit,VSTROPTION("Using_Splitting",STR(Using_Splitting)))
CALL PUT_line(Unit,VSTROPTION("Using_LogTransform",STR(Using_LogTransform)))
CALL PUT_line(Unit,VSTROPTION("Using_Jiggle",STR(Using_Jiggle)))
CALL PUT_line(Unit,VSTROPTION("Using_ExplodeFix",STR(Using_ExplodeFix)))
CALL PUT_line(Unit,VSTROPTION("Using_NoBacksies",STR(Using_NoBacksies)))
CALL PUT_line(Unit,VSTROPTION("Using_LongCharacteristics",STR(Using_LongCharacteristics)))
CALL PUT_line(Unit,VSTROPTION("Using_CachedLongCharacteristics",STR(Using_CachedLongCharacteristics)))
CALL PUT_line(Unit,VSTROPTION("Using_PackedCaching",STR(Using_PackedCaching)))
CALL PUT_line(Unit,VSTROPTION("Using_AFSymmetryCheck",STR(Using_AFSymmetryCheck)))

!!--end--
END SUBROUTINE





END MODULE
