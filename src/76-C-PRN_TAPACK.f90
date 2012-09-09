!!# PRINTING MODULE <<PRN_TAPACK>>
MODULE PRN_TAPACK

!!## PURPOSE
!! The printing routines for TAPACK.

!!## MODULES
USE KND_AngularFluxes                             !!((02-A-KND_AngularFluxes.f90))
USE KND_ScalarFluxes                              !!((02-A-KND_ScalarFluxes.f90))
USE KND_Currents                                  !!((02-A-KND_Currents.f90))
USE FUN_STR                                       !!((05-B-FUN_STR.f90))
USE PRN_Table                                     !!((11-B-PRN_Table.f90))
USE FUN_NewFile                                   !!((05-B-FUN_NewFile.f90))
USE KND_XSExpansion                               !!((02-A-KND_XSExpansion.f90))
USE KND_Source                                    !!((02-A-KND_Source.f90))
USE FUN_EXIST                                     !!((04-B-FUN_EXIST.f90))
USE USR_Source                                    !!((35-B-USR_Source.f90))
USE TBX_DiscreteOrdinates,ONLY: PolAngle,AziAngle !!((56-B-TBX_DiscreteOrdinates.f90))
USE PAR_Units                                     !!((02-A-PAR_Units.f90))
USE VAR_Units                                     !!((03-A-VAR_Units.f90))
USE FUN_OldFile                                   !!((06-C-FUN_OldFile.f90))
USE FUN_NewFile                                   !!((05-B-FUN_NewFile.f90))
USE SUB_DeleteFile                                !!((75-C-SUB_DeleteFile.f90))
USE FUN_COUNT_Lines                               !!((07-B-FUN_COUNT_Lines.f90))
USE SUB_IncludeFiles                              !!((12-C-SUB_IncludeFiles.f90))
USE PRN_Mesh                                      !!((16-C-PRN_Mesh.f90))
USE USR_fdbk                                      !!((08-C-USR_fdbk.f90))
USE FUN_Default                                   !!((04-A-FUN_Default.f90))
USE FUN_TimeStamp                                 !!((05-B-FUN_TimeStamp.f90))
USE FUN_STRTIME                                   !!((06-C-FUN_STRTIME.f90))
USE PAR_Constants_Rdp                             !!((02-A-PAR_Constants_Rdp.f90))
USE FUN_IsApprox                                  !!((03-A-FUN_IsApprox.f90))
USE SUB_CLEARn                                    !!((04-A-SUB_CLEARn.f90))
USE SUB_CLEAR                                     !!((04-A-SUB_CLEAR.f90))
USE USR_SimpleList                                !!((09-B-USR_SimpleList.f90))
USE USR_Mesh                                      !!((14-B-USR_Mesh.f90))
USE TBX_Mesh                                      !!((15-B-TBX_Mesh.f90))
USE USR_IntegralRegion                            !!((14-B-USR_IntegralRegion.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## DEFAULT ACCESS
PRIVATE

!!## PUBLIC ACCESS
PUBLIC :: PRINT_AngularFluxV_gen
PUBLIC :: PRINT_AngularFluxF_gen
PUBLIC :: PRINT_AngularFluxC_gen

PUBLIC :: PRINT_ScalarFluxV
PUBLIC :: PRINT_ScalarFluxF
PUBLIC :: PRINT_ScalarFluxC

PUBLIC :: PRINT_CurrentV
PUBLIC :: PRINT_CurrentF
PUBLIC :: PRINT_CurrentC
PUBLIC :: PRINT_CurrentFN

PUBLIC :: PRINT_ScalarFlux_CellRegions
PUBLIC :: PRINT_convergence_phi

PUBLIC :: PRINT_Current_Exiting
PUBLIC :: PRINT_convergence_flow

PUBLIC :: PRINT_BC

PUBLIC :: PRINT_TimeSummary
PUBLIC :: PRINT_TimeTable
PUBLIC :: PRINT_NuclearData

PUBLIC :: PRINT_EddingtonsF
PUBLIC :: PRINT_EddingtonsC
PUBLIC :: PRINT_EddingtonsV

PUBLIC :: PRINT_CBoundary

PUBLIC :: PRINT_KEYS

PUBLIC :: PRINT_InputFile

!PUBLIC :: PRINT_GridEddingtonsV
!PUBLIC :: PRINT_GridEddingtonsF
!PUBLIC :: PRINT_GridEddingtonsC
!
PUBLIC :: PRINT_GridScalarFluxV
PUBLIC :: PRINT_GridScalarFluxF
PUBLIC :: PRINT_GridScalarFluxC


!!## CONTAINED PROCEDURES
CONTAINS


SUBROUTINE PRINT_EddingtonsV( &
 Mesh , &
 KxxV , KyyV , KxyV , &
 ExxV , EyyV , ExyV , &
 Fdbk , &
 Unit   )
!!#### PURPOSE
!! Output the Eddington factors of verts.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
REAL(KIND_AngularFlux) ,INTENT(IN) :: KxxV(:,:),KyyV(:,:),KxyV(:,:)
REAL(KIND_AngularFlux) ,INTENT(IN) :: ExxV(:,:),EyyV(:,:),ExyV(:,:)


!!#### OPTIONAL INPUT
INTEGER,OPTIONAL,INTENT(IN) :: Unit

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
INTEGER :: g,k,Ng,Nk,n,Unit_
CHARACTER(32),POINTER :: D(:,:)
REAL(KIND_MSH) :: V(2)

!!--begin--

Unit_ = DEFAULT(DEFAULT_OUTPUT_UNIT,Unit)
Ng = SIZE(ExxV,1)
Nk = SIZE(ExxV,2)

!!clear the pointer space and allocate
CALL CLEARn(D)
ALLOCATE( D(0:Nk*Ng,6) )

!!clear the character space
CALL CLEAR(D)

!!setup labels
D(0,1:4) = (/"g         ",&
             "k         ",&
             "Vx(k)     ",&
             "Vy(k)     "/)

n = 0
DO k=1,Nk
 DO g=1,Ng
  n = n + 1
  D(n,1) = TRIM(STR(g))
  D(n,2) = TRIM(STR(k))

  V = Vert(Mesh,k)
  D(n,3) = TRIM(STR(V(1),"(Es10.3)"))
  D(n,4) = TRIM(STR(V(2),"(Es10.3)"))
 END DO
END DO

!print xx
WRITE(Unit_,"(a)")" * Vert-Eddington-xx"
D(0,5) = "KxxV(g,k)"
D(0,6) = "ExxV(g,k)"
n = 0
DO k=1,Nk
 DO g=1,Ng
  n = n + 1

  D(n,5) = TRIM(STR(KxxV(g,k),"(Es21.14)"))
  D(n,6) = TRIM(STR(ExxV(g,k),"(Es21.14)"))

 END DO
END DO
CALL Print_Table(D,Unit=Unit_)


!print yy
WRITE(Unit_,"(a)")" * Vert-Eddington-yy"
D(0,5) = "KyyV(g,k)"
D(0,6) = "EyyV(g,k)"
n = 0
DO k=1,Nk
 DO g=1,Ng
  n = n + 1

  D(n,5) = TRIM(STR(KyyV(g,k),"(Es21.14)"))
  D(n,6) = TRIM(STR(EyyV(g,k),"(Es21.14)"))

 END DO
END DO
CALL Print_Table(D,Unit=Unit_)


!print xy
WRITE(Unit_,"(a)")" * Vert-Eddington-xy"
D(0,5) = "KxyV(g,k)"
D(0,6) = "ExyV(g,k)"
n = 0
DO k=1,Nk
 DO g=1,Ng
  n = n + 1

  D(n,5) = TRIM(STR(KxyV(g,k),"(Es21.14)"))
  D(n,6) = TRIM(STR(ExyV(g,k),"(Es21.14)"))

 END DO
END DO
CALL Print_Table(D,Unit=Unit_)


CALL CLEARn(D)

!!--end--
END SUBROUTINE


SUBROUTINE PRINT_CBoundary( &
 Mesh , &
 C , jbdry , Phi_IN , J_IN , &
 Fdbk , &
 Unit   )
!!#### PURPOSE
!! Output the boundary factors of faces.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh)        ,INTENT(IN) :: Mesh
REAL(KIND_AngularFlux) ,INTENT(IN) :: C(:,:)
INTEGER                ,INTENT(IN) :: Jbdry(:)
REAL(KIND_AngularFlux) ,INTENT(IN) :: Phi_IN(:,:)
REAL(KIND_AngularFlux) ,INTENT(IN) :: J_IN(:,:)

!!#### OPTIONAL INPUT
INTEGER,OPTIONAL,INTENT(IN) :: Unit

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
INTEGER :: g,j,jb,Nb,Ng,n,Unit_
CHARACTER(32),POINTER :: D(:,:)
REAL(KIND_MSH) :: FC(2)

!!--begin--
Unit_ = DEFAULT(DEFAULT_OUTPUT_UNIT,Unit)
Ng = SIZE(C,1)
Nb = SIZE(jbdry)

!!clear the pointer space and allocate
CALL CLEARn(D)
ALLOCATE( D(0:Nb*Ng,7) )

!!clear the character space
CALL CLEAR(D)

!!setup labels
D(0,1:7) = (/"g             ",&
             "j             ",&
             "FCx(j)        ",&
             "FCy(j)        ",&
             "PhiInF(g,j)   ",&
             "JInF  (g,j)   ",&
             "CBoundary(g,j)"/)

n = 0
DO jb=1,Nb
 DO g=1,Ng
  n = n + 1
  D(n,1) = TRIM(STR(g))
  j = jbdry(jb)
  D(n,2) = TRIM(STR(j))

  FC = FaceCentroid(Mesh,j)
  D(n,3) = TRIM(STR(FC(1),"(Es10.3)"))
  D(n,4) = TRIM(STR(FC(2),"(Es10.3)"))

  D(n,5) = TRIM(STR(Phi_IN(g,jb),"(Es21.14)"))
  D(n,6) = TRIM(STR(J_IN(g,jb),"(Es21.14)"))
  D(n,7) = TRIM(STR(C(g,jb),"(Es21.14)"))

 END DO
END DO
CALL Print_Table(D,Unit=Unit_)
CALL CLEARn(D)

!!--end--
END SUBROUTINE


SUBROUTINE PRINT_EddingtonsF( &
 Mesh , &
 KxxF , KyyF , KxyF , &
 ExxF , EyyF , ExyF , &
 Fdbk , &
 Unit   )
!!#### PURPOSE
!! Output the Eddington factors of faces.

!!#### REQUIRED INPUT
REAL(KIND_AngularFlux) ,INTENT(IN) :: KxxF(:,:),KyyF(:,:),KxyF(:,:)
REAL(KIND_AngularFlux) ,INTENT(IN) :: ExxF(:,:),EyyF(:,:),ExyF(:,:)
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh

!!#### OPTIONAL INPUT
INTEGER,OPTIONAL,INTENT(IN) :: Unit

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
INTEGER :: g,j,Ng,Nj,n,Unit_
CHARACTER(32),POINTER :: D(:,:)
REAL(KIND_MSH) :: FC(2)

!!--begin--

Unit_ = DEFAULT(DEFAULT_OUTPUT_UNIT,Unit)
Ng = SIZE(ExxF,1)
Nj = SIZE(ExxF,2)

!!clear the pointer space and allocate
CALL CLEARn(D)
ALLOCATE( D(0:Nj*Ng,6) )

!!clear the character space
CALL CLEAR(D)

!!setup labels
D(0,1:4) = (/"g         ",&
             "j         ",&
             "FCx(j)    ",&
             "FCy(j)    "/)

n = 0
DO j=1,Nj
 DO g=1,Ng
  n = n + 1
  D(n,1) = TRIM(STR(g))
  D(n,2) = TRIM(STR(j))

  FC = FaceCentroid(Mesh,j)
  D(n,3) = TRIM(STR(FC(1),"(Es10.3)"))
  D(n,4) = TRIM(STR(FC(2),"(Es10.3)"))
 END DO
END DO

!print xx
WRITE(Unit_,"(a)")" * Face-Eddington-xx"
D(0,5) = "KxxF(g,j)"
D(0,6) = "ExxF(g,j)"
n = 0
DO j=1,Nj
 DO g=1,Ng
  n = n + 1

  D(n,5) = TRIM(STR(KxxF(g,j),"(Es21.14)"))
  D(n,6) = TRIM(STR(ExxF(g,j),"(Es21.14)"))

 END DO
END DO
CALL Print_Table(D,Unit=Unit_)


!print yy
WRITE(Unit_,"(a)")" * Face-Eddington-yy"
D(0,5) = "KyyF(g,j)"
D(0,6) = "EyyF(g,j)"
n = 0
DO j=1,Nj
 DO g=1,Ng
  n = n + 1

  D(n,5) = TRIM(STR(KyyF(g,j),"(Es21.14)"))
  D(n,6) = TRIM(STR(EyyF(g,j),"(Es21.14)"))

 END DO
END DO
CALL Print_Table(D,Unit=Unit_)


!print xy
WRITE(Unit_,"(a)")" * Face-Eddington-xy"
D(0,5) = "KxyF(g,j)"
D(0,6) = "ExyF(g,j)"
n = 0
DO j=1,Nj
 DO g=1,Ng
  n = n + 1

  D(n,5) = TRIM(STR(KxyF(g,j),"(Es21.14)"))
  D(n,6) = TRIM(STR(ExyF(g,j),"(Es21.14)"))

 END DO
END DO
CALL Print_Table(D,Unit=Unit_)


CALL CLEARn(D)

!!--end--
END SUBROUTINE


SUBROUTINE PRINT_EddingtonsC( &
 Mesh , &
 KxxC , KyyC , KxyC , &
 ExxC , EyyC , ExyC , &
 fdbk , &
 Unit )
!!#### PURPOSE
!! Output the Eddington factors of cells.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
REAL(KIND_AngularFlux) ,INTENT(IN) :: KxxC(:,:),KyyC(:,:),KxyC(:,:)
REAL(KIND_AngularFlux) ,INTENT(IN) :: ExxC(:,:),EyyC(:,:),ExyC(:,:)

!!#### OPTIONAL INPUT
INTEGER,OPTIONAL,INTENT(IN) :: Unit

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
INTEGER :: g,i,Ng,Ni,n,Unit_
CHARACTER(32),POINTER :: D(:,:)
REAL(KIND_MSH) :: CC(2)

!!--begin--

Unit_ = DEFAULT(DEFAULT_OUTPUT_UNIT,Unit)
Ng = SIZE(ExxC,1)
Ni = SIZE(ExxC,2)

!!clear the pointer space and allocate
CALL CLEARn(D)
ALLOCATE( D(0:Ni*Ng,6) )

!!clear the character space
CALL CLEAR(D)

!!setup labels
D(0,1:4) = (/"g         ",&
             "i         ",&
             "CCx(i)    ",&
             "CCy(i)    "/)

n = 0
DO i=1,Ni
 DO g=1,Ng
  n = n + 1
  D(n,1) = TRIM(STR(g))
  D(n,2) = TRIM(STR(i))

  CC = CellCentroid(Mesh,i)
  D(n,3) = TRIM(STR(CC(1),"(Es10.3)"))
  D(n,4) = TRIM(STR(CC(2),"(Es10.3)"))
 END DO
END DO

!print xx
WRITE(Unit_,"(a)")" * Cell-Eddington-xx"
D(0,5) = "KxxC(g,i)"
D(0,6) = "ExxC(g,i)"
n = 0
DO i=1,Ni
 DO g=1,Ng
  n = n + 1

  D(n,5) = TRIM(STR(KxxC(g,i),"(Es21.14)"))
  D(n,6) = TRIM(STR(ExxC(g,i),"(Es21.14)"))

 END DO
END DO
CALL Print_Table(D,Unit=Unit_)


!print yy
WRITE(Unit_,"(a)")" * Cell-Eddington-yy"
D(0,5) = "KyyC(g,i)"
D(0,6) = "EyyC(g,i)"
n = 0
DO i=1,Ni
 DO g=1,Ng
  n = n + 1

  D(n,5) = TRIM(STR(KyyC(g,i),"(Es21.14)"))
  D(n,6) = TRIM(STR(EyyC(g,i),"(Es21.14)"))

 END DO
END DO
CALL Print_Table(D,Unit=Unit_)


!print xy
WRITE(Unit_,"(a)")" * Cell-Eddington-xy"
D(0,5) = "KxyC(g,i)"
D(0,6) = "ExyC(g,i)"
n = 0
DO i=1,Ni
 DO g=1,Ng
  n = n + 1

  D(n,5) = TRIM(STR(KxyC(g,i),"(Es21.14)"))
  D(n,6) = TRIM(STR(ExyC(g,i),"(Es21.14)"))

 END DO
END DO
CALL Print_Table(D,Unit=Unit_)


CALL CLEARn(D)

!!--end--
END SUBROUTINE


SUBROUTINE PRINT_GridScalarFluxC( &
  Mesh , &
  ScalarFluxC , &
  fdbk , &
  Unit )
!!#### PURPOSE
!! Output the cell-average scalar Flux on a grid.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh)      ,INTENT(IN) :: Mesh
REAL(KIND_ScalarFlux),INTENT(IN) :: ScalarFluxC(:,:)

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### OPTIONAL INPUT
INTEGER,OPTIONAL,INTENT(IN) :: Unit

!!#### LOCAL VARIABLES
REAL(KIND(ScalarFluxC)) :: r(1:NUM_Dimensions(Mesh),1:SIZE(ScalarFluxC,2))
INTEGER                 :: i,Unit_

!!--begin--
Unit_ = DEFAULT(DEFAULT_OUTPUT_UNIT,Unit)

!get r(x,y)
DO i=1,SIZE(ScalarFluxC,2)
 r(:,i) = CellCentroid(Mesh,i)
END DO

!print the grid out
CALL PRINT_Grid(Mesh,Unit_,ScalarFluxC,r,FType="Cell",InterpType="Const")

!!--end--
END SUBROUTINE




SUBROUTINE PRINT_GridScalarFluxF(  &
  Mesh , &
  ScalarFluxF , &
  fdbk , &
  Unit )
!!#### PURPOSE
!! Output the face-average scalar Flux on a grid.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh)      ,INTENT(IN) :: Mesh
REAL(KIND_ScalarFlux),INTENT(IN) :: ScalarFluxF(:,:)

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### OPTIONAL INPUT
INTEGER,OPTIONAL,INTENT(IN) :: Unit

!!#### LOCAL VARIABLES
REAL(KIND(ScalarFluxF)) :: r(1:NUM_Dimensions(Mesh),1:SIZE(ScalarFluxF,2))
INTEGER                 :: j,Unit_

!!--begin--
Unit_ = DEFAULT(DEFAULT_OUTPUT_UNIT,Unit)

!get the values of r(x,y)
DO j=1,SIZE(ScalarFluxF,2)
 r(:,j) = FaceCentroid(Mesh,j)
END DO

!print the grid out
CALL PRINT_Grid(Mesh,Unit_,ScalarFluxF,r,FType="Face",InterpType="CShep")

!!--end--
END SUBROUTINE



SUBROUTINE PRINT_GridEddingtonsC( &
  Mesh , &
  ExxC , EyyC , ExyC , &
  fdbk , &
  Unit )
!!#### PURPOSE
!! Output the cell Eddington factors on a grid.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh)      ,INTENT(IN) :: Mesh
REAL(KIND_ScalarFlux),INTENT(IN) :: ExxC(:,:)
REAL(KIND_ScalarFlux),INTENT(IN) :: EyyC(:,:)
REAL(KIND_ScalarFlux),INTENT(IN) :: ExyC(:,:)

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### OPTIONAL INPUT
INTEGER,OPTIONAL,INTENT(IN) :: Unit

!!#### LOCAL VARIABLES
INTEGER :: i,Unit_
REAL(KIND_MSH) :: r(1:NUM_Dimensions(Mesh),1:NUM_Cells(Mesh))
!!--begin--
Unit_ = DEFAULT(DEFAULT_OUTPUT_UNIT,Unit)

!get r(x,y)
DO i=1,SIZE(ExxC,2)
 r(:,i) = CellCentroid(Mesh,i)
END DO

!print various f(r)
WRITE(Unit_,"(a)")" * Eddington-xx-Cell"
CALL PRINT_Grid(Mesh,Unit_,ExxC,r,FType="Cell",InterpType="Const")

WRITE(Unit_,"(a)")" * Eddington-yy-Cell"
CALL PRINT_Grid(Mesh,Unit_,EyyC,r,FType="Cell",InterpType="Const")

WRITE(Unit_,"(a)")" * Eddington-xy-Cell"
CALL PRINT_Grid(Mesh,Unit_,ExyC,r,FType="Cell",InterpType="Const")

!!--end--
END SUBROUTINE






SUBROUTINE PRINT_GridEddingtonsF( Mesh,ExxF,EyyF,ExyF,&
  fdbk , Unit )
!!#### PURPOSE
!! Output the face Eddington factors on a grid.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh)      ,INTENT(IN) :: Mesh
REAL(KIND_ScalarFlux),INTENT(IN) :: ExxF(:,:)
REAL(KIND_ScalarFlux),INTENT(IN) :: EyyF(:,:)
REAL(KIND_ScalarFlux),INTENT(IN) :: ExyF(:,:)

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### OPTIONAL INPUT
INTEGER,OPTIONAL,INTENT(IN) :: Unit

!!#### LOCAL VARIABLES
REAL(KIND_MSH) :: r(1:NUM_Dimensions(Mesh),1:NUM_Faces(Mesh))
INTEGER        :: j,Unit_
!!--begin--
Unit_ = DEFAULT(DEFAULT_OUTPUT_UNIT,Unit)

!get r(x,y)
DO j=1,SIZE(ExxF,2)
 r(:,j) = FaceCentroid(Mesh,j)
END DO

!print various f(r)
WRITE(Unit_,"(a)")" * Eddington-xx-Face"
CALL PRINT_Grid(Mesh,Unit_,ExxF,r,FType="Face",InterpType="CShep")

WRITE(Unit_,"(a)")" * Eddington-yy-Face"
CALL PRINT_Grid(Mesh,Unit_,EyyF,r,FType="Face",InterpType="CShep")

WRITE(Unit_,"(a)")" * Eddington-xy-Face"
CALL PRINT_Grid(Mesh,Unit_,ExyF,r,FType="Face",InterpType="CShep")

!!--end--
END SUBROUTINE



SUBROUTINE PRINT_GridEddingtonsV( Mesh,ExxV,EyyV,ExyV,&
  fdbk , Unit )
!!#### PURPOSE
!! Output the face Eddington factors on a grid.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh)      ,INTENT(IN) :: Mesh
REAL(KIND_ScalarFlux),INTENT(IN) :: ExxV(:,:)
REAL(KIND_ScalarFlux),INTENT(IN) :: EyyV(:,:)
REAL(KIND_ScalarFlux),INTENT(IN) :: ExyV(:,:)

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### OPTIONAL INPUT
INTEGER,OPTIONAL,INTENT(IN) :: Unit

!!#### LOCAL VARIABLES
REAL(KIND_MSH) :: r(1:NUM_Dimensions(Mesh),1:NUM_Verts(Mesh))
INTEGER        :: k,Unit_
!!--begin--
Unit_ = DEFAULT(DEFAULT_OUTPUT_UNIT,Unit)

!get r(x,y)
DO k=1,SIZE(ExxV,2)
 r(:,k) = Vert(Mesh,k)
END DO

!print various f(r)
WRITE(Unit_,"(a)")" * Eddington-xx-Vert"
CALL PRINT_Grid(Mesh,Unit_,ExxV,r,FType="Vert",InterpType="CShep")

WRITE(Unit_,"(a)")" * Eddington-yy-Vert"
CALL PRINT_Grid(Mesh,Unit_,EyyV,r,FType="Vert",InterpType="CShep")

WRITE(Unit_,"(a)")" * Eddington-xy-Vert"
CALL PRINT_Grid(Mesh,Unit_,ExyV,r,FType="Vert",InterpType="CShep")

!!--end--
END SUBROUTINE



!!### PRINTING SUBROUTINE: <PRINT_GridScalarFluxV>
SUBROUTINE PRINT_GridScalarFluxV( Mesh , ScalarFluxV , &
  fdbk , Unit)

!!#### PURPOSE
!! Output the vertex scalar flux on a grid.

!!#### REQUIRED INPUT
REAL(KIND_ScalarFlux),INTENT(IN) :: ScalarFluxV(:,:)
TYPE(TYPE_Mesh)      ,INTENT(IN) :: Mesh

!!#### OPTIONAL INPUT
INTEGER,OPTIONAL,INTENT(IN) :: Unit

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
REAL(KIND_MSH) :: r(1:NUM_Dimensions(Mesh),1:NUM_Verts(Mesh))
INTEGER :: k,Unit_

!!--begin--
Unit_ = Default(DEFAULT_OUTPUT_UNIT,Unit)

!get r(x,y)
DO k=1,SIZE(r,2)
 r(:,k) = Vert(Mesh,k)
END DO

!print stuff
CALL PRINT_Grid(Mesh,Unit_,ScalarFluxV,r,FType="Vert",InterpType="CShep")

!!--end--
END SUBROUTINE


!!### PRINTING SUBROUTINE: <PRINT_ScalarFluxF>
SUBROUTINE PRINT_ScalarFluxF( &
  Mesh , &
  ScalarFluxF , &
  fdbk , &
  Unit , varname )

!!#### PURPOSE
!! Output face-average scalar flux.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh)      ,INTENT(IN) :: Mesh
REAL(KIND_ScalarFlux),INTENT(IN) :: ScalarFluxF(:,:)


!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### OPTIONAL INPUT
INTEGER     ,OPTIONAL,INTENT(IN) :: Unit
CHARACTER(*),OPTIONAL,INTENT(IN) :: varname

!!#### LOCAL VARIABLES
INTEGER :: g,j,d,Nj,Nc,Nr,Nd,c,r,Ng
CHARACTER(64),ALLOCATABLE :: Tab(:,:)
REAL(KIND_MSH),ALLOCATABLE :: FC(:)

!!--begin--

Nj = NUM_Faces(Mesh)
Nd = NUM_Dimensions(Mesh)
Ng = SIZE(ScalarFluxF,1)
Nc = 2 + Nd + 1
Nr = 1 + Nj*Ng

ALLOCATE( Tab(Nr,Nc) , FC(Nd) )
CALL CLEAR(Tab)

Tab(1,1:Nc-1) = &
(/"g        ",&
  "j        ",&
  "FCx(j)   ",&
  "FCy(j)   "/)
IF( PRESENT(varname) )THEN
 Tab(1,Nc) = TRIM(varname)//"(g,j)"
ELSE
 Tab(1,Nc) = "PhiF(g,j)"
END IF

r=2
DO g=1,SIZE(ScalarFluxF,1)
 DO j=1,SIZE(ScalarFluxF,2)
  c = 1

  Tab(r,c) = STR(g)
  c = c + 1

  Tab(r,c) = STR(j)
  c = c + 1

  FC = FaceCentroid(Mesh,j)
  DO d=1,Nd
   Tab(r,c) = STR(FC(d),"(Es21.14)")
   c = c + 1
  END DO

  Tab(r,c) = STR(ScalarFluxF(g,j),"(Es21.14)")

  r = r + 1

 END DO
END DO

CALL PRINT_Table(Tab,Unit=Unit)
DEALLOCATE( Tab , FC )

!!--end--
END SUBROUTINE




!!### PRINTING SUBROUTINE: <PRINT_ScalarFluxC>
SUBROUTINE PRINT_ScalarFluxC( &
  Mesh , &
  ScalarFluxC , &
  fdbk , &
  Unit , varname )

!!#### PURPOSE
!! Output cell-average scalar flux.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh)      ,INTENT(IN) :: Mesh
REAL(KIND_ScalarFlux),INTENT(IN) :: ScalarFluxC(:,:)


!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### OPTIONAL INPUT
INTEGER     ,OPTIONAL,INTENT(IN) :: Unit
CHARACTER(*),OPTIONAL,INTENT(IN) :: varname

!!#### LOCAL VARIABLES
INTEGER :: g,i,d,Ni,Nc,Nr,Nd,c,r,Ng
CHARACTER(64),ALLOCATABLE :: Tab(:,:)
REAL(KIND_MSH),ALLOCATABLE :: CC(:)

!!--begin--

Ni = NUM_Cells(Mesh)
Nd = NUM_Dimensions(Mesh)
Ng = SIZE(ScalarFluxC,1)
Nc = 2 + Nd + 1
Nr = 1 + Ni*Ng

ALLOCATE( Tab(Nr,Nc) , CC(Nd) )
CALL CLEAR(Tab)

Tab(1,1:Nc-1) = &
(/"g        ",&
  "i        ",&
  "CCx(j)   ",&
  "CCy(j)   "/)
IF( PRESENT(varname) )THEN
 Tab(1,Nc) = TRIM(varname)//"(g,i)"
ELSE
 Tab(1,Nc) = "PhiC(g,i)"
END IF

r=2
DO g=1,SIZE(ScalarFluxC,1)
 DO i=1,SIZE(ScalarFluxC,2)
  c = 1

  Tab(r,c) = STR(g)
  c = c + 1

  Tab(r,c) = STR(i)
  c = c + 1

  CC = CellCentroid(Mesh,i)
  DO d=1,Nd
   Tab(r,c) = STR(CC(d),"(Es21.14)")
   c = c + 1
  END DO

  Tab(r,c) = STR(ScalarFluxC(g,i),"(Es21.14)")

  r = r + 1

 END DO
END DO

CALL PRINT_Table(Tab,Unit=Unit)
DEALLOCATE( Tab , CC )

!!--end--
END SUBROUTINE


!!### PRINTING SUBROUTINE: <PRINT_ScalarFluxV>
SUBROUTINE PRINT_ScalarFluxV( &
  Mesh , &
  ScalarFluxV , &
  fdbk , &
  Unit , varname )

!!#### PURPOSE
!! Output face-average scalar flux.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh)      ,INTENT(IN) :: Mesh
REAL(KIND_ScalarFlux),INTENT(IN) :: ScalarFluxV(:,:)


!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### OPTIONAL INPUT
INTEGER     ,OPTIONAL,INTENT(IN) :: Unit
CHARACTER(*),OPTIONAL,INTENT(IN) :: varname

!!#### LOCAL VARIABLES
INTEGER :: g,k,d,Nk,Nc,Nr,Nd,c,r,Ng
CHARACTER(64),ALLOCATABLE :: Tab(:,:)
REAL(KIND_MSH),ALLOCATABLE :: V(:)

!!--begin--

Nk = NUM_Verts(Mesh)
Nd = NUM_Dimensions(Mesh)
Ng = SIZE(ScalarFluxV,1)
Nc = 2 + Nd + 1
Nr = 1 + Nk*Ng

ALLOCATE( Tab(Nr,Nc) , V(Nd) )
CALL CLEAR(Tab)

Tab(1,1:Nc-1) = &
(/"g        ",&
  "k        ",&
  "Vx(k)    ",&
  "Vy(k)    "/)
IF( PRESENT(varname) )THEN
 Tab(1,Nc) = TRIM(varname)//"(g,k)"
ELSE
 Tab(1,Nc) = "PhiV(g,k)"
END IF

r=2
DO g=1,SIZE(ScalarFluxV,1)
 DO k=1,SIZE(ScalarFluxV,2)
  c = 1

  Tab(r,c) = STR(g)
  c = c + 1

  Tab(r,c) = STR(k)
  c = c + 1

  V = Vert(Mesh,k)
  DO d=1,Nd
   Tab(r,c) = STR(V(d),"(Es21.14)")
   c = c + 1
  END DO

  Tab(r,c) = STR(ScalarFluxV(g,k),"(Es21.14)")

  r = r + 1

 END DO
END DO

CALL PRINT_Table(Tab,Unit=Unit)
DEALLOCATE( Tab , V )

!!--end--
END SUBROUTINE





!!### PRINTING SUBROUTINE: <PRINT_CurrentFN>
SUBROUTINE PRINT_CurrentFN(  &
  Mesh , &
  CurrentFN , &
  Fdbk , &
  Unit , varname )

!!#### PURPOSE
!! Output high-order, face-average current.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh)   ,INTENT(IN) :: Mesh
REAL(KIND_Current),INTENT(IN) :: CurrentFN(:,:)

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### OPTIONAL INPUT
INTEGER       ,OPTIONAL,INTENT(IN) :: Unit
CHARACTER(*)  ,OPTIONAL,INTENT(IN) :: varname

!!#### LOCAL VARIABLES
INTEGER :: g,j,d,Nj,Nc,Nr,Nd,c,r,Ng
CHARACTER(64),ALLOCATABLE :: Tab(:,:)
REAL(KIND_MSH),ALLOCATABLE :: FC(:),FN(:)

!!--begin--

Nj = NUM_Faces(Mesh)
Nd = NUM_Dimensions(Mesh)
Ng = SIZE(CurrentFN,1)
Nc = 2 + Nd*2 + 1
Nr = 1 + Nj*Ng

ALLOCATE( Tab(Nr,Nc) , FC(Nd) , FN(Nd) )
CALL CLEAR(Tab)

Tab(1,1:Nc-1) = &
(/"g       ",&
  "j       ",&
  "FCx(j)  ",&
  "FCy(j)  ",&
  "FNx(j)  ",&
  "FNy(j)  "/)
IF( PRESENT(varname) )THEN
 Tab(1,Nc)=TRIM(varname)//"(g,j)"
ELSE
 Tab(1,Nc)="JFN(g,j)"
END IF

r=2
DO g=1,SIZE(CurrentFN,1)
 DO j=1,SIZE(CurrentFN,2)
  c = 1

  Tab(r,c) = STR(g)
  c = c + 1

  Tab(r,c) = STR(j)
  c = c + 1

  FC = FaceCentroid(Mesh,j)
  DO d=1,Nd
   Tab(r,c) = STR(FC(d),"(Es21.14)")
   c = c + 1
  END DO

  FN = FaceNormal(Mesh,j)
  DO d=1,Nd
   Tab(r,c) = STR(FN(d),"(Es21.14)")
   c = c + 1
  END DO

  Tab(r,c) = STR(CurrentFN(g,j),"(Es21.14)")

  r = r + 1

 END DO
END DO

CALL PRINT_Table(Tab,Unit=Unit)
DEALLOCATE( Tab , FC , FN )

!!--end--
END SUBROUTINE



SUBROUTINE PRINT_convergence_flow(Mesh,label, sums , fdbk ,varname)
!!#### PURPOSE
!! Output a convergence summary for region-average
!! scalar fluxes.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
CHARACTER(*),INTENT(IN) :: label
REAL(KIND_MSH),INTENT(IN) :: sums(1:3)

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk
CHARACTER(*),INTENT(IN),OPTIONAL :: varname

!!#### LOCAL VARIABLES
INTEGER :: Unit_,r,n,nr
CHARACTER(32),ALLOCATABLE :: D(:,:)

!!--begin--

IF( .NOT.Exist("convergence.flow") )THEN
 nr = 2
ELSE
 nr = 1
END IF
!get the unit to output to
Unit_ = NewFile( "convergence.flow" , Status="Unknown" , Position="Append"  , IfOpened="Close" )


!allocate the table
ALLOCATE( D(nr,6) )

!clear the table
CALL CLEAR(D)

!make header
IF( nr==2 )THEN
 D(1,1:3) = (/"TimeStamp",&
              "ProbName ",&
              "NUM_Faces"/)
 D(1,4) = "Area"
 D(1,5) = "JFN"
 D(1,6) = "Flow"
END IF

!make table body
D(nr,1) = TimestampM()
IF( PRESENT(varname) )THEN
 D(nr,2) = TRIM(varname)//"-"//TRIM(label)
ELSE
 D(nr,2) = label
END IF
D(nr,3) = STR( NUM_Faces(Mesh) , "(I)" )
D(nr,4) = STR( sums(1) , "(Es21.14)" )
D(nr,5) = STR( sums(2) , "(Es21.14)" )
D(nr,6) = STR( sums(3) , "(Es21.14)" )

!print the table
IF( Nr==2 )THEN
 CALL PRINT_Table(D,Unit=Unit_,&
   printseparator=(/.TRUE., .TRUE.,.FALSE./),&
   usefixedwidth=SPREAD(.TRUE.,1,SIZE(D,2)),&
   CenterFirstRow=.TRUE. )
ELSE
 CALL PRINT_Table(D,Unit=Unit_,&
   printseparator=(/.FALSE., .FALSE.,.FALSE./),&
   usefixedwidth=SPREAD(.TRUE.,1,SIZE(D,2)),&
   CenterFirstRow=.FALSE. )
END IF

DEALLOCATE( D )

!!--end--
END SUBROUTINE




SUBROUTINE PRINT_Current_Exiting(Mesh,label,exit_r1,exit_r2,CurrentFN,&
 fdbk,Unit,varname)
!!#### PURPOSE
!! Print out exiting current for defined boundary <CurrentFN>.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh)          ,INTENT(IN) :: Mesh
CHARACTER(*)             ,INTENT(IN) :: label
REAL(KIND_MSH)           ,INTENT(IN) :: exit_r1(:),exit_r2(:)
REAL(KIND_AngularFlux)   ,INTENT(IN) :: CurrentFN(:,:)

!!#### OPTIONAL INPUT
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk
INTEGER,INTENT(IN),OPTIONAL :: Unit
CHARACTER(*),INTENT(IN),OPTIONAL :: varname

!!#### LOCAL VARIABLES
INTEGER :: Nd,d,j,Nj,Ng,g,Unit_,row
CHARACTER(64),POINTER :: Tab(:,:)
REAL(KIND_MSH),POINTER :: r(:)
REAL(KIND_MSH) :: sum1,sum2,sum3,val
LOGICAL,PARAMETER :: Noisy_=.FALSE.

!!--begin--
Unit_ = DEFAULT(DEFAULT_OUTPUT_UNIT,Unit)

Nd = NUM_Dimensions(Mesh)
Ng = SIZE(CurrentFN,1)
Nj = NUM_Faces(Mesh)

ALLOCATE( Tab(1+Ng*Nj+1,nd+4) , r(nd) )
CALL CLEAR(Tab)
Tab(1,1) = "g"
IF( nd>=1 )Tab(1,2) = "x"
IF( nd>=2 )Tab(1,3) = "y"
IF( nd>=3 )Tab(1,4) = "z"
Tab(1,1+nd+1) = "AreaF"
Tab(1,1+nd+2) = "CurrentFN"
Tab(1,1+nd+3) = "Flow"

sum1 = 0._KIND_MSH
sum2 = 0._KIND_MSH
sum3 = 0._KIND_MSH
row = 2
DO g=1,Ng

 !Add face currents for this energy group
 DO j=1,Nj

  IF( .NOT.IsBoundaryFace(Mesh,j) )CYCLE

  r = FaceCentroid(Mesh,j)

  !special face consideration criteria
  DO d=1,nd
   IF( .NOT.( (exit_r1(d)<=r(d) .OR. IsApprox(exit_r1(d),r(d)) ) .AND. &
              (r(d)<=exit_r2(d) .OR. IsApprox(exit_r2(d),r(d)) )       )  )THEN
        IF( Noisy_)WRITE(*,*)"Face j="//TRIM(STR(j))//" is NOT an outgoing face!"
    GOTO 111
   END IF
  END DO
  IF( Noisy_)WRITE(*,*)"Face j="//TRIM(STR(j))//" is an outgoing face!"


  Tab(row,1) = STR(g)

  DO d=1,nd
   Tab(row,1+d) = STR(r(d),"(Es21.14)")
  END DO

  !put the face area value into a dummy to sum and stringify
  val = FaceArea(Mesh,j)
  sum1 = sum1 + val
  Tab(row,1+nd+1) = STR(val,"(Es21.14)")

  !put the face current value into a dummy to sum and stringify
  val = CurrentFN(g,j)
  Tab(row,1+nd+2) = STR(val,"(Es21.14)")

  !put the integral into a dummy to sum and stringify
  val = CurrentFN(g,j)*FaceArea(Mesh,j)
  sum3 = sum3 + val
  Tab(row,1+nd+3) = STR(val,"(Es21.14)")

  row = row + 1

  111 CONTINUE
 END DO

END DO

!Put final summation row in.
Tab(row,1:nd+1) = "-"
Tab(row,1+nd+1) = STR(sum1,"(Es21.14)")
sum2 = sum3/sum1
Tab(row,1+nd+2) = STR(sum2,"(Es21.14)")
Tab(row,1+nd+3) = STR(sum3,"(Es21.14)")

CALL PRINT_Table( Tab(1:row,:) , Unit=Unit_ )

DEALLOCATE( Tab , r )

CALL PRINT_convergence_flow( Mesh , label  , (/sum1,sum2,sum3/) , &
  varname=varname)

!!--end--
END SUBROUTINE




SUBROUTINE PRINT_NuclearData( Mesh , MacS , MacT , l_ , &
  ExtSourceCellFunction , &
  fdbk , Unit )
!!#### PURPOSE
!! Output nuclear data.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh)  ,INTENT(IN) :: Mesh
REAL(KIND_Mac)   ,INTENT(IN) :: MacS(:,:),MacT(:,:)
INTEGER          ,INTENT(IN) :: l_(:)
REAL(KIND_Source),INTENT(IN) :: ExtSourceCellFunction(:,:,:)

!!#### OPTIONAL INPUT
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### OPTIONAL INPUT
INTEGER,OPTIONAL,INTENT(IN) :: Unit

!!#### LOCAL VARIABLES
CHARACTER(32),ALLOCATABLE :: D(:,:)
INTEGER :: NRows,NCols,r,g,i,Ni,Unit_,Ng
REAL(KIND_AngularFlux) :: AvgSource

!!--begin--
!rows and columns
Unit_ = DEFAULT( DEFAULT_OUTPUT_UNIT , Unit )

Ni = NUM_Cells(Mesh)
Ng = SIZE(MacT,1)

NRows = Ni*Ng + 1
NCols = 4 + 1

!allocate and clear
ALLOCATE( D(NRows,NCols) )
CALL CLEAR(D)

!Set Columns
D(1,1) = "g"
D(1,2) = "i"
D(1,3) = "MacS(g,i)"
D(1,4) = "MacT(g,i)"
D(1,5) = "Qavg(g,i)"

!Set the Data
r = 1
DO g=1,Ng
 DO i=1,Ni
  r = r + 1
  D(r,1) = STR(g)
  D(r,2) = STR(i)
  D(r,3) = STR( MacS(g,l_(i)),"(Es21.14)")
  D(r,4) = STR( MacT(g,l_(i)),"(Es21.14)")
  AvgSource = EVAL_SourceAverage(Mesh,i,ExtSourceCellFunction(:,g,i))
  D(r,5) = STR(c_4_times_PI*AvgSource,"(Es21.14)")
 END DO
END DO

CALL PRINT_Table(D,Unit=Unit_)

!!--end--
END SUBROUTINE


SUBROUTINE PRINT_timetable( TAP_what , &
  TAP_time , fdbk , Unit )
!!#### PURPOSE
!! Output a time table.

!!#### REQUIRED INPUT
CHARACTER(*),POINTER :: TAP_what(:)
REAL        ,POINTER :: TAP_time(:)

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### OPTIONAL INPUT
INTEGER,OPTIONAL,INTENT(IN) :: Unit

!!#### LOCAL VARIABLES
INTEGER :: Unit_,u,Nu
CHARACTER(32),ALLOCATABLE :: D(:,:)

!!--begin--
IF( .NOT.ASSOCIATED(TAP_what) )RETURN

!finalize the list
CALL FINALIZE_LIST(TAP_what)
Nu = SIZE(TAP_what)

!get the unit to output to
Unit_ = DEFAULT( DEFAULT_OUTPUT_UNIT , Unit )

!allocate the table
ALLOCATE( D(1+Nu,2) )

!clear the table
CALL CLEAR(D)

!make header
D(1,1:2) = (/"Job      ","WallClock"/)

!start filling it in
DO u=1,Nu

 D(1+u,1) = TAP_what(u)
 D(1+u,2) = STR(TAP_time(u),"(F8.4)")

END DO

!print the table
CALL PRINT_Table(D,Unit=Unit_)

DEALLOCATE( D )

!!--end--
END SUBROUTINE


SUBROUTINE PRINT_timesummary( dt , fdbk , Unit )
!!#### PURPOSE
!! Output a time summary.

!!#### REQUIRED INPUT
REAL,INTENT(IN) :: dt(1:7)

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### OPTIONAL INPUT
INTEGER,OPTIONAL,INTENT(IN) :: Unit

!!#### LOCAL VARIABLES
INTEGER :: Unit_
CHARACTER(32),ALLOCATABLE :: D(:,:)

!!--begin--
!get the unit to output to
Unit_ = DEFAULT( DEFAULT_OUTPUT_UNIT , Unit )

!allocate the table
ALLOCATE( D(12,2) )

!clear the table
CALL CLEAR(D)

!make header
D(1,1:2) = (/"Job ","Time"/)

!make table body
D(02,1) = "Input"            ; D(02,2) = STRTIME(dt(1))
D(03,1) = "Setup"            ; D(03,2) = STRTIME(dt(2))
D(04,1) = "EchoInput"        ; D(04,2) = STRTIME(dt(3))
D(05,1) = "SolveHO"          ; D(05,2) = STRTIME(dt(4))
D(06,1) = "Refine"           ; D(06,2) = STRTIME(dt(5))
D(07,1) = "SolveLO"          ; D(07,2) = STRTIME(dt(6))
D(08,1) = "Print"            ; D(08,2) = STRTIME(dt(7))
D(09,1) = "TOTAL INITIALIZE" ; D(09,2) = STRTIME( SUM(dt(1:2)) )
D(10,1) = "TOTAL SOLVING"    ; D(10,2) = STRTIME( SUM(dt(4:6)) )
D(11,1) = "TOTAL PRINTING"   ; D(11,2) = STRTIME( dt(3)+dt(7)  )
D(12,1) = "GRAND TOTAL"      ; D(12,2) = STRTIME( SUM(dt)      )

!print the table
CALL PRINT_Table(D,Unit=Unit_)

DEALLOCATE( D )

!!--end--
END SUBROUTINE



SUBROUTINE PRINT_KEYS( KEYS , fdbk , Unit )
!!#### PURPOSE
!! Output a list of keys.

!!#### REQUIRED INPUT
CHARACTER(*),INTENT(IN) :: KEYS(:)

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### OPTIONAL INPUT
INTEGER,OPTIONAL,INTENT(IN) :: Unit

!!#### LOCAL VARIABLES
INTEGER :: Unit_,i

!!--begin--
!get the unit to output to
Unit_ = DEFAULT( DEFAULT_OUTPUT_UNIT , Unit )

DO i=1,SIZE(KEYS)
 WRITE(Unit_,"(10x,A)")KEYS(i)
END DO

!!--end--
END SUBROUTINE



SUBROUTINE PRINT_InputFile( InputFile , fdbk , Unit )
!!#### PURPOSE
!! Print the input file stripped of comment lines and blank lines.

!!#### REQUIRED INPUT
CHARACTER(*),INTENT(IN) :: InputFile

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### OPTIONAL INPUT
INTEGER,OPTIONAL,INTENT(IN) :: Unit

!!#### LOCAL VARIABLES
INTEGER :: Unit_,i,l,Nl
INTEGER :: TempUnit,InpUnit,iostat
TYPE(varying_string) :: VS
LOGICAL :: IsComment,IsBlank

!!--begin--
!get the unit to output to
Unit_ = DEFAULT( DEFAULT_OUTPUT_UNIT , Unit )

!dump input file with all other files include
IF( EXIST("PRINT_InputFile.temp") )THEN
 CALL DeleteFile("PRINT_InputFile.temp")
END IF

TempUnit = NewFile("PRINT_InputFile.temp")
WRITE(TempUnit,"(a)")"  "
!don't know why the above WRITE line is needed for the
!intel linux fortran compiler 10.1 to execute this routine
!properly---probably a bug that will be fixed eventually

InpUnit = OldFile(TRIM(STR(InputFile)))

CALL IncludeFiles(InpUnit,TempUnit)

!read in the temp and only echo nonblank/noncomment lines
REWIND(TempUnit)
Nl = COUNT_Lines(TempUnit)
REWIND(TempUnit)

DO l=1,Nl
 CALL get(TempUnit,VS,iostat=iostat)
 IF( iostat==END_OF_FILE )EXIT
 IsBlank = TRIM(ADJUSTL(VS))==''
 IsComment = Extract(ADJUSTL(VS),1,1)=="!"
 IF( .NOT.( IsComment .OR. IsBlank ) )THEN
  CALL put_line(Unit_,VS)
 END IF
END DO

CLOSE(TempUnit)
CALL DeleteFile("PRINT_InputFile.temp")

!!--end--
END SUBROUTINE



SUBROUTINE PRINT_ScalarFlux_CellRegions(Mesh,label,IntegralRegions,&
  ScalarFluxC,fdbk,Unit,varname)
!!#### PURPOSE
!! Print out integral regions info for <ScalarFluxC>.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh)          ,INTENT(IN) :: Mesh
CHARACTER(*)             ,INTENT(IN) :: label
TYPE(TYPE_IntegralRegion),POINTER    :: IntegralRegions(:)
REAL(KIND_AngularFlux)   ,INTENT(IN) :: ScalarFluxC(:,:)

!!#### OPTIONAL INPUT
INTEGER,INTENT(IN),OPTIONAL :: Unit
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk
CHARACTER(*),INTENT(IN),OPTIONAL :: varname

!!#### LOCAL VARIABLES
INTEGER :: Ng,Nr,g,Unit_
CHARACTER(10),ALLOCATABLE :: MoreIndices(:,:)

!!--begin--
Unit_ = DEFAULT(DEFAULT_OUTPUT_UNIT,Unit)

Nr = SIZE(IntegralRegions)
Ng = SIZE(ScalarFluxC,1)

ALLOCATE( MoreIndices(0:Nr,1) )
MoreIndices(0,1) = "g"

DO g=1,Ng
 MoreIndices(1:,1) = STR(g)
 CALL UPDATE_IntegralRegions( Mesh , IntegralRegions , ScalarFluxC(g,:) )
 CALL PRINT_IntegralRegions( IntegralRegions , Unit=Unit_ , MoreIndices=MoreIndices,&
   PrintSets=.FALSE.)
END DO

DEALLOCATE( MoreIndices )

CALL PRINT_convergence_phi( Mesh,label,IntegralRegions , fdbk , &
  varname=varname)

!!--end--
END SUBROUTINE


SUBROUTINE PRINT_convergence_phi( Mesh,label,IntegralRegions,fdbk ,varname)
!!#### PURPOSE
!! Output a convergence summary for region-average
!! scalar fluxes.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
CHARACTER(*),INTENT(IN) :: Label
TYPE(TYPE_IntegralRegion),POINTER :: IntegralRegions(:)
CHARACTER(*),INTENT(IN),OPTIONAL :: varname

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
INTEGER :: Unit_,r,n,nr
CHARACTER(32),ALLOCATABLE :: D(:,:)
REAL(KIND_MSH) :: vavg

!!--begin--
IF( .NOT.ASSOCIATED(IntegralRegions) )RETURN
IF( .NOT.Exist("convergence.phi") )THEN
 nr = 2
ELSE
 nr = 1
END IF
!get the unit to output to
Unit_ = NewFile( "convergence.phi" , Status="Unknown" , Position="Append" , IfOpened="Close" )


!allocate the table
ALLOCATE( D(nr,3+SIZE(IntegralRegions)) )

!clear the table
CALL CLEAR(D)

!make header
IF( nr==2 )THEN
 D(1,1:3) = (/"TimeStamp",&
              "ProbName ",&
              "NUM_Cells"/)
 DO r=1,SIZE(IntegralRegions)
  D(1,3+r) = "Phi-"//TRIM(IntegralRegions(r)%Label)
 END DO
END IF

!make table body
D(nr,1) = TimestampM()
IF( PRESENT(varname) )THEN
 D(nr,2) = TRIM(varname)//"-"//TRIM(label)
ELSE
 D(nr,2) = label
END IF
D(nr,3) = STR( NUM_Cells(Mesh) , "(I)" )
DO r=1,SIZE(IntegralRegions)
 D(1,3+r) = IntegralRegions(r)%Label
 !Print out only the average
 vavg =  IntegralRegions(r)%integrals(2)/&
        (IntegralRegions(r)%integrals(1)+1.d-20)
 D(nr,3+r) = STR( vavg , "(Es21.14)" )
END DO


!print the table
IF( Nr==2 )THEN
 CALL PRINT_Table(D,Unit=Unit_,&
   printseparator=(/.TRUE., .TRUE.,.FALSE./),&
   usefixedwidth=SPREAD(.TRUE.,1,SIZE(D,2)),&
   CenterFirstRow=.TRUE. )
ELSE
 CALL PRINT_Table(D,Unit=Unit_,&
   printseparator=(/.FALSE., .FALSE.,.FALSE./),&
   usefixedwidth=SPREAD(.TRUE.,1,SIZE(D,2)),&
   CenterFirstRow=.FALSE. )
END IF

DEALLOCATE( D )

!!--end--
END SUBROUTINE



!!### SUBROUTINE <<PRINT_AngularFluxV_gen>>
SUBROUTINE PRINT_AngularFluxV_gen( &
  Mesh , &
  Ordinates,Weights,AngularFluxV , &
  fdbk , &
  Unit )
!!#### PURPOSE
!! Output angular flux on vertices.

!!#### REQUIRED INPUT
REAL(KIND_AngularFlux),INTENT(IN) :: Ordinates(:,:),Weights(:)
REAL(KIND_AngularFlux),INTENT(IN) :: AngularFluxV(:,:,:)
TYPE(TYPE_Mesh)       ,INTENT(IN) :: Mesh

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### OPTIONAL INPUT
INTEGER,OPTIONAL,INTENT(IN) :: Unit

!!#### LOCAL VARIABLES
INTEGER :: g,k,Ng,Nk,n,m,Nm,Unit_
CHARACTER(32),POINTER :: D(:,:)
REAL(KIND_MSH) :: V(2),Azi,Pol,origin(2)

!!--begin--
Unit_ = DEFAULT(DEFAULT_OUTPUT_UNIT,Unit)
Ng = SIZE(AngularFluxV,1)
Nk = SIZE(AngularFluxV,2)
Nm = SIZE(AngularFluxV,3)

!!clear the pointer space and allocate
CALL CLEARn(D)
ALLOCATE( D(0:Nm*Nk*Ng,9) )

!!clear the character space
CALL CLEAR(D)

!!setup labels
D(0,1:8) = (/"g         ",&
             "k         ",&
             "m         ",&
             "Vx(k)     ",&
             "Vy(k)     ",&
             "Azi(m)    ",&
             "Pol(m)    ",&
             "Wt(m)     "/)
D(0,9) = "PsiV(g,k,m)"

n = 0
DO m=1,Nm
 DO k=1,Nk
  DO g=1,Ng
   n = n + 1
   D(n,1) = TRIM(STR(g))
   D(n,2) = TRIM(STR(k))
   D(n,3) = TRIM(STR(m))
   V = Vert(Mesh,k)
   D(n,4) = TRIM(STR(V(1),"(Es10.3)"))
   D(n,5) = TRIM(STR(V(2),"(Es10.3)"))

   D(n,6) = TRIM(STR(AziAngle(Ordinates(:,m)),"(f4.1)"))
   D(n,7) = TRIM(STR(PolAngle(Ordinates(:,m)),"(f4.1)"))

   D(n,8) = TRIM(STR(Weights(m),"(f4.1)"))

   D(n,9) = TRIM(STR(AngularFluxV(g,k,m),"(Es21.14)"))
  END DO
 END DO
END DO

CALL Print_Table(D,Unit=Unit_)

CALL CLEARn(D)

!!--end--
END SUBROUTINE



!!### SUBROUTINE <<PRINT_AngularFluxF_gen>>
SUBROUTINE PRINT_AngularFluxF_gen( &
  Mesh , &
  Ordinates,Weights,&
  AngularFluxF , &
  fdbk , &
  Unit )
!!#### PURPOSE
!! Output angular flux on faces.

!!#### REQUIRED INPUT
REAL(KIND_AngularFlux),INTENT(IN) :: Ordinates(:,:),Weights(:)
REAL(KIND_AngularFlux),INTENT(IN) :: AngularFluxF(:,:,:)
TYPE(TYPE_Mesh)       ,INTENT(IN) :: Mesh

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### OPTIONAL INPUT
INTEGER,OPTIONAL,INTENT(IN) :: Unit

!!#### LOCAL VARIABLES
INTEGER :: g,j,Ng,Nj,n,m,Nm,Unit_
CHARACTER(32),POINTER :: D(:,:)
REAL(KIND_MSH) :: FC(2),Azi,Pol,origin(2)

!!--begin--
Unit_ = DEFAULT(DEFAULT_OUTPUT_UNIT,Unit)
Ng = SIZE(AngularFluxF,1)
Nj = SIZE(AngularFluxF,2)
Nm = SIZE(AngularFluxF,3)

!!clear the pointer space and allocate
CALL CLEARn(D)
ALLOCATE( D(0:Nm*Nj*Ng,9) )

!!clear the character space
CALL CLEAR(D)

!!setup labels
D(0,1:8) = (/"g         ",&
             "k         ",&
             "m         ",&
             "FCx(k)    ",&
             "FCy(k)    ",&
             "Azi(m)    ",&
             "Pol(m)    ",&
             "Wt(m)     "/)
D(0,9) = "PsiF(g,j,m)"

n = 0
DO m=1,Nm
 DO j=1,Nj
  DO g=1,Ng
   n = n + 1
   D(n,1) = TRIM(STR(g))
   D(n,2) = TRIM(STR(j))
   D(n,3) = TRIM(STR(m))
   FC = FaceCentroid(Mesh,j)
   D(n,4) = TRIM(STR(FC(1),"(Es10.3)"))
   D(n,5) = TRIM(STR(FC(2),"(Es10.3)"))

   D(n,6) = TRIM(STR(AziAngle(Ordinates(:,m)),"(f4.1)"))
   D(n,7) = TRIM(STR(PolAngle(Ordinates(:,m)),"(f4.1)"))

   D(n,8) = TRIM(STR(Weights(m),"(f4.1)"))

   D(n,9) = TRIM(STR(AngularFluxF(g,j,m),"(Es21.14)"))
  END DO
 END DO
END DO

CALL Print_Table(D,Unit=Unit_)

CALL CLEARn(D)

!!--end--
END SUBROUTINE



!!### SUBROUTINE <<PRINT_AngularFluxC_gen>>
SUBROUTINE PRINT_AngularFluxC_gen( &
  Mesh , &
  Ordinates,Weights,&
  AngularFluxC , &
  fdbk , &
  Unit )
!!#### PURPOSE
!! Output angular flux on cells.

!!#### REQUIRED INPUT
REAL(KIND_AngularFlux),INTENT(IN) :: Ordinates(:,:),Weights(:)
REAL(KIND_AngularFlux),INTENT(IN) :: AngularFluxC(:,:,:)
TYPE(TYPE_Mesh)       ,INTENT(IN) :: Mesh

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### OPTIONAL INPUT
INTEGER,OPTIONAL,INTENT(IN) :: Unit

!!#### LOCAL VARIABLES
INTEGER :: g,i,Ng,Ni,n,m,Nm,Unit_
CHARACTER(32),POINTER :: D(:,:)
REAL(KIND_MSH) :: CC(2),Azi,Pol,origin(2)

!!--begin--
Unit_ = DEFAULT(DEFAULT_OUTPUT_UNIT,Unit)
Ng = SIZE(AngularFluxC,1)
Ni = SIZE(AngularFluxC,2)
Nm = SIZE(AngularFluxC,3)

!!clear the pointer space and allocate
CALL CLEARn(D)
ALLOCATE( D(0:Nm*Ni*Ng,9) )

!!clear the character space
CALL CLEAR(D)

!!setup labels
D(0,1:8) = (/"g         ",&
             "k         ",&
             "m         ",&
             "CCx(i)    ",&
             "CCy(i)    ",&
             "Azi(m)    ",&
             "Pol(m)    ",&
             "Wt(m)     "/)
D(0,9) = "PsiC(g,i,m)"

n = 0
DO m=1,Nm
 DO i=1,Ni
  DO g=1,Ng
   n = n + 1
   D(n,1) = TRIM(STR(g))
   D(n,2) = TRIM(STR(i))
   D(n,3) = TRIM(STR(m))
   CC = CellCentroid(Mesh,i)
   D(n,4) = TRIM(STR(CC(1),"(Es10.3)"))
   D(n,5) = TRIM(STR(CC(2),"(Es10.3)"))

   D(n,6) = TRIM(STR(AziAngle(Ordinates(:,m)),"(f4.1)"))
   D(n,7) = TRIM(STR(PolAngle(Ordinates(:,m)),"(f4.1)"))

   D(n,8) = TRIM(STR(Weights(m),"(f4.1)"))

   D(n,9) = TRIM(STR(AngularFluxC(g,i,m),"(Es21.14)"))
  END DO
 END DO
END DO

CALL Print_Table(D,Unit=Unit_)

CALL CLEARn(D)

!!--end--
END SUBROUTINE



!!### SUBROUTINE <<PRINT_BC>>
SUBROUTINE PRINT_BC( Mesh , BC , KEY_BC , fdbk , Unit )
!!#### PURPOSE
!! Print out boundary condition information.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER        ,INTENT(IN) :: BC(:)
CHARACTER(*)   ,INTENT(IN) :: KEY_BC(:)

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: FdBK

!!#### OPTIONAL INPUT
INTEGER,INTENT(IN),OPTIONAL :: Unit

!!#### LOCAL VARIABLES
INTEGER :: Unit_,jb

!!--begin--
!! Determine output unit.
IF( PRESENT(Unit) )THEN
 Unit_ = Unit
ELSE
 Unit_ = OutputUnit(fdbk)
END IF

!! Print boundary condition information.
DO jb=1,SIZE(Mesh%domain%faces)
 WRITE(Unit_,"(a,2x,i8.5 )")" Boundary-Face no.",jb
 WRITE(Unit_,"(a,2x,    a)")"  BC            : ",KEY_BC(BC(jb))
 WRITE(Unit_,"(a,2x,3f8.4)")"  Outward Normal: ",Mesh%domain%faces(jb)%FaceCoeff
 WRITE(Unit_,"(a,2x,3f8.4)")"  Centroid      : ",Mesh%domain%faces(jb)%FaceCentroid
 WRITE(Unit_,"(a,2x,1f8.4)")"  Area          : ",Mesh%domain%faces(jb)%FaceArea
END DO

!!--end--
END SUBROUTINE



!!### PRINTING SUBROUTINE: <PRINT_CurrentF>
SUBROUTINE PRINT_CurrentF( &
  Mesh , &
  CurrentF , &
  fdbk , &
  Unit , varname )

!!#### PURPOSE
!! Output face-average current.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh)      ,INTENT(IN) :: Mesh
REAL(KIND_Current),INTENT(IN) :: CurrentF(:,:,:)


!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### OPTIONAL INPUT
INTEGER     ,OPTIONAL,INTENT(IN) :: Unit
CHARACTER(*),OPTIONAL,INTENT(IN) :: varname

!!#### LOCAL VARIABLES
INTEGER :: g,j,d,Nj,Nc,Nr,Nd,c,r,Ng
CHARACTER(64),ALLOCATABLE :: Tab(:,:)
REAL(KIND_MSH),ALLOCATABLE :: FC(:)
CHARACTER,PARAMETER :: axes(3)=(/"x","y","z"/)

!!--begin--

Nj = NUM_Faces(Mesh)
Nd = NUM_Dimensions(Mesh)
Ng = SIZE(CurrentF,2)
Nc = 2 + 2*Nd
Nr = 1 + Nj*Ng

ALLOCATE( Tab(Nr,Nc) , FC(Nd) )
CALL CLEAR(Tab)

Tab(1,1:Nc-Nd) = &
(/"g        ",&
  "j        ",&
  "FCx(j)   ",&
  "FCy(j)   "/)

IF( PRESENT(varname) )THEN
 DO d=1-Nd,0
  Tab(1,Nc+d) = TRIM(varname)//axes(d+Nd)//"(g,j)"
 END DO
ELSE
 DO d=1-Nd,0
  Tab(1,Nc+d) = "JF"//axes(d+Nd)//"(g,j)"
 END DO
END IF

r=2
DO g=1,SIZE(CurrentF,2)
 DO j=1,SIZE(CurrentF,3)
  c = 1

  Tab(r,c) = STR(g)
  c = c + 1

  Tab(r,c) = STR(j)
  c = c + 1

  FC = FaceCentroid(Mesh,j)
  DO d=1,Nd
   Tab(r,c) = STR(FC(d),"(Es21.14)")
   c = c + 1
  END DO

  DO d=1,Nd
   Tab(r,c) = STR(CurrentF(d,g,j),"(Es21.14)")
   c=c+1
  END DO

  r = r + 1

 END DO
END DO

CALL PRINT_Table(Tab,Unit=Unit)
DEALLOCATE( Tab , FC )

!!--end--
END SUBROUTINE




!!### PRINTING SUBROUTINE: <PRINT_CurrentC>
SUBROUTINE PRINT_CurrentC( &
  Mesh , &
  CurrentC , &
  fdbk , &
  Unit , varname )

!!#### PURPOSE
!! Output cell-average current.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh)   ,INTENT(IN) :: Mesh
REAL(KIND_Current),INTENT(IN) :: CurrentC(:,:,:)


!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### OPTIONAL INPUT
INTEGER     ,OPTIONAL,INTENT(IN) :: Unit
CHARACTER(*),OPTIONAL,INTENT(IN) :: varname

!!#### LOCAL VARIABLES
INTEGER :: g,i,d,Ni,Nc,Nr,Nd,c,r,Ng
CHARACTER(64),ALLOCATABLE :: Tab(:,:)
REAL(KIND_MSH),ALLOCATABLE :: CC(:)
CHARACTER,PARAMETER :: axes(3)=(/"x","y","z"/)

!!--begin--

Ni = NUM_Cells(Mesh)
Nd = NUM_Dimensions(Mesh)
Ng = SIZE(CurrentC,2)
Nc = 2 + 2*Nd
Nr = 1 + Ni*Ng

ALLOCATE( Tab(Nr,Nc) , CC(Nd) )
CALL CLEAR(Tab)

Tab(1,1:Nc-Nd) = &
(/"g        ",&
  "j        ",&
  "CCx(j)   ",&
  "CCy(j)   "/)

IF( PRESENT(varname) )THEN
 DO d=1-Nd,0
  Tab(1,Nc+d) = TRIM(varname)//axes(d+Nd)//"(g,i)"
 END DO
ELSE
 DO d=1-Nd,0
  Tab(1,Nc+d) = "JC"//axes(d+Nd)//"(g,i)"
 END DO
END IF

r=2
DO g=1,SIZE(CurrentC,2)
 DO i=1,SIZE(CurrentC,3)
  c = 1

  Tab(r,c) = STR(g)
  c = c + 1

  Tab(r,c) = STR(i)
  c = c + 1

  CC = CellCentroid(Mesh,i)
  DO d=1,Nd
   Tab(r,c) = STR(CC(d),"(Es21.14)")
   c = c + 1
  END DO

  DO d=1,Nd
   Tab(r,c) = STR(CurrentC(d,g,i),"(Es21.14)")
   c=c+1
  END DO

  r = r + 1

 END DO
END DO

CALL PRINT_Table(Tab,Unit=Unit)
DEALLOCATE( Tab , CC )

!!--end--
END SUBROUTINE


!!### PRINTING SUBROUTINE: <PRINT_CurrentV>
SUBROUTINE PRINT_CurrentV( &
  Mesh , &
  CurrentV , &
  fdbk , &
  Unit , varname )

!!#### PURPOSE
!! Output face-average current.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh)      ,INTENT(IN) :: Mesh
REAL(KIND_Current),INTENT(IN) :: CurrentV(:,:,:)


!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### OPTIONAL INPUT
INTEGER     ,OPTIONAL,INTENT(IN) :: Unit
CHARACTER(*),OPTIONAL,INTENT(IN) :: varname

!!#### LOCAL VARIABLES
INTEGER :: g,k,d,Nk,Nc,Nr,Nd,c,r,Ng
CHARACTER(64),ALLOCATABLE :: Tab(:,:)
REAL(KIND_MSH),ALLOCATABLE :: V(:)
CHARACTER,PARAMETER :: axes(3)=(/"x","y","z"/)

!!--begin--

Nk = NUM_Verts(Mesh)
Nd = NUM_Dimensions(Mesh)
Ng = SIZE(CurrentV,2)
Nc = 2 + 2*Nd
Nr = 1 + Nk*Ng

ALLOCATE( Tab(Nr,Nc) , V(Nd) )
CALL CLEAR(Tab)

Tab(1,1:Nc-Nd) = &
(/"g        ",&
  "k        ",&
  "Vx(k)    ",&
  "Vy(k)    "/)

IF( PRESENT(varname) )THEN
 DO d=1-Nd,0
  Tab(1,Nc+d) = TRIM(varname)//axes(d+Nd)//"(g,k)"
 END DO
ELSE
 DO d=1-Nd,0
  Tab(1,Nc+d) = "JV"//axes(d+Nd)//"(g,k)"
 END DO
END IF

r=2
DO g=1,SIZE(CurrentV,2)
 DO k=1,SIZE(CurrentV,3)
  c = 1

  Tab(r,c) = STR(g)
  c = c + 1

  Tab(r,c) = STR(k)
  c = c + 1

  V = Vert(Mesh,k)
  DO d=1,Nd
   Tab(r,c) = STR(V(d),"(Es21.14)")
   c = c + 1
  END DO

  DO d=1,Nd
   Tab(r,c) = STR(CurrentV(d,g,k),"(Es21.14)")
   c=c+1
  END DO

  r = r + 1

 END DO
END DO

CALL PRINT_Table(Tab,Unit=Unit)
DEALLOCATE( Tab , V )

!!--end--
END SUBROUTINE

END MODULE
