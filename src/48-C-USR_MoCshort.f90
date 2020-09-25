!!# USER MODULE: <<USR_MoCshort>>
MODULE USR_MoCshort

!!## PURPOSE
!! The user routines for the Method of Characteristics
!! with short (intra-cell) interpolation.

!!## DEPENDENCIES
USE KND_AngularFluxes                                                     !!((02-A-KND_AngularFluxes.f90))
USE KND_ScalarFluxes                                                      !!((02-A-KND_ScalarFluxes.f90))
USE TBX_Mesh                                                              !!((15-B-TBX_Mesh.f90))
USE SLV_laVandermonde                                                     !!((04-B-SLV_laVandermonde.f90))
USE KND_DiscreteOrdinates                                                 !!((02-A-KND_DiscreteOrdinates.f90))
USE USR_pThread                                                           !!((03-A-USR_pThread.f90))
USE SUB_Sort_quick,ONLY: Sort=>Sort_quick                                 !!((03-A-SUB_Sort_quick.f90))
USE USR_Source                                                            !!((35-B-USR_Source.f90))
USE USR_Iptr                                                              !!((03-C-USR_Iptr.f90))
USE ISO_varying_string                                                    !!((03-A-ISO_varying_string.f90))
USE USR_fdbk                                                              !!((08-C-USR_fdbk.f90))
USE FUN_STR                                                               !!((05-B-FUN_STR.f90))
USE USR_SimpleSet                                                         !!((12-B-USR_SimpleSet.f90))
USE FUN_xyLINE                                                            !!((05-B-FUN_xyLINE.f90))
USE FUN_Default                                                           !!((04-A-FUN_Default.f90))
USE FUN_xyINTERSECT                                                       !!((06-A-FUN_xyINTERSECT.f90))
USE FUN_Reorder                                                           !!((05-A-FUN_Reorder.f90))
USE FUN_xySDIST                                                           !!((03-A-FUN_xySDIST.f90))
USE FUN_xyPLANE                                                           !!((05-B-FUN_xyPLANE.f90))
USE FUN_xyDIRECTION                                                       !!((04-B-FUN_xyDIRECTION.f90))
USE FUN_xyDOT                                                             !!((03-A-FUN_xyDOT.f90))
USE FUN_xyPERPCCW                                                         !!((03-A-FUN_xyPERPCCW.f90))
USE FUN_IsApprox                                                          !!((03-A-FUN_IsApprox.f90))
USE FUN_IsError                                                           !!((05-A-FUN_IsError.f90))
USE FUN_xyREFLECT                                                         !!((05-B-FUN_xyREFLECT.f90))
USE USR_fdbk

!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## DEFAULT ACCESS
PRIVATE

!!## INTERFACE for IsReflective
INTERFACE IsReflective
 MODULE PROCEDURE IsReflective_0
 MODULE PROCEDURE IsReflective_10
 MODULE PROCEDURE IsReflective_11
END INTERFACE


!!## PUBLIC ACCESS
PUBLIC :: EVAL_PiecewiseLinearAtOrigin
PUBLIC :: TEST_Interpolation
PUBLIC :: EVAL_ParabolicAtOrigin
PUBLIC :: TEST_Directions
PUBLIC :: VSErrMsg,VSTolMsg,VSRelErrMsg
PUBLIC :: TYPE_IPtr
PUBLIC :: IsReflective
PUBLIC :: SETUP_IntraCellDivision
PUBLIC :: EVAL_FixedBC
PUBLIC :: EVAL_VertFixedBC
PUBLIC :: GET_LongBC
PUBLIC :: EVAL_BalanceEquation0
PUBLIC :: EVAL_ReflectiveBC
PUBLIC :: EVAL_ReflectiveBC_POINT
PUBLIC :: GET_DiagCorners_CORNER,GET_DiagCorners_HANGING
PUBLIC :: GET_IntracellParams,GET_IntracellPos
PUBLIC :: SETUP_pThread_PlaneWave
PUBLIC :: SETUP_pThread_FwdPara
PUBLIC :: SETUP_pThread_BwdPara
PUBLIC :: SETUP_pThread_FwdLin
PUBLIC :: SETUP_pThread_BwdLin
PUBLIC :: CHECK_BalanceEquation

PUBLIC :: ReportNonPositivity

!!## MODULE PROCEDURES
CONTAINS



FUNCTION VSErrMsg(err) RESULT(VS)
REAL(KIND_AngularFlux) :: err
TYPE(varying_string) :: VS
VS = "[err="//TRIM(STR(err/EPSILON(err),"(es9.1)"))//"eps]"
END FUNCTION

FUNCTION VSTolMsg(err) RESULT(VS)
REAL(KIND_AngularFlux) :: err
TYPE(varying_string) :: VS
VS = "[tol="//TRIM(STR(err,"(es9.1)"))//"]"
END FUNCTION

FUNCTION VSRelErrMsg(err) RESULT(VS)
REAL(KIND_AngularFlux) :: err
TYPE(varying_string) :: VS
VS = "[relerr="//TRIM(STR(err,"(es9.1)"))//"]"
END FUNCTION

SUBROUTINE SETUP_IntraCellDivision(  jin , jout , &
                                     kin , kout , &
                                     Directions , Mesh , fdbk )
!!#### PURPOSE
!! Setup the divisions between incoming and outgoing faces and
!! verts within a cell.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Iptr),POINTER :: jin(:,:),jout(:,:)
TYPE(TYPE_Iptr),POINTER :: kin(:,:),kout(:,:)

!!#### REQUIRED INPUT
REAL(KIND_DOR) ,INTENT(IN) :: Directions(:,:)
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### LOCAL VARIABLES
INTEGER :: Nj,Ni,Nm,Nd
INTEGER :: m,i,n,j,o,p,k
REAL(KIND_MSH) :: FN(3),DP

!!--begin--

NULLIFY( jin , jout )
NULLIFY( kin , kout )

Nj = NUM_Faces(Mesh)
Ni = NUM_Cells(Mesh)
Nm = SIZE( Directions , 2 )
Nd = NUM_Dimensions(Mesh)

ALLOCATE( jin(Nm,Ni) , jout(Nm,Ni) )
ALLOCATE( kin(Nm,Ni) , kout(Nm,Ni) )

DO m=1,Nm
 DO i=1,Ni

  !get the incoming and outgoing faces
  DO n=1,NUM_Faces(Mesh%Cells(i))

   j = Mesh%Cells(i)%FaceList(n)

   FN = FaceNormal(Mesh,j)

   DP = DOT_PRODUCT( FN(1:Nd) , Directions(1:Nd,m) )

   !we have an outgoing face
   IF( DP>0._KIND_DOR )THEN
    CALL ADD_TO_SET( jout(m,i)%list , j )
   !we have an incoming face
   ELSE
    CALL ADD_TO_SET( jin (m,i)%list , j )
   END IF

  END DO


  !get the incoming and outgoing verts
  DO n=1,NUM_Faces(Mesh%Cells(i))

   j = Mesh%Cells(i)%FaceList(n)

   !get the index of the face in the outgoing list of faces
   !and get the outgoing verts
   o = FIND_IN_SET( jout(m,i)%list , j )
   IF( o/=0 )THEN
    DO p=1,NUM_Verts(Mesh%Faces(ABS(j)))
     k = Mesh%Faces(ABS(j))%VertList(p)
     CALL ADD_TO_SET( kout(m,i)%list , k )
    END DO
   END IF

   !get the index of the face in the incoming list of faces
   !and get the incoming verts
   o = FIND_IN_SET( jin(m,i)%list , j )
   IF( o/=0 )THEN
    DO p=1,NUM_Verts(Mesh%Faces(ABS(j)))
     k = Mesh%Faces(ABS(j))%VertList(p)
     CALL ADD_TO_SET     ( kin (m,i)%list , k )
     !remove this vert from the outgoing list because
     !it is in the incoming list and incoming trumps outgoing
     CALL REMOVE_FROM_SET( kout(m,i)%list , k )
    END DO
   END IF

  END DO
  CALL FINALIZE_SET( kout(m,i)%list )
  CALL FINALIZE_SET( kin (m,i)%list )
  CALL FINALIZE_SET( jout(m,i)%list )
  CALL FINALIZE_SET( jin (m,i)%list )

 END DO
END DO

!!--end--
END SUBROUTINE


SUBROUTINE GET_DiagCorners_Corner(Mesh,k,i,k1,k2,j1,j2)
!!#### PURPOSE
!! Determine the corner verts for a diagonal
!! <k1,k2> and for vertex <k> and cell <i>, based on
!! knowledge of one of the corner vertices <k>.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER,INTENT(IN) :: k,i
!!#### REQUIRED OUTPUT
INTEGER,INTENT(OUT) :: k1,k2,j1,j2

!!#### LOCAL VARIABLES
INTEGER :: jj,js,kk,ks

!!--begin--
k1=0
k2=0
j1=0
j2=0

!search through for corner verts
!and faces, k1,k2,j1,j2
DO js=1,NUM_Faces(Mesh%Cells(i))

 jj = ABS(Mesh%Cells(i)%FaceList(js))

 !found a face which this vert belongs to
 IF( ANY(Mesh%Faces(jj)%VertList(:)==k) )THEN
  DO ks=1,NUM_Verts(Mesh%Faces(jj))

   kk = Mesh%Faces(jj)%VertList(ks)
   IF( kk==k )CYCLE

   !found a valid corner
   IF( IsCornerVert(Mesh,kk,i) )THEN

    IF( k1==0 )THEN
     k1 = kk
    ELSE
     k2 = kk
    END IF
    IF( j1==0 )THEN
     j1 = jj
    ELSE
     j2 = jj
    END IF
    EXIT
   END IF

  END DO

 END IF
END DO

IF( k1==0 .OR. k2==0 )THEN
 WRITE(*,*)"FATAL ERROR finding verts"
ELSE IF( j1==0 .OR. j2==0 )THEN
 WRITE(*,*)"FATAL ERROR finding faces"
END IF

!!-end--
END SUBROUTINE


SUBROUTINE GET_DiagCorners_Hanging(Mesh,kh,i,Direction,j1,j2,k1,k2)
!!#### PURPOSE
!! Determine the corner verts for a diagonal
!! <k1,k2> and for vertex <k> and cell <i>, based on
!! knowledge of one of the hanging vertices <kh>.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
REAL(KIND_MSH),INTENT(IN) :: Direction(:)
INTEGER,INTENT(IN) :: kh,i
INTEGER,INTENT(IN) :: j1(:),j2(:)

!!#### REQUIRED INPUT/OUTPUT
INTEGER,INTENT(INOUT) :: k1(:),k2(:)
REAL(KIND_MSH) :: V(SIZE(Direction))

!!#### LOCAL VARIABLES
INTEGER :: j,kc

!!--begin--

!get the face the hanging vert is on
j = ParentFace( Mesh , kh , i )

IF( j==0 )THEN
 WRITE(*,*)"Fatal Error: the vert [k="//TRIM(STR(kh))//"] should be a &
 &hanging vert."
 STOP
END IF

!iterate through faces to find corner vert kc
DO kc=1,SIZE(j1)
 !do an inverse search by checking to see if either of the corner's
 !faces are the same as the hanging vert's parent face
 IF( j==j1(kc) .OR. j==j2(kc) )THEN

  !now check to make sure that both of these faces reside on this cell
  IF( HasCellFaces(Mesh,i, (/j1(kc),j2(kc)/) ) )THEN

   !the corner vert MUST be in the direction of the characteristic from
   !the hanging vert to corner
   !V = xyVECTOR_PP( Vert(Mesh,kh) , Vert(Mesh,kc) )
   !IF( DOT_PRODUCT(V,Direction)<=0._KIND_MSH )THEN
    !now we can just set the parameters for the hanging vert to
    !the same as the corner vert we just found
    k1(kh) = k1(kc)
    k2(kh) = k2(kc)
    RETURN

   !END IF

  END IF
 END IF

END DO

!if we get her then we have an error
WRITE(*,*)"Fatal Error: the proper diagonal could not be found for&
 & hanging vert [k="//TRIM(STR(kh))//"]."
STOP

!!-end--
END SUBROUTINE



FUNCTION TEST_Interpolation(tol,NTrials,relerr) RESULT(Pass)
USE FUN_Random                                                            !!((03-A-FUN_Random.f90))
USE SUB_Sort_quick,ONLY: Sort=>Sort_quick                                 !!((03-A-SUB_Sort_quick.f90))
USE FUN_Reorder                                                           !!((05-A-FUN_Reorder.f90))

REAL(KIND_AngularFlux),INTENT(IN) ,OPTIONAL :: tol
INTEGER               ,INTENT(IN) ,OPTIONAL :: NTrials
REAL(KIND_AngularFlux),INTENT(OUT),OPTIONAL :: relerr

LOGICAL :: Pass

REAL(KIND_AngularFlux) :: a0,a1,a2
REAL(KIND_AngularFlux) :: x1,x2,x3
REAL(KIND_AngularFlux) :: y1,y2,y3

REAL(KIND_AngularFlux) :: reldiff,exact,approx,max_reldiff
REAL(KIND_AngularFlux) :: tol_
INTEGER :: NTrials_,n
REAL(KIND_AngularFlux) :: y(3),x(3)
INTEGER :: order(3)

!!--begin--

NTrials_ = Default( 10 , NTrials )
tol_     = Default( SQRT(EPSILON(approx)) , tol )
max_reldiff = 0.d0
Pass = .TRUE.

CALL RANDOM_SEED()

DO n=1,NTrials_

 !test domain [-1e3,1e3]
 !test function $ y = a0 + a1 x + a2 x^2 $ with $a \in [-10,10]$
 a0=Random( (/-1.d1,1.d1/) )
 a1=Random( (/-1.d1,1.d1/) )
 a2=Random( (/-1.d1,1.d1/) )

 x1=Random( (/-1.d1,0.d0/)/REAL(n) )
 x2=Random( (/-1.d1,1.d1/)/REAL(n) )
 x3=Random( (/ 0.d0,1.d1/)/REAL(n) )

 y1 = a0 + a1*x1 + a2*x1**2
 y2 = a0 + a1*x2 + a2*x2**2
 y3 = a0 + a1*x3 + a2*x3**2

 y = (/y1,y2,y3/)
 x = (/x1,x2,x3/)
 CALL Sort( x , order )
 y = Reorder( y , order , side="L")

 exact = a0
 approx = EVAL_ParabolicAtOrigin( y , x )

 reldiff = ABS(approx-exact)/(y(3)-y(1))

 max_reldiff = MAX(reldiff,max_reldiff)
 Pass = Pass .AND. (max_reldiff<tol_)

 IF( PRESENT(relerr) )THEN
  relerr = max_reldiff
 END IF

END DO
!!--end--
END FUNCTION



FUNCTION EVAL_PiecewiseLinearAtOrigin( y , x ) RESULT(y0)
REAL(KIND_AngularFlux),INTENT(IN) :: y(3)
REAL(KIND_AngularFlux),INTENT(IN) :: x(3)

REAL(KIND_AngularFlux) :: y0

REAL(KIND_AngularFlux) :: c12,c23,c31

!!--begin--

c12 = x(1)*x(2)
c23 = x(2)*x(3)
c31 = x(3)*x(1)

!origin between x(1) and x(2)
IF( c12<0._KIND_AngularFlux )THEN

 y0 = ( y(1)*x(2) - y(2)*x(1) )/( x(2) - x(1) )

!origin between x(2) and x(3)
ELSE IF( c23<0._KIND_AngularFlux )THEN

 y0 = ( y(2)*x(3) - y(3)*x(2) )/( x(3) - x(2) )

END IF

!!--end--
END FUNCTION



FUNCTION EVAL_ParabolicAtOrigin( y , x ) RESULT(y0)
REAL(KIND_AngularFlux),INTENT(IN) :: y(3)
REAL(KIND_AngularFlux),INTENT(IN) :: x(3)

REAL(KIND_AngularFlux) :: y0

REAL(KIND_AngularFlux) :: c12,c23,c31

!!--begin--

c12 = x(1)*x(2)
c23 = x(2)*x(3)
c31 = x(3)*x(1)
y0  = c23*y(1)/(-c12+c23-c31+x(1)**2) - &
      c31*y(2)/(+c12+c23-c31-x(2)**2) + &
      c12*y(3)/(+c12-c23-c31+x(3)**2)

!!--end--
END FUNCTION



!!### FUNCTION <<TEST_Directions>>
FUNCTION TEST_Directions( Directions , tol , err )  RESULT(TEST)

!!#### PURPOSE
!! Test directions for consistency.

!!#### REQUIRED INPUT
REAL(KIND_DOR),INTENT(IN) :: Directions(:,:)

!!#### REQUIRED OUTPUT
LOGICAL :: TEST

!!#### OPTIONAL INPUT
REAL(KIND_DOR),INTENT(IN),OPTIONAL  :: tol

!!#### OPTIONAL OUTPUT
REAL(KIND_DOR),INTENT(OUT),OPTIONAL :: err

!!#### LOCAL VARIABLES
REAL(KIND_DOR) :: tol_,err_,thiserr
INTEGER        :: m

!!--begin--

tol_ = DEFAULT( 100._KIND_DOR*EPSILON(tol_) , tol )
err_ = 0._KIND_DOR

DO m=1,SIZE(Directions,2)
 thiserr = ABS( 1._KIND_DOR - SQRT(SUM(Directions(:,m)**2)) )
 err_ = MAX( err_ , thiserr )
END DO

TEST = (err_<=tol_)

IF( PRESENT(err) )THEN
 err = err_
END IF

!!--end--
END FUNCTION





!!### FUNCTION: <IsFixed>
FUNCTION IsFixed( BC )

!!#### PURPOSE
!! Whether the boundary condition passed is a "fixed"
!! boundary condition: i.e. the value of the angular flux
!! is fixed at a specified value and will not change.

!!#### EXTERNAL PARAMETERS
USE PAR_MoCshort,ONLY: unittest_,vacuum_,fixed_,planewave_,function_      !!((03-A-PAR_MoCshort.f90))

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: BC

!!#### REQUIRED OUTPUT
LOGICAL :: IsFixed

!!--begin--

IsFixed = (BC==unittest_)  .OR. &
          (BC==vacuum_  )  .OR. &
          (BC==fixed_   )  .OR. &
          (BC==planewave_) .OR. &
          (BC==function_)

!!--end--
END FUNCTION




!!### FUNCTION: <IsReflective>
FUNCTION IsReflective_0( BC )

!!#### PURPOSE
!! Whether the boundary condition passed is a
!! "reflective" boundary condition: i.e. the value
!! of the angular flux is given by symmetry conditions.

!!#### EXTERNAL PARAMETERS
USE PAR_MoCshort,ONLY: reflective_                                        !!((03-A-PAR_MoCshort.f90))

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: BC

!!#### REQUIRED OUTPUT
LOGICAL :: IsReflective_0

!!--begin--
IsReflective_0 = (BC==reflective_)

!!--end--
END FUNCTION




!!### FUNCTION: <IsReflective_10>
FUNCTION IsReflective_10( BC , j )

!!#### PURPOSE
!! Whether the boundary condition passed is a
!! "reflective" boundary condition: i.e. the value
!! of the angular flux is given by symmetry conditions.

!!#### EXTERNAL PARAMETERS
USE PAR_MoCshort,ONLY: reflective_                                        !!((03-A-PAR_MoCshort.f90))

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: BC(:)
INTEGER,INTENT(IN) :: j

!!#### REQUIRED OUTPUT
LOGICAL :: IsReflective_10

!!--begin--
IsReflective_10 = (BC(j)==reflective_)

!!--end--
END FUNCTION




!!### FUNCTION: <IsReflective_11>
FUNCTION IsReflective_11( BC )

!!#### PURPOSE
!! Whether the boundary condition passed is a
!! "reflective" boundary condition: i.e. the value
!! of the angular flux is given by symmetry conditions.

!!#### EXTERNAL PARAMETERS
USE PAR_MoCshort,ONLY: reflective_                                        !!((03-A-PAR_MoCshort.f90))

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: BC(:)

!!#### REQUIRED OUTPUT
LOGICAL :: IsReflective_11(1:SIZE(BC))

!!--begin--
IsReflective_11 = (BC==reflective_)

!!--end--
END FUNCTION



SUBROUTINE GET_IntraCellPos( Mesh , k , m , &
  Direction , Pn_front , Pn_dir , InterpVerts , SourceDist , &
  StreamDist , FrontPos )
!!#### PURPOSE
!! Determine the streaming distance and front position from
!! input data about this characteristic.
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER        ,INTENT(IN)    :: k,m
REAL(KIND_MSH) ,INTENT(IN)    :: Direction(:)
REAL(KIND_MSH) ,INTENT(IN)    :: Pn_front(:)
REAL(KIND_MSH) ,INTENT(IN)    :: Pn_dir(:)

!!#### REQUIRED INPUT/OUTPUT
INTEGER        ,INTENT(INOUT) :: InterpVerts(:)
REAL(KIND_MSH) ,INTENT(INOUT) :: SourceDist
REAL(KIND_MSH) ,INTENT(INOUT) :: StreamDist(SIZE(InterpVerts))
REAL(KIND_MSH) ,INTENT(INOUT) :: FrontPos  (SIZE(InterpVerts))


!!#### LOCAL VARIABLES
REAL(KIND_MSH),ALLOCATABLE :: Ln(:,:),P_front(:),P_interp(:)
INTEGER :: n
LOGICAL :: INTERSECT
INTEGER,ALLOCATABLE :: order(:)

!!--begin--

ALLOCATE( Ln(1:NUM_Dimensions(Mesh),2) )
Ln = 0._KIND_MSH
ALLOCATE( P_front(1:NUM_Dimensions(Mesh)) )
P_front = 0._KIND_MSH
ALLOCATE( P_interp(1:NUM_Dimensions(Mesh)) )
P_interp = 0._KIND_MSH
ALLOCATE( order(SIZE(InterpVerts)) )
order = 0

DO n=1,SIZE(InterpVerts)

 IF( InterpVerts(n)<=0 )CYCLE

 !determine the point to use in the interpolation
 P_interp = Vert(Mesh,InterpVerts(n))

 !construct a ray from this point and the direction of travel
 Ln = xyLINE_PU( P_interp , REAL(Direction(:),KIND_MSH) )

 !determine the intersection of this line the interpolation plane
 INTERSECT = xyINTERSECT_PnLn( Pn_front , Ln , &
                               SDIST       = StreamDist( n ) , &
                               P_intersect = P_front )
 IF( .NOT.INTERSECT )THEN
  WRITE(*,*)"There is a problem for n="//TRIM(STR(n))//", k="//TRIM(STR(k))//&
    ", m="//TRIM(STR(m))//"."
  WRITE(*,*)"We have should have found an intersection here..."
 END IF

 !by using this plane and determining distances the points are from the plane
 !now we get the correctly signed values for the points of the line of the front
 FrontPos ( n ) = xySDIST_PnP( Pn_dir , P_front )
END DO

!sort the front positions in ascending order
CALL Sort( FrontPos(:) , order )
InterpVerts(:) = Reorder( InterpVerts(:) , order , side="R" )
StreamDist(:)  = Reorder( StreamDist(:)  , order , side="R" )

DEALLOCATE( Ln )
DEALLOCATE( P_front )
DEALLOCATE( P_interp )
DEALLOCATE( order )

!!--end--
END SUBROUTINE



SUBROUTINE GET_IntraCellParams( Mesh , k , k1 , k2 , U , &
    Pn_front , Pn_dir , SourceDist )

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER        ,INTENT(IN) :: k,k1,k2
REAL(KIND_MSH) ,INTENT(IN) :: U(2)

!!#### REQUIRED INPUT/OUTPUT
REAL(KIND_MSH),INTENT(INOUT)  :: Pn_front(3),Pn_dir(3)

!!#### LOCAL VARIABLES
REAL(KIND_MSH) :: Ln(2,2),SourceDist,P_base(2)
LOGICAL           :: INTERSECT
LOGICAL,PARAMETER :: Noisy_=.FALSE.

!!--begin--

!make the plane that connects the two corners
Pn_front = xyPLANE_P2( RESHAPE(&
  (/Vert(Mesh,k1) , Vert(Mesh,k2)/),(/2,2/)) )

!sign change to make direction of face in the direction of motion
IF( xyDOT_VV( xyDIRECTION_Pn(Pn_front) , U )<0._KIND_MSH )THEN
 Pn_front =  -Pn_front
END IF


!get the line
Ln = xyLINE_PV( Vert(Mesh,k) , -U )

!get the distance from the plane to the vert of interest
INTERSECT = xyINTERSECT_PnLn( Pn_front , Ln , &
                              SDIST       = SourceDist , &
                              P_intersect = P_base )

IF( .NOT.INTERSECT )THEN
 WRITE(*,*)"MAJOR PROBLEM IN INTERSECTION"
END IF

!determine another plane that divides the interpolation plane (front) into
!two pieces
Pn_dir = xyPLANE_PV( P_base , xyPERPCCW_U( xyDIRECTION_Pn(Pn_front) )  )

IF( Noisy_ )THEN
 WRITE(*,*)"dist from Plane to P_base:",xySDIST_PnP( Pn_front , P_base )
 WRITE(*,*)"dist from Plane to P_1   :",xySDIST_PnP( Pn_front , Vert(Mesh,k1) )
 WRITE(*,*)"dist from Plane to P_2   :",xySDIST_PnP( Pn_front , Vert(Mesh,k2) )
END IF

!!--end--

END SUBROUTINE


SUBROUTINE SETUP_pthread_PlaneWave( pthread , k_ , Mesh , Ordinates )
!!#### PURPOSE
!! Determine the parallel threads variable <pThread> for traversing
!! the mesh with method of characteristics (short) by the
!! plane wave <PlaneWave> method.

!!#### REQUIRED OUTPUT
!! * the parallel threads variable
TYPE(TYPE_pthread),POINTER    :: pthread(:)

!!#### REQUIRED INPUT
INTEGER           ,INTENT(IN) :: k_(:,:,:)
TYPE(TYPE_Mesh)   ,INTENT(IN) :: Mesh
REAL(KIND_DOR)    ,INTENT(IN) :: Ordinates(:,:)

!!#### LOCAL VARIABLES
INTEGER :: MAX_ma,MAX_k,k,ma,Nd
INTEGER,ALLOCATABLE :: k2_(:,:),order(:)
REAL(KIND_MSH),ALLOCATABLE :: DIST(:)
REAL(KIND_MSH),ALLOCATABLE :: Pn(:)

!!--begin--

!get numbers
MAX_ma = SIZE(Ordinates,2)
MAX_k   = NUM_Verts(Mesh)
Nd = NUM_Dimensions(Mesh)

!allocate pthread
CALL ALLOCATE_pthread0( pthread , MAX_ma , MAX_k )

!allocate DIST
ALLOCATE( DIST(1:MAX_k) , Pn(Nd+1) )

!**Mesh SWEEP ORDER**
!determine the order in which to sweep the mesh
DO ma=1,MAX_ma
 !determine the plane to base ordering on
 Pn = xyPLANE_PV( Mesh%domain%cell%cellcentroid , REAL(Ordinates(:,ma),KIND_MSH) )

 DO k=1,MAX_k
  DIST(k) = xySDIST_PnP( Pn , Vert(Mesh,k) )
 END DO

 CALL Sort( DIST , pthread(ma)%path(1)%order )
END DO

DEALLOCATE( DIST , Pn )

!!--end--
END SUBROUTINE


SUBROUTINE SETUP_pthread_FwdPara( pthread , k_ , Mesh , Ordinates )
!!#### PURPOSE
!! Determine the parallel threads variable <pThread> for traversing
!! the mesh with method of characteristics (short) by the
!! forward parabolic method <FwdPara>.

!!#### REQUIRED OUTPUT
!! * the parallel threads variable
TYPE(TYPE_pthread),POINTER    :: pthread(:)

!!#### REQUIRED INPUT
INTEGER           ,INTENT(IN) :: k_(:,:,:)
TYPE(TYPE_Mesh)   ,INTENT(IN) :: Mesh
REAL(KIND_DOR)    ,INTENT(IN) :: Ordinates(:,:)

!!#### LOCAL VARIABLES
INTEGER :: MAX_ma,MAX_k,ma,Nd,o,k,kh
INTEGER,ALLOCATABLE :: order(:)
LOGICAL,ALLOCATABLE :: nodeused(:)
INTEGER,PARAMETER :: unit_noisy=6
LOGICAL,PARAMETER :: Noisy_=.FALSE.

!!--begin--

!get numbers
MAX_ma = SIZE(Ordinates,2)
MAX_k   = NUM_Verts(Mesh)
Nd = NUM_Dimensions(Mesh)

!allocate pthread
IF( ASSOCIATED(pthread) )THEN
 CALL DEALLOCATE_pthread( pthread )
END IF
CALL ALLOCATE_pthread0( pthread , MAX_ma , MAX_k )

!now setup pthread to have the <PlaneWave> order
CALL SETUP_pthread_PlaneWave( pthread , k_ , Mesh , Ordinates )

!allocate runtime
ALLOCATE( order(MAX_k) , nodeused(MAX_k) )


!iterate through directions
DO ma=1,MAX_ma
 IF( Noisy_ )write(unit_noisy,*)"ma=",ma

 !initialize the order for this direction
 CALL SETUP_order( k_(:,:,ma) , order , o , nodeused )

 !recurse through ending verts until we get the full chain
 !*DIFFERENCE FROM Bwd: backward has k=1,MAX_k,1
 !*
 DO k=MAX_k,1,-1
  kh = pthread(ma)%path(1)%order(k)
  IF( Noisy_)write(unit_noisy,*)"  kh=",kh
  IF( .NOT.nodeused(kh) )THEN
   !*DIFFERENCE FROM Lin: linear has k_(1:2,:,ma)
   !*
   CALL Recurse_Tree(k_(:,:,ma),order,kh,o,0,nodeused)
  END IF
  IF( order(MAX_k)/=0 )EXIT
 END DO

 !add any verts that didn't get added
 CALL WRAPUP_order( k_(:,:,ma) , order , o , nodeused )

 !set the final ordering for this direction
 pthread(ma)%path(1)%order = order

END DO

!!--end--
END SUBROUTINE



SUBROUTINE SETUP_pthread_FwdLin( pthread , k_ , Mesh , Ordinates )
!!#### PURPOSE
!! Determine the parallel threads variable <pThread> for traversing
!! the mesh with method of characteristics (short) by the
!! forward linear method <FwdLin>.

!!#### REQUIRED OUTPUT
!! * the parallel threads variable
TYPE(TYPE_pthread),POINTER    :: pthread(:)

!!#### REQUIRED INPUT
INTEGER           ,INTENT(IN) :: k_(:,:,:)
TYPE(TYPE_Mesh)   ,INTENT(IN) :: Mesh
REAL(KIND_DOR)    ,INTENT(IN) :: Ordinates(:,:)

!!#### LOCAL VARIABLES
INTEGER :: MAX_ma,MAX_k,ma,Nd,o,k,kh
INTEGER,ALLOCATABLE :: order(:)
LOGICAL,ALLOCATABLE :: nodeused(:)
INTEGER,PARAMETER :: unit_noisy=6
LOGICAL,PARAMETER :: Noisy_=.FALSE.

!!--begin--

!get numbers
MAX_ma = SIZE(Ordinates,2)
MAX_k   = NUM_Verts(Mesh)
Nd = NUM_Dimensions(Mesh)

!allocate pthread
IF( ASSOCIATED(pthread) )THEN
 CALL DEALLOCATE_pthread( pthread )
END IF
CALL ALLOCATE_pthread0( pthread , MAX_ma , MAX_k )

!now setup pthread to have the <PlaneWave> order
CALL SETUP_pthread_PlaneWave( pthread , k_ , Mesh , Ordinates )

!allocate runtime
ALLOCATE( order(MAX_k) , nodeused(MAX_k) )


!iterate through directions
DO ma=1,MAX_ma
 IF( Noisy_ )write(unit_noisy,*)"ma=",ma

 !initialize the order for this direction
 CALL SETUP_order( k_(:,:,ma) , order , o , nodeused )

 !recurse through ending verts until we get the full chain
 !*DIFFERENCE FROM Bwd: backward has k=1,MAX_k,1
 !*
 DO k=MAX_k,1,-1
  kh = pthread(ma)%path(1)%order(k)
  IF( Noisy_)write(unit_noisy,*)"  kh=",kh
  IF( .NOT.nodeused(kh) )THEN
   !*DIFFERENCE FROM Para: parabolic has k_( : ,:,ma)
   !*
   CALL Recurse_Tree(k_(1:2,:,ma),order,kh,o,0,nodeused)
  END IF
  IF( order(MAX_k)/=0 )EXIT
 END DO

 !add any verts that didn't get added
 CALL WRAPUP_order( k_(:,:,ma) , order , o , nodeused )

 !set the final ordering for this direction
 pthread(ma)%path(1)%order = order

END DO

!!--end--
END SUBROUTINE



SUBROUTINE SETUP_pthread_BwdPara( pthread , k_ , Mesh , Ordinates )
!!#### PURPOSE
!! Determine the parallel threads variable <pThread> for traversing
!! the mesh with method of characteristics (short) by the
!! backwafd parabolic method <BwdPara>.

!!#### REQUIRED OUTPUT
!! * the parallel threads variable
TYPE(TYPE_pthread),POINTER    :: pthread(:)

!!#### REQUIRED INPUT
INTEGER           ,INTENT(IN) :: k_(:,:,:)
TYPE(TYPE_Mesh)   ,INTENT(IN) :: Mesh
REAL(KIND_DOR)    ,INTENT(IN) :: Ordinates(:,:)

!!#### LOCAL VARIABLES
INTEGER :: MAX_ma,MAX_k,ma,Nd,o,k,kh
INTEGER,ALLOCATABLE :: order(:)
LOGICAL,ALLOCATABLE :: nodeused(:)
INTEGER,PARAMETER :: unit_noisy=6
LOGICAL,PARAMETER :: Noisy_=.FALSE.

!!--begin--

!get numbers
MAX_ma = SIZE(Ordinates,2)
MAX_k   = NUM_Verts(Mesh)
Nd = NUM_Dimensions(Mesh)

!allocate pthread
IF( ASSOCIATED(pthread) )THEN
 CALL DEALLOCATE_pthread( pthread )
END IF
CALL ALLOCATE_pthread0( pthread , MAX_ma , MAX_k )

!now setup pthread to have the <PlaneWave> order
CALL SETUP_pthread_PlaneWave( pthread , k_ , Mesh , Ordinates )

!allocate runtime
ALLOCATE( order(MAX_k) , nodeused(MAX_k) )


!iterate through directions
DO ma=1,MAX_ma
 IF( Noisy_ )write(unit_noisy,*)"ma=",ma

 !initialize the order for this direction
 CALL SETUP_order( k_(:,:,ma) , order , o , nodeused )

 !recurse through ending verts until we get the full chain
 !*DIFFERENCE FROM Fwd: forward has k=MAX_k,1,-1
 !*
 DO k=1,MAX_k,1
  kh = pthread(ma)%path(1)%order(k)
  IF( Noisy_)write(unit_noisy,*)"  kh=",kh
  IF( .NOT.nodeused(kh) )THEN
   !*DIFFERENCE FROM Lin: linear has k_(1:2,:,ma)
   !*
   CALL Recurse_Tree(k_(:,:,ma),order,kh,o,0,nodeused)
  END IF
  IF( order(MAX_k)/=0 )EXIT
 END DO

 !add any verts that didn't get added
 CALL WRAPUP_order( k_(:,:,ma) , order , o , nodeused )

 !set the final ordering for this direction
 pthread(ma)%path(1)%order = order

END DO

!!--end--
END SUBROUTINE



SUBROUTINE SETUP_pthread_BwdLin( pthread , k_ , Mesh , Ordinates )
!!#### PURPOSE
!! Determine the parallel threads variable <pThread> for traversing
!! the mesh with method of characteristics (short) by the
!! backward linear method <BwdLin>.

!!#### REQUIRED OUTPUT
!! * the parallel threads variable
TYPE(TYPE_pthread),POINTER    :: pthread(:)

!!#### REQUIRED INPUT
INTEGER           ,INTENT(IN) :: k_(:,:,:)
TYPE(TYPE_Mesh)   ,INTENT(IN) :: Mesh
REAL(KIND_DOR)    ,INTENT(IN) :: Ordinates(:,:)

!!#### LOCAL VARIABLES
INTEGER :: MAX_ma,MAX_k,ma,Nd,o,k,kh
INTEGER,ALLOCATABLE :: order(:)
LOGICAL,ALLOCATABLE :: nodeused(:)
INTEGER,PARAMETER :: unit_noisy=6
LOGICAL,PARAMETER :: Noisy_=.FALSE.

!!--begin--

!get numbers
MAX_ma = SIZE(Ordinates,2)
MAX_k   = NUM_Verts(Mesh)
Nd = NUM_Dimensions(Mesh)

!allocate pthread
IF( ASSOCIATED(pthread) )THEN
 CALL DEALLOCATE_pthread( pthread )
END IF
CALL ALLOCATE_pthread0( pthread , MAX_ma , MAX_k )

!now setup pthread to have the <PlaneWave> order
CALL SETUP_pthread_PlaneWave( pthread , k_ , Mesh , Ordinates )

!allocate runtime
ALLOCATE( order(MAX_k) , nodeused(MAX_k) )


!iterate through directions
DO ma=1,MAX_ma
 IF( Noisy_ )write(unit_noisy,*)"ma=",ma

 !initialize the order for this direction
 CALL SETUP_order( k_(:,:,ma) , order , o , nodeused )

 !recurse through ending verts until we get the full chain
 !*DIFFERENCE FROM Fwd: forward has k=MAX_k,1,-1
 !*
 DO k=1,MAX_k,1
  kh = pthread(ma)%path(1)%order(k)
  IF( Noisy_)write(unit_noisy,*)"  kh=",kh
  IF( .NOT.nodeused(kh) )THEN
   !*DIFFERENCE FROM Para: parabolic has k_( : ,:,ma)
   !*
   CALL Recurse_Tree(k_(1:2,:,ma),order,kh,o,0,nodeused)
  END IF
  IF( order(MAX_k)/=0 )EXIT
 END DO

 !add any verts that didn't get added
 CALL WRAPUP_order( k_(:,:,ma) , order , o , nodeused )

 !set the final ordering for this direction
 pthread(ma)%path(1)%order = order

END DO

!!--end--
END SUBROUTINE



SUBROUTINE SETUP_order( k_ , order , o , nodeused )
!!#### REQUIRED INPUT
INTEGER,INTENT(IN)    :: k_(:,:)
INTEGER,INTENT(INOUT) :: order(:)
INTEGER,INTENT(INOUT) :: o
LOGICAL,INTENT(INOUT) :: nodeused(:)

!!#### LOCAL VARIABLES
INTEGER :: k

!!--begin--

order = 0
o = 0
nodeused = .FALSE.

DO k=1,SIZE(k_,2)
 IF( ALL(k_(:,k)==0) )THEN
  o = o + 1
  order(o) = k
  nodeused(k) = .TRUE.
 END IF
END DO

!!--end--
END SUBROUTINE



SUBROUTINE WRAPUP_order( k_ , order , o , nodeused )

!!#### REQUIRED INPUT
INTEGER,INTENT(IN)    :: k_(:,:)
INTEGER,INTENT(INOUT) :: order(:)
INTEGER,INTENT(INOUT) :: o
LOGICAL,INTENT(INOUT) :: nodeused(:)

!!#### LOCAL VARIABLES
INTEGER :: k

!!--begin--

!use the rest of the nodes
DO k=1,SIZE(k_,2)
 IF( .NOT.nodeused(k) )THEN
  o = o + 1
  order(o) = k
 END IF
END DO

!!--end--
END SUBROUTINE


RECURSIVE SUBROUTINE Recurse_Tree(Tree,order,kh,o,level,nodeused)
INTEGER,INTENT(IN)    :: Tree(:,:)
INTEGER,INTENT(INOUT) :: order(SIZE(Tree,2))
INTEGER,INTENT(INOUT) :: kh
INTEGER,INTENT(INOUT) :: o
INTEGER,INTENT(IN)    :: level
LOGICAL,INTENT(INOUT) :: nodeused(SIZE(Tree,2))

!!locals
LOGICAL,PARAMETER :: Noisy_=.FALSE.
INTEGER,PARAMETER :: unit_noisy = 6
INTEGER :: Nc,No

!!--begin--
Nc = SIZE(Tree,1)
No = SIZE(Tree,2)

IF( Noisy_ )WRITE(unit_noisy,*)REPEAT("  ",level)//"k="//TRIM(STR(kh))
CALL Recurse(kh,o,level=level)

!!--end--
CONTAINS

RECURSIVE SUBROUTINE Recurse(kh,o,level)
INTEGER,INTENT(IN)    :: kh
INTEGER,INTENT(INOUT) :: o
INTEGER,INTENT(IN)    :: level
INTEGER :: kc,c
!!--begin--

IF( Noisy_ )WRITE(unit_noisy,*)REPEAT("  ",level)//"k="//TRIM(STR(kh))
IF( kh==0 )RETURN

!check if all children are dead and if not
!move to each non-dead child
DO c=1,Nc
 kc = Tree(c,kh)
 IF( kc==0 )CYCLE
 IF( nodeused(kc) )CYCLE
 CALL Recurse(kc,o,level+1)
END DO

!all children are dead
IF( .NOT.nodeused(kh) )THEN
 o = o + 1
 IF( Noisy_ )WRITE(unit_noisy,*)REPEAT("  ",level)//"<o>="//TRIM(STR(o))
 IF( Noisy_ )WRITE(unit_noisy,*)REPEAT("  ",level)//"<k>="//TRIM(STR(kh))
 nodeused(kh) = .TRUE.
 order(o) = kh
END IF

!!--end--
END SUBROUTINE


END SUBROUTINE Recurse_Tree




!!### FUNCTION: <<EVAL_fixedBC>>
FUNCTION EVAL_fixedBC( BC , FixedAngularFlux , FunctionAngularFlux , fdbk, &
  jd , Mesh , Ordinates , m , &
  closetozero , r , j ) RESULT(AngularFlux)

!!#### PURPOSE
!! Evaluate a static (fixed/non-reflective) boundary condition
!! on the domain boundary <jd>.

!!#### EXTERNAL PARAMETERS
USE PAR_MoCshort     ,ONLY: unittest_,vacuum_,function_,fixed_,planewave_ !!((03-A-PAR_MoCshort.f90))
USE PAR_Constants_Rdp,ONLY: c_4_times_PI                                  !!((02-A-PAR_Constants_Rdp.f90))
USE SUB_LOAD                                                              !!((06-B-SUB_LOAD.f90))
USE USR_FunctionParser                                                    !!((05-B-USR_FunctionParser.f90))
USE FUN_Integrate1_aq                                                     !!((06-B-FUN_Integrate1_aq.f90))

!!#### REQUIRED INPUT
INTEGER               ,INTENT(IN) :: BC(:)
REAL(KIND_AngularFlux),INTENT(IN) :: FixedAngularFlux(SIZE(BC))
INTEGER               ,INTENT(IN) :: FunctionAngularFlux(SIZE(BC))

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk)    ,INTENT(INOUT),OPTIONAL :: fdbk

!!#### OPTIONAL INPUT
INTEGER               ,INTENT(IN),OPTIONAL :: jd
TYPE(TYPE_Mesh)       ,INTENT(IN),OPTIONAL :: Mesh
REAL(KIND_AngularFlux),INTENT(IN),OPTIONAL :: Ordinates(:,:)
INTEGER               ,INTENT(IN),OPTIONAL :: m
REAL(KIND_AngularFlux),INTENT(IN),OPTIONAL :: closetozero
REAL(KIND_AngularFlux),INTENT(IN),OPTIONAL :: r(:)
INTEGER               ,INTENT(IN),OPTIONAL :: j

!!#### REQUIRED OUTPUT
REAL(KIND_AngularFlux) :: AngularFlux

!!#### LOCAL VARIABLES
INTEGER :: mp0,Nmp0,Nd
REAL(KIND_AngularFlux),ALLOCATABLE :: dp(:)
REAL(KIND_AngularFlux) :: min0
REAL(KIND_AngularFlux) :: V1(2),V2(2),t1,t2
REAL(KIND_AngularFlux) :: x,y,z,ox,oy,oz,mr,fnval
INTEGER :: k1,k2
CHARACTER(255) :: error2

!!--begin--
!initialize
AngularFlux=0.d0

SELECT CASE( BC(jd) )

 CASE( unittest_ ) ; AngularFlux = REAL(1,KIND(AngularFlux))/c_4_times_PI

 CASE( vacuum_   ) ; AngularFlux = REAL(0,KIND(AngularFlux))

 CASE( fixed_    )
   IF( .NOT.PRESENT(jd) )THEN
    AngularFlux = -1._KIND_AngularFlux
   ELSE
    AngularFlux = FixedAngularFlux(jd)
   END IF

 CASE( planewave_ )
   IF( (.NOT.PRESENT(jd)) .OR. (.NOT.PRESENT(Ordinates)) &
     .OR. (.NOT.PRESENT(Mesh)) .OR. (.NOT.PRESENT(m)) .OR. &
         (.NOT.PRESENT(closetozero)) )THEN
    AngularFlux = 0._KIND_AngularFlux
   ELSE
    Nmp0 = SIZE(Ordinates,2)
    Nd = NUM_Dimensions(Mesh)
    ALLOCATE( dp(Nmp0) )
    DO mp0=1,Nmp0
     dp(mp0) = DOT_PRODUCT(Ordinates(1:Nd,mp0),DomainFaceNormal(Mesh,jd))
    END DO
    min0 = MINVAL(dp,1)
    IF( IsApprox( dp(m),min0 ) )THEN
     !make only the azimuthal angle closest to the xy-plane nonzero
     IF( IsApprox( closetozero , 0._KIND_AngularFlux) )THEN
      AngularFlux = FixedAngularFlux(jd)
     ELSE
      AngularFlux = 0._KIND_AngularFlux
     END IF
    END IF
    DEALLOCATE( dp )

   END IF

 CASE( function_ )
  !the function has the following variables 'x y z ox oy oz m'
  IF( PRESENT(m) )THEN
   mr = m
  ELSE
   mr = 1
  END IF

  IF( PRESENT(r) )THEN
   x = r(1)
   y = r(2)
   IF( SIZE(r)<3 )THEN
    z = 0.d0
   ELSE
    z = r(3)
   END IF
  ELSE
   x = 0.d0
   y = 0.d0
   z = 0.d0
  END IF

  IF( PRESENT(Ordinates) )THEN
   ox = Ordinates(1,m)
   oy = Ordinates(2,m)
   oz = Ordinates(3,m)
  ELSE
   ox = SQRT(2.d0)/2.d0
   oy = SQRT(2.d0)/2.d0
   oz = 0.d0
  END IF

  !face value needed (only works for 2D)
  IF( PRESENT(j) )THEN

   !get bounds for integration
   k1 = GET_VertOnFace(Mesh,j,First=.TRUE.)
   k2 = GET_VertOnFace(Mesh,j,Last=.TRUE.)
   V1=Vert(Mesh,k1)
   V2=Vert(Mesh,k2)
   !bottom or top domain boundary
   IF( jd==1 .OR. jd==3 )THEN
    t1=V1(1)
    t2=V2(1)
   !left or right domain boundary
   ELSE IF( jd==2 .OR. jd==4 )THEN
    t1=V1(2)
    t2=V2(2)
   END IF

   !get a single value
   IF( t2==t1 )THEN
    AngularFlux = AngularFluxBCFunction(t1)

   !get average angular flux by integration of boundary values
   ELSE

    !integral
    AngularFlux = Integrate1_aq( AngularFluxBCFunction , &
     (/t1,t2/) , tol=1.d-14 , N=1000 )

    !check to make sure integration worked
    IF( IsError(AngularFlux) )THEN
     WRITE(*,*)"[[MCS]] Warning: Numerical integration of face value failed: center face value used."
     AngularFlux = AngularFluxBCFunction(0.5_KIND_AngularFlux*(t1+t2))
    ELSE
     AngularFlux = AngularFlux/(t2-t1)
    END IF

   END IF

  !vert value needed
  ELSE
   call s_evaluatefn(FunctionAngularFlux(jd), &
     (/x,y,z,ox,oy,oz,mr/), fnval, error2)
   AngularFlux = fnval
  END IF

END SELECT

!!--end--
CONTAINS


FUNCTION AngularFluxBCFunction(t) RESULT(val)
REAL(KIND_AngularFlux),INTENT(IN) :: t
REAL(KIND_AngularFlux) :: val
!!--begin--

IF( jd==1 .OR. jd==3 )THEN
 x=t
ELSE IF( jd==2 .OR. jd==4 )THEN
 y=t
END IF
!WRITE(*,*)"evaluation of BC"
call s_evaluatefn(FunctionAngularFlux(jd), &
 (/x,y,z,ox,oy,oz,mr/), val, error2)
IF( TRIM(error2)/="OK" )THEN
 WRITE(*,*)"[[MCS]] Error in function parser."
 STOP
END IF

!!--end--
END FUNCTION


END FUNCTION





!!### FUNCTION: <<EVAL_reflectiveBC>>
FUNCTION EVAL_reflectiveBC( BC , u , Ordinates , WithinCell , &
  n1 , n2 ) RESULT(m)

!!#### PURPOSE
!! Evaluate a reflective boundary condition, returning the
!! appropriate angular direction index which approximates the
!! the reflected direction best.  The normals of up to 2 planar
!! reflective surfaces may be provided.

!!#### GLOBAL VARIABLES
USE VAR_DiscreteOrdinates,ONLY: KIND_DOR                                  !!((47-B-VAR_DiscreteOrdinates.f90))

!!#### REQUIRED INPUT
!! * boundary condition
!! * direction that needs reflection
!! * list of directions
!! * directions to reflect u off of [n1,n2]
INTEGER,INTENT(IN) :: BC
REAL(KIND_DOR),INTENT(IN) :: u(:)
REAL(KIND_DOR),INTENT(IN) :: Ordinates(:,:)
INTEGER       ,INTENT(IN) :: WithinCell(:)
REAL(KIND_MSH),INTENT(IN) :: n1(1:size(u))
REAL(KIND_MSH),INTENT(IN),DIMENSION(1:size(u)),OPTIONAL :: n2

!!#### REQUIRED_OUTPUT
INTEGER :: m

!!#### LOCAL VARIABLES
REAL(KIND_MSH) :: reflection(1:3)
REAL(KIND_MSH) :: base_point(1:2)
REAL(KIND_MSH) :: Pn(1:3)

!!--begin--
!! Check if the BC is reflective.
IF( .NOT.IsReflective(BC) )THEN
 WRITE(*,*)"Warning: the <BC> passed was not reflective!"
 RETURN
END IF

!get the base point (the origin)
base_point = SPREAD(0._KIND_MSH,1,2)

!1. reflect the direction off the first plane
Pn = xyPLANE_PV( base_point , n1 )
!get xy-reflection
reflection(1:2) = xyREFLECT_PnV( Pn , REAL(u(1:2),KIND_MSH) )
!z-direction is unchanged by this xy refection
IF( SIZE(Ordinates,1)>2 )reflection(3) = u(3)

!evaluate direction index
m = GET_Direction(Ordinates,reflection)
IF( WithinCell(m)>=0 )RETURN


![begin::hack]
IF( PRESENT(n2) )THEN

 !2. reflect the direction off the second plane
 Pn = xyPLANE_PV( base_point , n2 )

 !get xy-reflection
 reflection(1:2) = xyREFLECT_PnV( Pn , REAL(u(1:2),KIND_MSH) )
 !z-direction is unchanged by this xy refection
 IF( SIZE(Ordinates,1)>2 )reflection(3) = u(3)

 !evaluate direction index
 m = GET_Direction(Ordinates,reflection)
 IF( WithinCell(m)>=0 )RETURN

END IF
![end::hack]

!if you get to here then we have an error
WRITE(*,*)"fatal error in EVAL_reflectiveBC!"
STOP

!!--end--
END FUNCTION


FUNCTION EVAL_VertFixedBC(BC,FixedAngularFlux,FunctionAngularFlux,Mesh,k,Ordinates,m,PolSin,Allow_Discontinuous,fdbk) RESULT(Val)
!!#### REQUIRED INPUT
INTEGER        ,INTENT(IN) :: BC(:)
REAL(KIND_AngularFlux),INTENT(IN) :: FixedAngularFlux(SIZE(BC))
INTEGER               ,INTENT(IN) :: FunctionAngularFlux(SIZE(BC))

TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
REAL(KIND_DOR) ,INTENT(IN) :: Ordinates(:,:)
INTEGER        ,INTENT(IN) :: k,m
REAL(KIND_DOR) ,INTENT(IN) :: PolSin(:)
LOGICAL        ,INTENT(IN) :: Allow_Discontinuous

!!#### REQUIRED OUTPUT
REAL(KIND_AngularFlux) :: val

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### LOCAL VARIABLES
INTEGER :: jd,jb1,jb2,jd1,jd2
INTEGER,SAVE :: m_last=0,k_last=0
REAL(KIND_MSH),DIMENSION(Mesh%NDim) :: outward_normal1,&
                                       outward_normal2,r1,r2
REAL(KIND_MSH) :: D1,D2,max0,val1,val2

!!--begin--

!get the boundary faces (jb1,jb2) and domain faces (jd1,jd2) that
!the vert is on
CALL BoundaryFacesFromVert(Mesh,k,jb1,jb2,jd1,jd2)

max0 = ABS(maxval( PolSin ))

!fix up for hanging verts
IF( jd2==0 )THEN
 jd2 = jd1
 jb2 = jb1
END IF

!non-corner verts
IF( jd1==jd2 )THEN

    outward_normal1 = DomainFaceNormal(Mesh,jd1)
    D1 = DOT_PRODUCT(Ordinates(1:2,m),outward_normal1)
    IF( D1<=0 )THEN
        r1 = Vert(Mesh,k)
        val = EVAL_fixedBC( BC , FixedAngularFlux,FunctionAngularFlux , fdbk, &
            jd1 , Mesh , Ordinates , m , max0-abs(Ordinates(3,m))  , r=r1 )
    ELSE
        val=-HUGE(1.d0)
    END IF

!corner verts
ELSE

    outward_normal1 = DomainFaceNormal(Mesh,jd1)
    outward_normal2 = DomainFaceNormal(Mesh,jd2)
    D1 = DOT_PRODUCT(Ordinates(1:2,m),outward_normal1)
    D2 = DOT_PRODUCT(Ordinates(1:2,m),outward_normal2)
    r1 = Vert(Mesh,k)
    val1 = EVAL_fixedBC( BC , FixedAngularFlux,FunctionAngularFlux , fdbk, &
        jd1 , Mesh , Ordinates , m , max0-abs(Ordinates(3,m))  , r=r1 )
    r2 = Vert(Mesh,k)
    val2 = EVAL_fixedBC( BC , FixedAngularFlux,FunctionAngularFlux , fdbk, &
        jd2 , Mesh , Ordinates , m , max0-abs(Ordinates(3,m))  , r=r2 )
    !incoming for both
    IF( D1<0 .AND. D2<0 )THEN
        IF( Allow_Discontinuous )THEN
            IF( (k_last /= k .AND. m_last /= m) )THEN
                k_last=k
                m_last=m
                CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MCS]] Discontinuous boundary conditions at&
                & corner of domain faces "//TRIM(STR(jd1))//" and "//TRIM(STR(jd2))//" with k="//&
                TRIM(STR(k))//" m="//TRIM(STR(m))//".")
            END IF
            IF( IsApprox(D1,D2) )THEN
                val = 0.5d0*(val1+val2)
            ELSEIF( D1<D2 )THEN
                val = val1
            ELSE
                val = val2
            END IF
        ELSE
            val=val1+val2
        END IF
    ELSEIF( D1<=0 )THEN
        val=val1
    ELSEIF( D2<=0 )THEN
        val=val2
    ELSE
        val=-HUGE(1.d0)
    END IF
END IF

!!--end--
END FUNCTION


!!### FUNCTION: <<EVAL_reflectiveBC_point>>
FUNCTION EVAL_reflectiveBC_point( m0 , Ordinates , n1 , n2 ) RESULT(m)

!!#### PURPOSE
!! Evaluate a reflective boundary condition, returning the
!! appropriate angular direction index which approximates the
!! the reflected direction best.  The normals of up to 2 planar
!! reflective surfaces may be provided.

!!#### GLOBAL VARIABLES
USE VAR_DiscreteOrdinates,ONLY: KIND_DOR                                  !!((47-B-VAR_DiscreteOrdinates.f90))

!!#### REQUIRED INPUT
!! * direction that needs reflection
!! * list of directions
!! * directions to reflect u off of [n1,n2]
INTEGER       ,INTENT(IN) :: m0
REAL(KIND_DOR),INTENT(IN) :: Ordinates(:,:)
REAL(KIND_MSH),INTENT(IN) :: n1(1:2)
REAL(KIND_MSH),INTENT(IN),DIMENSION(1:2),OPTIONAL :: n2

!!#### REQUIRED_OUTPUT
INTEGER :: m

!!#### LOCAL VARIABLES
REAL(KIND_DOR) :: u(3)
REAL(KIND_MSH) :: reflection(3)
REAL(KIND_MSH) :: base_point(2)
REAL(KIND_MSH) :: Pn(1:3)

!!--begin--

!get the base point (the origin)
base_point = 0._KIND_MSH

!1. reflect the direction off the first plane
Pn = xyPLANE_PV( base_point , n1 )

u = Ordinates(:,m0)

!get xy-reflection
reflection(1:2) = xyREFLECT_PnV( Pn , REAL(u(1:2),KIND_MSH) )
!z-direction is unchanged by this xy refection
reflection(3) = u(3)

IF( PRESENT(n2) )THEN

 !2. reflect the direction off the second plane
 Pn = xyPLANE_PV( base_point , n2 )

 !get xy-reflection
 reflection(1:2) = xyREFLECT_PnV( Pn , REAL(reflection(1:2),KIND_MSH) )

 !evaluate direction index
 m = GET_Direction(Ordinates,reflection)

END IF

!evaluate direction index
m = GET_Direction(Ordinates,reflection)

IF( m==0 )THEN
 WRITE(*,*)"problem with point reflection"
 STOP
END IF

!!--end--
END FUNCTION



SUBROUTINE CHECK_BalanceEquation(Mesh,Directions,&
  AngularFluxC,AngularFluxF,TotSourceCellFunction,MacT,l_,RecipSin,fdbk,&
  reltol,abstol,Tests)
USE LIB_Norm                                                              !!((04-B-LIB_Norm.f90))

TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
REAL(KIND_AngularFlux) :: Directions(:,:)
REAL(KIND_AngularFlux) :: AngularFluxF(:,:,:)
REAL(KIND_AngularFlux) :: AngularFluxC(:,:,:)
REAL(KIND_AngularFlux) :: TotSourceCellFunction(:,:,:),MacT(:,:),RecipSin(:)
INTEGER                :: l_(:)
REAL(KIND_AngularFlux) :: PsiF,PsiC,AreaVec(Mesh%NDim),Omega(Mesh%Ndim),Vol
REAL(KIND_AngularFlux) :: Balance(Mesh%NCells),Qavg
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk
CHARACTER(2),INTENT(IN),OPTIONAL :: Tests(:)

REAL(KIND_AngularFlux),INTENT(IN),OPTIONAL :: reltol,abstol
!!#### LOCAL VARIABLES
INTEGER :: g,m,i,n,j
LOGICAL :: Pass
REAL(KIND_AngularFlux) :: reltol_,abstol_,AvgSource
CHARACTER(2)           :: Tests_(6)
CHARACTER(2),PARAMETER :: ALL_TESTS(6)=(/"Ai","A1","A2","Ri","R1","R2"/)
REAL(KIND_AngularFlux) :: tol,result
CHARACTER(26) :: var
INTEGER :: N1,N2,N3
REAL(KIND_AngularFlux),ALLOCATABLE :: maxavg(:,:,:),abserr(:,:,:)

!!--begin--

IF( PRESENT(Tests) )THEN
 DO n=1,MAX(SIZE(Tests),6)
  Tests_(n) = Tests(n)
 END DO
ELSE
 Tests_ = ALL_TESTS
END IF

reltol_ = Default(1.d-10,reltol)
abstol_ = Default(1.d-16,abstol)
N1=SIZE(AngularFluxC,1)
N2=SIZE(AngularFluxC,2)
N3=SIZE(AngularFluxC,3)
ALLOCATE( abserr(N1,N2,N3) )
ALLOCATE( maxavg(N1,N2,N3) )

Pass = .TRUE.
DO g=1,N1
 DO i=1,N2
  AvgSource = EVAL_SourceAverage(Mesh,i,TotSourceCellFunction(:,g,i))
  DO m=1,N3
   Omega = Directions(1:2,m)
   abserr(g,i,m) = EVAL_BalanceEquation0(Mesh,i,Omega,AngularFluxC(g,i,m),&
       AngularFluxF(g,:,m),AvgSource,MacT(g,l_(i)),RecipSin(m),&
       maxavg=maxavg(g,i,m))
  END DO
 END DO
END DO

DO n=1,6
 IF( Tests_(n)=="  " )EXIT
 SELECT CASE(Tests_(n))
  CASE("Ai") ; tol = abstol
               result = NormInfty(abserr)
               var="abs. error (infinity norm)"
  CASE("A1") ; tol = abstol
               result = NormEll1(abserr)
               var="abs. error (L1 norm)"
  CASE("A2") ; tol = abstol
               result = NormEll2(abserr)
               var="abs. error (L2 norm)"
  CASE("Ri") ; tol = reltol
               result = NormInfty(abserr/maxavg)
               var="rel. error (infinity norm)"
  CASE("R1") ; tol = reltol
               result = NormEll1(abserr/NormEll1(maxavg))
               var="rel. error (L1 norm)"
               Pass=result<=reltol_
  CASE("R2") ; tol = reltol
               result = NormEll2(abserr/NormEll2(maxavg))
               var="rel. error (L2 norm)"
               Pass=result<=reltol_
 END SELECT
 Pass=result<=tol

 CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MCS]] Angular flux cell &
   &balance check "//TRIM(var)//" [result="//TRIM(STR(result,"(Es8.2)"))//"] &
   &using [tol="//TRIM(STR(tol,"(Es8.2)"))//"] we have &
   &[test="//MERGE("PASS","FAIL",Pass)//"].")

END DO


!!--end--
END SUBROUTINE




FUNCTION EVAL_BalanceEquation0(Mesh,i,Omega,&
  PsiC,PsiF,Q,MacT,RecipSin,relerr,maxavg) RESULT(Balance)
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER :: i
REAL(KIND_AngularFlux) :: PsiF(:)
REAL(KIND_AngularFlux) :: PsiC,MacT
REAL(KIND_AngularFlux) :: AreaVec(Mesh%NDim),Omega(Mesh%Ndim),Vol
REAL(KIND_AngularFlux) :: Balance,Q,RecipSin
REAL(KIND_AngularFlux),INTENT(OUT),OPTIONAL :: relerr,maxavg
INTEGER :: n,j
REAL(KIND_AngularFlux) :: maxavg_

!!--begin--
Vol     = CellVolume(Mesh,i)
Balance = 0._KIND_AngularFlux
maxavg_ = 0._KIND_AngularFlux

DO n=1,NUM_Faces_CellIndex(Mesh,i)
 j = Mesh%Cells(i)%FaceList(n)
 AreaVec  = FaceNormal(Mesh,j)*FaceArea(Mesh,ABS(j))
 Balance = Balance + DOT_PRODUCT(AreaVec,Omega)*PsiF(ABS(j))
 IF( PRESENT(relerr).OR. PRESENT(maxavg) )THEN
  maxavg_ = MAX(maxavg_,ABS(PsiF(ABS(j))))
 END IF
END DO

Balance = Balance + PsiC*MacT*Vol - Q*Vol

maxavg_ = MAXVAL( ABS( (/PsiC,Q,maxavg_/) ) )
IF( maxavg_==0._KIND_AngularFlux )THEN
 maxavg_=TINY(0._KIND_AngularFlux)
END IF

IF( PRESENT(relerr) )THEN
 relerr = ABS(Balance)/maxavg_
END IF
IF( PRESENT(maxavg) )THEN
 maxavg = maxavg_
END IF

!--end--
END FUNCTION



SUBROUTINE GET_LongBC(Mesh,BC,FixedAngularFlux,FunctionAngularFlux,Ordinates,fdbk,m,&
  r1,s01,r0,psi0)
INTEGER               ,INTENT(IN) :: BC(:)
REAL(KIND_AngularFlux),INTENT(IN) :: FixedAngularFlux(SIZE(BC))
INTEGER               ,INTENT(IN) :: FunctionAngularFlux(SIZE(BC))
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh
REAL(KIND_AngularFlux),INTENT(IN) :: r1(2),Ordinates(:,:)
TYPE(TYPE_fdbk),INTENT(INOUT) :: fdbk
REAL(KIND_AngularFlux),INTENT(OUT) :: r0(2)
REAL(KIND_AngularFlux),INTENT(OUT) :: s01,psi0
INTEGER,INTENT(IN) :: m
INTEGER :: jbdry
REAL(KIND_MSH),DIMENSION(2) :: FCb,FCr,FCt,FCl
REAL(KIND_AngularFlux) :: sxl,sxr,syt,syb,Omega(3)
!!--begin--

Omega = Ordinates(:,m)

!*!*!*!*!*!*!!*!*!*!*!*!*!*!*!*!*!**!*!*!*!*
!assumes rectangular domain oriented faces:
! bottom, right, top, left
!*!*!*!*!*!*!**!*!*!*!*!*!*!*!*!*!**!*!*!*!*
FCb = DomainFaceCentroid(Mesh,1)
FCr = DomainFaceCentroid(Mesh,2)
FCt = DomainFaceCentroid(Mesh,3)
FCl = DomainFaceCentroid(Mesh,4)

!bottom boundary
syb = (r1(2) - FCb(2) )/(Omega(2)+1.d-20)
!right boundary
sxr = (r1(1) - FCr(1) )/(Omega(1)+1.d-20)
!top boundary
syt = (r1(2) - FCt(2) )/(Omega(2)+1.d-20)
!left boundary
sxl = (r1(1) - FCl(1) )/(Omega(1)+1.d-20)


!*!*!*!*!*!**!*!*!*!**!*!*!*
!*!*!*!*!*!*!*!**!*!**!*!**!

!characteristic points to the left

!left side
IF( sxl>sxr )THEN

 !bottom or left
 IF( syb>syt )THEN
   s01 = MERGE(syb,sxl,syb<sxl)
   jbdry    = MERGE(  1,  4,syb<sxl)

 !top or left
 ELSE
   s01 = MERGE(syt,sxl,syt<sxl)
   jbdry    = MERGE(  3,  4,syt<sxl)

 END IF

!right side
ELSE

 !bottom or right
 IF( syb>syt )THEN
   s01 = MERGE(syb,sxr,syb<sxr)
   jbdry    = MERGE(  1,  2,syb<sxr)

 !top or right
 ELSE
   s01 = MERGE(syt,sxr,syt<sxr)
   jbdry    = MERGE(  3,  2,syt<sxr)

 END IF

END IF

!get position on boundary
r0 = r1 - Omega(1:Mesh%Ndim)*s01

![hack]cannot handle reflective bc
psi0 = EVAL_fixedBC( BC , FixedAngularFlux,FunctionAngularFlux , fdbk, &
  jbdry , Mesh , Ordinates , r=r0 , m=m )


!!--end--
END SUBROUTINE



!!### SUBROUTINE <<ReportNonPositivity>>
SUBROUTINE ReportNonPositivity( AngularFlux , VariantName , IndexNames , fdbk )

!!#### PURPOSE
!! Report angular fluxes non-positivity.

!!#### REQUIRED INPUT
REAL(KIND_AngularFlux),INTENT(IN) :: AngularFlux(:,:,:)
CHARACTER(*)          ,INTENT(IN) :: VariantName
CHARACTER(*)          ,INTENT(IN) :: IndexNames(3)

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
INTEGER :: n1,n2,n3

!!--begin--

DO n3=1,SIZE(AngularFlux,3)
 DO n2=1,SIZE(AngularFlux,2)
  DO n1=1,SIZE(AngularFlux,1)
   IF( AngularFlux(n1,n2,n3)<0._KIND_AngularFlux )THEN
    CALL UPDATE(fdbk_warning,fdbk,s="Negative value for "//&
      VariantName//&
      "("//IndexNames(1)//"="//TRIM(STR(n1))//","//&
           IndexNames(2)//"="//TRIM(STR(n2))//","//&
           IndexNames(3)//"="//TRIM(STR(n3))//")"//"="//&
           TRIM(STR(AngularFlux(n1,n2,n3),"(E)")) )
   END IF
  END DO
 END DO
END DO

!!--end--
END SUBROUTINE



END MODULE
