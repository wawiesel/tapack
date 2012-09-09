MODULE USR_QDAnalyticTest
USE KND_IntrinsicTypes !!((01-A-KND_IntrinsicTypes.f90))
USE KND_QuasiDiffusion !!((02-A-KND_QuasiDiffusion.f90))
USE PAR_Constants_Rdp  !!((02-A-PAR_Constants_Rdp.f90))
USE TBX_Mesh           !!((15-B-TBX_Mesh.f90))
USE SUB_SimpSurf       !!((08-C-SUB_SimpSurf.f90))
USE VAR_QDAnalyticTest !!((33-C-VAR_QDAnalyticTest.f90))
USE FUN_NewFile        !!((05-B-FUN_NewFile.f90))

IMPLICIT NONE

PRIVATE


!!#### PUBLIC ACCESS LIST
PUBLIC :: EXACT_Phi
PUBLIC :: EXACT_Jx,EXACT_Jy,EXACT_J
PUBLIC :: EXACT_Q
PUBLIC :: EXACT_Exx,EXACT_Exy,EXACT_Eyy

PUBLIC :: EXACT_dJx_dx,EXACT_dJy_dy

PUBLIC :: EXACT_PhiF,EXACT_PhiC
PUBLIC :: EXACT_JF,EXACT_JFN,EXACT_JC
PUBLIC :: EXACT_QC
PUBLIC :: EXACT_ExxF,EXACT_ExyF,EXACT_EyyF
PUBLIC :: EXACT_ExxC,EXACT_ExyC,EXACT_EyyC

PUBLIC :: SETUP_AnalyticData
PUBLIC :: GNUPLOT_AnalyticData
PUBLIC :: Using_AnalyticSource
PUBLIC :: AnalyticTestCase


!some "implicit" variables needed
REAL(KIND_Rdp) :: t1,t2,t3,t4,t5,t6,t7,t8,t9,t10
REAL(KIND_Rdp) :: t11,t12,t13,t14,t15,t16,t17,t18,t19,t20
REAL(KIND_Rdp) :: t21,t22,t23,t24,t25,t26,t27,t28,t29,t30
REAL(KIND_Rdp) :: t31,t32,t33,t34,t35,t36,t37,t38,t39,t40
REAL(KIND_Rdp) :: t41,t42,t43,t44,t45,t46,t47,t48,t49,t50
REAL(KIND_Rdp) :: t51,t52,t53

CONTAINS



!!--begin pointwise routines----------------------------------------------


FUNCTION EXACT_Phi( x , y ) RESULT(Phi)
USE KND_IntrinsicTypes !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_Rdp),INTENT(IN) :: x,y
REAL(KIND_Rdp) :: Phi

!!--begin--

SELECT CASE(AnalyticTestCase)
 CASE(1) ; Phi = Phi__1(x,y)
 CASE(4) ; Phi = Phi__4(x,y)
 CASE(5) ; Phi = Phi__5(x,y)
 CASE DEFAULT
 WRITE(*,*)'unknown AnalyticTestCase=',AnalyticTestCase,' (should be 1, 4, or 5)'
 STOP
END SELECT

!!--end--
END FUNCTION



FUNCTION EXACT_Jx( x , y ) RESULT(Jx)
USE KND_IntrinsicTypes !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_Rdp),INTENT(IN) :: x,y
REAL(KIND_Rdp) :: Jx
!!--begin--

SELECT CASE(AnalyticTestCase)
 CASE(1) ; Jx = Jx__1(x,y)
 CASE(4) ; Jx = Jx__4(x,y)
 CASE(5) ; Jx = Jx__5(x,y)
END SELECT

!!--end--
END FUNCTION



FUNCTION EXACT_Jy( x , y ) RESULT(Jy)
USE KND_IntrinsicTypes !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_Rdp),INTENT(IN) :: x,y
REAL(KIND_Rdp) :: Jy
!!--begin--

SELECT CASE(AnalyticTestCase)
 CASE(1) ; Jy = Jy__1(x,y)
 CASE(4) ; Jy = Jy__4(x,y)
 CASE(5) ; Jy = Jy__5(x,y)
END SELECT

!!--end--
END FUNCTION


FUNCTION EXACT_J( x , y ) RESULT(J)
USE KND_IntrinsicTypes !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_Rdp),INTENT(IN) :: x,y
REAL(KIND_Rdp) :: J(2)
!!--begin--

SELECT CASE(AnalyticTestCase)
 CASE(1) ; J(1) = Jx__1(x,y)
           J(2) = Jy__1(x,y)
 CASE(4) ; J(1) = Jx__4(x,y)
           J(2) = Jy__4(x,y)
 CASE(5) ; J(1) = Jx__5(x,y)
           J(2) = Jy__5(x,y)
END SELECT

!!--end--
END FUNCTION


FUNCTION EXACT_dJx_dx( x , y ) RESULT(dJx_dx)
USE KND_IntrinsicTypes !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_Rdp),INTENT(IN) :: x,y
REAL(KIND_Rdp) :: dJx_dx
!!--begin--

SELECT CASE(AnalyticTestCase)
 CASE(1) ; dJx_dx = dJx_dx__1(x,y)
 CASE(4) ; dJx_dx = dJx_dx__4(x,y)
 CASE(5) ; dJx_dx = dJx_dx__5(x,y)
END SELECT

!!--end--
END FUNCTION



FUNCTION EXACT_dJy_dy( x , y ) RESULT(dJy_dy)
USE KND_IntrinsicTypes !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_Rdp),INTENT(IN) :: x,y
REAL(KIND_Rdp) :: dJy_dy
!!--begin--

SELECT CASE(AnalyticTestCase)
 CASE(1) ; dJy_dy = dJy_dy__1(x,y)
 CASE(4) ; dJy_dy = dJy_dy__4(x,y)
 CASE(5) ; dJy_dy = dJy_dy__5(x,y)
END SELECT

!!--end--
END FUNCTION



FUNCTION EXACT_Q( x , y ) RESULT(Q)
USE KND_IntrinsicTypes !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_Rdp),INTENT(IN) :: x,y
REAL(KIND_Rdp) :: Q
!!--begin--

SELECT CASE(AnalyticTestCase)
 CASE(1) ; Q = Q__1(x,y)
 CASE(4) ; Q = Q__4(x,y)
 CASE(5) ; Q = Q__5(x,y)
END SELECT

!!--end--
END FUNCTION



FUNCTION EXACT_Exx( x , y ) RESULT(Exx)
USE KND_IntrinsicTypes !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_Rdp),INTENT(IN) :: x,y
REAL(KIND_Rdp) :: Exx
!!--begin--

SELECT CASE(AnalyticTestCase)
 CASE(1) ; Exx = Exx__1(x,y)
 CASE(4) ; Exx = Exx__4(x,y)
 CASE(5) ; Exx = Exx__5(x,y)
END SELECT

!!--end--
END FUNCTION



FUNCTION EXACT_Exy( x , y ) RESULT(Exy)
USE KND_IntrinsicTypes !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_Rdp),INTENT(IN) :: x,y
REAL(KIND_Rdp) :: Exy
!!--begin--

SELECT CASE(AnalyticTestCase)
 CASE(1) ; Exy = Exy__1(x,y)
 CASE(4) ; Exy = Exy__4(x,y)
 CASE(5) ; Exy = Exy__5(x,y)
END SELECT

!!--end--
END FUNCTION



FUNCTION EXACT_Eyy( x , y ) RESULT(Eyy)
USE KND_IntrinsicTypes !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_Rdp),INTENT(IN) :: x,y
REAL(KIND_Rdp) :: Eyy
!!--begin--

SELECT CASE(AnalyticTestCase)
 CASE(1) ; Eyy = Eyy__1(x,y)
 CASE(4) ; Eyy = Eyy__4(x,y)
 CASE(5) ; Eyy = Eyy__5(x,y)
END SELECT

!!--end--
END FUNCTION


!!--end pointwise routines----------------------------------------------



!!--begin face-average routines----------------------------------------------


FUNCTION EXACT_PhiF(Mesh,j) RESULT(PhiF)
!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER                     :: j

!!#### REQUIRED OUTPUT
REAL(KIND_QDF) :: PhiF

!!#### LOCAL VARIABLES
REAL(KIND_MSH) :: FC(2)

!!--begin--

SELECT CASE(EvaluationMethod_Phi)

 CASE(Average_)   ; PhiF = FaceAverage_F(Mesh,j,EXACT_Phi)

 CASE(Centroid_)  ; FC   = FaceCentroid(Mesh,j)
                    PhiF = EXACT_Phi( FC(1) , FC(2) )
END SELECT

!!--end--
END FUNCTION



FUNCTION EXACT_JF(Mesh,j) RESULT(JF)
!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER                    :: j

!!#### REQUIRED OUTPUT
REAL(KIND_QDF) :: JF(2)

!!#### LOCAL VARIABLES
REAL(KIND_MSH) :: FC(2)

!!--begin--

SELECT CASE(EvaluationMethod_J)

 CASE(Average_)   ; JF(1) = FaceAverage_F(Mesh,j,EXACT_Jx)
                    JF(2) = FaceAverage_F(Mesh,j,EXACT_Jy)

 CASE(Centroid_)  ; FC = FaceCentroid(Mesh,j)
                    JF = EXACT_J( FC(1) , FC(2) )
END SELECT

!!--end--
END FUNCTION


FUNCTION EXACT_JxF(Mesh,j) RESULT(JxF)
!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER                    :: j

!!#### REQUIRED OUTPUT
REAL(KIND_QDF) :: JxF

!!#### LOCAL VARIABLES
REAL(KIND_QDF) :: FC(2)

!!--begin--

SELECT CASE(EvaluationMethod_J)

 CASE(Average_)   ; JxF = FaceAverage_F(Mesh,j,EXACT_Jx)

 CASE(Centroid_)  ; FC  = FaceCentroid(Mesh,j)
                    JxF = EXACT_Jx( FC(1) , FC(2) )
END SELECT

!!--end--
END FUNCTION


FUNCTION EXACT_JyF(Mesh,j) RESULT(JyF)
!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER                    :: j

!!#### REQUIRED OUTPUT
REAL(KIND_QDF) :: JyF

!!#### LOCAL VARIABLES
REAL(KIND_QDF) :: FC(2)

!!--begin--

SELECT CASE(EvaluationMethod_J)

 CASE(Average_)   ; JyF = FaceAverage_F(Mesh,j,EXACT_Jy)

 CASE(Centroid_)  ; FC  = FaceCentroid(Mesh,j)
                    JyF = EXACT_Jy( FC(1) , FC(2) )
END SELECT

!!--end--
END FUNCTION


FUNCTION EXACT_JFN(Mesh,j) RESULT(JFN)
!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER                    :: j

!!#### REQUIRED OUTPUT
REAL(KIND_QDF) :: JFN

!!--begin--

JFN = DOT_PRODUCT( FaceNormal(Mesh,j) , EXACT_JF(Mesh,j) )

!!--end--
END FUNCTION



FUNCTION EXACT_ExxF(Mesh,j) RESULT(ExxF)
!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER                    :: j

!!#### REQUIRED OUTPUT
REAL(KIND_QDF) :: ExxF

!!#### LOCAL VARIABLES
REAL(KIND_MSH) :: FC(2)

!!--begin--

SELECT CASE(EvaluationMethod_E)

 CASE(Average_)   ; ExxF = FaceAverage_F(Mesh,j,EXACT_Exx)

 CASE(Centroid_)  ; FC   = FaceCentroid(Mesh,j)
                    ExxF = EXACT_Exx( FC(1) , FC(2) )
END SELECT

!!--end--
END FUNCTION



FUNCTION EXACT_EyyF(Mesh,j) RESULT(EyyF)
!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER                    :: j

!!#### REQUIRED OUTPUT
REAL(KIND_QDF) :: EyyF

!!#### LOCAL VARIABLES
REAL(KIND_MSH) :: FC(2)

!!--begin--

SELECT CASE(EvaluationMethod_E)

 CASE(Average_)   ; EyyF = FaceAverage_F(Mesh,j,EXACT_Eyy)

 CASE(Centroid_)  ; FC   = FaceCentroid(Mesh,j)
                    EyyF = EXACT_Eyy( FC(1) , FC(2) )
END SELECT

!!--end--
END FUNCTION



FUNCTION EXACT_ExyF(Mesh,j) RESULT(ExyF)
!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER                    :: j

!!#### REQUIRED OUTPUT
REAL(KIND_QDF) :: ExyF

!!#### LOCAL VARIABLES
REAL(KIND_MSH) :: FC(2)

!!--begin--

SELECT CASE(EvaluationMethod_E)

 CASE(Average_)   ; ExyF = FaceAverage_F(Mesh,j,EXACT_Exy)

 CASE(Centroid_)  ; FC   = FaceCentroid(Mesh,j)
                    ExyF = EXACT_Exy( FC(1) , FC(2) )
END SELECT

!!--end--
END FUNCTION


!!--end face-average routines----------------------------------------------



!!--begin cell-average routines----------------------------------------------


FUNCTION EXACT_ExxC(Mesh,i) RESULT(ExxC)
!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER                    :: i

!!#### REQUIRED OUTPUT
REAL(KIND_QDF) :: ExxC

!!#### LOCAL VARIABLES
REAL(KIND_MSH) :: CC(2)

!!--begin--

SELECT CASE(EvaluationMethod_E)

 CASE(Average_)   ; ExxC = CellAverage_F(Mesh,i,EXACT_Exx)

 CASE(Centroid_)  ; CC   = CellCentroid(Mesh,i)
                    ExxC = EXACT_Exx( CC(1) , CC(2) )
END SELECT

!!--end--
END FUNCTION



FUNCTION EXACT_EyyC(Mesh,i) RESULT(EyyC)
!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER                    :: i

!!#### REQUIRED OUTPUT
REAL(KIND_QDF) :: EyyC

!!#### LOCAL VARIABLES
REAL(KIND_MSH) :: CC(2)

!!--begin--

SELECT CASE(EvaluationMethod_E)

 CASE(Average_)   ; EyyC = CellAverage_F(Mesh,i,EXACT_Eyy)

 CASE(Centroid_)  ; CC   = CellCentroid(Mesh,i)
                    EyyC = EXACT_Eyy( CC(1) , CC(2) )
END SELECT

!!--end--
END FUNCTION



FUNCTION EXACT_ExyC(Mesh,i) RESULT(ExyC)
!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER                    :: i

!!#### REQUIRED OUTPUT
REAL(KIND_QDF) :: ExyC

!!#### LOCAL VARIABLES
REAL(KIND_MSH) :: CC(2)

!!--begin--

SELECT CASE(EvaluationMethod_E)

 CASE(Average_)   ; ExyC = CellAverage_F(Mesh,i,EXACT_Exy)

 CASE(Centroid_)  ; CC   = CellCentroid(Mesh,i)
                    ExyC = EXACT_Exy( CC(1) , CC(2) )
END SELECT

!!--end--
END FUNCTION


FUNCTION EXACT_PhiC(Mesh,i) RESULT(PhiC)
!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER                    :: i

!!#### REQUIRED OUTPUT
REAL(KIND_QDF) :: PhiC

!!#### LOCAL VARIABLES
REAL(KIND_MSH) :: CC(2)

!!--begin--

SELECT CASE(EvaluationMethod_Phi)

 CASE(Average_)   ; PhiC = CellAverage_F(Mesh,i,EXACT_Phi)

 CASE(Centroid_)  ; CC   = CellCentroid(Mesh,i)
                    PhiC = EXACT_Phi( CC(1) , CC(2) )
END SELECT

!!--end--
END FUNCTION


FUNCTION EXACT_JC(Mesh,i) RESULT(JC)
!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER                    :: i

!!#### REQUIRED OUTPUT
REAL(KIND_QDF) :: JC(2)

!!#### LOCAL VARIABLES
REAL(KIND_MSH) :: CC(2)

!!--begin--

SELECT CASE(EvaluationMethod_J)

 CASE(Average_)   ; JC(1) = CellAverage_F(Mesh,i,EXACT_Jx)
                    JC(2) = CellAverage_F(Mesh,i,EXACT_Jy)

 CASE(Centroid_)  ; CC    = CellCentroid(Mesh,i)
                    JC    = EXACT_J( CC(1) , CC(2) )
END SELECT

!!--end--
END FUNCTION



FUNCTION EXACT_QC(Mesh,i) RESULT(QC)
!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER                    :: i

!!#### REQUIRED OUTPUT
REAL(KIND_QDF) :: QC

!!#### LOCAL VARIABLES
REAL(KIND_MSH) :: CC(2)

!!--begin--

SELECT CASE(EvaluationMethod_Q)

 CASE(Average_)    ; QC = CellAverage_F(Mesh,i,EXACT_Q)

 CASE(Centroid_)   ; CC = CellCentroid(Mesh,i)
                     QC = EXACT_Q( CC(1) , CC(2) )

 CASE(Balance_)    ; QC = BalanceEquation(Mesh,i)

END SELECT

!!--end--
END FUNCTION



FUNCTION EXACT_QCfun(x,y) RESULT(QC)
!!#### PURPOSE
!! Return the cell-average source from (x,y)
!! coordinates by finding the cell we're in and evaluating
!! that cell-average source.

!!#### MODULES
!need the <Mesh> object but must keep interface with only <x> and <y>
USE VAR_Mesh           !!((46-B-VAR_Mesh.f90))

!!#### REQUIRED INPUT
REAL(KIND_QDF),INTENT(IN) :: x,y

!!#### REQUIRED OUTPUT
REAL(KIND_QDF) :: QC

!!#### LOCAL VARIABLES
INTEGER        :: i

!!--begin--

i  = InteriorCell(Mesh,(/x,y/))
QC = EXACT_QC(Mesh,i)

!!--end--
END FUNCTION


!!--end cell-average routines-----------------------------------------



!!### SETUP SUBROUTINE: <SETUP_AnalyticData>
SUBROUTINE SETUP_AnalyticData(Mesh,ExxF,EyyF,ExyF,&
   ExxC,EyyC,ExyC)

!!#### PURPOSE
!! Set the face-average and cell-average E's.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh
REAL(KIND_QDF) ,INTENT(INOUT) :: ExxF(:,:),EyyF(:,:),ExyF(:,:)
REAL(KIND_QDF) ,INTENT(INOUT) :: ExxC(:,:),EyyC(:,:),ExyC(:,:)

!!#### LOCAL VARIABLES
INTEGER :: j,i

!!--begin--

DO i=1,NUM_Cells(Mesh)

 ExxC(1,i)      = EXACT_ExxC(Mesh,i)
 EyyC(1,i)      = EXACT_EyyC(Mesh,i)
 ExyC(1,i)      = EXACT_ExyC(Mesh,i)

END DO

DO j=1,NUM_Faces(Mesh)

 ExxF(1,j)      = EXACT_ExxF(Mesh,j)
 EyyF(1,j)      = EXACT_EyyF(Mesh,j)
 ExyF(1,j)      = EXACT_ExyF(Mesh,j)

END DO

!!--end--
END SUBROUTINE




SUBROUTINE GNUPLOT_AnalyticData()
!!#### PURPOSE
!! Dump the analytic test data to some files gnuplot can read.

!!#### LOCAL VARIABLES
INTEGER :: Unit
REAL(KIND_QDF) :: xseq(3),yseq(3)
LOGICAL :: MakeTracks

!!--begin--

xseq = (/0.d0,0.01d0,1.001d0/)
yseq = xseq
MakeTracks = .FALSE.

Unit = NewFile("QC")
CALL SimpSurf(Unit,EXACT_QCfun   ,xseq,yseq,MakeTracks=MakeTracks)
CLOSE( Unit )

Unit = NewFile("Qfun")
CALL SimpSurf(Unit,EXACT_Q       ,xseq,yseq,MakeTracks=MakeTracks)
CLOSE( Unit )

Unit = NewFile("Phi")
CALL SimpSurf(Unit,EXACT_Phi     ,xseq,yseq,MakeTracks=MakeTracks)
CLOSE( Unit )

Unit = NewFile("Exx")
CALL SimpSurf(Unit,EXACT_Exx     ,xseq,yseq,MakeTracks=MakeTracks)
CLOSE( Unit )

Unit = NewFile("Exy")
CALL SimpSurf(Unit,EXACT_Exy     ,xseq,yseq,MakeTracks=MakeTracks)
CLOSE( Unit )

Unit = NewFile("Eyy")
CALL SimpSurf(Unit,EXACT_Eyy     ,xseq,yseq,MakeTracks=MakeTracks)
CLOSE( Unit )

Unit = NewFile("Jx")
CALL SimpSurf(Unit,EXACT_Jx      ,xseq,yseq,MakeTracks=MakeTracks)
CLOSE( Unit )

Unit = NewFile("Jy")
CALL SimpSurf(Unit,EXACT_Jy      ,xseq,yseq,MakeTracks=MakeTracks)
CLOSE( Unit )

Unit = NewFile("dJx_dx")
CALL SimpSurf(Unit,EXACT_dJx_dx  ,xseq,yseq,MakeTracks=MakeTracks)
CLOSE( Unit )

Unit = NewFile("dJy_dy")
CALL SimpSurf(Unit,EXACT_dJy_dy  ,xseq,yseq,MakeTracks=MakeTracks)
CLOSE( Unit )

!!--end--
END SUBROUTINE



FUNCTION BalanceEquation( Mesh , i ) RESULT(QC)

!!#### PURPOSE
!! Evaluate the average source within a cell using the
!! balance equation with average values calculated
!! via numerical integration of currents and scalar fluxes.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER        ,INTENT(IN) :: i

!!#### REQUIRED OUTPUT
REAL(KIND_QDF) :: QC

!!#### LOCAL VARIABLES
INTEGER        :: n,j
REAL(KIND_QDF) :: CV

!!--begin--

QC = siga*EXACT_PhiC(Mesh,i)
CV = CellVolume(Mesh,i)

DO n=1,NUM_Faces( Mesh%Cells(i) )

 j  = Mesh%Cells(i)%FaceList(n)

 QC = QC + EXACT_JFN(Mesh,j)*FaceArea(Mesh,j)/CV

END DO

!!--end--
END FUNCTION








!!**********************************************
!! __1 case
!!**********************************************

FUNCTION Phi__1( x , y ) RESULT(Phi)
REAL(KIND_QDF) :: Phi
REAL(KIND_QDF) :: x,y
!!--begin--

Phi = c_1 - TANH( c_5*(x-c_1_by_2)**2 + &
                  c_5*(y-c_1_by_2)**2 )
!!--end--
END FUNCTION


FUNCTION Jx__1( x , y ) RESULT(Jx)
REAL(KIND_QDF) :: Jx
REAL(KIND_QDF) :: x,y
REAL(KIND_QDF) :: sigt
!!--begin--

sigt = c_1

Jx = -( dExxPhi_dx__1(x,y) + &
        dExyPhi_dy__1(x,y)  )/sigt

!!--end--
END FUNCTION


FUNCTION Jy__1( x , y ) RESULT(Jy)
REAL(KIND_QDF) :: Jy
REAL(KIND_QDF) :: x,y
REAL(KIND_QDF) :: sigt

!!--begin--

sigt = c_1

Jy = -( dExyPhi_dx__1(x,y) + &
        dEyyPhi_dy__1(x,y)  )/sigt

!!--end--
END FUNCTION



FUNCTION dExyPhi_dx__1( x , y ) RESULT(dExyPhi_dx)
REAL(KIND_QDF) :: x,y
REAL(KIND_QDF) :: dExyPhi_dx
!!--begin--

dExyPhi_dx = Exy__1(x,y)*dPhi_dx__1(x,y) + &
             Phi__1(x,y)*dExy_dx__1(x,y)

!!--end--
END FUNCTION

FUNCTION dExyPhi_dy__1( x , y ) RESULT(dExyPhi_dy)
REAL(KIND_QDF) :: x,y
REAL(KIND_QDF) :: dExyPhi_dy
!!--begin--

dExyPhi_dy = Exy__1(x,y)*dPhi_dy__1(x,y) + &
             Phi__1(x,y)*dExy_dy__1(x,y)

!!--end--
END FUNCTION


FUNCTION dPhi_dx__1( x , y ) RESULT(dPhi_dx)
REAL(KIND_QDF) :: dPhi_dx
REAL(KIND_QDF) :: x,y
!!--begin--

dPhi_dx = -c_10*(x-c_1_by_2)/&
          ( COSH(c_5*( (x-c_1_by_2)**2 + &
                       (y-c_1_by_2)**2 )) )**2

!!--end--
END FUNCTION


FUNCTION dPhi_dy__1( x , y ) RESULT(dPhi_dy)
REAL(KIND_QDF) :: dPhi_dy
REAL(KIND_QDF) :: x,y
!!--begin--

dPhi_dy = -c_10*(y-c_1_by_2)/&
          ( COSH(c_5*( (x-c_1_by_2)**2 + &
                       (y-c_1_by_2)**2 )) )**2

!!--end--
END FUNCTION


FUNCTION d2Phi_dx2__1( x , y ) RESULT(d2Phi_dx2)
REAL(KIND_QDF) :: d2Phi_dx2
REAL(KIND_QDF) :: x,y
REAL(KIND_QDF) :: xn,yn,xns,yns,numer,denom
!!--begin--
xn = (x-c_1_by_2)
yn = (y-c_1_by_2)
xns = xn**2
yns = yn**2
denom = ( COSH(c_5*(xns+yns)) )**2
numer = c_1 - c_20*xns*TANH( c_5*( xns + yns ) )
d2Phi_dx2 = -c_10*numer/denom
!!--end--
END FUNCTION

FUNCTION d2Phi_dy2__1( x , y ) RESULT(d2Phi_dy2)
REAL(KIND_QDF) :: d2Phi_dy2
REAL(KIND_QDF) :: x,y
REAL(KIND_QDF) :: xn,yn,xns,yns,numer,denom
!!--begin--
xn = (x-c_1_by_2)
yn = (y-c_1_by_2)
xns = xn**2
yns = yn**2
denom = ( COSH(c_5*(xns+yns)) )**2
numer = c_1 - c_20*yns*TANH( c_5*( xns + yns ) )
d2Phi_dy2 = -c_10*numer/denom
!!--end--
END FUNCTION


FUNCTION d2Phi_dxdy__1( x , y ) RESULT(d2Phi_dxdy)
REAL(KIND_QDF) :: d2Phi_dxdy
REAL(KIND_QDF) :: x,y
REAL(KIND_QDF) :: xn,yn,xns,yns,numer,denom
!!--begin--
xn = (x-c_1_by_2)
yn = (y-c_1_by_2)
xns = xn**2
yns = yn**2
denom = ( COSH(c_5*(xns+yns)) )**2
numer = xn*yn*TANH( c_5*( xns + yns ) )
d2Phi_dxdy = c_10*numer/denom
!!--end--
END FUNCTION



FUNCTION dJx_dx__1( x , y ) RESULT(dJx_dx)
REAL(KIND_QDF) :: dJx_dx
REAL(KIND_QDF) :: x,y
REAL(KIND_QDF) :: sigt
!!--begin--

sigt = c_1

dJx_dx = -(  d2ExxPhi_dx2__1(x,y) + &
            d2ExyPhi_dxdy__1(x,y)  )/sigt

!!--end--
END FUNCTION


FUNCTION dJy_dy__1( x , y ) RESULT(dJy_dy)
REAL(KIND_QDF) :: dJy_dy
REAL(KIND_QDF) :: x,y
REAL(KIND_QDF) :: sigt

!!--begin--

sigt = c_1

dJy_dy = -( d2ExyPhi_dxdy__1(x,y) + &
            d2EyyPhi_dy2__1(x,y)  )/sigt

!!--end--
END FUNCTION



FUNCTION Exx__1( x , y ) RESULT(Exx)
REAL(KIND_QDF) :: x,y
REAL(KIND_QDF) :: Exx
REAL(KIND_QDF) :: xn,xns,yn,yns

!!--begin--

xn  = (x-c_1_by_2)
xns = xn**2
yn  = (y-c_1_by_2)
yns = yn**2

Exx = c_1_by_3*( c_1 + xns + yns )

!!--end--
END FUNCTION


FUNCTION Exy__1( x , y ) RESULT(Exy)
REAL(KIND_QDF) :: x,y
REAL(KIND_QDF) :: Exy
REAL(KIND_QDF) :: xn,xns,yn,yns

!!--begin--

xn  = (x-c_1_by_2)
xns = xn**2
yn  = (y-c_1_by_2)
yns = yn**2

Exy = xns*yns

!!--end--
END FUNCTION



FUNCTION Eyy__1( x , y ) RESULT(Eyy)
REAL(KIND_QDF) :: x,y
REAL(KIND_QDF) :: Eyy
REAL(KIND_QDF) :: xn,xns,yn,yns

!!--begin--

xn  = (x-c_1_by_2)
xns = xn**2
yn  = (y-c_1_by_2)
yns = yn**2

Eyy = c_1_by_3*( c_1 + xns + yns )

!!--end--
END FUNCTION



FUNCTION dExx_dx__1( x , y ) RESULT(dExx_dx)
REAL(KIND_QDF) :: x,y
REAL(KIND_QDF) :: dExx_dx
!!--begin--

dExx_dx = c_2_by_3*(x-c_1_by_2)

!!--end--
END FUNCTION


FUNCTION d2Exx_dx2__1( x , y ) RESULT(d2Exx_dx2)
REAL(KIND_QDF) :: x,y
REAL(KIND_QDF) :: d2Exx_dx2
!!--begin--

d2Exx_dx2 = c_2_by_3*x

!!--end--
END FUNCTION


FUNCTION dEyy_dy__1( x , y ) RESULT(dEyy_dy)
REAL(KIND_QDF) :: x,y
REAL(KIND_QDF) :: dEyy_dy
!!--begin--

dEyy_dy = c_2_by_3*(y-c_1_by_2)

!!--end--
END FUNCTION

FUNCTION d2Eyy_dy2__1( x , y ) RESULT(d2Eyy_dy2)
REAL(KIND_QDF) :: x,y
REAL(KIND_QDF) :: d2Eyy_dy2
!!--begin--

d2Eyy_dy2 = c_2_by_3*y

!!--end--
END FUNCTION

FUNCTION dExy_dx__1( x , y ) RESULT(dExy_dx)
REAL(KIND_QDF) :: x,y
REAL(KIND_QDF) :: dExy_dx
REAL(KIND_QDF) :: xn,xns,yn,yns

!!--begin--

xn  = (x-c_1_by_2)
xns = xn**2
yn  = (y-c_1_by_2)
yns = yn**2

dExy_dx = c_2*xn*yns

!!--end--
END FUNCTION


FUNCTION dExy_dy__1( x , y ) RESULT(dExy_dy)
REAL(KIND_QDF) :: x,y
REAL(KIND_QDF) :: dExy_dy
REAL(KIND_QDF) :: xn,xns,yn,yns

!!--begin--

xn  = (x-c_1_by_2)
xns = xn**2
yn  = (y-c_1_by_2)
yns = yn**2

dExy_dy = c_2*xns*yn

!!--end--
END FUNCTION


FUNCTION d2Exy_dxdy__1( x , y ) RESULT(d2Exy_dxdy)
REAL(KIND_QDF) :: x,y
REAL(KIND_QDF) :: d2Exy_dxdy
REAL(KIND_QDF) :: xn,xns,yn,yns

!!--begin--

xn  = (x-c_1_by_2)
xns = xn**2
yn  = (y-c_1_by_2)
yns = yn**2

d2Exy_dxdy = c_4*xn*yn

!!--end--
END FUNCTION


FUNCTION dExxPhi_dx__1( x , y ) RESULT(dExxPhi_dx)
REAL(KIND_QDF) :: x,y
REAL(KIND_QDF) :: dExxPhi_dx
!!--begin--
dExxPhi_dx = Exx__1(x,y)*dPhi_dx__1(x,y) + &
             Phi__1(x,y)*dExx_dx__1(x,y)
!!--end--
END FUNCTION


FUNCTION d2ExxPhi_dx2__1( x , y ) RESULT(d2ExxPhi_dx2)
REAL(KIND_QDF) :: x,y
REAL(KIND_QDF) :: d2ExxPhi_dx2
!!--begin--
d2ExxPhi_dx2 = dExx_dx__1(x,y)*dPhi_dx__1(x,y) + &
                   Exx__1(x,y)*d2Phi_dx2__1(x,y) + &
               dPhi_dx__1(x,y)*dExx_dx__1(x,y) + &
                   Phi__1(x,y)*d2Exx_dx2__1(x,y)
!!--end--
END FUNCTION


FUNCTION d2ExyPhi_dxdy__1( x , y ) RESULT(d2ExyPhi_dxdy)
REAL(KIND_QDF) :: x,y
REAL(KIND_QDF) :: d2ExyPhi_dxdy
!!--begin--
d2ExyPhi_dxdy = dExy_dy__1(x,y)*dPhi_dx__1(x,y) + &
                    Exy__1(x,y)*d2Phi_dxdy__1(x,y) + &
                dPhi_dy__1(x,y)*dExy_dx__1(x,y) + &
                    Phi__1(x,y)*d2Exy_dxdy__1(x,y)
!!--end--
END FUNCTION

FUNCTION d2EyyPhi_dy2__1( x , y ) RESULT(d2EyyPhi_dy2)
REAL(KIND_QDF) :: x,y
REAL(KIND_QDF) :: d2EyyPhi_dy2
!!--begin--
d2EyyPhi_dy2 = dEyy_dy__1(x,y)*dPhi_dy__1(x,y) + &
                   Eyy__1(x,y)*d2Phi_dy2__1(x,y) + &
               dPhi_dy__1(x,y)*dEyy_dy__1(x,y) + &
                   Phi__1(x,y)*d2Eyy_dy2__1(x,y)
!!--end--
END FUNCTION


FUNCTION dEyyPhi_dy__1( x , y ) RESULT(dEyyPhi_dy)
REAL(KIND_QDF) :: x,y
REAL(KIND_QDF) :: dEyyPhi_dy
!!--begin--
dEyyPhi_dy = Eyy__1(x,y)*dPhi_dy__1(x,y) + &
             Phi__1(x,y)*dEyy_dy__1(x,y)
!!--end--
END FUNCTION


FUNCTION Q__1( x , y ) RESULT(Q)
REAL(KIND_QDF) :: x,y
REAL(KIND_QDF) :: sigt,siga
REAL(KIND_QDF) :: Q

!!--begin--

sigt = c_1
siga = c_1_by_2

Q = dJx_dx__1(x,y) + &
    dJy_dy__1(x,y) + &
    siga*Phi__1( x , y )

!!--end--
END FUNCTION

!!************************************************



FUNCTION Phi__4( x , y ) RESULT(Phi)
!IMPLICIT DOUBLE PRECISION (t)
REAL(KIND_QDF) :: Phi
REAL(KIND_QDF) :: x,y
      t2 = (x-1.D0/2.D0)**2
      t4 = (y-1.D0/2.D0)**2
      t7 = tanh(alpha*(t2+t4))
      Phi = 1-t7
END FUNCTION

FUNCTION Jx__4( x , y ) RESULT(Jx)
!IMPLICIT DOUBLE PRECISION (t)
REAL(KIND_QDF) :: Jx
REAL(KIND_QDF) :: x,y
      t3 = x-1.D0/2.D0
      t4 = t3**2
      t5 = y-1.D0/2.D0
      t6 = t5**2
      t9 = tanh(alpha*(t4+t6))
      t10 = 1-t9
      t13 = t9**2
      t14 = 1-t13
      Jx  = -(2.D0/3.D0*x-1.D0/3.D0)*t10+(1+t4+t6)*t14*alpha*(2*x-1)/&
        3+t3*t10-t3*t5*t14*alpha*(2*y-1)

END FUNCTION


FUNCTION dJx_dx__4( x , y ) RESULT(dJx_dx)
!IMPLICIT DOUBLE PRECISION (t)
REAL(KIND_QDF) :: dJx_dx
REAL(KIND_QDF) :: x,y
dJx_dx = 0._KIND_QDF
END FUNCTION


FUNCTION Jy__4( x , y ) RESULT(Jy)
!IMPLICIT DOUBLE PRECISION (t)
REAL(KIND_QDF) :: Jy
REAL(KIND_QDF) :: x,y
      t1 = y-1.D0/2.D0
      t2 = x-1.D0/2.D0
      t3 = t2**2
      t4 = t1**2
      t7 = tanh(alpha*(t3+t4))
      t8 = 1-t7
      t11 = t7**2
      t12 = 1-t11
      Jy  = t1*t8-t2*t1*t12*alpha*(2*x-1)-(2.D0/3.D0*y-1.D0/3.D0)*t8+&
            (1+t3+t4)*t12*alpha*(2*y-1)/3

END FUNCTION


FUNCTION dJy_dy__4( x , y ) RESULT(dJy_dy)
!IMPLICIT DOUBLE PRECISION (t)
REAL(KIND_QDF) :: dJy_dy
REAL(KIND_QDF) :: x,y
dJy_dy = 0._KIND_QDF
END FUNCTION


FUNCTION Q__4( x , y ) RESULT(Q)
!IMPLICIT DOUBLE PRECISION (t)
REAL(KIND_QDF) :: Q
REAL(KIND_QDF) :: x,y
      t1 = x-1.D0/2.D0
      t2 = t1**2
      t3 = y-1.D0/2.D0
      t4 = t3**2
      t7 = tanh(alpha*(t2+t4))
      t11 = t7**2
      t12 = 1-t11
      t15 = 2*x-1
      t16 = alpha*t15
      t19 = 1+t2+t4
      t20 = t19*t7/3
      t21 = alpha**2
      t22 = t21*t12
      t23 = t15**2
      t35 = 2*y-1
      t36 = alpha*t35
      t50 = t35**2
      Q   = 7.D0/6.D0-7.D0/6.D0*t7+2*(2.D0/3.D0*x-1.D0/3.D0)*t12*t16-2*&
            t20*t22*t23+4.D0/3.D0*t19*t12*alpha-2*t1*t12*t16-2*t3*&
            t12*t36+4*t1*t3*t7*t22*t15*t35+2*(2.D0/3.D0*y-1.D0/3.D0)*&
            t12*t36-2*t20*t22*t50
END FUNCTION


FUNCTION Exx__4( x , y ) RESULT(Exx)
!IMPLICIT DOUBLE PRECISION (t)
REAL(KIND_QDF) :: Exx
REAL(KIND_QDF) :: x,y
      t2 = (x-1.D0/2.D0)**2
      t4 = (y-1.D0/2.D0)**2
      Exx = 1.D0/3.D0+t2/3+t4/3
END FUNCTION


FUNCTION Eyy__4( x , y ) RESULT(Eyy)
!IMPLICIT DOUBLE PRECISION (t)
REAL(KIND_QDF) :: Eyy
REAL(KIND_QDF) :: x,y
      t2 = (x-1.D0/2.D0)**2
      t4 = (y-1.D0/2.D0)**2
      Eyy = 1.D0/3.D0+t2/3+t4/3
END FUNCTION


FUNCTION Exy__4( x , y ) RESULT(Exy)
!IMPLICIT DOUBLE PRECISION (t)
REAL(KIND_QDF) :: Exy
REAL(KIND_QDF) :: x,y
      Exy = -(x-1.D0/2.D0)*(y-1.D0/2.D0)
END FUNCTION




FUNCTION Phi__5(x,y) RESULT(Phi)
!IMPLICIT DOUBLE PRECISION (t)
REAL(KIND_QDF),INTENT(IN) :: x,y
REAL(KIND_QDF) :: Phi

!!--begin--

t2 = (x-1.D0/2.D0)**2
t5 = (y-1.D0/2.D0)**2
t8 = tanh(alpha*t2+alpha*t5)
Phi = phi0-C*t8

!WRITE(*,*)'alpha=',alpha
!WRITE(*,*)'phi0=',phi0
!WRITE(*,*)'C=',C

!!--end--
END FUNCTION


FUNCTION Jx__5(x,y)  RESULT(Jx)
!IMPLICIT DOUBLE PRECISION (t)
REAL(KIND_QDF),INTENT(IN) :: x,y
REAL(KIND_QDF) :: Jx
!!--begin--
t2 = x-1.D0/2.D0
t4 = t2**2
t7 = (y-1.D0/2.D0)**2
t10 = tanh(alpha*t4+alpha*t7)
t12 = phi0-C*t10
t19 = t10**2
t20 = 1-t19
t25 = Exy0*t2
Jx  = -1/sigt*(2.D0/3.D0*Exx1*t2*t12-2.D0/3.D0*(Exx0+Exx1*t4+Exx2*t7)*&
      C*t20*alpha*t2-t25*t12+2*t25*t7*C*t20*alpha)
!!--end--
END FUNCTION


FUNCTION Jy__5(x,y) RESULT(Jy)
!IMPLICIT DOUBLE PRECISION (t)
REAL(KIND_QDF),INTENT(IN) :: x,y
REAL(KIND_QDF) :: Jy
!!--begin--
t2 = y-1.D0/2.D0
t5 = (x-1.D0/2.D0)**2
t7 = t2**2
t10 = tanh(alpha*t5+alpha*t7)
t12 = phi0-C*t10
t16 = t10**2
t17 = 1-t16
Jy  = -1/sigt*(-Exy0*t2*t12+2*Exy0*t5*t2*C*t17*alpha+2.D0/3.D0*&
      Eyy2*t2*t12-2.D0/3.D0*(Eyy0+Eyy1*t5+Eyy2*t7)*C*t17*alpha*t2)
!!--end--
END FUNCTION


FUNCTION dJx_dx__5(x,y) RESULT(dJx_dx)
!IMPLICIT DOUBLE PRECISION (t)
REAL(KIND_QDF),INTENT(IN) :: x,y
REAL(KIND_QDF) :: dJx_dx
!!--begin--
t3 = (x-1.D0/2.D0)**2
t6 = (y-1.D0/2.D0)**2
t9 = tanh(alpha*t3+alpha*t6)
t10 = C*t9
t11 = phi0-t10
t14 = Exx1*t3
t15 = t9**2
t16 = 1-t15
t18 = C*t16*alpha
t23 = (Exx0+t14+Exx2*t6)*C/3
t25 = alpha**2
t26 = t16*t25
t34 = Exy0*t3
dJx_dx = -1/sigt*(2.D0/3.D0*Exx1*t11-8.D0/3.D0*t14*t18+8*t23*t9*t26*&
         t3-2*t23*t16*alpha-Exy0*t11+2*t34*t18+2*Exy0*t6*t18-8*t34*t6*&
                 t10*t26)
!!--end--
END FUNCTION


FUNCTION dJy_dy__5(x,y) RESULT(dJy_dy)
!IMPLICIT DOUBLE PRECISION (t)
REAL(KIND_QDF),INTENT(IN) :: x,y
REAL(KIND_QDF) :: dJy_dy
!!--begin--
      t3 = (x-1.D0/2.D0)**2
      t6 = (y-1.D0/2.D0)**2
      t9 = tanh(alpha*t3+alpha*t6)
      t10 = C*t9
      t11 = phi0-t10
      t14 = t9**2
      t15 = 1-t14
      t17 = C*t15*alpha
      t20 = Exy0*t3
      t24 = alpha**2
      t25 = t15*t24
      t31 = Eyy2*t6
      t36 = (Eyy0+Eyy1*t3+t31)*C/3
      dJy_dy = -1/sigt*(-Exy0*t11+2*Exy0*t6*t17+2*t20*t17-8*t20*t6*t10*&
                   t25+2.D0/3.D0*Eyy2*t11-8.D0/3.D0*t31*t17+8*t36*t9*t25*t6-&
                           2*t36*t15*alpha)
!!--end--
END FUNCTION


FUNCTION Exx__5(x,y) RESULT(Exx)
!IMPLICIT DOUBLE PRECISION (t)
REAL(KIND_QDF),INTENT(IN) :: x,y
REAL(KIND_QDF) :: Exx
!!--begin--
t2 = (x-1.D0/2.D0)**2
t5 = (y-1.D0/2.D0)**2
Exx = Exx0/3+Exx1*t2/3+Exx2*t5/3
!!--end--
END FUNCTION


FUNCTION Eyy__5(x,y) RESULT(Eyy)
!IMPLICIT DOUBLE PRECISION (t)
REAL(KIND_QDF),INTENT(IN) :: x,y
REAL(KIND_QDF) :: Eyy
!!--begin--
t2  = (x-1.D0/2.D0)**2
t5  = (y-1.D0/2.D0)**2
Eyy = Eyy0/3+Eyy1*t2/3+Eyy2*t5/3
!!--end--
END FUNCTION


FUNCTION Exy__5(x,y) RESULT(Exy)
!IMPLICIT DOUBLE PRECISION (t)
REAL(KIND_QDF),INTENT(IN) :: x,y
REAL(KIND_QDF) :: Exy
!!--begin--
Exy = -Exy0*(x-1.D0/2.D0)*(y-1.D0/2.D0)
!!--end--
END FUNCTION


FUNCTION Q__5(x,y) RESULT(Q)
!IMPLICIT DOUBLE PRECISION (t)
REAL(KIND_QDF),INTENT(IN) :: x,y
REAL(KIND_QDF) :: Q
!!--begin--
t1 = 1/sigt
t3 = (x-1.D0/2.D0)**2
t6 = (y-1.D0/2.D0)**2
t9 = tanh(alpha*t3+alpha*t6)
t10 = C*t9
t11 = phi0-t10
t14 = Exx1*t3
t15 = t9**2
t16 = 1-t15
t18 = C*t16*alpha
t23 = (Exx0+t14+Exx2*t6)*C/3
t25 = alpha**2
t26 = t16*t25
t30 = t16*alpha
t33 = Exy0*t11
t34 = Exy0*t3
t36 = 2*t34*t18
t39 = 2*Exy0*t6*t18
t43 = 8*t34*t6*t10*t26
t48 = Eyy2*t6
t53 = (Eyy0+Eyy1*t3+t48)*C/3
Q   = -t1*(2.D0/3.D0*Exx1*t11-8.D0/3.D0*t14*t18+&
       8*t23*t9*t26*t3-2*t23*t30-t33+t36+t39-t43)-&
       t1*(-t33+t39+t36-t43+2.D0/3.D0*Eyy2*t11-8.D0/3.D0*&
       t48*t18+8*t53*t9*t26*t6-2*t53*t30)+siga*t11
!!--end--
END FUNCTION



END MODULE

















