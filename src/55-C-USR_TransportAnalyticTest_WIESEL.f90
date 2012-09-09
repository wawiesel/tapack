!!# MODULE <<USR_TransportAnalyticTest_WIESEL>>
MODULE USR_TransportAnalyticTest_WIESEL

!!## PURPOSE
!! Provide the necessary routines to implement an analytic test of Wieselquist,
!! where the exact angular flux is given by
!! $$ \psi_0 = C + A*D*Ey*Ex*(x-x^2)*(y-y^2), $$
!! where A,C are constants and D=(D1+Dx*ox^2+Dy*oy^2),
!! Ey=exp(By*oy*(1-y)) and Ex=exp(Bx*ox*(1-x)).

USE KND_MoCshort          !!((03-A-KND_MoCshort.f90))
USE VAR_DiscreteOrdinates !!((47-B-VAR_DiscreteOrdinates.f90))
USE LIB_genMoments        !!((13-B-LIB_genMoments.f90))
USE USR_fdbk              !!((08-C-USR_fdbk.f90))
USE KND_XSExpansion       !!((02-A-KND_XSExpansion.f90))
USE USR_Mesh              !!((14-B-USR_Mesh.f90))
USE FUN_STR               !!((05-B-FUN_STR.f90))
USE FUN_IsApprox          !!((03-A-FUN_IsApprox.f90))
USE VAR_MoCshort,ONLY: &  !!((47-B-VAR_MoCshort.f90))
  fixedangularflux,functionangularflux,BC,function_
USE PAR_Constants_Rdp     !!((02-A-PAR_Constants_Rdp.f90))
USE USR_FunctionParser    !!((05-B-USR_FunctionParser.f90))

IMPLICIT NONE

PRIVATE

!!## LOCAL VARIABLES
REAL(KIND_MCS) :: ANALYTIC_SIGMAT
REAL(KIND_MCS) :: ANALYTIC_SIGMAS
REAL(KIND_MCS) :: ANALYTIC_CONST
REAL(KIND_MCS) :: ANALYTIC_A=1
REAL(KIND_MCS) :: ANALYTIC_C=0
REAL(KIND_MCS) :: ANALYTIC_D1=1
REAL(KIND_MCS) :: ANALYTIC_DX=1
REAL(KIND_MCS) :: ANALYTIC_DY=1
REAL(KIND_MCS) :: ANALYTIC_BX=1
REAL(KIND_MCS) :: ANALYTIC_BY=1

!!## PUBLIC ACCESS

PUBLIC :: ANALYTIC_A,ANALYTIC_C,&
ANALYTIC_D1,ANALYTIC_DX,ANALYTIC_DY,&
ANALYTIC_BX,ANALYTIC_BY

PUBLIC :: SETUP_AnalyticTest_WIESEL

PUBLIC :: EXACT_Psi_WIESEL
PUBLIC :: EXACT_dPsi_dx_WIESEL
PUBLIC :: EXACT_dPsi_dy_WIESEL

PUBLIC :: EXACT_Qext_WIESEL
PUBLIC :: EXACT_SnQext_WIESEL

PUBLIC :: EXACT_Phi_WIESEL
PUBLIC :: EXACT_SnPhi_WIESEL

PUBLIC :: EXACT_Exx_WIESEL
PUBLIC :: EXACT_Eyy_WIESEL
PUBLIC :: EXACT_Exy_WIESEL
PUBLIC :: EXACT_SnExx_WIESEL
PUBLIC :: EXACT_SnEyy_WIESEL
PUBLIC :: EXACT_SnExy_WIESEL

PUBLIC :: EXACT_Kxx_WIESEL
PUBLIC :: EXACT_Kyy_WIESEL
PUBLIC :: EXACT_Kxy_WIESEL
PUBLIC :: EXACT_SnKxx_WIESEL
PUBLIC :: EXACT_SnKyy_WIESEL
PUBLIC :: EXACT_SnKxy_WIESEL

PUBLIC :: EXACT_Mom0Qext_WIESEL
PUBLIC :: EXACT_SnMom0Qext_WIESEL

PUBLIC :: EXACT_Mom1xQext_WIESEL
PUBLIC :: EXACT_SnMom1xQext_WIESEL

PUBLIC :: EXACT_Mom1yQext_WIESEL
PUBLIC :: EXACT_SnMom1yQext_WIESEL

PUBLIC :: EXACT_Jx_WIESEL
PUBLIC :: EXACT_SnJx_WIESEL

PUBLIC :: EXACT_Jy_WIESEL
PUBLIC :: EXACT_SnJy_WIESEL

PUBLIC :: EXACT_TEqn
PUBLIC :: EXACT_SnTEqn

!!## CONTAINED PROCEDURES
CONTAINS


SUBROUTINE SETUP_AnalyticTest_WIESEL(Caller,&
  TRANSPORT_ANALYTIC_SIGMAT,&
  TRANSPORT_ANALYTIC_SIGMAS,&
  TRANSPORT_ANALYTIC_CONST,&
  MacS,MacF,MacNu,MacT,l_,CoeffScalarFluxM,&
  Mesh,&
  fdbk)
CHARACTER(*),INTENT(IN) :: Caller
REAL(KIND_MCS),INTENT(INOUT) :: TRANSPORT_ANALYTIC_SIGMAT
REAL(KIND_MCS),INTENT(INOUT) :: TRANSPORT_ANALYTIC_SIGMAS
REAL(KIND_MCS),INTENT(INOUT) :: TRANSPORT_ANALYTIC_CONST
REAL(KIND_Mac),INTENT(INOUT) :: MacS(:,:),MacF(:,:),MacNu(:,:),MacT(:,:)
INTEGER,INTENT(IN) :: l_(:)
REAL(KIND_Mac),INTENT(INOUT) :: CoeffScalarFluxM(:,:)
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk

INTEGER :: Ng,Nl,g,l,n,m,jd
INTEGER :: ix,iy
CHARACTER(100) :: error1
REAL(KIND_MCS) :: thisr(2,4),x,y,check(2)
LOGICAL,PARAMETER :: TEST_TEQn=.FALSE.
CHARACTER(1000) :: function_str

!!--begin--

ANALYTIC_SIGMAT = TRANSPORT_ANALYTIC_SIGMAT
ANALYTIC_SIGMAS = TRANSPORT_ANALYTIC_SIGMAS

Ng = SIZE(MacT,1)
Nl = SIZE(MacT,2)

!Prepare the analytic test
 CALL UpdateAndDump(fdbk_comment,fdbk,s=Caller//" TransportAnalyticTest: &
  &Setting up the test variables for Wieselquist's analytic test of the high-order problem")
 !setup materials
 IF( Nl>1 )THEN
  CALL UpdateAndDump(fdbk_error,fdbk,s=Caller//" TransportAnalyticTest: &
   &Only one material can be used with Wieselquist's analytic test")
 END IF
 IF( Ng>1 )THEN
  CALL UpdateAndDump(fdbk_error,fdbk,s=Caller//" TransportAnalyticTest: &
   &Only one group can be used with Wieselquist's analytic test")
 END IF

 CALL UpdateAndDump(fdbk_warning,fdbk,s=Caller//" TransportAnalyticTest: &
   &Replacing the current material properties with the analytic ones")

 DO g=1,Ng
  MacS (g,1) = ANALYTIC_SIGMAS
  MacT (g,1) = ANALYTIC_SIGMAT
  MacF (g,1) = 0._KIND_MCS
  MacNu(g,1) = 0._KIND_MCS
 END DO
 CoeffScalarFluxM = (MacS + MacNu*MacF)/c_4_times_PI
 IF( SIZE(Mesh%Domain%Verts,2)/=4 )THEN
  CALL UpdateAndDump(fdbk_error,fdbk,s=Caller//" TransportAnalyticTest: &
    &The domain must have 4 corners.")
 END IF
 IF( Mesh%NDim/=2 )THEN
  CALL UpdateAndDump(fdbk_error,fdbk,s=Caller//" TransportAnalyticTest: &
    &The analytic test must have 2 dimensions.")
 END IF
 thisr(:,1)=(/0.d0,0.d0/)
 thisr(:,2)=(/1.d0,0.d0/)
 thisr(:,3)=(/1.d0,1.d0/)
 thisr(:,4)=(/0.d0,1.d0/)
 DO n=1,4
  IF( .NOT.IsApprox(Mesh%Domain%Verts(:,n),thisr(:,n)) )THEN
   CALL UpdateAndDump(fdbk_error,fdbk,s=Caller//" TransportAnalyticTest: &
     &The domain is not the right size---it must be a square with x,y \in [0,1].")
  END IF
 END DO

 IF( TEST_TEQn )THEN
    DO m=1,SIZE(Ordinates,2)
    check=0.d0
    DO ix=0,100
    DO iy=0,100
        x=ix/100.d0
        y=iy/100.d0
        check(1)=MAX(EXACT_TEqn(x,y,Ordinates(:,m)),check(1))
        check(2)=MAX(EXACT_SnTEqn(x,y,Ordinates(:,m)),check(2))
    END DO
    END DO

    CALL UpdateAndDump(fdbk_comment,fdbk,s=Caller//" TransportAnalyticTest: Transport Equation&
        & for [m="//TRIM(STR(m))//"] maximum abs. error using manufactured source [abserr="//&
        TRIM(STR(check(1),"(Es9.2)"))//"]")
    CALL UpdateAndDump(fdbk_comment,fdbk,s=Caller//" TransportAnalyticTest: Transport Equation&
        & for [m="//TRIM(STR(m))//"] maximum abs. error using manufactured source with Sn&
        & integration via current quadrature set [abserr="//&
        TRIM(STR(check(2),"(Es9.2)"))//"]")
    END DO
 END IF

! !for fine grid output
! OPEN(11,file='snphi_fine.dat',status='replace')
! OPEN(12,file='snkxx_fine.dat',status='replace')
! OPEN(13,file='snexx_fine.dat',status='replace')
! DO ix=0,200
!  DO iy=0,200
!   x=ix/200.d0
!   y=iy/200.d0
!   WRITE(11,"(1x,1Es14.5)",ADVANCE='no')EXACT_SnPhi_WIESEL(x,y)
!   WRITE(12,"(1x,1Es14.5)",ADVANCE='no')EXACT_SnKxx_WIESEL(x,y)
!   WRITE(13,"(1x,1Es14.5)",ADVANCE='no')EXACT_SnExx_WIESEL(x,y)
!  END DO
!  WRITE(11,"(a)",ADVANCE='yes')''
!  WRITE(12,"(a)",ADVANCE='yes')''
!  WRITE(13,"(a)",ADVANCE='yes')''
! END DO
! CLOSE(11)
! CLOSE(12)
! CLOSE(13)
! !            if( i==1 )then
!                 !for fine grid output
!                 OPEN(11,file='snexx1_fine.dat',status='replace')
!                 DO ix=0,20
!                     DO iy=0,20
!                         x=ix/200.d0
!                         y=iy/200.d0
!                         WRITE(11,"(1x,1Es14.5)",ADVANCE='no')EXACT_SnExx_WIESEL(x,y)
!                     END DO
!                     WRITE(11,"(a)",ADVANCE='yes')''
!                 END DO
!                 CLOSE(11)
! !            end if

 CALL UpdateAndDump(fdbk_comment,fdbk,s=Caller//" TransportAnalyticTest: &
     &Changing boundary conditions...")

 DO jd=1,4
  !WRITE(function_str,'(1+1*(x**2-x)*(y**2-y)*exp(1*ox*(1-x))*exp(1*oy*(1-y))*(1+1*ox**2+1*oy**2))')&
  !  ANALYTIC_C,ANALYTIC_A,ANALYTIC_BX,ANALYTIC_BY,ANALYTIC_D1,ANALYTIC_DX,ANALYTIC_DY
  !always 0 on boundary
  !"(1+1*(x**2-x)*(y**2-y)*exp(1*ox*(1-x))*exp(1*oy*(1-y))*(1+1*ox**2+1*oy**2))
  call s_createfn("("//TRIM(STR(ANALYTIC_C))//")", 'x y z ox oy oz m', &
   FunctionAngularFlux(jd), error1)
   BC(jd)=function_
   IF( error1/="OK" )THEN
    CALL UpdateAndDump(fdbk_error,fdbk,s=Caller//" TransportAnalyticTest: &
     &error reported in s_createfn [errmsg="//TRIM(error1)//"]")
   END IF
 END DO

!!--end--
END SUBROUTINE

FUNCTION EXACT_Psi_WIESEL(x,y,Omega) RESULT(Psi)
REAL(KIND_MCS) :: Psi
REAL(KIND_MCS),INTENT(IN) :: x,y,Omega(1:2)
REAL(KIND_MCS) :: EX,EY,D,ox,oy
!!--begin--
ox=Omega(1)
oy=Omega(2)
EX=exp(ANALYTIC_BX*ox*(1.d0-x))
EY=exp(ANALYTIC_BY*oy*(1.d0-y))
D=(ANALYTIC_D1+ANALYTIC_DX*ox**2+ANALYTIC_DY*oy**2)
Psi = ANALYTIC_C + &
ANALYTIC_A*( x-x**2 )*( y-y**2 )* &
EX*EY*D
!!--end--
END FUNCTION

FUNCTION EXACT_dPsi_dx_WIESEL(x,y,Omega) RESULT(dPsi_dx)
REAL(KIND_MCS) :: dPsi_dx
REAL(KIND_MCS),INTENT(IN) :: x,y,Omega(1:2)
REAL(KIND_MCS) :: EX,EY,D,ox,oy
!!--begin--
ox=Omega(1)
oy=Omega(2)
EX=exp(ANALYTIC_BX*ox*(1.d0-x))
EY=exp(ANALYTIC_BY*oy*(1.d0-y))
D=(ANALYTIC_D1+ANALYTIC_DX*ox**2+ANALYTIC_DY*oy**2)
dPsi_dx = ANALYTIC_A*D*EY*(y-y**2)* &
( (1-2*x)*EX + (x-x**2)*(-ANALYTIC_BX*ox*EX) )
!!--end--
END FUNCTION

FUNCTION EXACT_dPsi_dy_WIESEL(x,y,Omega) RESULT(dPsi_dy)
REAL(KIND_MCS) :: dPsi_dy
REAL(KIND_MCS),INTENT(IN) :: x,y,Omega(1:2)
REAL(KIND_MCS) :: EX,EY,D,ox,oy
!!--begin--
ox=Omega(1)
oy=Omega(2)
EX=exp(ANALYTIC_BX*ox*(1.d0-x))
EY=exp(ANALYTIC_BY*oy*(1.d0-y))
D=(ANALYTIC_D1+ANALYTIC_DX*ox**2+ANALYTIC_DY*oy**2)
dPsi_dy = ANALYTIC_A*D*EX*(x-x**2)* &
( (1-2*y)*EY + (y-y**2)*(-ANALYTIC_BY*oy*EY) )
!!--end--
END FUNCTION

FUNCTION EXACT_Phi_WIESEL(x,y) RESULT(Phi)
REAL(KIND_MCS) :: Phi
REAL(KIND_MCS),INTENT(IN) :: x,y
REAL(KIND_MCS) :: gamma,coeff
!!--begin--
Phi=1
!!--end--
END FUNCTION


FUNCTION EXACT_SnPhi_WIESEL(x,y) RESULT(SnPhi)
REAL(KIND_MCS) :: SnPhi
REAL(KIND_MCS),INTENT(IN) :: x,y
!!#### LOCAL VARIABLES
REAL(KIND_MCS) :: Omega(2)
REAL(KIND_MCS),ALLOCATABLE,SAVE :: Psi(:)
INTEGER :: m

!!--begin--
IF( .NOT.ALLOCATED(Psi) )THEN
 ALLOCATE( Psi(SIZE(Ordinates,2)) )
END IF

DO m=1,SIZE(Ordinates,2)
 Omega = Ordinates(1:2,m)
 Psi(m) = EXACT_Psi_WIESEL(x,y,Omega)
END DO

SnPhi = Moment0(Psi,Ordinates,Weights)

!!--end--
END FUNCTION


FUNCTION EXACT_Jx_WIESEL(x,y) RESULT(Jx)
REAL(KIND_MCS) :: Jx
REAL(KIND_MCS),INTENT(IN) :: x,y
!!#### LOCAL VARIABLES
REAL(KIND_MCS) :: gamma,coeff

!!--begin--

Jx = 0

!!--end--
END FUNCTION

FUNCTION EXACT_Jy_WIESEL(x,y) RESULT(Jy)
REAL(KIND_MCS) :: Jy
REAL(KIND_MCS),INTENT(IN) :: x,y
!!#### LOCAL VARIABLES
REAL(KIND_MCS) :: gamma,coeff

!!--begin--

Jy = 0

!!--end--
END FUNCTION

FUNCTION EXACT_SnJx_WIESEL(x,y) RESULT(SnJx)
REAL(KIND_MCS) :: SnJx
REAL(KIND_MCS),INTENT(IN) :: x,y
!!#### LOCAL VARIABLES
REAL(KIND_MCS) :: Omega(2)
REAL(KIND_MCS),ALLOCATABLE,SAVE :: Psi(:)
INTEGER :: m
!!--begin--
IF( .NOT.ALLOCATED(Psi) )THEN
 ALLOCATE( Psi(SIZE(Ordinates,2)) )
END IF

DO m=1,SIZE(Ordinates,2)
 Omega = Ordinates(1:2,m)
 Psi(m) = EXACT_Psi_WIESEL(x,y,Omega)
END DO

SnJx = Moment1_Dim(Psi,Ordinates,Weights,1)

!!--end--
END FUNCTION



FUNCTION EXACT_SnJy_WIESEL(x,y) RESULT(SnJy)
REAL(KIND_MCS) :: SnJy
REAL(KIND_MCS),INTENT(IN) :: x,y
!!#### LOCAL VARIABLES
REAL(KIND_MCS) :: Omega(2)
REAL(KIND_MCS),ALLOCATABLE,SAVE :: Psi(:)
INTEGER :: m
!!--begin--
IF( .NOT.ALLOCATED(Psi) )THEN
 ALLOCATE( Psi(SIZE(Ordinates,2)) )
END IF

DO m=1,SIZE(Ordinates,2)
 Omega = Ordinates(1:2,m)
 Psi(m) = EXACT_Psi_WIESEL(x,y,Omega)
END DO

SnJy = Moment1_Dim(Psi,Ordinates,Weights,2)

!!--end--
END FUNCTION


FUNCTION EXACT_Qext_WIESEL(x,y,Omega) RESULT(Qext)
REAL(KIND_MCS) :: Qext
REAL(KIND_MCS),INTENT(IN) :: x,y,Omega(1:2)
REAL(KIND_MCS) :: dPsi_dx,dPsi_dy,Psi,Phi
!!--begin--
dPsi_dx = EXACT_dPsi_dx_WIESEL(x,y,Omega)
dPsi_dy = EXACT_dPsi_dy_WIESEL(x,y,Omega)
Psi = EXACT_Psi_WIESEL(x,y,Omega)
Phi = EXACT_Phi_WIESEL(x,y)
Qext = Omega(1)*dPsi_dx + Omega(2)*dPsi_dy + &
       ANALYTIC_SIGMAT*Psi - &
       ANALYTIC_SIGMAS*Phi/c_4_times_PI
!!--end--
END FUNCTION


FUNCTION EXACT_SnQext_WIESEL(x,y,Omega) RESULT(SnQext)
REAL(KIND_MCS) :: SnQext
REAL(KIND_MCS),INTENT(IN) :: x,y,Omega(1:2)
REAL(KIND_MCS) :: dPsi_dx,dPsi_dy,Psi,SnPhi
!!--begin--
dPsi_dx = EXACT_dPsi_dx_WIESEL(x,y,Omega)
dPsi_dy = EXACT_dPsi_dy_WIESEL(x,y,Omega)
Psi = EXACT_Psi_WIESEL(x,y,Omega)
SnPhi = EXACT_SnPhi_WIESEL(x,y)
SnQext = Omega(1)*dPsi_dx + Omega(2)*dPsi_dy + &
       ANALYTIC_SIGMAT*Psi - &
       ANALYTIC_SIGMAS*SnPhi/c_4_times_PI
!!--end--
END FUNCTION


FUNCTION EXACT_Mom0Qext_WIESEL(x,y) RESULT(Mom0Qext)
REAL(KIND_MCS) :: Mom0Qext
REAL(KIND_MCS),INTENT(IN) :: x,y
REAL(KIND_MCS) :: sigs,sigt
!!--begin--
sigs = ANALYTIC_SIGMAS
sigt = ANALYTIC_SIGMAT
Mom0Qext = 0
!!--end--
END FUNCTION

FUNCTION EXACT_SnMom0Qext_WIESEL(x,y) RESULT(SnMom0Qext)
REAL(KIND_MCS) :: SnMom0Qext
REAL(KIND_MCS),INTENT(IN) :: x,y
!!#### LOCAL VARIABLES
REAL(KIND_MCS) :: Omega(2)
REAL(KIND_MCS),ALLOCATABLE,SAVE :: qext(:)
INTEGER :: m
REAL(KIND_MCS) :: dPsi_dx,dPsi_dy,Psi,SnPhi

!!--begin--

IF( .NOT.ALLOCATED(qext) )THEN
 ALLOCATE( qext(SIZE(Ordinates,2)) )
END IF

DO m=1,SIZE(Ordinates,2)
 Omega = Ordinates(1:2,m)
 dPsi_dx = EXACT_dPsi_dx_WIESEL(x,y,Omega)
 dPsi_dy = EXACT_dPsi_dy_WIESEL(x,y,Omega)
 Psi     = EXACT_Psi_WIESEL(x,y,Omega)
 SnPhi   = EXACT_SnPhi_WIESEL(x,y)
 qext(m) = Omega(1)*dPsi_dx + Omega(2)*dPsi_dy + &
           ANALYTIC_SIGMAT*Psi - &
           ANALYTIC_SIGMAS*SnPhi/c_4_times_PI
END DO

SnMom0Qext = Moment0(qext,Ordinates,Weights)
!!--end--
END FUNCTION


FUNCTION EXACT_Mom1xQext_WIESEL(x,y) RESULT(Mom1xQext)
REAL(KIND_MCS) :: Mom1xQext
REAL(KIND_MCS),INTENT(IN) :: x,y
REAL(KIND_MCS) :: sigs,sigt
!!--begin--
sigs = ANALYTIC_SIGMAS
sigt = ANALYTIC_SIGMAT
Mom1xQext = 0
!!--end--
END FUNCTION

FUNCTION EXACT_Mom1yQext_WIESEL(x,y) RESULT(Mom1yQext)
REAL(KIND_MCS) :: Mom1yQext
REAL(KIND_MCS),INTENT(IN) :: x,y
REAL(KIND_MCS) :: sigs,sigt
!!--begin--
sigs = ANALYTIC_SIGMAS
sigt = ANALYTIC_SIGMAT
Mom1yQext = 0
!!--end--
END FUNCTION


FUNCTION EXACT_SnMom1xQext_WIESEL(x,y) RESULT(SnMom1xQext)
REAL(KIND_MCS) :: SnMom1xQext
REAL(KIND_MCS),INTENT(IN) :: x,y
!!#### LOCAL VARIABLES
REAL(KIND_MCS) :: Omega(2)
REAL(KIND_MCS),ALLOCATABLE,SAVE :: qext(:)
INTEGER :: m
REAL(KIND_MCS) :: dPsi_dx,dPsi_dy,Psi,SnPhi

!!--begin--

IF( .NOT.ALLOCATED(qext) )THEN
 ALLOCATE( qext(SIZE(Ordinates,2)) )
END IF

DO m=1,SIZE(Ordinates,2)
 Omega = Ordinates(1:2,m)
 dPsi_dx = EXACT_dPsi_dx_WIESEL(x,y,Omega)
 dPsi_dy = EXACT_dPsi_dy_WIESEL(x,y,Omega)
 Psi     = EXACT_Psi_WIESEL(x,y,Omega)
 SnPhi   = EXACT_SnPhi_WIESEL(x,y)
 qext(m) = Omega(1)*dPsi_dx + Omega(2)*dPsi_dy + &
           ANALYTIC_SIGMAT*Psi - &
           ANALYTIC_SIGMAS*SnPhi/c_4_times_PI
END DO

SnMom1xQext = Moment1_Dim(qext,Ordinates,Weights,1)
!!--end--
END FUNCTION

FUNCTION EXACT_SnMom1yQext_WIESEL(x,y) RESULT(SnMom1yQext)
REAL(KIND_MCS) :: SnMom1yQext
REAL(KIND_MCS),INTENT(IN) :: x,y
!!#### LOCAL VARIABLES
REAL(KIND_MCS) :: Omega(2)
REAL(KIND_MCS),ALLOCATABLE,SAVE :: qext(:)
INTEGER :: m
REAL(KIND_MCS) :: dPsi_dx,dPsi_dy,Psi,SnPhi

!!--begin--

IF( .NOT.ALLOCATED(qext) )THEN
 ALLOCATE( qext(SIZE(Ordinates,2)) )
END IF

DO m=1,SIZE(Ordinates,2)
 Omega = Ordinates(1:2,m)
 dPsi_dx = EXACT_dPsi_dx_WIESEL(x,y,Omega)
 dPsi_dy = EXACT_dPsi_dy_WIESEL(x,y,Omega)
 Psi     = EXACT_Psi_WIESEL(x,y,Omega)
 SnPhi   = EXACT_SnPhi_WIESEL(x,y)
 qext(m) = Omega(1)*dPsi_dx + Omega(2)*dPsi_dy + &
           ANALYTIC_SIGMAT*Psi - &
           ANALYTIC_SIGMAS*SnPhi/c_4_times_PI
END DO

SnMom1yQext = Moment1_Dim(qext,Ordinates,Weights,2)
!!--end--
END FUNCTION





FUNCTION EXACT_SnExx_WIESEL(x,y) RESULT(SnExx)
REAL(KIND_MCS) :: SnExx
REAL(KIND_MCS),INTENT(IN) :: x,y
!!--begin--
SnExx = EXACT_SnKxx_WIESEL(x,y)/EXACT_SnPhi_WIESEL(x,y)
!!--end--
END FUNCTION


FUNCTION EXACT_SnEyy_WIESEL(x,y) RESULT(SnEyy)
REAL(KIND_MCS) :: SnEyy
REAL(KIND_MCS),INTENT(IN) :: x,y
!!--begin--
SnEyy = EXACT_SnKyy_WIESEL(x,y)/EXACT_SnPhi_WIESEL(x,y)
!!--end--
END FUNCTION



FUNCTION EXACT_SnExy_WIESEL(x,y) RESULT(SnExy)
REAL(KIND_MCS) :: SnExy
REAL(KIND_MCS),INTENT(IN) :: x,y
!!--begin--
SnExy = EXACT_SnKxy_WIESEL(x,y)/EXACT_SnPhi_WIESEL(x,y)
!!--end--
END FUNCTION



FUNCTION EXACT_Exx_WIESEL(x,y) RESULT(Exx)
REAL(KIND_MCS) :: Exx
REAL(KIND_MCS),INTENT(IN) :: x,y
!!--begin--
Exx = EXACT_Kxx_WIESEL(x,y)/EXACT_Phi_WIESEL(x,y)
!!--end--
END FUNCTION


FUNCTION EXACT_Eyy_WIESEL(x,y) RESULT(Eyy)
REAL(KIND_MCS) :: Eyy
REAL(KIND_MCS),INTENT(IN) :: x,y
!!--begin--
Eyy = EXACT_Kyy_WIESEL(x,y)/EXACT_Phi_WIESEL(x,y)
!!--end--
END FUNCTION



FUNCTION EXACT_Exy_WIESEL(x,y) RESULT(Exy)
REAL(KIND_MCS) :: Exy
REAL(KIND_MCS),INTENT(IN) :: x,y
!!--begin--
Exy = EXACT_Kxy_WIESEL(x,y)/EXACT_Phi_WIESEL(x,y)
!!--end--
END FUNCTION



FUNCTION EXACT_Kxx_WIESEL(x,y) RESULT(Kxx)
REAL(KIND_MCS) :: Kxx
REAL(KIND_MCS),INTENT(IN) :: x,y
!!--begin--
Kxx = 0
!!--end--
END FUNCTION


FUNCTION EXACT_Kyy_WIESEL(x,y) RESULT(Kyy)
REAL(KIND_MCS) :: Kyy
REAL(KIND_MCS),INTENT(IN) :: x,y
!!--begin--
Kyy = 0
!!--end--
END FUNCTION



FUNCTION EXACT_Kxy_WIESEL(x,y) RESULT(Kxy)
REAL(KIND_MCS) :: Kxy
REAL(KIND_MCS),INTENT(IN) :: x,y
!!--begin--
Kxy = 0
!!--end--
END FUNCTION



FUNCTION EXACT_SnKxx_WIESEL(x,y) RESULT(SnKxx)
REAL(KIND_MCS) :: SnKxx
REAL(KIND_MCS),INTENT(IN) :: x,y
!!#### LOCAL VARIABLES
REAL(KIND_MCS) :: Omega(2)
REAL(KIND_MCS),ALLOCATABLE,SAVE :: Psi(:)
INTEGER :: m
!!--begin--
IF( .NOT.ALLOCATED(Psi) )THEN
 ALLOCATE( Psi(SIZE(Ordinates,2)) )
END IF

DO m=1,SIZE(Ordinates,2)
 Omega = Ordinates(1:2,m)
 Psi(m) = EXACT_Psi_WIESEL(x,y,Omega)
END DO
SnKxx = Moment2_Dim(Psi,Ordinates,Weights,1,1)
!!--end--
END FUNCTION


FUNCTION EXACT_SnKyy_WIESEL(x,y) RESULT(SnKyy)
REAL(KIND_MCS) :: SnKyy
REAL(KIND_MCS),INTENT(IN) :: x,y
!!#### LOCAL VARIABLES
REAL(KIND_MCS) :: Omega(2)
REAL(KIND_MCS),ALLOCATABLE,SAVE :: Psi(:)
INTEGER :: m

!!--begin--
IF( .NOT.ALLOCATED(Psi) )THEN
 ALLOCATE( Psi(SIZE(Ordinates,2)) )
END IF

DO m=1,SIZE(Ordinates,2)
 Omega = Ordinates(1:2,m)
 Psi(m) = EXACT_Psi_WIESEL(x,y,Omega)
END DO
SnKyy = Moment2_Dim(Psi,Ordinates,Weights,2,2)
!!--end--
END FUNCTION



FUNCTION EXACT_SnKxy_WIESEL(x,y) RESULT(SnKxy)
REAL(KIND_MCS) :: SnKxy
REAL(KIND_MCS),INTENT(IN) :: x,y
!!#### LOCAL VARIABLES
REAL(KIND_MCS) :: Omega(2)
REAL(KIND_MCS),ALLOCATABLE,SAVE :: Psi(:)
INTEGER :: m

!!--begin--
IF( .NOT.ALLOCATED(Psi) )THEN
 ALLOCATE( Psi(SIZE(Ordinates,2)) )
END IF

DO m=1,SIZE(Ordinates,2)
 Omega = Ordinates(1:2,m)
 Psi(m) = EXACT_Psi_WIESEL(x,y,Omega)
END DO
SnKxy = Moment2_Dim(Psi,Ordinates,Weights,1,2)
!!--end--
END FUNCTION


FUNCTION EXACT_TEqn(x,y,Omega) RESULT(zero)
!!#### PURPOSE
!! Get the residual of the transport equation (should be zero for this manufactured
!! test.

REAL(KIND_MCS) :: zero
REAL(KIND_MCS),INTENT(IN) :: x,y,Omega(1:2)
!!--begin--
zero = Omega(1)*EXACT_dPsi_dx_WIESEL(x,y,Omega) + &
       Omega(2)*EXACT_dPsi_dy_WIESEL(x,y,Omega) + &
       ANALYTIC_SIGMAT*EXACT_Psi_WIESEL(x,y,Omega) - &
       ANALYTIC_SIGMAS*EXACT_Phi_WIESEL(x,y)/c_4_times_PI - &
       EXACT_Qext_WIESEL(x,y,Omega)
!!--end--
END FUNCTION

FUNCTION EXACT_SnTEqn(x,y,Omega) RESULT(zero)
!!#### PURPOSE
!! Get the residual of the Sn transport equation (should be zero for this manufactured
!! test.

REAL(KIND_MCS) :: zero
REAL(KIND_MCS),INTENT(IN) :: x,y,Omega(1:2)
!!--begin--
zero = Omega(1)*EXACT_dPsi_dx_WIESEL(x,y,Omega) + &
       Omega(2)*EXACT_dPsi_dy_WIESEL(x,y,Omega) + &
       ANALYTIC_SIGMAT*EXACT_Psi_WIESEL(x,y,Omega) - &
       ANALYTIC_SIGMAS*EXACT_SnPhi_WIESEL(x,y)/c_4_times_PI - &
       EXACT_SnQext_WIESEL(x,y,Omega)
!!--end--
END FUNCTION


END MODULE
