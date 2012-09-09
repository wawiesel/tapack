!!# MODULE <<USR_TransportAnalyticTest_WARSA>>
MODULE USR_TransportAnalyticTest_WARSA

!!## PURPOSE
!! Provide the necessary routines to implement an analytic test of Warsa,
!! where the exact angular flux is given by
!! $$ \psi_0 = (x^2-x^4)(y^2-y^4)(1+Omega_x^2+Omega_y^2). $$

USE KND_MoCshort                        !!((03-A-KND_MoCshort.f90))
USE VAR_DiscreteOrdinates               !!((47-B-VAR_DiscreteOrdinates.f90))
USE LIB_genMoments                      !!((13-B-LIB_genMoments.f90))
USE USR_fdbk                            !!((08-C-USR_fdbk.f90))
USE KND_XSExpansion                     !!((02-A-KND_XSExpansion.f90))
USE USR_Mesh                            !!((14-B-USR_Mesh.f90))
USE FUN_STR                             !!((05-B-FUN_STR.f90))
USE FUN_IsApprox                        !!((03-A-FUN_IsApprox.f90))
USE VAR_MoCshort,ONLY: fixedangularflux !!((47-B-VAR_MoCshort.f90))
USE PAR_Constants_Rdp                   !!((02-A-PAR_Constants_Rdp.f90))

IMPLICIT NONE

PRIVATE

!!## LOCAL VARIABLES
REAL(KIND_MCS) :: ANALYTIC_SIGMAT
REAL(KIND_MCS) :: ANALYTIC_SIGMAS
REAL(KIND_MCS) :: ANALYTIC_CONST

!!## PUBLIC ACCESS
PUBLIC :: SETUP_AnalyticTest_WARSA

PUBLIC :: EXACT_Psi_WARSA

PUBLIC :: EXACT_dPsi_dx_WARSA
PUBLIC :: EXACT_dPsi_dy_WARSA

PUBLIC :: EXACT_Qext_WARSA
PUBLIC :: EXACT_SnQext_WARSA

PUBLIC :: EXACT_Phi_WARSA
PUBLIC :: EXACT_SnPhi_WARSA

PUBLIC :: EXACT_Exx_WARSA
PUBLIC :: EXACT_Eyy_WARSA
PUBLIC :: EXACT_Exy_WARSA
PUBLIC :: EXACT_SnExx_WARSA
PUBLIC :: EXACT_SnEyy_WARSA
PUBLIC :: EXACT_SnExy_WARSA

PUBLIC :: EXACT_Kxx_WARSA
PUBLIC :: EXACT_Kyy_WARSA
PUBLIC :: EXACT_Kxy_WARSA
PUBLIC :: EXACT_SnKxx_WARSA
PUBLIC :: EXACT_SnKyy_WARSA
PUBLIC :: EXACT_SnKxy_WARSA

PUBLIC :: EXACT_Mom0Qext_WARSA
PUBLIC :: EXACT_SnMom0Qext_WARSA

PUBLIC :: EXACT_Mom1xQext_WARSA
PUBLIC :: EXACT_SnMom1xQext_WARSA

PUBLIC :: EXACT_Mom1yQext_WARSA
PUBLIC :: EXACT_SnMom1yQext_WARSA

PUBLIC :: EXACT_Jx_WARSA
PUBLIC :: EXACT_SnJx_WARSA

PUBLIC :: EXACT_Jy_WARSA
PUBLIC :: EXACT_SnJy_WARSA

PUBLIC :: EXACT_TEqn_WARSA
PUBLIC :: EXACT_SnTEqn_WARSA

!!## PARAMETERS
REAL(KIND_MCS) :: COEFF_1=20._KIND_MCS*c_PI/3._KIND_MCS
REAL(KIND_MCS) :: COEFF_Omegax=0._KIND_MCS
REAL(KIND_MCS) :: COEFF_Omegay=0._KIND_MCS
REAL(KIND_MCS) :: COEFF_OmegaxOmegay=0._KIND_MCS
REAL(KIND_MCS) :: COEFF_Omegax2=12._KIND_MCS*c_PI/5._KIND_MCS
REAL(KIND_MCS) :: COEFF_Omegay2=12._KIND_MCS*c_PI/5._KIND_MCS
REAL(KIND_MCS) :: INTEGRAL_1=4._KIND_MCS*c_PI
REAL(KIND_MCS) :: INTEGRAL_Omegax=0._KIND_MCS
REAL(KIND_MCS) :: INTEGRAL_Omegay=0._KIND_MCS
REAL(KIND_MCS) :: INTEGRAL_OmegaxOmegay=0._KIND_MCS
REAL(KIND_MCS) :: INTEGRAL_Omegax2=4._KIND_MCS*c_PI/3._KIND_MCS
REAL(KIND_MCS) :: INTEGRAL_Omegay2=4._KIND_MCS*c_PI/3._KIND_MCS

!!## CONTAINED PROCEDURES
CONTAINS


SUBROUTINE SETUP_AnalyticTest_WARSA(Caller,&
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

INTEGER :: Ng,Nl,g,l,n,m
INTEGER :: ix,iy
REAL(KIND_MCS) :: thisr(2,4),x,y,maxval
!!--begin--

ANALYTIC_SIGMAT = TRANSPORT_ANALYTIC_SIGMAT
ANALYTIC_SIGMAS = TRANSPORT_ANALYTIC_SIGMAS
ANALYTIC_CONST  = TRANSPORT_ANALYTIC_CONST

Ng = SIZE(MacT,1)
Nl = SIZE(MacT,2)

!Prepare the analytic test
 CALL UpdateAndDump(fdbk_comment,fdbk,s=Caller//" TransportAnalyticTest: &
  &Setting up the test variables for Warsa's analytic test of the high-order problem")
 !setup materials
 IF( Nl>1 )THEN
  CALL UpdateAndDump(fdbk_error,fdbk,s=Caller//" TransportAnalyticTest: &
   &Only one material can be used with Warsa's analytic test")
 END IF
 IF( Ng>1 )THEN
  CALL UpdateAndDump(fdbk_error,fdbk,s=Caller//" TransportAnalyticTest: &
   &Only one group can be used with Warsa's analytic test")
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

 DO m=1,SIZE(Ordinates,2)
  maxval=0.d0
  DO ix=0,100
   DO iy=0,100
    x=ix/100.d0
    y=iy/100.d0
    maxval=MAX(EXACT_TEqn_WARSA(x,y,Ordinates(:,m)),maxval)
   END DO
  END DO
  CALL UpdateAndDump(fdbk_comment,fdbk,s=Caller//" TransportAnalyticTest: Transport Equation&
       & for [m="//TRIM(STR(m))//"] maximum abs. error using manufactured source [abserr="//&
       TRIM(STR(maxval,"(Es9.2)"))//"]")
 END DO
 CALL UpdateAndDump(fdbk_comment,fdbk,s=Caller//" TransportAnalyticTest: &
     &The difference between the analytic coefficient and Sn coefficient is [diff="//&
      TRIM(STR(COEFF_Sn1()-COEFF_1))//"]")

 ANALYTIC_CONST = CEILING( (2.d0/(ANALYTIC_SIGMAT-ANALYTIC_SIGMAS))*&
   (1.d0+ANALYTIC_SIGMAS/24.d0) )

 TRANSPORT_ANALYTIC_CONST = ANALYTIC_CONST

 CALL UpdateAndDump(fdbk_comment,fdbk,s=Caller//" TransportAnalyticTest: &
     &The analytic constant is [const="//&
      TRIM(STR(ANALYTIC_CONST))//"]")

 CALL UpdateAndDump(fdbk_comment,fdbk,s=Caller//" TransportAnalyticTest: &
     &Changing boundary conditions...")
 fixedangularflux = ANALYTIC_CONST

!!--end--
END SUBROUTINE

FUNCTION EXACT_Psi_WARSA(x,y,Omega) RESULT(Psi)
REAL(KIND_MCS) :: Psi
REAL(KIND_MCS),INTENT(IN) :: x,y,Omega(1:2)
!!--begin--
Psi=(1+Omega(1)**2+Omega(2)**2)*(x**2)*(y**2)*(1-x**2)*(1-y**2) + &
  ANALYTIC_CONST
!!--end--
END FUNCTION

FUNCTION EXACT_dPsi_dx_WARSA(x,y,Omega) RESULT(dPsi_dx)
REAL(KIND_MCS) :: dPsi_dx
REAL(KIND_MCS),INTENT(IN) :: x,y,Omega(1:2)
REAL(KIND_MCS) :: alpha
!!--begin--
alpha = 1+Omega(1)**2+Omega(2)**2
dPsi_dx = alpha*(2*x-4*x**3)*(y**2-y**4)
!!--end--
END FUNCTION

FUNCTION EXACT_dPsi_dy_WARSA(x,y,Omega) RESULT(dPsi_dy)
REAL(KIND_MCS) :: dPsi_dy
REAL(KIND_MCS),INTENT(IN) :: x,y,Omega(1:2)
REAL(KIND_MCS) :: alpha
!!--begin--
alpha = 1+Omega(1)**2+Omega(2)**2
dPsi_dy = alpha*(2*y-4*y**3)*(x**2-x**4)
!!--end--
END FUNCTION

FUNCTION EXACT_Phi_WARSA(x,y) RESULT(Phi)
REAL(KIND_MCS) :: Phi
REAL(KIND_MCS),INTENT(IN) :: x,y
REAL(KIND_MCS) :: gamma,coeff
!!--begin--
coeff=(5.d0/3.d0)*C_4_times_PI
gamma = (x**2)*(y**2)*(1-x**2)*(1-y**2)
Phi=coeff*gamma + ANALYTIC_CONST*c_4_times_PI
!!--end--
END FUNCTION


FUNCTION COEFF_Sn1() RESULT(OUTCOEFF)
REAL(KIND_MCS) :: OUTCOEFF
REAL(KIND_MCS),ALLOCATABLE :: PsiLocal(:)
INTEGER :: m,Nm
LOGICAL,SAVE :: FirstTime=.TRUE.
REAL(KIND_MCS),SAVE :: coeff
IF( FirstTime )THEN
 Nm=SIZE(Ordinates,2)
 ALLOCATE( PsiLocal(Nm) )
 DO m=1,Nm
  PsiLocal(m) = (1+Ordinates(1,m)**2+Ordinates(2,m)**2)
 END DO
 coeff = Moment0( PsiLocal , Ordinates , Weights )
 DEALLOCATE( PsiLocal )
 FirstTime=.FALSE.
END IF
OUTCOEFF = coeff
END FUNCTION



FUNCTION COEFF_SnOmegax() RESULT(OUTCOEFF)
REAL(KIND_MCS) :: OUTCOEFF
REAL(KIND_MCS),ALLOCATABLE :: PsiLocal(:)
INTEGER :: m,Nm
LOGICAL,SAVE :: FirstTime=.TRUE.
REAL(KIND_MCS),SAVE :: coeff
IF( FirstTime )THEN
 Nm=SIZE(Ordinates,2)
 ALLOCATE( PsiLocal(Nm) )
 DO m=1,Nm
  PsiLocal(m) = Ordinates(1,m)*(1+Ordinates(1,m)**2+Ordinates(2,m)**2)
 END DO
 coeff = Moment0( PsiLocal , Ordinates , Weights )
 DEALLOCATE( PsiLocal )
 FirstTime=.FALSE.
END IF
OUTCOEFF = coeff
END FUNCTION

FUNCTION COEFF_SnOmegax2() RESULT(OUTCOEFF)
REAL(KIND_MCS) :: OUTCOEFF
REAL(KIND_MCS),ALLOCATABLE :: PsiLocal(:)
INTEGER :: m,Nm
LOGICAL,SAVE :: FirstTime=.TRUE.
REAL(KIND_MCS),SAVE :: coeff
IF( FirstTime )THEN
 Nm=SIZE(Ordinates,2)
 ALLOCATE( PsiLocal(Nm) )
 DO m=1,Nm
  PsiLocal(m) = (Ordinates(1,m)**2)*(1+Ordinates(1,m)**2+Ordinates(2,m)**2)
 END DO
 coeff = Moment0( PsiLocal , Ordinates , Weights )
 DEALLOCATE( PsiLocal )
 FirstTime=.FALSE.
END IF
OUTCOEFF = coeff
END FUNCTION


FUNCTION COEFF_SnOmegaxOmegay() RESULT(OUTCOEFF)
REAL(KIND_MCS) :: OUTCOEFF
REAL(KIND_MCS),ALLOCATABLE :: PsiLocal(:)
INTEGER :: m,Nm
LOGICAL,SAVE :: FirstTime=.TRUE.
REAL(KIND_MCS),SAVE :: coeff
IF( FirstTime )THEN
 Nm=SIZE(Ordinates,2)
 ALLOCATE( PsiLocal(Nm) )
 DO m=1,Nm
  PsiLocal(m) = Ordinates(1,m)*Ordinates(2,m)*(1+Ordinates(1,m)**2+Ordinates(2,m)**2)
 END DO
 coeff = Moment0( PsiLocal , Ordinates , Weights )
 DEALLOCATE( PsiLocal )
 FirstTime=.FALSE.
END IF
OUTCOEFF = coeff
END FUNCTION

FUNCTION COEFF_SnOmegay() RESULT(OUTCOEFF)
REAL(KIND_MCS) :: OUTCOEFF
REAL(KIND_MCS),ALLOCATABLE :: PsiLocal(:)
INTEGER :: m,Nm
LOGICAL,SAVE :: FirstTime=.TRUE.
REAL(KIND_MCS),SAVE :: coeff
IF( FirstTime )THEN
 Nm=SIZE(Ordinates,2)
 ALLOCATE( PsiLocal(Nm) )
 DO m=1,Nm
  PsiLocal(m) = Ordinates(2,m)*(1+Ordinates(1,m)**2+Ordinates(2,m)**2)
 END DO
 coeff = Moment0( PsiLocal , Ordinates , Weights )
 DEALLOCATE( PsiLocal )
 FirstTime=.FALSE.
END IF
OUTCOEFF = coeff
END FUNCTION

FUNCTION COEFF_SnOmegay2() RESULT(OUTCOEFF)
REAL(KIND_MCS) :: OUTCOEFF
REAL(KIND_MCS),ALLOCATABLE :: PsiLocal(:)
INTEGER :: m,Nm
LOGICAL,SAVE :: FirstTime=.TRUE.
REAL(KIND_MCS),SAVE :: coeff
IF( FirstTime )THEN
 Nm=SIZE(Ordinates,2)
 ALLOCATE( PsiLocal(Nm) )
 DO m=1,Nm
  PsiLocal(m) = (Ordinates(2,m)**2)*(1+Ordinates(1,m)**2+Ordinates(2,m)**2)
 END DO
 coeff = Moment0( PsiLocal , Ordinates , Weights )
 DEALLOCATE( PsiLocal )
 FirstTime=.FALSE.
END IF
OUTCOEFF = coeff
END FUNCTION


FUNCTION EXACT_SnPhi_WARSA(x,y) RESULT(SnPhi)
REAL(KIND_MCS) :: SnPhi
REAL(KIND_MCS),INTENT(IN) :: x,y
!!#### LOCAL VARIABLES
REAL(KIND_MCS) :: gamma,coeff

!!--begin--

coeff = COEFF_Sn1()
gamma = (x**2)*(y**2)*(1-x**2)*(1-y**2)
SnPhi = coeff*gamma + ANALYTIC_CONST*c_4_times_PI

!!--end--
END FUNCTION


FUNCTION EXACT_Jx_WARSA(x,y) RESULT(Jx)
REAL(KIND_MCS) :: Jx
REAL(KIND_MCS),INTENT(IN) :: x,y
!!#### LOCAL VARIABLES
REAL(KIND_MCS) :: gamma,coeff

!!--begin--

Jx = 0._KIND_MCS

!!--end--
END FUNCTION

FUNCTION EXACT_Jy_WARSA(x,y) RESULT(Jy)
REAL(KIND_MCS) :: Jy
REAL(KIND_MCS),INTENT(IN) :: x,y
!!#### LOCAL VARIABLES
REAL(KIND_MCS) :: gamma,coeff

!!--begin--

Jy = 0._KIND_MCS

!!--end--
END FUNCTION

FUNCTION EXACT_SnJx_WARSA(x,y) RESULT(SnJx)
REAL(KIND_MCS) :: SnJx
REAL(KIND_MCS),INTENT(IN) :: x,y
!!#### LOCAL VARIABLES
REAL(KIND_MCS) :: gamma,coeff

!!--begin--

gamma = (x**2)*(y**2)*(1-x**2)*(1-y**2)
coeff = COEFF_SnOmegax()
SnJx = gamma*coeff

!!--end--
END FUNCTION



FUNCTION EXACT_SnJy_WARSA(x,y) RESULT(SnJy)
REAL(KIND_MCS) :: SnJy
REAL(KIND_MCS),INTENT(IN) :: x,y
!!#### LOCAL VARIABLES
REAL(KIND_MCS) :: gamma,coeff

!!--begin--

gamma = (x**2)*(y**2)*(1-x**2)*(1-y**2)
coeff = COEFF_SnOmegay()
SnJy = gamma*coeff

!!--end--
END FUNCTION


FUNCTION EXACT_Qext_WARSA(x,y,Omega) RESULT(Qext)
REAL(KIND_MCS) :: Qext
REAL(KIND_MCS),INTENT(IN) :: x,y,Omega(1:2)
REAL(KIND_MCS) :: gamma,alpha,beta,coeff,dPsi_dx,dPsi_dy
!!--begin--
alpha = 1.d0 + (Omega(1))**2 + (Omega(2))**2
dPsi_dx = alpha*(2*x-4*x**3)*(y**2-y**4)
dPsi_dy = alpha*(2*y-4*y**3)*(x**2-x**4)
gamma = (x**2)*(y**2)*(1.d0-x**2)*(1.d0-y**2)
coeff = COEFF_1
Qext = Omega(1)*dPsi_dx + Omega(2)*dPsi_dy + &
       ANALYTIC_SIGMAT*gamma*alpha - &
       ANALYTIC_SIGMAS*coeff*gamma/c_4_times_PI + &
       ANALYTIC_CONST*(ANALYTIC_SIGMAT-ANALYTIC_SIGMAS)
!!--end--
END FUNCTION


FUNCTION EXACT_SnQext_WARSA(x,y,Omega) RESULT(SnQext)
REAL(KIND_MCS) :: SnQext
REAL(KIND_MCS),INTENT(IN) :: x,y,Omega(1:2)
REAL(KIND_MCS) :: gamma,alpha,beta,coeff,dPsi_dx,dPsi_dy
!!--begin--
alpha = 1.d0 + (Omega(1))**2 + (Omega(2))**2
dPsi_dx = alpha*(2*x-4*x**3)*(y**2-y**4)
dPsi_dy = alpha*(2*y-4*y**3)*(x**2-x**4)
gamma = (x**2)*(y**2)*(1.d0-x**2)*(1.d0-y**2)
coeff = COEFF_Sn1()
SnQext = Omega(1)*dPsi_dx + Omega(2)*dPsi_dy + &
       ANALYTIC_SIGMAT*gamma*alpha - &
       ANALYTIC_SIGMAS*coeff*gamma/c_4_times_PI + &
       ANALYTIC_CONST*(ANALYTIC_SIGMAT-ANALYTIC_SIGMAS)
!!--end--
END FUNCTION


FUNCTION EXACT_Mom0Qext_WARSA(x,y) RESULT(Mom0Qext)
REAL(KIND_MCS) :: Mom0Qext
REAL(KIND_MCS),INTENT(IN) :: x,y
REAL(KIND_MCS) :: gamma,beta,coeff,Phi
!!--begin--
gamma = (x**2)*(y**2)*(1-x**2)*(1-y**2)
coeff = COEFF_1
Phi   = coeff*gamma + ANALYTIC_CONST*INTEGRAL_1
Mom0Qext = ( ANALYTIC_SIGMAT - ANALYTIC_SIGMAS )*Phi
!!--end--
END FUNCTION

FUNCTION EXACT_SnMom0Qext_WARSA(x,y) RESULT(SnMom0Qext)
REAL(KIND_MCS) :: SnMom0Qext
REAL(KIND_MCS),INTENT(IN) :: x,y
REAL(KIND_MCS) :: gamma,beta,coeff,Phi
!!--begin--
gamma = (x**2)*(y**2)*(1-x**2)*(1-y**2)
coeff = COEFF_Sn1()
Phi   = coeff*gamma + ANALYTIC_CONST*INTEGRAL_Sn1()
SnMom0Qext = ( ANALYTIC_SIGMAT - ANALYTIC_SIGMAS )*Phi
!!--end--
END FUNCTION


FUNCTION EXACT_Mom1xQext_WARSA(x,y) RESULT(Mom1xQext)
REAL(KIND_MCS) :: Mom1xQext
REAL(KIND_MCS),INTENT(IN) :: x,y
!!--begin--
Mom1xQext = COEFF_Omegax2*(2*x-4*x**3)*(y**2-y**4)
!!--end--
END FUNCTION

FUNCTION EXACT_Mom1yQext_WARSA(x,y) RESULT(Mom1yQext)
REAL(KIND_MCS) :: Mom1yQext
REAL(KIND_MCS),INTENT(IN) :: x,y
!!--begin--
Mom1yQext = COEFF_Omegay2*(2*y-4*y**3)*(x**2-x**4)
!!--end--
END FUNCTION


FUNCTION EXACT_SnMom1xQext_WARSA(x,y) RESULT(SnMom1xQext)
REAL(KIND_MCS) :: SnMom1xQext
REAL(KIND_MCS),INTENT(IN) :: x,y
REAL(KIND_MCS) :: coeffx,coeffy
REAL(KIND_MCS) :: dgam_dx,dgam_dy
!!--begin--
coeffx = COEFF_SnOmegax2()
coeffy = COEFF_SnOmegaxOmegay()
dgam_dx = (2*x-4*x**3)*(y**2-y**4)
dgam_dy = (2*y-4*y**3)*(x**2-x**4)
SnMom1xQext = coeffx*dgam_dx + coeffy*dgam_dy
!!--end--
END FUNCTION

FUNCTION EXACT_SnMom1yQext_WARSA(x,y) RESULT(SnMom1yQext)
REAL(KIND_MCS) :: SnMom1yQext
REAL(KIND_MCS),INTENT(IN) :: x,y
REAL(KIND_MCS) :: coeffx,coeffy
REAL(KIND_MCS) :: dgam_dx,dgam_dy
!!--begin--
coeffx = COEFF_SnOmegaxOmegay()
coeffy = COEFF_SnOmegay2()
dgam_dx = (2*x-4*x**3)*(y**2-y**4)
dgam_dy = (2*y-4*y**3)*(x**2-x**4)
SnMom1yQext = coeffx*dgam_dx + coeffy*dgam_dy
!!--end--
END FUNCTION




FUNCTION INTEGRAL_SnOmegax() RESULT(OUTINT)
REAL(KIND_MCS) :: OUTINT
REAL(KIND_MCS),ALLOCATABLE :: PsiLocal(:)
INTEGER :: m,Nm
LOGICAL,SAVE :: FirstTime=.TRUE.
REAL(KIND_MCS),SAVE :: coeff
IF( FirstTime )THEN
 Nm=SIZE(Ordinates,2)
 ALLOCATE( PsiLocal(Nm) )
 DO m=1,Nm
  PsiLocal(m) = Ordinates(1,m)
 END DO
 coeff = Moment0( PsiLocal , Ordinates , Weights )
 DEALLOCATE( PsiLocal )
 FirstTime=.FALSE.
END IF
OUTINT = coeff
END FUNCTION


FUNCTION INTEGRAL_SnOmegay() RESULT(OUTINT)
REAL(KIND_MCS) :: OUTINT
REAL(KIND_MCS),ALLOCATABLE :: PsiLocal(:)
INTEGER :: m,Nm
LOGICAL,SAVE :: FirstTime=.TRUE.
REAL(KIND_MCS),SAVE :: coeff
IF( FirstTime )THEN
 Nm=SIZE(Ordinates,2)
 ALLOCATE( PsiLocal(Nm) )
 DO m=1,Nm
  PsiLocal(m) = Ordinates(2,m)
 END DO
 coeff = Moment0( PsiLocal , Ordinates , Weights )
 DEALLOCATE( PsiLocal )
 FirstTime=.FALSE.
END IF
OUTINT = coeff
END FUNCTION


FUNCTION INTEGRAL_Sn1() RESULT(OUTINT)
REAL(KIND_MCS) :: OUTINT
REAL(KIND_MCS),ALLOCATABLE :: PsiLocal(:)
INTEGER :: m,Nm
LOGICAL,SAVE :: FirstTime=.TRUE.
REAL(KIND_MCS),SAVE :: coeff
IF( FirstTime )THEN
 Nm=SIZE(Ordinates,2)
 ALLOCATE( PsiLocal(Nm) )
 DO m=1,Nm
  PsiLocal(m) = 1._KIND_MCS
 END DO
 coeff = Moment0( PsiLocal , Ordinates , Weights )
 DEALLOCATE( PsiLocal )
 FirstTime=.FALSE.
END IF
OUTINT = coeff
END FUNCTION


FUNCTION INTEGRAL_SnOmegax2() RESULT(OUTINT)
REAL(KIND_MCS) :: OUTINT
REAL(KIND_MCS),ALLOCATABLE :: PsiLocal(:)
INTEGER :: m,Nm
LOGICAL,SAVE :: FirstTime=.TRUE.
REAL(KIND_MCS),SAVE :: coeff
IF( FirstTime )THEN
 Nm=SIZE(Ordinates,2)
 ALLOCATE( PsiLocal(Nm) )
 DO m=1,Nm
  PsiLocal(m) = Ordinates(1,m)**2
 END DO
 coeff = Moment0( PsiLocal , Ordinates , Weights )
 DEALLOCATE( PsiLocal )
 FirstTime=.FALSE.
END IF
OUTINT = coeff
END FUNCTION


FUNCTION INTEGRAL_SnOmegaxOmegay() RESULT(OUTINT)
REAL(KIND_MCS) :: OUTINT
REAL(KIND_MCS),ALLOCATABLE :: PsiLocal(:)
INTEGER :: m,Nm
LOGICAL,SAVE :: FirstTime=.TRUE.
REAL(KIND_MCS),SAVE :: coeff
IF( FirstTime )THEN
 Nm=SIZE(Ordinates,2)
 ALLOCATE( PsiLocal(Nm) )
 DO m=1,Nm
  PsiLocal(m) = Ordinates(1,m)*Ordinates(2,m)
 END DO
 coeff = Moment0( PsiLocal , Ordinates , Weights )
 DEALLOCATE( PsiLocal )
 FirstTime=.FALSE.
END IF
OUTINT = coeff
END FUNCTION


FUNCTION INTEGRAL_SnOmegay2() RESULT(OUTINT)
REAL(KIND_MCS) :: OUTINT
REAL(KIND_MCS),ALLOCATABLE :: PsiLocal(:)
INTEGER :: m,Nm
LOGICAL,SAVE :: FirstTime=.TRUE.
REAL(KIND_MCS),SAVE :: coeff
IF( FirstTime )THEN
 Nm=SIZE(Ordinates,2)
 ALLOCATE( PsiLocal(Nm) )
 DO m=1,Nm
  PsiLocal(m) = Ordinates(2,m)**2
 END DO
 coeff = Moment0( PsiLocal , Ordinates , Weights )
 DEALLOCATE( PsiLocal )
 FirstTime=.FALSE.
END IF
OUTINT = coeff
END FUNCTION


FUNCTION EXACT_SnExx_WARSA(x,y) RESULT(SnExx)
REAL(KIND_MCS) :: SnExx
REAL(KIND_MCS),INTENT(IN) :: x,y
REAL(KIND_MCS) :: coeff,gamma,SnPhi,SnK
REAL(KIND_MCS) :: coeffn,coeffc
!!--begin--

coeff = COEFF_Sn1()
gamma = (x**2)*(y**2)*(1-x**2)*(1-y**2)
SnPhi = coeff*gamma + ANALYTIC_CONST*c_4_times_PI

coeffn = COEFF_SnOmegax2()
coeffc = INTEGRAL_SnOmegax2()

SnK = coeffn*gamma + ANALYTIC_CONST*coeffc

SnExx = SnK/SnPhi

!!--end--
END FUNCTION


FUNCTION EXACT_SnEyy_WARSA(x,y) RESULT(SnEyy)
REAL(KIND_MCS) :: SnEyy
REAL(KIND_MCS),INTENT(IN) :: x,y
REAL(KIND_MCS) :: coeff,gamma,SnPhi,SnK
REAL(KIND_MCS) :: coeffn,coeffc
!!--begin--

coeff = COEFF_Sn1()
gamma = (x**2)*(y**2)*(1-x**2)*(1-y**2)
SnPhi = coeff*gamma + ANALYTIC_CONST*c_4_times_PI

coeffn = COEFF_SnOmegay2()
coeffc = INTEGRAL_SnOmegay2()

SnK = coeffn*gamma + ANALYTIC_CONST*coeffc

SnEyy = SnK/SnPhi

!!--end--
END FUNCTION



FUNCTION EXACT_SnExy_WARSA(x,y) RESULT(SnExy)
REAL(KIND_MCS) :: SnExy
REAL(KIND_MCS),INTENT(IN) :: x,y
REAL(KIND_MCS) :: coeff,gamma,SnPhi,SnK
REAL(KIND_MCS) :: coeffn,coeffc
!!--begin--

coeff = COEFF_Sn1()
gamma = (x**2)*(y**2)*(1-x**2)*(1-y**2)
SnPhi = coeff*gamma + ANALYTIC_CONST*c_4_times_PI

coeffn = COEFF_SnOmegaxOmegay()
coeffc = INTEGRAL_SnOmegaxOmegay()

SnK = coeffn*gamma + ANALYTIC_CONST*coeffc

SnExy = SnK/SnPhi

!!--end--
END FUNCTION



FUNCTION EXACT_Exx_WARSA(x,y) RESULT(Exx)
REAL(KIND_MCS) :: Exx
REAL(KIND_MCS),INTENT(IN) :: x,y
REAL(KIND_MCS) :: coeff,gamma,Phi,K
REAL(KIND_MCS) :: coeffn,coeffc
!!--begin--

coeff = COEFF_1
gamma = (x**2)*(y**2)*(1-x**2)*(1-y**2)
Phi = coeff*gamma + ANALYTIC_CONST*c_4_times_PI

coeffn = COEFF_Omegax2
coeffc = INTEGRAL_Omegax2

K = coeffn*gamma + ANALYTIC_CONST*coeffc

Exx = K/Phi

!!--end--
END FUNCTION


FUNCTION EXACT_Eyy_WARSA(x,y) RESULT(Eyy)
REAL(KIND_MCS) :: Eyy
REAL(KIND_MCS),INTENT(IN) :: x,y
REAL(KIND_MCS) :: coeff,gamma,Phi,K
REAL(KIND_MCS) :: coeffn,coeffc
!!--begin--

coeff = COEFF_1
gamma = (x**2)*(y**2)*(1-x**2)*(1-y**2)
Phi = coeff*gamma + ANALYTIC_CONST*c_4_times_PI

coeffn = COEFF_Omegay2
coeffc = INTEGRAL_Omegay2

K = coeffn*gamma + ANALYTIC_CONST*coeffc

Eyy = K/Phi

!!--end--
END FUNCTION



FUNCTION EXACT_Exy_WARSA(x,y) RESULT(Exy)
REAL(KIND_MCS) :: Exy
REAL(KIND_MCS),INTENT(IN) :: x,y
REAL(KIND_MCS) :: coeff,gamma,Phi,K
REAL(KIND_MCS) :: coeffn,coeffc
!!--begin--

coeff = COEFF_1
gamma = (x**2)*(y**2)*(1-x**2)*(1-y**2)
Phi = coeff*gamma + ANALYTIC_CONST*c_4_times_PI

coeffn = COEFF_OmegaxOmegay
coeffc = INTEGRAL_OmegaxOmegay

K = coeffn*gamma + ANALYTIC_CONST*coeffc

Exy = K/Phi

!!--end--
END FUNCTION



FUNCTION EXACT_Kxx_WARSA(x,y) RESULT(Kxx)
REAL(KIND_MCS) :: Kxx
REAL(KIND_MCS),INTENT(IN) :: x,y
REAL(KIND_MCS) :: gamma
REAL(KIND_MCS) :: coeffn,coeffc
!!--begin--

gamma = (x**2)*(y**2)*(1-x**2)*(1-y**2)

coeffn = COEFF_Omegax2
coeffc = INTEGRAL_Omegax2

Kxx = coeffn*gamma + ANALYTIC_CONST*coeffc

!!--end--
END FUNCTION


FUNCTION EXACT_Kyy_WARSA(x,y) RESULT(Kyy)
REAL(KIND_MCS) :: Kyy
REAL(KIND_MCS),INTENT(IN) :: x,y
REAL(KIND_MCS) :: gamma
REAL(KIND_MCS) :: coeffn,coeffc
!!--begin--

gamma = (x**2)*(y**2)*(1-x**2)*(1-y**2)

coeffn = COEFF_Omegay2
coeffc = INTEGRAL_Omegay2

Kyy = coeffn*gamma + ANALYTIC_CONST*coeffc

!!--end--
END FUNCTION



FUNCTION EXACT_Kxy_WARSA(x,y) RESULT(Kxy)
REAL(KIND_MCS) :: Kxy
REAL(KIND_MCS),INTENT(IN) :: x,y
REAL(KIND_MCS) :: gamma
REAL(KIND_MCS) :: coeffn,coeffc
!!--begin--

gamma = (x**2)*(y**2)*(1-x**2)*(1-y**2)

coeffn = COEFF_OmegaxOmegay
coeffc = INTEGRAL_OmegaxOmegay

Kxy = coeffn*gamma + ANALYTIC_CONST*coeffc

!!--end--
END FUNCTION



FUNCTION EXACT_SnKxx_WARSA(x,y) RESULT(SnKxx)
REAL(KIND_MCS) :: SnKxx
REAL(KIND_MCS),INTENT(IN) :: x,y
REAL(KIND_MCS) :: gamma
REAL(KIND_MCS) :: coeffn,coeffc
!!--begin--

gamma = (x**2)*(y**2)*(1-x**2)*(1-y**2)

coeffn = COEFF_SnOmegax2()
coeffc = INTEGRAL_SnOmegax2()

SnKxx = coeffn*gamma + ANALYTIC_CONST*coeffc

!!--end--
END FUNCTION


FUNCTION EXACT_SnKyy_WARSA(x,y) RESULT(SnKyy)
REAL(KIND_MCS) :: SnKyy
REAL(KIND_MCS),INTENT(IN) :: x,y
REAL(KIND_MCS) :: gamma
REAL(KIND_MCS) :: coeffn,coeffc
!!--begin--

gamma = (x**2)*(y**2)*(1-x**2)*(1-y**2)

coeffn = COEFF_SnOmegay2()
coeffc = INTEGRAL_SnOmegay2()

SnKyy = coeffn*gamma + ANALYTIC_CONST*coeffc

!!--end--
END FUNCTION



FUNCTION EXACT_SnKxy_WARSA(x,y) RESULT(SnKxy)
REAL(KIND_MCS) :: SnKxy
REAL(KIND_MCS),INTENT(IN) :: x,y
REAL(KIND_MCS) :: gamma
REAL(KIND_MCS) :: coeffn,coeffc
!!--begin--

gamma = (x**2)*(y**2)*(1-x**2)*(1-y**2)

coeffn = COEFF_SnOmegaxOmegay()
coeffc = INTEGRAL_SnOmegaxOmegay()

SnKxy = coeffn*gamma + ANALYTIC_CONST*coeffc

!!--end--
END FUNCTION


FUNCTION EXACT_TEqn_WARSA(x,y,Omega) RESULT(zero)
!!#### PURPOSE
!! Get the residual of the transport equation (should be zero for this manufactured
!! test.

REAL(KIND_MCS) :: zero
REAL(KIND_MCS),INTENT(IN) :: x,y,Omega(1:2)
!!--begin--
zero = Omega(1)*EXACT_dPsi_dx_WARSA(x,y,Omega) + &
       Omega(2)*EXACT_dPsi_dy_WARSA(x,y,Omega) + &
       ANALYTIC_SIGMAT*EXACT_Psi_WARSA(x,y,Omega) - &
       ANALYTIC_SIGMAS*EXACT_Phi_WARSA(x,y)/c_4_times_PI - &
       EXACT_Qext_WARSA(x,y,Omega)
!!--end--
END FUNCTION

FUNCTION EXACT_SnTEqn_WARSA(x,y,Omega) RESULT(zero)
!!#### PURPOSE
!! Get the residual of the Sn transport equation (should be zero for this manufactured
!! test.

REAL(KIND_MCS) :: zero
REAL(KIND_MCS),INTENT(IN) :: x,y,Omega(1:2)
!!--begin--
zero = Omega(1)*EXACT_dPsi_dx_WARSA(x,y,Omega) + &
       Omega(2)*EXACT_dPsi_dy_WARSA(x,y,Omega) + &
       ANALYTIC_SIGMAT*EXACT_Psi_WARSA(x,y,Omega) - &
       ANALYTIC_SIGMAS*EXACT_SnPhi_WARSA(x,y)/c_4_times_PI - &
       EXACT_SnQext_WARSA(x,y,Omega)
!!--end--
END FUNCTION


END MODULE
