!!# MODULE <<LIB_RandomSubCellData>>
MODULE LIB_RandomSubcellData

!!## PURPOSE
!! Subroutines to generate random slices and random data on the slices:
!! * incoming angular fluxes and
!! * sources within the cell.

!!## MODULES
USE KND_IntrinsicTypes !!((01-A-KND_IntrinsicTypes.f90))
USE FUN_Random         !!((03-A-FUN_Random.f90))
USE FUN_Error          !!((04-A-FUN_Error.f90))

!!## DEFAULT IMPLICI
IMPLICIT NONE

!!## DEFAULT ACCESS
PRIVATE

!!## PUBLIC ACCESS
PUBLIC :: RandomSlice
PUBLIC :: RandomData_PosLin
PUBLIC :: RandomData_PosPara

!!## MODULE PROCEDURES
CONTAINS

SUBROUTINE RandomSlice(ub,t1,t2,smin1,smin2,smax1,smax2)
REAL(KIND_Rdp),INTENT(IN) :: ub
REAL(KIND_Rdp),INTENT(OUT) :: smin1,smin2,smax1,smax2,t1,t2
REAL(KIND_Rdp) :: ds2,ds1,dt

ds1=Random((/0._KIND_Rdp,ub/))
ds2=Random((/0._KIND_Rdp,ub/))
dt=Random((/0._KIND_Rdp,ub/))
t1=Random((/-ub,ub/))
t2=t1+dt
smin1=Random((/-ub,ub/))
smax1=smin1+ds1
smax2=Random((/smin1,ub/))
smin2=smax2-ds2
END SUBROUTINE



SUBROUTINE RandomData_PosLin(ubsig,ubq,t1,t2,smin1,smin2,smax1,smax2,sigma,q1,qs,qt,sintheta)
REAL(KIND_Rdp),INTENT(IN) :: ubsig,ubq,t1,t2,smin1,smin2,smax1,smax2
REAL(KIND_Rdp),INTENT(OUT) :: sigma,q1,qs,qt,sintheta
REAL(KIND_Rdp) :: qt1,qt2,qs1,qs2,smin,smax
REAL(KIND_Rdp) :: qp1,qp2,qp3,qp4
REAL(KIND_Rdp) :: power

!!--begin--

!get power
power = LOG10(ubsig)
power = Random((/-power,power/))

!get sigma
sigma=10._KIND_Rdp**(power)
sigma=MAX(0._KIND_Rdp,sigma) !bound >=0

smax = MAX(smax2,smax1)
smin = MIN(smin2,smin1)

sintheta=Random((/-0.1_KIND_Rdp,1.1_KIND_Rdp/))
sintheta=MIN(Max(0.0_KIND_Rdp,sintheta),1.0_KIND_Rdp) !bound to [0,1]

33 CONTINUE

!create a completely positive random source
!constant
q1=Random((/0._KIND_Rdp,ubq/))

!t part
qt1=Random((/0._KIND_Rdp,ubq/))
qt2=Random((/0._KIND_Rdp,ubq/))
qt=(qt2-qt1)/(t2-t1)

!s part
qs1=Random((/0._KIND_Rdp,ubq/))
qs2=Random((/0._KIND_Rdp,ubq/))
qs=(qs2-qs1)/(smax-smin)

!q(s,t) = qs*(s-smin) + qt*(t-t1) + q1
!modify source
q1=q1 - t1*qt - smin*qs

!calculate four points
qp1 = q1+qt*t1+qs*smin1
qp2 = q1+qt*t2+qs*smin2
qp3 = q1+qt*t2+qs*smax2
qp4 = q1+qt*t1+qs*smax1
IF( ANY((/qp1,qp2,qp3,qp4/)<0._KIND_Rdp) )THEN
 GOTO 33
END IF

!!--end--
END SUBROUTINE


SUBROUTINE RandomData_PosPara(ubsig,ubq,t1,t2,smin1,smin2,smax1,smax2,sigma,c,b,a,sintheta)
REAL(KIND_Rdp),INTENT(IN) :: ubsig,ubq,t1,t2,smin1,smin2,smax1,smax2
REAL(KIND_Rdp),INTENT(OUT) :: sigma,c,b,a,sintheta
REAL(KIND_Rdp) :: smin,smax
REAL(KIND_Rdp) :: Extrema,LenEdge
REAL(KIND_Rdp) :: Psi1,Psi2,PsiAvg,PsiEdge(3),PsiInOut(3)
LOGICAL        :: HasExtrema
REAL(KIND_Rdp) :: power

!!--begin--

!get power
power = LOG10(ubsig)
power = Random((/-power,power/))

!get sigma
sigma=10._KIND_Rdp**(power)
sigma=MAX(0._KIND_Rdp,sigma) !bound >=0

smax = MAX(smax2,smax1)
smin = MIN(smin2,smin1)

sintheta=Random((/-0.1_KIND_Rdp,1.1_KIND_Rdp/))
sintheta=MIN(Max(0.0_KIND_Rdp,sintheta),1.0_KIND_Rdp) !bound to [0,1]

34 CONTINUE

Psi1     = Random((/0._KIND_Rdp,ubq/))
Psi2     = Random((/0._KIND_Rdp,ubq/))
PsiAvg   = Random((/Psi1,Psi2/))
LenEdge  = SQRT( (t2-t1)**2 + (smin2-smin1)**2 )
PsiEdge  = PsiEdge_from_2Pt1Avg( PsiAvg , LenEdge , &
                                 Psi1 , Psi2 , &
                                 HasExtrema , Extrema )
! WRITE(*,*)"-----"
! WRITE(*,*)"Psi1=",Psi1
! WRITE(*,*)"Psi2=",Psi2
! WRITE(*,*)"PsiAvg=",PsiAvg
! WRITE(*,*)"LenEdge=",LenEdge
! WRITE(*,*)"PsiEdge=",PsiEdge
! WRITE(*,*)"HasExtrema=",HasExtrema
! WRITE(*,*)"Extrema=",Extrema
! WRITE(*,*)"-----"

IF( HasExtrema )THEN
 GOTO 34
END IF

PsiInOut = PsiInOut_from_PsiEdge( PsiEdge , LenEdge , (/t1,t2/) , &
                                  PsiAvg )
c = PsiInOut(1)
b = PsiInOut(2)
a = PsiInOut(3)
Extrema = (-b/(2*a)) + t1

! WRITE(*,*)"-----"
! WRITE(*,*)"c=",c
! WRITE(*,*)"b=",b
! WRITE(*,*)"a=",a
! WRITE(*,*)"Extrema=",Extrema
! WRITE(*,*)"t1=",t1
! WRITE(*,*)"t2=",t2
! WRITE(*,*)"-----"

IF( Extrema>t1 .AND. Extrema<t2 )THEN
 35 CONTINUE
 !can debug here if desired
END IF

!!--end--
END SUBROUTINE


!!### FUNCTION <<PsiEdge_from_2Pt1Avg>>
FUNCTION PsiEdge_from_2Pt1Avg( PsiAvg , LenEdge , &
  Psi1 , Psi2 , HasExtrema , Extrema ) RESULT(PsiEdge)

!!#### PURPOSE
!! Convert 3 values of the outgoing edge to a parabolic function.


!!#### REQUIRED INPUT
!! * the average over an outgoing edge <PsiAvg>
!! * the length of the edge $L=$<LenEdge>
!! * the point values at the left and right <Psi1> and <Psi2>
REAL(KIND_Rdp),INTENT(IN) :: PsiAvg
REAL(KIND_Rdp),INTENT(IN) :: LenEdge
REAL(KIND_Rdp),INTENT(IN) :: Psi1
REAL(KIND_Rdp),INTENT(IN) :: Psi2

!!#### REQUIRED OUTPUT
!! * the 1D parabolic edge function for $\ell \in [0,L]$
!      PsiEdge(1) + PsiEdge(2) * \ell + PsiEdge(3) * \ell**2
REAL(KIND_Rdp) :: PsiEdge(3)

!!#### OPTIONAL OUTPUT
!! * whether or not there is an extrema in the region
LOGICAL       ,INTENT(OUT),OPTIONAL :: HasExtrema
REAL(KIND_Rdp),INTENT(OUT),OPTIONAL :: Extrema

!!#### LOCAL VARIABLES
REAL(KIND_Rdp) :: Extrema_

!!--begin--

IF( PRESENT(Extrema) )THEN
 Extrema = Error(Extrema)
END IF

!parabolic interpolant
PsiEdge(3) = ( 3*Psi1 - 6*PsiAvg + 3*Psi2 )/LenEdge**2
PsiEdge(2) = (-4*Psi1 + 6*PsiAvg - 2*Psi2 )/LenEdge
PsiEdge(1) = Psi1

IF( PRESENT(HasExtrema) )THEN
 Extrema_ = -PsiEdge(2)/(2*PsiEdge(3))
 HasExtrema = Extrema_>0._KIND_Rdp .AND. Extrema_<LenEdge
 IF( PRESENT(Extrema) )THEN
  IF( HasExtrema )THEN
   Extrema = Extrema_
  END IF
 END IF
END IF

!!--end--
END FUNCTION


!!### FUNCTION <<PsiInOut_from_PsiEdge>>
FUNCTION PsiInOut_from_PsiEdge( PsiEdge , LenEdge , T , &
  PsiAvg ) RESULT(PsiInOut)

!!#### PURPOSE
!! Convert a parabolic function representation on an edge ($\ell$
!! coordinates) to the slice edge ($t$ coordinates).

!!#### DETAILS
!! The conversion centers around the following expression
!!
!! $$ \ell(t) = \frac{t-T_1}{T_2-T_1} L      \quad \ell \in [0,L] $$
!!
!! and the inverse
!!
!! $$ t(\ell) = T_1 + \frac{\ell}{L} (T_2-T_1) \quad t \in [T_1,T_2]. $$
!!
!! This $T_1$ and $T_2$ are not the same as the $t_1$ and $t_2$
!! that form the bounds for a slice.  These are for a whole edge.


!!#### REQUIRED INPUT
!! * the 1D parabolic edge function for $\ell \in [0,L]$
!      PsiEdge(1) + PsiEdge(2) * l + PsiEdge(3)* l**2
!! * the length of the edge $L=$<LenEdge>
!! * the t-bounds of the EDGE (not slice)
REAL(KIND_Rdp),INTENT(IN) :: PsiEdge(3)
REAL(KIND_Rdp),INTENT(IN) :: LenEdge
REAL(KIND_Rdp),INTENT(IN) :: T(2)
REAL(KIND_Rdp),INTENT(IN) :: PsiAvg

!!#### REQUIRED OUTPUT
!! * the 1D parabolic edge function for $t \in [t1,t2]$
!      PsiInOut(1) + PsiInOut(2) * t + PsiInOut(3) * t**2
REAL(KIND_Rdp) :: PsiInOut(3)

!!#### LOCAL VARIABLES
REAL(KIND_Rdp) :: Dt,LdivDt1,LdivDt2,Lm
INTEGER :: i

!!--begin--

!get some convenience variables
Dt = T(2) - T(1)

!set the output
IF( Dt<1.E-10_KIND_Rdp )THEN
 PsiInOut(1) = PsiAvg
 PsiInOut(2) = 0._KIND_Rdp
 PsiInOut(3) = 0._KIND_Rdp
ELSE
 LdivDt1 = LenEdge/Dt
 LdivDt2 = LdivDt1**2
 PsiInOut(1) = PsiEdge(1) - (T(1))*PsiEdge(2)*LdivDt1 +   (T(1)**2)*PsiEdge(3)*LdivDt2
 PsiInOut(2) =                     PsiEdge(2)*LdivDt1 - 2*(T(1)   )*PsiEdge(3)*LdivDt2
 PsiInOut(3) =                                                      PsiEdge(3)*LdivDt2
END IF

!!--end--
END FUNCTION



END MODULE
