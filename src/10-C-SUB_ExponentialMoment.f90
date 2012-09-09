!!# SUBROUTINE MODULE <<SUB_ExponentialMoment>>
MODULE SUB_ExponentialMoment

!!## PURPOSE
!! Return the numerically stable calculation of the exponential
!! moment functions, $M_n(x)$.


!!## DETAILS
!! Wouldn't use for $n>10$ or so, although this case is hardly ever
!! needed.


!!## REFERENCES
! D. J. Miller, K. A. Mathews*, and C. R. Brennan, "SPLIT-CELL
! DISCRETE ORDINATES TRANSPORT ON AN UNSTRUCTURED GRID OF
! TRIANGULAR CELLS", TRANSPORT THEORY AND STATISTICAL PHYSICS,
! 25(7). 833-867 (1996).


!!## OWNER
! [waw] william.wieselquist@gmail.com


!!## DEPENDENCIES
USE KND_IntrinsicTypes                  !!((01-A-KND_IntrinsicTypes.f90))
USE FUN_Factorial                       !!((06-B-FUN_Factorial.f90))
USE FUN_Default                         !!((04-A-FUN_Default.f90))
USE FUN_Integrate1_aq                   !!((06-B-FUN_Integrate1_aq.f90))


!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## DEFAULT ACCESS
PRIVATE

!!## LOCAL VARIABLES
!small switch must be greater than 1
REAL(KIND_Rdp),SAVE :: SMALL_x=0.5_KIND_Rdp
!REAL(KIND_Rdp),SAVE :: SMALL_x=1.E-3_KIND_Rdp


!!## PUBLIC ACCESS LIST
PUBLIC :: ExponentialMoment
PUBLIC :: PRINT_ExponentialMoment
PUBLIC :: PRINT_NExponentialMoment

CONTAINS



!!### SUBROUTINE <<NExponentialMoment>>
SUBROUTINE NExponentialMoment( N , x , M , tol )

!!#### PURPOSE
!! Return the exponential moments $M_n(x)$, for $n=0,1,...,N$
!! using adaptive numerical integration.


!!#### REQUIRED INPUT
INTEGER       ,INTENT(IN) :: N
REAL(KIND_Rdp),INTENT(IN) :: x

!!#### REQUIRED OUTPUT
REAL(KIND_Rdp),INTENT(OUT) :: M(0:N)

!!#### OPTIONAL INPUT
REAL(KIND_Rdp),INTENT(IN),OPTIONAL :: tol

!!#### LOCAL VARIABLES
INTEGER        :: i
REAL(KIND_Rdp) :: tol_

!!--begin--
tol_ = DEFAULT( 10._KIND_Rdp*EPSILON(tol) , tol )
M = 0._KIND_Rdp

DO i=0,N
 M(i) = NExponentialMoment0(i,x,tol_)
END DO

!!--end--
END SUBROUTINE



!!### FUNCTION <<NExponentialMoment0>>
FUNCTION NExponentialMoment0(n,x,tol) RESULT(val)

!!#### PURPOSE
!! Return the numerical integration of the
!! exponential moment function, $M_n(x)$ for a single $n$.

!!#### REQUIRED INPUT
!! * moment order $n$
!! * argument of simplified exponential moment function $M_n(x)$
!! * tolerance on solution
INTEGER       ,INTENT(IN) :: n
REAL(KIND_Rdp),INTENT(IN) :: x,tol

!!#### REQUIRED OUTPUT
REAL(KIND_Rdp) :: val

!!--begin--

val = Integrate1_aq( Mint , (/0._KIND_Rdp,1._KIND_Rdp/) , N=10000 , tol=tol )

!!--end--
CONTAINS

PURE FUNCTION Mint(t)
REAL(KIND_Rdp),INTENT(IN) :: t
REAL(KIND_Rdp) :: Mint
!!--begin--
Mint = (1._KIND_Rdp-t)**n * EXP(-x*t)
!!--end--
END FUNCTION

END FUNCTION





!!### SUBROUTINE <<ExponentialMoment>>
SUBROUTINE ExponentialMoment( N , x , M , tol )

!!#### PURPOSE
!! Return the exponential moments $M_n(x)$, for $n=0,1,...,N$.
!! For small arguments <x>, an expansion is used.

!!#### REQUIRED INPUT
INTEGER       ,INTENT(IN) :: N
REAL(KIND_Rdp),INTENT(IN) :: x

!!#### REQUIRED OUTPUT
REAL(KIND_Rdp),INTENT(OUT) :: M(0:N)

!!#### OPTIONAL INPUT
REAL(KIND_Rdp),INTENT(IN),OPTIONAL :: tol

!!#### LOCAL VARIABLES
INTEGER :: i
REAL(KIND_Rdp) :: tol_,reali

!!--begin--

tol_ = DEFAULT( 10._KIND_Rdp*EPSILON(tol) , tol )
M = 0._KIND_Rdp

!standard recursive algorithm if x is not too small
IF( x>SMALL_x )THEN

 !normal forward recursion (for most cases)
 !IF( x>REAL(N,KIND(x)) )THEN

  M(0) = ( 1._KIND_Rdp - EXP(-x) )/x
  DO i=1,N
   M(i) = ( 1._KIND_Rdp - i*M(i-1) )/x
  END DO

 !backward recursion (used mostly for large N or X)
 ![waw] commented about because it seemed to blow up
 !ELSE
 !
 ! M(N) = NExponentialMoment0(N,x,tol_)
 ! DO i=N,1,-1
 !  M(i-1) = ( 1._KIND_Rdp - x*M(i) )/REAL(i,KIND(x))
 ! END DO
 !
 !END IF


!x is small (use backward recursion)
ELSE

 M(N) = ZeroExpansion(N,x,tol_)
 DO i=N,1,-1
  M(i-1) = ( 1._KIND_Rdp - x*M(i) )/REAL(i,KIND(x))
 END DO

END IF


!!--end--
END SUBROUTINE


!!### FUNCTION <<ZeroExpansion>>
FUNCTION ZeroExpansion(n,x,tol) RESULT(val)

!!#### PURPOSE
!! Return the result MacLaurin expansion of the exponential moment
!! function, $M_n(x)$ for small $x$.

!!#### REQUIRED INPUT
!! * moment order $n$
!! * argument of exponential moment function $M_n(x)$
!! * tolerance on solution
INTEGER       ,INTENT(IN) :: n
REAL(KIND_Rdp),INTENT(IN) :: x,tol

!!#### LOCAL VARIABLES
INTEGER        :: k,i
REAL(KIND_Rdp) :: val,terms(0:100) ![hack] assume 100 terms needed at most (1-3 terms expected)

!!--begin--

k = -1
DO
 k = k + 1
 terms(k) = ( Factorial(n)/Factorial(n+1+k) ) * (-x)**k
 !WRITE(*,*)"terms(k))=",terms(k)
 IF( ABS(terms(k))<tol )EXIT
END DO

!accumulate terms in reverse order to not lose precision
!(terms are monotonically decreasing with k)
val = 0._KIND_Rdp
DO i=k,0,-1
 val = val + terms(i)
 !WRITE(*,*)"val=",val
END DO

!!--end--
END FUNCTION



!!### SUBROUTINE <<PRINT_ExponentialMoment>>
SUBROUTINE PRINT_ExponentialMoment(N,Unit,xstart,xdiv,ndiv,tol)

!!#### MODULES
USE FUN_STR                             !!((05-B-FUN_STR.f90))
USE FUN_Default                         !!((04-A-FUN_Default.f90))
USE VAR_Units,ONLY: DEFAULT_OUTPUT_UNIT !!((03-A-VAR_Units.f90))

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: N

!!#### OPTIONAL INPUT
INTEGER       ,OPTIONAL,INTENT(IN) :: Unit
REAL(KIND_Rdp),OPTIONAL,INTENT(IN) :: xstart,xdiv,tol
INTEGER       ,OPTIONAL,INTENT(IN) :: ndiv

!!#### LOCAL VARIABLES
REAL(KIND_Rdp) :: x,Mfun(0:N),xstart_,xdiv_
INTEGER        :: i,Unit_,ndiv_

!!--begin--
xstart_ = Default( 10._KIND_Rdp , xstart )
xdiv_   = Default( 10._KIND_Rdp , xdiv )
Unit_   = Default( DEFAULT_OUTPUT_UNIT , Unit )
ndiv_   = Default( 10 , ndiv )

x=xstart_
DO i=1,ndiv_+1
 CALL ExponentialMoment( N , x , Mfun(0:2) ,tol=tol )
 WRITE(Unit_,"("//TRIM(STR(N+2))//"(E,1x))")x,Mfun(0:N)
 x=x/xdiv_
END DO

!!--end--
END SUBROUTINE



!!### SUBROUTINE <<PRINT_NExponentialMoment>>
SUBROUTINE PRINT_NExponentialMoment(N,Unit,xstart,xdiv,ndiv,tol)

!!#### MODULES
USE FUN_STR                             !!((05-B-FUN_STR.f90))
USE FUN_Default                         !!((04-A-FUN_Default.f90))
USE VAR_Units,ONLY: DEFAULT_OUTPUT_UNIT !!((03-A-VAR_Units.f90))

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: N

!!#### OPTIONAL INPUT
INTEGER       ,OPTIONAL,INTENT(IN) :: Unit
REAL(KIND_Rdp),OPTIONAL,INTENT(IN) :: xstart,xdiv,tol
INTEGER       ,OPTIONAL,INTENT(IN) :: ndiv

!!#### LOCAL VARIABLES
REAL(KIND_Rdp) :: x,Mfun(0:N),xstart_,xdiv_
INTEGER        :: i,Unit_,ndiv_

!!--begin--
xstart_ = Default( 10._KIND_Rdp , xstart )
xdiv_   = Default( 10._KIND_Rdp , xdiv )
Unit_   = Default( DEFAULT_OUTPUT_UNIT , Unit )
ndiv_   = Default( 10 , ndiv )

x=xstart_
DO i=1,ndiv_+1
 CALL NExponentialMoment( N , x , Mfun(0:2) ,tol=tol )
 WRITE(Unit_,"("//TRIM(STR(N+2))//"(E,1x))")x,Mfun(0:N)
 x=x/xdiv_
END DO

!!--end--
END SUBROUTINE



END MODULE
