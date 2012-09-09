!!# SUBROUTINE MODULE <<USR_PolyExpoMoment>>
MODULE USR_PolyExpoMoment

!!## PURPOSE
!! Return the numerically stable calculation of the simplified
!! exponential moment functions, $PM_n(x)$.
!!
!! $$ PM_n(x) = \int_0^1 (t)^n Exp( -x t ) dt \quad n=0,1,...$$



!!## DETAILS
!! Note that Miller and Mathews have defined the exponential
!! moment functions $M_n$,
!!
!! $$ M_n(x) = \int_0^1 (1-t)^n Exp( -x t ) dt \quad n=0,1,...$$
!!
!! where the presence of $(1-t)^n$ makes it difficult to rewrite
!! moments of polynomial expressions of higher order than $t$, i.e.
!! $t^2$.


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
USE FUN_Integrate1_aq                   !!((06-B-FUN_Integrate1_aq.f90))
USE FUN_Default                         !!((04-A-FUN_Default.f90))
USE FUN_CoefficientBinomial             !!((10-B-FUN_CoefficientBinomial.f90))
USE FUN_STR                             !!((05-B-FUN_STR.f90))
USE FUN_FactorialLn                     !!((08-B-FUN_FactorialLn.f90))
USE FUN_IsApprox                        !!((03-A-FUN_IsApprox.f90))
USE FUN_NewFile                         !!((05-B-FUN_NewFile.f90))
USE FUN_Random                          !!((03-A-FUN_Random.f90))
USE LIB_FunctionParser                  !!((04-B-LIB_FunctionParser.f90))
USE FUN_GammaLn                         !!((05-B-FUN_GammaLn.f90))
USE LIB_NSWCspecfunc                    !!((03-C-LIB_NSWCspecfunc.f90))
USE PAR_FactorialsEtc                   !!((02-C-PAR_FactorialsEtc.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## DEFAULT ACCESS
PRIVATE

!!## LOCAL VARIABLES
!! * the kind for this module
INTEGER,PARAMETER  :: KIND_PM=KIND_Rdp
!! * the small $x$ value for which we start to use
!!   a series expansion (must be less than 1)
REAL(KIND_PM) :: SMALL_x=0.1_KIND_PM
!! * the default tolerance (only needed for series expansion truncation)
REAL(KIND_PM) :: PM_DEFAULT_TOL    =10._KIND_PM*EPSILON(1._KIND_PM)
REAL(KIND_PM) :: PM_DEFAULT_TESTTOL=1.E-12_KIND_PM

!!## PUBLIC ACCESS LIST
PUBLIC :: KIND_PM
PUBLIC :: SMALL_X
PUBLIC :: PM_DEFAULT_TOL
PUBLIC :: PM_DEFAULT_TESTTOL

PUBLIC :: PolyExpoMoment
PUBLIC :: PolyExpoMomentWithB
PUBLIC :: PolyExpoMoment2
PUBLIC :: PRINT_PolyExpoMoment
PUBLIC :: PRINT_PolyExpoMoment2
PUBLIC :: CHECK1_PolyExpoMoment
PUBLIC :: CHECK1_PolyExpoMoment2
PUBLIC :: CHECK1_EVAL_Pij
PUBLIC :: EVAL_Pij

LOGICAL,PARAMETER :: FORCE_FACTORIAL_EVAL = .FALSE.



CONTAINS




!!### FUNCTION <<EVAL_Pij>>
FUNCTION EVAL_Pij( i , j , b1 , bt ) RESULT(EVAL)
!!#### PURPOSE
!! Evaluate polynomial moments, $\mathcal{P}_{ij}$,
!!
!!\begin{eqnarray}\label{polymomdef}
!!\mathcal{P}_{ij} &=& \int_0^1 d\tilde{t} \, \tilde{t}^i \int_0^{B(\tilde{t})}\, \hspace{-10pt} d\tilde{s} \tilde{s}^j \\                 &=& \frac{1}{j+1} \sum_{k=0}^{j+1} \frac{(j+1)!}{(j+1-k)!\,k!} \frac{B_t^{k} B_1^{j+1-k} }{k+i+1}.\nonumber
!!\end{eqnarray}


!!#### REQUIRED INPUT
REAL(KIND_PM),INTENT(IN) :: b1,bt
INTEGER,INTENT(IN) :: i,j

!!#### REQUIRED OUTPUT
REAL(KIND_PM) :: EVAL

!!#### LOCAL VARIABLES
REAL(KIND_PM) :: factor,b1pow,btpow,sum,coeff
INTEGER :: k

!!--begin--
sum=0._KIND_PM
DO k=0,j+1

 !safer this way for large factorials
 if( FORCE_FACTORIAL_EVAL .OR. j+1>MAX_N_BinomialCoeff )then
  coeff = Exp( FactorialLn(j+1)-FactorialLn(j+1-k)-FactorialLn(k) )
 else
  coeff = c_BinomialCoeff(k,j+1)
 end if
 factor = coeff / REAL(k+i+1,KIND_PM)

 b1pow = b1**(j+1-k)
 btpow = bt**(k)
 sum = sum + b1pow*btpow*factor
 !/factor
END DO
EVAL = sum/REAL(j+1,KIND_PM)

!!--end--
END FUNCTION



!!### FUNCTION <<CHECK1_EVAL_Pij>>
FUNCTION CHECK1_EVAL_Pij() RESULT(Pass)

!!#### PURPOSE
!! Check a couple polynomial moments, $\mathcal{P}_{ij}$.


!!#### REQUIRED OUTPUT
LOGICAL :: Pass
REAL(KIND_PM) :: b1,bt,mine,exact

!!--begin--

b1=0.1_KIND_PM
bt=0.9_KIND_PM
mine = EVAL_Pij( 0 , 0 , b1 , bt )
exact = 11._KIND_PM/20._KIND_PM
Pass = IsApprox(mine,exact)
IF( .NOT.Pass )RETURN

b1=0.1_KIND_PM
bt=0.9_KIND_PM
mine = EVAL_Pij( 0 , 5 , b1 , bt )
exact = 1111111._KIND_PM/42000000._KIND_PM
Pass = IsApprox(mine,exact)
IF( .NOT.Pass )RETURN

b1=0.1_KIND_PM
bt=0.9_KIND_PM
mine = EVAL_Pij( 5 , 0 , b1 , bt )
exact = 61._KIND_PM/420._KIND_PM
Pass = IsApprox(mine,exact)
IF( .NOT.Pass )RETURN

b1=0.0_KIND_PM
bt=1.0_KIND_PM
mine = EVAL_Pij( 1 , 0 , b1 , bt )
exact = 1._KIND_PM/3._KIND_PM
Pass = IsApprox(mine,exact)
IF( .NOT.Pass )RETURN

b1=1.0_KIND_PM
bt=0.0_KIND_PM
mine = EVAL_Pij( 1 , 0 , b1 , bt )
exact = 0.5_KIND_PM
Pass = IsApprox(mine,exact)
IF( .NOT.Pass )RETURN


!!--end--
END FUNCTION



!!### SUBROUTINE <<PolyExpoMoment>>
SUBROUTINE PolyExpoMoment( MaxN , x , x0 , c , PM , tol )

!!#### PURPOSE
!! Return the simplified exponential moments $PM_n(x)$, for $n=0,1,...,N$.
!! For small arguments <x>, an expansion is used.

!!#### REQUIRED INPUT
INTEGER       ,INTENT(IN) :: MaxN
REAL(KIND_PM),INTENT(IN) :: x
REAL(KIND_PM),INTENT(IN) :: x0,c

!!#### REQUIRED OUTPUT
REAL(KIND_PM),INTENT(OUT) :: PM(0:MaxN)

!!#### OPTIONAL INPUT

REAL(KIND_PM),INTENT(IN),OPTIONAL :: tol

!!#### LOCAL VARIABLES
INTEGER :: n,ierr
REAL(KIND_PM) :: tol_,inside,PMrecurse(0:MaxN),NInt,ZExp,ExpX,a,ans,qans

!!--begin--

tol_ = DEFAULT( PM_DEFAULT_TOL , tol )
PM = 0._KIND_PM

!standard recursive algorithm if x is not too small
IF( x>SMALL_x )THEN

 !normal forward recursion (for most cases)
 IF( x>REAL(MaxN,KIND(x)) .OR. MaxN<5 )THEN

  inside = 1._KIND_PM - EXP(-x)
  PM(0)        = inside/x0
  PMrecurse(0) = inside/x

  DO n=1,MaxN
   inside = n*PMrecurse(n-1) - EXP(-x)
   PM(n)        = inside/x0
   PMrecurse(n) = inside/x
  END DO

 !backward recursion
 ELSE

  !remove need for numerical evaluation
  ExpX = EXP(-x)
  !NInt = NPolyExpoMoment0(MaxN,x,tol_)
  a=1.0_KIND_Rdp + REAL(MaxN,KIND_Rdp)
  CALL dgrat(a, x, ans, qans, ierr)
  NInt=EXP(GammaLn(a))*ans/(x**a)
  PM(MaxN)        = c*NInt
  PMrecurse(MaxN) =   NInt
  DO n=MaxN,1,-1
   PM(n-1)        = ( x0*PMrecurse(n) + ExpX )/REAL(n,KIND(x))
   PMrecurse(n-1) = (  x*PMrecurse(n) + ExpX )/REAL(n,KIND(x))
  END DO

 END IF


!x is small (use backward recursion)
ELSE

 !get the values directly
 DO n=MaxN,0,-1
  PM(n) = c*ZeroExpansion(n,x,tol_)
 END DO

 !get the values by recursion
 !ZExp            = ZeroExpansion(MaxN,x,tol_)
 !PM(MaxN)        = c*ZExp
 !PMrecurse(MaxN) =   ZExp
 !ExpX = EXP(-x)
 !DO n=MaxN,1,-1
 ! PM(n-1)        = ( x0*PMrecurse(n) + ExpX )/REAL(n,KIND(x))
 ! PMrecurse(n-1) = (  x*PMrecurse(n) + ExpX )/REAL(n,KIND(x))
 !END DO

END IF


!!--end--
END SUBROUTINE



!!### FUNCTION <<ZeroExpansion>>
FUNCTION ZeroExpansion(n,x,tol) RESULT(val)

!!#### PURPOSE
!! Return the result MacLaurin expansion of the simplified
!! exponential moment function, $PM_n(x)$ for small $x$.

!!#### REQUIRED INPUT
!! * moment order $n$
!! * argument of simplified exponential moment function $PM_n(x)$
!! * tolerance on solution
INTEGER      ,INTENT(IN) :: n
REAL(KIND_PM),INTENT(IN) :: x,tol

!!#### LOCAL VARIABLES
INTEGER       :: k,i
REAL(KIND_PM) :: val,terms(0:100) ![hack] assume 100 terms needed at most (1-3 terms expected)
REAL(KIND_PM) :: invfact

!!--begin--

k = -1
DO
 k = k + 1

 IF( FORCE_FACTORIAL_EVAL .OR. k>MAX_N_Factorial )THEN
  invfact = EXP(-FactorialLn(k))
 ELSE
  invfact = REAL( c_InvFactorial(k) , KIND_Rdp)
 END IF

 terms(k) = invfact * (-x)**k / ( REAL(n+k+1,KIND_Rdp) )

 !WRITE(*,*)"terms(k))=",terms(k)
 !relative tolerance
 IF( ABS(terms(k))<tol*terms(0) )EXIT
END DO

!accumulate terms in reverse order to not lose precision
!(terms are monotonically decreasing with k)
val = 0._KIND_PM
DO i=k,0,-1
 val = val + terms(i)
 !WRITE(*,*)"val=",val
END DO

!!--end--
END FUNCTION




!!### SUBROUTINE <<PolyExpoMomentWithB>>
SUBROUTINE PolyExpoMomentWithB( MaxN , x , x0 , c , b , PM , tol )

!!#### PURPOSE
!! Return the simplified exponential moments $PM_n(x)$, for $n=0,1,...,N$.
!! For small arguments <x>, an expansion is used, with $b$ (instead
!! of 1) as the upper bound.

!!#### REQUIRED INPUT
INTEGER      ,INTENT(IN) :: MaxN
REAL(KIND_PM),INTENT(IN) :: x
REAL(KIND_PM),INTENT(IN) :: x0,c,b

!!#### REQUIRED OUTPUT
REAL(KIND_PM),INTENT(OUT) :: PM(0:MaxN)

!!#### OPTIONAL INPUT

REAL(KIND_PM),INTENT(IN),OPTIONAL :: tol

!!#### LOCAL VARIABLES
INTEGER       :: n,ierr
REAL(KIND_PM) :: tol_,inside,PMrecurse(0:MaxN),NInt,ZExp,ExpX,a,ans,qans

!!--begin--

tol_ = DEFAULT( PM_DEFAULT_TOL , tol )
PM = 0._KIND_PM

!standard recursive algorithm if x is not too small
IF( x>SMALL_x )THEN

 !normal forward recursion (for most cases)
 IF( x>REAL(MaxN,KIND(x)) .OR. MaxN<5 )THEN
  !write(*,*)"forward recursion, x=",x,"b=",b

  ExpX         = EXP(-x*b)
  inside       = 1._KIND_PM - ExpX
  PM(0)        = inside/x0
  PMrecurse(0) = inside/x

  DO n=1,MaxN
   inside       = n*PMrecurse(n-1) - (b**n)*ExpX
   PM(n)        = inside/x0
   PMrecurse(n) = inside/x
  END DO

 !backward recursion
 ELSE

  !write(*,*)"backward recursion"
  ExpX = EXP(-x*b)
  !remove need for numerical evaluation
  !NInt = NPolyExpoMoment0(MaxN,x,tol_)
  a=1.0_KIND_Rdp + REAL(MaxN,KIND_Rdp)
  CALL dgrat(a, x*b, ans, qans, ierr)
  NInt=EXP(GammaLn(a))*ans/(x**a)
  PM(MaxN)        = c*NInt
  PMrecurse(MaxN) =   NInt
  DO n=MaxN-1,0,-1
   PM(n)        = ( x0*PMrecurse(n+1) + (b**(n+1))*ExpX )/REAL(n+1,KIND(x))
   PMrecurse(n) = (  x*PMrecurse(n+1) + (b**(n+1))*ExpX )/REAL(n+1,KIND(x))
  END DO

 END IF


!x is small (use backward recursion)
ELSE
 !write(*,*)"small x"
 !get the values directly
 DO n=MaxN,0,-1
  PM(n) = c*ZeroExpansionWithB(n,x,tol_,b)
 END DO

END IF


!!--end--
END SUBROUTINE



!!### FUNCTION <<ZeroExpansionWithB>>
FUNCTION ZeroExpansionWithB(n,x,tol,b) RESULT(val)

!!#### PURPOSE
!! Return the result MacLaurin expansion of the simplified
!! exponential moment function, $PM_n(x)$ for small $x$.

!!#### REQUIRED INPUT
!! * moment order $n$
!! * argument of simplified exponential moment function $PM_n(x)$
!! * tolerance on solution
!! * the upper bound $b \in [0,1]$
INTEGER      ,INTENT(IN) :: n
REAL(KIND_PM),INTENT(IN) :: x,tol,b

!!#### LOCAL VARIABLES
INTEGER       :: k,i
REAL(KIND_PM) :: val,terms(0:100) ![hack] assume 100 terms needed at most (1-3 terms expected)
REAL(KIND_PM) :: invfact

!!--begin--

k = -1
DO
 k = k + 1

 IF( FORCE_FACTORIAL_EVAL .OR. k>MAX_N_Factorial )THEN
  invfact = EXP(-FactorialLn(k))
 ELSE
  invfact = REAL( c_InvFactorial(k) , KIND_Rdp)
 END IF

 terms(k) = invfact * (-x*b)**k / ( REAL(n+k+1,KIND_Rdp) )

 !relative tolerance
 IF( ABS(terms(k))<tol*terms(0) )EXIT

END DO

!accumulate terms in reverse order to not lose precision
!(terms are monotonically decreasing with k)
val = 0._KIND_PM
DO i=k,0,-1
 val = val + terms(i)
END DO

!!--end--
END FUNCTION
!!### SUBROUTINE <<PRINT_PolyExpoMoment>>
SUBROUTINE PRINT_PolyExpoMoment(N,Unit,xstart,xdiv,ndiv,tol,&
  PrintDerivative)

!!#### MODULES
USE FUN_STR                             !!((05-B-FUN_STR.f90))
USE FUN_Default                         !!((04-A-FUN_Default.f90))
USE VAR_Units,ONLY: DEFAULT_OUTPUT_UNIT !!((03-A-VAR_Units.f90))

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: N

!!#### OPTIONAL INPUT
INTEGER       ,OPTIONAL,INTENT(IN) :: Unit
REAL(KIND_PM) ,OPTIONAL,INTENT(IN) :: xstart,xdiv,tol
INTEGER       ,OPTIONAL,INTENT(IN) :: ndiv
LOGICAL       ,OPTIONAL,INTENT(IN) :: PrintDerivative

!!#### LOCAL VARIABLES
REAL(KIND_PM) :: x,PM(0:N),xstart_,xdiv_,dPM(0:N),PMlast(0:N),xlast
INTEGER        :: i,Unit_,ndiv_
LOGICAL        :: PrintDerivative_

!!--begin--
xstart_ = Default( 10._KIND_PM , xstart )
xdiv_   = Default( 10._KIND_PM , xdiv )
Unit_   = Default( DEFAULT_OUTPUT_UNIT , Unit )
ndiv_   = Default( 10 , ndiv )
PrintDerivative_ = Default( .FALSE. , PrintDerivative )

IF( .NOT.PrintDerivative_ )THEN
 x=xstart_
 DO i=1,ndiv_+1
  CALL PolyExpoMoment( N , x , x0=x , c=1._KIND_PM , PM=PM(0:N) ,tol=tol )
  WRITE(Unit_,"("//TRIM(STR(N+2))//"(E,1x))")x,PM(0:N)
  x=x/xdiv_
 END DO
ELSE
 x=xstart_
 xlast=xstart_*xdiv_
 CALL PolyExpoMoment( N , xlast , x0=xlast , c=1._KIND_PM , PM=PMlast(0:N) ,tol=tol )
 DO i=1,ndiv_+1
  CALL PolyExpoMoment( N , x , x0=x , c=1._KIND_PM , PM=PM(0:N) ,tol=tol )
  dPM = (PM-PMlast)/(x-xlast)
  WRITE(Unit_,"("//TRIM(STR(2*(N+1)+1))//"(E,1x))")x,PM(0:N),dPM(0:N)
  PMlast=PM
  xlast=x
  x=x/xdiv_
 END DO
END IF
!!--end--
END SUBROUTINE



!!### SUBROUTINE <<PRINT_PolyExpoMoment2>>
SUBROUTINE PRINT_PolyExpoMoment2(i,MaxN,bt,b1,Unit,xstart,xdiv,ndiv,tol)

!!#### MODULES
USE FUN_STR                             !!((05-B-FUN_STR.f90))
USE FUN_Default                         !!((04-A-FUN_Default.f90))
USE VAR_Units,ONLY: DEFAULT_OUTPUT_UNIT !!((03-A-VAR_Units.f90))

!!#### REQUIRED INPUT
INTEGER       ,INTENT(IN) :: i,MaxN
REAL(KIND_PM),INTENT(IN) :: bt,b1

!!#### OPTIONAL INPUT
INTEGER       ,OPTIONAL,INTENT(IN) :: Unit
REAL(KIND_PM),OPTIONAL,INTENT(IN) :: xstart,xdiv,tol
INTEGER       ,OPTIONAL,INTENT(IN) :: ndiv

!!#### LOCAL VARIABLES
REAL(KIND_PM) :: x,PM(0:MaxN),xstart_,xdiv_
INTEGER        :: k,Unit_,ndiv_

!!--begin--
xstart_ = Default( 10._KIND_PM , xstart )
xdiv_   = Default( 10._KIND_PM , xdiv )
Unit_   = Default( DEFAULT_OUTPUT_UNIT , Unit )
ndiv_   = Default( 10 , ndiv )

x=xstart_
DO k=1,ndiv_+1
 CALL PolyExpoMoment2(i,MaxN,x,bt,b1,x0=x,c=1._KIND_PM,PM=PM)
 WRITE(Unit_,"("//TRIM(STR(MaxN+2))//"(E,1x))")x,PM(0:MaxN)
 x=x/xdiv_
END DO

!!--end--
END SUBROUTINE



!!### FUNCTION <<CHECK1_PolyExpoMoment2>>
FUNCTION CHECK1_PolyExpoMoment2(smallside,xstart,xdiv,ndiv,tol) RESULT(Pass)
!!#### PURPOSE
!! Check the 2D simplified exponential moment for:
!!
!! * positivity for all $x$
!!   $$ PM(x) >0 \forall x $$
!!
!! * boundedness for all $x$
!!   $$ PM(x) \in [0,1] \forall x $$
!!
!! * monotonic decrease with respect to $x$
!!   $$ PM(x+dx) < PM(x) \forall x $$
!!
!! * monotonic decrease with respect to $n$ and $i$
!!   $$ NI_{ni} < NI_{i'n} \quad i'<i $$
!!   $$ NI_{ni} < NI_{in'} \quad n'<n $$


!!#### REQUIRED INPUT
REAL(KIND_PM),OPTIONAL,INTENT(IN) :: smallside

!!#### OPTIONAL INPUT
REAL(KIND_PM),OPTIONAL,INTENT(IN) :: xstart,xdiv
INTEGER       ,OPTIONAL,INTENT(IN) :: ndiv
REAL(KIND_PM),OPTIONAL,INTENT(IN) :: tol

!!#### REQUIRED OUTPUT
LOGICAL :: Pass

!!#### LOCAL VARIABLES
INTEGER,PARAMETER :: MaxN=4,MaxI=4
REAL(KIND_PM) :: x,xstart_,xdiv_
INTEGER        :: k,Unit_,ndiv_,i,n
REAL(KIND_PM) :: bt,b1,tol_
LOGICAL        :: PassMonX,PassPos,PassBound,PassMonI,PassMonN
REAL(KIND_PM),ALLOCATABLE :: Allx(:),AllPM(:,:,:)
!!--begin--

xstart_ = Default( 10._KIND_PM , xstart )
xdiv_   = Default( 10._KIND_PM , xdiv )
ndiv_   = Default( 10 , ndiv )
tol_    = Default( PM_DEFAULT_TOL , tol )

!allocate local arrays and setup the x values we will test
ALLOCATE( AllX(ndiv_+1) ,AllPM(0:MaxI,0:MaxN,ndiv_+1) )
x=xstart_
DO k=1,ndiv_+1
 AllX(k)=x
 x=x/xdiv_
END DO


!NOTE
!b1 and bt must be positive and less than 1
!difference between top and bottom sides is given as
!
!         ds(t) = bt*t + b1   ,   for t=0->1
!
! thus side 1 has length b1 and
!      side 2 has length bt+b1.

IF( .NOT.PRESENT(smallside) )THEN
 !******default*******
 !     _
 ! _ - |
 ! |   |
 ! |   |
 ! - _ |
 !     -
 b1=0.9_KIND_PM
 bt=0.1_KIND_PM
ELSE
 b1=MAX(MIN(smallside,1._KIND_PM),0._KIND_PM)
 bt=1.0_KIND_PM-b1
END IF


DO k=1,ndiv_+1

 DO i=0,MaxI
  CALL PolyExpoMoment2(i,MaxN,AllX(k),bt,b1,x0=AllX(k),c=1._KIND_PM,&
    PM=AllPM(i,0:MaxN,k) , tol=tol_ )
 END DO

END DO

!check positivity and boundedness
PassPos = ALL(AllPM>=0._KIND_PM)
PassBound = ALL(AllPM<=1._KIND_PM)

!check monotonic decreasing with respect to x, i, n
PassMonI = MonoCheck1( AllPM )
PassMonN = MonoCheck2( AllPM )
PassMonX = MonoCheck3( AllPM )

!final return
Pass = PassMonX .AND. PassPos .AND. PassBound

!!wrapup
DEALLOCATE( AllX,AllPM)

!!--end--
END FUNCTION



!!### SUBROUTINE <<PolyExpoMoment2>>
SUBROUTINE PolyExpoMoment2(i,MaxN,x,bt,b1,x0,c,PM,tol)

!!#### PURPOSE
!! Return $\tilde{PM}_{in}$ for a single $i$ and all $n\in[0,MaxN]$.
!
! PM(i,n) = Integrate[ t^i * "
!             Integrate[ s^n Exp[-x s] , {s,0,bt*t+b1} ],
!               {t,0,1}]

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: i,MaxN
REAL(KIND_PM)     :: x,bt,b1,x0,c

!!#### REQUIRED OUTPUT
REAL(KIND_PM),INTENT(OUT) :: PM(0:MaxN)

!!#### OPTIONAL INPUT
REAL(KIND_PM),INTENT(IN),OPTIONAL :: tol

!!#### LOCAL VARIABLES
REAL(KIND_PM) :: PM1(0:MaxN+i),Enxb1,PMrecurse(0:MaxN),interior
INTEGER       :: n,k,kk
REAL(KIND_PM) :: terms(0:100)
REAL(KIND_PM) :: tol_,invfact

!!--begin--

tol_ = DEFAULT( PM_DEFAULT_TOL , tol )

IF( x>SMALL_X )THEN

 !get all the simplified exponential moments we need
 CALL PolyExpoMoment(MaxN+i,x*bt,x0=x*bt,c=1._KIND_PM,PM=PM1,tol=tol_)

 !get exponential term
 Enxb1 = EXP(-x*b1)

 !for each term PM(i,0),PM(i,1),...,PM(i,MaxN)
 DO n=0,MaxN

  !check if we are at the last recursion and use x0 to divide by instead of x
  IF( n==MaxN )THEN

   !first term
   IF( n==0 )THEN
    PM(0) = (1._KIND_PM/x0)*( 1._KIND_PM/(REAL(i+1,KIND_PM)) - PM1(i)*Enxb1 )
   !other terms
   ELSE
    PM(n) = (1._KIND_PM/x0)*( n*PMrecurse(n-1) - Enxb1*EVAL_DSum(n) )
   END IF

  !all but the last recursion
  ELSE

   !first term
   IF( n==0 )THEN
    interior     = 1._KIND_PM/(REAL(i+1,KIND_PM)) - PM1(i)*Enxb1
    PM(0)        = (1._KIND_PM/x0)*( interior )
    PMrecurse(0) = (1._KIND_PM/x )*( interior )
   !other terms
   ELSE
    interior = n*PMrecurse(n-1) - Enxb1*EVAL_DSum(n)
    PM(n)        = (1._KIND_PM/x0)*( interior )
    PMrecurse(n) = (1._KIND_PM/x )*( interior )
   END IF

  END IF

 END DO

!x is small
ELSE

 !for each term PM(i,0),PM(i,1),...,PM(i,MaxN)
 DO n=0,MaxN

  !go forward to find the proper truncation term
  k = -1
  DO
   k = k + 1
   IF( FORCE_FACTORIAL_EVAL .OR. k>MAX_N_InvFactorial )THEN
    invfact = EXP(-FactorialLn(k))
   ELSE
    invfact = c_InvFactorial(k)
   END IF
   terms(k) = invfact * (-x)**k * EVAL_CSum(i,n,k)
   !WRITE(*,*)"terms(k))=",terms(k)
   IF( ABS(terms(k))<tol_*terms(0) )EXIT
  END DO

  !backward to accumulate terms (prevents loss of accuracy)
  PM(n) = 0._KIND_PM
  DO kk=k,0,-1
   PM(n) = PM(n) + terms(kk)
   !WRITE(*,*)"val=",val
  END DO

 END DO

 !multiply by constant
 PM=PM*c

END IF

!!--end--
CONTAINS


FUNCTION EVAL_DSum(n) RESULT(sum)
INTEGER,INTENT(IN) :: n
REAL(KIND_PM) :: sum

!!#### LOCAL VARIABLES
INTEGER       :: m
REAL(KIND_PM) :: DCoeff(0:MaxN),coeff

!!--begin--

DO m=0,n
 IF( FORCE_FACTORIAL_EVAL .OR. n>MAX_N_BinomialCoeff )THEN
  coeff = CoefficientBinomial(n,m)
 ELSE
  coeff = c_BinomialCoeff(m,n)
 END IF
 DCoeff(m) = coeff * (bt)**m * (b1)**(n-m)
END DO

sum = 0._KIND_PM
DO m=0,n
 sum = sum + DCoeff(m)*PM1(m+i)
END DO

!!--end--
END FUNCTION



FUNCTION EVAL_CSum(i,n,k) RESULT(sum)
INTEGER,INTENT(IN) :: i,n,k
REAL(KIND_PM) :: sum

!!#### LOCAL VARIABLES
REAL(KIND_PM) :: CCoeff(0:100),coeff
INTEGER :: m

!!--begin--

DO m=0,k+n+1
 IF( FORCE_FACTORIAL_EVAL .OR. n>MAX_N_BinomialCoeff )THEN
  coeff = CoefficientBinomial(k+n+1,m)
 ELSE
  coeff = c_BinomialCoeff(m,k+n+1)
 END IF
 CCoeff(m) = coeff * (bt)**m * (b1)**(k+n+1-m)
 CCoeff(m) = CCoeff(m) /REAL(k+n+1,KIND_PM)
END DO

sum = 0._KIND_PM
DO m=0,k+n+1
 sum = sum + CCoeff(m)/REAL(m+i+1,KIND_PM)
END DO

!!--end--
END FUNCTION


END SUBROUTINE





!!### SUBROUTINE <<NPolyExpoMoment>>
SUBROUTINE NPolyExpoMoment( N , x , x0 , c , PM , tol )

!!#### PURPOSE
!! Return the simplified exponential moments $PM_n(x)$, for $n=0,1,...,N$
!! using adaptive numerical integration.


!!#### REQUIRED INPUT
INTEGER       ,INTENT(IN) :: N
REAL(KIND_PM),INTENT(IN) :: x
REAL(KIND_PM),INTENT(IN) :: x0
REAL(KIND_PM),INTENT(IN) :: c

!!#### REQUIRED OUTPUT
REAL(KIND_PM),INTENT(OUT) :: PM(0:N)

!!#### OPTIONAL INPUT
REAL(KIND_PM),INTENT(IN),OPTIONAL :: tol

!!#### LOCAL VARIABLES
INTEGER        :: i
REAL(KIND_PM) :: tol_

!!--begin--
tol_ = Default( PM_DEFAULT_TOL , tol )
PM = 0._KIND_PM

DO i=0,N
 PM(i) = c*NPolyExpoMoment0(i,x,tol_)
END DO

!!--end--
END SUBROUTINE



!!### FUNCTION <<NPolyExpoMoment0>>
FUNCTION NPolyExpoMoment0(n,x,tol) RESULT(val)

!!#### PURPOSE
!! Return the numerical integration of the
!! simplified exponential moment function, $PM_n(x)$ for a single $n$.

!!#### REQUIRED INPUT
!! * moment order $n$
!! * argument of simplified exponential moment function $PM_n(x)$
!! * tolerance on solution
INTEGER       ,INTENT(IN) :: n
REAL(KIND_PM),INTENT(IN) :: x,tol

!!#### REQUIRED OUTPUT
REAL(KIND_PM) :: val,tol_

!!#### LOCAL VARIABLES
!REAL(KIND_PM) :: Mint___
!EXTERNAL      :: Mint___

!!--begin--

tol_ = Default( PM_DEFAULT_TOL , tol )
val = Integrate1_aq( Mint___ , (/0._KIND_PM,1._KIND_PM/) , N=10000 , tol=tol_ )

!!--end--
CONTAINS

!!### FUNCTION <<Mint___>
FUNCTION Mint___(t)

!!#### REQUIRED INPUT
REAL(KIND_PM),INTENT(IN) :: t

!!#### REQUIRED OUTPUT
REAL(KIND_PM) :: Mint___

!!--begin--

Mint___ = (t)**n * EXP(-x*t)

!!--end--
END FUNCTION

END FUNCTION



!!### SUBROUTINE <<PRINT_NPolyExpoMoment>>
SUBROUTINE PRINT_NPolyExpoMoment(N,Unit,xstart,xdiv,ndiv,tol)

!!#### MODULES
USE FUN_STR                             !!((05-B-FUN_STR.f90))
USE FUN_Default                         !!((04-A-FUN_Default.f90))
USE VAR_Units,ONLY: DEFAULT_OUTPUT_UNIT !!((03-A-VAR_Units.f90))

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: N

!!#### OPTIONAL INPUT
INTEGER       ,OPTIONAL,INTENT(IN) :: Unit
REAL(KIND_PM),OPTIONAL,INTENT(IN) :: xstart,xdiv,tol
INTEGER       ,OPTIONAL,INTENT(IN) :: ndiv

!!#### LOCAL VARIABLES
REAL(KIND_PM) :: x,PM(0:N),xstart_,xdiv_
INTEGER        :: i,Unit_,ndiv_

!!--begin--
xstart_ = Default( 10._KIND_PM , xstart )
xdiv_   = Default( 10._KIND_PM , xdiv )
Unit_   = Default( DEFAULT_OUTPUT_UNIT , Unit )
ndiv_   = Default( 10 , ndiv )

x=xstart_
DO i=1,ndiv_+1
 CALL NPolyExpoMoment( N , x , x0=x , c=1._KIND_PM , PM=PM(0:2) ,tol=tol )
 WRITE(Unit_,"("//TRIM(STR(N+2))//"(E,1x))")x,PM(0:N)
 x=x/xdiv_
END DO

!!--end--
END SUBROUTINE


FUNCTION MonoCheck3( AllPM ) RESULT(Pass)
REAL(KIND_PM),INTENT(IN) :: AllPM(:,:,:)
INTEGER :: k,i,n
LOGICAL :: Pass

!!--begin--

Loop: DO k=1,SIZE(AllPM,3)-1
 DO n=1,SIZE(AllPM,2)
  DO i=1,SIZE(AllPM,1)

   !if it is zero, then the next must be zero
   IF( AllPM(i,n,k+1)==0._KIND_PM )THEN
    Pass = AllPM(i,n,k)==0._KIND_PM
   !otherwise, the term with greater x value (k) must be less than the next term
   ELSE
    Pass = AllPM(i,n,k)<AllPM(i,n,k+1) !difference here from other monochecks
   END IF

   !failure
   IF( .NOT.Pass )THEN
    EXIT Loop
   END IF

  END DO
 END DO

END DO Loop

!!--end--
END FUNCTION

FUNCTION MonoCheck1( AllPM ) RESULT(Pass)
REAL(KIND_PM),INTENT(IN) :: AllPM(:,:,:)
INTEGER :: k,i,n
LOGICAL :: Pass

!!--begin--

Loop: DO k=1,SIZE(AllPM,3)
 DO n=1,SIZE(AllPM,2)
  DO i=1,SIZE(AllPM,1)-1

   !if it is zero, then the next must be zero
   IF( AllPM(i+1,n,k)==0._KIND_PM )THEN
    Pass = AllPM(i,n,k)==0._KIND_PM
   ELSE
    Pass = AllPM(i,n,k)>AllPM(i+1,n,k) !difference here from other monochecks
   END IF

   !failure
   IF( .NOT.Pass )THEN
    EXIT Loop
   END IF

  END DO
 END DO

END DO Loop

!!--end--
END FUNCTION

FUNCTION MonoCheck2( AllPM ) RESULT(Pass)
REAL(KIND_PM),INTENT(IN) :: AllPM(:,:,:)
INTEGER :: k,i,n
LOGICAL :: Pass

!!--begin--

Loop: DO k=1,SIZE(AllPM,3)
 DO n=1,SIZE(AllPM,2)-1
  DO i=1,SIZE(AllPM,1)

   !if it is zero, then the next must be zero
   IF( AllPM(i,n+1,k)==0._KIND_PM )THEN
    Pass = AllPM(i,n,k)==0._KIND_PM
   ELSE
    Pass = AllPM(i,n,k)>AllPM(i,n+1,k) !difference here from other monochecks
   END IF

   !failure
   IF( .NOT.Pass )THEN
    EXIT Loop
   END IF

  END DO
 END DO

END DO Loop

!!--end--
END FUNCTION



!!### FUNCTION <<CHECK1_PolyExpoMoment>>
FUNCTION CHECK1_PolyExpoMoment(tol,testtol,&
  NoisyUnit,NCount) RESULT(Pass)
!!#### PURPOSE
!! Check the polynomial exponents for simple boundedness.

!!#### REQUIRED OUTPUT
LOGICAL :: Pass

!!#### OPTIONAL INPUT
REAL(KIND_PM),OPTIONAL,INTENT(IN) :: tol,testtol
INTEGER      ,OPTIONAL,INTENT(IN) :: NoisyUnit,NCount

!!#### LOCAL VARIABLES
REAL(KIND_PM) :: tol_,testtol_,x,PM1(0:10)
INTEGER       :: NoisyUnit_,NCount_,count
INTEGER       :: i

!!--begin--

testtol_   = Default( PM_DEFAULT_TESTTOL , testtol )
tol_       = Default( PM_DEFAULT_TOL     , tol     )
NCount_    = Default( 1000               , NCount  )
NoisyUnit_ = Default( 0                  , NoisyUnit )

Pass = .TRUE.

!code to test random moments and random optical thicknesses
!!--------------------------
count = 0
DO
 i=Random((/0,10/))
 x=Random((/-1.E-20_KIND_PM,1.E+20_KIND_PM/))
 CALL PolyExpoMoment(i,x,x0=x,c=1._KIND_PM,PM=PM1(0:i),tol=tol_)
 IF( ANY(PM1(0:i)<0._KIND_PM) .OR. ANY(PM1(0:i)>1._KIND_PM) )THEN
  IF( NoisyUnit_/=0 )THEN
   WRITE(NoisyUnit_,*)"failed for i=",i
   WRITE(NoisyUnit_,*)"           x=",x
   WRITE(NoisyUnit_,*)"with values PM(0:i)=",PM1(0:i)
   WRITE(NoisyUnit_,*)"****FATAL STOP****"
  END IF
  Pass = .FALSE.
  EXIT
 END IF

 count = count+1
 IF( MOD(count,1000)==0 )THEN
  IF( NoisyUnit_/=0 )THEN
   WRITE(NoisyUnit_,*)"random moments passed ",count,"/ 100,000"
  END IF
 END IF
 IF( count==NCount_ )EXIT

END DO
!!--------------------------

END FUNCTION

END MODULE
