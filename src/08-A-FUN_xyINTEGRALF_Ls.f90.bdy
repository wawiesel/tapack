
epsrel = DEFAULT(0._KIND_R,reltol)
epsabs = DEFAULT(SQRT(EPSILON(1._KIND_R)),abstol)
minpts = DEFAULT(1,nmin)
!we allow a maximum of 100 divisions by default
maxpts = DEFAULT(100,nmax)
LENGTH = xyDIST_PP( Ls(:,1) , Ls(:,2) )
ULINE  = xyDIRECTION_P2( Ls )

!the Integrate_aq routine only allows absolute tolerance
!so we calculate it ourselves based on length of Line Segment
p   = (/0._KIND_R,LENGTH/)
tol = MIN( LENGTH*epsrel , epsabs )
N   = maxpts


INTEGRALF = Integrate1_aq(flocal,p,tol,N)


!don't have any of these outputs set right now
IF( PRESENT(err) )THEN
 err = ERROR(err)
END IF
IF( PRESENT(num_eval) )THEN
 num_eval = ERROR(num_eval) 
END IF
IF( PRESENT(ierr) )THEN
 ierr = ERROR(ierr)
END IF


CONTAINS

FUNCTION flocal(s) 
REAL(KIND_R),INTENT(IN) :: s
REAL(KIND_R) :: flocal
REAL(KIND_R) :: x,y

!!--begin--

x = Ls(1,1) + ULINE(1)*s
y = Ls(2,1) + ULINE(2)*s
flocal = f(x,y)

!!--end--
END FUNCTION