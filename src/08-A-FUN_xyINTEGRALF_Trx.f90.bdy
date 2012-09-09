
!WRITE(*,*)"====entering xyINTEGRALF_Trx==="

epsrel = DEFAULT(0._KIND_R,reltol)
epsabs = DEFAULT(SQRT(EPSILON(1._KIND_R)),abstol)
minpts = DEFAULT(N,nmin)
!37 points are required for each triangle
!so we allow each to be refined 5 times
maxpts = DEFAULT(37*N*5,nmax)

lenver = 3*((MAXPTS-37*N)/(4*37)) + N
nw     = lenver*3 + MAX(32,8*N) + 1
ALLOCATE( ver(2,3,lenver) , work(nw) , iwork(lenver+1) )
ver(:,:,1:N) = Trx
restar = 0 !for first attempt (restar=1 and can "restart")

!WRITE(*,*)"ver=",ver
!WRITE(*,*)"N=",N
!WRITE(*,*)"minpts=",minpts
!WRITE(*,*)"maxpts=",maxpts
!WRITE(*,*)"lenver=",lenver
!WRITE(*,*)"nw=",nw

call dcutri (funsub, 1, ver, N, minpts, maxpts, &
     & epsabs, epsrel, lenver, nw, restar, result, abserr, neval, &
     & ifail, work, iwork)

!WRITE(*,*)"ifail=",ifail

INTEGRALF = result(1)

IF( PRESENT(err) )THEN
 err = abserr(1)
END IF
IF( PRESENT(ierr) )THEN
 ierr = ifail
END IF
IF( PRESENT(num_eval) )THEN
 num_eval = neval
END IF

DEALLOCATE( work , iwork )

!WRITE(*,*)"====leaving xyINTEGRALF_Trx==="

CONTAINS

SUBROUTINE funsub(z,nfun,fval) 
REAL(KIND_Rdp) :: z(2)
INTEGER        :: nfun
REAL(KIND_Rdp) :: fval(1:nfun)
!!--begin--
!WRITE(*,*)" in funsub "
!WRITE(*,*)" nfun = ",nfun
!WRITE(*,*)"    z(1) = ",z(1)
!WRITE(*,*)"    z(2) = ",z(2)
fval(1) = f(REAL(z(1),KIND_R),REAL(z(2),KIND_R))
!WRITE(*,*)" fval(1) = ",fval(1)

!!--end--
END SUBROUTINE