nrow   = MIN( A % N , B % N )
ncol   = MIN( MAXVAL( A % JA ) , MAXVAL( B % JA ) )
nzmax  = nrow * ncol

CALL allocate(C, nrow ,nzmax)
C % ia(1) = 1 
C % N = nrow

ALLOCATE( w(ncol) , t(ncol) , iw(ncol) ) 
FORALL(j=1:ncol) iw(j)=j

kc = 1

DO i=1,nrow
 t = ZERO
 w = .FALSE.
 DO ka = A%ia(i),A%ia(i+1)-1
  ja = A%ja(ka)
  IF( ja>ncol )EXIT
  t(ja) = t(ja) + A%a(ka)
  w(ja) = .TRUE.
 END DO
 DO kb = B%ia(i),B%ia(i+1)-1
  jb = B%ja(kb)
  IF( jb>ncol )EXIT
  t(jb) = t(jb) + B%a(kb)
  w(jb) = .TRUE.
 END DO
 
 nzrow = COUNT(w)
 c % a(kc:kc+nzrow-1) = PACK( t,w)
 c %ja(kc:kc+nzrow-1) = PACK(iw,w)
 kc = kc+nzrow
 c %ia(i+1) = kc

END DO

DEALLOCATE( w,t,iw )