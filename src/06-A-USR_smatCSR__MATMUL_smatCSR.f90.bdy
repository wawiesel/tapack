nrow  = A % N
ncol  = MAXVAL( B % JA )
nzmax = nrow * ncol

ALLOCATE( iw(ncol) ) ; iw = 0

CALL ALLOCATE(C, nrow ,nzmax)

nzactual = 0

len = 0

C%ia(1) = 1 

C % N = nrow

Outer_Loop: DO ii=1, nrow 
 
 DO 200 ka=A%ia(ii), A%ia(ii+1)-1 
  scal = A%a(ka)
  jj   = A%ja(ka)
  DO 100 kb=B%ia(jj),B%ia(jj+1)-1
   jcol = B%ja(kb)
   jpos = iw(jcol)
   IF( jpos==0 )THEN
    len = len+1
    IF( len>nzmax )THEN
     ierr = ii
     EXIT Outer_Loop
    ENDIF
    C%ja(len) = jcol
    iw(jcol)  = len
    C%a(len)  = scal*B%a(kb)
    nzactual  = nzactual + 1
   ELSE
    C%a(jpos) = C%a(jpos) + scal*B%a(kb)
   ENDIF
  100 END DO
 200 END DO

 FORALL(k=C%ia(ii):len) iw(C%ja(k)) = 0
 C%ia(ii+1) = len+1

END DO Outer_Loop

DEALLOCATE( iw )