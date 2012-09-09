errint_ = 0
IF( PRESENT(domain) )THEN
 domain_ = domain
ELSE
 domain_ = (/-1._KIND_Rdp,+1._KIND_Rdp/)
END IF

n = SIZE( abcissas )
m =(n+1)/2
xm=0.5_KIND_Rdp*(domain_(2)+domain_(1))
xl=0.5_KIND_Rdp*(domain_(2)-domain_(1))
z=cos( c_PI*( Sequence(1,1,m) - 0.25_KIND_Rdp )/( n+0.5_KIND_Rdp ) )
unfinished=.true.

do its=1,MAXIT
 WHERE( unfinished )
  p1=1.0_KIND_Rdp
  p2=0.0_KIND_Rdp
 END WHERE
 DO j=1,n
  WHERE( unfinished )
   p3=p2
   p2=p1
   p1=((2.0_KIND_Rdp*j-1.0_KIND_Rdp)*z*p2-(j-1.0_KIND_Rdp)*p3)/j
  END WHERE
 END DO
 WHERE( unfinished )
  pp=n*(z*p1-p2)/(z*z-1.0_KIND_Rdp)
  z1=z
  z=z1-p1/pp
  unfinished=(abs(z-z1) > EPS)
 END WHERE
 IF( .NOT.ANY(unfinished) )EXIT
END DO

IF( its==MAXIT+1 )THEN
 abcissas = -HUGE(abcissas)
 weights  = -HUGE(weights)
 errint_  = -1
 GOTO 666
END IF

abcissas(1:m)        = xm-xl*z
abcissas(n:n-m+1:-1) = xm+xl*z
weights(1:m)         = 2.0_KIND_Rdp*xl/((1.0_KIND_Rdp-z**2)*pp**2)
weights(n:n-m+1:-1)  = weights(1:m)


666 CONTINUE

IF( PRESENT(errint) )THEN
 errint = errint_
END IF

IF( PRESENT(errmsg) )THEN
 
 SELECT CASE( errint_ )

   CASE(  0 ) ; CALL CLEAR( errmsg )
   CASE( -1 ) ; errmsg = "The maximum number of iterations has been reached!"
 
 END SELECT

END IF