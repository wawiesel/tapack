N = SIZE(x)
s = REAL(0,KIND(s))
u = REAL(0,KIND(s))
s(N)=-x(1)
DO 13 i=2,N
 DO 12 j=N+1-i,N-1
  s(j)=s(j)-x(i)*s(j+1)
 12 END DO
 s(N)=s(N)-x(i)
13 END DO

DO 16 j=1,N
 phi=REAL(N,KIND(phi))
 DO 14 k=N-1,1,-1
  phi=k*s(k+1)+x(j)*phi
 14 END DO
 ff=f(j)/phi
 b=REAL(1,KIND(b))
 DO 15 k=N,1,-1
  u(k)=u(k)+b*ff
  b=s(k)+x(j)*b
 15 END DO
16 END DO