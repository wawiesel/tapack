errint_ = 0
CALL CLEAR(errmsg_)

!get number of ordinates
Nma = SIZE(wa)
Nmp = SIZE(wp)
Nm = Nma*Nmp

!allocate stuff
ALLOCATE( x(SIZE(xa,1)+1,1:Nm) , w(1:Nm) )
ALLOCATE( m_map(1:Nma,1:Nmp) )


!assemble the full quadrature set
m = 0
DO mp=1,Nmp
 DO ma=1,Nma
  
  m = m + 1
  
  m_map(ma,mp) = m
  
  w(m) = wa(ma)*wp(mp)
  
  x(1:SIZE(xa,1),m) = xa(:,ma)*SQRT(1._KIND_R-xp(mp)**2)
  x(1+SIZE(xa,1),m) = xp(mp)

 END DO
END DO

666 CONTINUE

IF( PRESENT(errint) )THEN
 errint = errint_
END IF

IF( PRESENT(errmsg) )THEN
 errmsg = errmsg_
END IF


