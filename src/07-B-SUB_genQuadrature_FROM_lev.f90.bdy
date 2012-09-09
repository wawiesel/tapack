Nm = 0
Nmp = SIZE(xp)
Nma = 0
DO mp=1,Nmp
 Nm_plus = SIZEa( xa(1,:,mp) )
 Nma = MAX(Nma,Nm_plus)
 Nm = Nm + Nm_plus
END DO

ALLOCATE( x(3,Nm),w(Nm),m_map(Nma,Nmp) )
m_map = 0
m=0
DO mp=1,Nmp
 Nma = SIZEa( xa(1,:,mp) )
 DO ma=1,Nma
  m = m+1
  x(1,m) = xa(1,ma,mp)
  x(2,m) = xa(2,ma,mp)
  x(3,m) = xp(mp)
  w(m)   = wp(mp)*wa(ma,mp)
  m_map(ma,mp) = m
 END DO
END DO
