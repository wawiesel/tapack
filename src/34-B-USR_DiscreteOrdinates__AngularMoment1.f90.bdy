!initialize
Moment1 = REAL(0,KIND(AngularFlux))

DO mp=1,SIZE(m_,2)
 DO ma=1,SIZE(m_,1)
  m = m_(ma,mp)
  DO k=1,SIZE(AngularFlux,2)
   DO g=1,SIZE(AngularFlux,1)
    Moment1(:,g,k) = Moment1(:,g,k) + &
      PolWts(mp)*AziWts(ma)*DirCos(:,mp)*AngularFlux(g,k,m)
   END DO
  END DO
 END DO
END DO