!initialize
Eddington = REAL(0,KIND(AngularFlux))

!calculate numerator
DO mp=1,SIZE(m_,2)
 DO ma=1,SIZE(m_,1)
  m = m_(ma,mp)
  DO g=1,SIZE(AngularFlux,1)
   Eddington(g) = Eddington(g) + &
	 PolWts(mp)*AziWts(ma)*DirCos1(mp)*DirCos2(mp)*AngularFlux(g,m)
  END DO
 END DO
END DO
