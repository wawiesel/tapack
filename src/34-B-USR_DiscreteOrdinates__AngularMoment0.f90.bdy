!initialize
Moment0 = REAL(0,KIND(AngularFlux))

DO mp=1,SIZE(m_,2)
 DO ma=1,SIZE(m_,1)
  m = m_(ma,mp)
  Moment0 = Moment0 + PolWts(mp)*AziWts(ma)*AngularFlux(:,:,m)
 END DO
END DO
