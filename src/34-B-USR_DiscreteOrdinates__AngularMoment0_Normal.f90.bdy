!initialize
Moment0 = REAL(0,KIND(AngularFlux))

DO mp=1,SIZE(m_,2)
 dot_prod = xyDOT_VV( REAL(DirCos(:,mp),KIND_MSH) , Normal )
 IF( (dot_prod>=0._KIND_MSH) )CYCLE 
 DO ma=1,SIZE(m_,1)
  m = m_(ma,mp)
  Moment0 = Moment0 + PolWts(mp)*AziWts(ma)*AngularFlux(:,m)
 END DO
END DO