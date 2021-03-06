!initialize
Moment1 = REAL(0,KIND(AngularFlux))

DO mp=1,SIZE(m_,2)
 dot_prod = xyDOT_VV( REAL(DirCos(:,mp),KIND_MSH) , Normal )
 
 IF( (dot_prod>=0._KIND_MSH) )THEN
  
  DO ma=1,SIZE(m_,1)
   m = m_(ma,mp)
   DO g=1,SIZE(AngularFlux,1)
    IF( .NOT.PRESENT(AziSin) )THEN
	 Moment1(g) = Moment1(g) + &
 	  PolWts(mp)*AziWts(ma)*dot_prod*AngularFlux(g,m)
    ELSE
	 Moment1(g) = Moment1(g) + &
 	  PolWts(mp)*AziWts(ma)*dot_prod*AngularFlux(g,m)*AziSin(ma)
	END IF
   END DO
   
  END DO

 END IF

END DO