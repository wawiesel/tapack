DO k=1,Nk
 CALL Randomize( u , (/-1._KIND_R,+1._KIND_R/) )
 u = u/NormEll2(u)
 m_IN  = Moment0_IN (f,x,w,u)
 m_OUT = Moment0_OUT(f,x,w,u)
 m     = Moment0    (f,x,w)
 m     = m_IN + m_OUT
 resid_= resid_ + ABS( m_IN+m_OUT-m )
END DO