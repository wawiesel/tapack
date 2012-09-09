DO k=1,Nk
 CALL Randomize( u , (/-1._KIND_R,+1._KIND_R/) )
 u = u/NormEll2(u)
 n_IN  = Moment1_IN (f,x,w,u)
 n_OUT = Moment1_OUT(f,x,w,u)
 n     = Moment1    (f,x,w)
 n     = n_IN + n_OUT
 resid_= resid_ + NormEll1( n_IN+n_OUT-n )
END DO