N = SIZE(VECS,1)

!! Get $R^{2N}$ equivalent of $Z^N$ VECS.
real_VECS(  1:  N,:) =  REAL(VECS)
real_VECS(N+1:2*N,:) = AIMAG(VECS)

!! Get $R^{2N}$ equivalent of $Z^N$ VEC.
real_VEC(  1:  N) = REAL(VEC)
real_VEC(N+1:2*N) = AIMAG(VEC)

!! Call the real routine.
loc = NEARLOC(real_VECS,real_VEC,NormSpec)