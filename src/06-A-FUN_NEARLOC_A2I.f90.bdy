!! Get $R^{N}$ equivalent of $I^{N}$ VECS.
real_VECS =  REAL(VECS,KIND_R)

!! Get $R^{N}$ equivalent of $I^{N}$ VEC.
real_VEC = REAL(VEC,KIND_R)

!! Call the real routine.
loc = NEARLOC(real_VECS,real_VEC,NormSpec)