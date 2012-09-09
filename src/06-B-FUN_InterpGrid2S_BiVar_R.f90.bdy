
md_ = DEFAULT(1,md)

call idsfft ( md_, &
   ndp = SIZE(F), &
   xd  = REAL(r(1,:),KIND_Rdp), &
   yd  = REAL(r(2,:),KIND_Rdp), &
   zd  = REAL(F,KIND_Rdp), &
   nxi = SIZE(xout), & 
   nyi = SIZE(yout), &
   nzi = SIZE(xout), &
   xi  = REAL(xout,KIND_Rdp),&
   yi  = REAL(yout,KIND_Rdp),&
   zi  = Fout_rdp          , errint=errint , errmsg=errmsg )

Fout = Fout_rdp