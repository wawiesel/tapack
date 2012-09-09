!exit if input x is retarded
IF( .NOT.ALL(x>c_0) )THEN
 LOG_gamma = ERROR(LOG_GAMMA)
ENDIF

x_=x
tmp=x_+5.5_dp
tmp=(x_+0.5_dp)*log(tmp)-tmp
ser=1.000000000190015_dp
y=x_
do i=1,size(coef)
 y=y+1.0_dp
 ser=ser+coef(i)/y
end do
LOG_gamma = tmp+log(stp*ser/x_)