!exit if input x is retarded
IF( .NOT.x>c_0 )THEN
 LOG_gamma = ERROR(LOG_GAMMA)
ENDIF


x_=x
tmp=x_+5.5_dp
tmp=(x_+0.5_dp)*log(tmp)-tmp
LOG_gamma = tmp + log( &
  stp*( 1.000000000190015_dp + &
        sum( coef(:)/Sequence(x_+1.0_dp,1.0_dp,size(coef)) ) &
      )/x_)
