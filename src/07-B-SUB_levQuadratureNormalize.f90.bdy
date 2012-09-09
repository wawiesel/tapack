sum_me_up = 0._KIND_R
DO mp=1,SIZE(wp,1)
 DO ma=1,SIZE(wa,1)
  IF( wa(ma,mp)==0._KIND_R )EXIT
   sum_me_up = sum_me_up + wa(ma,mp)*wp(mp)
 END DO 
END DO

!normalize accordingly
IF( .NOT.PRESENT(Norm) )THEN
 wa = wa*c_4_times_Pi/sum_me_up
ELSE
 wa = wa*Norm/sum_me_up
END IF