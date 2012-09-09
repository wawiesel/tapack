S_ = ADJUSTL(S)
READ(S_,*,IOSTAT=jerr)X
IF( jerr/=0 )THEN
 X = -HUGE(X)
ENDIF
