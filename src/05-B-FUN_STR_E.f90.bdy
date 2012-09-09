!handle options
AdjustLeft_  = DEFAULT( .TRUE.  , AdjustLeft )
AdjustRight_ = DEFAULT( .FALSE. , AdjustRight )

!write X into S with optional format
IF( PRESENT(FMT) )THEN
 WRITE(S,FMT,IOSTAT=jerr)X
ELSE
 WRITE(S,*,IOSTAT=jerr)X
ENDIF

!check for an error
IF( jerr/=0 )THEN
 CALL CLEAR(S)
 RETURN
END IF

!adjust
IF( AdjustLeft_ )THEN
 S = ADJUSTL(S) 
ELSE IF( AdjustRight_ )THEN
 S = ADJUSTR(S)
END IF
