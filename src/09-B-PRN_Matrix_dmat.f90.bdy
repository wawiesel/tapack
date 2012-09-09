! Set whether to include indices based on presence.
IF( PRESENT(IncludeIndices) )THEN
 IncludeIndices_ = IncludeIndices
ELSE
 IncludeIndices_ = DEFAULT_IncludeIndices
ENDIF

! Set local unit (unit_) according to presence.
IF( PRESENT(unit) )THEN
 unit_ = unit
ELSE
 unit_ = DEFAULT_unit
ENDIF

! Form the index headers.
IF( IncludeIndices_ )THEN

 CALL CLEAR(xHDR)
 DO i=1,SIZE(A,2)
  xHDR(i) = i
 END DO
 
 CALL CLEAR(yHDR)
 DO j=1,SIZE(A,1)
  yHDR(j) = j
 END DO
 
 CALL PRINT_Array2( TRANSPOSE(A) , UNIT=UNIT_ , FMT=FMT , xHDR=xHDR , yHDR=yHDR )

ELSE
 
 CALL PRINT_Array2( TRANSPOSE(A) , UNIT=UNIT_ , FMT=FMT )

ENDIF
