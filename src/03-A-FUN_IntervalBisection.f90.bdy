
!determine the span of the interval (number of points)
IF( PRESENT(MAX_span) )THEN
 span     = MIN( N , MAX_span ) 
 span     = MAX( span , 2 ) - 2
 halfspan = span/2 
 LB       = MAX(LB - span + halfspan , 1 )
 UB       = MIN(UB        + halfspan , N )
ENDIF

l = (/LB,UB/)