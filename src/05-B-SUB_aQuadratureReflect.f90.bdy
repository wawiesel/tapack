!! Error checking.
errint_ = 0

IF( MOD(aOrder,4)/=0 )THEN
 errint_ = -1
 GOTO 666
END IF

!! Calculate the weights and abcissas for quadrant I.
Q1 = aOrder/4
Q2 = aOrder/2
Q4 = aOrder

!! Get the weights around the unit circle.
wa( Q1+1 : Q2 ) = Reverse( wa( 1 : Q1 ) )
wa( Q2+1 : Q4 ) = Reverse( wa( 1 : Q2 ) )

!! Get the abcissa directions around the unit circle.

!! reflection to quadrant II
xa(1, Q1+1 : Q2 ) = -Reverse( xa(1,   1 : Q1 ) )
xa(2, Q1+1 : Q2 ) =  Reverse( xa(2,   1 : Q1 ) )

!reflection to quadrants III and IV
xa(1, Q2+1 : Q4 ) =  Reverse( xa(1, 1 : Q2 ) )
xa(2, Q2+1 : Q4 ) = -Reverse( xa(2, 1 : Q2 ) )


!! Jump to here on error.
666 CONTINUE

!! Set error integer.
IF( PRESENT(errint) )THEN
 errint = errint_
END IF

!! Output error message.
IF( PRESENT(errmsg) )THEN
  
 SELECT CASE( errint_ )
  CASE(  0 ) ; CALL CLEAR( errmsg )
  CASE( -1 ) ; errmsg = "ERROR: The order of the azimuthal quadrature <aOrder> &
                         &must be divisible by 4 for symmetry!"
 END SELECT

END IF