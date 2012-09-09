!! Error checking.
errint_ = 0

IF( MOD(pOrder,2)/=0 )THEN
 errint_ = -1
 GOTO 666
END IF

!! Get abcissas and weights for polar cosine $cos(\theta)$, where 
!! $\theta \in [0,\Pi]$.
CALL Quadrature_GL( xp , wp , domain=(/-1._KIND_R,1._KIND_R/) )
xp = Reverse( xp )
wp = Reverse( wp )

!! Jump to here on error.
666 CONTINUE

!! Set error integer.
IF( PRESENT(errint) )THEN
 errint = errint_
END IF

!! Output error message.
IF( PRESENT(errmsg) )THEN
  
 SELECT CASE( errint )
  CASE(  0 ) ; CALL CLEAR( errmsg )
  CASE( -1 ) ; errmsg = "ERROR: The order of the azimuthal quadrature <aOrder> &
                        &must be divisible by 2 for symmetry!"
 END SELECT

END IF