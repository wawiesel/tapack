Nm = Order
ALLOCATE( w(1:Nm) , x(1:3,1:Nm) )

!get general quadrature
SELECT CASE( Quadrature )
 !CASE( "AbuShamays"  ) ; CALL genQuadrature_AbuShamays( Nm , x,w , errint_ , errmsg )
 CASE DEFAULT   ; errint_ = -111
END SELECT
IF( errint_<0 )GOTO 666


!make 2D simplification
IF( PRESENT(UniformZGeometry) )THEN
 IF( UniformZGeometry )THEN
  IF( MOD(Nm,2)/=0 )THEN
   x = ERROR(x)
   w = ERROR(w)
   errint_ = -444
   GOTO 666
  ELSE
   Nm = Nm/2
   w = w*c_2
   DO m=1,Nm
    IF( x(3,m)<0._KIND_R )THEN
	 x(:,m) = HUGE( 0._KIND_R )
	   w(m) = HUGE( 0._KIND_R )
	END IF
   END DO
   CALL Sort2(x)
   CALL Sort(w)
   x_ => x
   w_ => w
   NULLIFY( x,w )
   ALLOCATE( x(3,Nm) , w(Nm) )
   x = x_(1:3,1:Nm)
   w = w_(1:Nm)
   DEALLOCATE( x_ , w_ )
  END IF
 END IF
END IF


666 CONTINUE !errors come to here

IF( PRESENT(errint) )THEN
 errint = errint_
END IF

IF( PRESENT(errmsg) )THEN

 IF( errint_/=0 )THEN
  SELECT CASE( errint_ )
   CASE( -111 ) ; errmsg = "ERROR: The specified quadrature doesn't exist"
   CASE( -444 ) ; errmsg = "ERROR: The number of ordinates must be divisible by 2"
  END SELECT
 ELSE
  CALL CLEAR(errmsg)
 END IF

END IF
