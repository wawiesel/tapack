Nmp = pOrder
Nma = aOrder
ALLOCATE( wp(1:Nmp) , xa(1:2,1:Nma,1:Nmp) , wa(1:Nma,1:Nmp) , xp(1:Nmp) )

!get quadrature
SELECT CASE( Quadrature )
 CASE( "LMLQ"  )  ; CALL levQuadrature_LMLQ( aOrder , xa,xp,wa,wp  )
 CASE DEFAULT  ; errint_ = -222
END SELECT
IF( ANY(IsError(wa)) .OR. ANY(IsError(wp)) )THEN
 errint_=-4
ELSE
 errint_=0
END IF

IF( errint_<0 )GOTO 666

!make 2D simplification
IF( PRESENT(UniformZGeometry) )THEN
 IF( UniformZGeometry )THEN
  IF( MOD(Nmp,2)/=0 )THEN
   xp = ERROR(xp)
   wp = ERROR(wp)
   errint_ = -444
   GOTO 666
  ELSE
   Nmp = Nmp/2
   wp = wp*c_2
   ALLOCATE( xp_(Nmp) , wp_(Nmp) )
   xp_ = xp(1:Nmp)
   wp_ = wp(1:Nmp)
   DEALLOCATE( xp , wp )
   xp => xp_
   wp => wp_
   NULLIFY( xp_ , wp_ )
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
   CASE( -222 ) ; errmsg = "ERROR: The specified quadrature doesn't exist"
   CASE( -444 ) ; errmsg = "ERROR: The number of ordinates must be divisible by 2"
  END SELECT
 ELSE
  CALL CLEAR(errmsg)
 END IF

END IF
