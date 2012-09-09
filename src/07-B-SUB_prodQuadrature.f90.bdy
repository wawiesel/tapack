Nmp = pOrder
Nma = aOrder
ALLOCATE( wp(1:Nmp) , xa(1:2,1:Nma) , wa(1:Nma) , xp(1:Nmp) )

!get polar quadrature (0,Pi)
SELECT CASE( TRIM(pQuadrature) )
 CASE( "GL"  )  ; CALL pQuadrature_GL ( Nmp , xp,wp , errint_ , errmsg )
 CASE( "dGL" )  ; CALL pQuadrature_dGL( Nmp , xp,wp , errint_ , errmsg )
 CASE( "AS"  )  ; CALL pQuadrature_AS ( Nmp , xp,wp , errint_ , errmsg )
 CASE DEFAULT   ; errint_ = -222
END SELECT
IF( errint_<0 )GOTO 666


!get azimuthal quadrature (0,2*Pi)
SELECT CASE( TRIM(aQuadrature) )
 CASE( "GL" )  ; CALL aQuadrature_GL( Nma , xa,wa , errint_ , errmsg )
 CASE( "AS" )  ; CALL aQuadrature_AS( Nma , xa,wa , errint_ , errmsg )
 CASE( "U"  )  ; CALL aQuadrature_U ( Nma , xa,wa , errint_ , errmsg )
 CASE DEFAULT  ; errint_ = -333
END SELECT
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
   CASE( -222 ) ; errmsg = "ERROR: The specified polar quadrature doesn't exist"
   CASE( -333 ) ; errmsg = "ERROR: The specified azimuthal quadrature doesn't exist"
   CASE( -444 ) ; errmsg = "ERROR: The number of polar ordinates must be divisible by 2"
  END SELECT
 ELSE
  CALL CLEAR(errmsg)
 END IF

END IF
