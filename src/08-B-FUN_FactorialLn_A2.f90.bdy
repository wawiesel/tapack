IF( Initialize<0 .AND. ASSOCIATED(a) )THEN
  
 !reallocate
 CALL Reallocate( a , Initialize )
  
ELSEIF( Initialize>0 )THEN
  
 !allocate
 IF( ASSOCIATED(a) )THEN
  IF( Initialize>SIZE(a) )THEN
   CALL REALLOCATE( a , Initialize-SIZE(a) )
   a(Initialize:) = GammaLn( Sequence( REAL(Initialize,KIND_R) , &
     1._KIND_R , Initialize-SIZE(a) ) )
  ENDIF
 ELSE
  ALLOCATE( a(1:Initialize) )
  a = GammaLn( Sequence( 1._KIND_R , 1._KIND_R , SIZE(a) ) )
 ENDIF
 
ELSE
 IF( ASSOCIATED(a) )THEN
  DEALLOCATE( a , STAT=jerr )
  NULLIFY(a)
 ENDIF
ENDIF

IF( x<=SIZE(a) )THEN
 y = a(x)
ELSE
 y = GammaLn( REAL(x + 1_KIND_I,KIND_R) )
ENDIF
