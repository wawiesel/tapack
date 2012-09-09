IF( TRIM(SIO%ACTION).sEQ.'Read' )THEN

 IF( SIO%Auto_Memory )THEN
  IF( ASSOCIATED(X) )THEN
   !just make it bigger
   CALL Reallocate( X , (/ n1(2) - SIZE(X,1) , &
                           n2(2) - SIZE(X,2) /) , FILL=.FALSE._KIND_L )
  ELSE
   ALLOCATE( X(1:n1(2),1:n2(2)) )
  ENDIF
 ENDIF

 CALL READ_DATABLOCK( SIO , X( n1(1):n1(2) , n2(1):n2(2) ) , FdBk )


ELSE
 
 CALL WRITE_DATABLOCK( SIO , X( n1(1):n1(2) , n2(1):n2(2) ) , FdBk )

 IF( SIO%Auto_Memory )THEN
  !just make it smaller
  CALL Reallocate( X , (/ n1(2) - n1(1) - 1 , &
                          n2(2) - n2(1) - 1 /) )
 ENDIF

ENDIF