IF( Reading(SIO) )THEN

 !automemory
 IF( SIO%Auto_Memory )THEN
  IF( ASSOCIATED(X) )THEN
   !just make it bigger
   CALL Reallocate( X , dn=n1(2)-UBOUND(X,1) , &
     dnl=n1(1)-LBOUND(x,1) , FILL=ERROR(X(1)) )
  ELSE
   ALLOCATE( X(n1(1):n1(2)) )
  ENDIF
 ENDIF

 !raed the datablock
 CALL READ_DATABLOCK ( SIO , X( n1(1):n1(2) ) , FdBk )


ELSE
 
 !write the datablock
 CALL WRITE_DATABLOCK( SIO , X( n1(1):n1(2) ) , FdBk , Indent=Indent )

 !auto memory
 IF( SIO%Auto_Memory )THEN
  
  !can only reallocate if the lower extent is the lower bound
  IF( n1(1)==LBOUND(x,1) )THEN
   
   CALL Reallocate( X , dn=0 , dnl=+n1(2)-n1(1)+1 )

  !or upper extent is the upper bound
  ELSE IF( n1(2)==UBOUND(x,1) )THEN

   CALL Reallocate( X , dn=+n1(1)-n1(2)-1 , dnl=0 )

  END IF

 ENDIF

ENDIF