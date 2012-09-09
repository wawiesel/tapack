IF( Reading(SIO) )THEN

 !pre-process memory
 IF( SIO%Auto_Memory )THEN
  IF( ASSOCIATED(X) )THEN
   !just make it bigger
   CALL Reallocate( X , dn=(/ n1(2) - SIZE(X,1) , &
                              n2(2) - SIZE(X,2) /) , &
                        fill = ERROR( x(LBOUND(x,1),LBOUND(x,2)) ) )
  ELSE
   ALLOCATE( X(n1(1):n1(2),n2(1):n2(2)) )
  ENDIF
 ENDIF

 !increment line number
 SIO%LINE_NUM = SIO%LINE_NUM + SIZE(X,2)

 !read the data
 CALL READ_DATABLOCK( SIO , X( n1(1):n1(2) , n2(1):n2(2) ) , FdBk )


ELSE
 
 !increment line number 
 SIO%LINE_NUM = SIO%LINE_NUM + SIZE(X,2)
 
 !write the data
 CALL WRITE_DATABLOCK( SIO , X( n1(1):n1(2) , n2(1):n2(2) ) , FdBk )

 !post-process memory
 IF( SIO%Auto_Memory )THEN
  !just make it smaller
  CALL Reallocate( X , (/ n1(2) - n1(1) - 1 , &
                          n2(2) - n2(1) - 1 /) )
 ENDIF

ENDIF