IF( .NOT.ASSOCIATED(X) )THEN
 ALLOCATE( S(1) )
 S = "NULL("//TYPE_KIND//")"
 RETURN
END IF

!USE <ptr_STR>
S => ptr_STR( X , FMT , AdjustLeft , AdjustRight )
