IF( .NOT.ASSOCIATED(VSin) )THEN
 ALLOCATE( S )
 S = "NULL("//TYPE_KIND//")"
 RETURN
END IF

!USE <ptr_STR>
S => ptr_STR( VSin , FMT , AdjustLeft , AdjustRight )
