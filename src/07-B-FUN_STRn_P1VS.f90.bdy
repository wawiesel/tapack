IF( .NOT.ASSOCIATED(VSin) )THEN
 ALLOCATE( S(1) )
 S = "NULL("//TYPE_KIND//")"
 RETURN
END IF

!USE <ptr_STR>
S => ptr_STR( VSin , FMT , AdjustLeft , AdjustRight )
