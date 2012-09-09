IF( .NOT.ASSOCIATED(Sin) )THEN
 ALLOCATE( S )
 S = "NULL("//TYPE_KIND//")"
 RETURN
END IF

!USE <ptr_STR>
S => ptr_STR( Sin , FMT , AdjustLeft , AdjustRight )
