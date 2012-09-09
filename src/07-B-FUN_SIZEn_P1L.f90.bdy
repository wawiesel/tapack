IF( .NOT.ASSOCIATED(POINTER_ARRAY) )THEN
 N = 0
ELSE
 !note, we do not have SIZEa for logicals so we just
 !use SIZE
 N = SIZE( POINTER_ARRAY )
END IF


