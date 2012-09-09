
IF( ASSOCIATED(DenseArray) )THEN
 CALL CLEARn( DenseArray )
END IF

ALLOCATE( DenseArray(N(1),N(2)) )
DenseArray = ERROR_
