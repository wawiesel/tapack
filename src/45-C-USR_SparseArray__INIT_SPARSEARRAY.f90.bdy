
IF( ASSOCIATED(SparseArray) )THEN
 CALL CLEARn( SparseArray )
END IF
ALLOCATE( SparseArray(N(1)*N(2)) )
SparseArray = ERROR_


IF( ASSOCIATED(IndexArray) )THEN
 CALL CLEARn( IndexArray )
END IF
ALLOCATE( IndexArray(N(2)) )
IndexArray = ERROR_I