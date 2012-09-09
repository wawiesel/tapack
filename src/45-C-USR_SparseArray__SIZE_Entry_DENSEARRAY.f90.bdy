
SIZE_Entry = 0
IF( ASSOCIATED(DenseArray) )THEN
 IF( i>=1 .AND. i<=SIZE(DenseArray,2) )THEN
  !handling null entries
  IF( DenseArray(1,i)==WARNING_ )THEN
   SIZE_Entry = 0
  ELSE
   SIZE_Entry = SIZEa(DenseArray(:,i))
  END IF
 END IF
END IF
