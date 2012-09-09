
SIZE_Entry = 0
IF( ASSOCIATED(IndexArray) )THEN
 IF( i==1 )THEN
  SIZE_Entry = IndexArray(i) 
 ELSE IF( i>=1 .AND. i<=SIZE(IndexArray) )THEN
  SIZE_Entry = IndexArray(i)-IndexArray(i-1)
 END IF
END IF
