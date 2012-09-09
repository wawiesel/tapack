
!Init
Entry = ERROR_

!index out of bounds
IF( .NOT.(j<0 .OR. j>SIZE_Entry_DENSEARRAY(DenseArray,i) .OR. &
  i<0 .OR. i>NUM_Entry_DENSEARRAY(DenseArray)) )THEN
 
 Entry = DenseArray(j,i)

END IF
