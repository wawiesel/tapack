!Init
Entry = ERROR_

!can't fit the stored values into the passed array <Entry> or
!index out of bounds
IF( .NOT.(SIZE(Entry)<SIZE_Entry_DENSEARRAY(DenseArray,i) .OR. &
  i<0 .OR. i>NUM_Entry_DENSEARRAY(DenseArray)) )THEN
 
 NE = SIZE_Entry_DENSEARRAY( DenseArray , i )

 Entry(1:NE) = DenseArray(1:NE,i)

ELSE

 NE = 0

END IF

IF( PRESENT(NUM_Entry) )THEN
 NUM_Entry = NE
END IF