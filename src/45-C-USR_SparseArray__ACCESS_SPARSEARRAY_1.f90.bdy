!Init
Entry = ERROR_

!can't fit the stored values into the passed array <Entry> or
!index out of bounds

NE = SIZE_Entry_SPARSEARRAY(SparseArray,IndexArray,i)

IF( .NOT.(SIZE(Entry)<NE .OR. &
  i<0 .OR. i>NUM_Entry_SPARSEARRAY(SparseArray,IndexArray)) )THEN
 
 IF( i==1 )THEN
  i1 = 1
 ELSE
  i1 = IndexArray(i-1)+1
 END IF

 i2 = IndexArray(i)

 Entry(1:NE) = SparseArray(i1:i2)

ELSE
 NE = 0
END IF

IF( PRESENT(NUM_Entry) )THEN
 NUM_Entry = NE
END IF