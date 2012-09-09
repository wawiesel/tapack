!Init
Entry = ERROR_

!can't fit the stored values into the passed array <Entry> or
!index out of bounds

NE = SIZE_Entry_SPARSEARRAY(SparseArray,IndexArray,i)

IF( j>=1 .AND. j<=NE .AND. i>=1 .AND. i<=NUM_Entry_SPARSEARRAY(SparseArray,IndexArray) )THEN
 
 IF( i==1 )THEN
  i1 = 0
 ELSE
  i1 = IndexArray(i-1)
 END IF

 j0 = i1 + j
 Entry = SparseArray(j0)

END IF
