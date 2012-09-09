
!require that <i> and <ilast> are only one apart
!but don't check it for now
!IF( .NOT.( i==ilast+1 ) )THEN
! WRITE(*,*)"you must assign Entries to a sparse array in order"
! STOP
!END IF
!
!ilast = i

NE = SIZE(Entry)

!initial allocation if required
IF( .NOT.ASSOCIATED(SparseArray) )THEN
 ALLOCATE( SparseArray(NE) )
 SparseArray = ERROR_
 ALLOCATE( IndexArray(1) )
 j = 0
ELSE
 IF( i==1 )THEN
  j = 0
 ELSE
  j = IndexArray(i-1)
 END IF
END IF

i1 = j + 1
i2 = j + NE

N(1) = SIZE(SparseArray)
N(2) = SIZE(IndexArray)

!check index array
IF( i>N(2) )THEN
 M = Factor*(i-N(2)) + 1
 CALL REALLOCATE( IndexArray , M , fill=ERROR_I )
END IF
IndexArray(i) = i2

!check value array
IF( i2>N(1) )THEN
 M = Factor*(i2-N(1)) + 1
 CALL REALLOCATE( SparseArray , M , fill=ERROR_ )
END IF

SparseArray(i1:i2) = Entry
