IF (.NOT.ALLOCATED(A))THEN
 !SetRow_CSR: matrix not allocated
 RETURN
END IF

N = A % N
IF (i<1.OR.i>A % N)THEN
 !SetRow_CSR: row out range
 RETURN
END IF
!
old_len = A % IA(i+1) - A % IA(i)
new_len = SIZE(cols)
diff_len = new_len - old_len
!
!  Check to see if there is room for the new elements in A
!
IF ( A % IA (N+1) + diff_len > SIZE( A % A ) )THEN
 CALL REALLOCATE(A, &
     & MAX(Block_Size, A % IA (N+1) + diff_len - SIZE(A % A)))
END IF
!
!  Move old values in A to fit in the new row.  Note that if the row is
!  appended (that is; there is no row with number > i inserted) the range
!  of the indices is empty and the next line is a 'no operation'.  Hence
!  the fastest way to construct a matrix is to append rows successively. 
!  This functionality is heavily dependent upon the initialisation of all
!  start of row pointers (AJ(1:N+1)) to the base of the storeage
!  vector (N+2)  (see the allocate_CSR routine)
!
A % A (A % IA (i+1)+diff_len:A % IA (N+1)+diff_len) = &
     & A % A (A % IA (i+1):A % IA (N+1))
A % JA (A % IA (i+1)+diff_len:A % IA (N+1)+diff_len) = &
     & A % JA (A % IA (i+1):A % IA (N+1))
!
!  Adjust start of row pointers
!
A % IA (i+1:N+1) = A % IA (i+1:N+1) + diff_len
!
!  Insert elements
!
A % A (A % IA(i):A % IA(i+1)-1) = vals
A % JA (A % IA(i):A % IA(i+1)-1) = cols