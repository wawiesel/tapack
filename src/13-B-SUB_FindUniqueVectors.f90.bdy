!! Get number of dimensions.
Nd = SIZE(r,1)

!! Get number of vectors.
Nv = SIZE(r,2)


!! Start off with same vectors as <r>.
ALLOCATE( r2(Nd,Nv) )
r2 = r

!! Print out the initial set of vectors.
IF( PRESENT(Unit) )THEN
 write(Unit,"(a)")"Initial Vectors"
 call PRINT_ColumnVectors(r2,Unit)
 IF( interactive )READ(*,*)
END IF


!! Sort vectors.
CALL SortVectors(r2,order,tol,reltol)

!! Print out <r2> after sorting.
IF( PRESENT(Unit) )THEN
 write(Unit,"(a)")"After Vector Sort"
 call PRINT_ColumnVectors(r2,Unit)
 IF( interactive )READ(*,*)
END IF


!! Allocate uniqueness mask.
ALLOCATE( unique(Nv) )

!! Remove duplicates and return the uniqueness mask.
CALL FlagDuplicateVectors0(r2,tol,reltol,unique)

!! Print out <r2>.
IF( PRESENT(Unit) )THEN
 write(Unit,"(a)")"After Flagging Duplicates"
 call PRINT_ColumnVectors(PackVectors(r2,unique),Unit)
 IF( interactive )READ(*,*)
END IF


!! Determine number of duplicates and uniques.
NUM_Unique = COUNT(unique)
NUM_Dups   = Nv - NUM_Unique


!! I know this looks strange, but it is the
!! right way to set the vectors that don't belong
!! order to 0.
WHERE( .NOT.unique )order = 0

!! If <r_unique> is present then return the unique, sorted vectors.
IF( PRESENT(r_unique) )THEN
 r2(:,1:NUM_Unique) = PackVectors(r2,unique)
 CALL Reallocate(r2,(/0,-NUM_Dups/))
 r_unique => r2

!! Otherwise, get rid of this pointer.
ELSE
 DEALLOCATE( r2 )
END IF


!! Don't need this guy either.
DEALLOCATE( unique )

