
!require that <i> and <ilast> are only one apart


!quick return ( assumption is that i=1,...,SIZE(Array,2) )
IF( i<1 )RETURN

!initial allocation if required
IF( .NOT.ASSOCIATED(DenseArray) )THEN
 ALLOCATE( DenseArray(SIZE(Entry),i) )
 DenseArray = ERROR_
END IF
 

!get sizes to increase allocation if needed
N(1) = SIZE(DenseArray,1)
N(2) = SIZE(DenseArray,2)
NE   = SIZE(Entry)
 
!second dimension
IF( i>N(2) )THEN
 M = Factor*(i-N(2)) + 1
 !WRITE(*,*)" reallocating dim 2 by M=",M
 CALL REALLOCATE( DenseArray , (/0,M/) , fill=ERROR_)
END IF
 
!first dimension
IF( NE>N(1) )THEN
 M = Factor*(NE-N(1)) + 1
 !WRITE(*,*)" reallocating dim 1 by M=",M
 CALL REALLOCATE( DenseArray , (/M,0/) , fill=ERROR_)
END IF


!actual assignment

!null entry handling
IF( NE==0 )THEN
 DenseArray(1,i) = WARNING_
ELSE
 DenseArray(1:NE,i) = Entry
END IF
