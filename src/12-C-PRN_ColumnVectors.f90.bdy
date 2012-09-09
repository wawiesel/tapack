
!get formats for indices and values
FMT_i_ = DEFAULT( DEFAULT_FMT_i , FMT_i ) 
FMT_x_ = DEFAULT( DEFAULT_FMT_x , FMT_x ) 

!get numbers
Ni = SIZE(x,1)
Nc = SIZE(x,2)


!allocate
ALLOCATE( D(1+Ni,1+Nc) )

!set the column name for the index column
IF( PRESENT(NAME_index) )THEN
 D(1,1) = NAME_index
ELSE
 D(1,1) = REPEAT(" ",LEN(D(1,1)))
END IF

!set the column name for the vectors
DO c=1,Nc
 IF( PRESENT(NAME_x) )THEN
  D(1,1+c) = NAME_x(c)
 ELSE
  D(1,1+c) = "x_"//TRIM(STR(c))
 END IF
END DO

!set the names of the indices
DO i=1,Ni
 IF( PRESENT(NAME_i) )THEN
  D(1+i,1) = NAME_i(i)
 ELSE
  D(1+i,1) = STR(i,FMT=FMT_i_)
 END IF
END DO

!set the meaty center
DO c=1,Nc
 DO i=1,Ni
  D(1+i,1+c) = STR(x(i,c),FMT=FMT_x_)
 END DO
END DO

!print
CALL PRINT_Table(D,Unit=Unit)

!deallocate
DEALLOCATE( D )
