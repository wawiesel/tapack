!! Set whether or not to display some statistics about the matrix.
IF( PRESENT(stats) )THEN
 stats_ = stats
ELSE
 stats_ = DEFAULT_stats
ENDIF

!! Set the unit to write to.
IF( PRESENT(unit) )THEN
 unit_ = unit
ELSE
 unit_ = DEFAULT_unit
ENDIF

!! Set the zero character.
IF( PRESENT(zero) )THEN
 zero_ = zero
ELSE
 zero_ = DEFAULT_zero
ENDIF

!! Set the unity character.
IF( PRESENT(unity) )THEN
 unity_ = unity
ELSE
 unity_ = DEFAULT_unity
ENDIF

!! Set the other character.
IF( PRESENT(other) )THEN
 other_ = other
ELSE
 other_ = DEFAULT_other
ENDIF

!! Set the lhighlight character.
IF( PRESENT(lhighlight) )THEN
 lhighlight_ = lhighlight
ELSE
 lhighlight_ = DEFAULT_lhighlight
ENDIF

!! Set the rhighlight character.
IF( PRESENT(rhighlight) )THEN
 rhighlight_ = rhighlight
ELSE
 rhighlight_ = DEFAULT_rhighlight
ENDIF

!! Initialize stats variables if they are being asked for.
IF( stats_ )THEN
 n_zero=0
 n_unity=0
 n_other=0
 n_upper=0
 n_lower=0
 n_diag=0
 n_nonzero=0
ENDIF

!! Print out a matrix with the character '0' for zero elements,
!! '1' for unity elements, and asterisks (*) for all other elements.
DO i=LBOUND(A,1),UBOUND(A,1)
 DO j=LBOUND(A,2),UBOUND(A,2)
  
  IF( A(i,j)==REAL(0,KIND(A)) )THEN
   char = zero_
   IF(stats_) n_zero = n_zero + 1
  
  ELSE
   n_nonzero = n_nonzero + 1
   IF     (i<j)THEN
    n_upper = n_upper + 1
   ELSEIF(i>j)THEN
    n_lower = n_lower + 1
   ELSE
    n_diag  = n_diag  + 1
   ENDIF

   IF( A(i,j)==REAL(1,KIND(A)) )THEN
    char = unity_
    IF(stats_) n_unity  = n_unity  + 1
   ELSE
    char = other_
    IF(stats_) n_other= n_other+ 1
   ENDIF

  ENDIF

  !! Write out a special grouping for the diagonal ( ) with a special
  !! format needed and do not advance unless we are at the last column.
  IF( i==j )THEN
   outchar = lhighlight_//char//rhighlight_
  ELSE
   outchar = " "//char//" "
  END IF
  
  WRITE(unit_,FMT    ="(a5)",&
              ADVANCE=MERGE('yes'         ,'no '       ,j==UBOUND(A,2)) )outchar

 END DO
END DO

!! These are the statistics.
IF( stats_ )THEN
 33 FORMAT(a60,i10)
 34 FORMAT(a60,f9.1,a1)
 WRITE(unit_,33)'Number of elements TOTAL: ',                          n_zero+n_unity+n_other
 WRITE(unit_,33)'Number of nonzero elements on DIAGONAL: ',            n_diag
 WRITE(unit_,33)'Number of nonzero elements in UPPER triangle: ',      n_upper
 WRITE(unit_,33)'Number of nonzero elements in LOWER triangle: ',      n_lower
 WRITE(unit_,34)'MATRIX DIAGONAL percentage (n_diag/n_total): ',       100.*REAL(n_diag)/REAL(n_diag+n_upper+n_lower),'%'
 WRITE(unit_,34)'MATRIX LOWER triangle percentage (n_lower/n_total): ',100.*REAL(n_lower)/REAL(n_diag+n_upper+n_lower),'%'
 WRITE(unit_,34)'MATRIX UPPER triangle percentage (n_upper/n_total): ',100.*REAL(n_upper)/REAL(n_diag+n_upper+n_lower),'%'
 WRITE(unit_,33)'Number of elements equal to  ZERO: ',   n_zero
 WRITE(unit_,33)'Number of elements equal to   ONE: ',   n_unity
 WRITE(unit_,33)'Number of elements equal to OTHER: ',   n_other
 WRITE(unit_,34)'MATRIX SPARSITY (1-n_nonzero/n_total) : ',            100.-100.*REAL(n_nonzero)/REAL(SIZE(A)),'%'
ENDIF