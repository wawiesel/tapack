IF( SIZE(VECS,1)==1 )THEN
 !sort the values, finally
 CALL Sort(VECS(1,:),order_)
ELSE
 
 !sort the leading dimensions values with dummies
 sort_dummy = VECS(1,:)
 CALL Sort(sort_dummy,order_)
 
 !reorder all dimensions based on <order_> and keep track
 DO J=1,SIZE(VECS,1)
  VECS(J,:) = Reorder(VECS(J,:),order_,side="R") 
 END DO

 !we are now about to determine of the consecutive values
 !in this dimension, what is the proper sorting of the next dimension?
 
 !set the start point
 k1 = 1
 
 !loop over segments
 DO 
  
  !get the number of consecutive values
  N = COUNT_Consecutive(sort_dummy(k1:),tol,reltol)
  
  !determine the extent to which coordinates of this dimension
  !are identical
  k2 = k1+N-1
  
  !if we have an actual interval (as opposed to 1 value)
  IF( k2>k1 )THEN
  
   !sort the next dimension for those coordinates that are identical
   !in this dimension
   CALL SORT2_quick(VECS(2:,k1:k2),order_dummy(k1:k2))
   
   !reorder the order array to reflect this new ordering from the
   !next dimension--we don't have to reorder the coordinates
   !in this dimension because the were identical in the first place!
   order_(k1:k2) = Reorder(order_(k1:k2),order_dummy(k1:k2),side="R")

  END IF

  !increment the start point
  k1 = k2+1
  
  !if we have finished then exit the loop
  IF( k1>=SIZE(sort_dummy) )EXIT
 END DO

END IF 

IF( PRESENT(order) )THEN
 order = order_
END IF