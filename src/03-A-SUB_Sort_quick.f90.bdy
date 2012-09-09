N = SIZE(list)
FORALL( i=1:N ) order_(i) = i

IF( PRESENT(max_simple_sort_size) )THEN
 max_simple_sort_size_ = max_simple_sort_size 
ELSE
 max_simple_sort_size_ = DEFAULT_max_simple_sort_size
END IF

!call recursive sorting algorithm for quick sort
CALL Sort_quick_( list , order_ , 1 , N , max_simple_sort_size_ )

IF( PRESENT(order) )THEN
  order = order_
ENDIF
!



 
 
