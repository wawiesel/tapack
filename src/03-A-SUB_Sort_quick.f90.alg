!!#### PURPOSE
!! Sort a list (list) and preserve the original ordering (order).

!!#### METHOD
!! Recursive partitioned (Quick) sort.  The recursive call is via
!! the host subroutine (Sort_quick_) on the list (list) and order (order) passed
!! as arguments.

reference = list((left_end + right_end)/2)
i = left_end - 1; j = right_end + 1

DO
  !! Scan list from left end until element >= reference is found
  DO
    i = i + 1
    IF (list(i) >= reference) EXIT
  END DO
  !! Scan list from right end until element <= reference is found
  DO
    j = j - 1
    IF (list(j) <= reference) EXIT
  END DO


  IF (i < j) THEN
    !! Swap two out-of-order elements
    temp = list(i); list(i) = list(j); list(j) = temp
    itemp = order(i); order(i) = order(j); order(j) = itemp
  ELSE IF (i == j) THEN
    i = i + 1
    EXIT
  ELSE
    EXIT
  END IF
END DO

IF (left_end < j)THEN
 CALL Sort_quick_(list,order,left_end, j,max_simple_sort_size_)
ENDIF

IF (i < right_end)THEN
 CALL Sort_quick_(list,order,i, right_end,max_simple_sort_size_)
ENDIF