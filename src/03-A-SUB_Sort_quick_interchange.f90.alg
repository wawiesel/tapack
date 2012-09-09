!!#### PURPOSE
!! Sort a list (list) and preserve the original 
!! ordering (order).

!!#### METHOD
!! Interchange sort algorithm---good for small lists.

DO i = left_end, right_end - 1
  DO j = i+1, right_end
    IF( list(i) > list(j) )THEN
      temp = list(i); list(i) = list(j); list(j) = temp
      itemp = order(i)
      order(i) = order(j)
      order(j) = itemp
    ENDIF
  END DO
END DO
