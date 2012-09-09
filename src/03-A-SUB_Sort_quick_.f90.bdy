IF (right_end < left_end + max_simple_sort_size_) THEN

  !USE interchange sort for small lists
  INCLUDE "03-A-SUB_Sort_quick_interchange.f90.alg"

ELSE

  !use quick sort for big lists
  INCLUDE "03-A-SUB_Sort_quick.f90.alg"

ENDIF