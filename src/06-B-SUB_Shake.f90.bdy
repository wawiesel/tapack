!! Get size
M = SIZE(ARRAY)

!! Get number of swaps to make.
NUM_ = DEFAULT(M,NUM)

!! Get random number interval.
l = (/1,M+1/)

!! Loop.
DO k=1,NUM_
 
 !! Get indices.
 i = Random(l)
 j = Random(l)
 
 !! Swap.
 CALL Swap(ARRAY(i),ARRAY(j))

END DO