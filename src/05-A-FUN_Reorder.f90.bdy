!!#### OPTIONAL INPUT
CHARACTER(LEN=1),OPTIONAL,INTENT(IN) :: Side

!!#### LOCAL VARIABLES
CHARACTER(LEN=1) :: Side_



!determine the ordering side.
Side_ = DEFAULT("L",Side)

SELECT CASE(Side_)
 CASE("L","l")
   FORALL(i=1:SIZE(old_list),order(i)>0)
    new_list(order(i)) = old_list(i) 
   END FORALL
 
 CASE("R","r")
   FORALL(i=1:SIZE(old_list),order(i)>0)
    new_list(i) = old_list(order(i)) 
   END FORALL

END SELECT