IF( Reading(SIO) )THEN

 !raed the datablock
 CALL READ_DATABLOCK ( SIO , X , FdBk , Keys=Keys , CaseSen=CaseSen )


ELSE
 
 !write the datablock
 CALL WRITE_DATABLOCK( SIO , X , FdBk , Keys=Keys , Indent=Indent )

END IF