IF( ASSOCIATED(X) )THEN
 NUM_Bytes = BYTES_PER_ELEMENT*SIZE(X)*LEN(X) + BYTES_FOR_OVERHEAD
ELSE
 NUM_Bytes = 0
END IF