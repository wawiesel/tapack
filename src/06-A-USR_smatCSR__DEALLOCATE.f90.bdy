!#LOCAL VARIABLES
INTEGER :: jerr

A % N = 0
DEALLOCATE (A % A, A % IA, A % JA , STAT=jerr )
