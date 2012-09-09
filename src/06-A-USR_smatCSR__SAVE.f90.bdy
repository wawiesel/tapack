Unit = NewUnit()
IF (Unit==-1) STOP "BIG TROUBLE: No more Fortran UNITs available."

IF (PRESENT(form)) THEN
   form_ = ADJUSTL(form)
ELSE
   form_ = "formatted"
END IF

OPEN (Unit, FILE=SA//".dat", STATUS = "UNKNOWN", IOSTAT = ios,&
     & ACTION = "write", form = form_)

IF (ios/=0) THEN
   WRITE(*,"(A)") "save_CSR: Could not open write file '"// SA //".dat'"
   STOP
END IF

SELECT CASE (form_(1:1))
CASE ("f", "F")
   write (Unit,*) A % N
   Field_Len = 2 + INT(LOG10(REAL(A % IA (A % N + 1))) + EPS )
   Rec_Len   = 80 / Field_Len
   Num_Recs  = CEILING (REAL(A % N) / REAL(Rec_Len))
   write (format,*) '(', Num_Recs, '(', Rec_Len, 'I', Field_Len, ':/))'
   write (Unit,format) A % IA
   Field_Len = 2 + INT(LOG10(REAL(A % N)) + EPS )
   Rec_Len   = 80 / Field_Len
   Num_Recs  = CEILING (REAL(A % IA (A % N + 1)-1) / REAL(Rec_Len))
   write (format,*) '(', Num_Recs, '(', Rec_Len, 'I', Field_Len, ':/))'
   write (Unit,format) A % JA (1:A % IA (A % N + 1) - 1)
   Field_Len = LEN
   Rec_Len   = 80 / Field_Len
   Num_Recs  = CEILING (REAL(A % IA (A % N + 1)-1) / REAL(Rec_Len))
   write (format,*) '(', Num_Recs, '(', Rec_Len, 'ES', Field_Len,&
        & '.', LEN-8, 'E2:/))'
   write (Unit,format) A % A (1:A % IA (A % N + 1) - 1)
CASE ("u", "U")
   WRITE (Unit) A % N
   DO i = 1, A % N + 1
      WRITE (Unit) A % IA (i)
   END DO
   DO i = 1, A % IA (A % N + 1) - 1
      WRITE (Unit) A % JA (i)
   END DO
   DO i = 1, A % IA (A % N + 1) - 1
      WRITE (Unit) A % A (i)
   END DO

END SELECT

CLOSE(Unit, STATUS = "keep")