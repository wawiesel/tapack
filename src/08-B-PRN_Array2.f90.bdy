! Determine the convenience variables NX and NY.
NX = SIZE(A,1)
NY = SIZE(A,2)

! Kick out on strange values.
IF( NX==0 .OR. NY==0 )RETURN

! Determine the number of spaces between groups.
IF( PRESENT(NSEP) )THEN
 NSpace=NSEP
ELSE
 NSpace=0
END IF

! Determinet the number of elements per line.
IF( PRESENT(NPER) )THEN
 NPerLine=NPER
ELSE
 NPerLine=SIZE(A,1)
END IF

! Determine the unit number.
IF( PRESENT(Unit) )THEN
 Unit_ = Unit
ELSE
 Unit_ = Window_unit
END IF

! Determine the format.
CALL CLEAR(FMT_)
IF( PRESENT(FMT) )THEN
 FMT_ = FMT
ELSE
 FMT_ = DEFAULT_FMT
END IF

! Set the write header variables.
writexheader = PRESENT(xHDR)
writeyheader = PRESENT(yHDR)

!Determine number of full lines, partial lines, with a fixup.
NumFullLines=INT(NX/NPerLine)
NumPartialLineA=MOD(NX,NPerLine)
AddLine = MERGE( 0 , 1 , NumPartialLineA==0 )

! Determine the appropriate format for the xheader.
IF( writexheader )THEN
 wstr = ADJUSTL(STR(FieldWidth(FMT_)))
END IF

! Determine the appropriate format for the yheader.
IF( writeyheader )THEN
 ynum = 0
 DO IY=LBOUND(yHDR,1),UBOUND(yHDR,1)
  ynum = MAX(ynum,LEN_TRIM(yHDR(IY)))
 END DO
 ystr = STR(ynum)
END IF

! Initialize the loop and go.
CurrVar=1
DO Line=1,NumFullLines+AddLine
 
 IF(Line<NumFullLines+AddLine)THEN
  AExpected=NPerLine
 ELSEIF(AddLine==0 .AND. Line==NumFullLines)THEN
  AExpected=NPerLine
 ELSEIF(AddLine==1 .AND. Line==NumFullLines+AddLine)THEN
  AExpected=NumPartialLineA
 END IF
 EndLine=CurrVar+AExpected-1
 
 ! Reset the number of elements on this line in string form.
 nstr = STR(EndLine-CurrVar+1)

 ! Write the first y-header.
 IF( writeyheader )WRITE(UNIT_,'(a'//TRIM(ystr)//')',ADVANCE='no')yHDR(0)
 
 ! Write the first x-header.
 IF( writexheader )WRITE(UNIT_,'('//TRIM(nstr)//'a'//TRIM(wstr)//')')&
   (TRIM(xHDR(IX)),IX=CurrVar,EndLine)

 ! Write the rest of the y-headers and data.
 DO IY=1,NY
  IF( writeyheader ) WRITE(UNIT_,'(a'//TRIM(ystr)//')',ADVANCE='no')yHDR(IY)
  WRITE(UNIT_,'('//TRIM(nstr)//TRIM(FMT_)//')')&
    (A(IX,IY),IX=CurrVar,EndLine)
 END DO
 
 ! Skip some spaces if needed.
 DO Ispace=1,NSpace
  IF(Line/=NumFullLines+AddLine) WRITE(UNIT_,*)
 END DO

 CurrVar=EndLine+1

END DO
