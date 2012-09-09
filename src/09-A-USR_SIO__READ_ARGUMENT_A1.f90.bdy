IF( Formatted(SIO) )THEN
 
 !
 IF( .NOT.sio%present(sio%currarg) )THEN
  IF( PRESENT(default) )THEN
   X=default
  END IF
  RETURN
 END IF

 !determine the subarguments bounds by finding the subargdelimeters
 !i.e. Locs(1)+1 to Locs(2)-1 are the beginning and end of the first 
 !subargument---note that an extra sub-argument delimeter has been
 !tacked on to the beginning and end in BEGIN_ARGUMENTS
 Locs=>NULL()
 CALL Find( SIO%subargdelim , STR(SIO%arg(SIO%currarg)) , LOCS=Locs)

 !now if we have a difference in the number of subarguments found
 !and the number asked for, we output an error
 IF( SIO%nsubarg(SIO%currarg)/=SIZE(X) )THEN
  VS = NOT_EQUAL(mod_,proc_,&
    var='SIZE(Argument#'//TRIM(STR(SIO%currarg))//')', &
    wrong=STR(SIZE(X)) , &
    right=STR(SIO%nsubarg(SIO%currarg)))
  CALL UPDATE(fdbk_error,FdBk,s=STR(VS) )
  VS = ""
 END IF

 !Now we extract the subarguments and put the actual value into the
 !data array passed into this routine, X, but we use a varying_string
 !intermediary to check if the argument is just the default character, 
 !SIO%argdefault, without risking trucating the data by using a fixed
 !length string.
 DO j=1,SIO%nsubarg(SIO%currarg)
  VS = EXTRACT(SIO%arg(SIO%currarg),Locs(j)+1,Locs(j+1)-1)
  IF( PRESENT(Default) .AND. VS==SIO%argdefault )THEN 
   X(j) = Default(j)
  ELSE
   X(j) = STR(VS)
  ENDIF
  VS = ""
 END DO
 !deallocate locs
 DEALLOCATE(Locs)

ELSE
 READ(SIO%UNIT,IOSTAT=SIO%IOSTAT) X
 IF( SIO%IOSTAT/=0 )THEN
  VS = MODPROC(mod_,proc_)
  CALL UPDATE(fdbk_error,FdBk,s=STR(VS)//&
  'an error occurred while reading unformatted arguments.')
  VS = ""
 END IF
END IF

 