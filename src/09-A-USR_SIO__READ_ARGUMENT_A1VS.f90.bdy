IF( Formatted(SIO) )THEN
 !determine the number of subarguments in a locations array
 !which bounds them:  i.e. Locs(1)+1 to Locs(2)-1 are the
 !beginning and end of the first subargument
 Locs=>NULL()
 CALL Find( SIO%subargdelim , STR(SIO%arg(SIO%currarg)) , LOCS=Locs)
 IF( ASSOCIATED(Locs) )THEN
  SIO%nsubarg(SIO%currarg) = SIZE(Locs)+1
  DEALLOCATE(Locs)
 ELSE
  SIO%nsubarg(SIO%currarg) = 1
 END IF
 CALL Reallocate( Locs , +2 )
 Locs(2:SIZE(Locs)-1) = Locs(1:SIZE(Locs)-2)
 Locs(1) = 0
 Locs(SIZE(Locs)) = LEN_TRIM(SIO%arg(SIO%currarg))+1

 !now if we have a difference in the number of subarguments found
 !and the number asked for, we output an error
 IF( SIO%nsubarg(SIO%currarg)/=SIZE(X) )THEN
  VS=NOT_EQUAL(mod_,proc_,&
    var = 'The number of elements in the array passed into this routine', &
    wrong = STR(SIZE(X)) , &
    right = STR(SIO%nsubarg(SIO%currarg)))
  CALL UPDATE(fdbk_error,FdBk,s=STR(VS) )
  VS=""
 END IF

 !Now we extract the subarguments and put the actual value into the
 !data array passed into this routine, X, but we use a varying_string
 !intermediary to check if the argument is just the default character, 
 !SIO%argdefault, without risking trucating the data by using a fixed
 !length string.
 DO j=1,SIO%nsubarg(SIO%currarg)
  X(j) = EXTRACT(SIO%arg(SIO%currarg),Locs(j)+1,Locs(j+1)-1)
  IF( PRESENT(Default) .AND. X(j)==SIO%argdefault )THEN 
   X(j) = Default(j)
  END IF
 END DO
 
ELSE

 DO j=1,SIO%nsubarg(SIO%currarg)
  CALL GET( SIO%UNIT , X(j) , SET=SIO%stop , IOSTAT=SIO%IOSTAT )
 END DO
 IF( SIO%IOSTAT/=0 )THEN
  VS = MODPROC(mod_,proc_)
  CALL UPDATE(fdbk_error,FdBk,s=STR(VS)//&
  'an error occurred while reading an unformatted varying string.  &
  &If this command was constructed correctly, the only problem I can think of &
  &is that the current stop character ('//SIO%stop//') was not the same as the &
  &one found?  The (unTRIMmed) varying string I found was: '//STR(X(j))//'.  &
  &The only thing you can do is check the varying string for the stop &
  &character and add a SYNTAX command before the unformatted input to change &
  &the stop character to the needed one and another SYNTAX command after to change &
  &the stop character back.')
  VS = ""
 END IF
END IF

 