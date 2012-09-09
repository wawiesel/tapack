IF( Formatted(SIO) )THEN
 !!determine the SubArguments bounds by finding the SubArgDelimeters
 !!i.e. Locs(1)+1 to Locs(2)-1 are the beginning and end of the first 
 !!SubArgument---note that an extra Sub-Argument delimeter has been
 !!tacked on to the beginning and end in BEGIN_ARGUMENTS
 Locs=>NULL()
 CALL Find( SIO%SubArgDelim , STR(SIO%Arg(SIO%CurrArg)) , LOCS=Locs)

 !!now if we have a difference in the number of SubArguments found
 !!and the number asked for, we output an error
 IF( SIO%NSubArg(SIO%CurrArg)/=SIZE(X) )THEN
  VS = NOT_EQUAL(mod_,proc_,&
    var = "SIZE(Argument#"//TRIM(STR(SIO%CurrArg))//")", &
    wrong = STR(SIZE(X)) , &
    right = STR(SIO%NSubArg(SIO%CurrArg)))
  CALL UPDATE( fdbk_error , FdBk , s=STR(VS) )
  VS = ""
 END IF

 !!Now we extract the SubArguments and put the actual value into the
 !!data array passed into this routine, X, but we use a varying_string
 !!intermediary to check if the Argument is just the default Character, 
 !!SIO%Argdefault, without risking trucating the data by using a fixed
 !!length string. 
 DO j=1,SIO%NSubArg(SIO%CurrArg)
  VS = EXTRACT(SIO%Arg(SIO%CurrArg),Locs(j)+1,Locs(j+1)-1)
  IF( PRESENT(Default) .AND. VS==SIO%Argdefault )THEN 
   X(j) = Default(j)
  ELSE
   X(j) = INDEXa(KEYS,TRIM(STR(VS)),CaseSen=.FALSE.)
  END IF
  VS = ""
 END DO

ELSE
 DO j=1,SIZE(X)
  READ(SIO%Unit,IOSTAT=SIO%IOSTAT)X(j)
 END DO
END IF
