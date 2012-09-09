do
 IF( PRESENT(domain) )THEN
  x1 = domain(1)
  x2 = domain(2)
 ELSE
  write (*,*) ' Enter x1,x2 (x1==x2 to exit)'
  read (*,*) x1,x2
  if (x1 == x2) RETURN
 ENDIF

 scr(1,1:JSCR)=yy
 scr(ISCR,1:JSCR)=yy
 scr(2:ISCR-1,1)=xx
 scr(2:ISCR-1,JSCR)=xx
 scr(2:ISCR-1,2:JSCR-1)=blank
 dx=(x2-x1)/(ISCR-1)
 x=x1
 do i=1,ISCR
     y(i)=f(x)
     x=x+dx
 end do
 IF( PRESENT(range) )THEN
  ysml = range(1)
  ybig = range(2)
 ELSE
  ysml=minval(y(:))
  ybig=maxval(y(:))
 END IF
 if (ybig == ysml) ybig=ysml+1.0
 
 dyj=REAL(JSCR-1,KIND_R)/(ybig-ysml)
 jz=1-INT(ysml*dyj,KIND_I)
 
 !put in the y=0 line if it appears in the range
 IF( jz>=1 .AND. jz<=JSCR )THEN
  scr(1:ISCR,jz)=zero
 END IF
 do i=1,ISCR
     j=1+(y(i)-ysml)*dyj
     scr(i,j)=ff
 end do
 write (*,'(1x,1p,e10.3,1x,80a1)') ybig,(scr(i,JSCR),i=1,ISCR)
 do j=JSCR-1,2,-1
     write (*,'(12x,80a1)') (scr(i,j),i=1,ISCR)
 end do
 write (*,'(1x,1p,e10.3,1x,80a1)') ysml,(scr(i,1),i=1,ISCR)
 write (*,'(12x,1p,e10.3,40x,e10.3)') x1,x2
 if( PRESENT(domain) )then
  EXIT
 endif
end do
