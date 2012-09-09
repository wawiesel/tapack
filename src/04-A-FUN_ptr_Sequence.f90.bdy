ALLOCATE( y(n) )

if (n > 0) y(1)=first
if (n <= MAX_DIRECT_SEQUENCE) then
        do k=2,n
                y(k)=y(k-1)+increment
        end do
else
        do k=2,MAX_DIRECT_SEQUENCE
                y(k)=y(k-1)+increment
        end do
        temp=increment*MAX_DIRECT_SEQUENCE
        k=MAX_DIRECT_SEQUENCE
        do
                if (k >= n) exit
                k2=k+k
                y(k+1:min(k2,n))=temp+y(1:min(k,n-k))
                temp=temp+temp
                k=k2
        end do
end if
