program main
implicit none

integer,parameter :: n=9
real*8 :: mat(n,n),b(n),L(n,n),u(n,n),sum,sum2,x(n),y(n)
integer :: i,r,k
open(unit=10,file="data.txt")
do i=1,n
read(10,*),mat(:,i)
end do

read(10,*),b

do r=1,n
    do i=r,n
	    sum=0
		do k=1,r-1
		    sum=sum+L(r,k)*u(k,i)
		end do
	    u(r,i)=mat(r,i)-sum
		sum2=0
		do k=1,r-1
		    sum2=sum2+L(i,k)*u(k,r)
		end do
		L(i,r)=(mat(i,r)-sum2)/u(r,r)
	end do
end do

do i=1,n
print "(9f8.3)", L(i,:)
end do

do i=1,n
print "(9f8.3)", u(i,:)
end do

!回代过程
do i=1,n
    sum=0
    do k=1,i-1
	    sum=sum+L(i,k)*y(k)
	end do
    y(i)=b(i)-sum
end do

do i=n,1,-1
    sum=0
	do k=i+1,n
	    sum=sum+u(i,k)*x(k)
	end do
	x(i)=(y(i)-sum)/u(i,i)
end do

print*,"This is the solution:"
do i=1,n
print*,x(i)
end do
end program