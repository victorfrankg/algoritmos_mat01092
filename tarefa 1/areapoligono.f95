program ousadiapoligono
implicit none
character(len=12):: name
integer::i,m,n
integer,dimension(:),allocatable::x,y
integer,dimension(:),allocatable::z
print*, 'nome do arquivo:'
read*, name
open(unit=10,file=name,form='unformatted',status='new')

read*,n
m=2*n
allocate(x(n))
allocate(y(n))
allocate(z(m))
do i=1,n
  print*,'insira os vertices(x)'
  read*,x
end do
do i=1,n
  print*,'insira os vertices(y)'
  read*,y
end do
z(1:m-2:2)=x(1:n-1)*y(2:n)
z(2:m-2:2)=x(2:n)*y(1:n-1)
z(m-1)=x(n)*y(1)
z(m)=x(1)*y(n)

write(10) z

print*, 'fim da criação do arquivo'
close(10)
end program ousadiapoligono




