program tarefa3
implicit none
logical::ok
integer::k,i,er=0,cnt=-1 !-eof
real::sda,sdp,vara,varp,meana,meanp,smeana,smeanp,svara,svarp,ssda,ssdp,height,weight,aux1=0,aux2=0,aux3=0,aux4=0
real,dimension(1000)::r
integer,dimension(1000)::a
integer,parameter::n=1000,u=44
!verificando existencia arquivo
inquire(file='internados.csv',exist=ok)
if(.not.ok) then
print*,'Erro, arquivo nao existe,terminando...'
stop
end if
open(u,file='internados.csv')
do while(er.ge.0)
read(u,*,iostat=er)
cnt=cnt+1
end do
print*,cnt
close(u)
1
!"randomizacao" [0,10000]
call random_number(r)
a=cnt*r
!populacional
open(u,file='internados.csv',form='formatted')
do i=1,cnt
read(u,'(59X,I3,1X,I6)')height,weight
aux1=aux1+height
aux2=aux2+weight
end do
aux2=aux2/1000.00
meana=aux1/cnt
meanp=aux2/cnt
close(u)
open(u,file='internados.csv',form='formatted')
do i=1,cnt
read(u,'(59X,I3.0,1X,I6)')height,weight
aux3=aux3+(height-meana)**2
aux4=aux4+(weight/1000.00-meanp)**2
end do
close(u)
vara=aux3/cnt
varp=aux4/cnt
sda=sqrt(vara)
sdp=sqrt(varp)
!amostral
open(u,file='internados.csv',form='formatted')
aux1=0;aux2=0;
do i=1,cnt
cnt=0
do k=1,n
if(i==a(k)) then
read(u,'(59X,I3.0,1X,I6)')height,weight
aux1=aux1+height
aux2=aux2+weight
cnt=cnt+1
end if
end do
if(cnt>0) then
read(u,'(59X,I3.0,1X,I6)')
end if
end do
close(u)
aux2=aux2/1000.00
smeana=aux1/n
smeanp=aux2/n
open(u,file='internados.csv',form='formatted')
do i=1,cnt
cnt=0
do k=1,n
if(i==a(k)) then
read(u,'(59X,I3.0,1X,I6)')height,weight
aux3=aux3+(height-smeana)**2
aux4=aux4+(weight/1000.00-smeanp)**2
cnt=cnt+1
end if
end do
if(cnt>0) then
read(u,'(59X,I3.0,1X,I6)')
Page 2
end if
end do
close(u)
svara=aux3/n
svarp=aux4/n
ssda=sqrt(svara)
ssdp=sqrt(svarp)
print*,'media altura(cm)',meana
print*,'media peso(kg)',meanp
print*,'var altura(cm)',vara
print*,'var peso(kg)',varp
print*,'desvio padrao altura(cm)',sda
print*,'desvio padrao peso(kg)',sdp
print*,'media amostral altura(cm)',smeana
print*,'media amostral peso(kg)',smeanp
print*,'variancia amostral altura(cm)',svara
print*,'variancia amostral peso(kg)',svarp
print*,'desvio padrao amostral altura',ssda
print*,'desvio padrao amostral peso',ssdp
end program tarefa3
