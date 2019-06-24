program t2
use matmat
implicit none
integer::i,k,rslt
real(tipo),allocatable::a(:,:),b(:,:),c(:,:)
real(tipo)::deb,fin
integer,dimension(5):: z=(/(2**i,i=10,14)/)
7 format(A,F10.3,A)
	do k=1,size(z)
      allocate(a(z(k),z(k)),b(z(k),z(k)),c(z(k),z(k)),stat=rslt)
      if (rslt/=0) then
        exit
      end if
      print*,"n=",z(k)
      
      call random_number(a)
      call random_number(b)
      !produto interno
      call cpu_time(deb)  
      call matmat_ijk (a,b,c,z(k))
      call cpu_time(fin)
      write(unit=6,fmt=7)"internal product: it took",fin-deb,"secs"
      !soma e mult
      call cpu_time(deb)  
      call matmat_jki (a,b,c,z(k))
      call cpu_time(fin)
      write(unit=6,fmt=7)"sum and mult: it took",fin-deb,"secs"
      !produto externo
      call cpu_time(deb)  
      call matmat_kji (a,b,c,z(k))
      call cpu_time(fin)
      write(unit=6,fmt=7)"external product: it took",fin-deb,"secs"
      deallocate(a,b,c)
    end do
    
end program t2