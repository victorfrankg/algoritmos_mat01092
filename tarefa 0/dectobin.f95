program dec_to_bin
    implicit none
    integer dec,count,i,bin(100),rep
    !Leitura
    write (*,*) "Insira o numero decimal:"
    read (*,*) dec
    write (*,*) "O valor decimal:",dec
    
    count=0
       do i=1,100
         if (mod(dec,2)==0) then
         	bin(i)=0
            else 
            bin(i)=1
        end if
        dec=dec/2 !Entroncamento
        count=count+1     
        if (dec==0) then
            exit
        end if    
    end do
    write(*,*) "O valor binario:"
    write(*,*)(bin(i),i=count,1,-1)
    write(*,*) 
    !maior numero será 2**31-1
end program dec_to_bin