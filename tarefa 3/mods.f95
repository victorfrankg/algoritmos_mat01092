module matmat
implicit none
integer,parameter :: real64=selected_real_kind(15,307)
integer,parameter::tipo = real64
contains 
subroutine matmat_ijk (a,b,c,n)
integer,intent(in)::n
real(tipo),dimension(n,n),intent(in)::a
real(tipo),dimension(n,n),intent(in)::b
real(tipo),dimension(n,n),intent(inout)::c
integer:: i,j,k
	c = 0.0
    do i=1,n
      do j=1,n
        do k=1,n
          c(i,j)= c(i,j)+a(i,k)*b(k,j)
        end do
      end do
    end do
end subroutine matmat_ijk

subroutine matmat_jki (a,b,c,n)
integer,intent(in)::n
real(tipo),dimension(n,n),intent(in)::a
real(tipo),dimension(n,n),intent(in)::b
real(tipo),dimension(n,n),intent(inout)::c
integer:: i,j,k
	c=0.0
    do j=1,n
      do k=1,n
        do i=1,n
          c(i,j) = c(i,j)+a(i,k)*b(k,j)
        end do
      end do
    end do
end subroutine matmat_jki

subroutine matmat_kji (a,b,c,n)
integer,intent(in)::n
real(tipo),dimension(n,n),intent(in)::a
real(tipo),dimension(n,n),intent(in)::b
real(tipo),dimension(n,n),intent(inout)::c
integer::i,j,k
	c=0.0
    do k=1,n
      do j=1,n
        do i=1,n
          c(i,j) = c(i,j)+a(i,k)*b(k,j)
        end do
      end do
    end do
end subroutine matmat_kji
end module matmat 