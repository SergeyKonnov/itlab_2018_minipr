program matrixvector
implicit none

include 'mpif.h'

integer::ierror
integer :: n[*], m[*], portion[*], i, j, k, tmp, tmp2, begin
integer, dimension(:), codimension[:], allocatable ::a, b, ans
!integer, dimension(25000), codimension[*] :: a, b, ans
integer, dimension(:), allocatable :: tmparr
logical::init
character(100)::num1char, num2char
real::t1, t2
call MPI_Initialized(init, ierror)
if(.not. init) then
    call MPI_Init(ierror)
endif

if (this_image()==1) then
    call get_command_argument(1, num1char)
    call get_command_argument(2, num2char)
    read(num1char, '(I10)') n
    read(num2char, '(I10)') m
endif

sync all

n = n[1]
m = m[1]

if(this_image() - 1 < mod(m, num_images())) then
    portion = m/num_images()+1
else
    portion = m/num_images()
endif

! allocate(a((100500))[*])
! allocate(b(100500)[*])
! allocate(ans(100500)[*])
allocate(a(m*n)[*])
allocate(b(n)[*])
allocate(ans(m)[*])

sync all

if(this_image() == 1) then
    do i = 1, n*m
        a(i) = i
    enddo
    do i = 1, n
        b(i) = i
    enddo
endif

sync all
t1 = MPI_Wtime()
begin = 0
if(this_image() - 1 < mod(m, num_images())) then
    begin = (this_image()-1)*portion
else
    begin = mod(m, num_images())*(portion+1) + (this_image()-mod(m, num_images())-1)*portion
endif

do i=0,portion-1
    do j=1,n
        a(i*n+j) = a((begin+i)*n+j)[1]
    enddo
enddo

do i = 1,n
    b(i) = b(i)[1]
enddo


! вычисление
do i=1, portion
    ans(i) = 0
    do j=1, n
        ans(i) = ans(i) + a((i-1)*n+j)*b(j)
    enddo
enddo

sync all
! сбор данных
if(this_image() == 1) then
    tmp = 2
    k = 0
    do i = portion+1,m
        k = k + 1
        ans(i) = ans(k)[tmp]
        if(tmp - 1 < mod(m, num_images())) then
            if(k == m/num_images()+1) then
                tmp = tmp + 1
                k = 0
            endif
        else
            if(k == m/num_images()) then
                tmp = tmp + 1
                k = 0
            endif
        endif
    enddo
    ! write(*, *) (ans(i), i=1,m)
endif
t2 = MPI_Wtime() 
if(this_image() == 1) then
    write(*, *) t2-t1
endif
deallocate(a)
deallocate(b)
deallocate(ans)
! print *, this_image(), " end program"
! call MPI_Finalize(ierror)
end program matrixvector
