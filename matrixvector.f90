program matrixvector
implicit none

integer :: ierror, n[*], m[*], portion[*], i, j, k, tmp, tmp2
integer, dimension(2500,2500), codimension[*]::a
integer, dimension(2500), codimension[*]::b, ans[*]
logical :: init

call MPI_Initialized(init, ierror)
if(.not. init) then
    call MPI_Init(ierror)
endif

if (this_image()==1) then
    open(1, action='read', file='input.txt')
    open(2, action='write', status='replace', file='output.txt' )
    read(1, *) n, m
    do i = 2, num_images()
        n[i] = n
        m[i] = m
    enddo
    do i=1, n
        read(1, *) (a(i,j), j=1,m)
    enddo
    read(1, *) (b(i), i=1,n)
endif

sync all

if(this_image()==1) then
    portion = m/num_images()
    tmp = portion
    do i = 2, num_images()-1
        portion[i] = portion
        do j = tmp+1, tmp+portion
        do k = 1, n
                a(k,j-tmp)[i] = a(k, j)
            enddo
        enddo
        tmp = tmp + portion
    enddo
    if(num_images() /= 1) then
        portion[num_images()] = m-tmp
        do j = tmp+1, m
            do k=1,n
                a(k,j-tmp)[num_images()] = a(k,j)
            enddo
        enddo
    endif
    do i=2,num_images()
        do j=1, n
            b(j)[i]= b(j)
        enddo
    enddo
endif
sync all

do i=1, portion
    ans(i) = 0
    do j=1, n
        ans(i) = ans(i) + a(j,i)*b(j)
    enddo
enddo
sync all
if(this_image() == 1) then
    k = 2
    j = 0
    do i=portion+1,m
        do while(k < num_images() .and. j == portion[k])
            k = k + 1
            j = 0
        enddo
        j = j + 1
        ans(i)[1] = ans(j)[k]
    enddo
    write(*, *) (ans(i), i=1,m)
endif
end program matrixvector
