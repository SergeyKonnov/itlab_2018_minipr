integer :: ierror, n, a(50)[*], summ[*], portion, port[*], tmp, i, j
logical :: init

call MPI_Initialized(init, ierror)
if(.not. init) then
    call MPI_Init(ierror)
endif

if(this_image()==1) then
    open(2, file='input.txt')
    read (2,*) n
    do i = 1, n
        read(2, *) a(i)
    enddo

    portion = n/num_images()
    port[1] = portion
    tmp = portion

    do i = 2, num_images()-1
        port[i] = portion
        do j = 1, portion
            a(j)[i] = a(j+tmp)
        enddo
        tmp = tmp + portion
    enddo
    if(num_images() .NE. 1) then
        port[num_images()] = n-tmp
        do i = tmp + 1, n
            a(i-tmp)[num_images()] = a(i)
        enddo
    endif
endif

sync all

summ = 0
do i=1, port
    summ = summ + a(i)
enddo

sync all

if(this_image() == 1) then
    do i = 2, num_images()
        summ = summ + summ[i]
    enddo
endif

if(this_image() == 1) then
    print *, summ
endif

!if(this_image()==1) then
!    do i=1, num_images()
!        print *, i, ':'
!        do j=1, port[i]
!            print *, a(j)[i]
!        enddo
!        print *, '------------'
!    enddo
!endif
end
