integer :: ierror, n, a(50)[*], summ[*]
logical :: init

call MPI_Initialized(init, ierror)
if(.not. init) then
    call MPI_Init(ierror)
endif

n = 10
do i=1, n
    a(i) = i*this_image()
enddo

summ = 0
do i=1, n
    summ = summ + a(i)
enddo

sync all

if(this_image()==1) then
    do i=2, num_images()
        summ = summ + summ[i]
    enddo
endif

sync all
if(this_image()==1) then
    print *,summ
endif
end
