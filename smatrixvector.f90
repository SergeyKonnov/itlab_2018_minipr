program smatrixvector
implicit none
integer n,  m, i, j, k
integer, dimension(50,50)::a
integer, dimension(50)::b, ans

open(1, file="input.txt");
read(1, *) n, m
do i=1,n
    read(1, *) (a(i, j), j=1,m)
enddo
read(1, *) (b(i), i=1,n)

do i=1,m
    ans(i) = 0
    do j=1, n
        ans(i) = ans(i) + a(j, i) * b(j)
    enddo
enddo

write(*, *) (ans(i), i=1,m)

end program smatrixvector
