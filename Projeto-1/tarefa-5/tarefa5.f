        program main
        parameter(idim=1e3)
        dimension M(idim,idim)
        dimension M2(idim,idim)
                open(unit=1,file='fort.1')
                open(unit=2,file='fort.1')
                n = 0
                do i = 1,idim
                read(1,*,END=1)
                        n = n + 1
                end do
1       continue
                close(1)

                do i = 1,n
                        read(2,*) (M(i,j),j=1,n)
                end do

                do i = 1,n*(n+1)
                        do k = i,n+1
                                M(i,k) = n+1-k
                        end do
                end do
        end program main
