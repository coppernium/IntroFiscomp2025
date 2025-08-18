        program main
        parameter(idim=1e3)
        dimension M(idim,idim)

                do i = 1,n*(n+1)
                        do j = 1,n+1
                                M(i,-i+j+1)=j
                        end do
                end do
        end program main
