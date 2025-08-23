        program main
                parameter(M=1e6)
                read(*,*) id
        x = carlo(1e0,id,M)
        write(*,*) x
        end program main

        function carlo(r,id,M)
                c = 0
                do i = 1,M
                        p = 0
                        do j = 1,id
                                p = p + (2e0*rand() -1e0)**2
                        end do
                        
                        if (p .LT. r*r) then
                                c = c + 1
                        end if
                end do

                carlo = (c/M)*((2*r)**id)
        end function carlo
