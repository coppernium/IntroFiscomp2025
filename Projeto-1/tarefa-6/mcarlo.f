        program main
                parameter(M=1e8)
                real*8 x,carlo

                do i =1,25
                write(2,*) i,carlo(1d0,i,M)
                end do
        end program main

        real*8 function carlo(r,id,M)
                real*8 c,r,p,rM
                c = 0
                rM=M
                do i = 1,M
                        p = 0
                        do j = 1,id
                                p = p + (2d0*rand() -1d0)**2
                        end do
                        
                        if (p .LT. r*r) then
                                c = c + 1d0
                        end if
                end do

                carlo = (c/rM)*((2d0*r)**id)
        end function carlo
