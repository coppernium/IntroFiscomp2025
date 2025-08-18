        program main
        real*8 g,pi

        pi = acos(-1d0)
        g = 1
        i = 3

        val = 1 + i/2d0
        ival = val
        ! Esse é o caso em que gamma é igual a fatorial
        if (mod(val,1d0) .EQ. 0) then
                do j=1,ival
                        g = g*j
                end do
        ! caso em que i é impar 
        else
                do while (val .GT. 0.5d0)
                        val = val - 1
                        g = g * val
                end do
                g = g * sqrt(pi)

        end if

        write(*,*) g
        end program main
