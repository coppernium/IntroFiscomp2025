        program ex6
        real*8 calc_vol,r,g
        read(*,*) id

        write(*,*) g(id)
        end program ex6

        function g(i)
        real*8 g

        g = 1

        val = 1 + i/2d0
        ! Esse é o caso em que gamma é igual a fatorial
        if (mod(val,1d0) .EQ. 0) then
                ival = val
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

        return
        end function g

