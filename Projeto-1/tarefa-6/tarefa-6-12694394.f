        program ex6
        real*8 calc_vol,r
        id = 4        
        r = 1

        write(*,*) calc_vol(id,r)
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

        function calc_vol(i,r)
                real*8 r,pi,calc_vol,g
                pi = acos(-1d0)
                
                calc_vol = (pi**(i/2))*(r**i)/g(i) 

                return
        end function calc_vol

        function mcarlo(i,r)
                real*8 r

                
        end function mcarlo
