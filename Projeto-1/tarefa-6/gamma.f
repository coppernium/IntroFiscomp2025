        program main
                read(*,*) d
                write(*,*) vol(1e0,d)
        end program main

        function f(x)
                pi = acos(-1.e0)
                f = 1
                if (mod(x,1e0) .NE. 0) then
                do while (x .GT. 0.5e0)
                        x = x-1
                        f = f * x
                end do
                        f = f * sqrt(pi)
                else
                        i = x
                        do j = 1,i
                                f = f * j
                        end do
                end if
        return
        end function f

        function vol(r,d)
                pi = acos(-1e0)
                
                vol = pi**(d/2e0)*(r**d)/f(1+d/2e0)
        
        return
        end function vol

        function mcarlo(r,d,M)
                parameter(iseed=1154)
                rr = rand(iseed)
                c = 0
                do i = 1,M
                        if ((r*rand())**d .LE. r**d) then
                                c = c + 1          
                        end if
                end do
                mcarlo = (M/c)*(r**d)
        end function mcarlo
