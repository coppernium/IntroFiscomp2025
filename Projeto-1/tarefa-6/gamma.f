        program main
                do i =1,100
                d = i
                write(1,*) i,vol(1e0,d)
                end do

        end program main

        function f(x)
                pi = acos(-1.e0)
                f = 1e0
                if (mod(x,1e0) .NE. 0e0) then
                        do while (x .GT. 0.5e0)
                                x = x-1e0
                                f = f * x
                        end do
                                f = f * sqrt(pi)
                else
                        i = x
                        do j = 1,i-1
                                f = f * j
                        end do
                end if
        return
        end function f

        function vol(r,d)
                real*4 r,d
                pi = acos(-1e0)
                vol = (pi**(d/2e0))*(r**d)/f(1e0+(d/2e0))
        return
        end function vol

