        program main
                real*8 d,r,vol
                r = 1.1d0

                open(unit=1,file='saida-3-12694394.txt')
                do i =0,25
                d = i
                        write(1,7) i,vol(r,d) 
                end do
                close(1)
7               format(I3,F12.8)

        end program main

        real*8 function f(x)
                real*8 x,pi
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

        real*8 function vol(r,d)
                real*8 r,d,pi,f
                pi = acos(-1d0)
                vol = (pi**(d/2d0))*(r**d)/f(1d0+(d/2d0))
        return
        end function vol

