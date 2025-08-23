        program ex6
                parameter(M=1e3)
                r = 3e0
                open(2,file='ex6.dat')
                write(1,*) '#MC','Gamma','|MC-Gamma|'
                do i = 1,100
                        d = i
                        x1 = carlo(r,d,M)
                        x2 = vol(r,d)
                        write(2,*) x1,x2,abs(x1-x2)
                end do
                close(2)
        end program ex6


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

        function carlo(r,d,M)
                c = 0
                id = d
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
