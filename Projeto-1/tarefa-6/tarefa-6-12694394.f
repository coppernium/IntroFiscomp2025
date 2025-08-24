        program main
                parameter(M=1e7)
                real*8 d,r,vol,carlo,x1,x2
                r = 1d0

                do i =2,4
                d = i
                x1= vol(r,d) 
                x2 = carlo(r,i,M)
                        write(*,*) i,x1,x2,abs(x1-x2)
                end do
7               format(I3,F12.8,F12.8)

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
