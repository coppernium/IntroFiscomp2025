        program main
        implicit real*8 (a-h,o-z)

        open(unit=1,file='saida-1-12694394.txt')
        write(1,3)

        x = 0.5d0
        do i = 1,12
            h = 5d0**(-i)
        write(1,7) i,f1(x,h),f2(x,h),f3(x,h),f4(x,h),f5(x,h),f6(x,h)
        end do
        write(1,3)

7       format('|',I3,6('|',F16.12),'|')
3       format(107('-'))
        close(1)
        end program main

        function f(x)
        real*8 f,x
        
        f = exp(2d0*x*x)*tanh(2d0*x)
        return
        end function f

        function f1(x,h)
        implicit real*8 (a-h,o-z)
        
        f1 = (f(x+h)-f(x))/h

        return
        end function f1

        function f2(x,h)
        real*8 f2,f,x,h

        f2 = (f(x)-f(x-h))/h

        return
        end function f2

        function f3(x,h)
        implicit real*8 (a-h,o-z)

        f3 = (f(x + h) - f(x - h))/(2d0*h)

        return
        end function f3

        function f4(x,h)
        implicit real*8 (a-h,o-z)

        f4 = (f(x-2d0*h)-8d0*f(x-h)+8d0*f(x+h)-f(x+2d0*h))/(12d0*h)
        return
        end function f4

        function f5(x,h)
        implicit real*8 (a-h,o-z)

        f5 = (f(x+h) -2*f(x) + f(x-h))/(h*h)

        return
        end function f5

        function f6(x,h)
        implicit real*8 (a-h,o-z)

        f6 =-f(x-2*h)+16*f(x-h)-30*f(x)+16*f(x+h)-f(x+2*h)
        f6=f6/(12*h*h)
        return
        end function f6