        program main
        implicit real*8 (a-h,o-z)

C       Constantes

        pi = acos(-1d0)

        a = 0d0
        b = 2d0*pi

C       Variáveis

        rval = 0.5d0*(1d0-exp(-2d0*pi))

        open(unit=1,file='saida-1-12694394.txt')

        do i = 2,13
        n = 2**i
        write(1,7) i,trap(a,b,n),simp(a,b,n),boole(a,b,n)
        end do
        write(1,3) rval
3       format(F16.11)
7       format(I3,3(',',F16.12))
        
        close(1)
        end program main

        function f(x)
        implicit real*8 (a-h,o-z)

        f = exp(-x)*sin(x)
        return
        end function f

        function trap(a,b,n)
        implicit real*8 (a-h,o-z)

        h = (b-a)/n
        trap = 0d0
        do i = 1,n
            x = a + (i-1)*h
            rr = 0.5d0*h*(f(x+h)+f(x))
            trap = trap + rr
        end do

        return
        end function trap

        function boole(a,b,n)
        implicit real*8 (a-h,o-z)


        h = (b-a)/n
        boole = 0d0
        do i = 1,n
        x = a+(i-1)*h
        r1 = (2d0/45d0)*h
        r2 =(7d0*f(x-2d0*h)+7d0*f(x+2d0*h))
        r3 = (32d0*f(x-h)+12*f(x+h)+32d0*f(x+h))

        boole = boole + r1*(r2 + r3)/4d0
        end do

        return
        end function boole

        function simp(a,b,n)
        implicit real*8 (a-h,o-z)


        h = (b-a)/n
        simp = 0d0

        do i = 1,n
        x = a+(i-1)*h
        rr = (3d0*h/8d0)*(f(x)+3d0*f(x+h)+3d0*f(x+2d0*h)+f(x+3d0*h))
        simp = simp + rr/3d0
        end do

        return
        end function simp