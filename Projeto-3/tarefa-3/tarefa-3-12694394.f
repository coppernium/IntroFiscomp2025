        program main
        implicit real*8 (a-h,o-z)

C   Constantes
        a = 0d0
        b = 5d0
        tol = 1d-6

C   Chama os métodos
        write(*,1) 'Bisseção: ', bissec(a,b,tol)
        write(*,1) 'Newton-Raphson: ', raphson(a,tol)
        write(*,1) 'Secante: ', secante(a,tol)

1       format(A15,F16.12)       

        end program main

        function f(x)
        implicit real*8 (a-h,o-z)
        f = 0.042d0 - 0.13d0*x - 0.6d0*x*x + x**3
C        f = x*x - 1
        return
        end function f

        function df(x)
        implicit real*8 (a-h,o-z)
        df = -0.13d0 - 1.2d0*x + 3d0*x*x
C        df = 2*x
        return
        end function df

        function bissec(a,b,tol)
        implicit real*8 (a-h,o-z)

C   Loop inicial para encontrar intervalo com troca de sinal
        stp = 0.01d0
        n = int((b-a)/stp)
        do i = 1,n
                if (f(a)*f(a+stp) .LT. 0d0) then
                b = a + stp
                exit
                end if
                a = a + stp
        end do

C       Arquivo de saída
        open(unit=13,file='saida-1-12694394.txt')

C        Loop principal da bisseção
        io = 0
        do while (abs(b-a) .GT. tol)
                c = (a + b)/2d0
                write(13,14) io,c
                if (f(a)*f(c) .LT. 0d0) then
                b = c
                else
                a = c
                end if
                io = io + 1
        end do

        close(13)
        bissec = (a+b)/2d0
14      format(I3,F16.12)
        return
        end function bissec

        function raphson(x,tol)
        implicit real*8 (a-h,o-z)

        io = 0
        rr = 1d0
        open(unit=15,file='saida-2-12694394.txt')

        do while(abs(rr) .GT. tol)
                write(15,16) io,x
                rr = f(x)/df(x)
                x = x - rr
                io = io + 1
        end do

        close(15)
        raphson = x
16      format(I3,F16.12)
        return
        end function raphson

        function secante(x,tol)
        implicit real*8 (a-h,o-z)
C         Constantes
        stp = 0.01
C         Váriaveis
        rr = 1d0
C
        io = 0
        open(unit=17,file='saida-3-12694394.txt')

        do while(abs(rr).GT.tol)
                write(17,18) io,x
                rr = f(x)*(x - (x-stp))/(f(x) - f(x-stp))
                x = x - rr
                io = io + 1
        end do
        secante = x
18      format(I3,F16.12)
        close(17)
        return
        end function secante