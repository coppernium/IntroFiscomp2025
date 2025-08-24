        program main
                complex*16 meu_ln
                real*8 x,x2,tol

                open(unit=1,file='saida-1-12694394.txt')
                write(1,*) 'x   meu_ln(x)       log(x)'
                tol = 1d-9
                x= 1d0
                do while (x .LT. 10d0)
                        x2 = abs(meu_ln(x,tol))
                        write(1,3) x, x2,dlog(x),abs(x2 - dlog(x))
                        x = x + 0.1d0

                end do
3               format(F16.8,F16.8,F16.8,F16.8)
                close(1)

        end program main


        function meu_ln(x,tol)
        complex*16 meu_ln
        real*8 x,tol,pi
        real*8 x1,rr,ri,k,slk

        x_ini = x   ! guarda valor original

        pi = acos(-1d0)
        slk  = 1d0
        if (x .LT. 1d0) then
                x = 1d0/x
        endif

        k = 1d0
        x1 = x
        do while (x1 .GE. 2d0)
                k = k * 2d0
                x1 = x ** (1d0/k)
        end do

        meu_ln = 0d0
        rr = 1d0
        i = 1
        do while (abs(rr) .GE. tol)
                ri = i
                rr  = -((1d0 - x1)**i)/ri
                meu_ln = meu_ln + rr
                i = i + 1
        end do

        meu_ln = slk * k*meu_ln

        !caso x seja negativo
        if (x_ini .LT. 0d0) then
        meu_ln = meu_ln + (0d0, 1d0)*pi
        endif

        return
        end function meu_ln