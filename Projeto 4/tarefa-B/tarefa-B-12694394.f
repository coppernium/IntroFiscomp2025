        program main
        call euler_crommer()
        end program main

        subroutine euler_crommer()
        parameter (imax=1e4)
        implicit real*8(a-h,o-z)
        dimension th(0:imax),w(0:imax)

C       Constantes
        rl = 9.81d0
        g = 9.81d0
        m = 1d0
        y = 0.050d0 !Gamma
        F0 = 0.50d0 !Força externa
        ome = 0.75d0 ! Frequência da força externa
        pi = acos(-1d0)
        dt = 0.04d0

C       Valores iniciais
        w(0) = 0
        th(0) = pi/3d0

C       Cálculo

        do i = 0,imax-1
                F_ex = -y*w(i) + F0*sin(ome*i*dt)
                w(i+1) = w(i)-(g/rl)*sin(th(i))*dt + F_ex*dt
                th(i+1) = th(i) + w(i+1)*dt
        end do

C       Salva a posição
        open(unit=1,file='saida-1-12694394.txt')
        do i =0,imax
        ! Trem das posições
        if (th(i+1) .GT. 2*pi) then
                th(i+1) = th(i+1) - 2*pi
        else if (th(i+1) .LT. 0 ) then
                th(i+1) = th(i+1) + 2*pi
        end if

                write(1,7) i*dt, w(i), th(i)
        end do
7       format(F12.6,F12.6,F12.6)
        close(1)
        end subroutine euler_crommer

C----------------------------------------------------------------

        subroutine calc()

        end subroutine calc