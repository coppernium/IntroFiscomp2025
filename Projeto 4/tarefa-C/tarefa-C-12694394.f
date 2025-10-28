        program main
        call euler_crommer()
        end program main

        subroutine euler_crommer()
            parameter(imax=(3*1e4))
            dimension w1(0:imax), th1(0:imax)
            dimension w2(0:imax), th2(0:imax)

C       Parametros
        g = 9.81e0
        rl = 9.81e0
        rm = 1e0
        pi = acos(-1e0)
        dt = 0.04e0
        ! Parametros da força
        F0_1 = 0.5e0
        F0_2 = 0.5e0
        ome_1 = 0.75e0
        ome_2 = 0.75e0
        y_1 = 0.05e0
        y_2 = 0.05e0

C       Condições iniciais
        ! Velocidades angulares
        w1(0) = 0
        w2(0) = 0
        ! Angulo inicial
        th1(0) = 1e0
        th2(0) = 1e0 + 0.001e0

C       Cálculo
        do i = 0,imax-1
            F1 = - y_1*w1(i) + F0_1*sin(ome_1*i*dt)
            F2 = - y_2*w2(i) + F0_2*sin(ome_2*i*dt)

            w1(i+1) = w1(i) - (g/rl)*sin(th1(i))*dt + F1*dt
            w2(i+1) = w2(i) - (g/rl)*sin(th2(i))*dt + F2*dt

            th1(i+1) = th1(i) + w1(i+1)*dt
            th2(i+1) = th2(i) + w2(i+1)*dt
        end do

C       Salva os resultados
        open(unit=1,file='saida-2-12694394.txt')
        write(1,3)
        do i = 0,imax
        if (th1(i+1) .GT. 2e0*pi) then
            th1(i+1) = th1(i+1) - 2e0*pi
        else if (th1(i+1) .LT. 0e0) then
            th1(i+1) = th1(i+1) + 2e0*pi
        end if

        if (th2(i+1) .GT. 2e0*pi) then
            th2(i+1) = th2(i+1) - 2e0*pi
        else if (th2(i+1) .LT. 0e0) then
            th2(i+1) = th2(i+1) + 2e0*pi
        end if

            r1 = th1(i)
            r2 = th2(i)
            write(1,7) dt*i,w1(i),r1,w2(i),r2,r2-r1
        end do
3       format('temp,omega1,theta1,omega2,theta2,theta2-theta1')
7       format(5(F12.6,','),F12.6)
        close(1)
        end subroutine euler_crommer

