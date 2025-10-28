        program main
        implicit real*8 (a-h,o-z)
        call euler_crommer()
        call euler()
        end program main

        subroutine euler_crommer()
        implicit real*8 (a-h,o-z)
        parameter (imax = 1e3)
        dimension w(0:imax), th(0:imax)

        g = 9.81d0
        m = 1d0
        rl = 9.81d0
        dt = 0.01d0
        pi = acos(-1d0)

C       Condições Iniciais
        w(0) = 0d0
        th(0) = pi/6



        do i = 0, imax-1
                w(i+1) = w(i) - (g/rl)*sin(th(i))*dt
                th(i+1) = th(i) + w(i+1)*dt

        end do

        open(unit=1,file='saida-1-12694394.txt')
        do i = 0, imax
                
                ! Trem do angulo
                if (th(i+1) .gt. 2*pi) then
                th(i+1) = th(i+1) - 2*pi
                else if (th(i+1) .lt. 0d0) then
                th(i+1) = th(i+1) + 2*pi
                end if

                write(1,*) i*dt, w(i), th(i)
        end do
        close(1)


        open(unit=2,file='saida-2-12694394.txt')
        do i = 0, imax
                E_kin = 0.5d0*m*((rl*w(i))**2) 
                E_pot = -m*g*rl*cos(th(i))
                

                write(2,*) i*dt, E_kin,E_pot,E_kin+E_pot
        end do
        close(2)

        end subroutine euler_crommer



        subroutine euler()
        implicit real*8 (a-h,o-z)
        parameter (imax = 1e4)
        dimension w(0:imax), th(0:imax)

        g = 9.81d0
        m = 1d0
        rl = 9.81d0
        dt = 0.01d0
        pi = acos(-1d0)

C       Condições Iniciais
        w(0) = 0d0
        th(0) = pi/6d0



        do i = 0, imax-1
                w(i+1) = w(i) - (g/rl) * sin(th(i)) * dt
                th(i+1) = th(i) + w(i) * dt
        end do

        open(unit=3,file='saida-3-12694394.txt')
        do i = 0, imax

                ! Trem dos angulos
                if (th(i+1) .gt. 2*pi) then
                th(i+1) = th(i+1) - 2*pi
                else if (th(i+1) .lt. 0d0) then
                th(i+1) = th(i+1) + 2*pi
                end if
        
                write(3,*) i*dt, w(i), th(i)
        end do
        close(3)


        open(unit=4,file='saida-4-12694394.txt')
        do i = 0, imax
                E_kin = 0.5d0*m*((rl*w(i))**2) 
                E_pot = -m*g*rl*cos(th(i))
                
                write(4,*) i*dt, E_kin,E_pot,E_kin+E_pot
        end do
        close(4)

        end subroutine euler
