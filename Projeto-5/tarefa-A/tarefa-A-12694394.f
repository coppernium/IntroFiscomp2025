        program main
        implicit real*8 (a-h,o-z)
        a = 39.53d0
        call calc(a)
        end program main


        subroutine calc(a)
        parameter (imax=1e5)
        implicit real*8 (a-h,o-z)
        dimension x(-1:imax),y(-1:imax)

C   Constantes
        pi = acos(-1d0)
        dt = 10d0/365d0
        GM = 4*pi*pi

C   Val in
        x(0) = 1d0*a !Distancia em UA
        y(0) = 0d0
        vx = 0d0
        vy = 2.0d0*pi/sqrt(a)


C  x(-1) e y(-1)
        x(-1) = x(0) - vx*dt
        y(-1) = y(0) - vy*dt 

C       Salva para lei de Kepler
        io = 0d0 !Variavel de segurança para pegar apenas a primeira volta

C   Calc
        do i = 0,imax-1
        r = sqrt((x(i)**2) + (y(i)**2))
        
        ax = - GM*x(i)/(r**3)
        ay = - GM*y(i)/(r**3)
        
        x(i+1) = 2d0*x(i) - x(i-1) + ax*dt*dt
        y(i+1) = 2d0*y(i) - y(i-1) + ay*dt*dt

        theta_new = atan2(y(i+1), x(i+1))
        if ((theta_old .LT. 0d0) .and. (theta_new .GE. 0d0)) then
                if (io .GT. 0d0) then
                        goto 7
                end if

                if (i*dt .GT. 5d0*dt) then      ! para n detectar o comeco
                        periodo = i * dt
                        write(*,3) periodo, r, (periodo**2)/(r**3)
3                       format(F12.4,2(",",F12.4))
                end if
                io = 1d0
        end if

        theta_old = theta_new
7       continue
        end do

C   Salva
        open(unit=1,file='saida-2-12694394.txt')
        do i = 0,imax

        write(1,2) dt*i,x(i),y(i)

        end do
2       format(F16.8,2(",",F16.8))
        close(1)


        end subroutine calc