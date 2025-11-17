        program main
        implicit real*8 (a-h,o-z)
        end program main

        subroutine calc()
        implicit real*8(a-h,o-z)
        parameter (imax = 1e4)
        dimension x(-1:imax),y(-1:imax)
C        dimension vx(0:imax),vy(0:imax)
        
        ! Constantes
        pi = acos(-1d0)
        Gm = 4*pi*pi
        ! Parâmetros
        dt = 0.01d0
        x(0) = 0d0
        y(0) = 0d0
        vx0 = 20d0
        vy0 = 03d0

        x(-1) = -vx0*dt + x(0) 
        y(-1) = -vy0*dt + y(0)

        do i = 0,imax-1
            r = sqrt(x(i)**2 + y(i)**2)
            
            ax = Gm*x(i)/r**3
            x(i+1) = 2*x(i) - x(i-1) + ax*dt*dt

            ay = Gm*y(i)/r**3
            y(i+1) = 2*y(i) - y(i-1) + ay*dt*dt
        
        end do

        ! Salva os dados

        open(unit=1,file='saida-1-12694394.txt')

        do i = 0,imax
            write(1,2) dt*i,x(i),y(i)
        end do 
2       format(F16.8,2(',',F16.8))
        close(1)
        end subroutine calc