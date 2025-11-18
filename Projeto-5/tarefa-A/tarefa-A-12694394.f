        program main
        implicit real*8 (a-h,o-z)
        call calc2()
        end program main

        subroutine calc()
        implicit real*8(a-h,o-z)
        parameter (imax = 1e4)
        dimension x(-1:imax),y(-1:imax),r(0:imax)
        
        ! Constantes
        pi = acos(-1d0)
        Gm = 4*pi*pi
        r0 = 1d0
        ! Parâmetros
        dt = 0.01d0
        x(0) = 0d0
        y(0) = 0d0
        r(0) = 1d0
        vx0 = 0d0
        vy0 = 2d0

        x(-1) = -vx0*dt + x(0) 
        y(-1) = -vy0*dt + y(0)

        do i = 0,imax-1
            write(*,*) r(i)

            ax = Gm*x(i)/r(i)**3
            x(i+1) = 2*x(i) - x(i-1) + ax*dt*dt

            ay = Gm*y(i)/r(i)**3
            y(i+1) = 2*y(i) - y(i-1) + ay*dt*dt
        
            r(i+1) = sqrt(x(i)**2 + y(i)**2)

        end do

        ! Salva os dados

        open(unit=1,file='saida-1-12694394.txt')
        
        write(1,3)
        do i = 0,imax
            write(1,2) dt*i,x(i),y(i)
        end do 
2       format(F16.8,2(',',F16.8))
3       format('t,x,y')
        close(1)
        end subroutine calc

        subroutine calc2()
        implicit real*8 (a-h,o-z)
        end subroutine calc2