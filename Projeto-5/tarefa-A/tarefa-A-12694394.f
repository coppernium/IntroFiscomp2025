        program main
        parameter (imax=2e4)
        implicit real*8 (a-h,o-z)
        dimension x(-1:imax),y(-1:imax)

C   Constantes
        pi = acos(-1d0)
        dt = 0.1d0/365d0
        GM = 4*pi*pi

C   Valores iniciais
        x(0) = 1d0
        y(0) = 0d0
        vx = 0d0
        vy = 2.0d0 * pi

C ----- Ace inicial
        r = sqrt(x(0)**2 + y(0)**2)
        ax = -GM*x(0)/r**3
        ay = -GM*y(0)/r**3

C ----- Pos anterior via Taylor reverso
        x(-1) = x(0) - vx*dt + 0.5d0*ax*dt*dt
        y(-1) = y(0) - vy*dt + 0.5d0*ay*dt*dt


C   Cálculo
        do i = 0,imax-1
        r = sqrt((x(i)**2) + (y(i)**2))
        
        ax = - GM*x(i)/(r**3)
        ay = - GM*y(i)/(r**3)
        
        x(i+1) = 2d0*x(i) - x(i-1) + ax*dt*dt
        y(i+1) = 2d0*y(i) - y(i-1) + ay*dt*dt


        end do

C   Salva
        open(unit=1,file='saida.txt')
        do i = 0,imax

        write(1,2) dt*i,x(i),y(i)

        end do
2       format(F16.8,2(",",F16.8))
        close(1)

        end program main