        program main
        implicit real*8 (a-h,o-z)
        
        call euler_cromer()
        
        end program main
        
        subroutine euler_cromer()
        parameter(imax=1e3)
        implicit real*8 (a-h,o-z)
        dimension w(0:imax),th(0:imax)
        
        ! Constantes
        rl = 9.81d0
        g = 9.81d0
        pi = acos(-1d0)
        dt = 0.01d0
        
        ! Valores iniciais
        w(0) = 0d0
        th(0) = pi/60d0
        
        ! Realiza as contas
        
        do i = 0,imax-1
            w(i+1) = w(i) - (g/rl)*sin((th(i)))*dt
            th(i+1) = th(i) + w(i+1)*dt
        end do
        
        ! Salva os dados
        
        open(unit=1,file="saida.txt")
        
        do i = 0,imax-1
        if (th(i+1) .GT. 2*pi) then
        th(i+1) = th(i+1) -2*pi
        else if (th(i+1) .LT. 0d0) then
        th(i+1) = th(i+1) - 2*pi
        end if
        
        write(1,7) i*dt,w(i),th(i)
        
        end do
        
        
7       format(F16.8,2(",",F16.8))
        close(1)
        end subroutine euler_cromer