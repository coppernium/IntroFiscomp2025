        program main
            implicit real*8 (a-h,o-z)
            call poincare()
        end program main

        subroutine poincare()
            parameter(imax=1e6)
            implicit real*8 (a-h,o-z)
            dimension w(0:imax), th(0:imax)
            dimension ival(0:imax)
C           Constantes
            pi = acos(-1d0)
            g = 9.81d0
            rl = 9.81d0
            dt = 0.04d0
            c1 = 0.05d0 ! Fator de amorteciemento (gamma)
            c2 = 1.2d0 ! F_0
            c3 = 0.666d0 ! Frequencia angular da força extena

C           Valores iniciais
            w(0) = 0
            th(0) = pi/60d0
            ival(0) = 0
C           Realiza a simulação
            open(unit=7,file='saida-2-12694394.txt')
            write(7,9)

            do i = 0,imax-1
                rr = c3*i*dt
                F = -c1*w(i) + c2*sin(rr)
                w(i+1) = w(i) -(g/rl)*sin(th(i))*dt +F*dt
                th(i+1) = th(i) + w(i+1)*dt
                
                if (abs(mod(rr, pi)) .LE. 1d-3) then
                    ival(i+1) = 1
                    write(7,8) w(i+1),th(i+1)
                else
                    ival(i+1) = 0
                end if

            end do

            close(7)
8           format(F16.8,',',F16.8)
9           format('omega,theta')


C           Salva os dados
            open(unit=1,file='saida-1-12694394.txt')
                write(1,3)
            do i = 0,imax
            ! Aqui eu tenho que eu tenho que fazer o trem do angulo
            if (th(i+1).GT. 2*pi) then
                th(i+1) = th(i+1) -2*pi
            else if (th(i+1) .LT. 0d0) then
                th(i+1) = th(i+1) + 2*pi
            end if


                write(1,2) ival(i),dt*i, w(i),th(i)
            end do
2           format(I2,3(',',F16.8))
3           format('poincare,temp,omega,theta')
            close(1)



        end subroutine poincare