        program main
            implicit real*8 (a-h,o-z)
            call poincare_secao()
        end program main

        subroutine poincare_secao()
            parameter(imax=1e6)
            implicit real*8 (a-h,o-z)
            dimension w(0:imax), th(0:imax)

            pi = acos(-1d0)
            g  = 9.81d0
            rl = 9.81d0
            dt = 0.04d0
            c1 = 0.05d0     ! Fator de amorteciemento (gamma)
            c2 = 0.5d0      ! F_0 btw
            c3 = 0.666d0    ! Frequencia angular da força extena

            w(0)  = 0.0d0
            th(0) = pi/60d0

            open(unit=10,file='saida-2-12694394.txt')
            write(10,9)

            do i = 0, imax-1
                t  = i*dt
                rr = c3*t
                F = -c1*w(i) + c2*sin(rr)
                w(i+1)  = w(i) - (g/rl)*sin(th(i))*dt + F*dt
                th(i+1) = th(i) + w(i+1)*dt

                if (th(i+1) .GT. 2*pi) then
                    th(i+1) = th(i+1) - 2*pi
                else if (th(i+1) .LT. 0d0) then
                    th(i+1) = th(i+1) + 2*pi
                end if

                if (abs(mod(c3*t, pi)) .LT. (c3*dt/2d0)) then
                    write(10,8) w(i+1), th(i+1)
                end if
            end do

            close(10)
8           format(F16.8,',',F16.8)
9           format('omega,theta')
            return
        end
