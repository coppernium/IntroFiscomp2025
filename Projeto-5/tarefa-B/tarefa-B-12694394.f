        program main
        implicit real*8 (a-h,o-z)

        call calc()
        call calc2()

        end program main

        subroutine calc()
        implicit real*8 (a-h,o-z)
        parameter (imax=5e3)

        dimension x_t(-1:imax), y_t(-1:imax)
        dimension x_j(-1:imax), y_j(-1:imax)

C       Constantes

        pi = acos(-1d0)
        dt = 1d0/365d0

        GM_S = 4d0*pi*pi
        GM_J = GM_S * 0.0009543d0 * 1   ! multiplica a massa
        GM_T = GM_S * 3.003d-6

C
        GM_S2 = GM_S + GM_J

C       C.I.

        x_t(0) = 1d0
        y_t(0) = 0d0
        vx_t  = 0d0
        vy_t  = 2d0*pi

        x_j(0) = 5.2d0
        y_j(0) = 0d0
        vx_j  = 0d0
        vy_j  = 2d0*pi*5.2d0/11.86d0

C       Intereaçao negativa
        x_t(-1) = x_t(0) - vx_t*dt
        y_t(-1) = y_t(0) - vy_t*dt

        x_j(-1) = x_j(0) - vx_j*dt
        y_j(-1) = y_j(0) - vy_j*dt

C       Contas
        do i = 0, imax-1

C       Acel. Terra
        r_ts = sqrt(x_t(i)**2 + y_t(i)**2)
        r_tj = sqrt((x_t(i)-x_j(i))**2 + (y_t(i)-y_j(i))**2)

        ax_t = -GM_S*x_t(i)/(r_ts**3) - GM_J*(x_t(i)-x_j(i))/(r_tj**3)
        ay_t = -GM_S*y_t(i)/(r_ts**3) - GM_J*(y_t(i)-y_j(i))/(r_tj**3)

C       Acel jupiter
        r_js = sqrt(x_j(i)**2 + y_j(i)**2)

        ax_j = -GM_S2*x_j(i)/(r_js**3)
        ay_j = -GM_S2*y_j(i)/(r_js**3)

C       atualiza os dados
        x_t(i+1) = 2d0*x_t(i) - x_t(i-1) + ax_t*dt*dt
        y_t(i+1) = 2d0*y_t(i) - y_t(i-1) + ay_t*dt*dt

        x_j(i+1) = 2d0*x_j(i) - x_j(i-1) + ax_j*dt*dt
        y_j(i+1) = 2d0*y_j(i) - y_j(i-1) + ay_j*dt*dt

        end do

C       Salva

        open(unit=1, file='saida_m_1.txt')
        do i = 0, imax
        write(1,2) dt*i, x_t(i), y_t(i), x_j(i), y_j(i)
        end do
2       format(F16.8, 4(",",F16.8))
        close(1)

        end subroutine calc


        subroutine calc2()
        implicit real*8 (a-h,o-z)
        parameter (imax=1e4)

        dimension xj(-1:imax), yj(-1:imax)
        dimension xa1(-1:imax), ya1(-1:imax)
        dimension xa2(-1:imax), ya2(-1:imax)
        dimension xa3(-1:imax), ya3(-1:imax)

C       Constantes

        pi   = acos(-1d0)
        dt   = 1d0/365d0

        GM_S = 4d0*pi*pi
        GM_J = GM_S * 0.0009543d0
        GM_S2 = GM_S + GM_J !Isso é para jupiter sentir a mudamça de massa

C       Cond Ini

C       Jupiter
        xj(0) = 5.2d0
        yj(0) = 0d0
        vxj   = 0d0
        vyj   = 2d0*pi*5.2d0/11.86d0

C       Asteroide 1
        xa1(0) = 3.0d0
        ya1(0) = 0d0
        vxa1   = 0d0
        vya1   = 3.628d0

C       Asteroide 2
        xa2(0) = 3.276d0
        ya2(0) = 0d0
        vxa2   = 0d0
        vya2   = 3.471d0
C       Asteroide 3
        xa3(0) = 3.700d0
        ya3(0) = 0d0
        vxa3   = 0d0
        vya3   = 3.267d0

C       Setup do i-1 do verlat

        xj(-1) = xj(0) - vxj*dt
        yj(-1) = yj(0) - vyj*dt

        xa1(-1) = xa1(0) - vxa1*dt
        ya1(-1) = ya1(0) - vya1*dt

        xa2(-1) = xa2(0) - vxa2*dt
        ya2(-1) = ya2(0) - vya2*dt

        xa3(-1) = xa3(0) - vxa3*dt
        ya3(-1) = ya3(0) - vya3*dt

C       Realiza as contas
        do i = 0, imax-1

C       Jupiter
        rjs = sqrt(xj(i)**2 + yj(i)**2)
        axj = -GM_S2*xj(i)/(rjs**3)
        ayj = -GM_S2*yj(i)/(rjs**3)

C       teroide 1
        ras = sqrt(xa1(i)**2 + ya1(i)**2)
        raj = sqrt((xa1(i)-xj(i))**2 + (ya1(i)-yj(i))**2)

        axa1 = -GM_S*xa1(i)/(ras**3) - GM_J*(xa1(i)-xj(i))/(raj**3)
        aya1 = -GM_S*ya1(i)/(ras**3) - GM_J*(ya1(i)-yj(i))/(raj**3)

C       teroide2
        ras = sqrt(xa2(i)**2 + ya2(i)**2)
        raj = sqrt((xa2(i)-xj(i))**2 + (ya2(i)-yj(i))**2)

        axa2 = -GM_S*xa2(i)/(ras**3) - GM_J*(xa2(i)-xj(i))/(raj**3)
        aya2 = -GM_S*ya2(i)/(ras**3) - GM_J*(ya2(i)-yj(i))/(raj**3)

C       teroide 3
        ras = sqrt(xa3(i)**2 + ya3(i)**2)
        raj = sqrt((xa3(i)-xj(i))**2 + (ya3(i)-yj(i))**2)

        axa3 = -GM_S*xa3(i)/(ras**3) - GM_J*(xa3(i)-xj(i))/(raj**3)
        aya3 = -GM_S*ya3(i)/(ras**3) - GM_J*(ya3(i)-yj(i))/(raj**3)

C       atualizaoção das posições usando verlat
        xj(i+1) = 2*xj(i) - xj(i-1) + axj*dt*dt
        yj(i+1) = 2*yj(i) - yj(i-1) + ayj*dt*dt

        xa1(i+1) = 2*xa1(i) - xa1(i-1) + axa1*dt*dt
        ya1(i+1) = 2*ya1(i) - ya1(i-1) + aya1*dt*dt

        xa2(i+1) = 2*xa2(i) - xa2(i-1) + axa2*dt*dt
        ya2(i+1) = 2*ya2(i) - ya2(i-1) + aya2*dt*dt

        xa3(i+1) = 2*xa3(i) - xa3(i-1) + axa3*dt*dt
        ya3(i+1) = 2*ya3(i) - ya3(i-1) + aya3*dt*dt

        end do

C       Salva

        open(unit=1,file='asteroides_saida.txt')
        open(unit=2,file='asteroides_saida_2.txt')

        do i = 0, imax
        write(1,10) dt*i, xj(i), yj(i), xa1(i), ya1(i)
        write(2,10) dt*i, xa2(i), ya2(i), xa3(i), ya3(i)
        end do

10      format(F12.6,4(",",F12.6))

        close(1)
        close(2)

        end subroutine calc2
