        program main
        implicit real*8 (a-h,o-z)

        call calc()

        end program main


        subroutine calc()
        implicit real*8 (a-h,o-z)
        parameter (imax=5e3)

        dimension x_t(-1:imax), y_t(-1:imax)
        dimension x_j(-1:imax), y_j(-1:imax)

C====================================================
C                   CONSTANTES
C====================================================

        pi = acos(-1d0)
        dt = 1d0/365d0

        GM_S = 4d0*pi*pi
        GM_J = GM_S * 0.0009543d0
        GM_T = GM_S * 3.003d-6

C====================================================
C               CONDI��ES INICIAIS
C====================================================

        x_t(0) = 1d0
        y_t(0) = 0d0
        vx_t  = 0d0
        vy_t  = 2d0*pi

        x_j(0) = 5.2d0
        y_j(0) = 0d0
        vx_j  = 0d0
        vy_j  = 2d0*pi*5.2d0/11.86d0

C====================================================
C       ACELERA��ES INICIAIS
C====================================================

C Terra
        r_ts = sqrt(x_t(0)**2 + y_t(0)**2)
        r_tj = sqrt((x_t(0)-x_j(0))**2 + (y_t(0)-y_j(0))**2)

        ax_t = -GM_S*x_t(0)/(r_ts**3) - GM_J*(x_t(0)-x_j(0))/(r_tj**3)
        ay_t = -GM_S*y_t(0)/(r_ts**3) - GM_J*(y_t(0)-y_j(0))/(r_tj**3)

C Jupiter
        r_js = sqrt(x_j(0)**2 + y_j(0)**2)
        r_jt = r_tj

        ax_j = -GM_S*x_j(0)/(r_js**3) - GM_T*(x_j(0)-x_t(0))/(r_jt**3)
        ay_j = -GM_S*y_j(0)/(r_js**3) - GM_T*(y_j(0)-y_t(0))/(r_jt**3)

C====================================================
C       PASSO -1 DO M�TODO VERLET
C====================================================

        x_t(-1) = x_t(0) - vx_t*dt + 0.5d0*ax_t*dt*dt
        y_t(-1) = y_t(0) - vy_t*dt + 0.5d0*ay_t*dt*dt

        x_j(-1) = x_j(0) - vx_j*dt + 0.5d0*ax_j*dt*dt
        y_j(-1) = y_j(0) - vy_j*dt + 0.5d0*ay_j*dt*dt

C====================================================
C                LOOP PRINCIPAL
C====================================================

        do i = 0, imax-1

C --- Acelera��o Terra ---
        r_ts = sqrt(x_t(i)**2 + y_t(i)**2)
        r_tj = sqrt((x_t(i)-x_j(i))**2 + (y_t(i)-y_j(i))**2)

        ax_t = -GM_S*x_t(i)/(r_ts**3) - GM_J*(x_t(i)-x_j(i))/(r_tj**3)
        ay_t = -GM_S*y_t(i)/(r_ts**3) - GM_J*(y_t(i)-y_j(i))/(r_tj**3)

C --- Acelera��o Jupiter ---
        r_js = sqrt(x_j(i)**2 + y_j(i)**2)
        r_jt = r_tj

        ax_j = -GM_S*x_j(i)/(r_js**3) - GM_T*(x_j(i)-x_t(i))/(r_jt**3)
        ay_j = -GM_S*y_j(i)/(r_js**3) - GM_T*(y_j(i)-y_t(i))/(r_jt**3)

C --- Atualiza��o das posi��es (Verlet) ---
        x_t(i+1) = 2d0*x_t(i) - x_t(i-1) + ax_t*dt*dt
        y_t(i+1) = 2d0*y_t(i) - y_t(i-1) + ay_t*dt*dt

        x_j(i+1) = 2d0*x_j(i) - x_j(i-1) + ax_j*dt*dt
        y_j(i+1) = 2d0*y_j(i) - y_j(i-1) + ay_j*dt*dt

        end do

C====================================================
C                   SALVAR DADOS
C====================================================

        open(unit=1, file='saida.txt')
        do i = 0, imax
            write(1,2) dt*i, x_t(i), y_t(i), x_j(i), y_j(i)
        end do
2       format(F16.8, 4(",",F16.8))
        close(1)

        end subroutine calc
