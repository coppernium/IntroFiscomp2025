        program main
        !define pi
        pi = acos(-1.0)

        !pede o valor dos raios
        write(*,2)
        read(*,*) r1
        write(*,3)
        read(*,*) r2

        ! realiza os calcs
        area = 4*(pi**2)*r1*r2
        vol = 2*(pi**2)*r1*(r2**2)

        ! printa os resultados
        write(*,7) area 
        write(*,8) vol 



2       format('Insira o raio externo.')
3       format('Insira o raio interno.')
7       format('Área: ', F12.3)
8       format('Volume: ', F12.3)

        end program main
