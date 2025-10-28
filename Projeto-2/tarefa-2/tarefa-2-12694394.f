        program main

        ! Eu vou definir as constantes
        m = 1e4 ! Número de bebados
        n = 1e2 ! Número de passos
        p = 0.50 ! Probabilidade de andar para direita
        x = calc(n,m,p)

        end program main

        function calc(n,m,p)
            parameter(iseed=1154)
            dimension ipos(-n:n),istp(0:1)
            
            ! Da o seed para a func rand()
            rr = rand(iseed)
            ! Eu vou iniciar o vetor posição
            do i = -n,n
                ipos(i) = 0
            end do
            ! Vou definir algumas variaveis
            istp(0) = 1
            istp(1) = -1

            rmed = 0
            rmed2 = 0
            ! Vou criar o arquivo de saida
            open(unit=1,file='saida-1-12694394.txt')
            ! Vou calcular as pos
            do i = 1,m
                ix = 0
                do j = 1,n
                    if (rand() .lt. p) then
                        irr = 0
                    else
                    irr = 1
                    end if

                    ix = ix + istp(irr) 
                end do
                rmed = rmed + ix
                rmed2 = rmed2 + (ix*ix)
                ipos(ix) = ipos(ix) + 1
            end do
                rm = m
                rmed = rmed/rm
                rmed2 = rmed2/rm
            
            !
            write(*,*) "<x>,<x^2>"
            write(*,9) rmed,rmed2
9           format(F12.4,F12.4)
            !
            do i = -n,n
                write(1,7) i, ipos(i)
            end do
7           format(I12,',',I12)
            close(1)
            return
        end function calc