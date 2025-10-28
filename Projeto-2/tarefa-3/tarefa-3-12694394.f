        program main
            n = 5000
            m = 1000
            x = calc(m,n)
        end program main

        function calc(m,n)
        parameter(iseed=12)
        dimension istep(1:2), ipos(-n:n,-n:n)
        
        ! Da o seed para o rand()
        rr = rand(iseed)

        ! Inicia o vetor istep
        istep(1) = 1
        istep(2) = -1

        ! Inicia o vetor posição
        do i =-n,n
            do j = -n,n
                ipos(i,j) = 0
            end do
        end do

        ! Cálculos
        ixm = 0
        iym = 0
        do i =1,m
            ix = 0
            iy = 0
            do j = 1,n
                ! Escolho se vou na direção x ou y
                idir = 2e0*rand()
                irand = 2e0*rand() + 1
                if (idir .EQ. 0) then
                    ix = ix + istep(irand)
                else
                    iy = iy + istep(irand)
                end if
            end do
            ipos(ix,iy) = ipos(ix,iy) + 1
        end do

        ! Cálcula o valor médio da posição <r>
        r1x = 0
        r1y = 0
        do i =-n,n
            do j =-n,n
                if (ipos(i,j) .NE. 0) then
                    r1x = r1x + i
                    r1y = r1y + j
                end if
            end do
        end do
        r1x = r1x/m
        r1y = r1y/m
        rr1 = r1x + r1y
        r1 = (r1x*r1x) + (r1y*r1y)

        ! Cálcula o valor médio de <r.r>
        r2=0
        do i =-n,n
            do j =-n,n
                if (ipos(i,j) .NE. 0) then
                    r2 = r2 + (i*i) + (j*j)
                end if
            end do
        end do
        r2 = r2/m

        ! Calcula o valor de <(delta_r)^2> = <r.r> - <r>.<r>
        dr2 = r2 - r1

        ! Escreve na tela <(delta_r)^2> e <r>
        write(*,*)" <r>, <(delta_r)^2>"
        write(*,11) rr1,dr2
        ! Salva os resultados
        open(unit=1,file='saida-1-12694394.txt')
        do i =-n,n
            do j = -n,n
            if (ipos(i,j) .GT. 0) then
                write(1,7) i,j,ipos(i,j)
            end if
            end do
        end do
7       format(I4,',',I4,',',I4)
11      format(F12.4, F12.4)
        close(1)
        end function calc