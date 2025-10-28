        program main
                m = 1000
                open(unit=1,file='saida-1-12694394.txt')
                do i = 100,1000
                        write(1,7) i,calc(m,i)
                end do
7               format(I12,',',F12.4)
                close(1)
        end program main

        function calc(m,n)
                parameter(iseed=1154)
                dimension ipos(-n:n,-n:n),istep(0:1)
                
                ! Número de micro estados
                irazao = n*0.1
                ! Tamanho do reticulado
                ksize = n/irazao 

                ! Da o seed para o rand()
                rr = rand(iseed)
                ! Inicia o vetor posição
                do i =-n,n
                        do j = -n,n
                                ipos(i,j) = 0
                        end do
                end do
                
                ! Valores dos passos
                istep(0) = 1
                istep(1) = -1
                
                ! Realiza os cálculos
                do i = 1,m
                        ix = 0
                        iy = 0
                        do j = 1,n
                        idir = 2e0*rand()
                        irand = 2e0*rand()

                        if (idir .EQ. 0) then
                                ix = ix + istep(irand)
                        else
                                iy = iy + istep(irand)
                        endif
                        end do
                        ipos(ix,iy) = ipos(ix,iy) + 1
                end do
                
                ! Calculo reticulado
                calc = 0
                n1 = -n
                n2 = n1 + ksize
                do k =1,2*irazao
                        rpro= 0e0

                        do i = n1,n2
                                do j = n1,n2
                                        rpro = rpro + ipos(i,j) 
                                end do
                        end do
                        
                        rpro = rpro/(irazao)
                        
                        if (rpro .NE. 0e0) then
                                calc = calc - rpro*log(rpro)

                        end if
                        ! Muda o valor dos indices
                        n1 = n2
                        n2 = n2 + ksize
                end do
                return
        end function calc