        program ex3
                parameter(idimax=1e6)
                real*8 rr
                real*8 lista
                dimension lista(idimax)

                iN = idimax
                open(unit=3,file='entrada-1-12694394.txt')
                open(unit=4,file='saida-1-12694394.txt')
                ic = 0
                do i =1,iN
                read(3,*,End=1) lista(i)
                ic = ic + 1
                end do
1               write(*,*) ic

                read(*,*) iM

                do i = 1,iM
                do j = 1,iM
                        if (lista(i) .LT. lista(j)) then
                                temp = lista(i)
                                lista(i) = lista(j)
                                lista(j) = temp
                        end if
                end do
                end do

                write(4,8) iM
                do i=1,iM
                write(4,7) lista(i)
                end do
7               format(F12.6)
8               format(I8)
        end program ex3
