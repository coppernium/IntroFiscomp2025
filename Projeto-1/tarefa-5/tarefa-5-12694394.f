        program ex5
        parameter (idimax = 1e3)
        dimension M(idimax)
        dimension M2(idimax)
        dimension M3(idimax)
       
                n = 0

                open(unit=1,file='entrada-1-12694394.txt')
                do i = 1,idimax
                        read(1,*,end=1)
                        n = n +1
                end do
1               continue
                        n = n/2
                        n = 2
                close(unit=1)

                open(unit=2,file='fort.1')
                ! open(unit=2,file='entrada-1-12694394.txt')
                read(2,*) (M(i),i=1,n), p

                M(n+1) = n+1
                write(*,*) (M(i),i=1,n+1), 1

                do k = 1,n
                        do i = 1,n+1
                                if (i .LE. n) then
                                        M2(i) = M(i+1)
                                else
                                        M2(i) = M(i-n)
                                end if
                        end do
                        write(*,*) (M2(i),i=1,n+1), 1

                        do i = 1,n+1
                                M(i) = M2(i)
                        end do

                end do

                        write(*,*) (M(i),i=1,n+1), 1

                ! AAAAAAAAAAAAAAAA !

                close(unit=2)
        end program ex5
