        program main
            parameter(iseed=1154)
            
            rr = rand(iseed)

            m = 1e6
            write(*,2) m
2           format('Para m =', I8)
            do i = 1,4
            write(*,7) i,calc(m,i)
            end do
7           format('n=', I1, ' -> ', F6.4)

            x=calc2(m,1)
        end program main


        function calc(m,n)
            calc = 0
            do i = 1,m
                calc = calc + (rand()**n)
            end do
            rm = m
            calc =calc/rm
            return
        end function calc

        function calc2(m,n)
            open(unit=1,file='saida-1-12694394.txt')
            do i = 1,m
                write(1,7) (rand()**n)
            end do
            close(1)
7           format(F6.4)
        end function calc2