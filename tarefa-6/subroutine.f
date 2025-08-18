        program main
                real*8 x

                x = 10d0

                call calc(x)


        end program main


        subroutine calc(val)
        real*8 val
        write(*,*) val
        end subroutine calc
