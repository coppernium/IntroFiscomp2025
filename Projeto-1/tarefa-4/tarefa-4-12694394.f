        program main
                real*8 x,tol
                x = 3d0
                tol = 1e-5

                write(*,*) calc1(x,tol), log(x)
        end program main

        function calc1(x,tol)
                real*8 x,tol

                i = 1
                calc1 = 1
                do while (abs(rr) .GE. tol)
                        rr = - ((1-x)**i)/i
                        calc1 = calc1 + rr
                        i = i + 1
                end do
                return
        end function calc1
