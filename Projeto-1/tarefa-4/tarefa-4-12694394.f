        program main
                real*8 x,tol,res,d,r
                tol = 1e-6
                x = 3.22d0

                write(*,*) calc1(x,tol), log(x)
        end program main

        function calc1(x,tol)
                real*8 x,tol,rr

                i = 1
                calc1 = 1
                do while (abs(rr) .GE. tol)
                        rr = - ((1-x)**i)/i
                        calc1 = calc1 + rr
                        i = i + 1
                end do
                return
        end function calc1
