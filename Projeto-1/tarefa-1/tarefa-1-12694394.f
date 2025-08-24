        program main
                write(*,*) 'Escreva o valor de Q.'
                read(*,*) Q
                write(*,*) 'Escreva o valor de N.'
                read(*,*) N !Inteiro
                write(*,*) 'Escreva o valor de AJM.'
                read(*,*) AJM

                AJM = AJM/100d0
                V = AJM*Q*((1d0+AJM)**N)/(((1d0+AJM)**N) - 1d0)

                write(*,7) V
7               format('O valor mensal pago é: ',F9.3)
        end program main
