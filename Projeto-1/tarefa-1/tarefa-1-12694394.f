        program main
        ! Lê as variáveis
                write(*,*) 'Escreva o valor de Q.'
                read(*,*) Q
                write(*,*) 'Escreva o valor de N.'
                read(*,*) N !Inteiro
                write(*,*) 'Escreva o valor de AJM.'
                read(*,*) AJM
        ! Converte AJM para decimais
                AJM = AJM
                V = AJM*Q*((1+AJM)**N)/(((1+AJM)**N) - 1)

                write(*,7) V
7               format('O valor mensal pago é: ',F9.3)
        end program main
