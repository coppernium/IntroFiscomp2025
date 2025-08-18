        program main
        real*4 AJM, V, Q
        ! Lê as variáveis
                write(*,*) 'Escreva o valor de Q.'
                read(*,*) Q
                write(*,*) 'Escreva o valor de N.'
                read(*,*) N !Inteiro
                write(*,*) 'Escreva o valor de AJM.'
                read(*,*) AJM
        ! Converte AJM para decimais
                AJM = AJM / 100.0e0
                V = Q * ( AJM / ( 1.0e0 - (1.0e0 + AJM)**(-N) ) )

                write(*,7) V
7               format('O valor mensal pago é: ',F9.3)
        end program main
