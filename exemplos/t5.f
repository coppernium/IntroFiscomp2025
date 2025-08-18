        program main
        parameter(idimax=1e3)
        dimension M(idimax), ip(idimax)
        dimension M2(idimax)

        n = 2
        open(unit=1,file='entrada.txt')

        do i =1,n
        read(1,*) (M(j),j=1,n), ip(i) 
        M2(i) = n+1
        do k = 1,n
                 
        end do

        end do 
        close(1)
        
        end program main
