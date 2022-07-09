module config
    implicit none
    character(len=100),parameter :: ctrl_file='control'
contains

subroutine jdos_config(sig,sub_size)
    implicit none
    character(len=100):: buffer, label
    integer:: pos_
    real(kind=8),intent(out) :: sig
    integer,intent(out) :: sub_size
    integer:: fh=3 ! external file unit number
    integer::ios=0, buffer_ios=0 ! i/o status
    
    ! default value
    sig=0.05 ! 
    sub_size=1

    open(fh,file=ctrl_file,status='old')
    do while (ios == 0)
        read(fh, '(A)',iostat=ios) buffer ! read all into one buffer
        !print *,buffer
        pos_=scan(buffer,' ')
        label = buffer(1:pos_)
        buffer=buffer(pos_+1:)
        
        select case (label)
        case ('sigma')
            read(buffer,*,iostat=buffer_ios) sig ! need to handle the EOF of buffer
            !print *, "Value of sig is ", sig
        case ('sub_size')
            read (buffer,*,iostat=buffer_ios) sub_size
            !print *, "Size of sub_arr is ", sub_size
        end select
    end do
    close(fh)
end subroutine
end module

