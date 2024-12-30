module plot3dtool
! "plot3dtool" is a Fortran library providing uniform interfaces
! helping process files in PLOT3D format.
! Developer: Zhenling Jia
!           BUAA,2024.12
!           email: jingquw@163.com, zhenling@buaa.edu.cn
contains
subroutine igrid_from_filename(fn,igrid)                                                                                    implicit none
    character(*),intent(in):: fn
    integer i
    integer igrid!1: grid
        igrid=0
        do i=1,len(fn)-3
            if( (fn(i:i)=='.') .and. (i+3<=80) .and. (fn(i+1:i+3)=="xyz") ) then
                igrid=1
                write(*,*) '**** plot3d info ****: file mesh'
                return
            endif
            if( (fn(i:i)=='.') .and. (i+1<=80) .and. (fn(i+1:i+1)=="x") ) then
                igrid=1
                write(*,*) '**** plot3d info ****: file mesh'
                return
            endif
         enddo
         write(*,*) '**** plot3d info ****: file solution'
end subroutine

end module
