module plot3dtool
! "plot3dtool" is a Fortran library providing uniform interfaces
! helping process files in PLOT3D format.
! Developer: Zhenling Jia
!           BUAA,2024.12
!           email: jingquw@163.com, zhenling@buaa.edu.cn
contains
subroutine igrid_from_filename(fn,igrid)
    implicit none
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
subroutine plot3d_readin(fn,mb,nvar,il,jl,kl,var)
    implicit none
    integer mb,nvar
    integer,dimension(:),pointer:: il,jl,kl
    real*8,dimension(:,:,:,:,:),pointer:: var
    character(*),intent(in):: fn
    integer mb1,mb2
    !
    print*,'processing ',trim(fn)
    !print*,'           len=',len(fn)
    !
    open(73,file=fn,form='UNFORMATTED',status='old')
    read(73) mb1
    close(73)
    if( mb1 > 10000 .or. mb1 < 1 )then
        open(73,file=fn,form='FORMATTED',status='old')
        read(73) mb2
        close(73)
        if( mb2 > 10000 .or. mb2 < 1 )then
            write(*,*) '**** plot3d info ****: file fomatted'
            call plot3d_readin_formatted(fn,mb,nvar,il,jl,kl,var)
        else
            write(*,*) 'file formatted or unformatted detecting failed!'
            write(*,*) 'mb1 for unformatted',mb1
            write(*,*) 'mb2 for   formatted',mb2
        endif
    else
        write(*,*) '**** plot3d info ****: file unfomatted'
        call plot3d_readin_unformatted(fn,mb,nvar,il,jl,kl,var)
    endif
end subroutine
subroutine plot3d_readin_unformatted(fn,mb,nvar,il,jl,kl,var)
    implicit none
    integer mb,nvar
    integer,dimension(:),pointer:: il,jl,kl
    real*8,dimension(:,:,:,:,:),pointer:: var
    character(*),intent(in):: fn
    !
    integer i,j,k,imb,ivar
    integer igrid!1: grid
    !
    call igrid_from_filename(fn,igrid)
    !
    open(73,file=fn,form='UNFORMATTED',status='old')
    read(73) mb
        allocate(il(mb),jl(mb),kl(mb))
    if( igrid==1) then
        read(73) (il(imb),jl(imb),kl(imb),imb=1,mb)
        nvar=3
    else
        read(73) (il(imb),jl(imb),kl(imb),nvar,imb=1,mb)
    endif
        allocate(var(maxval(il),maxval(jl),maxval(kl),mb,nvar))
    do imb=1,mb
        read(73) ((((var(i,j,k,imb,ivar),i=1,il(imb)),j=1,jl(imb)),k=1,kl(imb)),ivar=1,nvar)
    enddo      
    close(73)
end subroutine
subroutine plot3d_readin_formatted(fn,mb,nvar,il,jl,kl,var)
    implicit none
    integer mb,nvar
    integer,dimension(:),pointer:: il,jl,kl
    real*8,dimension(:,:,:,:,:),pointer:: var
    character(*),intent(in):: fn
    !
    integer i,j,k,imb,ivar
    integer igrid!1: grid
    !
    call igrid_from_filename(fn,igrid)
    !
    open(73,file=fn,form='FORMATTED',status='old')
    read(73,*) mb
        allocate(il(mb),jl(mb),kl(mb))
    if( igrid==1) then
        read(73,*) (il(imb),jl(imb),kl(imb),imb=1,mb)
        nvar=3
    else
        read(73,*) (il(imb),jl(imb),kl(imb),nvar,imb=1,mb)
    endif
        allocate(var(maxval(il),maxval(jl),maxval(kl),mb,nvar))
    do imb=1,mb
        read(73,*) ((((var(i,j,k,imb,ivar),i=1,il(imb)),j=1,jl(imb)),k=1,kl(imb)),ivar=1,nvar)
    enddo      
    close(73)
end subroutine
subroutine plot3d_write_formatted(fn,mb,nvar,il,jl,kl,var)
    implicit none
    integer mb,nvar
    integer,dimension(:),pointer:: il,jl,kl
    real*8,dimension(:,:,:,:,:),pointer:: var
    character(*),intent(in):: fn
    !
    integer i,j,k,imb,ivar
    integer igrid!1: grid
    !
    print*,'writing ',trim(fn)
    call igrid_from_filename(fn,igrid)
    !
    open(73,file=fn,form='FORMATTED')
    write(73,*) mb
    if( igrid==1) then
        write(73,*) (il(imb),jl(imb),kl(imb),imb=1,mb)
    else
        write(73,*) (il(imb),jl(imb),kl(imb),nvar,imb=1,mb)
    endif
    do imb=1,mb
        write(73,*) ((((var(i,j,k,imb,ivar),i=1,il(imb)),j=1,jl(imb)),k=1,kl(imb)),ivar=1,nvar)
    enddo      
    close(73)
end subroutine
subroutine plot3d_write_formatted_withib(fn,iblank,mb,nvar,il,jl,kl,var)
    implicit none
    integer mb,nvar
    integer,dimension(:),pointer:: il,jl,kl
    real*8,dimension(:,:,:,:,:),pointer:: var
    integer,dimension(:,:,:,:),pointer:: iblank
    character(*),intent(in):: fn
    !
    integer i,j,k,imb,ivar
    integer igrid!1: grid
    !
    print*,'writing with iblank ',trim(fn)
    call igrid_from_filename(fn,igrid)
    !
    open(73,file=fn,form='FORMATTED')
    write(73,*) mb
    if( igrid==1) then
        write(73,*) (il(imb),jl(imb),kl(imb),imb=1,mb)
    else
        write(73,*) (il(imb),jl(imb),kl(imb),nvar,imb=1,mb)
    endif
    do imb=1,mb
        write(73,*) ((((var(i,j,k,imb,ivar),i=1,il(imb)),j=1,jl(imb)),k=1,kl(imb)),ivar=1,nvar),&
                     (((  iblank(i,j,k,imb),i=1,il(imb)),j=1,jl(imb)),k=1,kl(imb))
    enddo      
    close(73)
end subroutine
subroutine plot3d_write_unformatted(fn,mb,nvar,il,jl,kl,var)
    implicit none
    integer mb,nvar
    integer,dimension(:),pointer:: il,jl,kl
    real*8,dimension(:,:,:,:,:),pointer:: var
    character(*),intent(in):: fn
    !
    integer i,j,k,imb,ivar
    integer igrid!1: grid
    !
    print*,'writing ',trim(fn)
    call igrid_from_filename(fn,igrid)
    !
    open(73,file=fn,form='UNFORMATTED')
    write(73) mb
    if( igrid==1) then
        write(73) (il(imb),jl(imb),kl(imb),imb=1,mb)
    else
        write(73) (il(imb),jl(imb),kl(imb),nvar,imb=1,mb)
    endif
    do imb=1,mb
        write(73) ((((var(i,j,k,imb,ivar),i=1,il(imb)),j=1,jl(imb)),k=1,kl(imb)),ivar=1,nvar)
    enddo      
    close(73)
end subroutine
subroutine plot3d_write_unformatted_withib(fn,iblank,mb,nvar,il,jl,kl,var)
    implicit none
    integer mb,nvar
    integer,dimension(:),pointer:: il,jl,kl
    real*8,dimension(:,:,:,:,:),pointer:: var
    integer,dimension(:,:,:,:),pointer:: iblank
    character(*),intent(in):: fn
    !
    integer i,j,k,imb,ivar
    integer igrid!1: grid
    !
    print*,'writing ',trim(fn)
    call igrid_from_filename(fn,igrid)
    !
    open(73,file=fn,form='UNFORMATTED')
    write(73) mb
    if( igrid==1) then
        write(73) (il(imb),jl(imb),kl(imb),imb=1,mb)
    else
        write(73) (il(imb),jl(imb),kl(imb),nvar,imb=1,mb)
    endif
    do imb=1,mb
        write(73) ((((var(i,j,k,imb,ivar),i=1,il(imb)),j=1,jl(imb)),k=1,kl(imb)),ivar=1,nvar), &
                   (((  iblank(i,j,k,imb),i=1,il(imb)),j=1,jl(imb)),k=1,kl(imb))
    enddo      
    close(73)
end subroutine
!
subroutine thold_cal(thold,mb,nvar,il,jl,kl,var)
    implicit none
    integer mb,nvar
    integer,dimension(:),pointer:: il,jl,kl
    real*8,dimension(:,:,:,:,:),pointer:: var
    real*8 thold
    integer i,j,k,imb
    !
    thold =1.0d20
    do imb=1,mb
    do i=1,il(imb)-1
    do j=1,jl(imb)
    do k=1,kl(imb)
        if(dabs(var(i+1,j,k,imb,1)-var(i,j,k,imb,1))<thold) then
            thold=dabs(var(i+1,j,k,imb,1)-var(i,j,k,imb,1))
        endif
    enddo
    enddo
    enddo
    do i=1,il(imb)
    do j=1,jl(imb)-1
    do k=1,kl(imb)
        if(dabs(var(i,j+1,k,imb,2)-var(i,j,k,imb,2))<thold) then
            thold=dabs(var(i,j+1,k,imb,2)-var(i,j,k,imb,2))
        endif
    enddo
    enddo
    enddo
    enddo
    !write(*,'(A,E20.12)') 'thold=',thold
    thold=thold/10.d0
    write(*,*) '**** plot3d info ****: thold=',thold
end
!
!
!
subroutine iblank_compute(iblank,mb,nvar,il,jl,kl,var)
    implicit none
    integer mb,nvar
    integer,dimension(:),pointer:: il,jl,kl
    real*8,dimension(:,:,:,:,:),pointer:: var
    !
    integer,dimension(:,:,:,:),pointer:: iblank
    !
    integer imb1,imb2,iface,ifound
    integer i1,i2,j1,j2,k1,k2,flag
    integer,dimension(mb,6)::link2
    real*8 thold
    integer,dimension(:,:),allocatable:: link,link_face,link_found
    !
        write(*,*) '**** plot3d info ****: iblank allocated',associated(iblank)
        if(.not. associated(iblank)) then
            allocate(iblank(maxval(il),maxval(jl),maxval(kl),mb),stat=i1)
            write(*,*) '**** plot3d info ****: iblank allocate iostat=',i1
            if( i1 /= 0 ) then
                write(*,*) 'allocate failed'
                stop
            endif
        endif
        write(*,*) '**** plot3d info ****: iblank allocated',associated(iblank)
    !
    write(*,*) '              before  thold,mb=',mb
    call thold_cal(thold,mb,nvar,il,jl,kl,var)
    !
    allocate(link(mb,6))
    allocate(link_face(mb,6))
    allocate(link_found(mb,6))
    link_face = -1
    link_found = 0
    !
    write(*,*) ' start recompute iblank... '
    iblank = 0 
    do imb1=1,mb
        i1=1; j1=2; k1=2
#       include "iblank_compute.inc"
        link_found(imb1,1) = ifound
        if(flag==0) then
            link2(imb1,1)=2
        else
            link2(imb1,1)=-imb2
            link_face(imb1,1)=iface
        endif
            do j1=1,jl(imb1)
            do k1=1,kl(imb1)
                iblank(i1,j1,k1,imb1)=link2(imb1,1)
            enddo
            enddo
        !
        i1=il(imb1); j1=2; k1=2
#       include "iblank_compute.inc"
        link_found(imb1,2) = ifound
        if(flag==0) then
            link2(imb1,2)=2
        else
            link2(imb1,2)=-imb2
            link_face(imb1,2)=iface
        endif
            do j1=1,jl(imb1)
            do k1=1,kl(imb1)
                iblank(i1,j1,k1,imb1)=link2(imb1,2)
            enddo
            enddo
        !
        i1=2; j1=1; k1=2
#       include "iblank_compute.inc"
        link_found(imb1,3) = ifound
        if(flag==0) then
            link2(imb1,3)=2
        else
            link2(imb1,3)=-imb2
            link_face(imb1,3)=iface
        endif
            do i1=1,il(imb1)
            do k1=1,kl(imb1)
                iblank(i1,j1,k1,imb1)=link2(imb1,3)
            enddo
            enddo
        !
        i1=2; j1=jl(imb1); k1=2
#       include "iblank_compute.inc"
        link_found(imb1,4) = ifound
        if(flag==0) then
            link2(imb1,4)=2
        else
            link2(imb1,4)=-imb2
            link_face(imb1,4)=iface
        endif
            do i1=1,il(imb1)
            do k1=1,kl(imb1)
                iblank(i1,j1,k1,imb1)=link2(imb1,4)
            enddo
            enddo
        !!!!
        !!!!
        !!!!
        !!!!
        i1=2; j1=2; k1=1
#       include "iblank_compute.inc"
        link_found(imb1,5) = ifound
        if(flag==0) then
            link2(imb1,5)=2
        else
            link2(imb1,5)=-imb2
            link_face(imb1,5)=iface
        endif
            do i1=1,il(imb1)
            do j1=1,jl(imb1)
                iblank(i1,j1,k1,imb1)=link2(imb1,5)
            enddo
            enddo
        !!!!
        i1=2; j1=2; k1=kl(imb1)
#       include "iblank_compute.inc"
        link_found(imb1,6) = ifound
        if(flag==0) then
            link2(imb1,6)=2
        else
            link2(imb1,6)=-imb2
            link_face(imb1,6)=iface
        endif
            do i1=1,il(imb1)
            do j1=1,jl(imb1)
                iblank(i1,j1,k1,imb1)=link2(imb1,6)
            enddo
            enddo
    enddo
    !
    link = link2
    write(*,*) 'iblank compute done'
end subroutine

subroutine link_check(imb1,i1,j1,k1,imb2,i2,j2,k2,flag,iface, &
                      mb,nvar,il,jl,kl,var,thold)
    implicit none
    integer mb,nvar
    integer,dimension(:),pointer:: il,jl,kl
    real*8,dimension(:,:,:,:,:),pointer:: var
    real*8 thold
    !
    integer,intent(in):: imb1,i1,j1,k1,imb2,i2,j2,k2
    integer,intent(inout):: flag,iface
    !
    integer i,j,k,ii,jj,kk,flag_tmp
    integer is1,js1,ks1,ie1,je1,ke1
    integer is2,js2,ks2,ie2,je2,ke2
    !
    if( i2/=1        .and. j2/=1        .and. k2/=1  .and.  &
        i2/=il(imb2) .and. j2/=jl(imb2) .and. k2/=kl(imb2) ) then
        flag = 0 
        return
    else
        is1 = 1;        js1 = 1;        ks1 = 1
        ie1 = il(imb1); je1 = jl(imb1); ke1 = kl(imb1)
        if( i1==1 ) ie1 = 1
        if( j1==1 ) je1 = 1
        if( k1==1 ) ke1 = 1
        if( i1==il(imb1) ) is1 = il(imb1)
        if( j1==jl(imb1) ) js1 = jl(imb1)
        if( k1==kl(imb1) ) ks1 = kl(imb1)
        !
        is2 = 1;        js2 = 1;        ks2 = 1
        ie2 = il(imb2); je2 = jl(imb2); ke2 = kl(imb2)
        if( i2==1 ) ie2 = 1
        if( j2==1 ) je2 = 1
        if( k2==1 ) ke2 = 1
        if( i2==il(imb2) ) is2 = il(imb2)
        if( j2==jl(imb2) ) js2 = jl(imb2)
        if( k2==kl(imb2) ) ks2 = kl(imb2)
        !
        do i=is1,ie1
        do j=js1,je1
        do k=ks1,ke1
            flag_tmp = 0
            do ii=is2,ie2
            do jj=js2,je2
            do kk=ks2,ke2
                if( dsqrt( &
                   (var(i,j,k,imb1,1)-var(ii,jj,kk,imb2,1))**2 +&
                   (var(i,j,k,imb1,2)-var(ii,jj,kk,imb2,2))**2 +&
                   (var(i,j,k,imb1,3)-var(ii,jj,kk,imb2,3))**2 )<thold)then
                   flag_tmp = 1
                endif
            enddo
            enddo
            enddo
            if( flag_tmp == 0 ) then
                flag = 0
                return
            endif
        enddo
        enddo
        enddo
        flag = 1
        if( is2==ie2) then
            if( is2 ==1 ) then
                iface = 1 
            else
                iface = 2
            endif
        elseif( js2==je2 ) then
            if( js2 ==1 ) then
                iface = 3
            else
                iface = 4
            endif
        elseif( ks2==ke2 ) then
            if( ks2 ==1 ) then
                iface = 5 
            else
                iface = 6
            endif
        endif
    endif
end subroutine
end module 

