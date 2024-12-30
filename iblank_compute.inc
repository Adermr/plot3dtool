      flag=0
      ifound = 0
           do imb2=1,mb
               if(imb2 .ne. imb1)then
                   do i2=1,il(imb2)
                   do j2=1,jl(imb2)
                   do k2=1,kl(imb2)
                       if( dsqrt( &
                          (var(i2,j2,k2,imb2,1)-var(i1,j1,k1,imb1,1))**2 +&
                          (var(i2,j2,k2,imb2,2)-var(i1,j1,k1,imb1,2))**2 +&
                          (var(i2,j2,k2,imb2,3)-var(i1,j1,k1,imb1,3))**2 )<thold)then
                          ifound = 1
                          call link_check(imb1,i1,j1,k1,imb2,i2,j2,k2,flag,iface,&
                                            mb,nvar,il,jl,kl,var,thold)
                          if(flag==1) exit
                       endif
                   enddo
                       if(flag==1) exit
                   enddo 
                       if(flag==1) exit
                   enddo
               endif
               if(flag==1) exit
           enddo

