tecio_path=/opt/tecplot2017/360ex_2017r3/

files = plot3dtool.o \
        


exe= libplot3dtool.so

fc=gfortran
#opt= -fcray-pointer  -g --tracer --debug --check=all -cpp -c -fPIC -fstack-check -fbounds-check -fcheck=all -Wall -g
opt= -Ofast -c -fPIC -cpp

include=-I${tecio_path}/include   #-I${tecio_path}/bin 
libpath=-L${tecio_path}/bin       -L${tecio_path}/lib
lib= #-ltecio -lstdc++ -lpthread #-lm  

#all:${files}

${exe}:${files}
	${fc}  -shared  $<  -o $@

${files}:%.o:%.f90
	${fc}  $<  ${opt}  -o $@

clean:
	rm *.o *mod

