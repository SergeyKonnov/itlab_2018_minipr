all: build

build: matrixvector

matrixvector: matrixvector.o
	mpif90 -o matrixvector matrixvector.o -lcaf_mpi

matrixvector.o: matrixvector.f90
	gfortran -fcoarray=lib -c ./matrixvector.f90

run: build
	mpiexec -n $(N) ./matrixvector