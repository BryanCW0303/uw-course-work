REGRESSIONS_LSTDFLG = -lopenblas -lm
REGRESSIONS_INCLUDE = 
REGRESSIONS_LIB = 
REGRESSIONS_OBJS = regressions

all: ${REGRESSIONS_OBJS}
	rm -f *.o

matrices.o: matrices.cpp matrices.h
	g++ -g -c matrices.cpp -o matrices.o ${REGRESSIONS_INCLUDE}

regmodels.o: regmodels.cpp regmodels.h
	g++ -g -c regmodels.cpp -o regmodels.o ${REGRESSIONS_INCLUDE}

main.o: main.cpp matrices.h
	g++ -g -c main.cpp -o main.o ${REGRESSIONS_INCLUDE}


regressions: main.o matrices.o regmodels.o
	g++ main.o regmodels.o matrices.o -o regressions ${REGRESSIONS_LIB} ${REGRESSIONS_LSTDFLG}

clean:
	rm -f *.o
	rm -f ${REGRESSIONS_OBJS}