TITLE = mishka.exe
POSITION = ../
OBJS = mishka2a.o ppplib.o
LIBS = -lblas -llapack
OPTS = -fdefault-real-8
FC = gfortran

all : $(OBJS)
	$(FC) -o $(POSITION)$(TITLE) $(OBJS) $(LIBS) $(OPTS);


mishka2a.o : pre
	./pre.exe <mishka2a.source.f> mishka2a.f;
	$(FC) -c -o mishka2a.o mishka2a.f $(OPTS);

ppplib.o : ppplib.f
	$(FC) -c -o ppplib.o ppplib.f $(OPTS);

pre :
	$(FC) -o pre.exe pre.f $(OPTS);

clean :
	rm *.o $(POSITION)$(TITLE) pre.exe;
