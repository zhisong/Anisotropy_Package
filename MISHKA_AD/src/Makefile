TITLE = mishka.exe
POSITION = ../
OBJS = cdotu.o cdotc.o mishka2a.o ppplib.o
LIBS = -lblas -llapack
OPTS = -r8
FC = ifort

all : $(OBJS)
	$(FC) -o $(POSITION)$(TITLE) $(OBJS) $(LIBS) $(OPTS);


mishka2a.o : pre
	./pre.exe <mishka2a.source.f> mishka2a.f;
	$(FC) -c -o mishka2a.o mishka2a.f $(OPTS);

ppplib.o : ppplib.f
	$(FC) -c -o ppplib.o ppplib.f $(OPTS);

cdotu.o : cdotu.f
	$(FC) -c -o cdotu.o cdotu.f $(OPTS);

cdotc.o : cdotc.f
	$(FC) -c -o cdotc.o cdotc.f $(OPTS);

pre :
	$(FC) -o pre.exe pre.f $(OPTS);

clean :
	rm *.o $(POSITION)$(TITLE) pre.exe;
