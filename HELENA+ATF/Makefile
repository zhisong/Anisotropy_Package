all : hel12a_double.f
	gfortran -w -o helcode hel12a_double.f -llapack -lblas 
	rm *.mod

nodelta : hel12a_double_nodelta.f
	gfortran -w -o helcode_nodelta hel12a_double_nodelta.f -llapack -lblas 
	rm *.mod

helnumtest : helnametest.cpp
	g++ -o helnumtest helnumtest.cpp

helautorun : helautorun.cpp
	g++ -o helautorun helautorun.cpp

helMASTtest: helMASTtest.cpp
	g++ -o helMASTtest helMASTtest.cpp

helscan : helscan.cpp
	g++ -o helscan helscan.cpp

helscan_equal : helscan_equal.cpp
	g++ -o helscan_equal helscan_equal.cpp

helscanprof : helscanprof.cpp
	g++ -o helscanprof helscanprof.cpp

helscandev: helscandev.cpp
	g++ -o helscandev helscandev.cpp

heldeltatest: heldeltatest.cpp
	g++ -o heldeltatest heldeltatest.cpp
helq0: helq0.cpp
	g++ -o helq0 helq0.cpp
helbal: helbal.cpp
	g++ -o helbal helbal.cpp

clean : 
	rm fort.* helcode

cleanfort :
	rm fort.*

cleanall :
	rm fort.* helcode helnumtest helautorun helscan helscanprof helMASTtest helq0 heldeltatest helscandev
