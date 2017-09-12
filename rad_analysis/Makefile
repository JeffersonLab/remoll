make:
	g++ -c rad_dose.cc -o rad_dose.o `root-config --cflags --glibs`
	g++ rad_dose.o -o rad_dose `root-config --cflags --glibs`
	rm rad_dose.o

clean:
	rm ./*.o
	rm ./fom
