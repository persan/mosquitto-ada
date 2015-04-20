

PERFIX?=$(dir $(shell dirname $(shell which gnatls)))
_includedir=${PERFIX}include/mosquitto
_libdir=${PERFIX}lib/mosquitto
_gprdir=${PERFIX}lib/gpr

all:compile test

compile:
	gprbuild -p P mosquitto.gpr

install:
	mkdir -p ${INSTALLDIR}${_includedir}
	mkdir -p ${INSTALLDIR}${_libdir}
	mkdir -p ${INSTALLDIR}${_gprdir}
	cp `find src -name "*.ad?"` ${INSTALLDIR}${_includedir}/
	cp mosquitto.gpr.in ${INSTALLDIR}${_gprdir}/mosquitto.gpr
	cp lib/*.ali lib/*.a ${INSTALLDIR}${_libdir}/


gen:src/gen/mosquitto-mosquitto_h.ads
src/gen/mosquitto-mosquitto_h.ads:  # IGNORE
	rm -rf src/gen
	mkdir -p src/gen
	cd src/gen;echo "#include <mosquitto.h>" >gen.cpp
	cd src/gen;gcc -C -c -fdump-ada-spec gen.cpp -fada-spec-parent=mosquitto
	cd src/gen;rm gen.*
	cd src/gen;sed "s-package mosquitto-private package mosquitto-" -i mosquitto-mosquitto_h.ads
	gprbuild -c -p -P mosquitto.gpr mosquitto-mosquitto_h.ads


test:
	${MAKE} -C test

testinstall:

clean:
