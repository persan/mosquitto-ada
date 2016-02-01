PROJECT=mosquitto-ada
TAG=$(shell if [[ -e bin/version ]] ; then bin/version ; else echo "----";fi)

VERSION=${PROJECT}-${TAG}
USER=$(shell if [[ -e  ~/.ssh/github.user ]] ; then cat ~/.ssh/github.user | sed "s- --"; fi)
ACCESS=$(shell if [[ -e ~/.ssh/github.token ]] ; then cat ~/.ssh/github.token | sed "s- --"; fi)


-include Makefile.conf

all:

Makefile.conf:Makefile  # IGNORE
	@if [[ -z `which gnatls` ]] ; then echo No gnatls found check your installation.; -1;fi
	@echo "PERFIX=$(dir $(shell dirname $(shell which gnatls)))">${@}
	@echo "_includedir=\$${PERFIX}include/mosquitto">>${@}
	@echo "_libdir=\$${PERFIX}lib/mosquitto">>${@}
	@echo "_gprdir=\$${PERFIX}lib/gnat">>${@}

all:compile test

compile:
	gprbuild -p -P mosquitto-helpers.gpr

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
	echo ${TAG}
	${MAKE} -C tests

clean:
	rm Makefile.conf
	rm .obj -rf
	rm lib -rf
	rm bin/* -rf
check:
	@if [ ! -z "`git status --porcelain`" ] ; then \
		echo Folder is not clean;\
		git status;\
		exit -1;\
	else\
		echo ready for release ${VERSION};\
	fi
	git pull -q
	git push -q

release:check
	curl --data '$(shell sed -e "s/@VERSION@/${VERSION}/" -e "s/@TAG@/${TAG}/" github-version.in)' \
		"https://api.github.com/repos/${USER}/${PROJECT}/releases?access_token=${ACCESS}"
