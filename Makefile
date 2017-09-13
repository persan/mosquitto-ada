PROJECT=mosquitto-ada
TAG=$(shell python ./helper.py bin/version)
GNATLS?=$(shell which gnatls)
VERSION=${PROJECT}-${TAG}
USER=$(shell python ./helper.py ~/.ssh/github.user)
ACCESS=$(shell python ./helper.py ~/.ssh/github.token)


-include Makefile.conf

all:
ifeq ("${GNATLS}","")
$(error no gantls found)
endif


Makefile.conf:Makefile  # IGNORE
	@echo "PERFIX?=$(dir $(shell dirname ${GNATLS}))">${@}
	@echo "export PATH:=${PATH}" >>${@}

all:compile test

compile:
	gprbuild -p -P mosquitto.gpr
	gprbuild -p -P mosquitto-helpers.gpr

install:
	gprinstall -p -P mosquitto.gpr
	#cp $(shell find -name "*.ads") ${DESTDIR}${PREFIX}/include/mosquitto/
	#cp -r lib/* ${DESTDIR}${PREFIX}/lib/

uninstall:
	gprinstall -p -P mosquitto.gpr --uninstall


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

.PHONY: test-install
test-install:
	${MAKE} -C test-install
