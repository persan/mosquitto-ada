all:
ifeq (${MAKELEVEL},0)
	${MAKE} -C .. ${@}
else
	${MAKE} compile test
endif

compile:
ifeq (${MAKELEVEL},0)
	${MAKE} -C .. ${@}
else
	gprbuild -p -P mosquitto-tests.gpr
endif

test:
ifeq (${MAKELEVEL},0)
	${MAKE} -C .. ${@}
else
	./bin/mosquitto-tests-main
endif

release:
ifeq (${MAKELEVEL},0)
	${MAKE} -C .. ${@}
endif
