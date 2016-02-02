all:
	-gprclean -P mosquitto-tests.gpr -XMISQUITTO_BUILD=relocatable
	gprbuild -p -P mosquitto-tests.gpr -XMISQUITTO_BUILD=relocatable
	./bin/mosquitto-tests-main
	ldd ./bin/mosquitto-tests-main

	-gprclean -P mosquitto-tests.gpr -XMISQUITTO_BUILD=static
	gprbuild -p -P mosquitto-tests.gpr -XMISQUITTO_BUILD=static
	./bin/mosquitto-tests-main
	ldd ./bin/mosquitto-tests-main
