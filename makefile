VERSION = 0.0.1

REBAR := ./rebar

ENVPATH = \
		  ebin\
		  deps/*/ebin

OPTS = \
	   -pa ${ENVPATH}

all:compile

help:
	@echo
	@echo "Useage:"
	@echo "    compile      Complie all erlang file."
	@echo "    clean        Clean all beam file."
	@echo "    f            Clean and compile erlang file."
	@echo "    deps         Get deps."
	@echo "    cleandeps    Delete all deps."
	@echo "    test         Run eunit."
	@echo "    erl          Run erl -pa ebin/"
	@echo "    run          Run gg_framework."
	@echo "    create_ct    Create common test from a template file."
	@echo "    ct           Run common test"
	@echo "    viewlog      View common test log on localhost:8000"
	@echo "    cleanlog     Clean logs"
	@echo "Warnimg:"
	@echo "    Do not make a user module when running common test!"
	@echo

f:
	${REBAR} clean compile

compile:
	${REBAR} compile

deps:
	${REBAR} get-deps

clean:
	${REBAR} clean

cleandeps:
	${REBAR} delete-deps

test:
	${REBAR} eunit

erl:
	erl ${OPTS}

run:
	erl ${OPTS} -run gg_framework_app start

create_ct:
	${REBAR} create template=ctsuite skip_deps=true

ct:
	${REBAR} ct skip_deps=true

viewlog:
	@echo "==============================================="
	@echo "View common test logs on http://localhost:8000/"
	@echo "==============================================="
	pushd logs; python -m SimpleHTTPServer; popd

cleanlog:
	rm -rf logs/*
