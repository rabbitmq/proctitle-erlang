PROJECT = proctitle
PROJECT_DESCRIPTION = A library to change the Erlang VM process title in ps(1) and top(1)
PROJECT_VERSION = 0.1.0

DEPS = host_triple
dep_host_triple = git https://github.com/rabbitmq/host_triple.git master

C_SRC_OUTPUT = $(CURDIR)/priv/proctitle_nif

include erlang.mk

-include c_src/env.mk

cppcheck:
	cppcheck -f --quiet \
		--error-exitcode=2 \
		--enable=all \
		--inconclusive \
		--std=posix \
		-I$(ERTS_INCLUDE_DIR) c_src

scan-build:
	$(MAKE) clean
	scan-build $(MAKE)
