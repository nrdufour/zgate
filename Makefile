PROJECT = zgate
PROJECT_DESCRIPTION = Zigbee Wireless Sensor Gateway
PROJECT_VERSION = 0.1

DEPS = srly hackney couchbeam erlang_iso8601 jsx

dep_srly = git https://github.com/nrdufour/srly
dep_couchbeam = git https://github.com/benoitc/couchbeam 1.3.0
dep_hackney = git https://github.com/benoitc/hackney 1.5.7
dep_erlang_iso8601 = git https://github.com/oubiwann/erlang_iso8601 fix-erlang-18-compile-issue

include erlang.mk
