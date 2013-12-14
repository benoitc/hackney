#!/bin/bash -ex
erl -pa ebin/ deps/*/ebin -boot start_sasl -s hackney
