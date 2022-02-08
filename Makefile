all: setup

setup:
	stack upgrade
	stack setup
	stack clean --full
	rm -rf /root/.stack/snapshots

install:
	stack build --copy-bins
