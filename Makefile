DEPLOY_DIR=$(shell stack path --local-bin)

all: setup

setup:
	stack setup

install:
	stack build --copy-bins
	cp server/mo-nomz/config/settings.yml $(DEPLOY_DIR)/settings.yml
	rm -rf $(DEPLOY_DIR)/migrations
	mkdir -p $(DEPLOY_DIR)/migrations
	cp -R server/mo-nomz/sql/migrations/ $(DEPLOY_DIR)
