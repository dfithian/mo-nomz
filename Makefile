DEPLOY_DIR=$(shell stack path --local-bin)

all: setup

setup:
	stack upgrade
	stack setup
	stack clean --full
	rm -rf /root/.stack/snapshots

install:
	stack build --copy-bins
	cp server/mo-nomz/config/settings.yml $(DEPLOY_DIR)/settings.yml
	rm -rf $(DEPLOY_DIR)/migrations
	mkdir -p $(DEPLOY_DIR)/migrations
	cp -R server/mo-nomz/sql/migrations/ $(DEPLOY_DIR)
	rm -rf $(DEPLOY_DIR)/assets
	mkdir -p $(DEPLOY_DIR)/assets
	cp -R server/mo-nomz/assets/ $(DEPLOY_DIR)
