.PHONY: dist
dist:
	rm -rf dist
	mkdir -p dist/config
	cp ./wizard-server-bin dist/
	cp ./config/application.yml ./config/integration.yml ./config/build-info.yml dist/config/
	cp -R ../lib dist/
