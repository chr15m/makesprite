IMG=public/logo.png public/social-meta.png

build: $(shell find src) public/* node_modules $(IMG)
	mkdir -p build
	npx shadow-cljs release app
	rsync -aLz --exclude js --exclude '.*.swp' public/ build
	touch build

node_modules: package.json
	pnpm i --shamefully-hoist

public/logo.png: src/logo.svg
	inkscape -o $@ -w 512 $<

public/social-meta.png: src/social-meta.svg
	inkscape -o $@ -w 800 $<

.PHONY: watch clean

watch: node_modules $(IMG)
	npx shadow-cljs watch app 

repl: node_modules
	npx shadow-cljs cljs-repl app

clean:
	rm -rf build

