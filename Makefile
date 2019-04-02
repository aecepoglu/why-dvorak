BCJS = _build/default/ocaml/*.bc.js
DEST = docs/

deploy: $(BCJS) public_assets
	@mkdir -p $(DEST)
	cp -r $(BCJS) $(DEST)
	firefox http://localhost:8080

$(BCJS): ocaml/code.ml
	cd ocaml
	dune build ocaml/code.bc.js

public_assets: public/*
	cp -r public/* $(DEST)

server: node_modules/.bin/http-server
	node_modules/.bin/http-server $(DEST)

node_modules/.bin/http-server:
	npm install http-server

clean:
	rm -rf $(DEST) node_modules package-lock.json
	dune clean
