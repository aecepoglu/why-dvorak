BCJS = _build/default/ocaml/*.bc.js
DEST = docs/

deploy: $(BCJS) public_assets dest
	cp -r $(BCJS) $(DEST)

$(BCJS): ocaml/code.ml
	cd ocaml
	dune build ocaml/code.bc.js

public_assets: public/* dest
	cp -r public/* $(DEST)
	firefox localhost:8080

dest:
	mkdir -p $(DEST)

server: node_modules/.bin/http-server
	node_modules/.bin/http-server $(DEST)

node_modules/.bin/http-server:
	npm install http-server

clean:
	rm -rf $(DEST) node_modules package-lock.json
	dune clean
