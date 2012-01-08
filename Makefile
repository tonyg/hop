APP=ocamlmsg

all: message.ml $(APP).native

message.ml: messages.json codegen.py
	python codegen.py > $@

clean:
	ocamlbuild -clean
	rm -f message.ml

$(APP).native: $(wildcard *.ml)
	ocamlbuild $@

run: all
	./$(APP).native
