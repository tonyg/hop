APP=ocamlmsg

all: message.ml amqp_spec.ml $(APP).native

message.ml: messages.json codegen.py
	python codegen.py > $@

amqp_spec.ml: amqp0-9-1.stripped.xml amqp_codegen.py
	python amqp_codegen.py > $@

clean:
	ocamlbuild -clean
	rm -f message.ml
	rm -f amqp_spec.ml

$(APP).native: $(wildcard *.ml)
	ocamlbuild $@

run: all
	./$(APP).native
