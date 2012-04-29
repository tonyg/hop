APP=ocamlmsg

all: message.ml amqp_spec.ml $(APP).native web/bootstrap/css/bootstrap.css

web/bootstrap/css/bootstrap.css: web/bootstrap/less/*.less
	recess --compile web/bootstrap/less/bootstrap.less > $@

message.ml: messages.json codegen.py
	python codegen.py > $@

amqp_spec.ml: amqp0-9-1.stripped.xml amqp_codegen.py
	python amqp_codegen.py > $@

clean:
	ocamlbuild -clean
	rm -f message.ml
	rm -f amqp_spec.ml

veryclean: clean
	rm -f web/bootstrap/css/bootstrap.css

$(APP).native: $(wildcard *.ml)
	ocamlbuild $@

run: all
	./$(APP).native
