APP=hop_server
TEMPLATES=$(wildcard web/bootstrap/templates/*.xml)
HTML=$(subst web/bootstrap/templates/,web/,$(subst .xml,.html,$(TEMPLATES)))

all: message.ml amqp_spec.ml $(APP).native webpages

webpages: $(HTML) web/bootstrap/css/bootstrap.css

web/bootstrap/css/bootstrap.css: web/bootstrap/less/*.less
	recess --compile web/bootstrap/less/bootstrap.less > $@

web/%.html: web/bootstrap/templates/%.xml web/bootstrap/template.xsl web/bootstrap/nav.xml
	xsltproc web/bootstrap/template.xsl $< > $@

message.ml: messages.json codegen.py
	python codegen.py > $@

amqp_spec.ml: amqp0-9-1.stripped.xml amqp_codegen.py
	python amqp_codegen.py > $@

webclean:
	rm -f $(HTML)

clean: webclean
	ocamlbuild -clean
	rm -f message.ml
	rm -f amqp_spec.ml

veryclean: clean
	rm -f web/bootstrap/css/bootstrap.css

$(APP).native: $(wildcard *.ml)
	ocamlbuild $@

run: all
	./$(APP).native
