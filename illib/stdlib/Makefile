include ../../ilconfigfile

ifeq ($(BOOTSTRAPFLAG),true)
CC=$(VM) ../../ocamil_bs.exe #-ildebug -noCLIexception
else
CC=../../ocamil #-ildebug -noCLIexception
endif

ifeq ($(OLDCAMIL),true)
COMPFLAGS=-rebuildtypes
LEXTABLES_IMPL=object[]
LEXBUF_IMPL=object[]
JACARELIB=
else
LEXTABLES_IMPL=class Lexing.lex_tables
LEXBUF_IMPL=class Lexing.lexbuf
JACARELIB=jacare.cmx ctsHierarchy.cmx ctsArray.cmx
endif

OCAMLDEP=ocamldep #-native

FILES= 	pervasives.cmx CLIinteraction.cmx array.cmx list.cmx char.cmx string.cmx \
	sys.cmx complex.cmx buffer.cmx map.cmx obj.cmx queue.cmx sort.cmx \
	hashtbl.cmx int32.cmx int64.cmx nativeint.cmx printf.cmx digest.cmx random.cmx camlinternalOO.cmx set.cmx \
	filename.cmx format.cmx arg.cmx stack.cmx printexc.cmx \
	weak.cmx gc.cmx lexing.cmx parsing.cmx \
	lazy.cmx marshal.cmx oo.cmx genlex.cmx stream.cmx  scanf.cmx \
	arrayLabels.cmx listLabels.cmx stringLabels.cmx stdLabels.cmx moreLabels.cmx $(JACARELIB)

all: core_camil.dll graphics.dll threads.dll dynlink.dll

lexing.ml: lexing.mlp
	sed -e "s|%%LEXTABLES_IMPL%%|$(LEXTABLES_IMPL)|" -e "s|%%LEXBUF_IMPL%%|$(LEXBUF_IMPL)|" lexing.mlp > lexing.ml

sys.ml: sys.mlp
	sed -e "s|%%OSTYPE%%|$(OSTYPE)|" sys.mlp > sys.ml

pervasives.cmi: pervasives.mli
	$(CC) $(COMPFLAGS) -c -nopervasives -nostdlib -ccl pervasives.mli

pervasives.cmx: pervasives.ml
	$(CC) $(COMPFLAGS)  -c -nopervasives -nostdlib -ccl pervasives.ml

arrayLabels.cmx: arrayLabels.cmi
	$(CC) $(COMPFLAGS) -c -nostdlib -ccl -nolabels arrayLabels.ml
listLabels.cmx: listLabels.cmi
	$(CC) $(COMPFLAGS) -c -nostdlib -ccl -nolabels listLabels.ml
stringLabels.cmx: stringLabels.cmi
	$(CC) $(COMPFLAGS) -c -nostdlib -ccl -nolabels stringLabels.ml
stdLabels.cmx: stdLabels.cmi
	$(CC) $(COMPFLAGS) -c -nostdlib -ccl -nolabels stdLabels.ml
moreLabels.cmx: moreLabels.cmi
	$(CC) $(COMPFLAGS) -c -nostdlib -ccl -nolabels moreLabels.ml

graphics.dll:  graphics.cmx
	$(CC) $(COMPFLAGS)  -key $(SNKEY) -a -o graphics.dll graphics.cmx

dynlink.dll: dynlink.cmx
	$(CC) $(COMPFLAGS)  -key $(SNKEY) -a -o dynlink.dll dynlink.cmx

core_camil.dll: ${FILES}
	$(CC) $(COMPFLAGS)  -key $(SNKEY) -nostdlib -ccl -a -o core_camil.dll -inlineIL core.il $(FILES) #>/dev/null

graphics.cmx:graphics.ml
	$(CC) $(COMPFLAGS) -c graphics.ml

dynlink.cmx:dynlink.ml
	$(CC) $(COMPFLAGS) -c dynlink.ml

clean::
	rm -f dynlink.cm*
	rm -f graphics.cm*

threads/thread.cmx:threads/thread.ml
	$(CC) $(COMPFLAGS) -I threads/ -c threads/thread.ml

threads/mutex.cmx:threads/mutex.ml
	$(CC) $(COMPFLAGS) -I threads/ -c threads/mutex.ml

threads/condition.cmx:threads/condition.ml
	$(CC) $(COMPFLAGS) -I threads/ -c threads/condition.ml

threads/event.cmx:threads/event.ml
	$(CC) $(COMPFLAGS) -I threads/ -c threads/event.ml

threads.dll: threads/thread.cmx threads/mutex.cmx threads/condition.cmx threads/event.cmx
	$(CC) $(COMPFLAGS) -I threads/ -key $(SNKEY) -a -o threads.dll threads/thread.cmx threads/mutex.cmx threads/condition.cmx threads/event.cmx

clean::
	(cd threads; rm -f *.cm*)

install:
	cp *.dll $(CAMILSTDLIB)/
	cp *.cm* $(CAMILSTDLIB)/
	cp threads/*.cm* $(CAMILSTDLIB)/

clean::
	for i in `ls ${FILES}`; do \
	  u=`basename $$i .cmx`; \
	  rm -f $$u.cmx ;\
	  rm -f $$u.cmi ;\
	done
	rm -f *.dll
	rm -f lexing.ml
	rm -f *.il

.SUFFIXES: .mli .cmi .ml .cmx

.mli.cmi:
	$(CC) $(COMPFLAGS) -I threads -c -nostdlib -ccl $<
.ml.cmx:
	$(CC) $(COMPFLAGS)  -c -nostdlib -ccl  $<


.depend:	
	$(OCAMLDEP) -native -I threads *.mli *.ml  threads/*.mli threads/*.ml > .depend

include .depend
