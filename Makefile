# $Id: Makefile,v 1.79 2006/11/10 01:37:30 montela Exp $

# The main Makefile

include config/Makefile
include ilconfigfile

CAMLC=ocamlc -I boot
#CAMLOPT=boot/ocamlrun ./ocamlopt -nostdlib -I stdlib
CAMLOPT=ocamil #-strictorder -noCLIexception  REM : si ildebug penser a editbin /stack:100000000 ...

OJACARE=ojacare.exe -nointerf

ifeq ($(BOOTSTRAPFLAG),true)
CAMLMLI=$(CAMLOPT)
CONFIGMODULE=config_bs.mlp
CAMLYACC=ocamilyacc.exe
CAMLDEP=ocamldep -native
DYNEMITOBJS=ilcomp/reflection.cmo ilcomp/dynemit.cmo
CAMILSTDLIB=$(BSLIBDIR)
ifeq ($(OLDCAMIL),true)
REBUILDFLAG=-rebuildtypes
else
CAMIL_MLI_1=parsing/asttypes.mli parsing/parsetree.mli
CAMIL_MLI_2=ilcomp/il.mli
CAMIL_MLI_3=typing/outcometree.mli
endif

else
BOOTSTRAPFLAG=false
CAMLMLI=$(CAMLC)
CONFIGMODULE=config.mlp
CAMLYACC=ocamlyacc
CAMLDEP=ocamldep #-native
DYNEMITOBJS=ilcomp/dynemit.cmo
CAMILSTDLIB=$(LIBDIR)
endif


YACCFLAGS=-v
CAMLLEX=ocamllex

DEPFLAGS=$(INCLUDES)
CAMLRUN=byterun/ocamlrun
BSCOMPILERNAME=ocamil_bs.exe
BSFRONTENDNAME=_$(BSCOMPILERNAME:.exe=.dll)
SHELL=/bin/sh
MKDIR=mkdir -p
INCLUDES=-I utils -I parsing -I typing -I bytecomp -I asmcomp -I driver \
         -I toplevel -I ilcomp
COMPFLAGS=$(REBUILDFLAG)  -warn-error A $(INCLUDES)
LINKFLAGS=$(REBUILDFLAG) $(INCLUDES) 

OPTUTILS=utils/misc.cmo utils/tbl.cmo utils/config.cmo \
  utils/clflags.cmo utils/terminfo.cmo utils/ccomp.cmo utils/warnings.cmo

BSUTILS=$(OPTUTILS:.cmo=.cmx)

PARSING=parsing/linenum.cmo parsing/location.cmo parsing/longident.cmo \
  parsing/syntaxerr.cmo $(CAMIL_MLI_1) parsing/parser.cmo \
  parsing/lexer.cmo parsing/parse.cmo parsing/printast.cmo

BSPARSING=$(PARSING:.cmo=.cmx)

TYPING=typing/ident.cmo typing/path.cmo \
  $(CAMIL_MLI_2) ilcomp/utils.cmo \
  ilcomp/ilbuild.cmo ilcomp/ilpredef.cmo \
  typing/primitive.cmo typing/types.cmo \
  typing/btype.cmo $(CAMIL_MLI_3) typing/oprint.cmo \
  typing/subst.cmo typing/predef.cmo \
  typing/datarepr.cmo typing/env.cmo \
  typing/typedtree.cmo typing/ctype.cmo \
  typing/printtyp.cmo typing/includeclass.cmo \
  typing/mtype.cmo typing/includecore.cmo \
  typing/includemod.cmo typing/parmatch.cmo \
  typing/typetexp.cmo typing/typecore.cmo \
  typing/typedecl.cmo typing/typeclass.cmo \
  typing/typemod.cmo

BSTYPING=$(TYPING:.cmo=.cmx)

COMP=bytecomp/lambda.cmo ilcomp/typedlambda.cmo bytecomp/printlambda.cmo ilcomp/printtypedlambda.cmo \
  bytecomp/typeopt.cmo bytecomp/switch.cmo bytecomp/matching.cmo \
  bytecomp/translobj.cmo bytecomp/translcore.cmo \
  bytecomp/translclass.cmo bytecomp/translmod.cmo \
  bytecomp/simplif.cmo bytecomp/runtimedef.cmo

BSCOMP=$(COMP:.cmo=.cmx)

ILCOMP= asmcomp/clambda.cmo ilcomp/ctypedlambda.cmo asmcomp/compilenv.cmo \
  asmcomp/closure.cmo \
  ilcomp/printulambda.cmo \
  ilcomp/naming.cmo \
  ilcomp/painfo.cmo \
  ilcomp/ilpath.cmo \
  ilcomp/inst.cmo \
  ilcomp/ilm.cmo \
  ilcomp/printilm.cmo \
  ilcomp/ilmcompile.cmo \
  ilcomp/rebuildtypes.cmo \
  ilcomp/compil.cmo \
  ilcomp/emitil.cmo \
  $(DYNEMITOBJS) \
  asmcomp/asmlink.cmo \
  ilcomp/ilcompile.cmo

BSILCOMP=$(ILCOMP:.cmo=.cmx)

OPTDRIVER= driver/pparse.cmo driver/opterrors.cmo driver/optcompile.cmo \
  driver/optmain.cmo

BSDRIVER=$(OPTDRIVER:.cmo=.cmx)


ILOBJS=$(OPTUTILS) $(PARSING) $(TYPING) $(COMP) $(ILCOMP) $(OPTDRIVER)
BSFRONTENDOBJS=$(BSUTILS) $(BSPARSING) $(BSTYPING) $(BSCOMP) $(BSILCOMP)


#BSILOBJS=$(BSUTILS) $(BSPARSING) $(BSTYPING) $(BSCOMP) $(BSILCOMP) $(BSDRIVER)

TOPLEVEL=bytecomp/symtable.cmx \
  driver/pparse.cmx driver/opterrors.cmx driver/optcompile.cmx \
  toplevel/genprintval.cmx ilcomp/ildynamic.cmx toplevel/toploop.cmx \
  toplevel/trace.cmx toplevel/topdirs.cmx toplevel/topmain.cmx
TOPLEVELSTART=toplevel/topstart.cmx
#TOPLIB=$(BSUTILS) $(BSPARSING) $(BSTYPING) $(BSCOMP) $(BSILCOMP) $(TOPLEVEL)
TOPOBJS=$(TOPLEVEL) $(TOPLEVELSTART)

#
all: camil illib

# The toplevel

ocamiltop: $(BSFRONTENDNAME) $(TOPOBJS) toplevelform.cs
	$(CAMLOPT) $(LINKFLAGS) -ildebug -o ocamiltop.exe -key camil.snk $(BSFRONTENDNAME:.dll=.cmxa) $(TOPOBJS:.mli=.cmx)
	mcs /r:MLTop=ocamiltop.exe /r:System.Drawing.dll /r:System.Windows.Forms.dll /r:illib/stdlib/core_camil.dll toplevelform.cs
	mcs /t:library /out:toplevelform.dll /r:MLTop=ocamiltop.exe /r:System.Drawing.dll /r:System.Windows.Forms.dll /r:illib/stdlib/core_camil.dll toplevelform.cs

partialclean::
	rm -f ocamiltop.exe


.PHONY: install
install:
	cp ocamil $(BINDIR)/ocamil
	mkdir -p $(LIBDIR)
	cd illib/stdlib; $(MAKE) install CAMILSTDLIB=$(LIBDIR)

clean: partialclean cleanlib


.PHONY: bsinstall
bsinstall:
	mkdir -p $(BSPREFIX)
	mkdir -p $(BSBINDIR)
	mkdir -p $(BSLIBDIR)
	cp ocamil_bs.exe _ocamil_bs.dll ocamiltop.exe toplevelform.exe $(BSBINDIR)
	cd illib/stdlib; $(MAKE) install CAMILSTDLIB=$(BSLIBDIR)

clean: partialclean cleanlib


camil: $(ILOBJS)
	$(CAMLC) $(LINKFLAGS) -o ocamil $(ILOBJS)

$(BSFRONTENDNAME): $(BSFRONTENDOBJS)
	$(CAMLOPT) $(LINKFLAGS) -a -o $(BSFRONTENDNAME) -key camil.snk $(BSFRONTENDOBJS:.mli=.cmx)

bscamil: $(BSFRONTENDNAME) $(BSDRIVER)
	$(CAMLOPT) $(LINKFLAGS) -o $(BSCOMPILERNAME) -key camil.snk $(BSFRONTENDNAME:.dll=.cmxa) $(BSDRIVER:.mli=.cmx)

# The configuration file

ilcomp/reflection.ml: ilcomp/reflection.idl
	$(OJACARE) ilcomp/reflection.idl

partialclean::
	(cd ilcomp;rm -f reflection.ml*)

ilcomp/painfo.ml: ilcomp/painfo.mlp ilcomp/painfo_bs.mlp
ifeq ($(BOOTSTRAPFLAG),true)
	#cp ilcomp/painfo_bs.mlp ilcomp/painfo.ml
	sed -e "s|%EXEC%|$(VM) $(BINDIR)/painfo.exe|" < ilcomp/painfo_bs.mlp > ilcomp/painfo.ml
else 
	#cp ilcomp/painfo.mlp ilcomp/painfo.ml
	sed -e "s|%EXEC%|$(VM) $(BINDIR)/painfo.exe|" < ilcomp/painfo.mlp > ilcomp/painfo.ml
endif

partialclean::
	rm -f ilcomp/painfo.ml


ilcomp/dynemit.mli: ilcomp/dynemit_dummy.mli ilcomp/dynemit_bs.mli
ifeq ($(BOOTSTRAPFLAG),true)
	cp ilcomp/dynemit_bs.mli ilcomp/dynemit.mli
else 
	cp ilcomp/dynemit_dummy.mli ilcomp/dynemit.mli
endif

partialclean::
	rm -f ilcomp/dynemit.mli

ilcomp/dynemit.ml: ilcomp/dynemit.mlp ilcomp/dynemit_bs.mlp
ifeq ($(BOOTSTRAPFLAG),true)
	cp ilcomp/dynemit_bs.mlp ilcomp/dynemit.ml
else 
	cp ilcomp/dynemit.mlp ilcomp/dynemit.ml
endif

partialclean::
	rm -f ilcomp/dynemit.ml

# The configuration file

utils/config.ml: utils/config.mlp utils/config_bs.mlp utils/config.cmi ilconfigfile
	@rm -f utils/config.ml
	sed -e 's|%%LIBDIR%%|$(CAMILSTDLIB)|' \
            -e 's|%%CCOMPTYPE%%|cc|' \
            -e 's|%%BYTECC%%|$(BYTECC) $(BYTECCCOMPOPTS) $(SHAREDCCCOMPOPTS)|' \
            -e 's|%%BYTELINK%%|$(BYTECC) $(BYTECCLINKOPTS)|' \
            -e 's|%%NATIVECC%%|$(NATIVECC) $(NATIVECCCOMPOPTS)|' \
            -e 's|%%NATIVELINK%%|$(NATIVECC) $(NATIVECCLINKOPTS)|' \
            -e 's|%%PARTIALLD%%|ld -r $(NATIVECCLINKOPTS)|' \
            -e 's|%%BYTECCLIBS%%|$(BYTECCLIBS)|' \
            -e 's|%%NATIVECCLIBS%%|$(NATIVECCLIBS)|' \
            -e 's|%%RANLIBCMD%%|$(RANLIBCMD)|' \
            -e 's|%%BINUTILS_NM%%|$(BINUTILS_NM)|' \
            -e 's|%%BINUTILS_OBJCOPY%%|$(BINUTILS_OBJCOPY)|' \
            -e 's|%%ARCH%%|$(ARCH)|' \
            -e 's|%%MODEL%%|$(MODEL)|' \
            -e 's|%%SYSTEM%%|$(SYSTEM)|' \
            -e 's|%%EXT_OBJ%%|.o|' \
            -e 's|%%EXT_ASM%%|.s|' \
            -e 's|%%EXT_LIB%%|.a|' \
            -e 's|%%EXT_DLL%%|.so|' \
            -e 's|%%CAMILBINDIR%%|$(BINDIR)|' \
	    -e 's|%%BOOTSTRAP%%|$(BOOTSTRAPFLAG)|' \
            utils/$(CONFIGMODULE) > utils/config.ml
	@chmod -w utils/config.ml

partialclean::
	rm -f utils/config.ml

beforedepend:: utils/config.ml

# The parser

parsing/parser.mli parsing/parser.ml: parsing/parser.mly
	(cd parsing;$(CAMLYACC) $(YACCFLAGS) parser.mly)

partialclean::
	rm -f parsing/parser.mli parsing/parser.ml parsing/parser.output

beforedepend:: parsing/parser.mli parsing/parser.ml

# The lexer

parsing/lexer.ml: parsing/lexer.mll
	$(CAMLLEX) parsing/lexer.mll

partialclean::
	rm -f parsing/lexer.ml

beforedepend:: parsing/lexer.ml

# The auxiliary lexer for counting line numbers

parsing/linenum.ml: parsing/linenum.mll
	$(CAMLLEX) parsing/linenum.mll

partialclean::
	rm -f parsing/linenum.ml

beforedepend:: parsing/linenum.ml

# The predefined exceptions and primitives

byterun/primitives:
	cd byterun; $(MAKE) primitives

bytecomp/runtimedef.ml: byterun/primitives byterun/fail.h
	(echo 'let builtin_exceptions = [|'; \
	 sed -n -e 's|.*/\* \("[A-Za-z_]*"\) \*/$$|  \1;|p' byterun/fail.h | \
	 sed -e '$$s/;$$//'; \
         echo '|]'; \
         echo 'let builtin_primitives = [|'; \
         sed -e 's/.*/  "&";/' -e '$$s/;$$//' byterun/primitives; \
	 echo '|]') > bytecomp/runtimedef.ml

partialclean::
	rm -f bytecomp/runtimedef.ml

beforedepend:: bytecomp/runtimedef.ml

partialclean::
	rm -f utils/*.cm[iox] utils/*.[so] utils/*~
	rm -f parsing/*.cm[iox] parsing/*.[so] parsing/*~
	rm -f typing/*.cm[iox] typing/*.[so] typing/*~
	rm -f bytecomp/*.cm[iox] bytecomp/*.[so] bytecomp/*~
	rm -f asmcomp/*.cm[iox] asmcomp/*.[so] asmcomp/*~
	rm -f driver/*.cm[iox] driver/*.[so] driver/*~
#ifeq ($(BOOTSTRAPFLAG),true)
	rm -f toplevel/*.cm[iox] toplevel/*.[so] toplevel/*~
#endif
	rm -f tools/*.cm[iox] tools/*.[so] tools/*~
	rm -f ilcomp/*.cm[iox] ilcomp/*.[so] ilcomp/*~	
	rm -f *~
	rm -f *.dll

# The library
.PHONY: illib
illib:	 
	cd illib/core; $(MAKE) SNKEY=$(SNKEY) \
		MSCORLIBTOKEN=$(MSCORLIBTOKEN) \
		MSCORLIBVER=$(MSCORLIBVER) \
		SYSTEMDRAWINGTOKEN=$(SYSTEMDRAWINGTOKEN) \
		OLDCAMIL=$(OLDCAMIL)
	cd illib/stdlib; export BOOTSTRAPFLAG;$(MAKE) SNKEY=$(SNKEY) \
		MSCORLIBTOKEN=$(MSCORLIBTOKEN) \
		MSCORLIBVER=$(MSCORLIBVER) \
		OLDCAMIL=$(OLDCAMIL)

.PHONY: illib_uni
illib_uni:	 
	cd illib/core; $(MAKE) core_uni.il SNKEY=$(SNKEY) \
		MSCORLIBTOKEN=$(MSCORLIBTOKEN) \
		MSCORLIBVER=$(MSCORLIBVER) \
		SYSTEMDRAWINGTOKEN=$(SYSTEMDRAWINGTOKEN) \
		OLDCAMIL=$(OLDCAMIL)
	cd illib/stdlib_uni; export BOOTSTRAPFLAG;$(MAKE) SNKEY=$(SNKEY) \
		MSCORLIBTOKEN=$(MSCORLIBTOKEN) \
		MSCORLIBVER=$(MSCORLIBVER) \
		OLDCAMIL=$(OLDCAMIL)
cleanlib:
	cd illib/core; $(MAKE) clean
	cd illib/stdlib; $(MAKE) clean

# Default rules

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) $(COMPFLAGS) -c $<

.mli.cmi:
	$(CAMLMLI) $(COMPFLAGS) -c $<

.ml.cmx:
	$(CAMLOPT) $(COMPFLAGS) -c $<

.depend:
	(for d in utils parsing typing bytecomp ilcomp asmcomp driver toplevel; \
         do $(CAMLDEP) $(DEPFLAGS) $$d/*.mli $$d/*.ml; \
         done) > .depend

alldepend:: .depend

FORCE:

include .depend
