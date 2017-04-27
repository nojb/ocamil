#
# $Id: Makefile,v 1.37 2006/11/10 01:37:30 montela Exp $
#

ifeq ($(OLDCAMIL),true)
LEXINGPARSING=lexingparsing0.asm
JACARELIB=/dev/null #dummy...
else
LEXINGPARSING=lexingparsing.asm
JACARELIB=jacare.asm
endif


#core_camil.dll: core.il
#	ilasm /dll /debug /out=core_camil.dll /key=$(SNKEY) core.il
#	@sn -e core_camil.dll pubfile.tmp > /dev/null
#	@sn -t pubfile.tmp | grep is | cut -c21- > core_camil.key
#	@rm -f pubfile.tmp
#	@peverify core_camil.dll

core.il: boxint.asm boxfloat.asm closure.asm constant.asm compare.asm exception.asm string.asm array.asm io.asm obj.asm sys.asm md5.asm hash.asm extern.asm dynamic.asm painfo.asm threads.asm lexingparsing0.asm lexingparsing.asm jacare.asm #
	cat begin-core.asm > core.ilp
	cat constant.asm >> core.ilp
	cat boxint.asm >> core.ilp
	cat boxfloat.asm >> core.ilp
	cat closure.asm >> core.ilp
	cat exception.asm >> core.ilp
	cat algtypes.asm >> core.ilp
	cat compare.asm >> core.ilp
	cat string.asm >> core.ilp
	cat array.asm >> core.ilp
	cat io.asm >> core.ilp
	cat obj.asm >> core.ilp
	cat sys.asm >> core.ilp
	cat md5.asm >> core.ilp
	cat hash.asm >> core.ilp
	cat weak.asm >> core.ilp
	cat gc_ctrl.asm >> core.ilp
	cat graphics.asm >> core.ilp
	cat extern.asm >> core.ilp
	cat dynamic.asm >> core.ilp
	cat painfo.asm >> core.ilp
	cat threads.asm >> core.ilp
	cat $(LEXINGPARSING) >> core.ilp
	cat $(JACARELIB) >> core.ilp
	cat end-core.asm >> core.ilp
	sed -e 's|%%MSCORLIBTOKEN%%|$(MSCORLIBTOKEN)|' \
		-e 's|%%MSCORLIBVER%%|$(MSCORLIBVER)|' \
		-e 's|%%SYSTEMDRAWINGTOKEN%%|$(SYSTEMDRAWINGTOKEN)|'  core.ilp > core.il
	cp core.il ../stdlib/


core_uni.il: boxint.asm boxfloat.asm closure.asm constant.asm compare.asm exception.asm string.asm array.asm ../core_uni/io.asm obj.asm sys.asm md5.asm hash.asm extern.asm dynamic.asm painfo.asm threads.asm lexingparsing.asm #
	cat begin-core.asm > core_uni.ilp
	cat constant.asm >> core_uni.ilp
	cat boxint.asm >> core_uni.ilp
	cat boxfloat.asm >> core_uni.ilp
	cat closure.asm >> core_uni.ilp
	cat exception.asm >> core_uni.ilp
	cat algtypes.asm >> core.ilp
	cat compare.asm >> core_uni.ilp
	cat string.asm >> core_uni.ilp
	cat array.asm >> core_uni.ilp
	cat ../core_uni/io.asm >> core_uni.ilp
	cat obj.asm >> core_uni.ilp
	cat sys.asm >> core_uni.ilp
	cat md5.asm >> core_uni.ilp
	cat hash.asm >> core_uni.ilp
	cat weak.asm >> core_uni.ilp
	cat gc_ctrl.asm >> core_uni.ilp
	cat graphics.asm >> core_uni.ilp
	cat extern.asm >> core_uni.ilp
	cat dynamic.asm >> core_uni.ilp
	cat painfo.asm >> core_uni.ilp
	cat threads.asm >> core_uni.ilp
	cat $(LEXINGPARSING) >> core.ilp
	cat $(JACARELIB) >> core.ilp
	cat end-core.asm >> core_uni.ilp
	sed -e 's|%%MSCORLIBTOKEN%%|$(MSCORLIBTOKEN)|' \
		-e 's|%%MSCORLIBVER%%|$(MSCORLIBVER)|' \
		-e 's|%%SYSTEMDRAWINGTOKEN%%|$(SYSTEMDRAWINGTOKEN)|'  core_uni.ilp > core_uni.il
	cp core_uni.il ../stdlib_uni/

clean::
	@rm -f *~ *.dll *.pdb core.il* core_uni.il*
