# Examples/xml/Makefile

srcdir		= .
top_srcdir	= ../..
top_builddir	= ../..

SWIGEXE         = $(top_builddir)/swig
SWIG_LIB_DIR    = $(top_srcdir)/Lib
SWIG_LIB_SET    = env SWIG_LIB=
SWIGINVOKE      = $(SWIG_LIB_SET) $(SWIGTOOL) $(SWIGEXE)

cleanup		= tail +2 \
		  | sed -e 's/ident="ID[0-9A-F]*"//g' \
			-e 's,name="/[^"]*/\([^/]*\.swg\)",name="\1",g'

all-dot-i-files =				\
	error.i					\
	example.i				\
	example_apply.i				\
	example_const.i				\
	example_gif.i				\
	example_inl.i				\
	example_p5.i				\
	example_ro.i				\
	example_title_add.i			\
	example_xml.i				\
	gnarly.i

check:
	for f in $(all-dot-i-files) ; do				\
	  base=`basename $$f .i` ;					\
	  xml=$$base.xml ;						\
	  $(SWIGINVOKE) -xml $$xml ${srcdir}/$$f ;	\
	  cat $$xml | $(cleanup) | diff -c ${srcdir}/$$base.expected-xml - ;	\
	done

clean:
	rm -f *.xml

distclean: clean
	rm -f Makefile

# from here on, non-developers beware!

%.expected-xml : %.i
	$(SWIGINVOKE) -xml tmp-file $^
	cat tmp-file | $(cleanup) > $@
	rm -f tmp-file

all-expected-xml:
	for f in $(all-dot-i-files) ; do \
	  make `basename $$f .i`.expected-xml ; done

all-expected-xml-clean:
	rm -f *.expected-xml

# Examples/xml/Makefile ends here
