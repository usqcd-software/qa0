SFC = $$HOME/Sandbox/esl/working/cee1/sfc -v
CC  = gcc -g -Wall

sources.sf =  \
	sfc.sf \
	common.sf \
        read.sf

sources.ss = \
	ast.ss \
	attr.ss \
	backend.ss \
	basis.ss \
	be-bgl-xlc.ss \
	be-c99-64.ss \
	be-c99.ss \
	be-ckind.ss \
	be-dry.ss \
	c2real.ss \
	cenv.ss \
	cfold.ss \
	cheader.ss \
	cx2dh.ss \
	main-sfc.ss \
	parser.ss \
	q2complex.ss \
	qa0.ss \
	qa0print.ss \
	verbose.ss \

obj.c = $(sources.sf:%.sf=%.c) $(sources.ss:%.ss=%.c)
obj.o = $(obj.c:%.c=%.o)

program = qa0

.PHONY: all clean realclean

all: $(program)

clean:
	$(RM) $(obj.o)

realclean: clean
	$(RM) $(program) $(obj.c)

$(program): $(obj.o)
	$(CC) -o $(program) $(obj.o)

$(obj.o): %.o: %.c
	$(CC) -c -o $@ $<

$(sources.ss:%.ss=%.c): %.c: %.ss
	$(SFC) $<

$(sources.sf:%.sf=%.c): %.c: %.sf
	$(SFC) $<

####
ast.c: sfc.sf
ast.c: common.sf
attr.c: sfc.sf
attr.c: common.sf
attr.c: ast.ss
attr.c: cenv.ss
backend.c: sfc.sf
backend.c: cenv.ss
basis.c: sfc.sf
be-bgl-xlc.c: sfc.sf
be-bgl-xlc.c: common.sf
be-bgl-xlc.c: ast.ss
be-bgl-xlc.c: parser.ss
be-bgl-xlc.c: attr.ss
be-bgl-xlc.c: backend.ss
be-bgl-xlc.c: cx2dh.ss
be-bgl-xlc.c: cenv.ss
be-bgl-xlc.c: cheader.ss
be-bgl-xlc.c: verbose.ss
be-bgl-xlc.c: be-ckind.ss
be-c99-64.c: sfc.sf
be-c99-64.c: common.sf
be-c99-64.c: ast.ss
be-c99-64.c: parser.ss
be-c99-64.c: attr.ss
be-c99-64.c: backend.ss
be-c99-64.c: cx2dh.ss
be-c99-64.c: cenv.ss
be-c99-64.c: cheader.ss
be-c99-64.c: verbose.ss
be-c99-64.c: be-ckind.ss
be-c99.c: sfc.sf
be-c99.c: common.sf
be-c99.c: ast.ss
be-c99.c: parser.ss
be-c99.c: attr.ss
be-c99.c: backend.ss
be-c99.c: cx2dh.ss
be-c99.c: cenv.ss
be-c99.c: cheader.ss
be-c99.c: verbose.ss
be-c99.c: be-ckind.ss
be-ckind.c: sfc.sf
be-ckind.c: common.sf
be-ckind.c: ast.ss
be-ckind.c: parser.ss
be-ckind.c: attr.ss
be-ckind.c: backend.ss
be-ckind.c: cenv.ss
be-ckind.c: cheader.ss
be-ckind.c: verbose.ss
be-dry.c: sfc.sf
be-dry.c: common.sf
be-dry.c: ast.ss
be-dry.c: parser.ss
be-dry.c: attr.ss
be-dry.c: backend.ss
be-dry.c: cx2dh.ss
be-dry.c: cenv.ss
be-dry.c: cheader.ss
be-dry.c: verbose.ss
be-dry.c: be-ckind.ss
c2real.c: sfc.sf
c2real.c: common.sf
c2real.c: ast.ss
c2real.c: cenv.ss
c2real.c: attr.ss
cenv.c: sfc.sf
cenv.c: common.sf
cenv.c: basis.ss
cenv.c: ast.ss
cfold.c: sfc.sf
cfold.c: common.sf
cfold.c: basis.ss
cfold.c: ast.ss
cfold.c: parser.ss
cfold.c: cenv.ss
cheader.c: sfc.sf
cheader.c: common.sf
cheader.c: ast.ss
cheader.c: cenv.ss
cheader.c: attr.ss
cheader.c: verbose.ss
common.c: sfc.sf
common.c: sfc.sf
cx2dh.c: sfc.sf
cx2dh.c: common.sf
cx2dh.c: ast.ss
main-sfc.c: sfc.sf
main-sfc.c: common.sf
main-sfc.c: qa0.ss
parser.c: sfc.sf
parser.c: common.sf
parser.c: read.sf
parser.c: ast.ss
parser.c: cenv.ss
q2complex.c: sfc.sf
q2complex.c: common.sf
q2complex.c: ast.ss
q2complex.c: cenv.ss
q2complex.c: attr.ss
qa0.c: sfc.sf
qa0.c: common.sf
qa0.c: cheader.ss
qa0.c: backend.ss
qa0.c: parser.ss
qa0.c: cfold.ss
qa0.c: q2complex.ss
qa0.c: c2real.ss
qa0.c: cx2dh.ss
qa0.c: qa0print.ss
qa0.c: be-c99.ss
qa0.c: be-c99-64.ss
qa0.c: be-dry.ss
qa0.c: be-bgl-xlc.ss
qa0print.c: sfc.sf
qa0print.c: common.sf
qa0print.c: ast.ss
verbose.c: sfc.sf
read.c: sfc.sf
