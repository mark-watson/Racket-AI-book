.PHONY: all run test clean

all: libwebkit_racket.dylib

libwebkit_racket.dylib: webkit_racket.m webkit_racket.h
	clang -dynamiclib -fobjc-arc -O2 -framework Cocoa -framework WebKit \
	      -o libwebkit_racket.dylib webkit_racket.m

run: libwebkit_racket.dylib
	racket main.rkt

test:
	raco test tests.rkt

clean:
	rm -rf compiled libwebkit_racket.dylib *.rkt~
