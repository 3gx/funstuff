all: run

toke.x: toke.hs
	ghc toke.hs -o toke.x

parser.x: parser.hs
	ghc parser.hs -o parser.x

run:  parser.x
	./parser.x
