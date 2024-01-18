
watch:
	dune exec fei -w -- --path=../test.epub

run:
	dune exec fei -- --path=../test.epub

build:
	dune build -w
