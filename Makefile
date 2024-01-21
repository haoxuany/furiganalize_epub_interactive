
watch:
	dune exec fei -w -- --path=../test.epub --local=local.dic

run:
	dune exec fei -- --path=../test.epub --local=local.dic

build:
	dune build -w
