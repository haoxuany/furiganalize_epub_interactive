
watch:
	dune exec fei -w -- -i ../test.epub -o output.epub -d local.dic

run:
	dune exec fei -- -i ../test.epub -o output.epub -d local.dic

build:
	dune build -w
