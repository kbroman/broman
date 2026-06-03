# build package documentation
doc:
	R -e 'devtools::document()'

test:
	R -e 'devtools::test()'
