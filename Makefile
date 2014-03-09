# build package documentation
doc:
	R -e 'library(devtools);document(roclets=c("namespace", "rd"))'
