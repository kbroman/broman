# build package documentation
all: doc vignette
.PHONY: doc test vignette

doc:
	R -e 'devtools::document()'

test:
	R -e 'devtools::test()'

vignette: docs/broman.html

docs/broman.html: vignettes/broman.Rmd
	[ -d docs ] || mkdir docs
	R $(R_OPTS) -e "rmarkdown::render('$<')"
	mv vignettes/broman.html $@
