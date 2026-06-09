## R/broman <a href="https://github.com/kbroman/broman"><img src="https://kbroman.org/broman/broman_logo.png" align="right" height="138" alt="R/broman logo"/></a>

[![R-CMD-check](https://github.com/kbroman/broman/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/kbroman/broman/actions/workflows/R-CMD-check.yaml)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/broman)](https://cran.r-project.org/package=broman)
[![r-universe badge](https://kbroman.r-universe.dev/broman/badges/version)](https://kbroman.r-universe.dev/broman)
[![zenodo DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.2636796.svg)](https://doi.org/10.5281/zenodo.2636796)

[Karl W Broman](https://kbroman.org)

---

R/broman is an [R](https://www.r-project.org) package with miscellaneous R functions that are
useful to me.

---

### Installation

Install the R/broman package from [CRAN](https://cran.r-project.org):

```r
install.packages("broman")
```

Alternatively, install it from [R
universe](https://kbroman.r-universe.dev):

```r
install.packages("broman", repos=c("https://kbroman.r-universe.dev",
                                    "https://cloud.r-project.org"))
```

Or use [remotes](https://remotes.r-lib.org) to install it from its GitHub source:

```r
install.packages("remotes")
remotes::install_github("kbroman/broman")
```



---

### Vignette


A vignette describing the use of the package is available
[on the web](https://kbroman.org/broman/broman.html).
Or view it from within R by loading the package and then using the
`vignette()` function.

```r
library(broman)
vignette("broman", package="broman")
```

---

### License

This package is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License, version 3, as
published by the Free Software Foundation.

This program is distributed in the hope that it will be useful, but
without any warranty; without even the implied warranty of
merchantability or fitness for a particular purpose.  See the GNU
General Public License for more details.

A copy of the GNU General Public License, version 3, is available at
<https://www.r-project.org/Licenses/GPL-3>
