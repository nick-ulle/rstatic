# Generic Makefile that can live in the same directory as an R package.

PKGNAME = $(shell awk '{if(/Package:/) print $$2}' DESCRIPTION)
VERSION = $(shell awk '{if(/Version:/) print $$2}' DESCRIPTION)
PKG = $(PKGNAME)_$(VERSION).tar.gz

# Helpful for debugging:
$(info R package is: $(PKG))

RFILES = $(wildcard R/*.R)
TESTFILES = $(wildcard tests/testthat/test*.R)
VIGNETTES = $(wildcard vignettes/*.Rmd)

# User local install
install: $(RFILES) DESCRIPTION
	R -e "roxygen2::roxygenize()"
	R CMD INSTALL .

test: $(TESTFILES)
	make install
	cd tests && Rscript testthat.R && cd ..

$(PKG): $(RFILES) $(TESTFILES) $(VIGNETTES) DESCRIPTION
	rm -f $(PKG)  # Otherwise it's included in build
	R CMD build .

check: $(PKG)
	R CMD check $(PKG) --as-cran

vignettes: $(VIGNETTES)
	make install
	R -e "tools::buildVignettes(dir = '.')"

clean:
	rm -rf vignettes/*.html $(PKG) *.Rcheck
