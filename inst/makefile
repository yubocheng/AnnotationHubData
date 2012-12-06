PKG=AnnotationHubRecipes
default: build install

help:
	egrep "^#" makefile | sed "s/^#//"

# --- quickbuild: no vignette
#
quickbuild:
	(cd ../..; R CMD build --no-vignettes $(PKG))


# --- build
#
build:
	(cd ../..; R CMD build --no-vignettes $(PKG))

# --- install
#
install:
	(cd ../..; R CMD install $(PKG))

# --- check
#
check: clean build install
	(cd ../..; R CMD check --no-manual --no-vignettes --no-codoc --no-examples --no-manual  $(PKG))

# --- checkfull
#
checkfull: 
	(cd ../..; R CMD build $(PKG))
	(cd ../..; R CMD check $(PKG))


# --- vanillaTest
# run all the unit tests, in a clean context
#

vanillaTest:  build install
	- rm vanillaTest.out
	R --vanilla < vanillaTest.R > vanillaTest.out 2>&1

# --- vt
# run all the unit tests, in a clean context
#

vt: vanillaTest


# --- checkvig
# check just the vignette
#

checkvig:
	(cd ../..; R CMD check --no-manual --no-codoc --no-tests --no-examples $(PKG))


# --- tangle
# extract the R code from the vignette file
#

tangle:
	(cd ../vignettes; R CMD Stangle $(PKG).Rnw)



# --- sweave
# creates $(PKG).tex, runs all embedded examples
# run this before the pdf target
#
sweave: 
	(cd ../vignettes; R CMD Sweave $(PKG).Rnw --pdf)

# --- pdf
# make and open $(PKG).pdf, the vignette 
#

pdf:  sweave
	(cd ../vignettes; open $(PKG).pdf)



