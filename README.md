![travis](https://travis-ci.org/bowerth/estate.svg?branch=master)

# estate

## Test

Create URL using createUrl(), open in browser and save web page only to html
file in `/inst/extdata/papfr-40.html`, for
example
[http://www.pap.fr/](http://www.pap.fr/annonce/appartement-a-vendre-paris-75-g439-40-annonces-par-page-2).

`$ Rscript -e 'devtools::document();devtools::install();devtools::test()'`

- activate downloading in vignette (`eval=TRUE`)

`$ cd vignettes && Rscript -e 'rmarkdown::render("estate.Rmd")'`

vignette using data in install location

`$ Rscript -e 'devtools::install()`

## Update Website

- deactivate downloading in vignette (`eval=FALSE`)

`$ Rscript -e 'pkgdown::build_articles()'`

## Serve Website

`cd docs && python -m SimpleHTTPServer`

## synchronise remote website

`git add docs/articles/estate.html`
`git add docs/articles/estate_files/`
