
# dsclim

<!-- badges: start -->
[![R-CMD-check](https://github.com/dinilu/dsclim/workflows/R-CMD-check/badge.svg)](https://github.com/dinilu/dsclim/actions)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of dsclim is to ease the use of the climate4r package to downscale TraCE21ka and CMIP5 climate data in combination with UERRA reanalysis data.

## Installation

You can install the development version of dsclim like so:

``` r
# install.packages("devtools")
devtools::install_github("dinilu/dsclim")
```

## Links for building the package and the documentation (to be removed at some point)

https://pkgdown.r-lib.org/articles/pkgdown.html

https://bookdown.org/yihui/rmarkdown/rticles-bookdown.html

https://sahirbhatnagar.com/blog/2020/03/03/creating-a-website-for-your-r-package/

## Code to build the package template

This code helps to build the package backbone (including all documentation and github links) for this package and website.

```r
usethis::create_package("dsclim")
usethis::use_readme_md()
usethis::use_gpl3_license()
usethis::use_news_md(open = FALSE)
usethis::use_vignette("package_intro")
usethis::use_git()
gitcreds::gitcreds_set()
usethis::use_github()
usethis::use_github_action_check_release()
usethis::use_pkgdown_github_pages()
pkgdown::build_site()
```

