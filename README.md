<!-- badges: start -->
[![R-CMD-check](https://github.com/CIMES-USNA-UMA/CardioRVARapp/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/CIMES-USNA-UMA/CardioRVARapp/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->



## Description

CardioRVARapp is a shiny app developed by Alvaro Chao-Ecija (intern student at the 
Department of Physiology and the Autonomic Nervous System Unit at CIMES, University of
Malaga) and PhD. Marc Stefan Dawid-Milner (head of the unit and supervisor of the project). 
This app is designed to work with the [CardioRVAR](https://github.com/CIMES-USNA-UMA/CardioRVAR) main package,
allowing a better experience without having to use the commands in R or Rstudio.

## Shiny application

To launch the shiny application, use the following code (packages *shiny* and *CardioRVAR* are required):

```ruby
shiny::runGitHub("CardioRVARapp", "CIMES-USNA-UMA", subdir = "inst/app", launch.browser = TRUE)
```

To install *CardioRVAR*, use the following code:

```ruby
remotes::install_github("CIMES-USNA-UMA/CardioRVAR", build_vignettes = TRUE)
```

## Installation

To install the package, use the following code line in R (package remotes is required):

```ruby
remotes::install_github("CIMES-USNA-UMA/CardioRVARapp", build_vignettes = TRUE)
```

Package *CardioRVAR* will also be installed at the same time.

## Tutorial

A tutorial for this package is available as a vignette. To access the tutorial vignette,
first ensure that packages *knitr* and *rmarkdown* are installed:

```ruby
install.packages("knitr")
install.packages("rmarkdown")
```

Then, install package *CardioRVARapp* as described above (with the option to build vignettes turned on as
described). Then, you can use the following commands to access the tutorial:

```ruby
library(CardioRVARapp)
vignette("CardioRVAR-App-Tutorial")

# Or alternatively:

vignette("CardioRVAR-App-Tutorial", package = "CardioRVARapp")
```
This will show the vignette in the *help* section of *RStudio*. If you want to access
the HTML version, use the following command:

```ruby
browseVignettes("CardioRVARapp")
```

An access for the vignette *CardioRVARapp Tutorial* will be shown. To see the vignette, click on
*HTML*.

## Issues and requests

Please use the following link to create an issue or request:

https://github.com/CIMES-USNA-UMA/CardioRVARapp/issues

## Contact information

Email: alvarochaoecija.rprojects@gmail.com

ORCID: https://orcid.org/0000-0002-2691-6936
