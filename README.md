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
devtools::install_github("CIMES-USNA-UMA/CardioRVAR")
```

## Installation

To install the package, use the following code line in R (package devtools is required):

```ruby
devtools::install_github("CIMES-USNA-UMA/CardioRVARapp")
```

Packages *shiny* and *CardioRVAR* will also be installed at the same time.

## Issues and requests

Please use the following link to create an issue or request:

https://github.com/CIMES-USNA-UMA/CardioRVARapp/issues

## Contact information

Email: alvarochaoecija.rprojects@gmail.com

ORCID: https://orcid.org/0000-0002-2691-6936
