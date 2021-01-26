# serpico2: seroprevalence of SARS-CoV-2
This repository contains R and Stan reproducible code for manuscript https://medrxiv.org/cgi/content/short/2020.10.20.20213116v1

## Usage
A vignette built in the package demonstrates the functions to estimate seroprevalence from a simulated dataset. It can be seen [here](http://htmlpreview.github.io/?https://github.com/slevu/serpico2/blob/main/vignettes/vignette.html) without installing the package.

## Installation
From an R session, package can be installed with:
```
devtools::install_github("slevu/serpico2", build_vignettes=TRUE, force=TRUE)
```
## Requirements
The package has been developed and tested with R version 3.6.2 on Windows 7 and MacOSX Version 10.13: "High Sierra" operating systems. The package should be compatible with Windows, Mac, and Linux operating systems.

## Package dependencies
The following packages are needed to execute functions:
```
install.packages(c("rstan", "StanHeaders", "Rcpp", "reshape2"))
```


