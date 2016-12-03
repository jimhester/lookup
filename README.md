# lookup

> Lookup R function definitions, including compiled code, S3 and S4 methods.

[![Travis-CI Build Status](https://travis-ci.org/jimhester/lookup.svg?branch=master)](https://travis-ci.org/jimhester/lookup)
[![Coverage Status](https://img.shields.io/codecov/c/github/jimhester/lookup/master.svg)](https://codecov.io/github/jimh
ester/lookup?branch=master)

![Imgur](http://i.imgur.com/hiMtsWD.jpg)

## Installation
```r
# install.packages("devtools")
devtools::install_github("jimhester/lookup")
```
## Example

### Normal Functions (with compiled code)
![Imgur](http://i.imgur.com/TjyfFFU.png)

### S3 generics and methods
![Imgur](http://i.imgur.com/u4XM6NX.png)

### S4 generics and methods
![Imgur](http://i.imgur.com/kMEVDnv.png)

### In RStudio IDE
![Imgur](http://i.imgur.com/8iH3FdB.png)

## Setup

lookup makes heavy use of the [GitHub API](https://developer.github.com/v3/),
which has a rate limit of [60 requests per
hour](https://developer.github.com/v3/#rate-limiting) when unauthenticated. You
can create a [Personal access token](https://github.com/settings/tokens) with
no scope, which will increase your limit to 5000 requests per hour.

Once you have generated a token, add it to your `~/.Renviron` file or shell
startup file and it will be automatically used for further requests.
```
GITHUB_PAT=7d8d0436835d1baXYZ1234
```
`gh::gh("/rate_limit")` can be used to query your current usage and limits.

## Default printing
lookup can be used as a complete replacement for function printing by attaching
the package. To make this the default simply add this to your `.Rprofile`.
```r
if (interactive()) {
  library(lookup)
}
```

# How this works

If a base R function is printed that calls compiled code the code is lookup up
using the [R git mirror](https://github.com/wch/r-source). If a CRAN package
has compiled code it is looked up on the [CRAN git
mirror](https://github.com/cran). If a package is installed with
`devtools::install_github()` or `devtools::install()` the remote or local
repository location is searched for the code.

This has been tested to work with `.Internal`, `.External`, `.Call` and
[Rcpp](https://github.com/RcppCore/Rcpp) calls.

## Issues ##
This package uses a number of heuristics to find function definitions. This
means it can fail in some cases, if you find a function that is not lookup up
properly, please [open an issue](https://github.com/jimhester/lookup/issues).

## Thanks ##
- [R Core and Community](https://www.r-project.org) For promoting open source software to make this possible.
- [Winston Chang](https://github.com/wch) For running the [Git mirror of the R Source](https://github.com/wch/r-source).
- [Gábor Csárdi](https://github.com/gaborcsardi) For the [gh](https://github.com/r-pkgs/gh) package, the [CRAN git mirror](https://github.com/cran) and inspiration and code for handling pagination and busy updating.
- [Jenny Bryan](https://github.com/jennybc) For codifying the process of [accessing the R source](https://github.com/jennybc/access-r-source), which was my main inspiration and motivation for starting this package.
