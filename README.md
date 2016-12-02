# lookup

> Lookup function definitions, compiled, S3, S4 methods

[![Travis-CI Build Status](https://travis-ci.org/jimhester/lookup.svg?branch=master)](https://travis-ci.org/jimhester/lookup)

![Lookup](http://i.giphy.com/l2Je66zG6mAAZxgqI.gif)

## Installation
```r
# install.packages("devtools")
devtools::install_github("jimhester/lookup")
```

## Setup

lookup makes heavy use of the [GitHub API](https://developer.github.com/v3/),
which has a rate limit of 60 requests per hour on unauthenticated requests [1]. Therefore
you should create a [Personal access
token](https://github.com/settings/tokens) with no scope, which will increase
your limit to 5000 per minute.

Once you have generated a token, add it to your `~/.Renviron` file or shell
startup and it will be automatically used for further requests.
```
GITHUB_PAT=7d8d0436835d1baXYZ1234
```
`gh::gh("/rate_limit")` can be used to query your limits.

## Default printing
lookup can be used as a complete replacement for function printing by attaching
the package. To make this the default simply add this to your `.Rprofile`.
```r
if (interactive()) {
  library(lookup)
}
```

## Example
```r
# Normal functions
lookup(sample)

# Normal functions with calls to compiled code
lookup(body)

# S3 generics
lookup(summary)

# S4 generics
lookup(show)
``````

## Issues ##
This package uses a number of heuristics to find function definitions. This means it can
miss some cases, if you find a function that is not lookup up properly, please
[open an issue](https://github.com/jimhester/lookup/issues).

## Thanks ##
[Gábor Csárdi](https://github.com/gaborcsardi) for [gh](https://github.com/r-pkgs/gh) and inspiration and code for handling pagination and busy updating.
[Jenny Bryan](https://github.com/jennybc) For codifying the process of
[accessing the R source](https://github.com/jennybc/access-r-source), my main
inspiration and motivation for starting this package.

[1]: https://developer.github.com/v3/#rate-limiting
