![Travis-CI Build Status](https://travis-ci.org/jimhester/lookup.svg?branch=master)](https://travis-ci.org/jimhester/lookup)

# Installation
```r
# install.packages("devtools")
devtools::install_github("jimhester/lookup")
```

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
`gh::gh("/rate_limit"))` can be used to query your limits.

# Drop in replacement for function printing
Add these in your `~/.RProfile`.
```r
print.function <- function(x, ...) { print(lookup::lookup(x, ...)) }
setMethod("show", "genericFunction", function(object) print(lookup::lookup(object))
```

## Uses heuristics ##
This package uses heuristics to find function definitions. This means it can
miss some cases, if you find a function that is not lookup up properly, please
[open an issue](https://github.com/jimhester/lookup/issues).

[1]: https://developer.github.com/v3/#rate-limiting
