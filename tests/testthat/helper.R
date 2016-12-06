with_package <- function(f, ...) {
  install_f <- memoise::memoise(function(name) {
    tryCatch(unloadNamespace(name), error = function(e) NULL)
    lib <- tempfile(tmpdir = ".cache")
    dir.create(lib)
    .libPaths(c(lib, .libPaths()))
    f(name, ...)
    lib
  }, ~ f, cache = memoise::cache_filesystem(".cache"))
  function(name, code) {
    old <- .libPaths()
    .libPaths(c(install_f(name), old))
    force(code)
    on.exit(.libPaths(old))
  }
}
with_cran_package <- with_package(function(...) {
  old <- options(repos = c(CRAN = "https://cloud.r-project.org"))
  on.exit(options(old))
  install.packages(..., quiet = TRUE)
})

with_local_package <- with_package(devtools::install, repo = NULL, type = "source", quiet = TRUE)

with_github_package <- with_package(devtools::install_github, quiet = TRUE)

# Counter is used if server state has changed (PUT, POST etc)
# counter <- 0
gh_mock <- testthat:::mock(name = "gh", env = asNamespace("lookup"), new = memoise::memoise(environment(gh)[["_f"]], cache = memoise::cache_filesystem(".cache")))
testthat:::set_mock(gh_mock)
