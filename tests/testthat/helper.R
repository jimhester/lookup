with_package <- function(f, ...) {
  function(name, code) {
    unloadNamespace(name)
    lib <- tempfile()
    on.exit(unlink(lib, recursive = TRUE), add = TRUE)
    dir.create(lib)
    libpath <- .libPaths()
    on.exit(.libPaths(libpath), add = TRUE)
    .libPaths(c(lib, .libPaths()))
    f(name, ...)
    force(code)
  }
}
with_cran_package <- with_package(function(...) {
  old <- options(repos = c(CRAN = "https://cloud.r-project.org"))
  on.exit(options(old))
  install.packages(..., quiet = TRUE)
})

with_local_package <- with_package(devtools::install, repo = NULL, type = "source", quiet = TRUE)

with_github_package <- with_package(devtools::install_github, quiet = TRUE)
