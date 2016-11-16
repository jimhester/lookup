with_package <- function(f, ...) {
  memoise::memoise(function(name, code) {
    tryCatch(unloadNamespace(name), error = function(e) NULL)
    lib <- tempfile(tmpdir = ".cache")
    dir.create(lib)
    libpath <- .libPaths()
    on.exit(.libPaths(libpath), add = TRUE)
    .libPaths(c(lib, .libPaths()))
    f(name, ...)
    force(code)
  }, cache = memoise::cache_filesystem(".cache"))
}
with_cran_package <- with_package(function(...) {
  old <- options(repos = c(CRAN = "https://cloud.r-project.org"))
  on.exit(options(old))
  install.packages(..., quiet = TRUE)
})

with_local_package <- with_package(devtools::install, repo = NULL, type = "source", quiet = TRUE)

with_github_package <- with_package(devtools::install_github, quiet = TRUE)

# Counter is used if server state has changed (PUT, POST etc)
.counter <- 0
gh <<- memoise::memoise(environment(gh)[["_f"]], cache = memoise::cache_filesystem(".cache"), ~ .counter)
r_github_content <<- memoise::memoise(environment(r_github_content)[["_f"]], cache = memoise::cache_filesystem(".cache"), ~ .counter)
package_github_content <<- memoise::memoise(environment(package_github_content)[["_f"]], cache = memoise::cache_filesystem(".cache"), ~ .counter)
