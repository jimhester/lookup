#' @useDynLib printr
#' @importFrom Rcpp sourceCpp

find_registration_github <- function(repo, path = "src") {
  response <- gh("/search/code", q = paste("in:file", paste0("repo:", repo), paste0("path:", path), "language:c", "R_CallMethodDef"))
  paths <- vapply(response$items, `[[`, character(1), "path")

  m <- regexpr("^\\{\"([^\"]+)\",[[:space:]]*([^,]+)", x, perl = TRUE)
  res <- na.omit(captures(x, m))
  setNames(res[[2]], res[[1]])
  compact(lapply(paths, function(path) {
    parse_source(x, path)
  }))
}


parse_array_def <- function(x) {
  re <- rex::rex("[]", any_spaces, "=", any_spaces, capture("{", anything))
  parse_array_definition(rex::re_matches(paste(collapse = "\n", x), re, options = "s")[[1]])
}
