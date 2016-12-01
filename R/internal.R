#' @include rcpp.R
NULL

internal_source <- function(name) {
  structure(list(name = name, type = "internal", language = "c", remote_type = "internal"), class = "internal")
}
fetch_symbol_map.internal <- function(s, branch = "trunk") {
  s$map_lines <- github_content(path = "src/main/names.c", owner = "wch", repo = "r-source", ref = branch)
  s
}
parse_symbol_map.internal <- function(s, name = s$name, ...) {
  lines <- s$map_lines
  m <- regexpr("^\\{\"([^\"]+)\",[[:space:]]*([^,]+)", lines, perl = TRUE)
  res <- na.omit(captures(lines, m))
  s$map <- setNames(res[[2]], res[[1]])
  s$search <- s$map[s$name]
  s$regex <- paste0("SEXP[[:space:]]+attribute_hidden[[:space:]]+", s$search, "\\([^)(]+\\)[[:space:]]*\\{")
  s
}

source_files.internal <- function(s, name = s$search, ...) {
  s$src_files <- github_code_search(name, path = "src/main", language = "c", owner = "wch", repo = "r-source")

  # Ignore the names.c file
  s$src_files <- s$src_files[s$src_files != "src/main/names.c"]
  s
}

fetch_source.internal <- function(s, path, branch = "trunk") {
  s$src_lines <- github_content(path = path, owner = "wch", repo = "r-source", ref = branch)
  s$src_path <- path
  s
}

parse_source.internal <- parse_source.rcpp
