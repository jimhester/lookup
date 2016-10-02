parse_internal <- function(name, path) {
  lines <- r_github_content(path)
  start <- grep(paste0("SEXP[[:space:]]+attribute_hidden[[:space:]]+", name, "\\([^)(]+)[[:space:]]*"), lines)
  if (length(start)) {
    length <- find_function_end(lines[seq(start, length(lines))])
    if (!is.na(length)) {
      end <- start + length - 1
      Compiled(path = path, start = start, end = end, content = paste(lines[seq(start, end)], collapse = "\n"), type = "c")
    }
  }
}

internal_source <- function(x) {
  response <- gh("/search/code", q = paste("in:file", "repo:wch/r-source", "path:src/main", "language:c", x))
  paths <- vapply(response$items, `[[`, character(1), "path")
  compact(lapply(paths, function(path) {
    parse_internal(x, path)
  }))
}

lookup_internal <- function(fun, envir = parent.frame(), ...) {
  browser()
  map <- names_map()
  source_name <- map[fun$name]

  internal_source(source_name)
}
