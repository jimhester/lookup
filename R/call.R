#' @include rcpp.R
parse_symbol_map.call <- function(s) {
  s$map <- s$map_lines$nativeRoutines[[1]]$symbolNames
  s$regex <- s$map[s$search]
  s
}

parse_source.call <- function(s, regex) {
  parse_source.rcpp(s, paste0("[[:space:]]+", regex, "\\([^)]+\\)[^;]*(?:$|\\{)"))
}

fetch_symbol_map.call_local <- function(s, ...) {
  res <- parseNamespaceFile(basename(s$description$Package), dirname(s$description$RemoteUrl), mustExist = FALSE)

  if (length(res$nativeRoutines) == 0) {
    s$map_lines <- character()
  }
  s$map_lines <- res
  s
}

source_files.call_local <-  function(s, ...) {
  path <- s$description$RemoteUrl
  s$src_files <- list.files(file.path(path, "src"),
    pattern = "[.][ch]$",
    ignore.case = TRUE,
    recursive = TRUE,
    full.names = FALSE)

  s$src_files <- file.path("src", s$src_files)
  s
}

  #map <- parse_symbol_map(fetch_symbol_map(desc))

  #files <- list.files(file.path(path, "src"),
    #pattern = "[.][ch]$",
    #ignore.case = TRUE,
    #recursive = TRUE,
    #full.names = TRUE)

#symbol_map.c_source <- function(x, ...) {
  #res <- parseNamespaceFile(basename(x), dirname(x), mustExist = FALSE)

  #if (length(res$nativeRoutines) == 0) {
    #return(character())
  #}

  #res$nativeRoutines[[1]]$symbolNames
#}

#lookup_source.c_call <- function(desc, name) {
  #desc <- as.source_type(package, "c")

  #map <- parse_symbol_map(fetch_symbol_map(desc))

  #files <- list.files(file.path(path, "src"),
    #pattern = "[.][ch]$",
    #ignore.case = TRUE,
    #recursive = TRUE,
    #full.names = TRUE)

  #find_compiled_function(paste0("[[:space:]]+", name, "\\([^)]+\\)[^;]*(?:$|\\{)"), files, "c")
#}
