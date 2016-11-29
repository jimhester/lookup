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

fetch_symbol_map.call_github <- function(s) {
  res <- github_content(
    path = paths(s$description$RemoteSubdir, "NAMESPACE"),
    owner = s$description$RemoteUsername,
    repo = s$description$RemoteRepo,
    ref = s$description$RemoteRef,
    api_url = s$descripiton$RemoteHost)
  temp_dir <- tempfile()
  dir.create(temp_dir)
  temp_file <- file.path(temp_dir, "NAMESPACE")
  writeLines(res, con = temp_file)
  s$map_lines <- parseNamespaceFile(basename(temp_dir), dirname(temp_dir))
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

source_files.call_github <- function(s, name = s$search, ...) {
  s$src_files <- github_code_search(
    name = name,
    path = paths(s$description$RemoteSubdir, "src"),
    owner = s$description$RemoteUsername,
    repo = s$description$RemoteRepo,
    language = "c",
    api_url = s$description$RemoteHost)

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
