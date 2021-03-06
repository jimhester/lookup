#' @include rcpp.R
NULL

native_routine_registration_map <- function(s, ...) {
  s2 <- source_files(s, "R_CallMethodDef")
  for (path in s2$src_files) {
    s <- fetch_source(s, path)
    if (any(grepl("R_CallMethodDef", s$src_lines))) {
      m <- regexpr('^[[:space:]]*[{]"([^"]+)",[[:space:]]*[(]DL_FUNC[)][[:space:]]*&([^,]+)', s$src_lines, perl = TRUE)
      res <- na.omit(captures(s$src_lines, m))
      return(setNames(res[[2]], res[[1]]))
    }
  }

  NULL
}

native_routine_registration_map.github <- function(s, ...) {
  for (f in s$src_files) {
    lines <- readLines(f)
    if (any(grepl("R_CallMethodDef", lines))) {
      m <- regexpr('^[[:space:]]*[{]"([^"]+)",[[:space:]]*[(]DL_FUNC[)] &([^,]+)', lines, perl = TRUE)
      res <- na.omit(captures(lines, m))
      return(setNames(res[[2]], res[[1]]))
    }
  }

  NULL
}

parse_symbol_map.call <- function(s) {
  native_routines <- s$map_lines$nativeRoutines
  if (length(native_routines) == 0) {
    s$map <- setNames(s$name, s$name)
  } else if (isTRUE(native_routines[[1]]$useRegistration)) {
    s$map <- native_routine_registration_map(s)
    if (is.null(s$map)) {
      vals <- s$name
      if (length(native_routines[[1]]$registrationFixes) > 0) {
        vals <- sub(paste0("^", native_routines[[1]]$registrationFixes[[1]]), native_routines[[1]]$registrationFixes[[2]], vals)
      }
      s$map <- setNames(vals, s$name)
    }
  } else {
    s$map <- s$map_lines$nativeRoutines[[1]]$symbolNames
  }

  s$search <- s$map[s$name]

  s$regex <- paste0("[[:space:]]+", s$search, "\\([^)]+\\)[^;]*(?:$|\\{)")
  s
}

parse_source.call <- parse_source.rcpp

fetch_symbol_map.call <- function(s, ...) {
  res <- parseNamespaceFile(s$description$Package, dirname(getNamespaceInfo(s$description$Package, "path")), mustExist = FALSE)

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

source_files.call_cran <- function(s, name = s$search, ...) {
  s$src_files <- github_code_search(
    name = name,
    path = "src/",
    owner = "cran",
    repo = s$description$Package,
    language = "c")

  s
}

source_files.call_bioc <- function(s, name = s$search, ...) {
  s$src_files <- github_code_search(
    name = name,
    path = "src/",
    owner = "Bioconductor-mirror",
    repo = s$description$Package,
    language = "c")

  s
}
source_files.call_base <- function(s, name = s$search, ...) {
  s$src_files <- github_code_search(
    name = name,
    path = paths("src/library", s$description$Package, "src"),
    owner = "wch",
    repo = "r-source",
    language = "c")

  s
}
