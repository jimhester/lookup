fetch_symbol_map <- function(desc, ...) UseMethod("fetch_symbol_map")
parse_symbol_map <- function(desc, ...) UseMethod("parse_symbol_map")
source_files <- function(desc, ...) UseMethod("source_files")
fetch_source <- function(desc, path) UseMethod("fetch_source")
parse_source <- function(desc, search) UseMethod("parse_source")

as.source_type <- function(package, type, search = NULL) {

  desc <- tryCatch(packageDescription(package, lib.loc = .libPaths()), warning = function(e) { stop(as.error(e)) })

  desc_file <- attr(desc, "file")
  if (basename(desc_file) != "package.rds") {
    desc$RemoteType <- "local"
    desc$RemoteUrl <- dirname(package)
  }

  if (desc$Repository %==% "CRAN") {
    desc$RemoteType <- "cran"
  }

  remote_type <- desc$RemoteType %||% "unknown"
  language <- switch(type,
    rcpp = "c++",
    type)

  structure(list(description = desc, search = search, type = type, language = language, remote_type = remote_type),
      class = c(paste0(type, "_", remote_type), type, remote_type))
}

in_map <- function(s, name) {
  !is.na(s$map[name])
}

lookup_function <- function(name, package, type) {
  s <- as.source_type(package, type, name)

  s <- parse_symbol_map(fetch_symbol_map(s))
  if (!in_map(s, name)) {
    return()
  }

  s <- source_files(s, name)
  for (path in s$src_files) {
    s <- parse_source(fetch_source(s, path), s$map[name])
    if (!is.null(s$fun_lines)) {
      return(Compiled(path = s$src_path,
          start = s$fun_start,
          end = s$fun_end,
          content = paste0(s$fun_lines, collapse = "\n"),
          type = s$language))
    }
  }
}

call_name <- function(f, type) {
  type <- as.symbol(type)

  call_calls <- function(x) {
    if (is.name(x) || is.atomic(x)) {
      return(NULL)
    }
    if (identical(x[[1]], type)) {
      return(x)
    }
    for (i in rev(seq_along(x))) {
      ccall <- call_calls(x[[i]])
      if (!is.null(ccall)) {
        return(ccall)
      }
    }
    NULL
  }
  call <- call_calls(body(f))
  as.character(call[[2]][[1]])
}

has_call <- function(f, type) {
  if (!is.function(f) || is.primitive(f)) {
    return(FALSE)
  }
  calls <- codetools::findGlobals(f, merge = FALSE)$functions
  any(calls %in% type)
}
