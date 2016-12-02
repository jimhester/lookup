#' @importFrom codetools findGlobals
NULL

fetch_symbol_map <- function(s, ...) UseMethod("fetch_symbol_map") # s$map_lines
parse_symbol_map <- function(s, ...) UseMethod("parse_symbol_map") # s$map, s$regex
source_files <- function(s, ...) UseMethod("source_files") # s$src_files
fetch_source <- function(s, path) UseMethod("fetch_source") # s$src_path, s$src_lines
parse_source <- function(s, search) UseMethod("parse_source") # s$fun_start, s$fun_end, s$fun_lines

fetch_symbol_map.unknown <- function(s, ...) s
parse_symbol_map.unknown <- function(s, ...) s
source_files.unknown <- function(s, ...) s
fetch_source.unknown <- function(s, path) s
parse_source.unknown <- function(s, path) s

as.source_type <- function(package, type, name = NULL) { # s$search, s$type, s$language, s$remote_type, class(s)

  desc <- tryCatch(packageDescription(package, lib.loc = .libPaths()), warning = function(e) { stop(as.error(e)) })

  desc_file <- attr(desc, "file")
  if (basename(desc_file) != "package.rds") {
    desc$RemoteType <- "local"
    desc$RemoteUrl <- dirname(package)
  }

  if (desc$Priority %==% "base") {
    desc$RemoteType <- "base"
  } else if (desc$Repository %==% "CRAN") {
    desc$RemoteType <- "cran"
  }

  remote_type <- desc$RemoteType %||% "unknown"
  language <- switch(type,
    rcpp = "c++",
    external =,
    internal =,
    call = "c",
    type)

  structure(list(description = desc, name = name, type = type, language = language, remote_type = remote_type),
      class = c(paste0(type, "_", remote_type), type, remote_type))
}

in_map <- function(s, name) {
  !is.na(s$map[name])
}

lookup_function <- function(name, type, package = NULL) {
  if (type == "internal") {
    s <- internal_source(name)
  } else {
    s <- as.source_type(package, type, name)
  }

  s <- parse_symbol_map(fetch_symbol_map(s))
  if (!in_map(s, name)) {
    return()
  }

  s <- source_files(s, s$search)
  for (path in s$src_files) {
    s <- parse_source(fetch_source(s, path), s$regex)
    if (!is.null(s$fun_lines)) {
      return(Compiled(
          name = s$search,
          path = s$src_path,
          start = s$fun_start,
          end = s$fun_end,
          content = paste0(s$fun_lines, collapse = "\n"),
          language = s$language,
          type = s$type,
          remote_type = s$remote_type))
    }
  }
}

call_names <- function(f, type, subset = 1) {
  calls <- character()
  i <- 0

  call_calls <- function(x) {
    if (is.name(x) || is.atomic(x)) {
      return(NULL)
    }
    if (is.function(x)) {
      call_calls(formals(x))
      call_calls(body(x))
      return()
    }
    if (is.call(x)) {
      if(as.character(x[[1]])[[1]] %in% type) {
        calls[[i <<- i + 1]] <<- as.character(x[[subset]])
      }
    }
    for (i in seq_along(x)) {
      call_calls(x[[i]])
    }
  }
  call_calls(body(f))

  calls
}

has_call <- function(f, type) {
  if (!is.function(f) || is.primitive(f)) {
    return(FALSE)
  }
  calls <- findGlobals(f, merge = FALSE)$functions
  any(calls %in% type)
}
