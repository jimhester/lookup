fetch_symbol_map <- function(desc, ...) UseMethod("fetch_symbol_map")
parse_symbol_map <- function(desc, ...) UseMethod("parse_symbol_map")
search_package <- function(desc, name) UseMethod("search_source")
fetch_source <- function(desc, name) UseMethod("fetch_source")
parse_source <- function(desc, path, lines) UseMethod("parse_source")

as.source_type <- function(package, type) {

  desc <- tryCatch(packageDescription(package), warning = function(e) { stop(as.error(e)) })

  desc_file <- attr(desc, "file")
  if (basename(desc_file) != "package.rds") {
    desc$RemoteType <- "local"
    desc$RemoteUrl <- dirname(package)
  }

  if (desc$Repository %==% "CRAN") {
    desc$RemoteType <- "cran"
  }

  remote_type <- desc$RemoteType %||% "unknown"
  class(desc) <- c(paste0(type, "_", remote_type), type, remote_type, class(desc))
  desc
}

find_compiled_function <- function(search, files, type) {
  for (f in files) {
    lines <- readLines(f)
    start <- grep(search, lines)
    if (length(start) > 0) {
      length <- find_function_end(lines[seq(start, length(lines))])
      if (!is.na(length)) {
        end <- start + length - 1
        return(
          list(Compiled(path = f,
              start = start,
              end = end,
              content = paste(lines[seq(start,
                  end)],
                collapse = "\n"),
              type = "c")))
      }
    }
  }
}

lookup_function <- function(name, package, type) {
  s <- as.source_type(package, type)

  s <- parse_symbol_map(fetch_symbol_map(desc))
  regex <- s$map[name]
  if (is.na(name)) {
    return()
  }

  files <- search_package(desc, name)
  for (f in files) {
    lines <- parse_source(fetch_source(f))
    start <- grep(regex, lines)
    if (length(start) > 0) {
      length <- find_function_end(lines[seq(start, length(lines))])
      if (!is.na(length)) {
        end <- start + length - 1
        return(
          list(Compiled(path = f,
              start = start,
              end = end,
              content = paste(lines[seq(start,
                  end)],
                collapse = "\n"),
              type = type)))
      }
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
