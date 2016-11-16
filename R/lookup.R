symbol_map <- function(desc, ...) {
  file <- fetch_symbol_map(desc)
  parse_symbol_map(file)
}

fetch_symbol_map <- function(desc, ...) UseMethod("fetch_symbol_map")
parse_symbol_map <- function(desc, ...) UseMethod("parse_symbol_map")
search_source <- function(desc, name) UseMethod("search_source")
fetch_source <- function(desc, name) UseMethod("fetch_source")
parse_source <- function(desc, path, lines) UseMethod("parse_source")

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
