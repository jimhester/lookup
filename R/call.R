c_symbol_map_local <- function(x) {
  res <- parseNamespaceFile(basename(x), dirname(x), mustExist = FALSE)

  if (length(res$nativeRoutines) == 0) {
    return(character())
  }

  res$nativeRoutines[[1]]$symbolNames
}

lookup_c_call <- function(name, package) {

  desc <- packageDescription(package)

  desc_file <- attr(desc, "file")

  if (basename(desc_file) != "package.rds") {
    path <- dirname(desc_file)
  } else if (!is.null(desc$RemoteType) && desc$RemoteType == "local") {
    path <- desc$RemoteUrl
  } else {
    stop("Unimplemented")
  }

  map <- c_symbol_map_local(path)
  name <- map[name]
  if (is.na(name)) {
    return()
  }

  files <- list.files(file.path(path, "src"),
    pattern = "[.][ch]$",
    ignore.case = TRUE,
    recursive = TRUE,
    full.names = TRUE)

  for (f in files) {
    lines <- readLines(f)
    start <- grep(paste0("[[:space:]]+", name, "\\([^)]+\\)[^;]*(?:$|\\{)"), lines)
    if (length(start) > 0) {
      length <- find_function_end(lines[seq(start, length(lines))])
      if (!is.na(length)) {
        end <- start + length - 1
        return(
          Compiled(path = path,
            start = start,
            end = end,
            content = paste(lines[seq(start,
                end)],
              collapse = "\n"),
            type = "c"))
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
  as.character(call[[2]])
}

has_call <- function(f, type) {
  if (!is.function(f) || is.primitive(f)) {
    return(FALSE)
  }
  calls <- codetools::findGlobals(f, merge = FALSE)$functions
  any(calls %in% type)
}
