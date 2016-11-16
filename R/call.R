symbol_map.c_source <- function(x, ...) {
  res <- parseNamespaceFile(basename(x), dirname(x), mustExist = FALSE)

  if (length(res$nativeRoutines) == 0) {
    return(character())
  }

  res$nativeRoutines[[1]]$symbolNames
}

lookup_function <- function(name, package, type) {
  desc <- as.source_type(package, type)

  map <- symbol_map(desc)
  regex <- map[name]
  if (is.na(name)) {
    return()
  }

  files <- search_package(desc, name)
  for (f in files) {
    lines <- read_file(f)
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
  #switch(class(desc)[[1]],
    #local = lookup_rcpp_local(name, desc$RemoteUrl),
    #cran = lookup_rcpp_cran(name, package),
    #github = lookup_rcpp_github(name, desc$RemoteUsername, desc$RemoteRepo),
    #stop("Unimplemented")
    #)
  #}
#}

lookup_source.c_call <- function(desc, name) {
  desc <- as.source_type(package)
  map <- symbol_map(desc)

  files <- list.files(file.path(path, "src"),
    pattern = "[.][ch]$",
    ignore.case = TRUE,
    recursive = TRUE,
    full.names = TRUE)

  find_compiled_function(paste0("[[:space:]]+", name, "\\([^)]+\\)[^;]*(?:$|\\{)"), files, "c")
}
