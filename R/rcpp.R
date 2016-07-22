rcpp_symbol_map_local <- function(path) {

  name <- basename(path)

  rcpp_exports <- file.path(path, "src", "RcppExports.cpp")

  if (!file.exists(rcpp_exports)) {
    return()
  }

  lines <- readLines(rcpp_exports)

  parse_rcpp_symbol_map(lines)
}

parse_rcpp_symbol_map <- function(lines) {
  comment_lines <- grep("// [[:alpha:]]+", lines[3:length(lines)]) + 2

  # remove // from comments
  comments <- sub("^// ", "", lines[comment_lines])

  # remove ; from declarations
  declarations <- sub(";$", "", lines[comment_lines + 1])

  # convert all spaces to match multiple spaces
  declarations <- gsub("[[:space:]]+", "[[:space:]]+", regex_escape(declarations))

  # add wildcards before , and ) to handle default arguments
  declarations <- gsub("(\\\\?[,)])", "[^,)]*\\1", declarations)

  setNames(declarations, comments)
}

lookup_rcpp <- function(name, package) {
  desc <- packageDescription(package)

  desc_file <- attr(desc, "file")
  if (is.null(desc$RemoteType)) {
    desc$RemoteType <- "cran"
  }

  if (basename(desc_file) != "package.rds") {
    return(lookup_rcpp_local(dirname(desc_file)))
  }
  switch(desc$RemoteType,
    local = lookup_rcpp_local(name, desc$RemoteUrl),
    cran = lookup_rcpp_cran(name, package),
    github = lookup_rcpp_github(name, desc$RemoteUrl),
    stop("Unimplemented")
    )
}


rcpp_symbol_map_cran <- function(name, package) {
  lines <- package_github_content(package, "src/RcppExports.cpp")
  parse_rcpp_symbol_map(lines)
}

package_github_content <- memoise::memoise(function(package, path, branch = "master") {
  tryCatch(readLines(paste(sep = "/", "https://raw.githubusercontent.com/cran", package, branch, path)), warning = function(e) character())
})

find_cpp_function <- function(search, lines, path) {
  start <- grep(search, lines)
  if (length(start) > 0) {
    browser()
    length <- find_function_end(lines[seq(start, length(lines))])
    if (!is.na(length)) {
      end <- start + length - 1
      return(
        list(Compiled(path = path,
            start = start,
            end = end,
            content = paste(lines[seq(start,
                end)],
              collapse = "\n"),
            type = "c++")))
    }
  }
}

lookup_rcpp_local <- function(name, path) {

  map <- rcpp_symbol_map_local(path)
  browser()
  regex <- map[name]
  if (is.na(regex)) {
    return()
  }

  files <- list.files(file.path(path, "src"),
    pattern = "[.]((c(c|pp))|(h(pp)?))$",
    ignore.case = TRUE,
    recursive = TRUE,
    full.names = TRUE)

  for (f in files) {

    # skip RcppExports
    if (basename(f) == "RcppExports.cpp") {
       next
    }
    res <- find_cpp_function(regex, readLines(f), f)
    if (!is.null(res)) {
       return(res)
    }
  }
}

lookup_rcpp_cran <- function(name, package) {
  response <- gh("/search/code", q = paste("in:file", paste0("repo:cran/", package), "path:src/", "language:c", "language:c++", name))
  paths <- vapply(response$items, `[[`, character(1), "path")
  regex <- rcpp_symbol_map_cran(name, package)[name]
  if (any(is.na(regex))) {
    return()
  }
  compact(lapply(paths, function(path) {
      if (!grepl("RcppExports\\.cpp", path)) {
        find_cpp_function(regex, package_github_content(package, path), path)
      }
  }))
}

