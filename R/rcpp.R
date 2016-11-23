# -- Applicable to all Rcpp types --
parse_symbol_map.rcpp <- function(s) {

  lines <- s$map_lines

  comment_lines <- grep("// [[:alpha:]]+", lines[3:length(lines)]) + 2

  # remove // from comments
  comments <- sub("^// ", "", lines[comment_lines])

  # remove ; from declarations
  declarations <- sub(";$", "", lines[comment_lines + 1])

  # convert all spaces to match multiple spaces
  declarations <- gsub("[[:space:]]+", "[[:space:]]+", regex_escape(declarations))

  # add wildcards before , and ) to handle default arguments
  declarations <- gsub("(\\\\?[,)])", "[^,)]*\\1", declarations)

  s$map <- setNames(declarations, comments)
  s$regex <- s$map[s$search]
  s
}

parse_source.rcpp <- function(s, regex) {

  start <- grep(regex, s$src_lines)
  s$fun_start <- s$fun_end <- s$fun_lines <- NULL
  if (length(start) > 0) {
    length <- find_function_end(s$src_lines[seq(start, length(s$src_lines))])
    if (!is.na(length)) {
      end <- start + length - 1
      s$fun_start <- start
      s$fun_end <- end
      s$fun_lines <- s$src_lines[seq(start, end)]
      return(s)
    }
  }
  s
}

# -- local repository methods --
fetch_symbol_map.rcpp_local <- function(s) {
  path <- s$description$RemoteUrl

  name <- basename(path)

  rcpp_exports <- file.path(path, "src", "RcppExports.cpp")

  if (!file.exists(rcpp_exports)) {
    return(list())
  }

  s$map_lines <- readLines(rcpp_exports)
  s
}

fetch_symbol_map.rcpp_github <- function(s) {
  s$map_lines <- github_content(s, "src/RcppExports.cpp")
  s
}

fetch_symbol_map.rcpp_cran <- function(s) {
  s$map_lines <- github_content(s, "src/RcppExports.cpp", owner = "cran", repo = s$description$Package, ref = s$description$Version, api_url = "https://api.github.com")
  s
}

source_files.rcpp_local <- function(s, ...) {
  path <- s$description$RemoteUrl
  s$src_files <- list.files(file.path(path, "src"),
    pattern = "[.]((c(c|pp))|(h(pp)?))$",
    ignore.case = TRUE,
    recursive = TRUE)

  # Ignore the RcppExports file, not what we want
  s$src_files <- file.path("src", s$src_files[basename(s$src_files) != "RcppExports.cpp"])
  s
}

fetch_source.local <- function(s, path) {
  s$src_path <- file.path(s$description$RemoteUrl, path)
  s$src_lines <- readLines(s$src_path)
  s
}

# -- Github --
source_files.rcpp_github <- function(s, name = s$search, ...) {
  s$src_files <- github_code_search(s, name = name)

  # Ignore the RcppExports file, not what we want
  s$src_files <- s$src_files[basename(s$src_files) != "RcppExports.cpp"]
  s
}

source_files.rcpp_cran <- function(s, name = s$search, ...) {
  s$src_files <- github_code_search(s, name = name, owner = "cran", repo = s$description$Package, api_url = "https://api.github.com")

  # Ignore the RcppExports file, not what we want
  s$src_files <- s$src_files[basename(s$src_files) != "RcppExports.cpp"]
  s
}

fetch_source.github <- function(s, path) {
  s$src_lines <- github_content(s, path)
  s
}

fetch_source.cran <- function(s, path) {
  s$src_lines <- github_content(s, path, owner = "cran", repo = s$description$Package, ref = s$description$Version, api_url = "https://api.github.com")
  s
}

lookup_rcpp <- function(name, package) {
  desc <- packageDescription(package)

  desc_file <- attr(desc, "file")
  if (basename(desc_file) != "package.rds") {
    desc$RemoteType <- "local"
    desc$RemoteUrl <- dirname(package)
  }

  if (desc$Repository %==% "CRAN") {
    desc$RemoteType <- "cran"
  }

  type <- desc$RemoteType %||% "unknown"
  switch(type,
    local = lookup_rcpp_local(name, desc$RemoteUrl),
    cran = lookup_rcpp_cran(name, package),
    github = lookup_rcpp_github(name, desc$RemoteUsername, desc$RemoteRepo),
    stop("`", type, "` unimplemented", call. = FALSE))
}


rcpp_symbol_map_cran <- function(name, package) {
  lines <- package_github_content(package, "src/RcppExports.cpp")
  parse_rcpp_symbol_map(lines)
}

package_github_content <- memoise::memoise(function(s, path) {
  tryCatch(readLines(paste(sep = "/", "https://raw.githubusercontent.com/cran", package, branch, path)), warning = function(e) character())
})

find_compiled_function <- function(search, lines, path, type) {
  start <- grep(search, lines)
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

lookup_rcpp_local <- function(name, path) {

  map <- rcpp_symbol_map_local(path)
  regex <- map[name]
  if (is.na(regex)) {
    return(list())
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
    res <- find_compiled_function(regex, readLines(f), f)
    if (!is.null(res)) {
       return(res)
    }
  }
}

lookup_rcpp_cran <- function(name, package) {
  lookup_rcpp_github(name, "cran", package)
}

lookup_rcpp_github <- function(name, user, package) {
  response <- gh("/search/code", q = paste("in:file", paste0("repo:", user, "/", package), "path:src/", "language:c", "language:c++", name))

  paths <- vapply(response$items, `[[`, character(1), "path")

  regex <- rcpp_symbol_map_cran(name, package)[name]
  if (any(is.na(regex))) {
    return(list())
  }
  compact(lapply(paths, function(path) {
      if (!grepl("RcppExports\\.cpp", path)) {
        find_compiled_function(regex, package_github_content(package, path), path)[[1]]
      }
  }))
}
