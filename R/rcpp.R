rcpp_exports <- function(package) {
  s <- as.source_type(package, "rcpp")
  tryCatch(names(parse_symbol_map(fetch_symbol_map(s))$map), "github_error" = function(e) character())
}

# -- Applicable to all Rcpp types --
generate_function_regex <- function(name) { }
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
  s$regex <- s$map[s$name]
  s$search <- s$name
  s
}

parse_source.rcpp <- function(s, regex = s$regex) {
  s$fun_start <- s$fun_end <- s$fun_lines <- NULL

  new_lines <- cumsum(nchar(s$src_lines) + 1)
  lines <- paste(s$src_lines, collapse = "\n")

  start <- regexpr(regex, lines)
  if (length(start) > 0) {
    end <- find_function_end(lines, start)
    if (!is.na(end)) {
      s$fun_start <- tail(which(new_lines <= start), n = 1L) + 1L
      s$fun_end <- head(which(new_lines >= end), n = 1L)
      s$fun_lines <- s$src_lines[seq(s$fun_start, s$fun_end)]
      return(s)
    }
  }
  s
}

# -- local repository methods --
fetch_symbol_map.rcpp_local <- function(s) {
  path <- s$description$RemoteUrl

  name <- basename(path)

  rcpp_exports <- paths(path, "src", "RcppExports.cpp")

  if (!file.exists(rcpp_exports)) {
    s$map_lines <- NULL
  } else {
    s$map_lines <- readLines(rcpp_exports)
  }

  s
}

fetch_symbol_map.rcpp_github <- function(s) {
  s$map_lines <- github_content(
    path = paths(s$description$RemoteSubdir, "src/RcppExports.cpp"),
    owner = s$description$RemoteUsername,
    repo = s$description$RemoteRepo,
    ref = s$description$RemoteRef,
    api_url = s$description$RemoteHost)
  s
}

fetch_symbol_map.rcpp_cran <- function(s) {
  s$map_lines <- github_content(
    path = "src/RcppExports.cpp",
    owner = "cran",
    repo = s$description$Package,
    ref = s$description$Version)
  s
}

source_files.rcpp_local <- function(s, ...) {
  path <- s$description$RemoteUrl
  s$src_files <- list.files(paths(path, "src"),
    pattern = "[.]((c(c|pp))|(h(pp)?))$",
    ignore.case = TRUE,
    recursive = TRUE)

  # Ignore the RcppExports file, not what we want
  s$src_files <- paths("src", s$src_files[basename(s$src_files) != "RcppExports.cpp"])
  s
}

fetch_source.local <- function(s, path) {
  s$src_path <- paths(s$description$RemoteUrl, path)
  s$src_lines <- readLines(s$src_path)
  s
}

# -- Github --
source_files.rcpp_github <- function(s, name = s$search, ...) {
  s$src_files <- github_code_search(
    name = name,
    path = paths(s$description$RemoteSubdir, "src"),
    owner = s$description$RemoteUsername,
    repo = s$description$RemoteRepo,
    api_url = s$description$RemoteHost)

  # Ignore the RcppExports file, not what we want
  s$src_files <- s$src_files[basename(s$src_files) != "RcppExports.cpp"]
  s
}

source_files.rcpp_cran <- function(s, name = s$search, ...) {
  s$src_files <- github_code_search(
    name = name,
    path = "src/",
    owner = "cran",
    repo = s$description$Package)

  # Ignore the RcppExports file, not what we want
  s$src_files <- s$src_files[basename(s$src_files) != "RcppExports.cpp"]
  s
}

fetch_source.github <- function(s, path) {
  s$src_lines <- github_content(
    path = path,
    owner = s$description$RemoteUsername,
    repo = s$description$RemoteRepo,
    ref = s$description$RemoteRef)

  s$src_path <- path
  s
}

fetch_source.cran <- function(s, path) {
  s$src_lines <- github_content(
    path,
    owner = "cran",
    repo = s$description$Package,
    ref = s$description$Version)

  s$src_path <- path
  s
}

fetch_source.base <- function(s, path) {
  s$src_lines <- github_content(
    path,
    owner = "wch",
    repo = "r-source")

  s$src_path <- path
  s
}
