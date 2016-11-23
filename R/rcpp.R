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
