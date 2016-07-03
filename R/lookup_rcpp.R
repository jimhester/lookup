rcpp_symbol_map <- function(path) {
  name <- basename(path)

  rcpp_exports <- file.path(path, "src", "RcppExports.cpp")

  if (!file.exists(rcpp_exports)) {
    return()
  }

  lines <- readLines(rcpp_exports)
  comment_lines <- grep("// [[:alpha:]]+", lines[3:length(lines)]) + 2

  # remove // from comments
  comments <- sub("^// ", "", lines[comment_lines])

  # remove ; from declarations
  declarations <- sub(";$", "", lines[comment_lines + 1])

  # convert all spaces to match multiple spaces
  declarations <- gsub("[[:space:]]+", "[[:space:]]+", regex_escape(declarations))

  setNames(declarations, paste0(name, "_", comments))
}

lookup_rcpp <- function(name, package) {

  desc <- packageDescription(package)

  desc_file <- attr(desc, "file")

  if (basename(desc_file) != "package.rds") {
    path <- dirname(desc_file)
  } else if (!is.null(desc$RemoteType) && desc$RemoteType == "local") {
    path <- desc$RemoteUrl
  } else {
    stop("Unimplemented")
  }

  map <- rcpp_symbol_map(path)
  name <- map[name]
  if (is.na(name)) {
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
    lines <- readLines(f)
    start <- grep(name, lines)
    if (length(start) > 0) {
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
}
