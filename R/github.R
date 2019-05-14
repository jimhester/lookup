#' @importFrom jsonlite base64_dec
NULL

github_content <- function(path, owner, repo, ref = "master", api_url = "https://api.github.com") {
  response <- gh("/repos/:owner/:repo/contents/:path", owner = owner, repo = repo, path = path, ref = ref, .api_url = api_url)
  strsplit(rawToChar(base64_dec(response$content)), "\n", fixed = TRUE)[[1]]
}

github_content_url <- function(path, owner, repo, ref = "master", api_url = "https://api.github.com") {
  response <- gh("/repos/:owner/:repo/contents/:path", owner = owner, repo = repo, path = path, ref = ref, .api_url = api_url)
  response$html_url
}

github_code_search <- function(name, path = "src/", owner, repo, language = c("c", "c++"), api_url = "https://api.github.com") {
  response <- gh("/search/code", q = paste("in:file", paste0("repo:", owner, "/", repo), paste0("path:", path), paste0("language:", language), name, collapse = "\n"), .api_url = api_url)

  vapply(response$items, `[[`, character(1), "path")
}

#' Search for code usage on CRAN
#'
#' This function uses GitHub's code search to search for a code usage. As default the
#' term will be searched in all packages on CRAN. It will open a browser window with
#' the results so they can be explored.
#' @param x The term to search
#' @param user The user. Default is cran.
#' @param language The language the term written by. Default value is R.
#' @export
#' @examples
#' \dontrun{
#' lookup_usage("grep")
#' lookup_usage("length", language = "C++")
#' lookup_usage("R_NamesSymbol", user = NULL, language = "C")
#' }
lookup_usage <- function(x, user = "cran", language = "R") {
  user_detail <- if (!(is.null(user) || is.na(user))) {
    paste0("+user%3A", URLencode(as.character(user), reserved = TRUE))
  } else {
    NULL
  }
  language_detail <- if (!(is.null(language) || is.na(language))) {
    paste0("+language%3A", URLencode(as.character(language), reserved = TRUE))
  } else {
    NULL
  }
  url <- paste(
    "https://github.com/search?l=&q=",
    URLencode(as.character(x)),
    user_detail,
    language_detail,
    "&ref=searchresults&type=Code&utf8=%E2%9C%93",
    sep = "",
    collapse = ""
  )
  browseURL(url)
}

