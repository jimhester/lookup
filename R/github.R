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
#' This function uses GitHub's code search to search for a code usage in all
#' packages on CRAN. It will open a browser window with the results so they can
#' be explored.
#' @param x The term to search
#' @export
#' @examples
#' \dontrun{
#' lookup_usage("grep")
#' }
lookup_usage <- function(x) {
  url <- paste("https://github.com/search?l=r&q=%22", as.character(x), "%22+user%3Acran+language%3AR&ref=searchresults&type=Code&utf8=%E2%9C%93", sep="", collapse="")
  browseURL(url)
}
