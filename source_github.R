library(dplyr)


source_github <- function(repo, branch = "main", file, # nolint
                          auth = Sys.getenv("GITHUB_PAT")) {
  stopifnot(
    all(sapply(c(repo, branch, file), is.character)),
    all(sapply(c(repo, branch, file), length) == 1)
  )
  
  # header_accept <- "application/vnd.github.v3.raw"  # nolint
  # header_auth <- if (is.null(auth)) {  # nolint
  #   NULL
  # } else {
  #   paste0("auth")
  # }
  
  tmpfile <- tempfile()
  on.exit(unlink(tmpfile))
  
  url <- paste0("https://api.github.com/repos/", repo, "/contents/", file,
                "?ref=", branch)
  
  # r <- httr::GET(url, config = list(
  #   add_headers(Accept = header_accept, Authorization = paste0("token ", auth))
  # ))
  
  r <- GET(url = url, 
           config = authenticate(auth, ""))
  
  
  httr::stop_for_status(r)
  
  r1 <- httr::GET(content(r)$download_url, write_disk(tmpfile, overwrite = T))
  
  httr::content(r)$content %>%
    base64enc::base64decode(.) %>%
    writeBin(tmpfile)
  
  base::source(tmpfile)
}
