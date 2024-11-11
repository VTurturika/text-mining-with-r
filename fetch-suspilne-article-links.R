library(rvest)

base_url <- "https://suspilne.media/tag/genderna-rivnist/"
query_param <- "page"
num_pages <- 17
file_path <- "suspilne_article_links.txt"
file_conn <- file(file_path, "w")

for (i in 1:num_pages) {
  cat("Fetching page", i, "of", num_pages, "\n")
  
  page_url <- paste0(base_url, "?", query_param, "=", i)
  web_content <- read_html(page_url)
  links <- html_attr(html_nodes(web_content, css = ".l-category__item > a"), "href")
  writeLines(links, file_conn)
}

close(file_conn)