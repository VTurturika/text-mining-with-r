library(rvest)

base_url <- "https://phm.cuspu.edu.ua/"
base_news_url <- "https://phm.cuspu.edu.ua/facultet/novini.html"
query_param <- "start"
num_pages <- 20
step <- 20
file_path <- "article_links.txt"
file_conn <- file(file_path, "w")

for (i in 1:num_pages) {
  cat("Fetching page", i, "of", num_pages, "\n")
  
  page_url <- paste0(base_news_url, "?", query_param, "=", (i-1)*step)
  web_content <- read_html(page_url)
  links <- html_attr(html_nodes(web_content, css = ".page-header a"), "href")
  full_links <- paste(base_url, links, sep = "")
  writeLines(full_links, file_conn)
}

close(file_conn)
