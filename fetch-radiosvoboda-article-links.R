library(rvest)

base_url <- "https://www.radiosvoboda.org"
base_news_url <- "https://www.radiosvoboda.org/s?k=гендерна+рівність&tab=news&r=any&pp=50"
query_param <- "pi"
num_pages <- 9
file_path <- "radiosvoboda_article_links.txt"
file_conn <- file(file_path, "w")

for (i in 1:num_pages) {
  cat("Fetching page", i, "of", num_pages, "\n")
  
  page_url <- paste0(base_news_url, "&", query_param, "=", i)
  web_content <- read_html(page_url)
  links <- html_attr(html_nodes(web_content, css = ".media-block__content > a"), "href")
  full_links <- paste(base_url, links, sep = "")
  writeLines(full_links, file_conn)
}

close(file_conn)