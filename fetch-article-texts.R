library(rvest)
library(stringr)

input_file <- "article_links.txt"
output_file <- "article_texts.csv"

article_links <- readLines(input_file)

start <- 1
end <- length(article_links)


results <-
  data.frame(
    date_published = character(),
    link = character(),
    title = character(),
    text = character(),
    stringsAsFactors = FALSE
  )

if (file.exists(output_file)) {
  existing_data <- read.csv(output_file, stringsAsFactors = FALSE)
  results <- rbind(results, existing_data)
}


for (i in seq(start, end)) {
  link <- article_links[i]

  cat("Fetching link", i, "of", end, ":", link, "\n")
  
  web_content <- read_html(link)
  date_published = substr(html_attr(html_node(web_content, css = ".published time"), "datetime"), 1, 10)
  title <- html_text2(html_node(web_content, css = ".page-header h1"))
  text <- html_text2(html_node(web_content, css = ".article__text"))
  text <- str_squish(text)
  
  results <- rbind(results, data.frame(date_published = date_published,
                                       link = link,
                                       title = title,
                                       text = text,
                                       stringsAsFactors = FALSE))

  cat("Successfully fetched and stored text from link", i, "\n")
}

write.csv(results, output_file, row.names = FALSE)
