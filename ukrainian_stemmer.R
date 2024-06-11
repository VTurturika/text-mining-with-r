library(stringi)

# Функція стемінгу українських слів за алгоритмом Портера
ukrainian_stemmer <- function(word) {
  vowel <- "аеиоуюяіїє"
  perfectiveground <- "(ив|ивши|ившись|ыв|ывши|ывшись)$"
  reflexive <- "(с[яьи])$"
  adjective <- "(ими|ій|ий|а|е|ова|ове|ів|є|їй|єє|еє|я|ім|ем|им|ім|их|іх|ою|йми|іми|у|ю|ого|ому|ої)$"
  participle <- "(ий|ого|ому|им|ім|а|ій|у|ою|ій|і|их|йми|их)$"
  verb <- "(сь|ся|ив|ать|ять|у|ю|ав|али|учи|ячи|вши|ши|е|ме|ати|яти|є)$"
  noun <- "(а|ев|ов|е|ями|ами|еи|и|ей|ой|ий|й|иям|ям|ием|ем|ам|ом|о|у|ах|иях|ях|ы|ь|ию|ью|ю|ия|ья|я|і|ові|ї|ею|єю|ою|є|еві|ем|єм|ів|їв|ю)$"
  rvre <- "[аеиоуюяіїє]"
  derivational <- "[^аеиоуюяіїє][аеиоуюяіїє]+[^аеиоуюяіїє]+[аеиоуюяіїє].*ость?$"
  
  preprocess_word <- function(word) {
    word <- tolower(word)
    word <- gsub("'", "", word)
    word <- gsub("ё", "е", word)
    word <- gsub("ъ", "ї", word)
    return(word)
  }

  substitute <- function(st, reg, to) {
    original <- st
    result <- gsub(reg, to, st)
    return(list(result = result, changed = (original != result)))
  }

  word <- preprocess_word(word)
  
  if (!grepl(rvre, word)) {
    return(word)
  }
  
  p <- regexpr(rvre, word)
  start <- substr(word, 1, p + attr(p, "match.length") - 1)
  RV <- substr(word, p + attr(p, "match.length"), nchar(word))

  res <- substitute(RV, perfectiveground, "")
  RV <- res$result
  if (!res$changed) {
    RV <- gsub(reflexive, "", RV)
    res <- substitute(RV, adjective, "")
    RV <- res$result
    if (res$changed) {
      RV <- gsub(participle, "", RV)
    } else {
      res <- substitute(RV, verb, "")
      RV <- res$result
      if (!res$changed) {
        RV <- gsub(noun, "", RV)
      }
    }
  }

  RV <- gsub("и$", "", RV)
  
  if (grepl(derivational, RV)) {
    RV <- gsub("ость$", "", RV)
  }

  res <- substitute(RV, "ь$", "")
  RV <- res$result
  if (res$changed) {
    RV <- gsub("ейше?$", "", RV)
    RV <- gsub("нн$", "н", RV)
  }
  
  stemmed_word <- paste0(start, RV)
  return(stemmed_word)
}

test_words <- c("читання", "працюють", "працюючий", "співробітники", "розвиток", "розвивати")
stemmed_words <- sapply(test_words, ukrainian_stemmer)
print(stemmed_words)
