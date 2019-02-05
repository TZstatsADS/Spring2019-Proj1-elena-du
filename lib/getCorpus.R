words = c("happy", "happiness", "past", "years", "favorite", "enjoy", "enjoyed", "year", "months", "event", "made", "felt", "feel", "good", "ago", "great", "awesome", "today", "yesterday", "lot", "week", "finally", "day", "time", "nice", "amazing", "month")

getCorpus = function(text) {
  corpus = VCorpus(VectorSource(text)) %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(stripWhitespace) %>%
    tm_map(removeWords, stopwords("en")) %>%
    tm_map(removeWords, stopwords("SMART")) %>%
    tm_map(removeWords, words)
  return(corpus)
}