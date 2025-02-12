library(tm)
library(textclean)
library(textstem)
library(tidyverse)
library(emoji)
library(readxl)

clean_preprocess_text <- function(text_data) {
  
  if (is.na(text_data) || text_data == "") {
    return("")
  }
  
  text_data <- str_replace_all(text_data, '^"|"$', "")
  
  other_stop <- c("u", "ur", "uve", "ive", "im", "ain't", "aint", "ago", "asap",
                  "dont", "must", "also", "r", "vr", "mani", "sooo", "repost",
                  "go", "rli", "oh", "soo", "much", "will", "'s", "'re", "yall",
                  "wont", "n't", "'ve", "y'all", "'ll", "many", "more", "ouch",
                  "lol", "gonna", "wanna", "fyi", "gotta", "can", "shit", "wtf",
                  "bruh")
  
  stop_words <- c(stopwords("en"), other_stop)
  
  words <- text_data %>%
    tolower() %>%
    str_replace_all("[[:digit:]]", "") %>%        
    str_split(" ") %>% unlist() %>% .[! . %in% stop_words]
  
  text_data <- paste(words, collapse = " ") %>%
    emoji::emoji_replace("") %>%
    str_replace_all("[\r\n]+", " ") %>%
    str_replace_all("https?://\\S+|www\\.\\S+", " ") %>%
    str_replace_all("\\S*\\.com\\S*", " ") %>%
    str_replace_all("https?://\\S+|ftp://\\S+|www\\.\\S+", " ") %>%
    str_replace_all("rt[\\s]+|@\\S+|#\\S+|\\[|\\]|â\\\\x92|'s|â\\S+|[[:punct:]]", " ") %>%
    str_replace_all("covid\\S*|corona\\S*", "coronavirus") %>%
    str_replace_all("monkeypox\\S*|monkey[[:space:]]*pox|mpx\\S*|mpv\\S*|mpox\\S*", "mpox") %>%
    str_replace_all("\\|", " ") %>%
    str_replace_all("[^[:ascii:]]", " ") %>%
    stringi::stri_replace_all_regex("[\\p{Zs}\\p{Cf}]+", " ") %>%
    str_replace_all("~+|>{1,}|\\+{1,}|\\${1,}|={1,}", " ") %>%
    str_replace_all("\\b[a-zA-Z]{1,2}\\b|\\bhm+m*\\b", " ") %>%
    str_squish() %>%
    emoji::emoji_replace(" ") %>%
    str_replace_all("[[:digit:]]", " ")
  
  text_data <- str_split(text_data, " ") %>%
    unlist() %>% .[! . %in% stop_words] %>% 
    textstem::lemmatize_words() %>% paste(collapse = " ") 
  
  return(paste(text_data, collapse = " "))
}


df <- read_excel("Dataset.xlsx")

sf <- df %>% filter(Sentiment!="surprise") %>%
  mutate(`Post description` = tolower(trimws(`Post description`))) %>%
  distinct(`Post description`, .keep_all = TRUE)


df1 <- sf %>%
  mutate(
    label = case_when(
      Sentiment %in% c('fear', 'sadness', 'anger','disgust') ~ 'Negative',
      Sentiment == "joy" ~ "Positive", TRUE ~ "Neutral"),
    
    post = map_chr(`Translated Post Description`, clean_preprocess_text)
    
  ) %>%
  select(Date, post, Sentiment, label) %>% filter(post != "") %>% 
  drop_na() %>% distinct() %>% distinct(post, .keep_all = TRUE)


write_csv(df1, "clean_data.csv")


