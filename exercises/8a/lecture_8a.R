



## ----setup, include=FALSE-----------------------
library(tidyverse)
library(knitr)
library(kableExtra)
library(textdata)
library(tm)

opts_chunk$set(echo = TRUE,   
               out.width = "100%",
               message = FALSE,
               warning = FALSE,
               collapse = TRUE,
               fig.height = 4,
               fig.width = 8,
               fig.align = "center",
               cache = FALSE)

as_table <- function(...) knitr::kable(..., format='html', digits = 3)


## ----list-pkgs----------------------------------
library(tidytext)
library(gutenbergr)


## ----show-text----------------------------------
text <- c("This will be an uncertain time for us my love",
          "I can hear the echo of your voice in my head",
          "Singing my love",
          "I can see your face there in my hands my love",
          "I have been blessed by your grace and care my love",
          "Singing my love")

text


## ----tidy-text-tile-----------------------------
text_df <- tibble(line = seq_along(text), text = text)

text_df


## ----unnest-tokens------------------------------
text_df %>%
  unnest_tokens(
    output = word,
    input = text,
    token = "words" # default option
  ) 


## ----unnest-tokens-chars------------------------
text_df %>%
  unnest_tokens(
    output = word,
    input = text,
    token = "characters"
  )


## ----unnest-tokens-ngram-2----------------------
text_df %>%
  unnest_tokens(
    output = word,
    input = text,
    token = "ngrams",
    n = 2
  )


## ----unnest-tokens-ngram-3----------------------
text_df %>%
  unnest_tokens(
    output = word,
    input = text,
    token = "ngrams",
    n = 3
  )


## ---- echo = TRUE-------------------------------
acnh_user_reviews <- read_tsv(here::here("slides/data/acnh_user_reviews.tsv"))
glimpse(acnh_user_reviews)


## ----review-grades, echo = FALSE, out.width = "90%"----
acnh_user_reviews %>% 
  count(grade) %>% 
  ggplot(aes(x = grade, y = n )) +
  geom_col()


## ----pos-reviews, echo = TRUE, eval = FALSE-----
## set.seed(1999)
## acnh_user_reviews %>%
##   filter(grade > 8) %>%
##   sample_n(3) %>%
##   pull(text)


## ----ref.label="pos-reviews", eval = TRUE, echo = FALSE----


## ----neg-reviews, echo = TRUE, eval = FALSE-----
## set.seed(2099)
## acnh_user_reviews %>%
##   filter(grade == 0) %>%
##   sample_n(3) %>%
##   pull(text)


## ----ref.label="neg-reviews", eval = TRUE, echo = FALSE----


## -----------------------------------------------
acnh_user_reviews_parsed <- acnh_user_reviews %>% 
  mutate(text = str_remove(text, "Expand$"))


## ----unnest-tokens-acnh-------------------------
user_reviews_words <- acnh_user_reviews_parsed %>%
  unnest_tokens(output = word, input = text)

user_reviews_words


## ----word-histogram, out.width="90%"------------
user_reviews_words %>% 
  count(user_name) %>% 
  ggplot(aes(x = n)) +
  geom_histogram()


## ----common-words-------------------------------
user_reviews_words %>%
  count(word, sort = TRUE)


## ----eng-stopwords------------------------------
get_stopwords()


## ----spanish-stopwords--------------------------
get_stopwords(language = "es")


## ----other-lexicons-----------------------------
get_stopwords(source = "smart")


## ----repeat-------------------------------------
user_reviews_words %>%
  count(word, sort = TRUE)


## ----stopwords-anti-join------------------------
stopwords_smart <- get_stopwords(source = "smart")

user_reviews_words %>%
  anti_join(stopwords_smart) 


## ----animate-anti-join, echo = FALSE, out.width = "50%"----
include_graphics("gifs/anti-join.gif")


## ----stopwords-anti-join-complete---------------
user_reviews_words %>%
  anti_join(stopwords_smart) %>%
  count(word, sort = TRUE) 


## ----gg-common-words, eval=FALSE----------------
## user_reviews_words %>%
##   anti_join(stopwords_smart) %>%
##   count(word) %>%
##   arrange(-n) %>%
##   top_n(20) %>%
##   ggplot(aes(fct_reorder(word, n), n)) +
##   geom_col() +
##   coord_flip() +
##   theme_minimal() +
##   labs(title = "Frequency of words in user reviews",
##        subtitle = "",
##        y = "",
##        x = "")


## ----gg-common-words-out, ref.label = 'gg-common-words', echo = FALSE, out.width = "100%"----


## ----show-sentiment-afinn-----------------------
get_sentiments("afinn")


## ----show-sentiment-bing------------------------
get_sentiments("bing")


## ----show-sentiment-bing2-----------------------
get_sentiments(lexicon = "bing")


## ----show-sentiment-loughran--------------------
get_sentiments(lexicon = "loughran")


## ----sentiment-reviews--------------------------
sentiments_bing <- get_sentiments("bing")

user_reviews_words %>%
  inner_join(sentiments_bing) %>%
  count(sentiment, word, sort = TRUE) 


## ----gg-sentiment, echo=FALSE, message=FALSE----
user_reviews_words %>%
  inner_join(sentiments_bing) %>%
  count(sentiment, word, sort = TRUE) %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  ggplot(aes(fct_reorder(word, n), n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~sentiment, scales = "free") +
  theme_minimal() +
  labs(
    title = "Sentiments in user reviews",
    x = ""
  )


## ----gg-sentiment2, eval = FALSE----------------
## user_reviews_words %>%
##   inner_join(sentiments_bing) %>%
##   count(sentiment, word, sort = TRUE) %>%
##   arrange(desc(n)) %>%
##   group_by(sentiment) %>%
##   top_n(10) %>%
##   ungroup() %>%
##   ggplot(aes(fct_reorder(word, n), n, fill = sentiment)) +
##   geom_col() +
##   coord_flip() +
##   facet_wrap(~sentiment, scales = "free") +
##   theme_minimal() +
##   labs(
##     title = "Sentiments in user reviews",
##     x = ""
##   )


## ----common-user-words-stop---------------------
user_reviews_words %>%
  anti_join(stopwords_smart) %>%
  count(grade, word, sort = TRUE) 


## ----common-user-words--------------------------
user_reviews_words %>%
  count(grade, word, sort = TRUE)


## ----doc-example--------------------------------
document <- user_reviews_words %>% 
    anti_join(stopwords_smart) %>% 
    filter(user_name == "Discoduckasaur")
document


## ----tf-----------------------------------------
tbl_tf <- document %>% 
  count(word, sort = TRUE) %>% 
  mutate(tf = n / sum(n))
tbl_tf %>% 
  arrange(desc(tf))


## ----idf----------------------------------------
tbl_idf <- user_reviews_words %>% 
    anti_join(stopwords_smart) %>%
    mutate(collection_size = n_distinct(user_name)) %>% 
    group_by(collection_size, word) %>% 
    summarise(times_word_used = n_distinct(user_name)) %>% 
    mutate(freq = collection_size / times_word_used,
           idf = log(freq)) 
arrange(tbl_idf, idf)


## ----tf-idf-------------------------------------
tbl_tf %>% 
    left_join(tbl_idf) %>% 
    select(word, tf, idf) %>% 
    mutate(tf_idf = tf * idf) %>% 
    arrange(desc(tf_idf))


## ----calc-tf-idf--------------------------------
user_reviews_counts <- user_reviews_words %>%
      anti_join(stopwords_smart) %>% 
      count(user_name, word, sort = TRUE) %>% 
      bind_tf_idf(term = word, document = user_name, n = n)

user_reviews_counts


## ----gg-tf-idf, echo=FALSE,message=FALSE, fig.height = 5----
pos_reviews <- acnh_user_reviews_parsed %>% 
    select(user_name, grade) %>% 
    filter(grade > 8) %>% 
    sample_n(3)

user_reviews_counts_pos <- user_reviews_counts %>%
  inner_join(pos_reviews, by = "user_name") 

user_reviews_counts_pos %>% 
  group_by(user_name) %>%
  top_n(10, wt = tf_idf) %>%
  ungroup() %>%
  ggplot(aes(fct_reorder(word, tf_idf), tf_idf, fill = user_name)) +
  geom_col(show.legend = FALSE) + 
  coord_flip() +
  facet_wrap(~user_name, ncol = 1, scales = "free") +
  scale_y_continuous() +
  theme_minimal() +
  labs(x = NULL, y = "tf-idf")


## ----endslide, child="components/endslide.Rmd"----



