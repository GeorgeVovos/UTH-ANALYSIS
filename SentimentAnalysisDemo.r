library(readr)
library(dplyr)
library(tidytext)
library(ggplot2)
library(textdata)
library(wordcloud)
library(RColorBrewer)
library(tidyr)

mastodon_data <- read_csv("C:/dev/UTH/UTH-ANALYSIS/mastodon_posts_Trump_3000.csv")
text_df <- tibble(line = 1:nrow(mastodon_data), text = mastodon_data$Content)

tidy_text <- text_df %>%
  unnest_tokens(word, text) %>%
  anti_join(get_stopwords(), by = "word")

bing_lexicon <- get_sentiments("bing")

word_sentiment_freq <- tidy_text %>%
  inner_join(bing_lexicon, by = "word") %>%
  count(word, sentiment, sort = TRUE)

set.seed(123)
wordcloud(words = word_sentiment_freq$word,
          freq = word_sentiment_freq$n,
          scale = c(4, 0.5),
          random.order = FALSE,
          colors = ifelse(word_sentiment_freq$sentiment == "positive", "green3", "red3"))

bing_sentiment <- tidy_text %>%
  inner_join(bing_lexicon, by = "word") %>%
  distinct(line, sentiment)

all_lines <- tibble(line = 1:nrow(mastodon_data))
combined_sentiment <- all_lines %>%
  left_join(bing_sentiment, by = "line") %>%
  mutate(sentiment = ifelse(is.na(sentiment), "neutral", sentiment)) %>%
  count(sentiment) %>%
  mutate(percent = (n / nrow(text_df)) * 100,
         label = paste0(sentiment, ": ", round(percent, 1), "%"))

general_plot <- ggplot(combined_sentiment, aes(x = reorder(sentiment, -percent), y = percent, fill = sentiment)) +
  geom_col() +
  geom_text(aes(label = label), vjust = -0.5, size = 4) +
  labs(title = "Overall Sentiment (Positive, Negative, Neutral)", x = "Sentiment", y = "Percentage of Posts") +
  theme_minimal() +
  theme(legend.position = "none")

print(general_plot)

nrc_lexicon <- get_sentiments("nrc")

line_emotions <- tidy_text %>%
  inner_join(nrc_lexicon, by = "word", relationship = "many-to-many") %>%
  distinct(line, sentiment)

emotion_summary <- line_emotions %>%
  count(sentiment) %>%
  mutate(percent = (n / nrow(text_df)) * 100,
         label = paste0(round(percent, 1), "%"))

bar_plot <- ggplot(emotion_summary, aes(x = reorder(sentiment, -percent), y = percent, fill = sentiment)) +
  geom_col() +
  geom_text(aes(label = label), vjust = -0.5, size = 4) +
  labs(title = "Emotion Distribution by Post (Bar Chart)", x = "Emotion", y = "Percentage of Posts") +
  theme_minimal() +
  theme(legend.position = "none")

print(bar_plot)

pie_chart <- ggplot(emotion_summary, aes(x = "", y = percent, fill = sentiment)) +
  geom_col(width = 1, color = "white") +
  coord_polar("y") +
  labs(title = "Emotion Distribution by Post (Pie Chart)") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 4) +
  theme_void()

print(pie_chart)

