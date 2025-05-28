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

# Tokenize and clean text
tidy_text <- text_df %>%
  unnest_tokens(word, text) %>%
  anti_join(get_stopwords(), by = "word")

# Load Bing sentiment lexicon
bing_lexicon <- get_sentiments("bing")

# Word frequency by sentiment (for wordcloud)
word_sentiment_freq <- tidy_text %>%
  inner_join(bing_lexicon, by = "word") %>%
  count(word, sentiment, sort = TRUE)


set.seed(123)
wordcloud(
  words = word_sentiment_freq$word,
  freq = word_sentiment_freq$n,
  scale = c(4, 0.5),
  random.order = FALSE,
  colors = ifelse(word_sentiment_freq$sentiment == "positive", "green3", "red3")
)


bing_counts <- tidy_text %>%
  inner_join(bing_lexicon, by = "word") %>%
  count(line, sentiment)

# Separate and merge positive/negative counts explicitly
positive_counts <- bing_counts %>% filter(sentiment == "positive") %>% select(line, positive = n)
negative_counts <- bing_counts %>% filter(sentiment == "negative") %>% select(line, negative = n)

post_sentiment_counts <- full_join(positive_counts, negative_counts, by = "line") %>%
  mutate(
    positive = replace_na(positive, 0),
    negative = replace_na(negative, 0),
    sentiment = case_when(
      positive > negative ~ "positive",
      negative > positive ~ "negative",
      TRUE ~ "neutral"
    )
  )


all_lines <- tibble(line = 1:nrow(mastodon_data))

combined_sentiment <- all_lines %>%
  left_join(post_sentiment_counts, by = "line") %>%
  mutate(sentiment = ifelse(is.na(sentiment), "neutral", sentiment)) %>%
  count(sentiment) %>%
  mutate(
    sentiment = factor(sentiment, levels = c("positive", "neutral", "negative")),
    percent = (n / sum(n)) * 100,
    label = paste0(sentiment, ": ", round(percent, 1), "%")
  )


general_plot <- ggplot(combined_sentiment, aes(x = sentiment, y = percent, fill = sentiment)) +
  geom_col() +
  geom_text(aes(label = label), vjust = -0.5, size = 4) +
  labs(title = "Overall Sentiment (Positive, Neutral, Negative)", x = "Sentiment", y = "Percentage of Posts") +
  theme_minimal() +
  theme(legend.position = "none")

print(general_plot)


nrc_lexicon <- get_sentiments("nrc")


dominant_emotions <- tidy_text %>%
  inner_join(nrc_lexicon, by = "word") %>%
  count(line, sentiment) %>%
  group_by(line) %>%
  slice_max(n, with_ties = TRUE) %>%
  slice_min(sentiment, with_ties = FALSE) %>%  
  ungroup()


emotion_summary <- dominant_emotions %>%
  count(sentiment) %>%
  mutate(
    percent = (n / sum(n)) * 100,
    label = paste0(round(percent, 1), "%")
  )


bar_plot <- ggplot(emotion_summary, aes(x = reorder(sentiment, -percent), y = percent, fill = sentiment)) +
  geom_col() +
  geom_text(aes(label = label), vjust = -0.5, size = 4) +
  labs(title = "Dominant Emotion Distribution by Post", x = "Emotion", y = "Percentage of Posts") +
  theme_minimal() +
  theme(legend.position = "none")

print(bar_plot)


pie_chart <- ggplot(emotion_summary, aes(x = "", y = percent, fill = sentiment)) +
  geom_col(width = 1, color = "white") +
  coord_polar("y") +
  labs(title = "Dominant Emotion Distribution by Post (Pie Chart)") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 4) +
  theme_void()

print(pie_chart)