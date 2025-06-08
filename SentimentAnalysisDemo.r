# Load required libraries
library(readr)
library(dplyr)
library(stringr)
library(tidytext)
library(textclean)
library(syuzhet)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(scales)

# Load the data

data <- read_csv("C:/dev/UTH/UTH-ANALYSIS/mastodon_posts_Trump_3000.csv")

clean_text <- function(text) {
  # Remove HTML tags
  text <- str_remove_all(text, "<[^>]*>")
  # Remove URLs
  text <- str_remove_all(text, "https?://[^\\s]+")
  # Remove extra whitespace
  text <- str_squish(text)
  # Convert to lowercase
  text <- tolower(text)
  return(text)
}

# Function to handle negations better
handle_negations <- function(text) {
  # Common negation patterns
  text <- str_replace_all(text, "\\b(no|not|never|none|nobody|nothing|neither|nowhere|barely|hardly|scarcely|seldom|rarely)\\s+", "NOT_")
  text <- str_replace_all(text, "\\b(can't|cannot|won't|wouldn't|shouldn't|couldn't|doesn't|don't|didn't|isn't|aren't|wasn't|weren't|hasn't|haven't|hadn't)\\s+", "NOT_")
  text <- str_replace_all(text, "\\b(zero|lack of|absence of)\\s+", "NOT_")
  return(text)
}

# Clean the content column
data$cleaned_content <- sapply(data$Content, function(x) handle_negations(clean_text(x)))

# Remove empty or very short posts (less than 10 characters)
data <- data %>%
  filter(nchar(cleaned_content) >= 10)

# Tokenize the text and remove stop words
tokens <- data %>%
  select(Post_ID = `Post ID`, cleaned_content) %>%
  unnest_tokens(word, cleaned_content) %>%
  anti_join(stop_words, by = "word") %>%
  # Remove specific terms that may skew sentiment analysis
  filter(!word %in% c("president", "trump", "donald", "administration", "white", "house", "government", "political", "politics", "policy", "policies")) %>%
  # Remove numbers and single characters
  filter(!str_detect(word, "^\\d+$")) %>%
  filter(nchar(word) > 1)

# Get sentiment scores using multiple lexicons for better accuracy
sentiment_afinn <- tokens %>%
  inner_join(get_sentiments("afinn"), by = "word")

sentiment_nrc <- tokens %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  filter(sentiment %in% c("positive", "negative", "joy", "anger", "fear", "sadness", "trust", "disgust"))

# Calculate AFINN-based sentiment scores (numeric values)
post_sentiment_afinn <- sentiment_afinn %>%
  group_by(Post_ID) %>%
  summarise(
    sentiment_score = sum(value),
    word_count = n(),
    .groups = "drop"
  ) %>%
  mutate(
    # Apply minimum threshold for classification
    overall_sentiment = case_when(
      word_count < 3 ~ "Neutral",  # Too few sentiment words
      sentiment_score >= 2 ~ "Positive",
      sentiment_score <= -2 ~ "Negative", 
      TRUE ~ "Neutral"
    )
  )

# Categorize sentiments into positive, negative, neutral
sentiment_nrc$sentiment_category <- case_when(
  sentiment_nrc$sentiment %in% c("positive", "joy", "trust") ~ "Positive",
  sentiment_nrc$sentiment %in% c("negative", "anger", "fear", "sadness", "disgust") ~ "Negative",
  TRUE ~ "Neutral"
)

# Calculate sentiment by post using NRC method
post_sentiment_nrc <- sentiment_nrc %>%
  group_by(Post_ID, sentiment_category) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = sentiment_category, values_from = count, values_fill = 0) %>%
  mutate(
    total_sentiment = Positive + Negative,
    overall_sentiment_nrc = case_when(
      total_sentiment == 0 ~ "Neutral",
      Positive > Negative ~ "Positive",
      Negative > Positive ~ "Negative",
      TRUE ~ "Neutral"
    )
  )

# Combine AFINN and NRC results for better accuracy
post_sentiment <- post_sentiment_afinn %>%
  left_join(post_sentiment_nrc %>% select(Post_ID, overall_sentiment_nrc), by = "Post_ID") %>%
  mutate(
    # Use AFINN as primary, NRC as secondary
    final_sentiment = case_when(
      # If AFINN is neutral but NRC shows strong signal, use NRC
      overall_sentiment == "Neutral" & !is.na(overall_sentiment_nrc) ~ overall_sentiment_nrc,
      # Otherwise use AFINN
      !is.na(overall_sentiment) ~ overall_sentiment,
      # Fallback to NRC if AFINN is missing
      !is.na(overall_sentiment_nrc) ~ overall_sentiment_nrc,
      # Last resort
      TRUE ~ "Neutral"
    )
  ) %>%
  select(Post_ID, sentiment_score, word_count, overall_sentiment = final_sentiment)

# Calculate overall percentages
sentiment_summary <- post_sentiment %>%
  count(overall_sentiment) %>%
  mutate(percentage = n / sum(n) * 100)

print("Sentiment Analysis Summary:")
print(sentiment_summary)

# Create word cloud of sentiment words
sentiment_words <- sentiment_nrc %>%
  count(word, sentiment_category) %>%
  arrange(desc(n))

# Filter top words for word cloud
top_sentiment_words <- sentiment_words %>%
  group_by(sentiment_category) %>%
  top_n(50, n) %>%
  ungroup()

# Create color palette
colors <- c("Positive" = "#2E8B57", "Negative" = "#DC143C", "Neutral" = "#708090")

# Word Cloud
set.seed(123)
par(mfrow = c(1, 1))
wordcloud(
  words = top_sentiment_words$word,
  freq = top_sentiment_words$n,
  min.freq = 1,
  max.words = 200,
  random.order = FALSE,
  rot.per = 0.35,
  colors = brewer.pal(8, "Dark2"),
  main = "Sentiment Words Cloud"
)
title("Sentiment Words from Mastodon Posts", line = -1)

# Bar chart of sentiment percentages
sentiment_plot <- ggplot(sentiment_summary, aes(x = overall_sentiment, y = percentage, fill = overall_sentiment)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  scale_fill_manual(values = c("Positive" = "#2E8B57", "Negative" = "#DC143C", "Neutral" = "#708090")) +
  labs(
    title = "Sentiment Analysis of Mastodon Posts",
    subtitle = "Distribution of Positive, Negative, and Neutral Sentiments",
    x = "Sentiment Category",
    y = "Percentage (%)",
    fill = "Sentiment"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "none"
  ) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 

            vjust = -0.5, size = 4, fontface = "bold") +
  ylim(0, max(sentiment_summary$percentage) * 1.1)

print(sentiment_plot)

# Pie chart of individual emotions
emotion_summary <- sentiment_nrc %>%
  count(sentiment) %>%
  mutate(percentage = n / sum(n) * 100)

# Create color palette for emotions
emotion_colors <- c(
  "positive" = "#2E8B57", "joy" = "#FFD700", "trust" = "#4169E1",
  "negative" = "#DC143C", "anger" = "#FF4500", "fear" = "#8B0000", 
  "sadness" = "#4682B4", "disgust" = "#9932CC"
)

pie_chart <- ggplot(emotion_summary, aes(x = "", y = percentage, fill = sentiment)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = emotion_colors) +
  labs(
    title = "Emotion Distribution - Pie Chart",
    subtitle = "Proportion of Individual Emotions in Posts",
    fill = "Emotion"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  ) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 3, fontface = "bold", color = "white")

print(pie_chart)

# Additional detailed analysis
cat("\n=== Detailed Sentiment Analysis ===\n")
cat("Total posts analyzed:", nrow(post_sentiment), "\n")
cat("Posts with positive sentiment:", sum(post_sentiment$overall_sentiment == "Positive"), "\n")
cat("Posts with negative sentiment:", sum(post_sentiment$overall_sentiment == "Negative"), "\n")
cat("Posts with neutral sentiment:", sum(post_sentiment$overall_sentiment == "Neutral"), "\n")

# Top sentiment words by category
cat("\n=== Top Sentiment Words ===\n")
for(sentiment_cat in c("Positive", "Negative")) {
  cat("\nTop", sentiment_cat, "words:\n")
  top_words <- sentiment_words %>%
    filter(sentiment_category == sentiment_cat) %>%
    top_n(10, n) %>%
    arrange(desc(n))
    for(i in seq_len(min(10, nrow(top_words)))) {
    cat(paste0(i, ". ", top_words$word[i], " (", top_words$n[i], " occurrences)\n"))
  }
}

# Save results to CSV
#write_csv(sentiment_summary, "sentiment_analysis_results.csv")
#write_csv(top_sentiment_words, "top_sentiment_words.csv")

#cat("\nAnalysis complete! Results saved to CSV files.\n")