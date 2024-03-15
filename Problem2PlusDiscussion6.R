# Load necessary libraries
library(tidyverse)  # For data manipulation and visualization
library(tokenizers)  # For tokenizing text
library(quanteda)  # For text processing and creating document-feature matrices
library(stm)  # For structural topic modeling
library(ggplot2)  # For plotting
library(stringr)  # For string operations

# Set the working directory to where your datasets are located
# You need to change this path to your specific dataset location
setwd("C:/Users/mansi/OneDrive/Desktop/TextAsData")

# Load the EU negotiations dataset
metadata <- read_csv("eu_negotiations.csv")

# Convert 'gov_eu_supporter_ches' into a factor indicating Europhile or Eurosceptic
metadata$gov_eu_supporter_ches_factor <- as.factor(metadata$gov_eu_supporter_ches)

# Pre-process the text for STM analysis
corpus_sotu <- corpus(metadata, text_field = "text")
toks <- tokens(corpus_sotu, remove_punct = TRUE, remove_numbers = TRUE)
toks <- tokens_wordstem(toks)
toks <- tokens_remove(toks, stopwords("en"))
dfm_sotu <- dfm(toks)
dfm_trimmed <- dfm_trim(dfm_sotu, min_docfreq = 0.05, docfreq_type = "prop")

# Prepare the data for STM
processed <- textProcessor(documents = metadata$text, metadata = metadata)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)

# Fit the STM model with 30 topics
set.seed(12345)
model.stm <- stm(out$documents, out$vocab, K = 30, prevalence = ~ gov_eu_supporter_ches_factor, data = out$meta, max.em.its = 100, init.type = "Spectral")

# Sentiment Analysis
# Create a vector for the length of each document
text_length <- str_length(metadata$text)

# Apply the Lexicoder sentiment dictionary to the texts
# Assuming the Lexicoder dictionary is loaded as 'data_dictionary_LSD2015'
data_dictionary_LSD2015_pos_neg <- data_dictionary_LSD2015[1:2]
toks_gov_dict <- tokens_lookup(toks, dictionary = data_dictionary_LSD2015_pos_neg)
dfm_neg_pos <- dfm(toks_gov_dict)
metadata$snippet_sent_score <- as.numeric(dfm_neg_pos[,"positive"]) - as.numeric(dfm_neg_pos[,"negative"])

# Plot with text length on the x-axis and sentiment score on the y-axis (not normalized)
ggplot(metadata, aes(x = text_length, y = snippet_sent_score)) +
  geom_point(alpha = 0.5) +
  labs(x = "Length of Document (characters)", 
       y = "Sentiment Score",
       title = "Document Length vs Sentiment Score") +
  theme_minimal()

# Save the plot
ggsave("length_vs_sentiment_plot.png", width = 8, height = 6, dpi = 300)

# Normalize the sentiment score by text length
metadata <- metadata %>%
  mutate(normalized_sentiment_score = snippet_sent_score / text_length)

# Plot with text length on the x-axis and normalized sentiment score on the y-axis
ggplot(metadata, aes(x = text_length, y = normalized_sentiment_score)) +
  geom_point(alpha = 0.5) +
  labs(x = "Length of Document (characters)", 
       y = "Normalized Sentiment Score",
       title = "Document Length vs Normalized Sentiment Score") +
  theme_minimal()

# Save the normalized plot
ggsave("normalized_length_vs_sentiment_plot.png", width = 8, height = 6, dpi = 300)
# Assuming metadata has a column 'gov_eu_supporter_ches_factor' that is a factor
# with levels indicating Europhile or Eurosceptic and a column 'normalized_sentiment_score'
# with the sentiment scores.

# Calculate the mean sentiment score for Europhile governments
europhile_mean <- metadata %>%
  filter(gov_eu_supporter_ches_factor == "Europhile") %>%
  summarise(mean_sentiment = mean(normalized_sentiment_score, na.rm = TRUE))

# Calculate the mean sentiment score for Eurosceptic governments
eurosceptic_mean <- metadata %>%
  filter(gov_eu_supporter_ches_factor == "Eurosceptic") %>%
  summarise(mean_sentiment = mean(normalized_sentiment_score, na.rm = TRUE))

# Display the results
print(europhile_mean)
print(eurosceptic_mean)

# To perform a more formal comparison, you might want to conduct a t-test or similar statistical test
t_test_result <- t.test(normalized_sentiment_score ~ gov_eu_supporter_ches_factor, data = metadata)

# Display the t-test results
print(t_test_result)

# Calculate median sentiment scores for each group
median_scores <- metadata %>%
  group_by(gov_eu_supporter_ches_factor) %>%
  summarize(median_sentiment = median(normalized_sentiment_score, na.rm = TRUE))

# Display the median scores
print(median_scores)

