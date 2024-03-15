# Install necessary packages if they are not already installed
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("tokenizers")) install.packages("tokenizers")
if (!require("quanteda")) install.packages("quanteda")
if (!require("stm")) install.packages("stm")

# Load package libraries
library(tidyverse)
library(tokenizers)
library(quanteda)
library(stm)
library(ggplot2)

# Set working directory to the location of your CSV file
# Replace with your actual file path
setwd("C:/Users/mansi/OneDrive/Desktop/TextAsData")

# Load data from CSV file
# Replace 'eu_negotiations.csv' with your actual file name
metadata <- read_csv("eu_negotiations.csv")

# Check the data structure
print(head(metadata))

# Create a corpus of the text
corpus_sotu <- corpus(metadata, text_field = "text")

# Pre-process the text
toks <- tokens(corpus_sotu, remove_punct = TRUE, remove_numbers = TRUE)
toks <- tokens_wordstem(toks)
toks <- tokens_remove(toks, stopwords("en"))
dfm_sotu <- dfm(toks)
dfm_trimmed <- dfm_trim(dfm_sotu, min_docfreq = 0.05, docfreq_type = "prop")

# Prepare the data for STM
processed <- textProcessor(documents = metadata$text, metadata = metadata)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)

# Fit the STM model
# Here we are using 'gov_eu_supporter_ches' as a covariate for topic prevalence
# and 'Actor' (assuming it is a variable in your data) for content covariate
# Adjust the number of topics (K) and other parameters as needed for your analysis
# Check for NA values after coercion to numeric
metadata$gov_eu_supporter_ches_numeric <- as.numeric(metadata$gov_eu_supporter_ches)
if(any(is.na(metadata$gov_eu_supporter_ches_numeric))) {
  warning("NA values introduced by coercion to numeric. Check 'gov_eu_supporter_ches' values.")
}

# Ensure the number of rows in metadata matches the number of documents
if(nrow(metadata) != length(corpus_sotu)) {
  stop("The number of metadata rows does not match the number of documents in the corpus.")
}

# Proceed with the STM model without covariates to ensure that documents are correctly processed
simple_model.stm <- stm(out$documents, out$vocab, K = 20, max.em.its = 10)

# If simple model runs without error, proceed to add covariates one by one
# For now, let's assume 'gov_eu_supporter_ches' is a factor. If it is supposed to be numeric,
# make sure it does not contain categories or non-numeric data before converting
metadata$gov_eu_supporter_ches_factor <- as.factor(metadata$gov_eu_supporter_ches)

# Re-prep the documents after ensuring covariate is correctly formatted
processed <- textProcessor(documents = metadata$text, metadata = metadata)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)

# Fit the STM model with the factor covariate
model.stm.with_covariate <- stm(out$documents, out$vocab, K = 20,
                                prevalence = ~ gov_eu_supporter_ches_factor,
                                data = out$meta, max.em.its = 10)
# Fit the STM model with 30 topics
set.seed(12345)
model.stm <- stm(out$documents, out$vocab, K = 30, prevalence = ~ gov_eu_supporter_ches_factor, data = out$meta, max.em.its = 100, init.type = "Spectral")
# Use estimateEffect to examine differences in topic prevalence
# Assuming 'gov_eu_supporter_ches' is correctly coded as a factor indicating Europhile vs Eurosceptic
effects <- estimateEffect(1:30 ~ gov_eu_supporter_ches, model.stm, metadata = out$meta)

# Plot the effects for the two topics of interest
# Replace 'x' and 'y' with the actual topic numbers you identified for Europhile and Eurosceptic discussions
plot(effects, topics = c(24, 7), covariate = "gov_eu_supporter_ches", model = model.stm)

# Label the topics and get the most probable words for each
topics <- labelTopics(model.stm)
# Assuming 'model.stm' is your fitted STM model
# Get the top terms for each topic directly from the model
top_terms <- stm::labelTopics(model.stm, n = 5)$top.terms

# Assuming 'topic_labels' contains your custom topic labels
topic_labels <- c(
  "Diplomacy", "Trade Agreements", "Economic Policy", "Immigration", "Defense",
  "Legislation", "Environmental Policy", "Healthcare", "Education", "Technology",
  "Foreign Aid", "Human Rights", "Employment", "Energy", "Agriculture",
  "Transportation", "Finance", "Tourism", "Social Welfare", "Security",
  "International Relations", "Justice", "Cultural Affairs", "Sports", "Infrastructure",
  "Research and Development", "Consumer Rights", "Rural Development", "Urban Planning", "Digitalization"
)

# Create a data frame to store the labeled topics and their top words
labeled_topics <- data.frame(
  Label = topic_labels,
  Word1 = character(30),
  Word2 = character(30),
  Word3 = character(30),
  Word4 = character(30),
  Word5 = character(30),
  stringsAsFactors = FALSE
)

# Using 'findThoughts' as a safer alternative to directly accessing potentially problematic structures
num_terms <- 5  # Number of terms to retrieve for each topic
top_terms_list <- list()

for (i in 1:30) {
  # Attempt to retrieve top terms for each topic and handle any potential errors gracefully
  tryCatch({
    top_terms <- stm::findThoughts(model.stm, topics = i, n = num_terms, texts = NULL)$words[[1]]
    top_terms_list[[i]] <- top_terms
  }, error = function(e) {
    # If there's an error, store NA values to keep the structure intact
    top_terms_list[[i]] <- rep(NA, num_terms)
  })
}

# Now, construct the labeled_topics data frame
labeled_topics <- data.frame(Label = topic_labels, stringsAsFactors = FALSE)

# Add columns for the top words
for (i in 1:num_terms) {
  labeled_topics[paste0("Word", i)] <- sapply(top_terms_list, function(x) ifelse(is.null(x[i]), NA, x[i]))
}

# Print the labeled topics with words
print(labeled_topics)
# Check the model's convergence and results
print(summary(model.stm))


# Visualize the topics
plot(model.stm, type = "summary")

# Find representative documents for certain topics
findThoughts(model.stm, texts = out$meta$text, topics = 1, n = 3)

# Estimate the relationship between covariates and topics
model.stm.ee <- estimateEffect(1:20 ~ gov_eu_supporter_ches, model.stm, meta = out$meta)

# Plot the effects for each topic separately to avoid overlap
for (i in 1:20) {
  plot(model.stm.ee, "gov_eu_supporter_ches", topics = i) +
    ggtitle(paste("Effect of gov_eu_supporter_ches on Topic", i)) +
    theme_minimal() +
    theme(text = element_text(size = 10), axis.text.x = element_text(angle = 45, hjust = 1))
  
  # You can uncomment the next line if you want to pause between plots
  # if (interactive()) readline(prompt="Press [enter] to continue")
}
# Visualization of the effect of 'Actor' on topics, if 'Actor' is a numeric covariate
# If 'Actor' is not numeric, you would need to adjust this part of the code
plot(model.stm.ee, "Actor", method = "continuous", topics = c(1, 2))
