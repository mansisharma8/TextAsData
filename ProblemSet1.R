
# Load necessary libraries
library(quanteda)
library(readr)
library(quanteda.textplots)
library(quanteda.textstats)
# Load the data
data <- read_csv("C:/Users/mansi/OneDrive/Desktop/TextAsData/eu_negotiations.csv") # Replace with the correct file path

# Create a tokens object
tokens <- tokens(data$text, remove_punct = TRUE, remove_numbers = TRUE)

# Remove stopwords and convert to lowercase
tokens <- tokens_remove(tokens, stopwords("en"))
tokens <- tokens_tolower(tokens)

# Create a document-feature matrix
dfm <- dfm(tokens)

# Trim the document feature matrix to only include words that appear in at least 4% of the documents
dfm_trimmed <- dfm_trim(dfm, min_docfreq = 0.04 * ndoc(dfm))

# Plot a word cloud
set.seed(1234) # For reproducibility of the word cloud


# Save the word cloud to a PNG file
png("wordcloud.png", width = 800, height = 600) # Adjust the width and height as needed
textplot_wordcloud(dfm_trimmed, min_count = 10, max_words = 100, random_order = FALSE)
dev.off() # Close the graphics device


# Extract text for Euroskeptic and Pro-EU governments
text_euroskeptic <- data$text[data$gov_eu_supporter_ches == "Eurosceptic Government"]
text_proeu <- data$text[data$gov_eu_supporter_ches == "Europhile Government"]

# Create tokens
tokens_euroskeptic <- tokens(text_euroskeptic, remove_punct = TRUE, remove_numbers = TRUE) %>%
  tokens_remove(stopwords("en")) %>%
  tokens_tolower()

tokens_proeu <- tokens(text_proeu, remove_punct = TRUE, remove_numbers = TRUE) %>%
  tokens_remove(stopwords("en")) %>%
  tokens_tolower()

# Create document-feature matrices
dfm_euroskeptic <- dfm(tokens_euroskeptic)
dfm_proeu <- dfm(tokens_proeu)

# Function to find top 25 words
find_top_words <- function(dfm, n = 25) {
  # Calculate term frequency
  term_freq <- textstat_frequency(dfm, n = n)
  
  # Return the top N words
  return(term_freq$feature[1:n])
}

# Find top 25 words for both groups
top_25_words_euroskeptic <- find_top_words(dfm_euroskeptic, 25)
top_25_words_proeu <- find_top_words(dfm_proeu, 25)

# Display the top 25 words for each group
list(Euroskeptic = top_25_words_euroskeptic, ProEU = top_25_words_proeu)
