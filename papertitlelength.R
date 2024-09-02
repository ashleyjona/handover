# Libraries
if (!requireNamespace("rentrez", quietly = TRUE)) {
  install.packages("rentrez")
}

if (!requireNamespace("stringr", quietly = TRUE)) {
  install.packages("stringr")
}

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

library(rentrez)
library(stringr)
library(ggplot2)

data <- read.csv("Julia Adjudicated.csv", stringsAsFactors = FALSE)

# first 50 PubMed IDs
pubmed_ids <- as.character(data$PubMed.Id[1:50])

# fetch article titles from PubMed
fetch_titles <- function(pubmed_ids) {
  titles <- c()
  for (id in pubmed_ids) {
    # Fetch summary information for the PubMed ID
    summary <- entrez_summary(db = "pubmed", id = id)
    # Extract the title from the summary
    title <- summary$title
    titles <- c(titles, title)
  }
  return(titles)
}

# first 50 PubMed IDs
titles <- fetch_titles(pubmed_ids)

# stats
calculate_statistics <- function(titles) {
 
  lengths <- nchar(titles)
  
  mean_length <- mean(lengths)
  std_dev <- sd(lengths)
  min_length <- min(lengths)
  max_length <- max(lengths)
  
  stats <- list(
    mean_length = mean_length,
    std_dev = std_dev,
    min_length = min_length,
    max_length = max_length,
    lengths = lengths
  )
  
  return(stats)
}

stats <- calculate_statistics(titles)
print(stats)

# TITLE LENGTHS
title_lengths <- data.frame(length = stats$lengths)

# histogram visualisation
ggplot(title_lengths, aes(x = length)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = stats$mean_length), color = "red", linetype = "dashed", size = 1) +
  labs(title = "Distribution of PubMed Article Title Lengths",
       x = "Title Length (Number of Characters)",
       y = "Frequency") +
  theme_minimal()

# detailed statistics
cat("Mean Length: ", stats$mean_length, "\n")
cat("Standard Deviation: ", stats$std_dev, "\n")
cat("Minimum Length: ", stats$min_length, "\n")
cat("Maximum Length: ", stats$max_length, "\n")
