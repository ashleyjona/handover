# Libraries
library(dplyr)
library(tidyr)
library(stringr)

data <- read.csv("First Grid View.csv")
print("Column names in the dataset:")
print(colnames(data))

data$Disease <- gsub('"', '', data$Disease)

split_diseases <- str_split(data$Disease, pattern = ",\\s*")

all_diseases <- data.frame(PubMed_Id = rep(data$PubMed.Id, sapply(split_diseases, length)), Disease = trimws(unlist(split_diseases)))

# deduplicate 
all_diseases <- all_diseases %>% distinct(PubMed_Id, Disease, .keep_all = TRUE)

# filter out rows with actions
filtered_data <- data %>%
  filter(
    (is.na(Type.0.Action) | Type.0.Action == "") &
      (is.na(Type.1.Action) | Type.1.Action == "") &
      (is.na(Type.2.Action) | Type.2.Action == "") &
      (is.na(Type.3.Action) | Type.3.Action == "")
  )

split_diseases_filtered <- str_split(filtered_data$Disease, pattern = ",\\s*")

all_diseases_filtered <- data.frame(PubMed_Id = rep(filtered_data$PubMed.Id, sapply(split_diseases_filtered, length)), Disease = trimws(unlist(split_diseases_filtered)))

# Extra work for comma split Mesh terms
all_diseases_filtered$Disease[grep("^Gestational$", all_diseases_filtered$Disease)] <- "Condition:Diabetes, Gestational"

all_diseases_filtered$Disease[grep("Arthritis", all_diseases_filtered$Disease)] <- "Condition:Arthritis, Rheumatoid"

# Check the unique values in Disease column
print("Unique diseases after modification:")
print(unique(all_diseases_filtered$Disease))

all_diseases_filtered <- all_diseases_filtered %>% distinct(PubMed_Id, Disease, .keep_all = TRUE)

gestational_diabetes_pmids <- all_diseases_filtered %>%
  filter(Disease == "Condition:Diabetes, Gestational") %>%
  select(PubMed_Id) %>%
  distinct()

print("PubMed IDs for 'Condition:Diabetes, Gestational':")
print(gestational_diabetes_pmids)

stress_disorders_pmids <- all_diseases_filtered %>%
  filter(Disease == "Condition:Arthritis, Rheumatoid") %>%
  select(PubMed_Id) %>%
  distinct()

print("PubMed IDs for 'Arthritis, Rheumatoid':")
print(stress_disorders_pmids)

# LIST OF CONDITIONS
diseases_to_count <- c("Condition:Chronic Fatigue Syndrome", "Condition:Fibromyalgia", "Condition:Sleep Initiation and Maintenance Disorders", "Condition:Sleep Wake Disorders",
                       "Condition:Back Pain", "Condition:Low Back Pain", "Condition:Chronic Pain", "Condition:Joint Pain", "Condition:Osteoporosis", "Condition:Arthritis", "Condition:Arthritis, Rheumatoid")

# Count 
disease_counts <- all_diseases_filtered %>%
  filter(Disease %in% diseases_to_count) %>%
  count(Disease, name = "Count")

print("Disease counts:")
print(disease_counts)
