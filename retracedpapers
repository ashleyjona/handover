install.packages("httr")
install.packages("jsonlite")

library(httr)
library(jsonlite)

check_retractions <- function(pubmed_ids) {
  base_url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi"
  retracted_ids <- c()
  
  for (pubmed_id in pubmed_ids) {
    response <- GET(base_url, query = list(db = "pubmed", id = pubmed_id, retmode = "json"))
    
    if (status_code(response) == 200) {
      data <- fromJSON(content(response, "text", encoding = "UTF-8"))
      print(data)  # Debug output
      
      if (!is.null(data$result[[as.character(pubmed_id)]]$pubtype)) {
        pubtype <- data$result[[as.character(pubmed_id)]]$pubtype
        print(pubtype)  # Debug output
        
        if ("Retracted Publication" %in% pubtype) {
          retracted_ids <- c(retracted_ids, pubmed_id)
        }
      } else {
        warning("No publication type found for PubMed ID: ", pubmed_id)
      }
    } else {
      warning("Failed to retrieve data for PubMed ID: ", pubmed_id)
    }
  }
  
  return(retracted_ids)
}

install.packages("dplyr")
library(dplyr)

names(Second_Attempt_Grid_5)[1] <- "pubmed_ids"
pubmed_ids <- as.character(Second_Attempt_Grid_5$pubmed_ids)

# Example usage with a known retracted PMID
pubmed_ids <- c("35251295", "32594730") # Known retracted article
retracted_papers <- check_retractions(pubmed_ids)
print(paste("Retracted papers:", paste(retracted_papers, collapse = ", ")))


