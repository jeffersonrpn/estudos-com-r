library("tidyverse")

conferences <- read.csv("data/conferences.csv", header = FALSE, stringsAsFactors = FALSE)
names(conferences) = c("conf_global_key", "conf_key", "year", "publisher", "conf_title", "conf_link", "cs", "de", "se", "th")

papers <- read.csv("data/papers.csv", header = FALSE, stringsAsFactors = FALSE)
names(papers) <- c("paper_global_key", "paper_key", "conf_key", "paper_link", "paper_title", "pages", "citations")

authors <- read.csv("data/authors.csv", header = FALSE, stringsAsFactors = FALSE)
names(authors) = c("author_paper_key", "position", "full_name")

ethnicity <- read.csv("data/ethnicity.csv", header = FALSE, stringsAsFactors = FALSE)
names(ethnicity) = c("full_name", "l0", "l1", "l2", "gender")

ethnicity_gender <- papers %>%
  inner_join(authors, by = c("paper_key" = "author_paper_key")) %>%
  inner_join(ethnicity, by = "full_name")

authors_papers <- ethnicity_gender %>%
  group_by(full_name, l1) %>% 
  summarise(publications = n())

names(authors_papers) = c("full_name", "ethnicity", "count")

write_csv(authors_papers, "authors_on_computer_science_conferences.csv")
