library("tidyverse")

conferences <- read.csv("data/conferences.csv", header = FALSE, stringsAsFactors = FALSE)
names(conferences) = c("conf_global_key", "conf_key", "year", "publisher", "conf_title", "conf_link", "cs", "de", "se", "th")

papers <- read.csv("data/papers.csv", header = FALSE, stringsAsFactors = FALSE)
names(papers) <- c("paper_global_key", "paper_key", "conf_key", "paper_link", "paper_title", "pages", "citations")

authors <- read.csv("data/authors.csv", header = FALSE, stringsAsFactors = FALSE)
names(authors) = c("author_paper_key", "position", "full_name")

ethnicity <- read.csv("data/ethnicity.csv", header = FALSE, stringsAsFactors = FALSE)
names(ethnicity) = c("full_name", "l0", "l1", "l2", "gender")

ethnicity_gender <- conferences %>%
  inner_join(papers, by = "conf_key") %>%
  inner_join(authors, by = c("paper_key" = "author_paper_key")) %>%
  inner_join(ethnicity, by = "full_name") %>% 
  select("conf_key", "year", "conf_title", "paper_title", "full_name", "l0", "l1" ,"l2")

ethnicity_by_year <- ethnicity_gender %>%
  group_by(year, l1) %>%
  summarise(ethnicity_count = n())

names(ethnicity_by_year) =  c("year", "ethnicity", "ethnicity_count")

ethnicity_by_year <- ethnicity_by_year %>%
  group_by(year) %>%
  mutate(year_count = sum(ethnicity_count), proportion = ethnicity_count / year_count) %>% 
  # filter(year_count >= 1000) %>%
  select(year, ethnicity, ethnicity_count, proportion)

names(ethnicity_by_year) =  c("year", "ethnicity", "count", "proportion")

write_csv(ethnicity_by_year, "ethnicity_by_year_on_computer_science_conferences.csv")
