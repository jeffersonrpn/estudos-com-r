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

ethnicity_conferences <- ethnicity_gender %>%
  group_by(conf_title, l1) %>%
  summarise(ethnicity_count = n())

ethnicity_porportion <- ethnicity_conferences %>% 
  group_by(conf_title) %>% 
  mutate(conference_count = sum(ethnicity_count), 
         proportion = ethnicity_count / conference_count, 
         homogeneity = sd(proportion)) %>% 
  filter(conference_count >= 1000) %>% 
  arrange(desc(proportion)) %>% 
  select(conf_title, l1, ethnicity_count, proportion, homogeneity)  
  
names(ethnicity_porportion) = c("conference", "ethnicity", "count", "proportion", "homogeneity")

write_csv(ethnicity_porportion, "ethnicity_proportion_on_computer_science_conferences.csv")
