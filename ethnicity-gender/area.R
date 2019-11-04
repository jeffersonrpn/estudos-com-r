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
  inner_join(ethnicity, by = "full_name")

ethnicity_by_area <- ethnicity_gender %>%
  gather(area, area_value, cs, de, se, th)

ethnicity_porportion <- ethnicity_by_area %>% 
  group_by(l1, area) %>% 
  summarise(area_count = sum(area_value)) %>% 
  mutate(area_name = case_when(area == "cs" ~ "Computer Science",
                               area == "de" ~ "Data Engineering",
                               area == "se" ~ "Software Engineering",
                               area == "th" ~ "Theory")) %>% 
  ungroup() %>% 
  select(l1, area_name, area_count)
  
names(ethnicity_porportion) = c("ethnicity", "area", "count")

write_csv(ethnicity_porportion, "ethnicity_by_area_on_computer_science_conferences.csv")
