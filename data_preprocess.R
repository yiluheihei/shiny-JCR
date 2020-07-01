library(readxl)
library(dplyr)

#' load journal citation reports
#' 
#' @param year integer, the JCR year to load
#' @return a two length data frame, contains 'Full Journal Title' and "Journal
#' Impact Factor'
read_jcr <- function(year, n_max) {
  file <- file.path("data", paste0(year,".xlsx"))
  var_names <- c("rank", "journal", "total_cites", "bad",
    "impact_f", "eigenfactor_score")
  jcr <- read_xlsx(file, skip = 3, col_names = var_names) %>%
    distinct() %>% 
    select(journal, impact_f) %>% 
    slice((-n() + 1):-n()) %>% 
    mutate(impact_f = ifelse(impact_f == "Not Available", NA, impact_f))
  
  jcr
}

year_end <- 
  list.files("data/", all.files = FALSE, pattern = "^\\d+") %>% 
  tools::file_path_sans_ext() %>% 
  as.numeric() %>% 
  max()

jcr <- purrr::map(year_end:2010, read_jcr) %>% 
  reduce(full_join, by = "journal") %>% 
  set_names(c("journal", year_end:2010)) %>% 
  mutate(across(-journal, as.numeric))

readr::write_tsv(jcr, "data/jcr.tsv")
