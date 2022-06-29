library(readxl)
library(dplyr)

#' load journal citation reports
#' 
#' @param year integer, the JCR year to load
#' @return a two length data frame, contains 'Full Journal Title' and "Journal
#' Impact Factor'
read_jcr <- function(year) {
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

# year_end <- 
#   list.files("data/", all.files = FALSE, pattern = "^\\d+") %>% 
#   tools::file_path_sans_ext() %>% 
#   as.numeric() %>% 
#   max()
year_end <- 2020

jcr <- purrr::map(year_end:2010, read_jcr) %>% 
  purrr::reduce(full_join, by = "journal") %>% 
  rlang::set_names(c("journal", year_end:2010)) %>% 
  mutate(across(-journal, as.numeric))

# readr::write_tsv(jcr, "data/jcr.tsv")


#-------------------------------- 2022 ----------------------------------------
# http://jcr.help.clarivate.com/Content/downloading.htm
# 2022 年开始 JCR 不像之前那样可以下载全部 SCIE 和 SSCI 的数据
# 下载 SCIE 和 SSCI 杂志列表 https://mjl.clarivate.com/collection-list-downloads
# 然后根据从网上下载的 2021 年的 IF 提取 SCIE 和 SSCI 相关杂志的IF
# 
# 2022 公布的影响因子是 2021年的
scie_2022 <- read.csv("data/wos-core_SCIE 2022-June-22.csv")
ssci_2022 <- read.csv("data/wos-core_SSCI 2022-June-22.csv")
journals <- c(scie_2022$Journal.title, ssci_2022$Journal.title) |> 
  toupper()
journals <- journals[!duplicated(journals)]
journals_if_2021 <- readxl::read_xlsx("data/2021.xlsx")
journals_if_2021$journal_name <- toupper(journals_if_2021$journal_name)
if_2021 <- journals_if_2021[ match(journals, journals_if_2021$journal_name, nomatch = 0), ] |> 
  dplyr::mutate(IF = ifelse(if_2022 == "N/A", NA, as.numeric(if_2022)))

if_2021 <- dplyr::select(
  if_2021,
  journal = journal_name, 
  `2021` = IF
)

if_before_2021 <- dplyr::mutate(jcr, journal = toupper(journal))

if_2010_2021 <- dplyr::full_join(if_2021, if_before_2022) |> 
  dplyr::arrange(desc(`2021`))
write.table(
  if_2010_2021, 
  file = "data/if_2020_2021.tsv", 
  sep = "\t", 
  row.names = FALSE,
  quote = FALSE
)
  