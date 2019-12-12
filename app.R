library(shiny)
library(DT)
library(dplyr)
library(purrr)
library(shinyWidgets)
library(readxl)

ui <- fluidPage(
  # shinyFeedback::useShinyFeedback(),
  # 设置validation error字体为红色
  tags$head(
    tags$style(HTML("
      .shiny-output-error-validation {
        color: red;
      }
    "))
  ),
  
  numericRangeInput('start_end', '请选择起止年份', value = c(2010, 2018)),
  dataTableOutput("jcr_table")
)

server <- function(input, output, session) {
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
  
  jcr <- reactive({
    
    # 添加时间验证
    validate(
      need(min(input$start_end) > 2009, "仅支持2010至2018影响因子查询"),
      need(max(input$start_end) < 2019, "仅支持2010至2018影响因子查询")
    )
    
    # shinyFeedback 不起作用？
    # cond <- min(input$start_end) <= 2009
    # message(cond) 
    # shinyFeedback::feedbackDanger("start_end", cond, "error")
    # req(!cond, cancelOutput = FALSE)
    
    purrr::map(input$start_end[2]:input$start_end[1], read_jcr) %>% 
      reduce(full_join, by = "journal") %>% 
      set_names(c("journal", input$start_end[2]:input$start_end[1]))
  })
  
  output$jcr_table <- renderDT(
    jcr(), options = list(pageLength = 50)
  )
}

shinyApp(ui, server)