library(shiny)
library(DT)
library(dplyr)

ui <- fluidPage(
  selectInput("year", label = "请选择年份", choices = 2019),
  dataTableOutput("jcr_table")
)

server <- function(input, output, session) {
  jcr <- reactive({
    readxl::read_xlsx(file.path("data", paste0(input$year,".xlsx")),
      skip = 2) %>% 
      select(one_of(c("Rank", "Full Journal Title", "Total Cites",
        "Journal Impact Factor", "Eigenfactor Score"
        )))
      
  })

  output$jcr_table <- renderDT(
    jcr(),
    options = list(pageLength = 50))
}

shinyApp(ui, server)