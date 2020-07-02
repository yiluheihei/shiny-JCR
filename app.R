library(shiny)
library(DT)
library(dplyr)
library(purrr)
library(shinyWidgets)
# library(b)

# 终止年限
year_end <- 
  list.files("data/", all.files = FALSE, pattern = "^\\d+") %>% 
  tools::file_path_sans_ext() %>% 
  as.numeric() %>% 
  max()

ui <- 
  tagList(
    
    # shinyFeedback::useShinyFeedback(),
    # 设置validation error字体为红色
    tags$head(tags$style("
      .shiny-output-error-validation {
        color: red;
      }
      .navbar {display: flex; font-size:1.8rem}
    ")),
    
    navbarPage(title="",
  
      tabPanel("2010年至今影响因子查询",
        fluidRow(
          column(2, numericRangeInput('start_end', '时间', 
            value = c(2010, year_end), separator = "至")),
          column(10, DT::dataTableOutput("jcr_table"))
        )
      ),
  
  
      # navbarMenu不支持tabPanel列表,所以不能直接使用lapply(map)构建tabPanel列表，
      # 使用do.call unlist the tabPanel list
      # https://stackoverflow.com/questions/42539946/looping-to-create-tabs-in-tabsetpanel-in-shiny?answertab=active#tab-top
      # 
      do.call(function(...) navbarMenu(..., icon = icon("save")),
        c(title = "下载",
          list(tabPanel(downloadLink("all", paste0("2010~", year_end)))),
          purrr::map(2019:2010,
            ~ tabPanel(downloadLink(paste0("year", .x), .x))))
      )
    )
    
  )

server <- function(input, output, session) {
 
  
  # jcr data.frame
  jcr <- reactive({
    # 添加时间验证
    validate(
      need(
        min(input$start_end) > 2009, 
        paste0("仅支持2010至", year_end, "影响因子查询")
      ),
      need(
        max(input$start_end) < year_end + 1, 
        paste0("仅支持2010至", year_end, "影响因子查询")
      )
    )
    
    # shinyFeedback 不起作用？
    # cond <- min(input$start_end) <= 2009
    # message(cond) 
    # shinyFeedback::feedbackDanger("start_end", cond, "error")
    # req(!cond, cancelOutput = FALSE)
    readr::read_tsv("data/jcr.tsv")  
   
  })
  
  output$jcr_table <- DT::renderDataTable(
    jcr(), options = list(pageLength = 50)
  )
  
  # download for each year
  purrr::map(
    year_end:2010,
    function(i) {
      output[[paste0("year", i)]] <- downloadHandler(
        filename = function() {
          paste0(i, ".xlsx")
        },
        content = function(file) {
          # 为什么paste0("data/", file)不可以
          file.copy(paste0("data/",  paste0(i, ".xlsx")), file)
        },
        contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
      )
    }
  )
  # download all
  output$all <- downloadHandler(
    filename = function() {
      paste0("2010_", year_end, ".csv")
    },
    content = function(file) {
      readr::write_csv(jcr(), file)
    }
  )
}

shinyApp(ui, server)