#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(readxl)
library(tidyverse)
library(pals)

# color_by <- varSelectInput(
#   "color_by", "Color by",
#   penguins[c("species", "island", "sex")],
#   selected = "species"
# )

# Define UI for application
ui <- page_navbar(
  # Application title
  title = "Analyse MCQ Performance",
  sidebar = fileInput(inputId = 'upload', label = 'Upload File'),
  # tabPanel("Input", fileInput('upload', 'Upload File...')),
  # tabPanel("Plots", verbatimTextOutput("summary")),
  # tabPanel("Table", contents)
  navset_card_underline(
    title = "Explore MCQ Stats",
    nav_panel("Table", tableOutput("contents")),
    nav_panel("Q Summaries", tableOutput("summary")),
    nav_panel("Plots", plotOutput("plot_grid"))
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  output$contents <- renderTable({
    # input$upload will be NULL initially. After the user selects
    # and uploads a file, all rows then shown
    req(input$upload)
    
    df <- read_xlsx(input$upload$datapath, sheet = "Sheet1")
    #df <- read_xlsx('example_binary.xlsx', sheet="Sheet1")
    df <- df %>% mutate(across(1:ncol(df), as.integer))
    
    tidy_df <- pivot_longer(
      df,
      cols = starts_with("Question"),
      names_to = "Question",
      names_prefix = "Question_",
      values_to = "correct"
    )
    
    df <- mutate(df, CandidateScore = rowSums(df[, 2:ncol(df)]))
    df <- arrange(df, desc(CandidateScore))
    #
    nCandidate <- nrow(df)
    nQuestion <- ncol(df) - 2
    #
    quartileSize <- nCandidate / 4
    #
    firstQ <- round(nCandidate * 0.25)
    secondQ <- round(nCandidate * 0.5)
    thirdQ <- round(nCandidate * 0.75)
    forthQ <- round(nCandidate)
    
    #
    
    return(df)
  })
  output$summary <- renderTable({
    req(input$upload)
    summaries <- tibble(Question = colnames(df)[2:(ncol(df)-1)],
                        SHigh = NA,
                        SLow= NA,
                        DI = NA,
                        Difficulty=NA)
    summaries$SLow <- colSums(df[(thirdQ + 1):forthQ, 2:(ncol(df)-1)])
    summaries$SHigh <- colSums(df[1:firstQ, 2:(ncol(df)-1)])
    summaries <- mutate(summaries, DI=(SHigh - SLow) / (0.27 * nCandidate))
    summaries$Difficulty <- colSums(df[,2:(ncol(df)-1)]) / nCandidate
    return(summaries)
  })
  output$plot_grid <- renderPlot({
    req(input$upload)
    i = 1
    while (i <= nQuestion) {
      if (i == 1) {
        fp <- data.frame(
          Question = rep(colnames(df)[i+1], 4),
          quartile = paste0('Q', 1:4),
          QScore = c(sum(df[1:firstQ, i + 1]), 
                     sum(df[(firstQ + 1):secondQ, i + 1]), 
                     sum(df[(secondQ + 1):thirdQ, i + 1]), 
                     sum(df[(thirdQ + 1):forthQ, i + 1]))
        )
      }
      else {
        fp <- rbind(fp,
                    data.frame(
                      Question = rep(colnames(df)[i+1], 4),
                      quartile = paste0('Q', 1:4),
                      QScore = c(sum(df[1:firstQ, i + 1]), 
                                 sum(df[(firstQ + 1):secondQ, i + 1]), 
                                 sum(df[(secondQ + 1):thirdQ, i + 1]), 
                                 sum(df[(thirdQ + 1):forthQ, i + 1]))
                    ))
      }
      
      i <- i + 1
    }
    fp$Question <- factor(fp$Question, levels=paste0("Question_", 1:(nQuestion+10)))
    p <- ggplot(fp, aes(x = quartile, y = QScore)) +
      geom_bar(stat = 'identity', aes(fill=quartile)) + 
      scale_fill_manual(values=stepped3(4)) +
      facet_wrap(~Question) +
      theme_minimal() +
      theme(legend.position="none")
    return(p)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
