library(shiny)
library(cauda)
library(pdftools)

ui <- fluidPage(
  titlePanel("Cauda - Extract Causal Claims from Research Papers"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("pdf_file", "Upload PDF Paper", accept = ".pdf"),
      actionButton("extract_btn", "Extract Causal Claims", class = "btn-primary"),
      hr(),
      p("Once you upload a PDF, click the button above to extract causal claims using OpenAI GPT-3.5-turbo."),
      p("Make sure your OpenAI API key is set in the environment variable OPENAI_API_KEY.")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Results",
          textOutput("status"),
          br(),
          uiOutput("claims_table")
        ),
        tabPanel("Raw Output",
          verbatimTextOutput("raw_claims")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  claims_data <- reactiveVal(NULL)
  
  observeEvent(input$extract_btn, {
    req(input$pdf_file)
    
    output$status <- renderText("Processing... extracting text from PDF")
    
    tryCatch({
      pdf_path <- input$pdf_file$datapath
      
      text <- pdftools::pdf_text(pdf_path)
      full_text <- paste(text, collapse = "\n")
      
      if (nchar(full_text) == 0) {
        output$status <- renderText("Error: Could not extract text from PDF")
        return()
      }
      
      output$status <- renderText("Calling OpenAI API... this may take a moment")
      
      claims <- extract_causal_claims(full_text)
      
      claims_data(claims)
      
      output$status <- renderText("Success! Causal claims extracted.")
      
      output$raw_claims <- renderText(claims)
      
      output$claims_table <- renderUI({
        if (is.null(claims_data())) {
          return(NULL)
        }
        
        HTML(paste0(
          "<div style='font-family: monospace; white-space: pre-wrap; background: #f5f5f5; padding: 15px; border-radius: 5px;'>",
          claims_data(),
          "</div>"
        ))
      })
      
    }, error = function(e) {
      output$status <- renderText(paste("Error:", e$message))
    })
  })
}

shinyApp(ui, server)
