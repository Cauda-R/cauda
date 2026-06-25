library(shiny)
library(cauda)
library(pdftools)

ui <- fluidPage(
  titlePanel("Cauda - Extract Causal Claims & Generate DAGs"),

  sidebarLayout(
    sidebarPanel(
      h4("Step 1: Upload PDF"),
      fileInput("pdf_file", "Select PDF Paper", accept = ".pdf"),

      h4("Step 2: Extract Claims"),
      actionButton("extract_btn", "Extract Causal Claims", class = "btn-primary btn-lg"),
      br(), br(),

      h4("Step 3: Generate DAG"),
      p("Once claims are extracted, click below to create a causal DAG:"),
      actionButton("dag_btn", "Generate DAG from Claims", class = "btn-success btn-lg"),

      hr(),
      p(em("Powered by GPT-4-turbo + Cauda"), style = "color: #888; font-size: 12px;")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Status & Claims",
          h4("Status"),
          textOutput("status"),
          br(),
          h4("Extracted Claims"),
          tableOutput("claims_table"),
          br()
        ),

        tabPanel("Causal DAG",
          h4("Causal Graph"),
          plotOutput("dag_plot", height = "600px"),
          br(),
          h4("DAG Summary"),
          verbatimTextOutput("dag_summary")
        ),

        tabPanel("Debug Info",
          h4("Raw GPT-4 Response"),
          verbatimTextOutput("raw_claims"),
          br(),
          h4("Parsed Claims (R Format)"),
          verbatimTextOutput("parsed_claims_output")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Reactive values to store state
  pdf_text <- reactiveVal(NULL)
  claims_df <- reactiveVal(NULL)
  dag_obj <- reactiveVal(NULL)
  raw_response <- reactiveVal(NULL)

  # STEP 1: Extract text from PDF
  observeEvent(input$extract_btn, {
    req(input$pdf_file)

    output$status <- renderText("⏳ Extracting text from PDF...")

    tryCatch({
      pdf_path <- input$pdf_file$datapath

      # Extract text from all pages
      text_pages <- pdftools::pdf_text(pdf_path)
      full_text <- paste(text_pages, collapse = "\n")

      if (nchar(full_text) == 0) {
        output$status <- renderText("❌ Error: Could not extract text from PDF. Is it a valid PDF?")
        return()
      }

      pdf_text(full_text)
      output$status <- renderText(sprintf("✓ PDF loaded (%d chars). Calling OpenAI API...", nchar(full_text)))

      # Call improved extraction function
      output$status <- renderText("⏳ Extracting claims with GPT-4-turbo... (this may take 30-60 seconds)")

      claims <- cauda::cauda.extract(
        full_text,
        model = "gpt-4-turbo",
        temperature = 0.3,
        max_tokens = 4000
      )

      # Also get raw response for debugging
      raw_resp <- cauda::cauda.extract(
        full_text,
        model = "gpt-4-turbo",
        temperature = 0.3,
        max_tokens = 4000,
        return_raw_text = TRUE
      )
      raw_response(raw_resp)

      # Store the dataframe
      claims_df(claims)

      # Update status and display results
      n_claims <- nrow(claims)
      output$status <- renderText(
        sprintf("✓ Success! Extracted %d causal claims. Ready to generate DAG.", n_claims)
      )

      # Display claims as table
      output$claims_table <- renderTable({
        if (nrow(claims) == 0) {
          return(data.frame(Message = "No claims extracted. Try a different paper or section."))
        }

        # Format for display - show most important columns
        display_df <- claims[, c("source", "target", "confidence", "effect_size", "p_value", "pathway")]
        colnames(display_df) <- c("Source", "Target", "Conf.", "Effect Size", "P-Value", "Pathway")

        # Truncate for readability
        display_df$Source <- substr(display_df$Source, 1, 20)
        display_df$Target <- substr(display_df$Target, 1, 20)

        display_df
      }, striped = TRUE, hover = TRUE, width = "100%")

      # Display parsed claims for debugging
      output$parsed_claims_output <- renderPrint({
        print(claims)
      })

      # Display raw response for debugging
      output$raw_claims <- renderText(raw_resp)

    }, error = function(e) {
      output$status <- renderText(paste("❌ Error:", e$message))
    })
  })

  # STEP 2: Generate DAG from extracted claims
  observeEvent(input$dag_btn, {
    req(claims_df())

    if (nrow(claims_df()) == 0) {
      output$status <- renderText("❌ No claims available. Extract claims first.")
      return()
    }

    output$status <- renderText("⏳ Generating causal DAG...")

    tryCatch({
      # Create DAG from claims using cauda function
      dag <- cauda::cauda.claims_to_dag(
        claims_df(),
        confidence_threshold = "low",
        include_speculative = TRUE,
        verbose = TRUE
      )

      dag_obj(dag)
      output$status <- renderText("✓ DAG generated successfully!")

      # Plot the DAG
      output$dag_plot <- renderPlot({
        if (is.null(dag_obj())) {
          return(NULL)
        }

        # Use cauda's DAG plotting function
        tryCatch({
          cauda::cauda.dag_theory(dag_obj(), verbose = TRUE)
        }, error = function(e) {
          # Fallback to basic bnlearn plotting
          plot(dag_obj(), main = "Causal DAG from Extracted Claims")
        })
      })

      # Display DAG summary
      output$dag_summary <- renderPrint({
        if (is.null(dag_obj())) {
          cat("DAG not yet generated.\n")
          return()
        }

        dag <- dag_obj()
        cat("=== DAG Summary ===\n")
        cat("Nodes:", length(bnlearn::nodes(dag)), "\n")
        cat("Edges:", nrow(bnlearn::arcs(dag)), "\n")
        cat("\nEdges:\n")
        print(bnlearn::arcs(dag))
        cat("\n")
      })

    }, error = function(e) {
      output$status <- renderText(paste("❌ Error generating DAG:", e$message))
    })
  })

  # Initialize empty outputs
  output$status <- renderText("📄 Upload a PDF to get started")
  output$claims_table <- renderTable(
    data.frame(Step = "1. Upload PDF", Action = "Select a research paper"),
    width = "100%"
  )
  output$dag_plot <- renderPlot({
    plot(1, type = "n", axes = FALSE, main = "DAG will appear here")
    text(1, 1, "Generate claims first", cex = 1.5, col = "gray")
  })
  output$dag_summary <- renderPrint({
    cat("DAG not yet generated. Extract claims and click 'Generate DAG'.\n")
  })
  output$raw_claims <- renderText("Raw GPT-4 response will appear here")
  output$parsed_claims_output <- renderPrint({
    cat("Parsed claims dataframe will appear here\n")
  })
}

# Run the app
shinyApp(ui, server)
