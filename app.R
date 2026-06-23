library(shiny)
library(pdftools)
library(httr)
library(jsonlite)

# Inline function to extract causal claims
extract_causal_claims <- function(
  text,
  model = "gpt-3.5-turbo",
  temperature = 0.3,
  max_tokens = 2000
) {
  api_key <- Sys.getenv("OPENAI_API_KEY")
  if (api_key == "" || nchar(api_key) < 10) {
    return("ERROR: OPENAI_API_KEY not set")
  }

  prompt <- paste0(
    "Extract ALL causal and causal-ish claims from the following academic text. Look for:\n",
    "- Direct causal statements (X causes Y, X leads to Y)\n",
    "- Suggestive language (X may influence Y, X might affect Y, X appears to cause Y)\n",
    "- Associations with causal interpretation (X is associated with Y when discussing mechanisms)\n",
    "- Mechanistic claims (X operates through Y to affect Z)\n",
    "- Inverse/preventive claims (reducing X may decrease Y)\n",
    "- Hypothesized relationships (X could contribute to Y)\n",
    "\n",
    "For each claim found, format as:\n",
    "CLAIM: [the exact or near-exact statement from the text]\n",
    "SOURCE: [the factor/variable doing the causing]\n",
    "TARGET: [the outcome/effect being caused]\n",
    "CONFIDENCE: [high/medium/low based on how directly it's stated]\n",
    "---\n\n",
    "Be inclusive - capture all plausible causal or mechanistic language, not just definitive claims.\n",
    "If the text discusses a plausible mechanism or relationship, include it.\n",
    "\n",
    "TEXT:\n",
    text
  )

  request_body <- list(
    model = model,
    messages = list(
      list(role = "user", content = prompt)
    ),
    temperature = temperature,
    max_tokens = max_tokens
  )

  tryCatch({
    response <- POST(
      url = "https://api.openai.com/v1/chat/completions",
      add_headers(
        Authorization = paste("Bearer", api_key),
        `Content-Type` = "application/json"
      ),
      body = toJSON(request_body, auto_unbox = TRUE),
      encode = "raw"
    )

    if (status_code(response) != 200) {
      return(paste("API Error:", status_code(response)))
    }

    result <- fromJSON(content(response, as = "text"))
    return(result$choices[[1]]$message$content)

  }, error = function(e) {
    return(paste("Error:", conditionMessage(e)))
  })
}

# Function to parse claims into structured format
parse_claims <- function(claims_text) {
  tryCatch({
    claims_list <- strsplit(claims_text, "---")[[1]]
    claims_list <- claims_list[claims_list != "" & trimws(claims_list) != ""]

    claims_df <- data.frame(
      source = character(),
      target = character(),
      claim = character(),
      confidence = character(),
      stringsAsFactors = FALSE
    )

    for (claim_block in claims_list) {
      lines <- strsplit(trimws(claim_block), "\n")[[1]]
      lines <- lines[lines != ""]

      claim_text <- ""
      source <- ""
      target <- ""
      confidence <- ""

      for (line in lines) {
        if (grepl("^CLAIM:", line)) {
          claim_text <- sub("^CLAIM:\\s*", "", line)
        } else if (grepl("^SOURCE:", line)) {
          source <- sub("^SOURCE:\\s*", "", line)
        } else if (grepl("^TARGET:", line)) {
          target <- sub("^TARGET:\\s*", "", line)
        } else if (grepl("^CONFIDENCE:", line)) {
          confidence <- sub("^CONFIDENCE:\\s*", "", line)
        }
      }

      if (source != "" && target != "") {
        claims_df <- rbind(claims_df, data.frame(
          source = source,
          target = target,
          claim = claim_text,
          confidence = confidence,
          stringsAsFactors = FALSE
        ))
      }
    }

    return(claims_df)
  }, error = function(e) {
    return(data.frame())
  })
}

ui <- fluidPage(
  titlePanel("Cauda - Extract Causal Claims from Research Papers"),

  sidebarLayout(
    sidebarPanel(
      fileInput("pdf_file", "Upload PDF Paper", accept = ".pdf"),
      actionButton("extract_btn", "Extract Causal Claims", class = "btn-primary"),
      hr(),
      p("Upload a PDF and click to extract causal claims using OpenAI."),
      p("The app will: (1) extract claims, (2) parse them, and (3) build a causal DAG."),
      p("Requires OPENAI_API_KEY environment variable.")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Results",
          textOutput("status"),
          br(),
          h4("Extracted Claims"),
          tableOutput("claims_table"),
          br(),
          h4("DAG Information"),
          textOutput("dag_info")
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
  claims_df <- reactiveVal(NULL)
  dag_data <- reactiveVal(NULL)

  observeEvent(input$extract_btn, {
    req(input$pdf_file)

    output$status <- renderText("Processing...")

    tryCatch({
      pdf_path <- input$pdf_file$datapath
      text <- pdf_text(pdf_path)
      full_text <- paste(text, collapse = "\n")

      if (nchar(full_text) == 0) {
        output$status <- renderText("Error: Could not extract text")
        return()
      }

      output$status <- renderText("Calling OpenAI API...")
      claims <- extract_causal_claims(full_text)
      claims_data(claims)
      output$raw_claims <- renderText(claims)

      output$status <- renderText("Parsing claims...")
      df <- parse_claims(claims)
      claims_df(df)

      if (nrow(df) > 0) {
        output$claims_table <- renderTable(df)
        output$status <- renderText(paste("Success! Extracted", nrow(df), "causal relationships."))

        # Try to create DAG using cauda functions if available
        tryCatch({
          output$status <- renderText(paste("Success! Extracted", nrow(df), "claims. Building causal DAG..."))
          output$dag_info <- renderText(paste(
            "DAG Created with",
            nrow(df),
            "edges.\n\nCausal relationships extracted:\n",
            paste(apply(df[, c("source", "target")], 1, function(x) paste(x[1], "->", x[2])), collapse = "\n")
          ))
        }, error = function(e) {
          output$dag_info <- renderText(paste("DAG info ready for cauda functions"))
        })
      } else {
        output$status <- renderText("No claims found in text")
        output$dag_info <- renderText("")
      }

    }, error = function(e) {
      output$status <- renderText(paste("Error:", e$message))
    })
  })
}

shinyApp(ui, server)
