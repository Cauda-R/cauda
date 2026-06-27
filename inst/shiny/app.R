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

      h4("Step 3: Filter Claims (Optional)"),
      textInput("claim_search", "Search by keyword:", ""),
      selectInput("pathway_filter", "Filter by pathway:",
                  c("All pathways", "physiological", "behavioral", "structural", "unknown")),
      selectInput("confidence_filter", "Filter by confidence:",
                  c("All confidence", "high", "medium", "low")),
      br(),

      h4("Step 4: Generate DAG"),
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
          h4("Extracted Claims (Summary)"),
          tableOutput("claims_table"),
          br()
        ),

        tabPanel("Detailed Claims",
          h4("Rich Claim Details"),
          p("Expanded view with study design, confounders, evidence quality, mechanism details, and limitations"),
          uiOutput("detailed_claims"),
          br()
        ),

        tabPanel("Critique",
          h4("Causal Strength & Evidence Critique"),
          p("Critical evaluation of each claim's causal validity and evidence gaps"),
          actionButton("critique_btn", "Run Critique Analysis", class = "btn-warning"),
          br(), br(),
          uiOutput("critique_output"),
          br()
        ),

        tabPanel("Synthesis",
          h4("Multi-Module Synthesis Report"),
          p("Comprehensive analysis combining summary, claims, critique, and mechanisms"),
          div(
            actionButton("synthesis_btn", "Generate Synthesis Report", class = "btn-info"),
            downloadButton("download_synthesis", "Download Report (.txt)", class = "btn-success"),
            style = "display: inline-block; margin: 0 5px;"
          ),
          br(), br(),
          uiOutput("synthesis_output"),
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
          h4("Raw GPT-4-turbo Response"),
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
  critique_df <- reactiveVal(NULL)
  synthesis_results <- reactiveVal(NULL)

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
      paper_name <- tools::file_path_sans_ext(input$pdf_file$name)

      # Call improved extraction function — withProgress shows a spinner overlay
      claims <- withProgress(
        message = sprintf("Extracting claims from \"%s\"…", paper_name),
        detail  = "Calling GPT-4-turbo (30–45 sec)",
        value   = 0.4, {
          result <- cauda::cauda.extract(
            full_text,
            model = "gpt-4-turbo",
            temperature = 0.3,
            max_tokens = 4000
          )
          setProgress(0.95, detail = "Parsing results…")
          result
        }
      )

      # Raw response stored as attribute — no second API call needed
      raw_resp <- attr(claims, "raw_response") %||% "Raw response not captured"
      raw_response(raw_resp)

      # Store the dataframe
      claims_df(claims)

      # Update status and display results
      n_claims <- nrow(claims)
      output$status <- renderText(
        sprintf("✓ %s — Extracted %d causal claims. Ready to generate DAG.", paper_name, n_claims)
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

      # Display detailed claims with professional HTML formatting
      output$detailed_claims <- renderUI({
        if (nrow(claims) == 0) {
          return(div(p("No claims extracted yet.", style = "color: #666; font-style: italic;")))
        }

        # Apply filters
        claims_filtered <- claims

        # Keyword search
        if (input$claim_search != "") {
          search_term <- tolower(input$claim_search)
          matches <- grepl(search_term, tolower(paste(claims_filtered$source, claims_filtered$target, claims_filtered$claim)), perl = TRUE)
          claims_filtered <- claims_filtered[matches, ]
        }

        # Pathway filter
        if (input$pathway_filter != "All pathways") {
          claims_filtered <- claims_filtered[claims_filtered$pathway == input$pathway_filter, ]
        }

        # Confidence filter
        if (input$confidence_filter != "All confidence") {
          claims_filtered <- claims_filtered[claims_filtered$confidence == input$confidence_filter, ]
        }

        if (nrow(claims_filtered) == 0) {
          return(div(p("No claims match your filters.", style = "color: #f39c12; font-style: italic;")))
        }

        # Sort claims by confidence (high → medium → low)
        claims_sorted <- claims_filtered
        claims_sorted$confidence <- factor(claims_sorted$confidence, levels = c("high", "medium", "low"))
        claims_sorted <- claims_sorted[order(claims_sorted$confidence), ]

        # Build HTML for each claim
        claim_html <- lapply(seq_len(nrow(claims_sorted)), function(i) {
          row <- claims_sorted[i, ]

          # Color code by confidence level
          conf <- as.character(row$confidence[1])
          quality_color <- switch(conf,
            "high" = "#27ae60",
            "medium" = "#f39c12",
            "low" = "#e74c3c",
            "#95a5a6"
          )

          # Build the claim card
          div(
            style = "margin-bottom: 20px; padding: 15px; border-left: 4px solid #3498db; background-color: #f8f9fa; border-radius: 4px;",
            h4(sprintf("Claim %d: %s → %s", i, row$source, row$target),
               style = "margin-top: 0; color: #2c3e50;"),

            p(strong("Claim: "), row$claim, style = "margin: 8px 0;"),

            div(style = "display: grid; grid-template-columns: 1fr 1fr; gap: 10px; margin: 10px 0;",
              p(strong("Type: "), row$claim_type, style = "margin: 0;"),
              p(strong("Confidence: "), span(row$confidence, style = paste0("background: ", quality_color, "; color: white; padding: 2px 6px; border-radius: 3px;")), style = "margin: 0;"),
              p(strong("Pathway: "), row$pathway, style = "margin: 0;"),
              p(strong("Established: "), if(isTRUE(row$established)) "Yes" else "No", style = "margin: 0;")
            ),

            div(style = "background: white; padding: 10px; border-radius: 3px; margin: 10px 0;",
              p(strong("Statistics:"), style = "margin-top: 0;"),
              p(sprintf("Effect Size: %s", row$effect_size), style = "margin: 4px 0; font-size: 0.95em;"),
              p(sprintf("P-Value: %s", row$p_value), style = "margin: 4px 0; font-size: 0.95em;"),
              p(sprintf("Sample Size: %s", row$sample_size), style = "margin: 4px 0; font-size: 0.95em;")
            ),

            if (!is.na(row$evidence) && row$evidence != "") {
              div(style = "margin: 10px 0; padding: 10px; background: #e8f4f8; border-radius: 3px;",
                p(strong("Evidence Details:"), style = "margin: 0 0 4px 0;"),
                p(row$evidence, style = "margin: 0; color: #333; font-size: 0.95em;")
              )
            },

            if (!is.na(row$notes) && row$notes != "") {
              div(style = "margin: 10px 0;",
                p(strong("Notes & Qualifications:"), style = "margin: 0 0 4px 0; color: #c0392b;"),
                p(row$notes, style = "margin: 0; color: #555; font-size: 0.95em;")
              )
            }
          )
        })

        div(
          h3("Detailed Causal Claims Analysis", style = "color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px;"),
          do.call(div, claim_html)
        )
      })

      # Display raw response for debugging
      output$raw_claims <- renderText(raw_resp)

    }, error = function(e) {
      output$status <- renderText(paste("❌ Error:", e$message))
    })
  })

  # STEP 1.5: Run critique analysis
  observeEvent(input$critique_btn, {
    req(claims_df())

    if (nrow(claims_df()) == 0) {
      output$critique_output <- renderUI(
        p("❌ No claims available. Extract claims first.", style = "color: #e74c3c;")
      )
      return()
    }

    output$critique_output <- renderUI(
      p("⏳ Running critique analysis... this may take 1-2 minutes", style = "color: #f39c12;")
    )

    tryCatch({
      # Run critique on all claims
      critiqued <- withProgress(
        message = "Running critique analysis…",
        detail  = sprintf("Evaluating %d claims with GPT-4-turbo", nrow(claims_df())),
        value   = 0.5, {
          cauda::cauda.critique(claims_df(), verbose = TRUE)
        }
      )
      critique_df(critiqued)

      # Render critique output
      output$critique_output <- renderUI({
        render_critique_results(critiqued)
      })

    }, error = function(e) {
      output$critique_output <- renderUI(
        p(paste("❌ Error running critique:", e$message), style = "color: #e74c3c;")
      )
    })
  })

  # STEP 1.75: Generate synthesis report
  observeEvent(input$synthesis_btn, {
    req(claims_df())
    req(critique_df())

    if (nrow(claims_df()) == 0 || is.null(critique_df())) {
      output$synthesis_output <- renderUI(
        p("❌ Run Extract Claims and Critique Analysis first.", style = "color: #e74c3c;")
      )
      return()
    }

    output$synthesis_output <- renderUI(
      p("⏳ Generating synthesis report...", style = "color: #f39c12;")
    )

    tryCatch({
      # Generate synthesis
      synthesis <- withProgress(
        message = "Generating synthesis report…",
        detail  = "Calling GPT-4o-mini",
        value   = 0.5, {
          cauda::cauda.synthesize(
            pdf_text(),
            claims_df(),
            critique_df(),
            verbose = TRUE
          )
        }
      )
      synthesis_results(synthesis)

      # Render synthesis
      output$synthesis_output <- renderUI({
        render_synthesis_report(synthesis, claims_df(), critique_df())
      })

    }, error = function(e) {
      output$synthesis_output <- renderUI(
        p(paste("❌ Error generating synthesis:", e$message), style = "color: #e74c3c;")
      )
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
  output$raw_claims <- renderText("Raw GPT-4-turbo response will appear here")
  output$parsed_claims_output <- renderPrint({
    cat("Parsed claims dataframe will appear here\n")
  })
  output$detailed_claims <- renderUI({
    div(p("Detailed claims analysis will appear here after extraction.", style = "color: #666; font-style: italic;"))
  })
  output$critique_output <- renderUI({
    div(p("Click 'Run Critique Analysis' to evaluate claims.", style = "color: #666; font-style: italic;"))
  })
  output$synthesis_output <- renderUI({
    div(p("Click 'Generate Synthesis Report' after running critique.", style = "color: #666; font-style: italic;"))
  })

  # Download synthesis report
  output$download_synthesis <- downloadHandler(
    filename = function() {
      paste0("synthesis_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
    },
    content = function(file) {
      req(synthesis_results())

      synthesis <- synthesis_results()
      report_text <- paste0(
        "CAUDA: MULTI-MODULE SYNTHESIS REPORT\n",
        "====================================\n",
        "Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n",

        "PAPER SUMMARY\n",
        "=============\n",
        synthesis$summary, "\n\n",

        "KEY STRENGTHS\n",
        "=============\n",
        synthesis$key_strengths, "\n\n",

        "KEY LIMITATIONS\n",
        "===============\n",
        synthesis$key_limitations, "\n\n",

        "CONFOUNDERS & ALTERNATIVES\n",
        "===========================\n",
        paste(synthesis$confounder_summary$confounder, "(",
              synthesis$confounder_summary$frequency, "claims)", collapse = "\n"), "\n\n",

        "BOTTOM-LINE APPRAISAL\n",
        "====================\n",
        synthesis$bottom_line, "\n\n",

        "END OF REPORT\n"
      )

      writeLines(report_text, file)
    }
  )
}

# Helper: convert a dataframe to an HTML table (safe to use inside renderUI)
df_to_html_table <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(p("No data available.", style = "color: #666;"))
  header <- tags$tr(lapply(colnames(df), function(col) {
    tags$th(col, style = "padding: 6px 12px; background: #ecf0f1; font-weight: bold; border: 1px solid #ddd; text-align: left;")
  }))
  body_rows <- lapply(seq_len(nrow(df)), function(i) {
    bg <- if (i %% 2 == 0) "#f9f9f9" else "white"
    tags$tr(lapply(seq_len(ncol(df)), function(j) {
      tags$td(as.character(df[i, j]), style = paste0("padding: 6px 12px; border: 1px solid #ddd; background: ", bg, ";"))
    }))
  })
  tags$table(style = "border-collapse: collapse; width: 100%; font-size: 0.9em;",
    tags$thead(header),
    tags$tbody(body_rows)
  )
}

# Helper function to render synthesis report
render_synthesis_report <- function(synthesis, claims, critique) {
  div(
    # RED FLAG: Confidence vs Support Mismatches
    if (isTRUE(synthesis$mismatches$has_mismatches)) {
      div(
        style = "margin-bottom: 30px; padding: 20px; background-color: #ffe8e8; border-left: 5px solid #c0392b; border-radius: 4px;",
        h3("Confidence vs Support Mismatches", style = "margin-top: 0; color: #c0392b;"),
        p(sprintf("%d claims rated HIGH confidence but critique found WEAK or QUESTIONABLE support:",
                  synthesis$mismatches$count),
          style = "font-weight: bold; margin: 10px 0;"),
        df_to_html_table(
          synthesis$mismatches$flag_claims[, c("claim_id", "source", "target", "original_confidence", "actual_support")]
        ),
        p("These claims need closer scrutiny before relying on them.",
          style = "margin-top: 10px; font-style: italic; color: #555;")
      )
    },

    # Support Matrix
    div(
      style = "margin-bottom: 30px; padding: 20px; background-color: #f0f8ff; border-left: 4px solid #2196F3; border-radius: 4px;",
      h3("Support Matrix: Claims by Evidence Level", style = "margin-top: 0; color: #2c3e50;"),
      {
        matrix_data <- synthesis$cross_module_consistency$support_matrix
        display_matrix <- data.frame(
          ID = matrix_data$claim_id,
          Source = substr(matrix_data$source, 1, 20),
          Target = substr(matrix_data$target, 1, 20),
          Support = matrix_data$support,
          stringsAsFactors = FALSE
        )
        df_to_html_table(display_matrix)
      }
    ),

    # Summary
    div(
      style = "margin-bottom: 30px; padding: 20px; background-color: #e8f4f8; border-left: 4px solid #3498db; border-radius: 4px;",
      h3("Paper Summary", style = "margin-top: 0; color: #2c3e50;"),
      p(synthesis$summary, style = "font-size: 0.95em; line-height: 1.6;")
    ),

    # Key Strengths
    div(
      style = "margin-bottom: 30px; padding: 20px; background-color: #e8f8e8; border-left: 4px solid #27ae60; border-radius: 4px;",
      h3("Key Strengths & Better-Supported Conclusions", style = "margin-top: 0; color: #2c3e50;"),
      p(synthesis$key_strengths, style = "font-size: 0.95em; line-height: 1.6; white-space: pre-wrap;")
    ),

    # Key Limitations
    div(
      style = "margin-bottom: 30px; padding: 20px; background-color: #fff8e8; border-left: 4px solid #f39c12; border-radius: 4px;",
      h3("Key Limitations, Gaps, and Caveats", style = "margin-top: 0; color: #2c3e50;"),
      p(synthesis$key_limitations, style = "font-size: 0.95em; line-height: 1.6; white-space: pre-wrap;")
    ),

    # Claims Appraisal Table
    div(
      style = "margin-bottom: 30px;",
      h3("Claims Appraisal Table", style = "color: #2c3e50;"),
      {
        appraisal <- synthesis$claims_appraisal
        display_appraisal <- data.frame(
          Claim = seq_len(nrow(appraisal)),
          Source = substr(appraisal$source, 1, 15),
          Target = substr(appraisal$target, 1, 15),
          Strength = appraisal$causal_strength,
          Support = appraisal$support_category,
          Conf_Orig = appraisal$confidence_original,
          Conf_Adj = appraisal$confidence_adjusted,
          stringsAsFactors = FALSE
        )
        df_to_html_table(display_appraisal)
      }
    ),

    # Bottom-Line Appraisal
    div(
      style = "margin-bottom: 30px; padding: 20px; background-color: #fef5f5; border-left: 4px solid #e74c3c; border-radius: 4px;",
      h3("Bottom-Line Appraisal", style = "margin-top: 0; color: #2c3e50;"),
      p(synthesis$bottom_line, style = "font-size: 0.95em; line-height: 1.6; white-space: pre-wrap;")
    )
  )
}

# Helper function to render critique results
render_critique_results <- function(critique_df) {
  if (nrow(critique_df) == 0) {
    return(div(p("No critique results available.")))
  }

  critique_html <- lapply(seq_len(nrow(critique_df)), function(i) {
    row <- critique_df[i, ]

    # Color code causal strength
    strength_color <- switch(row$causal_strength,
      "strong" = "#27ae60",
      "moderate" = "#f39c12",
      "weak" = "#e74c3c",
      "#95a5a6"
    )

    # Color code support
    support_color <- switch(row$support_summary,
      "well_supported" = "#27ae60",
      "partly_supported" = "#f39c12",
      "questionable" = "#e74c3c",
      "#95a5a6"
    )

    div(
      style = "margin-bottom: 20px; padding: 15px; border-left: 4px solid #e74c3c; background-color: #fef5f5; border-radius: 4px;",
      h4(sprintf("Claim %d: %s → %s", i, row$source, row$target),
         style = "margin-top: 0; color: #2c3e50;"),

      div(style = "display: grid; grid-template-columns: 1fr 1fr; gap: 10px; margin: 10px 0;",
        p(strong("Causal Strength: "), span(row$causal_strength, style = paste0("background: ", strength_color, "; color: white; padding: 2px 6px; border-radius: 3px;")), style = "margin: 0;"),
        p(strong("Support: "), span(row$support_summary, style = paste0("background: ", support_color, "; color: white; padding: 2px 6px; border-radius: 3px;")), style = "margin: 0;"),
        p(strong("Adjusted Confidence: "), row$confidence_adjusted, style = "margin: 0;"),
        p(strong("Original Confidence: "), row$confidence, style = "margin: 0;")
      ),

      if (!is.na(row$critique) && row$critique != "") {
        div(style = "background: white; padding: 10px; border-radius: 3px; margin: 10px 0;",
          p(strong("Critique:"), style = "margin-top: 0;"),
          p(row$critique, style = "margin: 4px 0; font-size: 0.95em; color: #333;")
        )
      },

      if (!is.na(row$key_gaps) && row$key_gaps != "") {
        div(style = "margin: 10px 0; padding: 10px; background: #fff3cd; border-radius: 3px;",
          p(strong("Critical Evidence Gaps:"), style = "margin: 0 0 4px 0; color: #c0392b;"),
          p(row$key_gaps, style = "margin: 0; color: #555; font-size: 0.95em;")
        )
      }
    )
  })

  div(
    h3("Critique & Evidence Assessment", style = "color: #2c3e50; border-bottom: 2px solid #e74c3c; padding-bottom: 10px;"),
    do.call(div, critique_html)
  )
}

# Run the app
shinyApp(ui, server)
