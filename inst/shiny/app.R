library(shiny)
# cauda and pdftools loaded in global.R (handles shinyapps.io install too)

ui <- fluidPage(
  titlePanel("Cauda — Causal Claim Extraction & DAG Comparison"),

  tags$style(HTML("
    .claim-card  { margin-bottom:18px; padding:14px; border-left:4px solid #3498db;
                   background:#f8f9fa; border-radius:4px; }
    .badge       { display:inline-block; padding:2px 8px; border-radius:3px;
                   color:white; font-size:0.82em; font-weight:600; }
    .badge-high  { background:#27ae60; }
    .badge-med   { background:#f39c12; }
    .badge-low   { background:#e74c3c; }
    .badge-A     { background:#95a5a6; }
    .badge-B     { background:#3498db; }
    .badge-C     { background:#9b59b6; }
    .badge-D     { background:#e67e22; }
    .badge-causal{ background:#1abc9c; }
    .badge-policy{ background:#e74c3c; }
    .quote-block { border-left:3px solid #bdc3c7; padding:6px 12px; margin:8px 0;
                   font-style:italic; color:#555; background:#fff; border-radius:2px; }
    .paper-tag   { display:inline-block; padding:1px 7px; border-radius:10px;
                   background:#2c3e50; color:white; font-size:0.78em; margin-right:5px; }
    .edge-row    { padding:6px 10px; border-bottom:1px solid #eee; }
    .edge-row:hover { background:#f0f0f0; }

    .ci-list     { border:1px solid #e6e6e6; border-radius:6px; overflow:hidden; margin-bottom:10px; }
    .ci-row      { display:flex; align-items:center; flex-wrap:wrap; gap:6px;
                   padding:6px 12px; border-bottom:1px solid #eee; font-size:0.88em; }
    .ci-row:last-child { border-bottom:none; }
    .ci-row:nth-child(even) { background:#fafbfb; }
    .ci-var      { display:inline-block; padding:1px 7px; border-radius:3px;
                   background:#eaf2f8; border:1px solid #d6e8f5; color:#1a5276;
                   font-family:'SFMono-Regular',Consolas,monospace; font-weight:600;
                   font-size:0.9em; }
    .ci-symbol   { color:#aaa; font-weight:700; }
    .ci-cond-label { color:#999; font-size:0.85em; margin-left:2px; }
    .ci-cond-chip { display:inline-block; padding:1px 6px; margin:0 1px; border-radius:8px;
                   background:#fdf2e9; border:1px solid #f5e0c6; color:#935116;
                   font-size:0.8em; }
    .ci-cond-none { color:#bbb; font-size:0.82em; font-style:italic; }
    .show-more-toggle { cursor:pointer; color:#3498db; font-size:0.85em; padding:8px 12px;
                   border-top:1px solid #eee; background:#fafbfb; }
    .dagitty-toggle { cursor:pointer; color:#3498db; font-size:0.85em; margin:6px 0 14px; }
    .dagitty-box { background:#2c3e50; color:#ecf0f1; padding:12px 16px; border-radius:4px;
                   font-family:'SFMono-Regular',Consolas,monospace; font-size:0.82em;
                   white-space:pre-wrap; margin-bottom:14px; }
    .theory-card { margin-bottom:14px; padding:10px 14px; background:#f8f9fa;
                   border-left:4px solid #8e44ad; border-radius:4px; }
    .theory-badge{ display:inline-block; padding:2px 9px; border-radius:10px;
                   color:white; font-size:0.8em; font-weight:600; margin-right:8px; }
    .empty-hint  { color:#888; font-style:italic; padding:10px 0; }
    .test-row    { display:flex; align-items:center; flex-wrap:wrap; gap:8px;
                   padding:7px 12px; border-bottom:1px solid #eee; font-size:0.87em;
                   border-left:4px solid #ccc; }
    .test-row:last-child { border-bottom:none; }
    .test-row-violated  { border-left-color:#c0392b; background:#fdf2f1; }
    .test-row-consistent{ border-left-color:#27ae60; background:#f2faf5; }
    .test-stat   { color:#888; font-size:0.85em; margin-left:auto; white-space:nowrap; }
    .test-verdict{ display:inline-block; padding:1px 8px; border-radius:10px; color:white;
                   font-size:0.78em; font-weight:600; }
    .verdict-violated  { background:#c0392b; }
    .verdict-consistent{ background:#27ae60; }
    .untestable-row { padding:5px 12px; border-bottom:1px solid #eee; font-size:0.85em; color:#888; }
    .untestable-row:last-child { border-bottom:none; }
    .summary-table { width:100%; border-collapse:collapse; margin-bottom:16px; font-size:0.9em; }
    .summary-table th, .summary-table td { padding:7px 10px; border-bottom:1px solid #eee; text-align:left; }
    .summary-table th { color:#888; font-weight:600; font-size:0.82em; text-transform:uppercase; }
    .varmap-row  { display:flex; align-items:center; gap:10px; padding:4px 0; }
    .varmap-label{ flex:1; font-size:0.87em; }
  ")),

  sidebarLayout(
    sidebarPanel(width = 3,
      h4("Step 1: Upload Papers"),
      fileInput("pdf_files", "Select PDF(s)", accept = ".pdf", multiple = TRUE),
      uiOutput("paper_list_ui"),

      hr(),
      h4("Step 2: Extract Claims"),
      actionButton("extract_btn", "Extract from All Papers",
                   class = "btn-primary btn-block"),
      br(),

      h4("Step 3: Filter"),
      textInput("claim_search", "Keyword search:", ""),
      uiOutput("paper_filter_ui"),
      selectInput("class_filter", "Claim class:",
                  c("All classes","A","B","C","D")),
      selectInput("confidence_filter", "Confidence:",
                  c("All confidence","high","medium","low")),
      selectInput("pathway_filter", "Pathway:",
                  c("All pathways","physiological","behavioral",
                    "structural","gateway","common_liability","unknown")),
      checkboxInput("causal_only", "Causal triggers only", FALSE),
      checkboxInput("policy_only", "Policy triggers only",  FALSE),

      hr(),
      h4("Step 4: Generate DAG"),
      actionButton("dag_btn", "Build DAG from Claims",
                   class = "btn-success btn-block"),

      hr(),
      p(em("Powered by GPT-4o + Cauda"),
        style = "color:#888; font-size:11px;")
    ),

    mainPanel(width = 9,
      tabsetPanel(
        # ── STATUS ──────────────────────────────────────────────────────────
        tabPanel("Status & Summary",
          br(),
          uiOutput("status_ui"),
          br(),
          h4("Claims Summary Table"),
          tableOutput("claims_table")
        ),

        # ── DETAILED CLAIMS ─────────────────────────────────────────────────
        tabPanel("Detailed Claims",
          br(),
          uiOutput("detailed_claims")
        ),

        # ── CRITIQUE ────────────────────────────────────────────────────────
        tabPanel("Critique",
          br(),
          actionButton("critique_btn", "Run Critique", class = "btn-warning"),
          br(), br(),
          uiOutput("critique_output")
        ),

        # ── SYNTHESIS ───────────────────────────────────────────────────────
        tabPanel("Synthesis",
          br(),
          actionButton("synthesis_btn", "Generate Synthesis", class = "btn-info"),
          downloadButton("download_synthesis", "Download (.txt)", class = "btn-success"),
          br(), br(),
          uiOutput("synthesis_output")
        ),

        # ── CAUSAL DAG ──────────────────────────────────────────────────────
        tabPanel("Causal DAG",
          br(),
          fluidRow(
            column(8,
              plotOutput("dag_plot", height = "680px")
            ),
            column(4,
              h5("Edit DAG Edges"),
              wellPanel(
                h6("Add Edge"),
                textInput("edge_from", "From node:", ""),
                textInput("edge_to",   "To node:",   ""),
                actionButton("add_edge_btn", "Add Edge", class = "btn-sm btn-primary"),
                hr(),
                h6("Current Edges"),
                p("Click an edge to select it, then use buttons below. ",
                  span("●", style="color:#2ecc71;"), " = backed by a paper, ",
                  span("●", style="color:#ccc;"), " = no source.",
                  style="font-size:11px; color:#888;"),
                div(style="max-height:250px; overflow-y:auto;",
                  uiOutput("edge_list_ui")
                ),
                br(),
                h6("Selected Edge Source"),
                uiOutput("edge_source_ui"),
                br(),
                actionButton("reverse_edge_btn", "Reverse Selected", class = "btn-sm btn-warning"),
                actionButton("delete_edge_btn",  "Delete Selected",  class = "btn-sm btn-danger"),
                hr(),
                actionButton("replot_btn", "Replot DAG", class = "btn-sm btn-success")
              )
            )
          ),
          br(),
          verbatimTextOutput("dag_summary")
        ),

        # ── TESTABLE IMPLICATIONS (DAGitty) ────────────────────────────────
        tabPanel("Testable Implications",
          br(),
          p("Derives the conditional independencies your current DAG implies — the ",
            "predictions that MUST hold in real data if this causal theory is correct. ",
            "These are the concrete things to go test. Sorted simplest-to-test first ",
            "(fewest variables to control for).", style = "font-size:12px; color:#888;"),
          actionButton("dagitty_btn", "Derive Testable Implications", class = "btn-sm btn-primary"),
          downloadButton("download_dagitty_string", "Download dagitty model (.txt)", class = "btn-sm btn-success"),
          br(), br(),
          uiOutput("dagitty_implications")
        ),

        # ── COMPARE THEORIES ────────────────────────────────────────────────
        tabPanel("Compare Theories",
          br(),
          p("Builds one candidate DAG per uploaded paper (paper = theory) and flags which ",
            "testable implications are unique to a single paper's theory — the most useful ",
            "place to start testing against data, since a shared implication can't tell the ",
            "theories apart.", style = "font-size:12px; color:#888;"),
          actionButton("compare_theories_btn", "Compare Papers as Competing Theories", class = "btn-sm btn-warning"),
          br(), br(),
          uiOutput("discriminating_table")
        ),

        # ── TEST AGAINST REAL DATA (Cox workflow step 5) ─────────────────────
        tabPanel("Test Against Real Data",
          br(),
          p("Checks each theory's testable implications against real data. ",
            "A DAG's node labels are abstract claim text ('Purdue marketing', ",
            "'psychosocial despair') that don't literally match a data column, so ",
            "map each node to the real variable that measures it below. Implications ",
            "where every variable involved has a mapped column get tested; the rest ",
            "are reported as untestable, with the reason, rather than silently skipped.",
            style = "font-size:12px; color:#888;"),
          p(strong("Run 'Compare Papers as Competing Theories' first"),
            " — this tab tests the same per-paper DAGs built there.",
            style = "font-size:12px; color:#c0392b;"),
          radioButtons("real_data_source", "Data source:",
                       choices = c("Bundled opioid dataset (CDC + DEA ARCOS)" = "bundled",
                                   "Upload your own CSV" = "upload"),
                       selected = "bundled", inline = TRUE),
          conditionalPanel(
            condition = "input.real_data_source == 'bundled'",
            p("Real CDC overdose-death counts and DEA pill-distribution volumes ",
              "by state/year — only useful for opioid-crisis papers.",
              style = "font-size:12px; color:#888;"),
            actionButton("load_real_data_btn", "Load Real Opioid Dataset",
                         class = "btn-sm btn-info")
          ),
          conditionalPanel(
            condition = "input.real_data_source == 'upload'",
            p("Bring your own real dataset for any topic. First row must be a header; ",
              "each column becomes a variable you can map to a DAG node below.",
              style = "font-size:12px; color:#888;"),
            fileInput("real_data_upload", NULL, accept = c(".csv"), buttonLabel = "Browse...",
                      placeholder = "No CSV selected")
          ),
          br(),
          uiOutput("real_data_status_ui"),
          br(),
          uiOutput("var_map_ui"),
          br(),
          actionButton("run_empirical_test_btn", "Test Implications Against Real Data",
                       class = "btn-sm btn-danger"),
          br(), br(),
          uiOutput("empirical_test_results_ui")
        ),

        # ── DEBUG ───────────────────────────────────────────────────────────
        tabPanel("Debug",
          br(),
          h5("Raw GPT JSON Response"),
          verbatimTextOutput("raw_claims"),
          br(),
          h5("Claims Dataframe"),
          verbatimTextOutput("parsed_claims_output")
        )
      )
    )
  )
)


# ══════════════════════════════════════════════════════════════════════════════
server <- function(input, output, session) {

  # ── Reactive state ─────────────────────────────────────────────────────────
  all_claims     <- reactiveVal(NULL)   # combined multi-paper claims df
  dag_obj        <- reactiveVal(NULL)   # bnlearn DAG
  custom_edges   <- reactiveVal(NULL)   # data.frame(from, to) of user-edited edges
  selected_edge  <- reactiveVal(NULL)   # index into custom_edges
  raw_response   <- reactiveVal(NULL)
  critique_df    <- reactiveVal(NULL)
  synthesis_res  <- reactiveVal(NULL)
  paper_texts    <- reactiveVal(list()) # named list: paper_id -> full text

  # ── Paper list UI ──────────────────────────────────────────────────────────
  output$paper_list_ui <- renderUI({
    req(input$pdf_files)
    names_list <- input$pdf_files$name
    tagList(
      p(strong(sprintf("%d paper(s) queued:", length(names_list))),
        style = "margin:6px 0 2px;"),
      tags$ul(
        lapply(names_list, function(n)
          tags$li(n, style = "font-size:12px; color:#555;")
        )
      )
    )
  })

  # ── Paper filter UI (populated after extraction) ───────────────────────────
  output$paper_filter_ui <- renderUI({
    df <- all_claims()
    if (is.null(df) || !"paper_id" %in% names(df)) return(NULL)
    ids <- c("All papers", unique(na.omit(df$paper_id)))
    selectInput("paper_filter", "Filter by paper:", ids)
  })

  # ── EXTRACT ────────────────────────────────────────────────────────────────
  observeEvent(input$extract_btn, {
    req(input$pdf_files)
    files <- input$pdf_files

    output$status_ui <- renderUI(
      p("⏳ Extracting from ", nrow(files), " paper(s)…", style="color:#f39c12;")
    )

    tryCatch({
      texts <- list()
      for (i in seq_len(nrow(files))) {
        pid   <- tools::file_path_sans_ext(files$name[i])
        pages <- pdftools::pdf_text(files$datapath[i])
        texts[[pid]] <- paste(pages, collapse = "\n")
      }
      paper_texts(texts)

      combined <- withProgress(
        message = sprintf("Extracting from %d paper(s)…", length(texts)),
        detail  = "Calling GPT-4o…",
        value   = 0.3, {
          cauda::cauda.extract_multi(texts, is_pdf = FALSE,
                                     model = "gpt-4o",
                                     temperature = 0.3, max_tokens = 4000)
        }
      )

      all_claims(combined)
      custom_edges(NULL)
      dag_obj(NULL)
      raw_response(attr(combined, "raw_response") %||% "")

      if (nrow(combined) == 0) {
        # Every paper failed extraction (cauda.extract_multi() catches
        # per-paper errors and only logs them server-side via message()),
        # so this used to render a cheerful green "Extracted 0 claims"
        # success message that hid a real failure (e.g. OpenAI API key
        # missing/invalid, or account out of credits). Show it as an error.
        output$status_ui <- renderUI(
          p("❌ Extraction failed for every paper (0 claims). This usually means the ",
            "OpenAI API key is missing/invalid or the account is out of credits — ",
            "check the Debug tab or server logs for the underlying error.",
            style = "color:#e74c3c; font-weight:bold;")
        )
      } else {
        output$status_ui <- renderUI(
          p(sprintf("✓ Extracted %d claims from %d paper(s). Ready to build DAG.",
                    nrow(combined), length(texts)),
            style = "color:#27ae60; font-weight:bold;")
        )
      }

    }, error = function(e) {
      output$status_ui <- renderUI(
        p(paste("❌ Error:", e$message), style="color:#e74c3c;")
      )
    })
  })

  # ── Filtered claims (reactive) ────────────────────────────────────────────
  filtered_claims <- reactive({
    df <- all_claims()
    if (is.null(df) || nrow(df) == 0) return(df)

    # Paper filter
    if (!is.null(input$paper_filter) && input$paper_filter != "All papers")
      df <- df[df$paper_id == input$paper_filter, ]

    # Keyword
    if (!is.null(input$claim_search) && nchar(trimws(input$claim_search)) > 0) {
      kw <- tolower(trimws(input$claim_search))
      df <- df[grepl(kw, tolower(paste(df$source, df$target, df$claim)), perl=TRUE), ]
    }

    # Claim class
    if (!is.null(input$class_filter) && input$class_filter != "All classes")
      df <- df[df$claim_class == input$class_filter, ]

    # Confidence
    if (!is.null(input$confidence_filter) && input$confidence_filter != "All confidence")
      df <- df[df$confidence == input$confidence_filter, ]

    # Pathway
    if (!is.null(input$pathway_filter) && input$pathway_filter != "All pathways")
      df <- df[df$pathway == input$pathway_filter, ]

    # Triggers
    if (isTRUE(input$causal_only)) df <- df[df$causal_trigger == TRUE, ]
    if (isTRUE(input$policy_only))  df <- df[df$policy_trigger == TRUE,  ]

    df
  })

  # ── Summary table ─────────────────────────────────────────────────────────
  output$claims_table <- renderTable({
    df <- filtered_claims()
    if (is.null(df) || nrow(df) == 0)
      return(data.frame(Message = "No claims yet. Upload PDFs and click Extract."))

    show <- data.frame(
      Paper      = ifelse(is.na(df$paper_id), "", substr(df$paper_id, 1, 15)),
      Source     = substr(df$source, 1, 20),
      Target     = substr(df$target, 1, 20),
      Class      = df$claim_class,
      Confidence = df$confidence,
      `Effect Size` = df$effect_size,
      `P-Value`     = df$p_value,
      Pathway    = df$pathway,
      check.names = FALSE, stringsAsFactors = FALSE
    )
    show
  }, striped = TRUE, hover = TRUE, width = "100%")

  # ── Detailed claims ────────────────────────────────────────────────────────
  output$detailed_claims <- renderUI({
    df <- filtered_claims()
    if (is.null(df) || nrow(df) == 0)
      return(div(p("No claims to display.", style="color:#666; font-style:italic;")))

    # Sort: high → medium → low
    df$confidence <- factor(df$confidence, levels = c("high","medium","low"))
    df <- df[order(df$confidence), ]

    cards <- lapply(seq_len(nrow(df)), function(i) {
      r <- df[i, ]

      conf_cls <- switch(as.character(r$confidence),
        high="badge-high", medium="badge-med", low="badge-low", "badge-low")
      cls_cls <- paste0("badge-", r$claim_class)

      div(class = "claim-card",
        # Header row
        div(style="display:flex; align-items:center; flex-wrap:wrap; gap:6px; margin-bottom:8px;",
          if (!is.na(r$paper_id))  span(r$paper_id, class="paper-tag"),
          span(paste("Class", r$claim_class), class=paste("badge", cls_cls)),
          span(r$confidence,  class=paste("badge", conf_cls)),
          if (isTRUE(r$causal_trigger)) span("causal trigger", class="badge badge-causal"),
          if (isTRUE(r$policy_trigger))  span("policy trigger",  class="badge badge-policy")
        ),

        h5(paste0(r$source, " → ", r$target), style="margin:0 0 6px;"),

        # Verbatim quote
        if (!is.na(r$verbatim_quote) && r$verbatim_quote != "")
          div(class="quote-block", paste0('"', r$verbatim_quote, '"')),

        p(strong("Claim: "), r$claim, style="margin:6px 0;"),

        div(style="display:grid; grid-template-columns:1fr 1fr 1fr; gap:6px; font-size:0.9em;",
          p(strong("Type: "),       r$claim_type,  style="margin:0;"),
          p(strong("Scope: "),      r$claim_scope, style="margin:0;"),
          p(strong("Pathway: "),    r$pathway,     style="margin:0;"),
          p(strong("Established: "), if(isTRUE(r$established)) "Yes" else "No", style="margin:0;"),
          p(strong("Effect: "),  r$effect_size,  style="margin:0;"),
          p(strong("P-value: "), r$p_value,      style="margin:0;")
        ),

        if (!is.na(r$evidence) && r$evidence != "")
          div(style="margin-top:8px; padding:8px; background:#e8f4f8; border-radius:3px; font-size:0.9em;",
            strong("Evidence: "), r$evidence),

        if (!is.na(r$notes) && r$notes != "")
          div(style="margin-top:6px; font-size:0.88em; color:#777;",
            strong("Notes: "), r$notes)
      )
    })

    div(
      h4(sprintf("%d Claims", nrow(df)),
         style="border-bottom:2px solid #3498db; padding-bottom:8px;"),
      do.call(div, cards)
    )
  })

  # ── Critique ───────────────────────────────────────────────────────────────
  observeEvent(input$critique_btn, {
    req(all_claims())
    df <- all_claims()
    if (nrow(df) == 0) {
      output$critique_output <- renderUI(p("No claims to critique.", style="color:#e74c3c;"))
      return()
    }
    tryCatch({
      crit <- withProgress(
        message = "Running critique…",
        detail  = sprintf("Evaluating %d claims", nrow(df)),
        value   = 0.5,
        cauda::cauda.critique(df, verbose = TRUE)
      )
      critique_df(crit)
      output$critique_output <- renderUI(render_critique_results(crit))
    }, error = function(e) {
      output$critique_output <- renderUI(p(paste("❌", e$message), style="color:#e74c3c;"))
    })
  })

  # ── Synthesis ──────────────────────────────────────────────────────────────
  observeEvent(input$synthesis_btn, {
    req(all_claims(), critique_df())
    tryCatch({
      syn <- withProgress(
        message = "Generating synthesis…", detail = "Calling GPT-4o-mini", value = 0.5,
        cauda::cauda.synthesize(
          paste(unlist(paper_texts()), collapse="\n\n---\n\n"),
          all_claims(), critique_df(), verbose = TRUE
        )
      )
      synthesis_res(syn)
      output$synthesis_output <- renderUI(render_synthesis_report(syn, all_claims(), critique_df()))
    }, error = function(e) {
      output$synthesis_output <- renderUI(p(paste("❌", e$message), style="color:#e74c3c;"))
    })
  })

  # ── Build DAG ─────────────────────────────────────────────────────────────
  observeEvent(input$dag_btn, {
    req(all_claims())
    df <- all_claims()
    if (nrow(df) == 0) { output$status_ui <- renderUI(p("No claims.")); return() }

    tryCatch({
      dag <- cauda::cauda.claims_to_dag(df, confidence_threshold="low",
                                        include_speculative=TRUE, verbose=TRUE)
      dag_obj(dag)

      # Seed custom_edges from the DAG's edge_metadata (display names + pathway,
      # not the sanitized bnlearn node names arcs() would return). Also carry
      # through paper_id/verbatim_quote/claim_id so the edge list can show
      # provenance — this is the source-traceability requirement from Cox's
      # workflow ("preserve the source of every proposed edge so users can
      # trace it back to the paper and ideally the exact supporting text").
      edge_meta <- attr(dag, "edge_metadata")
      if (!is.null(edge_meta) && nrow(edge_meta) > 0) {
        custom_edges(data.frame(
          from           = edge_meta$from_display,
          to             = edge_meta$to_display,
          pathway        = edge_meta$pathway,
          paper_id       = if ("paper_id" %in% names(edge_meta)) edge_meta$paper_id else NA_character_,
          verbatim_quote = if ("verbatim_quote" %in% names(edge_meta)) edge_meta$verbatim_quote else NA_character_,
          claim_id       = if ("claim_id" %in% names(edge_meta)) edge_meta$claim_id else NA_character_,
          stringsAsFactors = FALSE
        ))
      } else {
        custom_edges(data.frame(from=character(), to=character(), pathway=character(),
                                 paper_id=character(), verbatim_quote=character(), claim_id=character(),
                                 stringsAsFactors=FALSE))
      }

      replot_dag()

    }, error = function(e) {
      output$status_ui <- renderUI(p(paste("❌ DAG error:", e$message), style="color:#e74c3c;"))
    })
  })

  # ── DAG edge list UI ──────────────────────────────────────────────────────
  # Edges that came from an extracted claim carry a paper_id; a small dot
  # marks those as sourced vs. hand-added/edited (no paper_id) so it's visible
  # at a glance which edges in the DAG are backed by a paper and which aren't.
  output$edge_list_ui <- renderUI({
    edges <- custom_edges()
    if (is.null(edges) || nrow(edges) == 0) return(p("No edges.", style="color:#888; font-size:12px;"))
    sel <- selected_edge()
    has_source <- "paper_id" %in% names(edges)

    rows <- lapply(seq_len(nrow(edges)), function(i) {
      bg <- if (!is.null(sel) && sel == i) "#d5e8d4" else "transparent"
      sourced <- has_source && !is.na(edges$paper_id[i]) && edges$paper_id[i] != ""
      dot <- span(style=paste0("display:inline-block; width:7px; height:7px; border-radius:50%; margin-right:5px; background:",
                                if (sourced) "#2ecc71" else "#ccc", ";"), title = if (sourced) "Sourced from a paper" else "No source")
      div(class="edge-row", style=paste0("cursor:pointer; background:", bg, ";"),
        onclick = sprintf("Shiny.setInputValue('selected_edge_click', %d, {priority: 'event'})", i),
        dot,
        span(edges$from[i], style="font-weight:600;"),
        " → ",
        span(edges$to[i])
      )
    })
    do.call(div, rows)
  })

  observeEvent(input$selected_edge_click, {
    selected_edge(input$selected_edge_click)
  })

  # ── Selected-edge source panel ─────────────────────────────────────────────
  # Shows exactly which paper + verbatim quote produced the selected edge, or
  # says plainly that it has none (added by hand, or reversed and therefore no
  # longer verbatim-supported in that direction). This is the UI half of the
  # source-traceability feature — the data half lives in cauda.claims_to_dag()'s
  # edge_metadata (see R/10-cauda.R). Added 2026-08-01 per Cox's request.
  output$edge_source_ui <- renderUI({
    edges <- custom_edges()
    sel   <- selected_edge()
    if (is.null(edges) || is.null(sel) || nrow(edges) == 0 || sel > nrow(edges)) {
      return(p("Click an edge above to see its source.", style="font-size:11px; color:#888; font-style:italic;"))
    }
    pid   <- if ("paper_id" %in% names(edges)) edges$paper_id[sel] else NA
    quote <- if ("verbatim_quote" %in% names(edges)) edges$verbatim_quote[sel] else NA

    if (is.na(pid) || pid == "") {
      div(style="background:#fff3cd; border:1px solid #ffe08a; border-radius:4px; padding:8px; font-size:11px;",
        strong("No source — "),
        "this edge was added or reversed by hand and isn't backed by a specific paper or quote."
      )
    } else {
      div(style="background:#eafaf1; border:1px solid #a3e4c1; border-radius:4px; padding:8px; font-size:11px;",
        div(strong("Source paper: "), pid, style="margin-bottom:4px;"),
        if (!is.na(quote) && quote != "")
          div(style="font-style:italic; color:#333;", paste0('"', quote, '"'))
        else
          div(style="color:#888;", "(no verbatim quote recorded for this claim)")
      )
    }
  })

  # ── Add edge ──────────────────────────────────────────────────────────────
  # NOTE: add/reverse/delete all call replot_dag() themselves at the end, so
  # the DAG plot updates immediately after each edit. Previously only the
  # separate "Replot DAG" button triggered a redraw, so add/reverse/delete
  # silently changed the edge list without ever touching the plot — it looked
  # like editing did nothing. (Fixed 2026-07-31.) "Replot DAG" is kept as a
  # manual fallback/refresh button.
  observeEvent(input$add_edge_btn, {
    f <- trimws(input$edge_from)
    t <- trimws(input$edge_to)
    if (f == "" || t == "") return()
    edges <- custom_edges() %||% data.frame(from=character(), to=character(), pathway=character(),
                                             paper_id=character(), verbatim_quote=character(), claim_id=character(),
                                             stringsAsFactors=FALSE)
    # Prevent duplicate. Hand-added edges get NA paper_id/verbatim_quote —
    # that's the flag the edge list / source panel use to show "no source".
    if (!any(edges$from == f & edges$to == t))
      custom_edges(rbind(edges, data.frame(from=f, to=t, pathway="unknown",
                                            paper_id=NA_character_, verbatim_quote=NA_character_, claim_id=NA_character_,
                                            stringsAsFactors=FALSE)))
    updateTextInput(session, "edge_from", value="")
    updateTextInput(session, "edge_to",   value="")
    replot_dag()
  })

  # ── Reverse edge ──────────────────────────────────────────────────────────
  observeEvent(input$reverse_edge_btn, {
    sel   <- selected_edge()
    edges <- custom_edges()
    if (is.null(sel) || is.null(edges) || sel > nrow(edges)) return()
    tmp <- edges$from[sel]
    edges$from[sel] <- edges$to[sel]
    edges$to[sel]   <- tmp
    # A reversed edge no longer matches the direction its verbatim quote
    # actually supported, so its source provenance is cleared — showing the
    # original paper/quote next to a flipped arrow would misrepresent what
    # that paper said.
    if ("paper_id" %in% names(edges))       edges$paper_id[sel]       <- NA_character_
    if ("verbatim_quote" %in% names(edges)) edges$verbatim_quote[sel] <- NA_character_
    if ("claim_id" %in% names(edges))       edges$claim_id[sel]       <- NA_character_
    custom_edges(edges)
    replot_dag()
  })

  # ── Delete edge ───────────────────────────────────────────────────────────
  observeEvent(input$delete_edge_btn, {
    sel   <- selected_edge()
    edges <- custom_edges()
    if (is.null(sel) || is.null(edges) || sel > nrow(edges)) return()
    custom_edges(edges[-sel, , drop=FALSE])
    selected_edge(NULL)
    replot_dag()
  })

  # ── Replot (manual refresh button, kept as fallback) ───────────────────────
  observeEvent(input$replot_btn, replot_dag())

  replot_dag <- function() {
    edges <- custom_edges()
    df    <- all_claims()
    req(!is.null(edges), !is.null(df))

    tryCatch({
      # Rebuild dag from custom_edges
      all_nodes <- unique(c(edges$from, edges$to))
      if (length(all_nodes) == 0) {
        output$dag_plot <- renderPlot({
          plot(1, type="n", axes=FALSE, main="No edges in DAG")
          text(1, 1, "Add edges to display DAG", cex=1.5, col="gray")
        }, height = 680)
        output$dag_summary <- renderPrint(cat("No edges.\n"))
        return()
      }

      # Build lookup from display names to safe (bnlearn-valid) names, same
      # scheme cauda.claims_to_dag() uses, so cauda.dag_theory() can render this
      safe           <- make.unique(make.names(all_nodes), sep = "_")
      node_lookup    <- setNames(safe, all_nodes)   # display -> safe
      display_lookup <- setNames(all_nodes, safe)   # safe -> display

      new_dag <- bnlearn::empty.graph(safe)

      # Rebuild edge_metadata (from/to safe names, display names, pathway) so
      # cauda.dag_theory() keeps pathway coloring and readable labels post-edit.
      # paper_id/verbatim_quote/claim_id are carried through too so the DAG
      # object itself stays a complete record of provenance, not just the
      # custom_edges() reactive the UI reads from.
      edge_metadata <- data.frame(
        from = character(), to = character(),
        from_display = character(), to_display = character(),
        pathway = character(), established = logical(),
        paper_id = character(), verbatim_quote = character(), claim_id = character(),
        stringsAsFactors = FALSE
      )

      for (i in seq_len(nrow(edges))) {
        f <- node_lookup[edges$from[i]]
        t <- node_lookup[edges$to[i]]
        if (!is.na(f) && !is.na(t) && f != t) {
          tryCatch({
            new_dag <- bnlearn::set.arc(new_dag, from = f, to = t)
            pw <- if (!is.null(edges$pathway)) edges$pathway[i] else NA
            pid   <- if ("paper_id" %in% names(edges)) edges$paper_id[i] else NA_character_
            quote <- if ("verbatim_quote" %in% names(edges)) edges$verbatim_quote[i] else NA_character_
            cid   <- if ("claim_id" %in% names(edges)) edges$claim_id[i] else NA_character_
            edge_metadata <- rbind(edge_metadata, data.frame(
              from = f, to = t,
              from_display = edges$from[i], to_display = edges$to[i],
              pathway = if (is.null(pw) || is.na(pw) || pw == "") "unknown" else pw,
              established = TRUE,
              paper_id = pid, verbatim_quote = quote, claim_id = cid,
              stringsAsFactors = FALSE
            ))
          }, error = function(e) {
            message(sprintf("Skipped edge %s -> %s: %s", edges$from[i], edges$to[i], conditionMessage(e)))
          })
        }
      }

      attr(new_dag, "edge_metadata")  <- edge_metadata
      attr(new_dag, "display_lookup") <- display_lookup
      attr(new_dag, "pathway_colors") <- c(
        gateway = "#E84545", common_liability = "chartreuse4",
        structural = "royalblue3", behavioral = "#F2A623",
        physiological = "#9B59B6", unknown = "#888888"
      )

      dag_obj(new_dag)

      # Height scales with node count instead of shrinking nodes/text to fit
      # a fixed canvas — a dense claims DAG (20-30+ nodes) needs real vertical
      # room, not tinier and tinier circles. Reference points: 8 nodes ~ 736px,
      # 24 nodes ~ 1248px, capped at 1500px so it never runs away.
      output$dag_plot <- renderPlot({
        tryCatch(
          cauda::cauda.dag_theory(new_dag, verbose=FALSE),
          error = function(e) {
            plot(new_dag, main="Causal DAG")
          }
        )
      }, height = function() {
        n <- length(bnlearn::nodes(new_dag))
        max(600, min(1500, 480 + n * 32))
      })

      output$dag_summary <- renderPrint({
        cat("Nodes:", length(bnlearn::nodes(new_dag)), "\n")
        cat("Edges:", nrow(bnlearn::arcs(new_dag)), "\n\n")
        cat("Edge list:\n")
        print(edge_metadata[, c("from_display", "to_display", "pathway")])
      })

    }, error = function(e) {
      output$dag_plot <- renderPlot({
        plot(1, type="n", axes=FALSE)
        text(1, 1, paste("DAG error:", e$message), cex=1, col="red")
      }, height = 680)
    })
  }

  # ── DAGitty: testable implications ────────────────────────────────────────
  dagitty_res <- reactiveVal(NULL)   # cauda.dagitty() result for the current DAG
  theory_cmp  <- reactiveVal(NULL)   # cauda.dagitty_compare() result across papers
  theory_dags <- reactiveVal(NULL)   # named list of per-paper DAGs, built by compare_theories_btn
  real_data       <- reactiveVal(NULL)   # data frame loaded from inst/extdata
  node_label_map  <- reactiveVal(NULL)   # varmap input id -> DAG display label, for the current node set
  empirical_result<- reactiveVal(NULL)   # cauda.test_implications_compare() result

  observeEvent(input$dagitty_btn, {
    req(dag_obj())
    tryCatch({
      dagitty_res(cauda::cauda.dagitty(dag_obj(), verbose = FALSE))
    }, error = function(e) {
      dagitty_res(NULL)
      showNotification(paste("DAGitty error:", e$message), type = "error")
    })
  })

  output$dagitty_implications <- renderUI({
    render_dagitty_implications(dagitty_res())
  })

  output$download_dagitty_string <- downloadHandler(
    filename = function() paste0("dagitty_model_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt"),
    content  = function(file) {
      req(dagitty_res())
      writeLines(dagitty_res()$dagitty_string, file)
    }
  )

  # NOTE: cauda.claims_to_dag() stop()s per-paper if that paper's claims
  # produced zero directed causal claims (claim_type not in causal_effect/
  # mechanism/mediation/interaction/moderation — e.g. a purely descriptive or
  # predictive-model paper). That tryCatch used to swallow the error and
  # silently drop the paper from `dags`, so the Compare Theories tab would
  # just render fewer cards than papers uploaded with no explanation — the
  # same silent-failure pattern already fixed once for Critique/Synthesis.
  # Now the reason for every skipped paper is captured and surfaced as a
  # persistent banner in the tab (not just a toast that disappears). (Fixed
  # 2026-08-01.)
  observeEvent(input$compare_theories_btn, {
    req(all_claims())
    df <- all_claims()
    if (!"paper_id" %in% names(df) || length(unique(stats::na.omit(df$paper_id))) < 2) {
      showNotification("Need extracted claims from at least 2 papers to compare theories.", type = "warning")
      return()
    }
    tryCatch({
      pids <- unique(stats::na.omit(df$paper_id))
      dags <- list()
      skipped <- character(0)
      for (pid in pids) {
        sub <- df[!is.na(df$paper_id) & df$paper_id == pid, ]
        d <- tryCatch(
          cauda::cauda.claims_to_dag(sub, confidence_threshold = "low", include_speculative = TRUE, verbose = FALSE),
          error = function(e) {
            skipped[[length(skipped) + 1]] <<- sprintf("%s (%s)", pid, conditionMessage(e))
            NULL
          }
        )
        if (!is.null(d)) dags[[pid]] <- d
      }
      if (length(dags) < 2) {
        theory_cmp(list(result = NULL, skipped = skipped))
        theory_dags(NULL)
        showNotification("Fewer than 2 papers produced a usable DAG (each needs at least one directed claim).", type = "warning")
        return()
      }
      theory_dags(dags)
      theory_cmp(list(result = cauda::cauda.dagitty_compare(dags, verbose = FALSE), skipped = skipped))
    }, error = function(e) {
      showNotification(paste("Comparison error:", e$message), type = "error")
    })
  })

  output$discriminating_table <- renderUI({
    tc <- theory_cmp()
    render_theory_comparison(tc$result, tc$skipped)
  })

  # ── TEST AGAINST REAL DATA (Cox workflow step 5) ───────────────────────────
  # Two data sources feed the same `real_data()` reactive:
  #  (a) the bundled opioid dataset — inst/extdata/opioid_real_data.csv, built
  #      by data-raw/build_opioid_test_data.R from CDC VSRR + DEA ARCOS.
  #      Loaded via system.file() so it works both in local devtools::load_all()
  #      sessions and once the package is installed on shinyapps.io.
  #  (b) any user-uploaded CSV, for topics we don't have a bundled dataset for.
  # Everything downstream (var_map_ui, run_empirical_test_btn) is agnostic to
  # which source populated real_data() — it just needs a data frame.
  observeEvent(input$load_real_data_btn, {
    path <- system.file("extdata", "opioid_real_data.csv", package = "cauda")
    if (path == "" || !file.exists(path)) {
      showNotification("Bundled opioid_real_data.csv not found in the installed package.", type = "error")
      return()
    }
    tryCatch({
      df <- read.csv(path, stringsAsFactors = FALSE)
      real_data(df)
    }, error = function(e) {
      showNotification(paste("Failed to load real data:", e$message), type = "error")
    })
  })

  observeEvent(input$real_data_upload, {
    file <- input$real_data_upload
    if (is.null(file)) return()
    tryCatch({
      df <- read.csv(file$datapath, stringsAsFactors = FALSE)
      if (ncol(df) < 2) {
        showNotification("That CSV only has one column — need at least an ID column and one variable to map.", type = "error")
        return()
      }
      real_data(df)
    }, error = function(e) {
      showNotification(paste("Failed to read uploaded CSV:", e$message), type = "error")
    })
  })

  output$real_data_status_ui <- renderUI({
    df <- real_data()
    if (is.null(df)) {
      return(p("No real data loaded yet.", class = "empty-hint"))
    }
    id_cols  <- intersect(c("state", "state_name", "year"), names(df))
    val_cols <- setdiff(names(df), id_cols)
    div(
      p(sprintf("✓ Loaded %d rows x %d columns.", nrow(df), ncol(df)),
        style = "color:#27ae60; font-weight:bold; font-size:0.9em;"),
      p(strong("Real variables available to map: "), paste(val_cols, collapse = ", "),
        style = "font-size:0.85em; color:#555;")
    )
  })

  # Build the node -> data-column mapping UI once both a real dataset and a
  # set of per-paper theory DAGs exist. Node labels come straight out of
  # each DAG's display_lookup (cauda.claims_to_dag()'s human-readable claim
  # text), deduplicated across all theories so a construct shared by two
  # theories (e.g. "Overdose death") only needs mapping once.
  output$var_map_ui <- renderUI({
    df   <- real_data()
    dags <- theory_dags()
    if (is.null(df) || is.null(dags)) {
      return(p("Load the real dataset and run 'Compare Papers as Competing Theories' first.",
                class = "empty-hint"))
    }
    id_cols  <- intersect(c("state", "state_name", "year"), names(df))
    val_cols <- setdiff(names(df), id_cols)

    all_labels <- unique(unlist(lapply(dags, function(d) unname(attr(d, "display_lookup")))))
    all_labels <- sort(all_labels)

    ids <- paste0("varmap_", seq_along(all_labels))
    node_label_map(setNames(all_labels, ids))

    rows <- lapply(seq_along(all_labels), function(i) {
      div(class = "varmap-row",
        span(all_labels[i], class = "varmap-label"),
        div(style = "width:280px;",
          selectInput(ids[i], NULL, choices = c("(no real data)" = "", val_cols), width = "100%")
        )
      )
    })

    div(
      h6(sprintf("Map %d DAG node(s) to real data columns", length(all_labels))),
      do.call(tagList, rows)
    )
  })

  observeEvent(input$run_empirical_test_btn, {
    df   <- real_data()
    dags <- theory_dags()
    labs <- node_label_map()
    if (is.null(df) || is.null(dags) || is.null(labs)) {
      showNotification("Load the real dataset and compare theories first.", type = "warning")
      return()
    }
    ids <- names(labs)
    selected <- sapply(ids, function(id) {
      v <- input[[id]]
      if (is.null(v)) "" else v
    })
    var_map <- selected[selected != ""]
    names(var_map) <- unname(labs[names(var_map)])

    if (length(var_map) == 0) {
      showNotification("Map at least one DAG node to a real data column first.", type = "warning")
      return()
    }

    tryCatch({
      empirical_result(cauda::cauda.test_implications_compare(dags, df, var_map, verbose = FALSE))
    }, error = function(e) {
      empirical_result(NULL)
      showNotification(paste("Empirical test error:", e$message), type = "error")
    })
  })

  output$empirical_test_results_ui <- renderUI({
    render_empirical_results(empirical_result())
  })

  # ── Download synthesis ────────────────────────────────────────────────────
  output$download_synthesis <- downloadHandler(
    filename = function() paste0("synthesis_", format(Sys.time(),"%Y%m%d_%H%M%S"), ".txt"),
    content  = function(file) {
      req(synthesis_res())
      s <- synthesis_res()
      writeLines(paste0(
        "CAUDA SYNTHESIS REPORT\n",
        "Generated: ", format(Sys.time(),"%Y-%m-%d %H:%M:%S"), "\n\n",
        "SUMMARY\n=======\n", s$summary, "\n\n",
        "STRENGTHS\n=========\n", s$key_strengths, "\n\n",
        "LIMITATIONS\n===========\n", s$key_limitations, "\n\n",
        "BOTTOM LINE\n===========\n", s$bottom_line, "\n"
      ), file)
    }
  )

  # ── Debug ─────────────────────────────────────────────────────────────────
  output$raw_claims         <- renderText(raw_response() %||% "No response yet.")
  output$parsed_claims_output <- renderPrint({
    df <- all_claims()
    if (is.null(df)) cat("No claims yet.\n") else print(df)
  })

  # ── Initial placeholders ──────────────────────────────────────────────────
  # NOTE: output$detailed_claims is intentionally NOT reset here — it already
  # has a real reactive renderUI() bound above (driven by filtered_claims())
  # that handles the "no claims yet" case itself. Re-assigning it here would
  # overwrite that reactive binding with a static, never-updating placeholder,
  # permanently pinning the tab to "Extract claims first." even after claims
  # are extracted. (This was a real bug, fixed 2026-07-31.)
  output$status_ui      <- renderUI(p("📄 Upload PDF(s) to get started."))
  output$critique_output <- renderUI(p("Run critique after extracting.", style="color:#666;"))
  output$synthesis_output<- renderUI(p("Run synthesis after critique.", style="color:#666;"))
  output$dag_plot <- renderPlot({
    plot(1,type="n",axes=FALSE,main="DAG will appear here")
    text(1,1,"Build DAG from Claims",cex=1.5,col="gray")
  }, height = 680)
  output$dag_summary <- renderPrint(cat("DAG not yet generated.\n"))
}


# ── Helper: critique render ───────────────────────────────────────────────────
render_critique_results <- function(crit) {
  if (is.null(crit) || nrow(crit) == 0) return(p("No critique results."))
  rows <- lapply(seq_len(nrow(crit)), function(i) {
    r <- crit[i,]
    sc <- switch(r$causal_strength %||% "",
      strong="badge-high", moderate="badge-med", weak="badge-low", "badge-low")
    div(style="margin-bottom:16px; padding:12px; border-left:4px solid #e74c3c;
               background:#fef5f5; border-radius:4px;",
      h5(paste0(r$source," → ",r$target), style="margin:0 0 6px;"),
      div(style="display:flex; gap:8px; flex-wrap:wrap; margin-bottom:8px;",
        span(r$causal_strength %||% "?", class=paste("badge",sc)),
        span(r$support_summary %||% "?", class="badge",
             style="background:#3498db;"),
        span(paste("adj:", r$confidence_adjusted %||% "?"),
             class="badge", style="background:#555;")
      ),
      if (!is.na(r$critique) && r$critique != "")
        p(r$critique, style="font-size:0.9em; margin:4px 0;"),
      if (!is.na(r$key_gaps) && r$key_gaps != "")
        div(style="margin-top:6px; padding:8px; background:#fff3cd; border-radius:3px;
                   font-size:0.88em;",
          strong("Gaps: "), r$key_gaps)
    )
  })
  div(h4("Critique Results",
         style="border-bottom:2px solid #e74c3c; padding-bottom:8px;"),
      do.call(div, rows))
}


# ── Helper: synthesis render ──────────────────────────────────────────────────
render_synthesis_report <- function(s, claims, crit) {
  div(
    div(style="padding:16px; background:#e8f4f8; border-left:4px solid #3498db;
               border-radius:4px; margin-bottom:20px;",
      h4("Summary", style="margin-top:0;"), p(s$summary)),
    div(style="padding:16px; background:#e8f8e8; border-left:4px solid #27ae60;
               border-radius:4px; margin-bottom:20px;",
      h4("Strengths", style="margin-top:0;"), p(s$key_strengths, style="white-space:pre-wrap;")),
    div(style="padding:16px; background:#fff8e8; border-left:4px solid #f39c12;
               border-radius:4px; margin-bottom:20px;",
      h4("Limitations", style="margin-top:0;"), p(s$key_limitations, style="white-space:pre-wrap;")),
    div(style="padding:16px; background:#fef5f5; border-left:4px solid #e74c3c;
               border-radius:4px;",
      h4("Bottom Line", style="margin-top:0;"), p(s$bottom_line, style="white-space:pre-wrap;"))
  )
}


# ── Helper: render a single "X _||_ Y | {Z}" conditional independence row ─────
render_ci_row <- function(r) {
  z_chips <- if (is.null(r$Z) || is.na(r$Z) || r$Z == "" || r$Z == "(nothing)") {
    span("(no conditioning set)", class = "ci-cond-none")
  } else {
    tagList(
      span("given", class = "ci-cond-label"),
      lapply(trimws(strsplit(r$Z, ",")[[1]]), function(z) span(z, class = "ci-cond-chip"))
    )
  }
  div(class = "ci-row",
    span(r$X, class = "ci-var"),
    span("⊥", class = "ci-symbol"),
    span(r$Y, class = "ci-var"),
    z_chips
  )
}

# ── Helper: render a data frame of CI rows as a compact bordered list,
# capped at `cap` visible rows with the rest tucked behind a native
# <details> "Show N more" toggle. A full DAG's basis set can easily run to
# 20-40+ implications, and rendering each as its own big padded card was
# overwhelming ("overkill") — this keeps the common case short while still
# making every implication reachable.
render_ci_list <- function(df, cap = 8) {
  n <- nrow(df)
  rows <- lapply(seq_len(n), function(i) render_ci_row(df[i, ]))
  if (n <= cap) {
    return(div(class = "ci-list", do.call(tagList, rows)))
  }
  div(class = "ci-list",
    do.call(tagList, rows[1:cap]),
    tags$details(
      tags$summary(sprintf("Show %d more", n - cap), class = "show-more-toggle"),
      do.call(tagList, rows[(cap + 1):n])
    )
  )
}

# ── Helper: dagitty testable-implications render ──────────────────────────────
render_dagitty_implications <- function(res) {
  if (is.null(res)) {
    return(div(p("Build a DAG in the 'Causal DAG' tab first, then click ",
                 "'Derive Testable Implications'.", class = "empty-hint")))
  }

  model_toggle <- tags$details(
    tags$summary("View dagitty model definition (paste into dagitty.net)", class = "dagitty-toggle"),
    div(class = "dagitty-box", res$dagitty_string)
  )

  if (res$n_implications == 0) {
    return(div(
      model_toggle,
      p("No testable implications: every pair of nodes is adjacent (the DAG is ",
        "saturated), or there are too few nodes to imply any conditional independence.",
        class = "empty-hint")
    ))
  }

  div(
    model_toggle,
    h4(sprintf("%d Testable Implications", res$n_implications),
       style = "border-bottom:2px solid #16a085; padding-bottom:8px;"),
    render_ci_list(res$implied_CIs, cap = 8)
  )
}

# ── Helper: theory color palette (stable per theory name) ─────────────────────
theory_color <- function(name, all_names) {
  palette <- c("#8e44ad","#2980b9","#16a085","#d35400","#c0392b","#27ae60","#f39c12","#2c3e50")
  idx <- match(name, unique(all_names))
  palette[((idx - 1) %% length(palette)) + 1]
}

# ── Helper: compare-theories render ────────────────────────────────────────────
# `skipped` is a character vector of "paper_id (reason)" strings for any
# paper that couldn't produce a usable DAG (e.g. no directed causal claims) —
# see the compare_theories_btn observer. Rendered as a persistent banner so
# it's not just a toast the user has to catch before it fades. This is why
# e.g. a purely descriptive/predictive-model paper can silently be absent
# from the comparison otherwise: it never produced a DAG to compare with.
render_theory_comparison <- function(cmp, skipped = NULL) {
  skip_note <- if (!is.null(skipped) && length(skipped) > 0) {
    div(style = "margin-bottom:12px; padding:8px 12px; background:#fff3cd;
                 border-left:4px solid #f0ad4e; border-radius:4px; font-size:0.85em; color:#7a5c00;",
      strong(sprintf("%d paper(s) excluded from comparison: ", length(skipped))),
      paste(skipped, collapse = "; ")
    )
  } else NULL

  if (is.null(cmp)) {
    return(div(skip_note,
      p("Extract claims from 2+ papers, build a DAG, then click ",
        "'Compare Papers as Competing Theories'.", class = "empty-hint")))
  }
  if (is.null(cmp$discriminating) || nrow(cmp$discriminating) == 0) {
    return(div(skip_note,
      p("No discriminating implications found — every theory's predictions ",
        "overlap completely, or the comparison hasn't found any yet.",
        class = "empty-hint")))
  }

  disc <- cmp$discriminating
  theories <- unique(disc$theory)

  cards <- lapply(theories, function(th) {
    sub <- disc[disc$theory == th, ]
    color <- theory_color(th, disc$theory)
    div(class = "theory-card", style = paste0("border-left-color:", color, ";"),
      div(style = "margin-bottom:8px;",
        span(th, class = "theory-badge", style = paste0("background:", color, ";")),
        span(sprintf("%d implication(s) unique to this theory", nrow(sub)),
             style = "color:#888; font-size:0.85em;")
      ),
      render_ci_list(sub, cap = 6)
    )
  })

  div(
    skip_note,
    h4(sprintf("%d Discriminating Implications Across %d Theories", nrow(disc), length(theories)),
       style = "border-bottom:2px solid #8e44ad; padding-bottom:8px;"),
    p("Each implication below is predicted by exactly one theory — the best place to ",
      "start testing against data, since a violation would count as evidence against ",
      "that theory specifically.", style = "font-size:12px; color:#888;"),
    do.call(div, cards)
  )
}


# ── Helper: render a single tested-implication row with real stats ────────────
render_test_row <- function(r) {
  violated <- grepl("^VIOLATED", r$conclusion)
  row_class <- if (violated) "test-row test-row-violated" else "test-row test-row-consistent"
  verdict <- if (violated) {
    span("VIOLATED", class = "test-verdict verdict-violated")
  } else {
    span("consistent", class = "test-verdict verdict-consistent")
  }
  z_txt <- if (is.null(r$Z) || is.na(r$Z) || r$Z == "" || r$Z == "(nothing)") {
    "(no conditioning set)"
  } else {
    paste("given", r$Z)
  }
  div(class = row_class,
    span(r$X, class = "ci-var"),
    span("⊥", class = "ci-symbol"),
    span(r$Y, class = "ci-var"),
    span(z_txt, class = "ci-cond-label"),
    verdict,
    span(sprintf("estimate=%.3f, p=%.3g", r$estimate, r$p_value), class = "test-stat")
  )
}

# ── Helper: render cauda.test_implications_compare() results ──────────────────
# Per Cox's explicit framing this is empirical CRITICISM, not confirmation --
# the summary/notes below are written to keep that distinction visible rather
# than reading like a scoreboard of which theory "won".
render_empirical_results <- function(cmp) {
  if (is.null(cmp)) {
    return(p("Map at least one DAG node to a real data column, then click ",
             "'Test Implications Against Real Data'.", class = "empty-hint"))
  }

  summ <- cmp$summary
  summary_table <- tags$table(class = "summary-table",
    tags$thead(tags$tr(
      tags$th("Theory"), tags$th("Tested"), tags$th("Untestable"),
      tags$th("Violated"), tags$th("% Survived")
    )),
    tags$tbody(
      lapply(seq_len(nrow(summ)), function(i) {
        tags$tr(
          tags$td(summ$theory[i]),
          tags$td(summ$n_tested[i]),
          tags$td(summ$n_untestable[i]),
          tags$td(summ$n_violated[i]),
          tags$td(if (is.na(summ$pct_survived[i])) "—" else paste0(summ$pct_survived[i], "%"))
        )
      })
    )
  )

  theory_sections <- lapply(names(cmp$per_theory), function(nm) {
    res <- cmp$per_theory[[nm]]
    color <- theory_color(nm, names(cmp$per_theory))

    tested_ui <- if (!is.null(res$tested) && nrow(res$tested) > 0) {
      div(do.call(tagList, lapply(seq_len(nrow(res$tested)), function(i) render_test_row(res$tested[i, ]))))
    } else {
      p("No implications had full data coverage.", class = "empty-hint")
    }

    untestable_ui <- if (!is.null(res$untestable) && nrow(res$untestable) > 0) {
      tags$details(
        tags$summary(sprintf("%d untestable implication(s) — no data mapped", nrow(res$untestable)),
                     class = "show-more-toggle"),
        div(do.call(tagList, lapply(seq_len(nrow(res$untestable)), function(i) {
          r <- res$untestable[i, ]
          div(class = "untestable-row",
            sprintf("%s ⊥ %s | %s — %s", r$X, r$Y, r$Z, r$reason))
        })))
      )
    } else NULL

    div(class = "theory-card", style = paste0("border-left-color:", color, ";"),
      div(style = "margin-bottom:8px;",
        span(nm, class = "theory-badge", style = paste0("background:", color, ";"))
      ),
      tested_ui,
      untestable_ui
    )
  })

  div(
    h4("Empirical Test Results", style = "border-bottom:2px solid #c0392b; padding-bottom:8px;"),
    summary_table,
    p(strong("Reading this: "), "\"% survived\" is the share of a theory's TESTABLE ",
      "implications that were NOT contradicted by data — a measure of how much a theory ",
      "has survived empirical criticism so far, not a probability it is \"true\". Theories ",
      "differ a lot in how much of their claim chain current public data can even speak to ",
      "(see the untestable counts) — that gap is itself a real finding, not a limitation of ",
      "this tool.", style = "font-size:12px; color:#888; margin-bottom:16px;"),
    do.call(div, theory_sections)
  )
}


`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

shinyApp(ui, server)
