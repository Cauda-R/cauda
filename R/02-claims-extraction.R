#' Verify Extracted Quotes Against Source Text
#'
#' Checks each claim's `verbatim_quote` against the actual source text it was
#' supposedly drawn from, and flags quotes that don't actually appear there.
#' This closes the gap documented in the accompanying paper's Section 4.3 and
#'7: the extraction prompt instructs the model to quote exactly, but nothing
#' previously checked whether it complied, and roughly 15% of quotations
#' across that paper's two applications turned out to contain interpretive
#' framing the source paper never states. This function is that check.
#'
#' Matching is done in three tiers, from strictest to loosest: (1) an exact
#' substring match after whitespace normalization, (2) an approximate match
#' via [agrepl()] within `max_distance` (a fraction of the quote's own
#' length) to catch trivial OCR/whitespace/typographic differences without
#' being fooled by a genuine paraphrase, and (3) anything that fails both is
#' flagged as not found — which per Section 4.3's audit is the right default
#' bucket for a paraphrase or fabrication, not something to guess at.
#'
#' @param claims Data frame with a `verbatim_quote` column, as returned by
#'   [cauda.extract()].
#' @param source_text The full source text the claims were extracted from
#'   (same string passed to [cauda.extract()], or the paper's raw text).
#' @param max_distance Numeric in \[0, 1\). Maximum edit distance, as a
#'   fraction of the quote's length, still counted as a match. Default 0.15
#'   (deliberately tight — this is meant to catch near-identical text, not
#'   loose paraphrases; anything looser than this should be flagged, not
#'   silently passed).
#' @param verbose Logical. Print a verification summary. Default `TRUE`.
#'
#' @return `claims` with two columns added: `quote_verified` (logical) and
#'   `quote_verification` (character, one of `"exact match"`, a near-exact
#'   match message, `"NOT FOUND in source text -- treat as unverified
#'   (possible paraphrase or fabrication)"`, or `"no quote provided"`).
#'
#' @examples
#' \dontrun{
#'   claims <- cauda.extract_pdf("paper.pdf", paper_id = "paper1")
#'   # cauda.extract_pdf() already runs this automatically; to re-check
#'   # (e.g. with a different threshold) or verify claims from another source:
#'   text <- paste(pdftools::pdf_text("paper.pdf"), collapse = "\n")
#'   claims <- cauda.verify_quotes(claims, text)
#'   mean(claims$quote_verified)
#' }
#'
#' @export
cauda.verify_quotes <- function(claims, source_text, max_distance = 0.15, verbose = TRUE) {

  if (nrow(claims) == 0) {
    claims$quote_verified <- logical(0)
    claims$quote_verification <- character(0)
    return(claims)
  }

  # Normalizes whitespace AND common Unicode punctuation variants (curly
  # quotes, em/en dashes, non-breaking hyphens) to their plain-ASCII
  # equivalents before comparing. Without this, a genuinely verbatim quote
  # fails a naive string match purely because the PDF renders "-" as an
  # em-dash or a straight quote as a curly one -- a typographic difference,
  # not a content difference, and treating it as "not found" would produce
  # false positives on the very fabrication check this function exists to
  # perform. (Discovered during testing: without this normalization, a
  # genuinely verbatim sentence from the real opioid Theory B source paper
  # was wrongly flagged as unverified solely because of em-dash/curly-quote
  # rendering differences.)
  #
  # All remaining quote marks (' and ") are then stripped entirely, not just
  # normalized to straight quotes. Reason: when the LLM extractor emits a
  # verbatim_quote as a JSON string value, a nested double quote in the
  # source (e.g. a term the source itself puts in quotes, like "deaths of
  # despair") gets serialized as a single quote to avoid escaping conflicts
  # inside the JSON string. That's a serialization artifact of the
  # extraction pipeline, not a content change, so quote-mark type must not
  # be load-bearing for the verbatim match. (Discovered during testing: the
  # Case & Deaton "deaths of despair" sentence -- genuinely verbatim -- was
  # flagged NOT FOUND solely because the source's double-quoted term came
  # back from the LLM wrapped in single quotes.)
  .norm <- function(x) {
    x <- gsub("[‘’ʼ´`]", "'", x)
    x <- gsub("[“”]", '"', x)
    x <- gsub("[–—−]", "-", x)
    x <- gsub("['\"]", "", x)
    x <- gsub("\\s+", " ", x)
    trimws(x)
  }
  source_norm <- .norm(source_text)

  status <- vapply(claims$verbatim_quote, function(q) {
    if (is.na(q) || .norm(q) == "") return("no quote provided")
    q_norm <- .norm(q)

    if (grepl(q_norm, source_norm, fixed = TRUE)) return("exact match")

    found <- tryCatch(
      isTRUE(agrepl(q_norm, source_norm, max.distance = max_distance, ignore.case = TRUE)),
      error = function(e) FALSE
    )

    if (found) {
      sprintf("near-exact match (within %.0f%% edit distance)", max_distance * 100)
    } else {
      "NOT FOUND in source text -- treat as unverified (possible paraphrase or fabrication)"
    }
  }, character(1), USE.NAMES = FALSE)

  claims$quote_verification <- status
  claims$quote_verified <- status == "exact match" | grepl("^near-exact", status)

  if (verbose) {
    n_ok <- sum(claims$quote_verified)
    cat("=== Quote Verification ===\n")
    cat(sprintf("Verified: %d / %d claims (%.0f%%)\n", n_ok, nrow(claims), 100 * n_ok / nrow(claims)))
    flagged <- claims[!claims$quote_verified, , drop = FALSE]
    if (nrow(flagged) > 0) {
      cat("\nFlagged (quote not found verbatim or near-verbatim in source text):\n")
      for (i in seq_len(nrow(flagged))) {
        cat(sprintf('  [%s -> %s]  "%s"\n',
                     flagged$source[i], flagged$target[i],
                     substr(flagged$verbatim_quote[i], 1, 90)))
      }
    }
    cat("\n")
  }

  claims
}


#' Extract Causal Claims from Academic Text (Hybrid Module 0 + Cauda)
#'
#' Extracts causal and causal-ish claims from any academic paper type —
#' empirical studies, narrative reviews, perspective pieces, and policy papers.
#'
#' Combines:
#' - Cox Module 0 approach: verbatim anchoring, claim_class A-D, causal/policy triggers
#' - Cauda approach: source/target DAG structure, effect sizes, pathway tagging
#'
#' @param text Character string containing the paper text
#' @param model GPT model. Default: "gpt-4o" (switched from gpt-4-turbo on
#'   2026-08-01 for cost: gpt-4-turbo is $10/$30 per million input/output
#'   tokens and deprecated by OpenAI; gpt-4o is $2.50/$10 — about 4x cheaper
#'   with comparable quality for this structured extraction task)
#' @param temperature Numeric 0-1. Default: 0.3
#' @param max_tokens Integer. Default: 4000
#' @param paper_id Optional string to tag claims with source paper. Default: NULL
#' @param return_raw_text Logical. Return raw GPT JSON string. Default: FALSE
#' @param verify_quotes Logical. Automatically check every returned claim's
#'   `verbatim_quote` against this call's own source text via
#'   [cauda.verify_quotes()], adding `quote_verified`/`quote_verification`
#'   columns. Default: TRUE. See that function's documentation for what
#'   "verified" means and why this check exists.
#' @param verbose Logical. Print status messages. Default: TRUE
#'
#' @return Data frame with columns:
#'   paper_id, verbatim_quote, source, target, claim,
#'   claim_type (causal_effect/mechanism/mediation/interaction/moderation/association),
#'   claim_class (A/B/C/D), causal_trigger, policy_trigger, claim_scope,
#'   confidence (high/medium/low), effect_size, p_value, sample_size,
#'   pathway, established, evidence, notes, and (when `verify_quotes = TRUE`,
#'   the default) quote_verified, quote_verification -- see
#'   [cauda.verify_quotes()]
#'
#' @export
#' @importFrom httr POST add_headers status_code content
#' @importFrom jsonlite toJSON fromJSON
cauda.extract <- function(
  text,
  model        = "gpt-4o",
  temperature  = 0.3,
  max_tokens   = 4000,
  paper_id     = NULL,
  return_raw_text = FALSE,
  verify_quotes = TRUE,
  verbose      = TRUE
) {

  # Load .Renviron if present (important for ShinyApps.io)
  if (file.exists(".Renviron")) readRenviron(".Renviron")

  api_key <- Sys.getenv("OPENAI_API_KEY")
  if (api_key == "" || nchar(api_key) < 10) {
    stop("OPENAI_API_KEY not set. Use Sys.setenv(OPENAI_API_KEY = 'sk-...')")
  }

  # Truncate to ~60k chars (~45k tokens) leaving room for prompt + response
  max_text_chars <- 60000
  if (nchar(text) > max_text_chars) {
    text <- substr(text, 1, max_text_chars)
    if (verbose) message(sprintf("Text truncated to %d characters", max_text_chars))
  }

  prompt <- paste0(
    "You are a world-class causal inference expert. Extract ALL causal and causal-ish claims ",
    "from the paper below. This includes empirical findings, narrative arguments, policy ",
    "implications, and mechanistic speculation — not just RCT results.\n\n",

    "=== CLAIM CLASSIFICATION ===\n\n",

    "claim_class (assign to EVERY claim):\n",
    "  A = Descriptive or associational only; no intervention/action implication\n",
    "      Example: 'UPF consumption is associated with higher crime rates'\n",
    "  B = Directional/risk language stronger than description, but still observational\n",
    "      Example: 'Higher UPF intake is linked to increased aggression risk'\n",
    "  C = Language implying that changing X would change Y, without explicit recommendation\n",
    "      Example: 'Reducing UPF consumption may lower rates of antisocial behavior'\n",
    "  D = Explicit intervention, recommendation, policy, discouragement, or decision-use\n",
    "      Example: 'Prison food programs should eliminate ultra-processed foods'\n\n",

    "claim_type (pick one):\n",
    "  causal_effect  = Direct causal claim with empirical support (RCT, quasi-experiment)\n",
    "  mechanism      = Proposes biological/psychological/social pathway\n",
    "  mediation      = X affects Y through mediator M\n",
    "  interaction    = Effect of X on Y depends on Z\n",
    "  moderation     = Z moderates the X->Y relationship\n",
    "  association    = Observational association without causal framing\n\n",

    "claim_scope (pick one):\n",
    "  overall_exposure       = Claim about the main exposure broadly\n",
    "  component_or_subtype   = Claim about a specific component or subgroup\n",
    "  policy_implication     = What policy should follow from the evidence\n",
    "  recommendation         = Explicit recommendation to act\n",
    "  mechanism_or_speculation = Mechanistic or speculative claim\n\n",

    "causal_trigger: true if the quote implies changing X would change Y. false otherwise.\n",
    "policy_trigger: true if the quote supports, implies, or recommends action/policy. false otherwise.\n\n",

    "=== CAUDA QUANTITATIVE FIELDS ===\n\n",

    "confidence (for empirical papers — use 'low' for narrative/perspective):\n",
    "  high   = RCT/randomized experiment + p<0.05 + large effect (d≥0.8, OR≥2.5, HR≤0.6)\n",
    "  medium = RCT with moderate effect, OR a well-controlled quasi-experimental/\n",
    "           observational design (matched cohort, instrumental variable,\n",
    "           difference-in-differences, regression discontinuity, or similar)\n",
    "           with p<0.05 and a moderate-to-large effect\n",
    "  low    = p>0.05, small effect, purely correlational with no causal design or\n",
    "           confounder adjustment, prediction/classification-model claims (e.g.\n",
    "           ML accuracy findings) that aren't causal claims at all, or\n",
    "           speculative/mechanistic claims\n",
    "  Do not default to 'low' just because a study isn't a literal RCT — reward\n",
    "  genuinely well-controlled quasi-experimental designs with 'medium'. But\n",
    "  papers about prediction/classification models (not causal effects) should\n",
    "  still be 'low' regardless of model accuracy, since accuracy isn't causal\n",
    "  evidence.\n\n",

    "pathway (mechanism type):\n",
    "  gateway | common_liability | structural | behavioral | physiological | unknown\n\n",

    "=== OUTPUT FORMAT ===\n\n",
    "Return ONLY valid JSON. No markdown. No code fences. No commentary.\n\n",
    "Use exactly this structure:\n",
    '{\n',
    '  "study_summary": {\n',
    '    "paper_type": "empirical|narrative_review|perspective|meta_analysis|other",\n',
    '    "topic": "brief topic description",\n',
    '    "study_design": "RCT|observational|review|perspective|etc"\n',
    '  },\n',
    '  "claims": [\n',
    '    {\n',
    '      "verbatim_quote": "exact quote from paper anchoring this claim",\n',
    '      "source": "independent variable / causal factor",\n',
    '      "target": "dependent variable / outcome",\n',
    '      "claim": "full claim statement with specifics",\n',
    '      "claim_type": "causal_effect|mechanism|mediation|interaction|moderation|association",\n',
    '      "claim_class": "A|B|C|D",\n',
    '      "causal_trigger": true,\n',
    '      "policy_trigger": false,\n',
    '      "claim_scope": "overall_exposure|component_or_subtype|policy_implication|recommendation|mechanism_or_speculation",\n',
    '      "confidence": "high|medium|low",\n',
    '      "effect_size": "e.g. d=1.2 or unclear",\n',
    '      "p_value": "e.g. p<0.001 or unreported",\n',
    '      "sample_size": "e.g. N=284 or unreported",\n',
    '      "pathway": "gateway|common_liability|structural|behavioral|physiological|unknown",\n',
    '      "established": true,\n',
    '      "evidence": "specific support: test stats, mechanism, citation",\n',
    '      "notes": "qualifications, moderators, limitations"\n',
    '    }\n',
    '  ]\n',
    '}\n\n',

    "=== EXTRACTION RULES ===\n\n",
    "1. INCLUDE ALL PAPER TYPES. Narrative reviews, perspective pieces, and policy papers\n",
    "   contain valuable causal-ish claims — extract them even without p-values.\n",
    "2. Anchor every claim to a verbatim_quote from the paper.\n",
    "3. One quote = one claim row. Do not merge multiple quotes.\n",
    "4. Extract every distinct causal or causal-ish claim the paper actually supports —\n",
    "   do not target a fixed number. A focused paper with one central finding may\n",
    "   genuinely yield only a handful of claims; a paper weighing several mechanisms\n",
    "   or confounders may yield many more. Under-extraction is worse than a short list:\n",
    "   scan Results, Discussion, AND Conclusion thoroughly before finishing.\n",
    "5. For narrative/perspective papers: most claims will be class B or C, confidence=low.\n",
    "   That is expected and correct — still extract them.\n",
    "6. Include mechanistic speculation (claim_type=mechanism, established=false).\n",
    "7. Include policy implications (claim_class=D, policy_trigger=true).\n",
    "8. Assign established=false for speculative/unconfirmed claims.\n",
    "9. effect_size, p_value, sample_size = 'unreported' for narrative papers.\n\n",

    "=== PAPER TEXT ===\n\n",
    text
  )

  request_body <- list(
    model       = model,
    messages    = list(list(role = "user", content = prompt)),
    temperature = temperature,
    max_tokens  = max_tokens
  )

  tryCatch({
    response <- httr::POST(
      url = "https://api.openai.com/v1/chat/completions",
      httr::add_headers(
        `Authorization` = paste("Bearer", api_key),
        `Content-Type`  = "application/json"
      ),
      body   = jsonlite::toJSON(request_body, auto_unbox = TRUE),
      encode = "raw"
    )

    if (httr::status_code(response) != 200) {
      err <- tryCatch(
        jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8")),
        error = function(e) list(error = list(message = "Unknown error"))
      )
      stop(sprintf("OpenAI API Error (%d): %s",
                   httr::status_code(response), err$error$message))
    }

    response_text <- httr::content(response, as = "text", encoding = "UTF-8")
    result <- jsonlite::fromJSON(response_text, simplifyVector = FALSE)

    if (is.null(result$choices) || length(result$choices) == 0)
      stop("No choices in API response")

    raw_json <- result$choices[[1]]$message$content

    if (return_raw_text) return(raw_json)

    claims_df <- parse_claims_json(raw_json, paper_id = paper_id)

    if (verify_quotes && nrow(claims_df) > 0) {
      claims_df <- cauda.verify_quotes(claims_df, text, verbose = verbose)
    }

    attr(claims_df, "raw_response")  <- raw_json
    attr(claims_df, "study_summary") <- tryCatch({
      parsed <- jsonlite::fromJSON(raw_json, simplifyVector = FALSE)
      parsed$study_summary
    }, error = function(e) NULL)

    return(claims_df)

  }, error = function(e) {
    stop(sprintf("Error extracting claims: %s", conditionMessage(e)))
  })
}


#' Parse JSON Claims Response into Structured Dataframe
#'
#' @param raw_json Character string containing GPT JSON response
#' @param paper_id Optional paper identifier tag
#' @return Data frame with all claim fields
#' @keywords internal
parse_claims_json <- function(raw_json, paper_id = NULL) {

  # Strip markdown code fences if GPT included them anyway
  raw_json <- gsub("^```(?:json)?\\s*", "", raw_json, perl = TRUE)
  raw_json <- gsub("\\s*```$", "", raw_json, perl = TRUE)
  raw_json <- trimws(raw_json)

  parsed <- tryCatch(
    jsonlite::fromJSON(raw_json, simplifyVector = FALSE),
    error = function(e) {
      stop(sprintf("Failed to parse GPT JSON response: %s\nRaw response:\n%s",
                   conditionMessage(e), substr(raw_json, 1, 500)))
    }
  )

  claims_raw <- parsed$claims
  if (is.null(claims_raw) || length(claims_raw) == 0) {
    return(.empty_claims_df())
  }

  claims_list <- lapply(claims_raw, function(cl) {
    data.frame(
      paper_id       = if (!is.null(paper_id)) paper_id else NA_character_,
      verbatim_quote = .coalesce_str(cl$verbatim_quote),
      source         = .coalesce_str(cl$source),
      target         = .coalesce_str(cl$target),
      claim          = .coalesce_str(cl$claim),
      claim_type     = .coalesce_str(cl$claim_type),
      claim_class    = .coalesce_str(cl$claim_class),
      causal_trigger = isTRUE(cl$causal_trigger),
      policy_trigger = isTRUE(cl$policy_trigger),
      claim_scope    = .coalesce_str(cl$claim_scope),
      confidence     = .coalesce_str(cl$confidence),
      effect_size    = .coalesce_str(cl$effect_size),
      p_value        = .coalesce_str(cl$p_value),
      sample_size    = .coalesce_str(cl$sample_size),
      pathway        = .coalesce_str(cl$pathway),
      established    = isTRUE(cl$established),
      evidence       = .coalesce_str(cl$evidence),
      notes          = .coalesce_str(cl$notes),
      stringsAsFactors = FALSE
    )
  })

  # Drop rows missing both source and target
  claims_list <- Filter(function(x) {
    !is.na(x$source) && x$source != "" &&
    !is.na(x$target) && x$target != ""
  }, claims_list)

  if (length(claims_list) == 0) return(.empty_claims_df())

  df <- do.call(rbind, claims_list)
  rownames(df) <- NULL

  # Normalize controlled vocabularies
  df$confidence  <- .normalize(df$confidence,  c("high","medium","low"),   default = "low")
  df$claim_type  <- .normalize(df$claim_type,
    c("causal_effect","mechanism","mediation","interaction","moderation","association"),
    default = "association")
  df$claim_class <- .normalize(df$claim_class, c("A","B","C","D"), default = "B")
  df$pathway     <- .normalize(df$pathway,
    c("gateway","common_liability","structural","behavioral","physiological","unknown"),
    default = "unknown")
  df$claim_scope <- .normalize(df$claim_scope,
    c("overall_exposure","component_or_subtype","policy_implication",
      "recommendation","mechanism_or_speculation"),
    default = "overall_exposure")

  df
}


# ---- Internal helpers -------------------------------------------------------

.empty_claims_df <- function() {
  data.frame(
    paper_id       = character(),
    verbatim_quote = character(),
    source         = character(),
    target         = character(),
    claim          = character(),
    claim_type     = character(),
    claim_class    = character(),
    causal_trigger = logical(),
    policy_trigger = logical(),
    claim_scope    = character(),
    confidence     = character(),
    effect_size    = character(),
    p_value        = character(),
    sample_size    = character(),
    pathway        = character(),
    established    = logical(),
    evidence       = character(),
    notes          = character(),
    quote_verified = logical(),
    quote_verification = character(),
    stringsAsFactors = FALSE
  )
}

.coalesce_str <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  x <- as.character(x[[1]])
  if (is.na(x) || x == "") NA_character_ else x
}

.normalize <- function(x, valid, default = valid[1]) {
  x <- tolower(trimws(x))
  x[!x %in% tolower(valid)] <- default
  x
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x


# ---- Multi-paper extraction -------------------------------------------------

#' Extract Claims from Multiple Papers
#'
#' Runs cauda.extract() on a list of PDFs or texts and combines results
#' into a single dataframe with paper_id tracking on every claim.
#'
#' @param papers Named list where names are paper IDs and values are either:
#'   - Character string of paper text, OR
#'   - File path to a PDF (if is_pdf = TRUE)
#' @param is_pdf Logical. If TRUE, treat values as PDF file paths. Default: FALSE
#' @param ... Additional arguments passed to cauda.extract()
#'
#' @return Combined data frame with all claims tagged by paper_id
#'
#' @examples
#' \dontrun{
#'   papers <- list(
#'     "prescott2024" = "~/papers/prescott2024.pdf",
#'     "jones2022"    = "~/papers/jones2022.pdf"
#'   )
#'   all_claims <- cauda.extract_multi(papers, is_pdf = TRUE)
#' }
#'
#' @export
cauda.extract_multi <- function(papers, is_pdf = FALSE, ...) {

  if (!is.list(papers) || length(papers) == 0)
    stop("papers must be a non-empty named list")

  if (is.null(names(papers)) || any(names(papers) == ""))
    stop("All papers must have names (used as paper_id). ",
         "Use: list(paper1 = ..., paper2 = ...)")

  results <- vector("list", length(papers))

  for (i in seq_along(papers)) {
    pid   <- names(papers)[i]
    input <- papers[[i]]

    message(sprintf("[%d/%d] Extracting from: %s", i, length(papers), pid))

    tryCatch({
      if (is_pdf) {
        if (!file.exists(input))
          stop(sprintf("PDF not found: %s", input))
        if (!requireNamespace("pdftools", quietly = TRUE))
          stop("pdftools required. install.packages('pdftools')")
        pages <- pdftools::pdf_text(input)
        text  <- paste(pages, collapse = "\n")
      } else {
        text <- input
      }

      df <- cauda.extract(text, paper_id = pid, ...)
      results[[i]] <- df

    }, error = function(e) {
      message(sprintf("  WARNING: Failed for '%s': %s", pid, conditionMessage(e)))
      results[[i]] <<- .empty_claims_df()
    })
  }

  combined <- do.call(rbind, Filter(function(x) nrow(x) > 0, results))
  if (is.null(combined) || nrow(combined) == 0) {
    message("No claims extracted from any paper.")
    return(.empty_claims_df())
  }

  rownames(combined) <- NULL
  message(sprintf("Done. Extracted %d total claims from %d papers.",
                  nrow(combined), length(papers)))
  combined
}


#' Extract Causal Claims from a PDF File
#'
#' Convenience wrapper: reads a PDF and calls cauda.extract().
#'
#' @param pdf_path Path to PDF file
#' @param paper_id Optional ID tag for this paper
#' @param ... Additional arguments passed to cauda.extract()
#' @return Data frame with extracted claims
#'
#' @export
#' @importFrom pdftools pdf_text
cauda.extract_pdf <- function(pdf_path, paper_id = NULL, ...) {

  if (!file.exists(pdf_path))
    stop("PDF file not found: ", pdf_path)

  if (!requireNamespace("pdftools", quietly = TRUE))
    stop("pdftools required. Install with: install.packages('pdftools')")

  pages     <- pdftools::pdf_text(pdf_path)
  full_text <- paste(pages, collapse = "\n")

  if (nchar(full_text) == 0)
    stop("Could not extract text from PDF: ", pdf_path)

  if (is.null(paper_id))
    paper_id <- tools::file_path_sans_ext(basename(pdf_path))

  cauda.extract(full_text, paper_id = paper_id, ...)
}
