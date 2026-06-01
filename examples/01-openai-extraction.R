#' Example: Extract Causal Claims Using OpenAI API
#'
#' This example shows how to use the OpenAI integration to extract
#' causal claims from academic papers.
#'

library(cauda)

# ============================================================
# Step 1: Set up your OpenAI API key
# ============================================================

# Option A: Set temporarily in this session
Sys.setenv(OPENAI_API_KEY = "sk-your-api-key-here")

# Option B: Set permanently in ~/.Renviron file (recommended)
# Add this line to ~/.Renviron:
# OPENAI_API_KEY=sk-your-actual-key
# Then restart R

# ============================================================
# Step 2: Extract causal claims from sample text
# ============================================================

sample_paper <- "
Economic stress significantly increases addiction risk. Research shows that
individuals facing financial hardship and unemployment have higher rates of
substance abuse. Job loss directly leads to depression, which in turn
contributes to increased drug usage. Poverty also limits access to treatment,
making addiction more severe and persistent.
"

cat("Input text:\n")
cat(sample_paper)
cat("\n\nExtracting causal claims...\n\n")

# Extract claims (requires OPENAI_API_KEY to be set)
claims <- extract_causal_claims(sample_paper)

cat("Extracted claims:\n")
cat(claims)

# ============================================================
# Step 3: Parse and integrate with cauda functions
# ============================================================

# Manually create a structured format from the extracted claims
claims_df <- data.frame(
  source = c("Economic stress", "Job loss", "Poverty"),
  target = c("Addiction risk", "Depression", "Treatment access"),
  type = c("causal", "causal", "causal"),
  confidence = c("high", "high", "high")
)

cat("\n\nCausal relationships extracted:\n")
print(claims_df)

# Build DAG from claims
dag <- cauda.claims_to_dag(claims_df)

cat("\n\nDAG structure created. You can now:\n")
cat("- Visualize: plot(dag)\n")
cat("- Analyze: cauda.dag_theory(dag)\n")
cat("- Combine with data analysis\n")

# ============================================================
# Step 4: Use with multiple papers
# ============================================================

# Process multiple papers
papers <- list(
  list(
    title = "Paper 1",
    text = "Economic stress increases depression. Depression increases anxiety.",
    domain = "psychology"
  ),
  list(
    title = "Paper 2",
    text = "Unemployment leads to financial hardship. Financial hardship increases stress.",
    domain = "economics"
  )
)

cat("\n\nExtracting from multiple papers...\n")

all_claims <- list()
for (i in seq_along(papers)) {
  paper <- papers[[i]]
  cat(sprintf("Processing %s...\n", paper$title))
  claims <- extract_causal_claims(paper$text)
  all_claims[[paper$title]] <- claims
}

cat("\nAll claims extracted!\n")
str(all_claims)

# ============================================================
# Step 5: Cost estimation
# ============================================================

cat("\n\nCost estimation:\n")
cat("- Model: gpt-3.5-turbo\n")
cat("- Cost per 100 papers: ~$0.23\n")
cat("- Perfect for bulk research analysis\n")
