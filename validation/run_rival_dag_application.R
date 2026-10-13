# Prespecified application: four rival DAGs for prescription-opioid supply (P),
# heroin deaths (H) and synthetic-opioid deaths (S), with background causes
# L (log 2010 population) and C (mean SAIPE poverty). Follows
# validation/protocol_rival_dags.md. Run from the repository root.

options(stringsAsFactors = FALSE)
suppressWarnings({
  if (requireNamespace("cauda", quietly = TRUE)) library(cauda) else
    for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)
})
source("tests/testthat/helper-claims.R", local = TRUE)  # make_claims_df schema helper

# ---- data ------------------------------------------------------------------
d <- "validation/data/"
h <- read.csv(paste0(d, "wonder_T401_2015_2017.csv"))
s <- read.csv(paste0(d, "wonder_T404_2018_2022.csv"))
pv <- read.csv(paste0(d, "saipe_poverty_2006_2010_2014.csv"))
pp <- read.csv(paste0(d, "pop2010.csv"))
ar <- read.csv("inst/extdata/opioid_real_data.csv")
ar <- unique(ar[, c("state_name", "arcos_total_pills_2006_2014")])
names(ar) <- c("state", "pills")
dat <- Reduce(function(a, b) merge(a, b, by = "state", all = TRUE),
              list(h, s, pv[, c("state", "poverty_mean")], pp, ar))
dat$P <- log(dat$pills); dat$H <- log(dat$deaths_T401_2015_2017)
dat$S <- log(dat$deaths_T404_2018_2022); dat$L <- log(dat$pop2010)
dat$C <- dat$poverty_mean
cat("Rows before exclusions:", nrow(dat), "\n")
excl <- dat$state[!complete.cases(dat[, c("P", "H", "S", "L", "C")])]
cat("Excluded (missing or suppressed):", if (length(excl)) excl else "none", "\n")
dat <- dat[complete.cases(dat[, c("P", "H", "S", "L", "C")]), ]
cat("Analysis n =", nrow(dat), "\n")

region <- c(
  Northeast = "Connecticut|Maine|Massachusetts|New Hampshire|Rhode Island|Vermont|New Jersey|New York|Pennsylvania",
  Midwest = "Illinois|Indiana|Michigan|Ohio|Wisconsin|Iowa|Kansas|Minnesota|Missouri|Nebraska|North Dakota|South Dakota",
  South = "Delaware|District of Columbia|Florida|Georgia|Maryland|North Carolina|South Carolina|Virginia|West Virginia|Alabama|Kentucky|Mississippi|Tennessee|Arkansas|Louisiana|Oklahoma|Texas")
dat$region <- "West"
for (r in names(region)) dat$region[grepl(paste0("^(", region[r], ")$"), dat$state)] <- r
dat$region <- factor(dat$region)

# ---- graphs (protocol_rival_dags_mapping.md) -------------------------------
bg <- data.frame(source = rep(c("L", "C"), each = 3), target = rep(c("P", "H", "S"), 2))
mk <- function(extra) {
  e <- rbind(bg, extra)
  cauda.claims_to_dag(make_claims_df(e$source, e$target),
                      canonicalize_nodes = FALSE, verbose = FALSE)
}
dags <- list(
  G1_substitution = mk(data.frame(source = c("P", "H"), target = c("H", "S"))),
  G2_independent_fentanyl = mk(data.frame(source = "P", target = "H")),
  G3_background_only = mk(data.frame(source = character(0), target = character(0))),
  G4_direct = mk(data.frame(source = c("P", "P"), target = c("H", "S")))
)
cmp <- cauda.dagitty_compare(dags, verbose = TRUE)
saveRDS(cmp, "validation/output/rival_dag_compare.rds")

# ---- unique tests and entailment by d-separation ---------------------------
ai <- cmp$all_implications
zset <- function(z) { if (length(z) == 0 || is.na(z)) return(character(0)); z <- trimws(unlist(strsplit(as.character(z), ","))); z[nzchar(z) & !is.na(z) & z != "(nothing)"] }
key <- function(x, y, z) paste(paste(sort(c(x, y)), collapse = "~"), "|", paste(sort(z), collapse = ","))
tests <- list()
for (i in seq_len(nrow(ai))) {
  x <- ai$X[i]; y <- ai$Y[i]; z <- zset(ai$Z[i]); k <- key(x, y, z)
  if (is.null(tests[[k]])) tests[[k]] <- list(x = x, y = y, z = z)
}
tab <- do.call(rbind, lapply(tests, function(t) {
  ent <- vapply(cmp$per_theory, function(r)
    dagitty::dseparated(r$dagitty, t$x, t$y, t$z), logical(1))
  data.frame(X = t$x, Y = t$y, Z = paste(t$z, collapse = ","),
             entailed_by = paste(names(ent)[ent], collapse = ";"),
             n_entail = sum(ent), discriminating = any(!ent) && any(ent))
}))
rownames(tab) <- NULL

pcor_test <- function(df, x, y, z, rank = FALSE, extra = NULL) {
  v <- unique(c(x, y, z)); dd <- df[, v, drop = FALSE]
  if (rank) dd[] <- lapply(dd, rank)
  zz <- c(z, extra)
  rhs <- if (length(zz)) paste(zz, collapse = "+") else "1"
  if (!is.null(extra)) { dd <- cbind(dd, df[, extra, drop = FALSE]) }
  if (rank && !is.null(extra)) dd[extra] <- df[extra]
  rx <- resid(lm(as.formula(paste(x, "~", rhs)), dd))
  ry <- resid(lm(as.formula(paste(y, "~", rhs)), dd))
  r <- cor(rx, ry); k <- length(zz); n <- nrow(dd)
  dfree <- n - 2 - k; tt <- r * sqrt(dfree / (1 - r^2))
  c(r = r, p = 2 * pt(-abs(tt), dfree), n = n, k = k)
}
run <- function(df, rank = FALSE, extra = NULL) {
  out <- t(mapply(function(x, y, z) pcor_test(df, x, y, if (nzchar(z)) strsplit(z, ",")[[1]] else character(0), rank, extra),
                  tab$X, tab$Y, tab$Z))
  as.data.frame(out)
}
prim <- run(dat)
res <- cbind(tab, prim)
fam <- res$discriminating
res$p_holm <- NA_real_; res$p_holm[fam] <- p.adjust(res$p[fam], "holm")
res$reject <- fam & res$p_holm < 0.05
m <- sum(fam); n <- nrow(dat)
mdr <- function(k, alpha) tanh((qnorm(1 - alpha / 2) + qnorm(0.8)) / sqrt(n - k - 3))
res$min_detectable_r_80 <- vapply(res$k, mdr, numeric(1), alpha = 0.05 / m)
cat("\n=== Primary results (discriminating family size m =", m, ", n =", n, ") ===\n")
print(res[, c("X", "Y", "Z", "entailed_by", "discriminating", "r", "p", "p_holm", "reject", "min_detectable_r_80")],
      row.names = FALSE, digits = 3)

verdict <- function(res, rej_col = "reject") {
  vapply(names(dags), function(g) {
    inset <- res$discriminating & grepl(g, res$entailed_by, fixed = TRUE)
    if (!any(inset)) return("no testable discriminating implication")
    if (any(res[[rej_col]][inset])) "contradicted" else "not contradicted"
  }, character(1))
}
vd <- verdict(res)
cat("\n=== Verdicts (primary) ===\n"); print(vd)

# ---- sensitivity (prespecified) --------------------------------------------
sens <- function(df, rank = FALSE, extra = NULL, label) {
  r <- cbind(tab, run(df, rank, extra))
  r$p_holm <- NA_real_; r$p_holm[fam] <- p.adjust(r$p[fam], "holm")
  r$reject <- fam & r$p_holm < 0.05
  data.frame(variant = label, t(verdict(r)), n = nrow(df), check.names = FALSE)
}
big3 <- dat$state[order(-dat$pop2010)][1:3]
sv <- rbind(
  data.frame(variant = "primary", t(vd), n = n, check.names = FALSE),
  sens(dat, TRUE, label = "Spearman (ranks)"),
  sens(dat, FALSE, "region", label = "plus Census-region indicators"),
  sens(dat[!dat$state %in% big3, ], label = paste("drop", paste(big3, collapse = ", ")))
)
loo <- do.call(rbind, lapply(dat$state, function(st) sens(dat[dat$state != st, ], label = paste("LOO", st))))
flips <- apply(loo[, names(dags)], 1, function(v) any(v != vd))
cat("\n=== Sensitivity verdicts ===\n"); print(sv, row.names = FALSE)
cat("\nLeave-one-state-out: verdict changed in", sum(flips), "of", nrow(loo), "fits",
    if (any(flips)) paste0("(", paste(sub("LOO ", "", loo$variant[flips]), collapse = ", "), ")") else "", "\n")

# Shared (non-discriminating) implications, reported but outside the Holm family
cat("\n=== Shared implications (entailed by all graphs; not in the family) ===\n")
print(res[!res$discriminating, c("X", "Y", "Z", "r", "p")], row.names = FALSE, digits = 3)

write.csv(res, "validation/output/rival_dag_results.csv", row.names = FALSE)
write.csv(rbind(sv, loo), "validation/output/rival_dag_sensitivity.csv", row.names = FALSE)
write.csv(dat[, c("state", "P", "H", "S", "L", "C", "region")], "validation/output/rival_dag_analysis_data.csv", row.names = FALSE)
cat("\nWrote validation/output/rival_dag_*.csv\n")
