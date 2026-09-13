## Full statistical detail (test name, t, df, exact CI) for Table 4 of
## Section 7, both raw-count and population-adjusted specifications.
## Uses `agg`, already built by run_section7_popadj.R in this R session
## (48 states, raw counts and per-capita rates both present as columns).

setwd("~/Downloads/Cauda/git-ready")

pcor_test <- function(x, y, z, data) {
  rx <- residuals(lm(data[[x]] ~ data[[z]]))
  ry <- residuals(lm(data[[y]] ~ data[[z]]))
  cor.test(rx, ry)
}

fmt <- function(label, ct) {
  ci <- ct$conf.int
  sprintf("%-42s r=%.4f t=%.4f df=%d p=%.5g CI=[%.4f, %.4f]",
          label, unname(ct$estimate), unname(ct$statistic), unname(ct$parameter),
          ct$p.value, ci[1], ci[2])
}

raw_partial <- pcor_test("heroin_deaths", "natural_semisynth_deaths", "arcos_total_pills_2006_2014", agg)
raw_unc     <- cor.test(agg$heroin_deaths, agg$natural_semisynth_deaths)
raw_r2      <- cor.test(agg$synthetic_opioid_deaths, agg$heroin_deaths)
raw_r3      <- cor.test(agg$synthetic_opioid_deaths, agg$natural_semisynth_deaths)
raw_r4      <- cor.test(agg$synthetic_opioid_deaths, agg$arcos_total_pills_2006_2014)

rate_partial <- pcor_test("heroin_rate", "natsemi_rate", "pills_rate", agg)
rate_unc     <- cor.test(agg$heroin_rate, agg$natsemi_rate)
rate_r2      <- cor.test(agg$synth_rate, agg$heroin_rate)
rate_r3      <- cor.test(agg$synth_rate, agg$natsemi_rate)
rate_r4      <- cor.test(agg$synth_rate, agg$pills_rate)

out <- c(
  sprintf("n states = %d, missing-data handling = complete-case (states with any missing year excluded before aggregation)", nrow(agg)),
  "--- RAW COUNTS ---",
  fmt("Heroin|Nat/semi (partial, given Pills)", raw_partial),
  fmt("Heroin~Nat/semi (unconditional)",        raw_unc),
  fmt("Synthetic~Heroin",                        raw_r2),
  fmt("Synthetic~Nat/semi",                      raw_r3),
  fmt("Synthetic~Pills",                         raw_r4),
  "--- PER-CAPITA RATES ---",
  fmt("Heroin|Nat/semi (partial, given Pills)", rate_partial),
  fmt("Heroin~Nat/semi (unconditional)",        rate_unc),
  fmt("Synthetic~Heroin",                        rate_r2),
  fmt("Synthetic~Nat/semi",                      rate_r3),
  fmt("Synthetic~Pills",                         rate_r4)
)
writeLines(out)
writeLines(out, "validation/output/section7_fullstats.txt")
saveRDS(list(raw_partial=raw_partial, raw_unc=raw_unc, raw_r2=raw_r2, raw_r3=raw_r3, raw_r4=raw_r4,
             rate_partial=rate_partial, rate_unc=rate_unc, rate_r2=rate_r2, rate_r3=rate_r3, rate_r4=rate_r4),
        "validation/output/section7_fullstats.rds")
cat("\nDONE\n")
