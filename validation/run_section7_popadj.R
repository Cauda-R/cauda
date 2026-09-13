## Population-adjusted sensitivity analysis for Section 7 of CAUDA_paper.tex
## Real 2020 U.S. Census state population counts, used only to convert the
## Section 7 raw-count analysis into per-100,000-population rates as a
## sensitivity check against the population-scale confound.

setwd("~/Downloads/Cauda/git-ready")
library(cauda); library(dagitty); library(jsonlite)

pop2020 <- c(AL=5024279,AK=733391,AZ=7151502,AR=3011524,CA=39538223,CO=5773714,
  CT=3605944,DE=989948,DC=689545,FL=21538187,GA=10711908,HI=1455271,ID=1839106,
  IL=12812508,IN=6785528,IA=3190369,KS=2937880,KY=4505836,LA=4657757,ME=1362359,
  MD=6177224,MA=7029917,MI=10077331,MN=5706494,MS=2961279,MO=6154913,MT=1084225,
  NE=1961504,NV=3104614,NH=1377529,NJ=9288994,NM=2117522,NY=20201249,NC=10439388,
  ND=779094,OH=11799448,OK=3959353,OR=4237256,PA=13002700,RI=1097379,SC=5118425,
  SD=886667,TN=6910840,TX=29145505,UT=3271616,VT=643077,VA=8631393,WA=7705281,
  WV=1793716,WI=5893718,WY=576851)

d <- read.csv("inst/extdata/opioid_real_data.csv")

agg <- aggregate(cbind(overdose_deaths_total,heroin_deaths,natural_semisynth_deaths,
  synthetic_opioid_deaths) ~ state, data = d, FUN = function(x) mean(x, na.rm = TRUE))
pills <- aggregate(arcos_total_pills_2006_2014 ~ state, data = d,
  FUN = function(x) mean(x, na.rm = TRUE))
agg <- merge(agg, pills, by = "state")
agg$pop <- pop2020[agg$state]
stopifnot(sum(is.na(agg$pop)) == 0, nrow(agg) == 48)

agg$heroin_rate  <- agg$heroin_deaths / agg$pop * 1e5
agg$natsemi_rate <- agg$natural_semisynth_deaths / agg$pop * 1e5
agg$synth_rate   <- agg$synthetic_opioid_deaths / agg$pop * 1e5
agg$pills_rate   <- agg$arcos_total_pills_2006_2014 / agg$pop

pcor_test <- function(x, y, z, data) {
  rx <- residuals(lm(data[[x]] ~ data[[z]]))
  ry <- residuals(lm(data[[y]] ~ data[[z]]))
  cor.test(rx, ry)
}

rr1 <- pcor_test("heroin_rate", "natsemi_rate", "pills_rate", agg)
rr2 <- cor.test(agg$synth_rate, agg$heroin_rate)
rr3 <- cor.test(agg$synth_rate, agg$natsemi_rate)
rr4 <- cor.test(agg$synth_rate, agg$pills_rate)
runc <- cor.test(agg$heroin_rate, agg$natsemi_rate)

out <- c(
  sprintf("n states = %d", nrow(agg)),
  sprintf("rr1 partial (heroin_rate ~ natsemi_rate | pills_rate): r=%.4f t=%.4f df=%d p=%.5f",
          rr1$estimate, rr1$statistic, rr1$parameter, rr1$p.value),
  sprintf("runc unconditional (heroin_rate ~ natsemi_rate): r=%.4f p=%.5g",
          runc$estimate, runc$p.value),
  sprintf("rr2 (synth_rate ~ heroin_rate): r=%.4f p=%.5g", rr2$estimate, rr2$p.value),
  sprintf("rr3 (synth_rate ~ natsemi_rate): r=%.4f p=%.5g", rr3$estimate, rr3$p.value),
  sprintf("rr4 (synth_rate ~ pills_rate): r=%.4f p=%.5g", rr4$estimate, rr4$p.value)
)
writeLines(out)
writeLines(out, "validation/output/section7_popadjusted_results.txt")
saveRDS(list(agg=agg, rr1=rr1, rr2=rr2, rr3=rr3, rr4=rr4, runc=runc),
        "validation/output/section7_popadjusted_results.rds")
cat("\nDONE\n")
