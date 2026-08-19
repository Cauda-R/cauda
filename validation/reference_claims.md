# Independent Reference Claim Set (Author-Constructed, Single-Coder)

Built by direct reading of the four real source papers in `opioid_papers/`, with NO reference to `claims.json` (which was found to describe a generic/public-health "three waves" narrative that does not match the actual content of these papers — most notably Theory D, which the old file describes as heroin/counterfeit-pill fentanyl supply contamination, when the actual paper (Moore, Olney & Hansen) is about international trade/import volumes as the smuggling channel). This reference set is the gold standard for Section 4 validation, disclosed in the paper as author-constructed via careful reading (single-coder limitation).

## Theory A — Alpert, Evans, Lieber & Powell (2019), "Origins of the Opioid Crisis and Its Enduring Impacts," NBER WP 26500

1. Triplicate prescription-monitoring laws → OxyContin distribution (negative: triplicate programs reduced OxyContin marketing effectiveness/distribution in adopting states)
2. OxyContin distribution → overdose deaths (positive, direct; ~36–44% fewer overdose deaths in triplicate states vs. non-triplicate states as of ~2010)
3. Early (1996–2000) OxyContin distribution exposure → later-era (heroin/fentanyl-phase) overdose deaths (persistence effect — durable stock of dependent individuals carries risk forward in time)

## Theory B — Case & Deaton (2017), "Mortality and Morbidity in the 21st Century," Brookings Papers on Economic Activity

1. Declining labor-market opportunity (deindustrialization) → cumulative disadvantage (across labor market, marriage, health, across successive birth cohorts)
2. Cumulative disadvantage → chronic pain / disability
3. Chronic pain / disability → prescription opioid exposure
4. Cumulative disadvantage ("despair") → deaths of despair (overdose + suicide + alcoholic liver disease, treated jointly), with opioid supply framed explicitly as an amplifier/moderator of an underlying despair-driven mortality trend, not the fundamental cause

## Theory C — Alpert, Powell & Pacula (2017), "Supply-Side Drug Policy in the Presence of Substitutes," NBER WP 23031

1. Abuse-deterrent OxyContin reformulation (2010) → OxyContin misuse (negative: reformulation reduced misuse)
2. Reduction in OxyContin misuse → heroin deaths (positive, substitution; concentrated in states with high pre-2010 OxyContin misuse; explains the substantial majority of the 2010–2013 rise in heroin overdose deaths, on the order of 80%)
3. Reduction in OxyContin misuse → synthetic-opioid (fentanyl) deaths (positive, suggestive substitution)
4. Heroin + fentanyl substitution → little or no net reduction in total opioid-related mortality (offsetting effect)

## Theory D — Moore, Olney & Hansen (2023, rev. 2024), "Importing the Opioid Crisis? International Trade and Fentanyl Overdoses," NBER WP 31885

1. State-level international imports (legal trade volume) → fentanyl smuggling into the illicit drug supply (imports serve as a channel/proxy for smuggling capacity)
2. Fentanyl smuggling / import exposure → fentanyl overdose deaths (positive, direct; attributable to roughly 14,000–20,000 deaths/year)
3. Discrimination claim: this import → fentanyl-death relationship is NOT explained by (i.e., robust to controls for) deaths-of-despair proxies, general opioid demand, import competition/manufacturing job loss — an explicit test against Theory B's mechanism

## Notes on divergence from `claims.json`

- `claims.json` cites general public-facing sources (addicted.org, CBS News, ethicsunwrapped.utexas.edu, STAT News, DEA press releases, "CDC three-waves framework") rather than the specific NBER/Brookings papers in `opioid_papers/`.
- Its Theory D ("fentanyl_supply_contamination") describes heroin dealers cutting product with illicit fentanyl — a real and well-documented phenomenon in the broader literature, but not what Moore, Olney & Hansen (2023/2024) argue; their paper is about legal international trade volume as a proxy for smuggling capacity, tested against Census import data.
- This confirms `claims.json` cannot honestly be presented as CAUDA's automated extraction output for these four specific source PDFs — it appears to be an earlier, independently hand-authored stand-in describing the general opioid-crisis narrative rather than live pipeline output on this paper set.
