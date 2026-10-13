# Pre-analysis protocol: rival causal DAGs for prescription-opioid supply and synthetic-opioid mortality

Status: written before any data for the primary analysis were pulled or inspected.
Authors: A. Soni, L. A. Cox Jr.
Protocol date: see the git commit date of this file.

## Disclosure of prior exposure to related data

The authors had earlier analyzed a different construction of related data (CDC VSRR state-year counts, mean over years with all counts reported, ARCOS 2006-2014 pill totals) and saw its correlations, including sensitivity analyses. The primary data in this protocol (pooled-window counts from CDC WONDER, Census population, SAIPE poverty) had not been pulled or viewed when this protocol was written. This is therefore a prespecified analysis, not a blind one, and it will be described that way.

## Question

Is synthetic-opioid mortality in the fentanyl era related to earlier prescription-opioid supply through heroin (substitution), directly, not at all beyond shared background causes, or independently of both? The answer bears on which supply-side measures are expected to affect synthetic-opioid deaths.

## Units and variables (50 states and DC)

- L: log of 2010 Census resident population.
- C: mean of the SAIPE all-ages poverty rate (percent) for 2006, 2010, and 2014.
- P: log of the DEA ARCOS total opioid pills distributed to the state, 2006-2014 (existing file).
- H: log of heroin-involved drug-poisoning deaths (multiple cause T40.1; underlying cause X40-X44, X60-X64, X85, Y10-Y14), pooled over 2015-2017, CDC WONDER.
- S: log of synthetic-opioid-involved drug-poisoning deaths (multiple cause T40.4; same underlying causes), pooled over 2018-2022, CDC WONDER.

Time order is P (2006-2014), then H (2015-2017), then S (2018-2022). The 2010 OxyContin reformulation falls inside the P window; this is a stated limitation, not a design feature.

States whose pooled count is suppressed (fewer than 10 deaths) for H or S are excluded from the primary analysis; the number and identity of excluded states will be reported.

## Rival graphs

L and C are measured background causes of P, H, and S in every graph (L -> P, H, S; C -> P, H, S). The graphs differ only in the claims about P, H, and S, which come from the CAUDA extraction of the four source papers.

- G1, substitution chain (Alpert et al. 2019; Alpert, Powell, Pacula 2017): P -> H -> S.
- G2, independent fentanyl supply shock (Moore, Olney, Hansen 2023): P -> H; no edge into S from P or H.
- G3, shared background only (Case and Deaton 2017): no edges among P, H, S.
- G4, direct prescription effect (Alpert et al. 2019): P -> H and P -> S.

The claim-to-edge mapping, with extracted-claim identifiers, is recorded in `validation/protocol_rival_dags_mapping.md` before the analysis.

## Procedure

1. Build the four graphs in CAUDA and run `cauda.dagitty_compare()`. Implications are compared by d-separation in each rival; the discriminating set is the one it returns.
2. Test every discriminating implication X _||_ Y | Z with the partial correlation of the (log) variables, using ordinary least squares residuals and a t-test with n - 2 - k degrees of freedom, where k is the size of Z.
3. Control the family-wise error rate with the Holm procedure at 0.05 across all discriminating implications tested.
4. A graph is "contradicted" if any of its discriminating implications is rejected after Holm. A graph that is not contradicted is reported as not contradicted, never as supported. Graphs that remain uncontradicted are reported as not separated by these data.
5. Report, for the achieved n, the smallest absolute partial correlation detectable with 80 percent power at the Holm-adjusted threshold.

## Prespecified sensitivity analyses

Spearman (rank) versions of every test; leave-one-state-out ranges and verdict changes; adding four Census-region indicators to every conditioning set; dropping the three most populous states. These are reported alongside the primary results, whatever they show.

## Interpretation rules

A rejected implication criticizes the graph as specified, not any single edge. A non-rejection does not confirm a graph. Linear partial correlations on log scale assume approximately linear, homoscedastic relations; ecological aggregation, spatial dependence, and measurement error are not modeled and will be listed as limitations.

## Decision relevance (stated in advance)

G1 implies that restricting prescription supply would reduce synthetic-opioid mortality only to the extent it reduces heroin involvement. G2 and G3 imply no such effect beyond background causes. G4 implies a direct effect. The tests cannot estimate effect sizes; their output is the set of graphs the data contradict, which narrows the structural uncertainty behind supply-side recommendations.

## Deviations

Any deviation from this protocol will be listed in the paper with the reason.
