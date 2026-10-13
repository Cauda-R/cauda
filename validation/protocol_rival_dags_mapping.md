# Claim-to-edge mapping for the rival DAGs (written before analysis)

Row numbers are 0-based positions in `validation/output/real_extraction_output.json` (the unedited CAUDA extraction of the four opioid papers). The mapping from a claim's own wording to the measured variables P, H, S is an analyst judgment, not an automated step.

| Graph | Source paper | Extracted claim (row) | Edge used |
|---|---|---|---|
| G1 | Alpert et al. 2019 (A) | triplicate prescription programs -> OxyContin distribution (0) | P is the exposure measuring distribution |
| G1 | Alpert, Powell, Pacula 2017 (C) | reformulation of OxyContin -> heroin deaths (13); supply reduction in abusable OxyContin -> substitution to heroin (17) | P -> H |
| G1 | Moore, Olney, Hansen 2023 (D) | fentanyl smuggling via imports -> opioid problems (21), used only for the H -> S step | H -> S (analyst assumption: heroin and fentanyl markets overlap; not stated by the sources) |
| G2 | Moore, Olney, Hansen 2023 (D) | fentanyl smuggling via imports -> opioid problems (21); import-volume claims typed as associations are excluded (19, 20, 23) | no P -> S and no H -> S edge; S is driven by a separate supply channel |
| G2 | Alpert, Powell, Pacula 2017 (C) | 13, 17 | P -> H |
| G3 | Case and Deaton 2017 (B) | accumulation of disadvantage -> mortality increase (10); labor-market and social policies -> cumulative disadvantage (12); overprescription of opioids -> mortality crisis, typed as association (11) | no edges among P, H, S; C (poverty) and L are the common causes |
| G4 | Alpert et al. 2019 (A) | OxyContin introduction and marketing -> overdose deaths (3); OxyContin exposure -> opioid dependence and illicit opioid use (4) | P -> H and P -> S |

Background edges in every graph: L -> P, H, S and C -> P, H, S. These are analyst assumptions, stated in the protocol.
