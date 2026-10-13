# Inputs for the rival-DAG application (retrieved 2026-10-12/13)

- `wonder_T401_2015_2017.csv`: CDC WONDER, Multiple Cause of Death 1999-2020 (D77). Multiple cause T40.1; underlying cause drug poisoning (X40-X44, X60-X64, X85, Y10-Y14); years 2015-2017; grouped by state. Total 43,940. Suppressed values not shown; no state count below 16.
- `wonder_T404_2018_2022.csv`: CDC WONDER, Multiple Cause of Death 2018-2024 (D157). Multiple cause T40.4; same underlying causes; years 2018-2022; grouped by state. Total 268,649.
- `saipe_poverty_2006_2010_2014.csv`: Census SAIPE state estimates, poverty percent all ages (est06us, est10us, est14us); `poverty_mean` is the mean of the three. Raw files in `saipe_raw/`.
- `pop2010.csv`: 2010 Census resident population by state; transcribed from a secondary tabulation, national total checked against 308,745,538.
- ARCOS pills: `inst/extdata/opioid_real_data.csv`.

WONDER data are subject to the NCHS data use restrictions (no presentation of counts of 9 or fewer).
