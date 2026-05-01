# NYC Middle-Wage Labor Market Analysis (2015–2023)

**Tracking the structural reshaping of middle-wage, middle-skill employment in the New York–Newark–Jersey City metro area.**

-----

## Overview

This project identifies, classifies, and tracks middle-wage occupations in the NYC metropolitan area between 2015 and 2023 using a dual-threshold framework: an occupation must fall within an employment-weighted wage band and be predominantly accessible without a four-year degree to qualify as middle-wage. It then maps those occupation-level classifications onto individual workers in the American Community Survey (ACS) to characterize the demographic and structural composition of the middle-wage workforce.

The analysis answers three core questions:

1. Which occupations held middle-wage status in both years, which exited, and which entered?
2. What structural forces (e.g. automation, minimum wage policy, credential inflation, wage erosion) drove those transitions?
3. Who works in middle-wage occupations, and how did that workforce change in race/ethnicity, nativity, education, and income?

-----

## Data Sources

| Dataset | Years | Source | Use |
|---|---|---|---|
| OEWS (Occupational Employment and Wage Statistics) | 2015, 2023 | BLS | Occupation-level median wages and employment counts for the NYC MSA |
| O\*NET Education, Training & Experience | 2015, 2023 | O\*NET Resource Center | Education requirement distributions by occupation (element 2.D.1) |
| O\*NET Skills | 2015, 2023 | O\*NET Resource Center | Skill importance scores aggregated into five skill clusters |
| SOC 2010–2018 Crosswalk | — | BLS | Bridges occupation codes across the 2018 SOC redesign |
| NEM-to-ACS Crosswalk | — | BLS National Employment Matrix | Maps SOC codes to Census OCCP codes |
| Census 2010–2018 OCCP Crosswalk | — | U.S. Census Bureau | Bridges Census occupation codes across the 2018 redesign |
| ACS PUMS (1-year) | 2015, 2023 | U.S. Census Bureau / IPUMS | Person-level employment, wages, demographics for NYC MSA workers |
| CPI-U (Annual Average) | 2015, 2023 | BLS | Deflates 2015 nominal wages to 2023 dollars |

-----

## Methodology

### Middle-Wage Definition

An occupation is classified as **middle-wage** if it satisfies both conditions simultaneously:

**1. Wage threshold:**
The occupation's annual median wage (from OEWS) falls within a band defined relative to the NYC employment-weighted median wage:
- Lower bound: 67% of the weighted median
- Upper bound: 200% of the weighted median

The band is re-derived independently for each year using that year's wage structure, so it reflects the actual middle of the NYC workforce rather than an inflation-adjusted fixed range.

**2. Skill/credential threshold**
The occupation is classified as *middle-skill* — the majority of workers in that role do not require a four-year degree, as measured by O\*NET element 2.D.1 (Required Level of Education). Categories 1–5 (sub-baccalaureate) are summed and compared to categories 6–12 (BA+). An occupation is middle-skill if `Sub_BA > BA_Plus`.

Missing O\*NET education data is conservatively recoded to `FALSE` (not middle-skill).

### ACS Integration

ACS PUMS occupation codes (OCCP) are mapped to OEWS middle-wage flags via a three-step bridge:

1. **2023 ACS**: OCCP (2018 Census) → SOC 2018 (via NEM-ACS crosswalk) → `occ_id_2023`
2. **2015 ACS**: OCCP (2010 Census) → OCCP 2018 (via Census 2010–2018 OCCP crosswalk) → SOC 2018 (via NEM-ACS crosswalk) → SOC 2010 (via reverse SOC bridge) → `occ_id_2015`

Where one OCCP code contains multiple SOC codes, the middle-wage flag is assigned by **employment-weighted majority**: an OCCP is flagged middle-wage if ≥60% of OEWS employment in that group falls in middle-wage SOC codes.
