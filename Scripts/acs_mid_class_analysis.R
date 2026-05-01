# ==============================================================================
# Story 2 — Part 2: ACS Labor Force Classification
# Map ACS PUMS occupation codes to OEWS/ONET middle-wage flags
# ==============================================================================

# ----------- OEWS/ONET Middle-Class Jobs classification -----------
# import the final OEWS/ONET middle-class classification files
occ_id_2015 <- read.csv("occ_id_2015.csv")
occ_id_2023 <- read.csv("occ_id_2023.csv")

# -------------  Setup and Load ---------------
library(tidyverse)
library(spatstat.geom)

# Load ACS Data
acs_data <- readRDS("Z:/Adleh_work/ACS.Labor.Analysis - Copy/Datasets/ACS_raw_2014-2024.rds")

# Filter to civilian employed only (ESR 1 = at work, 2 = with job not at work)
prep_acs <- function(data, target_year) {
  data %>%
    filter(year == target_year, ESR %in% c("1", "2")) %>%
    select(year, SERIALNO, SPORDER, PWGTP, OCCP, AGEP, SEX, RAC1P, HISP, CIT, SCHL, ESR, ADJINC, WAGP) %>% 
    mutate(OCCP = as.numeric(OCCP))}

acs_2015 <- prep_acs(acs_data, 2015)
acs_2023 <- prep_acs(acs_data, 2023)

cat("ACS 2015 persons:", nrow(acs_2015), "\n")
cat("ACS 2023 persons:", nrow(acs_2023), "\n")

# Load NEM-to-ACS crosswalk 2023 
# NEM-to-ACS crosswalk: maps BLS SOC codes (2018 vintage) to Census OCCP codes (2018 vintage).
soc_acs_crosswalk <- readxl::read_xlsx("Z:/Adleh_work/ACS.Labor.Analysis - Copy/Datasets/nem-occcode-acs-crosswalk.xlsx") %>%
  rename(SOC = 2, OCCP = 4) %>%
  mutate(SOC = str_remove_all(str_sub(SOC, 1, 7), "-"),
         OCCP = as.numeric(OCCP))

cat("NEM crosswalk rows:", nrow(soc_acs_crosswalk), "\n")
cat("Unique OCCP codes in NEM crosswalk:", n_distinct(soc_acs_crosswalk$OCCP), "\n")

# ---------- ACS 2023: Map to OEWS Middle-Class Flag ------------
# First create an occupation look-up table

# With SOC to ACS crosswalk, join with the OEWS/ONET file using SOC
pre_agg_2023 <- soc_acs_crosswalk %>%
  rename(soc_2018 = SOC) %>%
  full_join(occ_id_2023,
            by = c("soc_2018" = "SOC6")) # Diagnose HERE

# This part is sensitive -- one OCCP code can contain multiple SOC codes with different MW flags.
# The question is: How do we compress multiple SOCs with different education requirements into one OCCP category?

# I use employment-weighted majority. An OCCP is flagged middle-wage
# if >= 60% of SOC employment in that OCCP group is in middle-wage SOC codes.
# I also take the employment-weighted average of skills then redefine the relative importance based on the new averages
occp_lookup_2023 <- pre_agg_2023 %>% 
  group_by(OCCP) %>%
  summarise(
    valid_soc_count = sum(!is.na(is_middle_wage_occ)), # How many SOCs in this group have a valid wage/edu classification?
    total_oews_emp = sum(TOT_EMP, na.rm = TRUE),
    has_oews_support = total_oews_emp > 0 | valid_soc_count > 0,  # We have support if there's employment OR if we have valid wage/edu flags
    is_middle_class_occp = case_when(
      # If we have employment weights, use the Weighted Majority (Best)
      total_oews_emp > 0 ~ (sum(TOT_EMP[is_middle_wage_occ == TRUE], na.rm = TRUE) / total_oews_emp) >= 0.6,
      # If no weights but we have flags, use simple majority of titles
      valid_soc_count > 0 ~ (sum(is_middle_wage_occ == TRUE, na.rm = TRUE) / valid_soc_count) >= 0.6, TRUE ~ FALSE),
    avg_analytical = if_else(total_oews_emp > 0, weighted.mean(Analytical, TOT_EMP, na.rm = TRUE), mean(Analytical, na.rm = TRUE)),
    avg_fundamental = if_else(total_oews_emp > 0, weighted.mean(Fundamental, TOT_EMP, na.rm = TRUE), mean(Fundamental, na.rm = TRUE)),
    avg_managerial = if_else(total_oews_emp > 0, weighted.mean(Managerial, TOT_EMP, na.rm = TRUE), mean(Managerial, na.rm = TRUE)),
    avg_mechanical = if_else(total_oews_emp > 0, weighted.mean(Mechanical, TOT_EMP, na.rm = TRUE), mean(Mechanical, na.rm = TRUE)),
    avg_social = if_else(total_oews_emp > 0, weighted.mean(Social, TOT_EMP, na.rm = TRUE), mean(Social, na.rm = TRUE)),
    .groups = "drop") %>% 
  mutate(
    Total_Skill_Sum = avg_analytical + avg_fundamental + avg_managerial + avg_mechanical + avg_social,
    Rel_Social = round(avg_social / Total_Skill_Sum, 2),
    Rel_Analytical = round(avg_analytical / Total_Skill_Sum, 2),
    Rel_Fundamental = round(avg_fundamental / Total_Skill_Sum, 2),
    Rel_Managerial = round(avg_managerial / Total_Skill_Sum, 2),
    Rel_Mechanical = round(avg_mechanical / Total_Skill_Sum, 2))

cat("2023 Total OCCP codes:", nrow(occp_lookup_2023), "\n")
cat("2023 OCCP codes with OEWS support:", sum(occp_lookup_2023$has_oews_support, na.rm = TRUE), "\n")
cat("2023 OCCP codes MW=TRUE:", sum(occp_lookup_2023$is_middle_class_occp == TRUE, na.rm = TRUE), "\n")

# Now I join the Occupation look-up table with my ACS 2023 file
acs_2023_final <- acs_2023 %>%
  left_join(occp_lookup_2023, by = "OCCP") %>%
  mutate(
    # true match definition: occupation has OEWS employment figure
    matched_to_oews_2023 = has_oews_support == TRUE,
    # Final MW flag: If no OEWS employment figure -> NA initially (so I can diagnose)
    is_middle_class_occ_2023_final = case_when(
      matched_to_oews_2023 == TRUE ~ is_middle_class_occp, TRUE ~ NA))

# Diagnostics match rates
cat("2023 ACS persons:", nrow(acs_2023_final), "\n")
cat("Unweighted % matched to OEWS support:",
    round(100 * mean(acs_2023_final$matched_to_oews_2023, na.rm = TRUE), 1), "%\n")

cat("Weighted % matched to OEWS support:",
    round(100 * sum(acs_2023_final$PWGTP[acs_2023_final$matched_to_oews_2023 == TRUE], na.rm = TRUE) /
            sum(acs_2023_final$PWGTP, na.rm = TRUE), 1), "%\n")

cat("=== ACS 2023 Coverage ===\n")
acs_2023_final %>%
  summarise(
    total_persons = n(),
    has_oews = sum(matched_to_oews_2023, na.rm = TRUE),
    no_oews = sum(!matched_to_oews_2023, na.rm = TRUE),
    pct_oews_covered = round(100 * sum(PWGTP[matched_to_oews_2023 == TRUE], na.rm = TRUE) / sum(PWGTP), 1),
    mw_true = sum(is_middle_class_occ_2023_final == TRUE,  na.rm = TRUE),
    mw_false = sum(is_middle_class_occ_2023_final == FALSE, na.rm = TRUE),
    mw_na = sum(is.na(is_middle_class_occ_2023_final))) %>%
  print()

# finalize MW flag for analysis (convert NA -> FALSE only at the very end)
acs_2023_final <- acs_2023_final %>%
  mutate(is_middle_class_occ_2023_final = replace_na(is_middle_class_occ_2023_final, FALSE)) %>% 
  select(-valid_soc_count, -total_oews_emp, -has_oews_support, -is_middle_class_occp)

cat("\nACS 2023 final weighted MW share:\n")
cat(round(100 * sum(acs_2023_final$PWGTP[acs_2023_final$is_middle_class_occ_2023_final == TRUE], na.rm = TRUE) /
            sum(acs_2023_final$PWGTP, na.rm = TRUE),1), "%\n")

write.csv(acs_2023_final, "acs_2023_final.csv")

# ------- ACS 2015: Map to OEWS Middle-Wage Flag -----------

# In my final occ_id_2015 file that flags my occupations as middle-class or not, 
# I need to translate its SOC 2010 code to 2018
soc_bridge <- readxl::read_xlsx(
  "Z:/Adleh_work/ACS.Labor.Analysis - Copy/Datasets/soc_2010_to_2018_crosswalk.xlsx") %>%
  rename(soc_2010 = `2010 SOC Code`, title_2010 = `2010 SOC Title`,
         soc_2018 = `2018 SOC Code`, title_2018 = `2018 SOC Title`) %>%
  mutate(soc_2010 = str_remove_all(str_sub(soc_2010, 1, 7), "-"),
         soc_2018 = str_remove_all(str_sub(soc_2018, 1, 7), "-")) %>% 
  select(soc_2010, soc_2018) 

occp_crosswalk <- readxl::read_xlsx(
  "Z:/Adleh_work/ACS.Labor.Analysis - Copy/Datasets/2018-occupation-code-list-and-crosswalk.xlsx",
  sheet = "2010 to 2018 Crosswalk ", skip = 3,
  col_names = c("soc_2010", "occp_2010", "title_2010",
                "soc_2018", "occp_2018", "title_2018")) %>%
  fill(occp_2010, .direction = "down") %>%
  filter(!is.na(occp_2018), !is.na(occp_2010)) %>%
  mutate(occp_2010 = as.numeric(occp_2010),
         occp_2018 = as.numeric(occp_2018)) %>%
  select(occp_2010, occp_2018) %>%
  distinct()

# Clean soc_bridge -- strip # and ## flags from codes/titles
soc_bridge_clean <- soc_bridge %>%
  mutate(soc_2010_clean = str_trim(str_remove(soc_2010, "#+")),
         soc_2018_clean = str_trim(str_remove(soc_2018, "#+")),
         is_split = str_detect(title_2010, "#") & !str_detect(soc_2018, "##"), # 1 → many
         is_merge = str_detect(title_2018, "##")) %>% 
  distinct(soc_2010_clean, soc_2018_clean, .keep_all = TRUE)


# Here I will build proportional split weights from occ_id_2023.
# For every 2010 SOC that splits into multiple 2018 SOCs (#),
# use 2023 employment to compute each child's share of parent employment.
split_weights <- soc_bridge_clean %>%
  filter(is_split) %>% select(soc_2010_clean, soc_2018_clean) %>%
  left_join(occ_id_2023 %>% select(SOC6, TOT_EMP) %>% rename(emp_2023 = TOT_EMP),
            by = c("soc_2018_clean" = "SOC6")) %>%
  # treat NA employment as 0, child exists in crosswalk but not in 2023 OEWS
  mutate(emp_2023 = replace_na(emp_2023, 0)) %>%
  group_by(soc_2010_clean) %>%
  mutate(total_emp_2023 = sum(emp_2023, na.rm = TRUE),
         split_weight = case_when(
           # when at least one child has 2023 employment, use proportional weights
           # children with 0 employment get weight 0 (they existed but had no employment)
           total_emp_2023 > 0 ~ emp_2023 / total_emp_2023,
           # No children have any 2023 employment at all, i use an equal split fallback
           TRUE ~ 1 / n())) %>% ungroup() %>%
  select(soc_2010_clean, soc_2018_clean, split_weight)

# Verify weights sum to 1 for every parent
split_weights %>%
  group_by(soc_2010_clean) %>%
  summarise(weight_sum = sum(split_weight)) %>%
  filter(abs(weight_sum - 1) > 0.01) %>%
  nrow() %>% cat("Split weight violations:", ., "\n")

# Here I am joining the crosswalk and weights with my OEWS/ONET file to apply the 
# split for the occupatios that did and check employment numbers
occ_2015_bridged <- occ_id_2015 %>%
  rename(soc_2010_clean = SOC6) %>%
  left_join(soc_bridge_clean %>% select(soc_2010_clean, soc_2018_clean, is_split, is_merge),
            by = "soc_2010_clean",
            relationship = "many-to-many") %>%
  left_join(split_weights, by = c("soc_2010_clean", "soc_2018_clean")) %>%
  mutate(TOT_EMP_adj = case_when(is_split & !is.na(split_weight) ~ TOT_EMP * split_weight,
                                 TRUE ~ as.numeric(TOT_EMP)))

cat("Original employment:", sum(occ_id_2015$TOT_EMP, na.rm = TRUE), "\n")
cat("After bridge employment:", sum(occ_2015_bridged$TOT_EMP_adj, na.rm = TRUE), "\n")

writexl::write_xlsx(occ_2015_bridged, "occ_2015_bridged.xlsx")

# Collapse to 2018 SOC level --
# for merges, sum employment, weighted average wages
# identify is_middle_wage_occ by an employment-weighted majority ≥ 60%
# for skills, employment-weighted average
occ_2015_soc2018 <- occ_2015_bridged %>%
  group_by(soc_2018_clean) %>%
  summarise(TOT_EMP = sum(TOT_EMP_adj, na.rm = TRUE),
            A_MEDIAN = if_else(sum(TOT_EMP_adj, na.rm = TRUE) > 0,
                               weighted.mean(A_MEDIAN, TOT_EMP_adj, na.rm = TRUE),
                               mean(A_MEDIAN, na.rm = TRUE)),
            is_middle_wage_occ = case_when(sum(TOT_EMP_adj, na.rm = TRUE) > 0 ~
                                             (sum(TOT_EMP_adj[is_middle_wage_occ == TRUE], na.rm = TRUE) /
                                                sum(TOT_EMP_adj, na.rm = TRUE)) >= 0.6,
                                           TRUE ~ (sum(is_middle_wage_occ == TRUE, na.rm = TRUE) / n()) >= 0.6),
            Analytical  = if_else(sum(TOT_EMP_adj, na.rm = TRUE) > 0,
                                  weighted.mean(Analytical, TOT_EMP_adj, na.rm = TRUE), 
                                  mean(Analytical,  na.rm=TRUE)),
            Fundamental = if_else(sum(TOT_EMP_adj, na.rm = TRUE) > 0,
                                  weighted.mean(Fundamental, TOT_EMP_adj, na.rm =TRUE),
                                  mean(Fundamental, na.rm = TRUE)), 
            Managerial = if_else(sum(TOT_EMP_adj, na.rm = TRUE) > 0,
                                 weighted.mean(Managerial, TOT_EMP_adj, na.rm = TRUE),
                                 mean(Managerial, na.rm = TRUE)), 
            Mechanical = if_else(sum(TOT_EMP_adj, na.rm = TRUE) > 0,
                                 weighted.mean(Mechanical, TOT_EMP_adj, na.rm = TRUE),
                                 mean(Mechanical, na.rm = TRUE)), 
            Social = if_else(sum(TOT_EMP_adj, na.rm = TRUE) > 0,
                             weighted.mean(Social, TOT_EMP_adj, na.rm = TRUE),
                             mean(Social, na.rm = TRUE)), .groups = "drop")

cat("2010 SOC codes (original):", n_distinct(occ_id_2015$SOC6), "\n")
cat("2018 SOC codes (after bridge):", nrow(occ_2015_soc2018), "\n")

cat("Original employment:", sum(occ_id_2015$TOT_EMP, na.rm = TRUE), "\n")
cat("After bridge employment:", sum(occ_2015_soc2018$TOT_EMP, na.rm = TRUE), "\n")

# Translate occ_2015_soc2018 from 2018 SOC to 2018 ACS OCCP
# using soc_acs_crosswalk (same crosswalk as 2023)

pre_agg_2015 <- soc_acs_crosswalk %>%
  rename(soc_2018 = SOC) %>%
  full_join(occ_2015_soc2018, by = c("soc_2018" = "soc_2018_clean"))

# now I have 2015 OEWS/ONET file translated into 2018 SOC codes, YAY

cat("pre_agg_2015 rows:", nrow(pre_agg_2015), "\n")

# Build occp_lookup_2015, just like i did in occp_lookup_2023 — same aggregation logic
# this file will aggregate my soc_2018 into ACS OCCP 2018
occp_lookup_2015 <- pre_agg_2015 %>%
  group_by(OCCP) %>%
  summarise(
    valid_soc_count = sum(!is.na(is_middle_wage_occ)),
    total_oews_emp = sum(TOT_EMP, na.rm = TRUE),
    has_oews_support = total_oews_emp > 0 | valid_soc_count > 0,
    is_middle_class_occp = case_when(
      total_oews_emp > 0 ~
        (sum(TOT_EMP[is_middle_wage_occ == TRUE], na.rm = TRUE) / total_oews_emp) >= 0.6,
      valid_soc_count > 0 ~
        (sum(is_middle_wage_occ == TRUE, na.rm = TRUE) / valid_soc_count) >= 0.6,
      TRUE ~ FALSE),
    avg_analytical  = if_else(total_oews_emp > 0,
                              weighted.mean(Analytical,  TOT_EMP, na.rm=TRUE),
                              mean(Analytical,  na.rm=TRUE)),
    avg_fundamental = if_else(total_oews_emp > 0,
                              weighted.mean(Fundamental, TOT_EMP, na.rm=TRUE),
                              mean(Fundamental, na.rm=TRUE)),
    avg_managerial  = if_else(total_oews_emp > 0,
                              weighted.mean(Managerial,  TOT_EMP, na.rm=TRUE),
                              mean(Managerial,  na.rm=TRUE)),
    avg_mechanical  = if_else(total_oews_emp > 0,
                              weighted.mean(Mechanical,  TOT_EMP, na.rm=TRUE),
                              mean(Mechanical,  na.rm=TRUE)),
    avg_social = if_else(total_oews_emp > 0,
                         weighted.mean(Social,      TOT_EMP, na.rm=TRUE),
                         mean(Social,      na.rm=TRUE)),
    .groups = "drop") %>%
  mutate(
    Total_Skill_Sum = avg_analytical + avg_fundamental + avg_managerial + avg_mechanical + avg_social,
    Rel_Analytical  = round(avg_analytical  / Total_Skill_Sum, 2),
    Rel_Fundamental = round(avg_fundamental / Total_Skill_Sum, 2),
    Rel_Managerial  = round(avg_managerial  / Total_Skill_Sum, 2),
    Rel_Mechanical  = round(avg_mechanical  / Total_Skill_Sum, 2),
    Rel_Social = round(avg_social      / Total_Skill_Sum, 2))

cat("2015 Total OCCP codes:", nrow(occp_lookup_2015), "\n")
cat("2015 OCCP codes with OEWS support:", sum(occp_lookup_2015$has_oews_support, na.rm = TRUE), "\n")
cat("2015 OCCP codes MW=TRUE:", sum(occp_lookup_2015$is_middle_class_occp == TRUE, na.rm = TRUE), "\n")

occp_lookup_2015 <- occp_lookup_2015 %>% 
  left_join(occp_crosswalk, by = c("OCCP" = "occp_2018"))



# 1. Create a "Bridge" Lookup: 2010 OCCP -> 2018 OCCP characteristics
# We use the crosswalk to roll the 2018 traits back into 2010 categories
occp_bridge_2015 <- occp_crosswalk %>%
  # Join our 2018-based lookup to the crosswalk
  left_join(occp_lookup_2015, by = c("occp_2018" = "OCCP")) %>%
  
  # Group by the OLD 2010 code
  group_by(occp_2010.x) %>%
  summarise(
    # RATIONALE: Weighted Majority for the Middle-Class flag
    # If the 2018 'children' that are Middle-Class represent >60% of the 
    # employment for that 2010 'parent', the parent is Middle-Class.
    total_pool_emp = sum(total_oews_emp, na.rm = TRUE),
    mw_pool_emp  = sum(total_oews_emp[is_middle_class_occp == TRUE], na.rm = TRUE),
    # Final 2010-level flag
    is_middle_class_2015 = if_else(total_pool_emp > 0, 
                                   (mw_pool_emp / total_pool_emp) >= 0.6, 
                                   FALSE),
    # Roll up skills using weighted means
    across(starts_with("avg_"), ~weighted.mean(.x, total_oews_emp, na.rm = TRUE)),
    across(starts_with("Rel_"), ~weighted.mean(.x, total_oews_emp, na.rm = TRUE)),
    matched_to_oews = any(has_oews_support == TRUE, na.rm = TRUE),
    .groups = "drop")

# 2. Join to ACS 2015 Residents
# This join is now 1-to-1 because we collapsed the bridge to 2010 codes first.
acs_2015_final <- acs_2015 %>%
  left_join(occp_bridge_2015, by = c("OCCP" = "occp_2010.x")) %>%
  mutate(is_middle_class_2015 = replace_na(is_middle_class_2015, FALSE)) %>% 
  select(-total_pool_emp, -mw_pool_emp)

# 3. VERIFY PERSON COUNT
cat("Original count:", nrow(acs_2015), "\n")
cat("Final count:", nrow(acs_2015_final), "\n")
stopifnot(nrow(acs_2015_final) == nrow(acs_2015))

cat("final file weight sum:", sum(acs_2015_final$PWGTP, na.rm = TRUE))
cat("original file weigted sum:", sum(acs_2015$PWGTP, na.rm = TRUE))

cat("2015 ACS persons:", nrow(acs_2015_final), "\n")
cat("Unweighted % matched to OEWS support:",
    round(100 * mean(acs_2015_final$matched_to_oews, na.rm = TRUE), 1), "%\n")
cat("Weighted % matched to OEWS support:",
    round(100 * sum(acs_2015_final$PWGTP[acs_2015_final$matched_to_oews == TRUE], na.rm = TRUE) /
            sum(acs_2015_final$PWGTP, na.rm = TRUE), 1), "%\n")

cat("=== ACS 2015 Coverage ===\n")
acs_2015_final %>%
  summarise(
    total_persons  = n(),
    has_oews = sum(matched_to_oews, na.rm = TRUE),
    no_oews = sum(!matched_to_oews, na.rm = TRUE),
    pct_oews_covered = round(100 * sum(PWGTP[matched_to_oews == TRUE], na.rm = TRUE) / sum(PWGTP), 1),
    mw_true = sum(is_middle_class_2015 == TRUE,  na.rm = TRUE),
    mw_false = sum(is_middle_class_2015 == FALSE, na.rm = TRUE),
    mw_na = sum(is.na(is_middle_class_2015))) %>%
  print()


cat("\nACS 2015 final weighted MW share:\n")
cat(round(100 * sum(acs_2015_final$PWGTP[acs_2015_final$is_middle_class_2015 == TRUE],
                    na.rm = TRUE) / sum(acs_2015_final$PWGTP, na.rm = TRUE), 1), "%\n")

write.csv(acs_2015_final, "acs_2015_final.csv")


# Create a summary table for your Progress Report
trend_comparison <- bind_rows(
  acs_2015_final %>% 
    summarise(Year = 2015,
              Total_Pop = sum(PWGTP),
              MW_Pop = sum(PWGTP[is_middle_class_2015 == TRUE]),
              MW_Share = (MW_Pop / Total_Pop) * 100),
  acs_2023_final %>% 
    summarise(Year = 2023,
              Total_Pop = sum(PWGTP),
              MW_Pop = sum(PWGTP[is_middle_class_occp == TRUE]),
              MW_Share = (MW_Pop / Total_Pop) * 100))

print(trend_comparison)

# Calculate the "Shrinkage"
shrinkage <- trend_comparison$MW_Share[1] - trend_comparison$MW_Share[2]
cat("\nChange in Middle-Class Share:", round(shrinkage, 2), "percentage points\n")



# ------------ Compare ACS 2015 vs ACS 2023 --------------------
progress_summary <- data.frame(
  Metric = c("Total Sample (Persons)", "Weighted Match Quality (% of Workforce Covered)", "Middle-Wage Workforce Share (%)"),
  `2015_Result` = c(
    nrow(acs_2015_final),
    round(100 * sum(acs_2015_final$PWGTP[acs_2015_final$matched_to_oews], na.rm=T) / sum(acs_2015_final$PWGTP), 1),
    round(100 * sum(acs_2015_final$PWGTP[acs_2015_final$is_middle_class_2015]) / sum(acs_2015_final$PWGTP), 1)),
  `2023_Result` = c(
    nrow(acs_2023_final),
    round(100 * sum(acs_2023_final$PWGTP[acs_2023_final$matched_to_oews_2023], na.rm=T) / sum(acs_2023_final$PWGTP), 1),
    round(100 * sum(acs_2023_final$PWGTP[acs_2023_final$is_middle_class_occ_2023_final]) / sum(acs_2023_final$PWGTP), 1)))

print(progress_summary)

# write.csv(progress_summary, "NYC_Middle_Wage_Progress_Report.csv", row.names = FALSE)

