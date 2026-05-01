# ==============================================================================
# Story 2: Establishing a Middle Wage
# Data: O*NET & OEWS
# ==============================================================================

# ------ Part 1. 2015 Data — Education + Wages + Middle-Wage Classification -----

# --- Load O*NET Education Requirement 2015 Data for analysis ---
onet_education_data_2015 <- readxl::read_xlsx("Z:/Adleh_work/ACS.Labor.Analysis - Copy/Datasets/Education, Training, and Experience10.2015.xlsx")

# O*NET (Required Level of Education, Data Value) is a distribution
# across 12 categories, where Data Value = the % of workers who say  that education
# category applies. Categories 1–5 are sub-baccalaureate (less than BA); 6–12 are BA+.

# Method of classification:
# 1. Sum category percentages into two buckets: Sub_BA and BA_Plus.
# 2. An occupation is "middle-skill" if Sub_BA > BA_Plus

onet_education_cleaned_data_2015 <- onet_education_data_2015 %>%
  filter(`Element ID` == "2.D.1") %>%
  mutate(SOC6 = str_remove_all(str_sub(`O*NET-SOC Code`, 1, 7), "-")) %>%
  group_by(SOC6, Category) %>%
  summarise(avg_data_value = mean(`Data Value`, na.rm = TRUE), .groups = "drop") %>%
  mutate(bucket = ifelse(Category <= 5, "Sub_BA", "BA_Plus")) %>%
  group_by(SOC6, bucket) %>%
  summarise(bucket_pct = sum(avg_data_value), .groups = "drop") %>%
  pivot_wider(names_from = bucket, values_from = bucket_pct, values_fill = 0) %>%
  mutate(is_middle_edu = Sub_BA > BA_Plus,
         dominant_requirement = ifelse(is_middle_edu, "Sub-Baccalaureate", "Bachelor's or Higher"))

print(paste("2015 Non-BA Occupations:", sum(onet_education_cleaned_data_2015$dominant_requirement == "Sub-Baccalaureate", na.rm = TRUE)))
print(paste("2015 Total Occupations:", nrow(onet_education_cleaned_data_2015)))


# NOTE: I use a majority-rule threshold, not a hard cutoff, which
# I deemed to be better to occupations with mixed credential distributions.

# --- Load O*NET Skills 2015 Data for analysis ---
onet_skills_data_2015 <- readxl::read_xlsx("Z:/Adleh_work/ACS.Labor.Analysis - Copy/Datasets/Skills.10.2015.xlsx")

# create a mapping based on the Element IDs provided in the PEW report
skill_mapping <- data.frame(
  Element_ID = c(
    # Social: Monitoring, Social Perceptiveness, Coordination, Persuasion, etc.
    "2.A.2.d", "2.B.1.a", "2.B.1.b", "2.B.1.c", "2.B.1.d", "2.B.1.e", "2.B.1.f",
    # Fundamental: Reading, Writing, Speaking, Listening, Math, Science
    "2.A.1.a", "2.A.1.b", "2.A.1.c", "2.A.1.d", "2.A.2.a", "2.A.2.b", "2.A.2.c", "2.B.4.e",
    # Analytical: Critical Thinking, Active Learning, Operations Analysis, etc.
    "2.A.1.e", "2.A.1.f", "2.B.2.i", "2.B.3.a", "2.B.3.b", "2.B.4.g", "2.B.3.e", "2.B.4.h",
    # Managerial: Personnel, Financial, Material Resources, Time Management
    "2.B.5.a", "2.B.5.b", "2.B.5.c", "2.B.5.d",
    # Mechanical: Equipment Selection, Installation, Programming, Troubleshooting, Repair
    "2.B.3.c", "2.B.3.d", "2.B.3.g", "2.B.3.h", "2.B.3.j", "2.B.3.k", "2.B.3.l", "2.B.3.m"),
  Family = c(rep("Social", 7), rep("Fundamental", 8), rep("Analytical", 8), 
    rep("Managerial", 4), rep("Mechanical", 8)))

# Aggregate skills into families
onet_skills_cleaned_2015 <- onet_skills_data_2015 %>%
  filter(`Scale ID` == "IM") %>% # focus on importance as per report
  mutate(SOC6 = str_remove_all(str_sub(`O*NET-SOC Code`, 1, 7), "-")) %>%
  # before aggregating into families, I will aggregate 8-digit occupations to the 6-digit level by averaging the scores
  group_by(SOC6, `Element ID`) %>% 
  mutate(Data_Value = mean(`Data Value`, na.rm = TRUE)) %>% 
  # Now I will join by the skill family classification and aggregate 
  inner_join(skill_mapping, by = c("Element ID" = "Element_ID")) %>%
  group_by(SOC6, Family) %>%
  summarise(family_score = mean(Data_Value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Family, values_from = family_score)


# Now, calculate the relative importance
onet_skills_cleaned_2015 <- onet_skills_cleaned_2015 %>%
  mutate(Total_Skill_Sum = Social + Fundamental + Analytical + Managerial + Mechanical,
         Rel_Social = round(Social / Total_Skill_Sum, 2),
         Rel_Analytical = round(Analytical / Total_Skill_Sum, 2),
         Rel_Fundamental = round(Fundamental / Total_Skill_Sum, 2),
         Rel_Managerial = round(Managerial / Total_Skill_Sum, 2),
         Rel_Mechanical = round(Mechanical / Total_Skill_Sum, 2))


# Now I create one, all-encompassing file with SOC codes, education, & sills
education_skills_soc_2015 <- onet_education_cleaned_data_2015 %>% 
  full_join(onet_skills_cleaned_2015, by = "SOC6")

# --- OEWS Middle-wage-bands ---
oews_2015 <- readxl::read_xlsx("Z:/Adleh_work/ACS.Labor.Analysis - Copy/Datasets/aMSA_M2015_dl.xlsx")  

# Filter to the NYC-Newark-Jersey City MSA and detailed occupation group only
oews_nyc_2015 <- oews_2015 %>%
  filter(OCC_GROUP == "detailed",
         AREA_NAME == "New York-Newark-Jersey City, NY-NJ-PA") %>%
  filter(A_MEDIAN != c("*", "	#"), TOT_EMP != "**") %>% 
  mutate(SOC6 = str_remove_all(str_sub(OCC_CODE, 1, 7), "-"),
         A_MEDIAN = suppressWarnings(as.numeric(A_MEDIAN)),
         TOT_EMP = suppressWarnings(as.numeric(TOT_EMP))) %>%
  select(SOC6, OCC_TITLE, TOT_EMP, A_MEDIAN)

print(paste("Number of Occupations from OEWS that dont have wages:",
            sum(is.na(oews_nyc_2015$A_MEDIAN), na.rm = TRUE)))

# I chose an employment-weighted median to reflect where the a typical worker sits in the wage 
# distribution and to describe the middle of the workforce & not the middle of job categories.

# Band definition:
# Lower bound = 67% of weighted median
# Upper bound = 200% of weighted median

# These multipliers are consistent across both years, so the band is defined
# relative to each year's own wage structure rather than being a fixed dollar range
oews_nyc_wages_2015 <- oews_nyc_2015 %>% filter(!is.na(A_MEDIAN))

nyc_overall_median_2015 <- weighted.median(
  x = oews_nyc_wages_2015$A_MEDIAN,
  w = oews_nyc_wages_2015$TOT_EMP)

lower_bound_2015 <- nyc_overall_median_2015 * 0.67
upper_bound_2015 <- nyc_overall_median_2015 * 2.00

print(paste("2015 NYC-NJ-PA Metro Middle-Wage Range:", round(lower_bound_2015), "to", round(upper_bound_2015)))

# --- Classify 2015 Occupations as Middle-wage ---

# join OEWS data with ONET data
occ_id_2015_na <- oews_nyc_wages_2015 %>%
  left_join(education_skills_soc_2015, by = "SOC6")

# Quality check -- What % of OEWS was identified in ONET??
print(paste("Number of Occupations from OEWS that is NA in ONET:",
            sum(is.na(occ_id_2015_na$is_middle_edu), na.rm = TRUE)))

na_count <- sum(is.na(occ_id_2015_na$is_middle_edu))
total_count <- nrow(occ_id_2015_na)
na_pct <- na_count / total_count * 100

print(paste("Percent of Occupations from OEWS that are NA in ONET:",
            round(na_pct, 2), "%"))

# Since some occupations identified in OEWS don't have education flags, I did a manual diagnostic and found out that many 
# of these occupations are in the "All Other" groups.So I will do the following: 

# 1. identify occupations that have OEWS employment/wage figures but not ONET flags ---
onet_minor_na_occp_2015 <- occ_id_2015_na %>%
  arrange(SOC6) %>%
  mutate(is_imputed = is.na(is_middle_edu)) # Create a flag to track which rows are being imputed

# 2. Use fill() to propagate the education and skill family flags from the 
# occupation immediately preceding it in the SOC hierarchy. I do this because I am
# making the assumption that those occupations will have similar educational requirements as the occupations closest to them.

# 3.After assigning that middle_education flag, occupations are identified as middle_wage_occp if:
# 1. Its annual median wage falls within the NYC middle-wage band AND
# 2. It is a middle-skill occupation (majority of workers don't need BA+)
occ_id_2015 <- onet_minor_na_occp_2015 %>% 
  fill(is_middle_edu, dominant_requirement, Social, Fundamental, Analytical, Managerial, Mechanical,
       Rel_Social, Rel_Analytical, Rel_Fundamental, Rel_Managerial, Rel_Mechanical,
       .direction = "down") %>%  # Take from the SOC code right before
  mutate(is_middle_wage = A_MEDIAN >= lower_bound_2015 & A_MEDIAN <= upper_bound_2015,
         is_middle_wage_occ = is_middle_wage & is_middle_edu,
         SOC = SOC6)

write.csv(occ_id_2015, "occ_id_2015.csv")

# What is the share of middle wage occupation within this dataset?
print(paste("2015 Middle Wage Occupation Count:", sum(occ_id_2015$is_middle_wage_occ == "TRUE", na.rm = TRUE)))


# ------ Part 2. 2023 Data — Education + Wages + Middle-Wage Classification -----
# Identical methodology to Part 2, applied to the 2023 vintage.

# --- Load O*NET Education Requirement 2015 Data for analysis ---
onet_education_data_2023 <- readxl::read_xlsx("Z:/Adleh_work/ACS.Labor.Analysis - Copy/Datasets/Education, Training, and Experience11.2023.xlsx")

onet_education_cleaned_data_2023 <- onet_education_data_2023 %>%
  filter(`Element ID` == "2.D.1") %>%
  mutate(SOC6 = str_remove_all(str_sub(`O*NET-SOC Code`, 1, 7), "-")) %>%
  group_by(SOC6, Category) %>%
  summarise(avg_data_value = mean(`Data Value`, na.rm = TRUE), .groups = "drop") %>%
  mutate(bucket = ifelse(Category <= 5, "Sub_BA", "BA_Plus")) %>%
  group_by(SOC6, bucket) %>%
  summarise(bucket_pct = sum(avg_data_value), .groups = "drop") %>%
  pivot_wider(names_from = bucket, values_from = bucket_pct, values_fill = 0) %>%
  mutate(is_middle_edu  = Sub_BA > BA_Plus,
         dominant_requirement = ifelse(is_middle_edu, "Sub-Baccalaureate", "Bachelor's or Higher"))

print(paste("2023 Non-BA Count:", sum(onet_education_cleaned_data_2023$dominant_requirement == "Sub-Baccalaureate", na.rm = TRUE)))
print(paste("2023 Total Occupations:", nrow(onet_education_cleaned_data_2023)))

# --- Load O*NET Skills 2023 Data for analysis ---
onet_skills_data_2023 <- readxl::read_xlsx("Z:/Adleh_work/ACS.Labor.Analysis - Copy/Datasets/Skills11.2023.xlsx")

onet_skills_cleaned_2023 <- onet_skills_data_2023 %>%
  filter(`Scale ID` == "IM") %>% # focus on importance as per report
  mutate(SOC6 = str_remove_all(str_sub(`O*NET-SOC Code`, 1, 7), "-")) %>%
  group_by(SOC6, `Element ID`) %>% 
  mutate(Data_Value = mean(`Data Value`, na.rm = TRUE)) %>% 
  inner_join(skill_mapping, by = c("Element ID" = "Element_ID")) %>%
  group_by(SOC6, Family) %>%
  summarise(family_score = mean(Data_Value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Family, values_from = family_score)

# Now, calculate the relative importance
onet_skills_cleaned_2023 <- onet_skills_cleaned_2023 %>%
  mutate(Total_Skill_Sum = Social + Fundamental + Analytical + Managerial + Mechanical,
         Rel_Social = round(Social / Total_Skill_Sum, 2),
         Rel_Analytical = round(Analytical / Total_Skill_Sum, 2),
         Rel_Fundamental = round(Fundamental / Total_Skill_Sum, 2),
         Rel_Managerial = round(Managerial / Total_Skill_Sum, 2),
         Rel_Mechanical = round(Mechanical / Total_Skill_Sum, 2))

education_skills_soc_2023 <- onet_education_cleaned_data_2023 %>% 
  full_join(onet_skills_cleaned_2023, by = "SOC6")

# --- OEWS Middle-wage-bands ---
oews_2023 <- readxl::read_xlsx("Z:/Adleh_work/ACS.Labor.Analysis - Copy/Datasets/MSA_M2023_dl.xlsx")

oews_nyc_2023 <- oews_2023 %>%
  filter(O_GROUP == "detailed",             
         AREA_TITLE == "New York-Newark-Jersey City, NY-NJ-PA") %>% 
  mutate(SOC6 = str_remove_all(str_sub(OCC_CODE, 1, 7), "-"),
         A_MEDIAN = suppressWarnings(as.numeric(A_MEDIAN)),
         TOT_EMP = suppressWarnings(as.numeric(TOT_EMP))) %>%
  select(SOC6, OCC_TITLE, TOT_EMP, A_MEDIAN)

print(paste("Number of Occupations from OEWS that dont have wages:",
            sum(is.na(oews_nyc_2023$A_MEDIAN), na.rm = TRUE)))

oews_nyc_wages_2023 <- oews_nyc_2023 %>% filter(!is.na(A_MEDIAN))

nyc_overall_median_2023 <- weighted.median(
  x = oews_nyc_wages_2023$A_MEDIAN,
  w = oews_nyc_wages_2023$TOT_EMP)

lower_bound_2023 <- nyc_overall_median_2023 * 0.67
upper_bound_2023 <- nyc_overall_median_2023 * 2.00

print(paste("2023 Middle-Wage Range:", round(lower_bound_2023), "to", round(upper_bound_2023)))


# --- Classify 2023 Occupations as Middle-wage ---
occ_id_2023_na <- oews_nyc_wages_2023 %>%
  left_join(education_skills_soc_2023, by = "SOC6") 

# Quality check -- What % of OEWS was identified in ONET??
print(paste("Number of Occupations from OEWS that is NA in ONET:",
            sum(is.na(occ_id_2023_na$is_middle_edu), na.rm = TRUE)))

na_count <- sum(is.na(occ_id_2023_na$is_middle_edu))
total_count <- nrow(occ_id_2023_na)
na_pct <- na_count / total_count * 100

print(paste("Percent of Occupations from OEWS that are NA in ONET:",
            round(na_pct, 2), "%"))

# identify occupations that have OEWS employment/wage figures but not ONET flags ---
onet_minor_na_occp <- occ_id_2023_na %>%
  arrange(SOC6) %>%
  mutate(is_imputed = is.na(is_middle_edu)) # Create a flag to track which rows are being imputed

# Use fill() to propagate the education and skill family flags from the 
# occupation immediately preceding it in the SOC hierarchy
occ_id_2023 <- onet_minor_na_occp %>% 
  fill(is_middle_edu, dominant_requirement, Social, Fundamental, Analytical, Managerial, Mechanical,
       Rel_Social, Rel_Analytical, Rel_Fundamental, Rel_Managerial, Rel_Mechanical,
       .direction = "down") %>%  # Take from the SOC code right before
  mutate(is_middle_wage = A_MEDIAN >= lower_bound_2023 & A_MEDIAN <= upper_bound_2023,
         is_middle_wage_occ = is_middle_wage & is_middle_edu,
         SOC = SOC6)


print(paste("2023 Middle Wage Occupation Count:", sum(occ_id_2023$is_middle_wage_occ == "TRUE", na.rm = TRUE)))

write.csv(occ_id_2023, "occ_id_2023.csv")

