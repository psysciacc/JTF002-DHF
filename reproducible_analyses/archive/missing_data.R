# ============================================================================
# MISSING DATA ANALYSIS
# ============================================================================

print(paste(rep("\n=", 70), collapse = ""))
print("MISSING DATA ANALYSIS")
print(paste(rep("=", 70), collapse = ""))

# Key variables to check for missing data (BEHAVIORAL MEASURES REMOVED)
key_variables <- c(
  "Age", "gender", "ethnicity", "religion", "student", "country_res_full",
  "D_end", "H_end", "F_end", 
  "D_self_end", "D_other_end", "H_self_end", "H_other_end", "F_self_end", "F_other_end",
  "D_norm", "H_norm", "F_norm", "D_self_norm", "D_other_norm", "H_self_norm", "H_other_norm", "F_self_norm", "F_other_norm",  
  "Positive_Reciprocity", "Negative_Reciprocity",
  "Total_Violence_Scale", "Other_Face_Concern", "Self_Face_Concern", 
  "Retaliation", "Withdrawal", "Humor", 
  "Self_Transcendence", "Self_Enhancement", "Openness_to_Change", "Conservation", 
  "MFQ_Care", "MFQ_Equality", "MFQ_Proportionality", "MFQ_Loyalty", "MFQ_Authority", "MFQ_Purity"
)

# Calculate missing data statistics
missing_data_summary <- data.frame(
  Variable = character(),
  N_Missing = numeric(),
  Percent_Missing = numeric(),
  N_Valid = numeric(),
  Percent_Valid = numeric(),
  stringsAsFactors = FALSE
)

for(var in key_variables) {
  if(var %in% names(dt4)) {
    n_missing <- sum(is.na(dt4[[var]]))
    n_valid <- sum(!is.na(dt4[[var]]))
    pct_missing <- round(n_missing / nrow(dt4) * 100, 2)
    pct_valid <- round(n_valid / nrow(dt4) * 100, 2)
    
    missing_data_summary <- rbind(missing_data_summary, 
                                  data.frame(
                                    Variable = var,
                                    N_Missing = n_missing,
                                    Percent_Missing = pct_missing,
                                    N_Valid = n_valid,
                                    Percent_Valid = pct_valid
                                  ))
  }
}

# Sort by percent missing (descending)
missing_data_summary <- missing_data_summary %>% arrange(desc(Percent_Missing))

# Save as CSV (best for Excel/spreadsheets)
write.csv(missing_data_summary, 
          "missing_data_summary.csv", 
          row.names = FALSE)




# ========================================
# DICTATOR AND ULTIMATUM GAME SAMPLE EXPLORATION
# ========================================

library(dplyr)
library(tidyr)
library(knitr)
library(psych)
library(writexl)

# Create output directory if it doesn't exist
if(!dir.exists("game_sample_exploration")) {
  dir.create("game_sample_exploration")
}

# ========================================
# 1. OVERALL EXCLUSION RATES
# ========================================

cat("\n=== OVERALL SAMPLE SIZES ===\n")
cat("Main sample (dt4):", nrow(dt4), "\n")
cat("Dictator game sample (dt5):", nrow(dt5), "\n")
cat("Ultimatum game sample (dt6):", nrow(dt6), "\n\n")

# Create overall summary table
overall_summary <- data.frame(
  Sample = c("Main sample (dt4)", "Dictator game (dt5)", "Ultimatum game (dt6)"),
  N = c(nrow(dt4), nrow(dt5), nrow(dt6))
)

# Dictator Game Exclusions
cat("=== DICTATOR GAME EXCLUSIONS ===\n")
dictator_excluded <- nrow(dt4) - nrow(dt5)
dictator_excl_pct <- (dictator_excluded / nrow(dt4)) * 100

cat("Participants excluded:", dictator_excluded, "\n")
cat("Exclusion rate:", round(dictator_excl_pct, 2), "%\n")
cat("Participants retained:", nrow(dt5), 
    "(", round((nrow(dt5)/nrow(dt4))*100, 2), "%)\n\n")

# Ultimatum Game Exclusions
cat("=== ULTIMATUM GAME EXCLUSIONS ===\n")
ultimatum_excluded <- nrow(dt4) - nrow(dt6)
ultimatum_excl_pct <- (ultimatum_excluded / nrow(dt4)) * 100

cat("Participants excluded:", ultimatum_excluded, "\n")
cat("Exclusion rate:", round(ultimatum_excl_pct, 2), "%\n")
cat("Participants retained:", nrow(dt6), 
    "(", round((nrow(dt6)/nrow(dt4))*100, 2), "%)\n\n")

# Create exclusion summary table
exclusion_summary <- data.frame(
  Game = c("Dictator", "Ultimatum"),
  N_starting = c(nrow(dt4), nrow(dt4)),
  N_retained = c(nrow(dt5), nrow(dt6)),
  N_excluded = c(dictator_excluded, ultimatum_excluded),
  Exclusion_rate_pct = c(round(dictator_excl_pct, 2), round(ultimatum_excl_pct, 2)),
  Retention_rate_pct = c(round((nrow(dt5)/nrow(dt4))*100, 2), 
                         round((nrow(dt6)/nrow(dt4))*100, 2))
)

# Save overall summary
write.csv(overall_summary, "game_sample_exploration/01_overall_sample_sizes.csv", row.names = FALSE)
write.csv(exclusion_summary, "game_sample_exploration/02_exclusion_summary.csv", row.names = FALSE)

cat("✓ Saved: 01_overall_sample_sizes.csv\n")
cat("✓ Saved: 02_exclusion_summary.csv\n\n")

# Additional exclusions during variable creation
cat("=== ADDITIONAL EXCLUSIONS IN ULTIMATUM GAME ===\n")
cat("After creating positive & negative reciprocity variables:\n")
cat("Participants in dt6 after variable creation: 20545\n")
cat("Additional exclusions: 81 (0.4%)\n\n")

additional_exclusions <- data.frame(
  Stage = c("After comprehension checks", "After variable creation", "Additional exclusions"),
  N = c(nrow(dt6), 20545, 81),
  Note = c("Initial dt6", "Final dt6", "Lost during processing")
)

write.csv(additional_exclusions, "game_sample_exploration/03_additional_exclusions_ultimatum.csv", row.names = FALSE)
cat("✓ Saved: 03_additional_exclusions_ultimatum.csv\n\n")

# ========================================
# 2. COMPREHENSION CHECK DETAILS
# ========================================

cat("\n=== DICTATOR GAME COMPREHENSION CHECK BREAKDOWN ===\n")

# Create detailed comprehension check variables
dt4$dict_comp1_both <- ifelse(dt4$Dictator.comp1.1_dich=="Correct" & 
                                dt4$Dictator.comp1.2_dich=="Correct", 1, 0)
dt4$dict_comp2_both <- ifelse(dt4$Dictator.comp2.1_dich=="Correct" & 
                                dt4$Dictator.comp2.2_dich=="Correct", 1, 0)

# Summary of comprehension patterns
dict_comp1_both_n <- sum(dt4$dict_comp1_both, na.rm=TRUE)
dict_comp2_both_n <- sum(dt4$dict_comp2_both, na.rm=TRUE)
dict_pass_n <- sum(dt4$dictator_cpass, na.rm=TRUE)

cat("\nDictator comprehension check patterns:\n")
cat("Passed first attempt (both questions):", dict_comp1_both_n, 
    "(", round(mean(dt4$dict_comp1_both, na.rm=TRUE)*100, 2), "%)\n")
cat("Passed second attempt (both questions):", dict_comp2_both_n, 
    "(", round(mean(dt4$dict_comp2_both, na.rm=TRUE)*100, 2), "%)\n")
cat("Passed overall:", dict_pass_n, 
    "(", round(mean(dt4$dictator_cpass, na.rm=TRUE)*100, 2), "%)\n\n")

# Create comprehension summary table for dictator
dictator_comp_summary <- data.frame(
  Attempt = c("First attempt (both questions correct)", 
              "Second attempt (both questions correct)", 
              "Overall pass (either attempt)"),
  N_passed = c(dict_comp1_both_n, dict_comp2_both_n, dict_pass_n),
  Pass_rate_pct = c(round(mean(dt4$dict_comp1_both, na.rm=TRUE)*100, 2),
                    round(mean(dt4$dict_comp2_both, na.rm=TRUE)*100, 2),
                    round(mean(dt4$dictator_cpass, na.rm=TRUE)*100, 2))
)

write.csv(dictator_comp_summary, "game_sample_exploration/04_dictator_comprehension_summary.csv", row.names = FALSE)
cat("✓ Saved: 04_dictator_comprehension_summary.csv\n\n")

cat("=== ULTIMATUM GAME COMPREHENSION CHECK BREAKDOWN ===\n")

# Create detailed comprehension check variables
dt4$ult_comp1_both <- ifelse(dt4$Ultimatum.comp.1.1_dich=="Correct" & 
                               dt4$Ultimatum.comp.1.2_dich=="Correct", 1, 0)
dt4$ult_comp2_both <- ifelse(dt4$Ultimatum.comp.2.1_dich=="Correct" & 
                               dt4$Ultimatum.comp.2.2_dich=="Correct", 1, 0)

ult_comp1_both_n <- sum(dt4$ult_comp1_both, na.rm=TRUE)
ult_comp2_both_n <- sum(dt4$ult_comp2_both, na.rm=TRUE)
ult_pass_n <- sum(dt4$ultimatum_cpass, na.rm=TRUE)

cat("\nUltimatum comprehension check patterns:\n")
cat("Passed first attempt (both questions):", ult_comp1_both_n, 
    "(", round(mean(dt4$ult_comp1_both, na.rm=TRUE)*100, 2), "%)\n")
cat("Passed second attempt (both questions):", ult_comp2_both_n, 
    "(", round(mean(dt4$ult_comp2_both, na.rm=TRUE)*100, 2), "%)\n")
cat("Passed overall:", ult_pass_n, 
    "(", round(mean(dt4$ultimatum_cpass, na.rm=TRUE)*100, 2), "%)\n\n")

# Create comprehension summary table for ultimatum
ultimatum_comp_summary <- data.frame(
  Attempt = c("First attempt (both questions correct)", 
              "Second attempt (both questions correct)", 
              "Overall pass (either attempt)"),
  N_passed = c(ult_comp1_both_n, ult_comp2_both_n, ult_pass_n),
  Pass_rate_pct = c(round(mean(dt4$ult_comp1_both, na.rm=TRUE)*100, 2),
                    round(mean(dt4$ult_comp2_both, na.rm=TRUE)*100, 2),
                    round(mean(dt4$ultimatum_cpass, na.rm=TRUE)*100, 2))
)

write.csv(ultimatum_comp_summary, "game_sample_exploration/05_ultimatum_comprehension_summary.csv", row.names = FALSE)
cat("✓ Saved: 05_ultimatum_comprehension_summary.csv\n\n")

# ========================================
# 3. COUNTRY-LEVEL EXCLUSION RATES
# ========================================

cat("\n=== COUNTRY-LEVEL EXCLUSION RATES ===\n\n")

# Check if country_res_full exists
if("country_res_full" %in% names(dt4)) {
  
  # Dictator Game by Country
  dict_country <- dt4 %>%
    group_by(country_res_full) %>%
    summarise(
      N_total = n(),
      N_passed = sum(dictator_cpass, na.rm=TRUE),
      N_excluded = N_total - N_passed,
      Pass_rate_pct = round((N_passed/N_total)*100, 2),
      Exclusion_rate_pct = round((N_excluded/N_total)*100, 2)
    ) %>%
    arrange(desc(Exclusion_rate_pct))
  
  cat("DICTATOR GAME - Country-level exclusion rates:\n")
  print(kable(dict_country, format="simple"))
  cat("\n")
  
  # Save dictator country table
  write.csv(dict_country, "game_sample_exploration/06_dictator_exclusions_by_country.csv", row.names = FALSE)
  cat("✓ Saved: 06_dictator_exclusions_by_country.csv\n\n")
  
  # Ultimatum Game by Country
  ult_country <- dt4 %>%
    group_by(country_res_full) %>%
    summarise(
      N_total = n(),
      N_passed = sum(ultimatum_cpass, na.rm=TRUE),
      N_excluded = N_total - N_passed,
      Pass_rate_pct = round((N_passed/N_total)*100, 2),
      Exclusion_rate_pct = round((N_excluded/N_total)*100, 2)
    ) %>%
    arrange(desc(Exclusion_rate_pct))
  
  cat("\nULTIMATUM GAME - Country-level exclusion rates:\n")
  print(kable(ult_country, format="simple"))
  cat("\n")
  
  # Save ultimatum country table
  write.csv(ult_country, "game_sample_exploration/07_ultimatum_exclusions_by_country.csv", row.names = FALSE)
  cat("✓ Saved: 07_ultimatum_exclusions_by_country.csv\n\n")
  
  # Summary statistics
  cat("\n=== COUNTRY-LEVEL EXCLUSION RATE SUMMARY ===\n")
  cat("\nDictator Game:\n")
  cat("Mean exclusion rate across countries:", 
      round(mean(dict_country$Exclusion_rate_pct), 2), "%\n")
  cat("SD:", round(sd(dict_country$Exclusion_rate_pct), 2), "%\n")
  cat("Range:", round(min(dict_country$Exclusion_rate_pct), 2), "% to",
      round(max(dict_country$Exclusion_rate_pct), 2), "%\n\n")
  
  cat("Ultimatum Game:\n")
  cat("Mean exclusion rate across countries:", 
      round(mean(ult_country$Exclusion_rate_pct), 2), "%\n")
  cat("SD:", round(sd(ult_country$Exclusion_rate_pct), 2), "%\n")
  cat("Range:", round(min(ult_country$Exclusion_rate_pct), 2), "% to",
      round(max(ult_country$Exclusion_rate_pct), 2), "%\n\n")
  
  # Create summary statistics table
  country_excl_summary <- data.frame(
    Game = c("Dictator", "Ultimatum"),
    N_countries = c(nrow(dict_country), nrow(ult_country)),
    Mean_exclusion_rate_pct = c(round(mean(dict_country$Exclusion_rate_pct), 2),
                                round(mean(ult_country$Exclusion_rate_pct), 2)),
    SD_exclusion_rate = c(round(sd(dict_country$Exclusion_rate_pct), 2),
                          round(sd(ult_country$Exclusion_rate_pct), 2)),
    Min_exclusion_rate_pct = c(round(min(dict_country$Exclusion_rate_pct), 2),
                               round(min(ult_country$Exclusion_rate_pct), 2)),
    Max_exclusion_rate_pct = c(round(max(dict_country$Exclusion_rate_pct), 2),
                               round(max(ult_country$Exclusion_rate_pct), 2))
  )
  
  write.csv(country_excl_summary, "game_sample_exploration/08_country_exclusion_summary_stats.csv", row.names = FALSE)
  cat("✓ Saved: 08_country_exclusion_summary_stats.csv\n\n")
  
} else {
  cat("Warning: country_res_full variable not found in dt4\n")
}

# ========================================
# 4. MISSING VALUES IN CREATED VARIABLES
# ========================================

cat("\n=== MISSING VALUES IN CREATED VARIABLES ===\n\n")

# Dictator Game Variables
cat("DICTATOR GAME SAMPLE (dt5):\n")
prosocial_valid <- sum(!is.na(dt5$prosocial))
prosocial_missing <- sum(is.na(dt5$prosocial))
prosocial_pct_missing <- round((prosocial_missing/nrow(dt5))*100, 2)

cat("prosocial variable:\n")
cat("  N valid:", prosocial_valid, "\n")
cat("  N missing:", prosocial_missing, "\n")
cat("  % missing:", prosocial_pct_missing, "%\n\n")

# Get descriptive statistics
prosocial_desc <- describe(dt5$prosocial)
cat("Descriptive statistics for prosocial:\n")
print(prosocial_desc)
cat("\n")

# Create missing values summary for dictator
dict_missing_summary <- data.frame(
  Variable = "prosocial",
  N_total = nrow(dt5),
  N_valid = prosocial_valid,
  N_missing = prosocial_missing,
  Pct_missing = prosocial_pct_missing,
  Mean = round(prosocial_desc$mean, 2),
  SD = round(prosocial_desc$sd, 2),
  Median = prosocial_desc$median,
  Min = prosocial_desc$min,
  Max = prosocial_desc$max
)

write.csv(dict_missing_summary, "game_sample_exploration/09_dictator_missing_values.csv", row.names = FALSE)
cat("✓ Saved: 09_dictator_missing_values.csv\n\n")

# Ultimatum Game Variables
cat("\nULTIMATUM GAME SAMPLE (dt6):\n")

# UG_negrec (negative reciprocity)
negrec_valid <- sum(!is.na(dt6$UG_negrec))
negrec_missing <- sum(is.na(dt6$UG_negrec))
negrec_pct_missing <- round((negrec_missing/nrow(dt6))*100, 2)

cat("UG_negrec (negative reciprocity):\n")
cat("  N valid:", negrec_valid, "\n")
cat("  N missing:", negrec_missing, "\n")
cat("  % missing:", negrec_pct_missing, "%\n\n")

negrec_desc <- describe(dt6$UG_negrec)
cat("Descriptive statistics for UG_negrec:\n")
print(negrec_desc)
cat("\n")

# UG_posrec (positive reciprocity)
posrec_valid <- sum(!is.na(dt6$UG_posrec))
posrec_missing <- sum(is.na(dt6$UG_posrec))
posrec_pct_missing <- round((posrec_missing/nrow(dt6))*100, 2)

cat("UG_posrec (positive reciprocity):\n")
cat("  N valid:", posrec_valid, "\n")
cat("  N missing:", posrec_missing, "\n")
cat("  % missing:", posrec_pct_missing, "%\n\n")

posrec_desc <- describe(dt6$UG_posrec)
cat("Descriptive statistics for UG_posrec:\n")
print(posrec_desc)
cat("\n")

# Create missing values summary for ultimatum
ult_missing_summary <- data.frame(
  Variable = c("UG_negrec", "UG_posrec"),
  N_total = c(nrow(dt6), nrow(dt6)),
  N_valid = c(negrec_valid, posrec_valid),
  N_missing = c(negrec_missing, posrec_missing),
  Pct_missing = c(negrec_pct_missing, posrec_pct_missing),
  Mean = c(round(negrec_desc$mean, 2), round(posrec_desc$mean, 2)),
  SD = c(round(negrec_desc$sd, 2), round(posrec_desc$sd, 2)),
  Median = c(negrec_desc$median, posrec_desc$median),
  Min = c(negrec_desc$min, posrec_desc$min),
  Max = c(negrec_desc$max, posrec_desc$max)
)

write.csv(ult_missing_summary, "game_sample_exploration/10_ultimatum_missing_values.csv", row.names = FALSE)
cat("✓ Saved: 10_ultimatum_missing_values.csv\n\n")

# ========================================
# 5. COUNTRY-LEVEL MISSING VALUES
# ========================================

if("country_res_full" %in% names(dt5) && "country_res_full" %in% names(dt6)) {
  cat("\n=== COUNTRY-LEVEL MISSING VALUES ===\n\n")
  
  # Dictator Game
  dict_missing_country <- dt5 %>%
    group_by(country_res_full) %>%
    summarise(
      N = n(),
      N_missing_prosocial = sum(is.na(prosocial)),
      Pct_missing_prosocial = round((N_missing_prosocial/N)*100, 2),
      Mean_prosocial = round(mean(prosocial, na.rm=TRUE), 2),
      SD_prosocial = round(sd(prosocial, na.rm=TRUE), 2)
    ) %>%
    arrange(desc(Pct_missing_prosocial))
  
  cat("DICTATOR GAME - Missing prosocial by country:\n")
  print(kable(dict_missing_country, format="simple"))
  cat("\n")
  
  write.csv(dict_missing_country, "game_sample_exploration/11_dictator_missing_by_country.csv", row.names = FALSE)
  cat("✓ Saved: 11_dictator_missing_by_country.csv\n\n")
  
  # Ultimatum Game
  ult_missing_country <- dt6 %>%
    group_by(country_res_full) %>%
    summarise(
      N = n(),
      N_missing_negrec = sum(is.na(UG_negrec)),
      Pct_missing_negrec = round((N_missing_negrec/N)*100, 2),
      N_missing_posrec = sum(is.na(UG_posrec)),
      Pct_missing_posrec = round((N_missing_posrec/N)*100, 2),
      Mean_negrec = round(mean(UG_negrec, na.rm=TRUE), 2),
      SD_negrec = round(sd(UG_negrec, na.rm=TRUE), 2),
      Mean_posrec = round(mean(UG_posrec, na.rm=TRUE), 2),
      SD_posrec = round(sd(UG_posrec, na.rm=TRUE), 2)
    ) %>%
    arrange(desc(Pct_missing_negrec))
  
  cat("ULTIMATUM GAME - Missing values by country:\n")
  print(kable(ult_missing_country, format="simple"))
  cat("\n")
  
  write.csv(ult_missing_country, "game_sample_exploration/12_ultimatum_missing_by_country.csv", row.names = FALSE)
  cat("✓ Saved: 12_ultimatum_missing_by_country.csv\n\n")
}

# ========================================
# 6. OVERLAP BETWEEN SAMPLES
# ========================================

cat("\n=== OVERLAP BETWEEN DICTATOR AND ULTIMATUM SAMPLES ===\n\n")

# Find participants in both samples
both_samples <- intersect(dt5$ResponseId, dt6$ResponseId)
both_n <- length(both_samples)
both_pct_dt4 <- round((both_n/nrow(dt4))*100, 2)
both_pct_dt5 <- round((both_n/nrow(dt5))*100, 2)
both_pct_dt6 <- round((both_n/nrow(dt6))*100, 2)

cat("Participants who passed both comprehension checks:", both_n, "\n")
cat("Percentage of dt4:", both_pct_dt4, "%\n")
cat("Percentage of dt5:", both_pct_dt5, "%\n")
cat("Percentage of dt6:", both_pct_dt6, "%\n\n")

# Only in dictator
only_dictator <- setdiff(dt5$ResponseId, dt6$ResponseId)
only_dict_n <- length(only_dictator)
only_dict_pct <- round((only_dict_n/nrow(dt5))*100, 2)

cat("Participants only in dictator sample:", only_dict_n, "\n")
cat("Percentage of dt5:", only_dict_pct, "%\n\n")

# Only in ultimatum
only_ultimatum <- setdiff(dt6$ResponseId, dt5$ResponseId)
only_ult_n <- length(only_ultimatum)
only_ult_pct <- round((only_ult_n/nrow(dt6))*100, 2)

cat("Participants only in ultimatum sample:", only_ult_n, "\n")
cat("Percentage of dt6:", only_ult_pct, "%\n\n")

# Create overlap summary table
overlap_summary <- data.frame(
  Category = c("Both samples", "Only Dictator", "Only Ultimatum", "Neither (excluded from both)"),
  N = c(both_n, only_dict_n, only_ult_n, nrow(dt4) - nrow(dt5) - only_ult_n),
  Pct_of_dt4 = c(both_pct_dt4, 
                 round((only_dict_n/nrow(dt4))*100, 2),
                 round((only_ult_n/nrow(dt4))*100, 2),
                 round(((nrow(dt4) - nrow(dt5) - only_ult_n)/nrow(dt4))*100, 2)),
  Pct_of_dt5 = c(both_pct_dt5, only_dict_pct, NA, NA),
  Pct_of_dt6 = c(both_pct_dt6, NA, only_ult_pct, NA)
)

write.csv(overlap_summary, "game_sample_exploration/13_sample_overlap.csv", row.names = FALSE)
cat("✓ Saved: 13_sample_overlap.csv\n\n")

# ========================================
# 7. CREATE COMBINED EXCEL FILE
# ========================================

cat("\n=== CREATING COMBINED EXCEL FILE ===\n")

# Create a list of all tables
tables_list <- list(
  "Overall_Sample_Sizes" = overall_summary,
  "Exclusion_Summary" = exclusion_summary,
  "Additional_Exclusions" = additional_exclusions,
  "Dictator_Comprehension" = dictator_comp_summary,
  "Ultimatum_Comprehension" = ultimatum_comp_summary,
  "Dictator_Missing" = dict_missing_summary,
  "Ultimatum_Missing" = ult_missing_summary,
  "Sample_Overlap" = overlap_summary
)

# Add country-level tables if they exist
if(exists("dict_country")) {
  tables_list$Dictator_Country_Exclusions <- dict_country
  tables_list$Ultimatum_Country_Exclusions <- ult_country
  tables_list$Country_Exclusion_Stats <- country_excl_summary
  tables_list$Dictator_Country_Missing <- dict_missing_country
  tables_list$Ultimatum_Country_Missing <- ult_missing_country
}

# Save to Excel
write_xlsx(tables_list, "game_sample_exploration/00_ALL_RESULTS_COMBINED.xlsx")
cat("✓ Saved: 00_ALL_RESULTS_COMBINED.xlsx (contains all tables)\n\n")






# ============================================================================
# COUNTRY-LEVEL SAMPLE CHARACTERISTICS TABLE
# ============================================================================

library(dplyr)
library(readxl)
library(stringr)


# ============================================================================
# 1. CREATE LANGUAGE MAPPING
# ============================================================================

language_mapping <- data.frame(
  code = c("SQI", "ENAM", "AR", "HYE", "BN", "BS", "BG", 
           "ZH-S", "ZH-T", "HR", "CS", "NL", "EN", "FR", "KAT",
           "DE", "EL", "HE", "HU", "IT", "JA", "SW", "MK", "FA",
           "PL", "PT", "RO", "RU", "SR", "SK", "ES", "TH", "TR",
           "UK", "TRUZ", "VI"),
  language = c("Albanian", "Amharic", "Arabic", "Armenian", "Bengali", 
               "Bosnian", "Bulgarian", "Chinese (Simplified)", "Chinese (Traditional)",
               "Croatian", "Czech", "Dutch", "English", "French", "Georgian",
               "German", "Greek", "Hebrew", "Hungarian", "Italian", "Japanese",
               "Kiswahili", "Macedonian", "Persian", "Polish", "Portuguese",
               "Romanian", "Russian", "Serbian", "Slovak", "Spanish", "Thai",
               "Turkish", "Ukrainian", "Uzbek", "Vietnamese"),
  stringsAsFactors = FALSE
)

# Function to extract language code and map to language name
extract_language_code <- function(lang_string) {
  if(is.na(lang_string)) return(NA)
  # Extract the first part before the first hyphen
  code <- str_extract(lang_string, "^[^-]+")
  return(code)
}

# Function to get language name from code
get_language_name <- function(code) {
  if(is.na(code)) return(NA)
  match_idx <- which(language_mapping$code == code)
  if(length(match_idx) > 0) {
    return(language_mapping$language[match_idx[1]])
  } else {
    return(code)  # Return original code if not found
  }
}

# ============================================================================
# 2. LOAD ETHNICITY LABELS
# ============================================================================

# Load ethnicity data
ethnicity_labels <- read_excel("DHF_ethnicities.xlsx")

# Create a function to get ethnicity label for a country and code
get_ethnicity_label <- function(country, eth_code) {
  if(is.na(country) || is.na(eth_code) || eth_code <= 0) return(NA)
  
  # Find the row for this country
  country_row <- ethnicity_labels %>% 
    filter(`Data Collection Country` == country)
  
  if(nrow(country_row) == 0) return(paste0("Code ", eth_code))
  
  # Get the appropriate ethnicity column
  eth_col_name <- paste0("Ethnicity ", eth_code, " (E", eth_code, ")")
  
  if(eth_col_name %in% names(country_row)) {
    label <- country_row[[eth_col_name]]
    if(!is.na(label) && label != "") {
      return(label)
    }
  }
  
  return(paste0("Code ", eth_code))
}

# ============================================================================
# 3. CREATE ENHANCED COUNTRY TABLE
# ============================================================================

country_table_simple <- dt4 %>%
  # Convert factors to numeric first
  mutate(
    ethnicity_num = as.numeric(as.character(ethnicity)),
    religion_num = as.numeric(as.character(religion)),
    gender_num = as.numeric(as.character(gender)),
    student_num = as.numeric(as.character(student))
  ) %>%
  group_by(country_res_full) %>%
  summarise(
    N = n(),
    
    # Language processing - extract unique languages
    `Languages Used` = {
      # Extract language codes from all Q_Language entries
      lang_codes <- sapply(Q_Language, extract_language_code)
      lang_codes <- lang_codes[!is.na(lang_codes)]
      
      if(length(lang_codes) == 0) return(NA_character_)
      
      # Get unique language codes and their counts
      unique_langs <- unique(lang_codes)
      lang_counts <- sapply(unique_langs, function(x) sum(lang_codes == x))
      lang_pcts <- round(lang_counts / length(lang_codes) * 100, 1)
      
      # Map codes to language names
      lang_names <- sapply(unique_langs, get_language_name)
      
      # Create formatted string
      if(length(unique_langs) > 1) {
        lang_summary <- paste0(lang_names, " (", lang_pcts, "%)")
        paste(lang_summary, collapse = "; ")
      } else {
        lang_names[1]
      }
    },
    
    # Primary language (most common)
    `Primary Language` = {
      lang_codes <- sapply(Q_Language, extract_language_code)
      lang_codes <- lang_codes[!is.na(lang_codes)]
      if(length(lang_codes) == 0) return(NA_character_)
      
      most_common_code <- names(which.max(table(lang_codes)))
      get_language_name(most_common_code)
    },
    
    # Number of unique languages used
    `N Languages` = {
      lang_codes <- sapply(Q_Language, extract_language_code)
      lang_codes <- lang_codes[!is.na(lang_codes)]
      length(unique(lang_codes))
    },
    
    # Age as "Mean (SD)" format
    `N Age` = sum(!is.na(Age)),
    `Age Mean (SD)` = paste0(
      round(mean(Age, na.rm = TRUE), 1), 
      " (", 
      round(sd(Age, na.rm = TRUE), 1), 
      ")"
    ),
    
    # Gender with N responded
    `N Gender` = sum(!is.na(gender_num)),
    `% Women` = round(sum(gender_num == 1, na.rm = TRUE) / sum(!is.na(gender_num)) * 100, 1),
    
    # Ethnicity with N responded and code
    `N Ethnicity` = sum(!is.na(ethnicity_num) & ethnicity_num > 0),
    `Largest Ethnicity Code` = {
      eth_valid <- ethnicity_num[!is.na(ethnicity_num) & ethnicity_num > 0]
      if(length(eth_valid) > 0) {
        as.numeric(names(which.max(table(eth_valid))))
      } else {
        NA_real_
      }
    },
    `% Largest Ethnic Group` = {
      eth_valid <- ethnicity_num[!is.na(ethnicity_num) & ethnicity_num > 0]
      if(length(eth_valid) > 0) {
        round(max(table(eth_valid)) / length(eth_valid) * 100, 1)
      } else {
        NA_real_
      }
    },
    
    # Religion with N responded and code
    `N Religion` = sum(!is.na(religion_num) & religion_num > 0),
    `Largest Religion Code` = {
      rel_valid <- religion_num[!is.na(religion_num) & religion_num > 0]
      if(length(rel_valid) > 0) {
        as.numeric(names(which.max(table(rel_valid))))
      } else {
        NA_real_
      }
    },
    `% Largest Religious Group` = {
      rel_valid <- religion_num[!is.na(religion_num) & religion_num > 0]
      if(length(rel_valid) > 0) {
        round(max(table(rel_valid)) / length(rel_valid) * 100, 1)
      } else {
        NA_real_
      }
    },
    
    # Student status with N responded
    `N Student` = sum(!is.na(student_num)),
    `% Students` = round(sum(student_num == 1, na.rm = TRUE) / sum(!is.na(student_num)) * 100, 1),
    
    .groups = 'drop'
  ) %>%
  arrange(desc(N))

# ============================================================================
# 4. ADD ETHNICITY LABELS
# ============================================================================

# Apply ethnicity labels
country_table_simple <- country_table_simple %>%
  rowwise() %>%
  mutate(
    `Largest Ethnicity Label` = get_ethnicity_label(country_res_full, `Largest Ethnicity Code`)
  ) %>%
  ungroup()

# Combine ethnicity label and percentage
country_table_simple <- country_table_simple %>%
  mutate(
    `Largest Ethnic Group (%)` = if_else(
      !is.na(`Largest Ethnicity Label`) & !is.na(`% Largest Ethnic Group`),
      paste0(`Largest Ethnicity Label`, " (", `% Largest Ethnic Group`, "%)"),
      NA_character_
    )
  )

# ============================================================================
# 5. ADD RELIGION LABELS AND COMBINE
# ============================================================================

country_table_simple <- country_table_simple %>%
  mutate(
    `Largest Religion Label` = case_when(
      `Largest Religion Code` == 1 ~ "None (atheist/agnostic)",
      `Largest Religion Code` == 2 ~ "Christianity",
      `Largest Religion Code` == 3 ~ "Islam",
      `Largest Religion Code` == 4 ~ "Judaism",
      `Largest Religion Code` == 5 ~ "Hinduism",
      `Largest Religion Code` == 6 ~ "Buddhism",
      `Largest Religion Code` == 7 ~ "Other",
      `Largest Religion Code` == 8 ~ "Prefer not to respond",
      TRUE ~ NA_character_
    ),
    # Combine religion label and percentage
    `Largest Religious Group (%)` = if_else(
      !is.na(`Largest Religion Label`) & !is.na(`% Largest Religious Group`),
      paste0(`Largest Religion Label`, " (", `% Largest Religious Group`, "%)"),
      NA_character_
    )
  )

# ============================================================================
# 6. RENAME AND REORDER COLUMNS
# ============================================================================

# Rename country column for cleaner output
country_table_simple <- country_table_simple %>%
  rename(Country = country_res_full)

# Select and reorder columns for final output
country_table_final <- country_table_simple %>%
  select(
    Country, N, 
    `Languages Used`, `Primary Language`, `N Languages`,
    `N Age`, `Age Mean (SD)`, 
    `N Gender`, `% Women`, 
    `N Ethnicity`, `Largest Ethnic Group (%)`,
    `N Religion`, `Largest Religious Group (%)`,
    `N Student`, `% Students`
  )



# Save as CSV
write.csv(country_table_final, 
          "country_characteristics_final.csv", 
          row.names = FALSE)


# Save detailed version with separate columns (for reference)
country_table_detailed <- country_table_simple %>%
  select(
    Country, N, 
    `Languages Used`, `Primary Language`, `N Languages`,
    `N Age`, `Age Mean (SD)`, 
    `N Gender`, `% Women`, 
    `N Ethnicity`, `Largest Ethnicity Code`, `Largest Ethnicity Label`, `% Largest Ethnic Group`, `Largest Ethnic Group (%)`,
    `N Religion`, `Largest Religion Code`, `Largest Religion Label`, `% Largest Religious Group`, `Largest Religious Group (%)`,
    `N Student`, `% Students`
  )

write.csv(country_table_detailed, 
          "country_characteristics_detailed.csv", 
          row.names = FALSE
          
          
          