PSG_Analysis_Final
================

## Statistical analysis for diabetes peer support group study

# 1. Baseline characteristics of the study sample

``` r
library(readxl)
library(tableone)
library(flextable)
library(officer)
```

    ## 
    ## Attaching package: 'officer'

    ## The following object is masked from 'package:readxl':
    ## 
    ##     read_xlsx

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(stringr)
library(tidyr)
library(lme4)
```

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'Matrix'

    ## The following objects are masked from 'package:tidyr':
    ## 
    ##     expand, pack, unpack

``` r
library(lmerTest)    
```

    ## 
    ## Attaching package: 'lmerTest'

    ## The following object is masked from 'package:lme4':
    ## 
    ##     lmer

    ## The following object is masked from 'package:stats':
    ## 
    ##     step

``` r
library(performance) 
library(emmeans)
```

    ## Welcome to emmeans.
    ## Caution: You lose important information if you filter this package's results.
    ## See '? untidy'

``` r
library(ggplot2)
library(writexl)
psg_data<- read_excel("Clean.xlsx")
```

    ## New names:
    ## • `In the last 3 months how much did you spend on Diabetes?Transport - Rs.` ->
    ##   `In the last 3 months how much did you spend on Diabetes?Transport -
    ##   Rs....72`
    ## • `In the last 3 months how much did you spend on Diabetes?Loss of wages - Rs.`
    ##   -> `In the last 3 months how much did you spend on Diabetes?Loss of wages -
    ##   Rs....73`
    ## • `In the last 3 months how much did you spend on Diabetes?Other cost - Rs.` ->
    ##   `In the last 3 months how much did you spend on Diabetes?Other cost -
    ##   Rs....74`
    ## • `Were you hospitalized in the last 3 months ?` -> `Were you hospitalized in
    ##   the last 3 months ?...75`
    ## • `If yes, Reason for hospitalization` -> `If yes, Reason for
    ##   hospitalization...76`
    ## • `In the last 3 months how much did you spend on Diabetes?Transport - Rs.` ->
    ##   `In the last 3 months how much did you spend on Diabetes?Transport -
    ##   Rs....215`
    ## • `In the last 3 months how much did you spend on Diabetes?Loss of wages - Rs.`
    ##   -> `In the last 3 months how much did you spend on Diabetes?Loss of wages -
    ##   Rs....216`
    ## • `In the last 3 months how much did you spend on Diabetes?Other cost - Rs.` ->
    ##   `In the last 3 months how much did you spend on Diabetes?Other cost -
    ##   Rs....217`
    ## • `Were you hospitalized in the last 3 months ?` -> `Were you hospitalized in
    ##   the last 3 months ?...218`
    ## • `If yes, Reason for hospitalization` -> `If yes, Reason for
    ##   hospitalization...219`

``` r
# 2. Define variables for Table 1
# List all variables to be included
all_vars <- c("Age", "Gender", "Religion", "Caste", "Marital status", "Financial status")
# List only the categorical variables
cat_vars <- c("Gender", "Religion", "Caste", "Marital status", "Financial status")

# 3. Generate the Table One object
# By default, CreateTableOne uses independent t-tests for continuous variables 
# and Chi-square tests for categorical variables.
table1_obj <- CreateTableOne(
  vars = all_vars, 
  strata = "intervention/ control", 
  data = psg_data, 
  factorVars = cat_vars, 
  test = TRUE
)
```

    ## Warning in ModuleReturnVarsExist(vars, data): The data frame does not have:
    ## Marital status Financial status Dropped

    ## Warning in ModuleReturnVarsExist(factorVars, data): The data frame does not
    ## have: Marital status Financial status Dropped

``` r
# 4. Format the table into a matrix
# test = TRUE includes the p-values; exact = FALSE ensures Chi-square is used.
table1_matrix <- print(
  table1_obj, 
  nonnormal = NULL, # Change if any continuous variables are non-normal (uses Wilcoxon)
  catDigits = 1, 
  contDigits = 1, 
  pDigits = 3, 
  printToggle = FALSE, 
  showAllLevels = TRUE, 
  exact = FALSE 
)

# 5. Convert the matrix to a data frame for flextable processing
table1_df <- as.data.frame(table1_matrix)
table1_df <- cbind(Characteristic = rownames(table1_matrix), table1_df)
rownames(table1_df) <- NULL

# Clean up the row names/characteristics column for a cleaner look
table1_df$Characteristic <- gsub("mean \\(SD\\)", "", table1_df$Characteristic)
table1_df$Characteristic <- gsub("\\(%)", "", table1_df$Characteristic)

# 6. Style the table using flextable (Publication Ready Format)
ft <- flextable(table1_df) %>%
  set_header_labels(
    Characteristic = "Baseline Characteristic",
    p = "p-value"
  ) %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  bold(part = "header") %>%
  align(j = 2:ncol(table1_df), align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  # Classic APA/Medical Journal borders (top, bottom, and under header)
  border_remove() %>%
  hline_top(border = fp_border(width = 1.5), part = "header") %>%
  hline_bottom(border = fp_border(width = 1.5), part = "header") %>%
  hline_bottom(border = fp_border(width = 1.5), part = "body") %>%
  autofit() %>%
  # Footnote explaining the statistical tests
  add_footer_lines("Data presented as mean (SD) for continuous variables and n (%) for categorical variables.") %>%
  add_footer_lines("p-values calculated using independent t-test for continuous variables and Chi-square test for categorical variables.") %>%
  font(fontname = "Times New Roman", part = "footer") %>%
  fontsize(size = 9, part = "footer")

# 7. Export the beautiful table directly to a Word Document
doc <- read_docx() %>%
  body_add_par("Table 1: Baseline Characteristics of Study Participants", style = "heading 1") %>%
  body_add_flextable(ft)

# Save the document to your working directory
print(doc, target = "Baseline_Characteristics_Table.docx")

cat("Success! 'Baseline_Characteristics_Table.docx' has been created in your working directory.")
```

    ## Success! 'Baseline_Characteristics_Table.docx' has been created in your working directory.

\#2. Baseline Summary Diabetes Self Care Activities Scores and Diabetes
Distress Scores

``` r
# 2. Define the continuous score variables for comparison
score_vars <- c("sdsca0", "Diet0", "Exercise0", "Foot0", "Test0", "Med0", "Smoke0")

# 3. Create the Table One object
# By default, CreateTableOne treats numeric variables as continuous and 
# automatically performs an independent t-test when comparing across two groups.
table1_obj <- CreateTableOne(
  vars = score_vars, 
  strata = "intervention/ control", 
  data = psg_data, 
  test = TRUE
)

# 4. Format and extract the table statistics into a matrix
table1_matrix <- print(
  table1_obj, 
  nonnormal = NULL,     # Treats all variables as normally distributed (Mean and SD)
  contDigits = 2,       # Sets decimals to 2 places for scores
  pDigits = 3,          # Sets decimals to 3 places for p-values
  printToggle = FALSE, 
  exact = FALSE 
)

# 5. Convert the matrix to a data frame for custom label formatting
table1_df <- as.data.frame(table1_matrix)
table1_df <- cbind(Characteristic = rownames(table1_matrix), table1_df)
rownames(table1_df) <- NULL

# Clean up the raw column labels to look highly professional for publication
table1_df <- table1_df %>%
  mutate(Characteristic = case_when(
    Characteristic == "n"                  ~ "Sample Size (n)",
    Characteristic == "sdsca0 (mean (SD))" ~ "Overall Self-Management Score",
    Characteristic == "Diet0 (mean (SD))"  ~ "Diet Score",
    Characteristic == "Exercise0 (mean (SD))" ~ "Exercise Score",
    Characteristic == "Foot0 (mean (SD))"  ~ "Foot Care Score",
    Characteristic == "Test0 (mean (SD))"  ~ "Blood Testing Score",
    Characteristic == "Med0 (mean (SD))"   ~ "Medication Adherence Score",
    Characteristic == "Smoke0 (mean (SD))" ~ "Non-Smoking Score",
    TRUE ~ Characteristic
  ))

# 6. Style the table using flextable (Standard Academic Journal Style)
ft <- flextable(table1_df) %>%
  set_header_labels(
    Characteristic = "Participant Scores",
    p = "p-value"
  ) %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 11, part = "all") %>%
  bold(part = "header") %>%
  align(j = 2:ncol(table1_df), align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  # Classic medical journal borders (top, bottom, and below header only)
  border_remove() %>%
  hline_top(border = fp_border(width = 1.5), part = "header") %>%
  hline_bottom(border = fp_border(width = 1.5), part = "header") %>%
  hline_bottom(border = fp_border(width = 1.5), part = "body") %>%
  autofit() %>%
  # Add footnotes explaining abbreviations and statistical methods
  add_footer_lines("Data are presented as Mean (SD) unless specified otherwise.") %>%
  add_footer_lines("p-values are calculated using the independent-samples t-test.") %>%
  font(fontname = "Times New Roman", part = "footer") %>%
  fontsize(size = 9, part = "footer")

# 7. Create and export the styled table to a Word Document
doc <- read_docx() %>%
  body_add_par("Table 1: Comparison of Baseline Self-Management Scores Between Groups", style = "heading 1") %>%
  body_add_flextable(ft)

# Save the document to your working directory
print(doc, target = "Scores_Comparison_Table.docx")

cat("Success! 'Scores_Comparison_Table.docx' has been created in your working directory.")
```

    ## Success! 'Scores_Comparison_Table.docx' has been created in your working directory.

# 3. Fidelity to the peer support and diabetes self management education interventions

``` r
# ==========================================
# 1. DATA IMPORT & CLEANING
# ==========================================

group_var <- "intervention/ control"

# Clean empty grouping rows and strip white spaces
psg_data <- psg_data %>% 
  filter(!is.na(.data[[group_var]])) %>%
  mutate(clean_group = trimws(as.character(.data[[group_var]])))

# Dynamically identify the group labels to avoid hardcoding errors
unique_groups <- unique(psg_data$clean_group)
int_label     <- unique_groups[str_detect(tolower(unique_groups), "int|peer|1")][1]
ctrl_label    <- unique_groups[!unique_groups %in% int_label][1]


# ==========================================
# 2. ANALYSIS PART 1: Intervention Meetings
# ==========================================
# Subset for intervention arm participants only
intervention_df <- psg_data %>% filter(clean_group == int_label)

# Calculate Mean and SD for Meetings
mean_meetings      <- mean(intervention_df$`No of meetings`, na.rm = TRUE)
sd_meetings        <- sd(intervention_df$`No of meetings`, na.rm = TRUE)
mean_prop_meetings <- mean(intervention_df$`Proportion of meetings`, na.rm = TRUE)
sd_prop_meetings   <- sd(intervention_df$`Proportion of meetings`, na.rm = TRUE)


# ==========================================
# 3. ANALYSIS PART 2: Video Metrics (All Participants)
# ==========================================
# Identify columns containing specified phrases dynamically
video_received_cols <- names(psg_data)[str_detect(names(psg_data), "(?i)did you receive the video")]
video_viewed_cols   <- names(psg_data)[str_detect(names(psg_data), "(?i)How many times you saw the video")]

# Run data transformations for all participants
processed_df <- psg_data %>%
  # Safe-cast video count rows to numeric values
  mutate(across(all_of(video_viewed_cols), ~ as.numeric(as.character(.)))) %>%
  rowwise() %>%
  mutate(
    # Count total 'Yes' string matches for video reception
    total_yes_received   = sum(c_across(all_of(video_received_cols)) == "Yes", na.rm = TRUE),
    prop_videos_received = total_yes_received / length(video_received_cols),
    
    # Total number of times video columns were viewed across items
    total_times_watched  = sum(c_across(all_of(video_viewed_cols)), na.rm = TRUE)
  ) %>%
  ungroup()
```

    ## Warning: There were 2 warnings in `mutate()`.
    ## The first warning was:
    ## ℹ In argument: `across(all_of(video_viewed_cols),
    ##   ~as.numeric(as.character(.)))`.
    ## Caused by warning:
    ## ! NAs introduced by coercion
    ## ℹ Run `dplyr::last_dplyr_warnings()` to see the 1 remaining warning.

``` r
# Extract group summaries for videos received (Proportion)
summary_received <- processed_df %>%
  group_by(clean_group) %>%
  summarise(mean_sd = sprintf("%.2f (%.2f)", mean(prop_videos_received, na.rm = TRUE), sd(prop_videos_received, na.rm = TRUE)), .groups = 'drop')

# Extract group summaries for video viewing counts (Mean)
summary_watched <- processed_df %>%
  group_by(clean_group) %>%
  summarise(mean_sd = sprintf("%.2f (%.2f)", mean(total_times_watched, na.rm = TRUE), sd(total_times_watched, na.rm = TRUE)), .groups = 'drop')


# ==========================================
# 4. STRUCTURE THE UNIFIED DATAFRAME
# ==========================================
int_received  <- summary_received$mean_sd[summary_received$clean_group == int_label]
ctrl_received <- summary_received$mean_sd[summary_received$clean_group == ctrl_label]

int_watched   <- summary_watched$mean_sd[summary_watched$clean_group == int_label]
ctrl_watched  <- summary_watched$mean_sd[summary_watched$clean_group == ctrl_label]

fidelity_table <- data.frame(
  Fidelity_Metric = c(
    "Intervention Meetings (Intervention Arm Only)",
    "  Number of meetings attended [Mean (SD)]",
    "  Proportion of meetings attended [Mean (SD)]",
    "Video Interventions (Both Arms)",
    "  Proportion of digital videos received [Mean (SD)]",
    "  Total times videos were seen [Mean (SD)]"
  ),
  Intervention_Group = c(
    "",
    sprintf("%.2f (%.2f)", mean_meetings, sd_meetings),
    sprintf("%.2f (%.2f)", mean_prop_meetings, sd_prop_meetings),
    "",
    int_received,
    int_watched
  ),
  Control_Group = c(
    "",
    "N/A*",
    "N/A*",
    "",
    ctrl_received,
    ctrl_watched
  ),
  stringsAsFactors = FALSE
)


# ==========================================
# 5. CREATE THE PUBLICATION READY TABLE
# ==========================================
ft <- flextable(fidelity_table) %>%
  set_header_labels(
    Fidelity_Metric    = "Fidelity Characteristic",
    Intervention_Group = paste(int_label, "Group"),
    Control_Group      = paste(ctrl_label, "Group")
  ) %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 11, part = "all") %>%
  bold(i = c(1, 4), bold = TRUE) %>%      # Bold subheaders 
  bold(part = "header") %>%               # Bold columns
  align(j = 2:3, align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  
  # Standard peer-reviewed 3-line table layout configuration
  border_remove() %>%
  hline_top(border = fp_border(width = 1.5), part = "header") %>%
  hline_bottom(border = fp_border(width = 1.5), part = "header") %>%
  hline_bottom(border = fp_border(width = 1.5), part = "body") %>%
  autofit() %>%
  
  # Footer disclosures
  add_footer_lines("*Note: Meeting metrics are unique to the active protocol structure of the intervention strategy.") %>%
  font(fontname = "Times New Roman", part = "footer") %>%
  fontsize(size = 9, part = "footer")


# ==========================================
# 6. EXPORT DIRECTLY TO MICROSOFT WORD
# ==========================================
# Define a paragraph format with 12pt space after it
title_style <- fp_text(font.size = 16, bold = TRUE, font.family = "Times New Roman")

doc <- read_docx() %>%
  # Add the title with built-in spacing properties
  body_add_fpar(
    fpar(
      ftext("Table: Evaluation of Intervention Fidelity and Program Delivery Adherence", prop = title_style),
      fp_p = fp_par(padding.bottom = 12) # This replaces body_add_spacing cleanly!
    )
  ) %>%
  # Add the table right below it
  body_add_flextable(ft)

# Save the file
print(doc, target = "Fidelity_Analysis_Table.docx")
cat("\nSuccess! 'Fidelity_Analysis_Table.docx' has been saved to your workspace.\n")
```

    ## 
    ## Success! 'Fidelity_Analysis_Table.docx' has been saved to your workspace.

# 4. Effect of peer support on overall diabetes self management

``` r
group_var   <- "intervention/ control"
cluster_var <- "PSG group number"

psg_data <- psg_data %>%
  filter(!is.na(.data[[group_var]]), !is.na(.data[[cluster_var]])) %>%
  mutate(!!sym(group_var) := trimws(as.character(.data[[group_var]])))

time_cols <- c("sdsca1", "sdsca2", "sdsca3", "sdsca4", "sdsca5", "sdsca6", "sdsca7", "sdsca8")

# ==========================================
# 2. RESHAPE FROM WIDE TO LONG FORMAT
# ==========================================
data_long <- psg_data %>%
  pivot_longer(
    cols = all_of(time_cols),
    names_to = "Time_Point",
    values_to = "SDSCA_Score"
  ) %>%
  mutate(
    Time = case_when(
      Time_Point == "sdsca1" ~ 3,  Time_Point == "sdsca2" ~ 6,
      Time_Point == "sdsca3" ~ 9,  Time_Point == "sdsca4" ~ 12,
      Time_Point == "sdsca5" ~ 15, Time_Point == "sdsca6" ~ 18,
      Time_Point == "sdsca7" ~ 21, Time_Point == "sdsca8" ~ 24
    )
  )

names(data_long)[names(data_long) == group_var]   <- "Group"
names(data_long)[names(data_long) == cluster_var] <- "ClusterID"

# ==========================================
# 3. FIXED LINEAR MIXED MODELS (Simplified Random Effects)
# ==========================================
# Changed from (1 + Time | ClusterID) to (1 | ClusterID) to eliminate singularity
null_model <- lmer(SDSCA_Score ~ 1 + (1 | ClusterID), data = data_long, REML = FALSE)

full_model <- lmer(
  SDSCA_Score ~ Group * Time + sdsca0 + Age + Gender + Religion + Caste + 
                `Marital_status` + `Financial_status` + (1 | ClusterID), 
  data = data_long, 
  REML = FALSE
)

# ==========================================
# 4. DIRECT VARIANCE EXTRACTION (Bypasses performance package locks)
# ==========================================
# Extract the variance components from the model object
variance_data <- as.data.frame(VarCorr(full_model))

# 1. Cluster Random Intercept Variance (sigma^2_alpha)
rand_intercept_var <- variance_data$vcov[variance_data$grp == "ClusterID"]

# 2. Residual Error Variance (sigma^2_e)
residual_var <- variance_data$vcov[variance_data$grp == "Residual"]

# 3. Fixed Effects Variance (sigma^2_f)
fixed_predictions <- predict(full_model, re.form = NA) # Predictions based ONLY on fixed variables
fixed_effects_var <- var(fixed_predictions)

# ==========================================
# 5. MANUAL CALCULATION OF FIT INDICES
# ==========================================
# Standard formulas defined by Nakagawa & Schielzeth
icc_value    <- rand_intercept_var / (rand_intercept_var + residual_var)
marginal_r2  <- fixed_effects_var / (fixed_effects_var + rand_intercept_var + residual_var)
cond_r2      <- (fixed_effects_var + rand_intercept_var) / (fixed_effects_var + rand_intercept_var + residual_var)

# Extract Model Summary Matrix Coefficients
model_summary    <- summary(full_model)$coefficients
fixed_effects_df <- as.data.frame(model_summary)
fixed_effects_df <- cbind(Parameter = rownames(model_summary), fixed_effects_df)
rownames(fixed_effects_df) <- NULL

colnames(fixed_effects_df) <- c("Parameter", "Estimate", "Std_Error", "df", "t_value", "p_value")

fixed_effects_df <- fixed_effects_df %>%
  mutate(
    `Estimate (SE)` = sprintf("%.2f (%.2f)", Estimate, Std_Error),
    `p-value` = ifelse(p_value < 0.001, "<.001", sprintf("%.3f", p_value))
  ) %>%
  select(Parameter, `Estimate (SE)`, `p-value`)

# Create rows for fit parameters to match the table bottom margin
fit_rows <- data.frame(
  Parameter = c("Random Effects & Model Fit Measures", 
                "  Intraclass Correlation Coefficient (ICC)", 
                "  Marginal R-squared (Fixed Effects Variance)", 
                "  Conditional R-squared (Total Model Variance)"),
  `Estimate (SE)` = c("", sprintf("%.3f", icc_value), sprintf("%.3f", marginal_r2), sprintf("%.3f", cond_r2)),
  `p-value` = c("", "", "", ""),
  stringsAsFactors = FALSE
)

colnames(fit_rows) <- colnames(fixed_effects_df)
lmm_table_df       <- rbind(fixed_effects_df, fit_rows)

# ==========================================
# 6. GENERATE PUBLICATION READY FLEXTABLE
# ==========================================
lmm_ft <- flextable(lmm_table_df) %>%  
  set_header_labels(
    Parameter     = "Fixed Effect / Parameter Fit Model",
    `Estimate (SE)` = "Coefficient Estimate (SE)",
    `p-value`     = "p-value"
  ) %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  bold(i = which(lmm_table_df$Parameter == "Random Effects & Model Fit Measures"), bold = TRUE) %>%
  bold(part = "header") %>%
  align(j = 2:3, align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  
  border_remove() %>%
  hline_top(border = fp_border(width = 1.5), part = "header") %>%
  hline_bottom(border = fp_border(width = 1.5), part = "header") %>%
  hline_bottom(border = fp_border(width = 1.5), part = "body") %>%
  autofit() %>%
  
  add_footer_lines("Dependent Variable: SDSCA Self-Management score measured across 8 tracking loops (Months 3 to 24).") %>%
  add_footer_lines("Model specifies a random intercept structured nested within 'PSG group number' clusters.") %>%
  add_footer_lines("Marginal R² represents variance explained by fixed variables; Conditional R² indicates total model variance representation.") %>%
  font(fontname = "Times New Roman", part = "footer") %>%
  fontsize(size = 9, part = "footer")

# ==========================================
# 7. EXPORT DIRECTLY TO MICROSOFT WORD
# ==========================================
title_style <- fp_text(font.size = 14, bold = TRUE, font.family = "Times New Roman")

lmm_doc <- read_docx() %>% 
  body_add_fpar(
    fpar(
      ftext("Table: Linear Mixed-Effects Model Predicting Diabetes Self-Management Longitudinal Scores", prop = title_style),
      fp_p = fp_par(padding.bottom = 12)
    )
  ) %>%
  body_add_flextable(lmm_ft)

print(lmm_doc, target = "Linear_Mixed_Model_Output.docx")

cat("\nSUCCESS! Singularity resolved. Check your directory for 'Linear_Mixed_Model_Output.docx'.\n")
```

    ## 
    ## SUCCESS! Singularity resolved. Check your directory for 'Linear_Mixed_Model_Output.docx'.

# 5. Effect of peer support on diabetes self management – Intention to Treat versus Per Protocol Analysis

``` r
group_var   <- "intervention/ control"
cluster_var <- "PSG group number"
ltfu_var    <- "LTFU"

# Standardize text and filter out completely blank structural rows
psg_data <- psg_data %>%
  filter(!is.na(.data[[group_var]]), !is.na(.data[[cluster_var]])) %>%
  mutate(
    clean_group = trimws(as.character(.data[[group_var]])),
    clean_ltfu  = ifelse(is.na(.data[[ltfu_var]]), "COMPLETED", trimws(as.character(.data[[ltfu_var]])))
  )

time_cols <- c("sdsca1", "sdsca2", "sdsca3", "sdsca4", "sdsca5", "sdsca6", "sdsca7", "sdsca8")

# ==========================================
# 2. HELPER FUNCTION TO RESHAPE AND FIT MODEL
# ==========================================
# This function automates the extraction so we can build ITT and PP uniformly
run_lmm_pipeline <- function(data_subset) {
  
  # Reshape wide to long
  data_long <- data_subset %>%
    pivot_longer(cols = all_of(time_cols), names_to = "Time_Point", values_to = "SDSCA_Score") %>%
    mutate(
      Time = case_when(
        Time_Point == "sdsca1" ~ 3,  Time_Point == "sdsca2" ~ 6,
        Time_Point == "sdsca3" ~ 9,  Time_Point == "sdsca4" ~ 12,
        Time_Point == "sdsca5" ~ 15, Time_Point == "sdsca6" ~ 18,
        Time_Point == "sdsca7" ~ 21, Time_Point == "sdsca8" ~ 24
      )
    )
  
  # Rename for internal formula consistency
  names(data_long)[names(data_long) == "clean_group"] <- "Group"
  names(data_long)[names(data_long) == cluster_var]  <- "ClusterID"
  
  # Fit Model
  model <- lmer(
    SDSCA_Score ~ Group * Time + sdsca0 + Age + Gender + Religion + Caste + 
                  `Marital_status` + `Financial_status` + (1 | ClusterID), 
    data = data_long, REML = FALSE
  )
  
  # Extract Coefficients
  coef_matrix <- as.data.frame(summary(model)$coefficients)
  coef_matrix$Parameter <- rownames(coef_matrix)
  
  # Format Estimates and P-values
  coef_cleaned <- coef_matrix %>%
    mutate(
      Est_SE = sprintf("%.2f (%.2f)", Estimate, `Std. Error`),
      P_Val  = ifelse(`Pr(>|t|)` < 0.001, "<.001", sprintf("%.3f", `Pr(>|t|)`))
    ) %>%
    select(Parameter, Est_SE, P_Val)
  
  # Manual Fit Statistics Extraction (Bypasses performance package lockouts)
  var_data           <- as.data.frame(VarCorr(model))
  rand_intercept_var <- var_data$vcov[var_data$grp == "ClusterID"]
  residual_var       <- var_data$vcov[var_data$grp == "Residual"]
  fixed_var          <- var(predict(model, re.form = NA))
  
  icc_val     <- rand_intercept_var / (rand_intercept_var + residual_var)
  marginal_r2 <- fixed_var / (fixed_var + rand_intercept_var + residual_var)
  cond_r2     <- (fixed_var + rand_intercept_var) / (fixed_var + rand_intercept_var + residual_var)
  
  # Return data frame containing both coefficients and fit metrics
  fit_rows <- data.frame(
    Parameter = c("Model Fit Statistics", 
                  "  Intraclass Correlation (ICC)", 
                  "  Marginal R-squared (R²m)", 
                  "  Conditional R-squared (R²c)"),
    Est_SE = c("", sprintf("%.3f", icc_val), sprintf("%.3f", marginal_r2), sprintf("%.3f", cond_r2)),
    P_Val = c("", "", "", ""),
    stringsAsFactors = FALSE
  )
  
  return(rbind(coef_cleaned, fit_rows))
}

# ==========================================
# 3. RUN MODEL PIPELINES (ITT vs PP)
# ==========================================

# ITT: Includes all randomized individuals
itt_results <- run_lmm_pipeline(psg_data)

# PP: Excludes those marked as 'LOST' in the LTFU tracking column
pp_data <- psg_data %>% filter(clean_ltfu != "LOST")
pp_results <- run_lmm_pipeline(pp_data)


# ==========================================
# 4. MERGE RESULTS SIDE-BY-SIDE
# ==========================================
comparison_table <- itt_results %>%
  inner_join(pp_results, by = "Parameter", suffix = c("_ITT", "_PP"))


# ==========================================
# 5. GENERATE PUBLICATION READY FLEXTABLE
# ==========================================
ft_compare <- flextable(comparison_table) %>%
  # Construct a multi-level spanning header layout
  set_header_labels(
    Parameter    = "Fixed Effect / Model Parameter",
    Est_SE_ITT   = "Estimate (SE)", P_Val_ITT = "p-value",
    Est_SE_PP    = "Estimate (SE)", P_Val_PP  = "p-value"
  ) %>%
  add_header_row(
    values = c("", "Intention-to-Treat (ITT) Model", "Per-Protocol (PP) Model"),
    colwidths = c(1, 2, 2)
  ) %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  bold(part = "header") %>%
  bold(i = which(comparison_table$Parameter == "Model Fit Statistics"), bold = TRUE) %>%
  align(j = 2:5, align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  
  # Standard Medical Journal Layout (Three-Line Horizon Borders)
  border_remove() %>%
  hline_top(border = fp_border(width = 1.5), part = "header") %>%
  hline_bottom(border = fp_border(width = 1.0), part = "header") %>%
  hline_bottom(border = fp_border(width = 1.5), part = "body") %>%
  autofit() %>%
  
  # Explanatory Footnotes
  add_footer_lines("ITT analysis keeps all participant records; PP analysis removes participants tracked as 'LOST' in the LTFU vector.") %>%
  add_footer_lines("Models account for random intercept grouping clusters matching your unique 'PSG group number' constraints.") %>%
  font(fontname = "Times New Roman", part = "footer") %>%
  fontsize(size = 9, part = "footer")


# ==========================================
# 6. EXPORT DIRECTLY TO MICROSOFT WORD
# ==========================================
title_style <- fp_text(font.size = 14, bold = TRUE, font.family = "Times New Roman")

doc_compare <- read_docx() %>% 
  body_add_fpar(
    fpar(
      ftext("Table: Side-by-Side Comparison of ITT and PP Linear Mixed-Effects Model Structures", prop = title_style),
      fp_p = fp_par(padding.bottom = 12)
    )
  ) %>%
  body_add_flextable(ft_compare)

print(doc_compare, target = "ITT_vs_PP_Comparison_Report.docx")

cat("\nSUCCESS! Check your directory for 'ITT_vs_PP_Comparison_Report.docx'.\n")
```

    ## 
    ## SUCCESS! Check your directory for 'ITT_vs_PP_Comparison_Report.docx'.

# 6. Effect of peer support on diabetes self management – dose response relationship

``` r
group_var   <- "intervention/ control"
cluster_var <- "PSG group number"
dose_var    <- "Proportion of meetings"

# Drop completely empty rows and clean text fields
psg_data <- psg_data %>%
  filter(!is.na(.data[[group_var]]), !is.na(.data[[cluster_var]])) %>%
  mutate(
    clean_group = trimws(as.character(.data[[group_var]])),
    # Handle missing dose entries safely by coercing to 0 (unexposed/baseline)
    clean_dose  = ifelse(is.na(.data[[dose_var]]), 0, as.numeric(.data[[dose_var]]))
  )

time_cols <- c("sdsca1", "sdsca2", "sdsca3", "sdsca4", "sdsca5", "sdsca6", "sdsca7", "sdsca8")

# ==========================================
# 2. LONG FORMAT DATA RESHAPING
# ==========================================
data_long <- psg_data %>%
  pivot_longer(
    cols = all_of(time_cols),
    names_to = "Time_Point",
    values_to = "SDSCA_Score"
  ) %>%
  mutate(
    Time = case_when(
      Time_Point == "sdsca1" ~ 3,  Time_Point == "sdsca2" ~ 6,
      Time_Point == "sdsca3" ~ 9,  Time_Point == "sdsca4" ~ 12,
      Time_Point == "sdsca5" ~ 15, Time_Point == "sdsca6" ~ 18,
      Time_Point == "sdsca7" ~ 21, Time_Point == "sdsca8" ~ 24
    )
  )

# Standardize column mappings for formula inputs
names(data_long)[names(data_long) == "clean_group"] <- "Group"
names(data_long)[names(data_long) == cluster_var]  <- "ClusterID"

# ==========================================
# 3. FIT THE MODELS (Null vs Model 1 vs Model 2)
# ==========================================

# Base Null Intercept Model
null_model <- lmer(SDSCA_Score ~ 1 + (1 | ClusterID), data = data_long, REML = FALSE)

# Model 1: Primary Fixed Effects Model
model1 <- lmer(
  SDSCA_Score ~ Group * Time + sdsca0 + Age + Gender + Religion + Caste + 
                Marital_status + Financial_status + (1 | ClusterID), 
  data = data_long, REML = FALSE
)

# Model 2: Dose-Response Model (Adding the Proportion of Meetings Attended)
model2 <- lmer(
  SDSCA_Score ~ Group * Time + sdsca0 + Age + Gender + Religion + Caste + 
                Marital_status + Financial_status + clean_dose + (1 | ClusterID), 
  data = data_long, REML = FALSE
)

# ==========================================
# 4. COEFFICIENT EXTRACTOR HELPER FUNCTION
# ==========================================
extract_model_metrics <- function(model_obj) {
  # Extract fixed effect values
  coef_data <- as.data.frame(summary(model_obj)$coefficients)
  coef_data$Parameter <- rownames(coef_data)
  
  coef_cleaned <- coef_data %>%
    mutate(
      Est_SE = sprintf("%.2f (%.2f)", Estimate, `Std. Error`),
      P_Val  = ifelse(`Pr(>|t|)` < 0.001, "<.001", sprintf("%.3f", `Pr(>|t|)`))
    ) %>%
    select(Parameter, Est_SE, P_Val)
  
  # Extract Random Variance metrics manually to guarantee calculations display
  var_data           <- as.data.frame(VarCorr(model_obj))
  rand_intercept_var <- var_data$vcov[var_data$grp == "ClusterID"]
  residual_var       <- var_data$vcov[var_data$grp == "Residual"]
  fixed_var          <- var(predict(model_obj, re.form = NA))
  
  icc_val     <- rand_intercept_var / (rand_intercept_var + residual_var)
  marginal_r2 <- fixed_var / (fixed_var + rand_intercept_var + residual_var)
  cond_r2     <- (fixed_var + rand_intercept_var) / (fixed_var + rand_intercept_var + residual_var)
  
  fit_summary <- data.frame(
    Parameter = c("Model Fit Statistics", 
                  "  Intraclass Correlation (ICC)", 
                  "  Marginal R-squared (R²m)", 
                  "  Conditional R-squared (R²c)"),
    Est_SE = c("", sprintf("%.3f", icc_val), sprintf("%.3f", marginal_r2), sprintf("%.3f", cond_r2)),
    P_Val = c("", "", "", ""),
    stringsAsFactors = FALSE
  )
  
  return(rbind(coef_cleaned, fit_summary))
}

# Run extraction logic across our models
m1_summary <- extract_model_metrics(model1)
m2_summary <- extract_model_metrics(model2)

# ==========================================
# 5. SIDE-BY-SIDE MERGE AND MATRIX STRUCTURING
# ==========================================
# Use full_join to preserve the unique 'clean_dose' parameter row inside Model 2
comparison_table <- full_join(m1_summary, m2_summary, by = "Parameter", suffix = c("_M1", "_M2"))

# Clean up empty text fragments generated by structural mismatch spacing
comparison_table[is.na(comparison_table)] <- "—"

# Ensure the summary metrics stay clipped cleanly to the bottom rows of your data frame
fit_indices <- comparison_table %>% filter(str_detect(Parameter, "Model Fit|Correlation|R-squared"))
coef_indices <- comparison_table %>% filter(!Parameter %in% fit_indices$Parameter)
final_table_df <- rbind(coef_indices, fit_indices)

# ==========================================
# 6. CONSTRUCT PUBLICATION FLEXTABLE
# ==========================================
ft_dose <- flextable(final_table_df) %>%
  set_header_labels(
    Parameter   = "Fixed Effect / Model Parameter",
    Est_SE_M1   = "Estimate (SE)", P_Val_M1 = "p-value",
    Est_SE_M2   = "Estimate (SE)", P_Val_M2 = "p-value"
  ) %>%
  add_header_row(
    values = c("", "Model 1: Primary Adjusted LMM", "Model 2: Dose-Response Adherence LMM"),
    colwidths = c(1, 2, 2)
  ) %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  bold(part = "header") %>%
  bold(i = which(final_table_df$Parameter == "Model Fit Statistics"), bold = TRUE) %>%
  align(j = 2:5, align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  
  # Classical Medical Journal Horizontal Borders (Three-Line Standard Layout)
  border_remove() %>%
  hline_top(border = fp_border(width = 1.5), part = "header") %>%
  hline_bottom(border = fp_border(width = 1.0), part = "header") %>%
  hline_bottom(border = fp_border(width = 1.5), part = "body") %>%
  autofit() %>%
  
  # Scholarly Disclosures
  add_footer_lines("Dependent Variable: Longitudinal SDSCA Self-Management scores measured at 8 follow-up points (Months 3 to 24).") %>%
  add_footer_lines("Model 2 evaluates program delivery intensity by incorporating 'Proportion of meetings' attended as a fixed continuous covariate.") %>%
  add_footer_lines("All models account for random intercept clustering matching your 'PSG group number' parameter constraints.") %>%
  font(fontname = "Times New Roman", part = "footer") %>%
  fontsize(size = 9, part = "footer")

# ==========================================
# 7. WRITE OUT DIRECTLY TO MICROSOFT WORD
# ==========================================
title_style <- fp_text(font.size = 14, bold = TRUE, font.family = "Times New Roman")

lmm_doc <- read_docx() %>% 
  body_add_fpar(
    fpar(
      ftext("Table: Side-by-Side Model Comparison of Main Analysis and Session Attendance Dose-Response Analysis", prop = title_style),
      fp_p = fp_par(padding.bottom = 12)
    )
  ) %>%
  body_add_flextable(ft_dose)

print(lmm_doc, target = "LMM_Dose_Response_Comparison.docx")

cat("\nDone! 'LMM_Dose_Response_Comparison.docx' has been successfully generated in your directory.\n")
```

    ## 
    ## Done! 'LMM_Dose_Response_Comparison.docx' has been successfully generated in your directory.

# 7. Visualisation of effect of peer support on overall diabetes self management – trajectory plot

``` r
psg_data <- psg_data %>%
  mutate(ParticipantID = row_number()) %>%
  filter(!is.na(.data[[group_var]]), !is.na(.data[[cluster_var]])) %>%
  mutate(clean_group = trimws(as.character(.data[[group_var]])))

# Map all 9 time points (including baseline sdsca0)
time_cols <- c("sdsca0", "sdsca1", "sdsca2", "sdsca3", "sdsca4", "sdsca5", "sdsca6", "sdsca7", "sdsca8")

# ==========================================
# 2. RESHAPE WIDE TO LONG FORMAT
# ==========================================
data_long <- psg_data %>%
  pivot_longer(
    cols = all_of(time_cols),
    names_to = "Time_Point",
    values_to = "SDSCA_Score"
  ) %>%
  mutate(
    Month = case_when(
      Time_Point == "sdsca0" ~ 0,  Time_Point == "sdsca1" ~ 3,  
      Time_Point == "sdsca2" ~ 6,  Time_Point == "sdsca3" ~ 9,  
      Time_Point == "sdsca4" ~ 12, Time_Point == "sdsca5" ~ 15, 
      Time_Point == "sdsca6" ~ 18, Time_Point == "sdsca7" ~ 21, 
      Time_Point == "sdsca8" ~ 24
    )
  )

# Align system-wide variable definitions
names(data_long)[names(data_long) == "clean_group"] <- "Group"
names(data_long)[names(data_long) == cluster_var]  <- "ClusterID"

# Clean group string labels dynamically to prevent case bugs
unique_groups <- unique(data_long$Group)
int_label     <- unique_groups[stringr::str_detect(tolower(unique_groups), "int|peer|1")][1]
ctrl_label    <- unique_groups[!unique_groups %in% int_label][1]


# ==========================================
# 3. FIT THE MODEL & EXTRACT PREDICTED MEANS
# ==========================================
# Fit a random intercept mixed model tracking continuous Month variations
lmm_fit <- lmer(SDSCA_Score ~ Group * Month + (1 | ClusterID), data = data_long, REML = FALSE)

# Fix: Explicitly invoke emmeans using the :: operator to bypass library loading errors
predicted_means <- emmeans::emmeans(
  lmm_fit, 
  ~ Group | Month, 
  at = list(Month = c(0, 3, 6, 9, 12, 15, 18, 21, 24))
) %>%
  as.data.frame()
```

    ## Note: D.f. calculations have been disabled because the number of observations exceeds 3000.
    ## To enable adjustments, add the argument 'pbkrtest.limit = 3798' (or larger)
    ## [or, globally, 'set emm_options(pbkrtest.limit = 3798)' or larger];
    ## but be warned that this may result in large computation time and memory use.

    ## Note: D.f. calculations have been disabled because the number of observations exceeds 3000.
    ## To enable adjustments, add the argument 'lmerTest.limit = 3798' (or larger)
    ## [or, globally, 'set emm_options(lmerTest.limit = 3798)' or larger];
    ## but be warned that this may result in large computation time and memory use.

``` r
# ==========================================
# 4. PLOT GENERATION VIA GGPLOT2
# ==========================================
trajectory_plot <- ggplot() +
  # Layer 1: Background participant-level spaghetti lines (Light Grey)
  geom_line(data = data_long, 
            aes(x = Month, y = SDSCA_Score, group = `Member ID`), 
            color = "#D3D3D3", alpha = 0.4, linewidth = 0.5) +
  
  # Layer 2: Forefront Aggregate Predicted Mean Lines
  geom_line(data = predicted_means, 
            aes(x = Month, y = emmean, color = Group, group = Group), 
            linewidth = 1.3) +
  
  # Layer 3: Discrete points tracking specific assessment intervals
  geom_point(data = predicted_means, 
             aes(x = Month, y = emmean, color = Group), 
             size = 3.5) +
  
  # Layer 4: High-resolution value text labels (Displaying values on the plot)
  geom_text(data = predicted_means, 
            aes(x = Month, y = emmean, label = sprintf("%.1f", emmean)),
            vjust = -1.2, fontface = "bold", size = 3.2, show.legend = FALSE) +
  
  # Color palette declarations: Dark Red for Control, Dark Green for Intervention
  scale_color_manual(
    breaks = c(int_label, ctrl_label),
    values = c("#006400", "#8B0000") # DarkGreen and DarkRed hex codes
  ) +
  
  # Scale Configuration
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12, 15, 18, 21, 24)) +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.15))) + # Extra margin buffer for text labels
  
  # Labels and Layout Styling
  labs(
    title = "Longitudinal Trajectory of Diabetes Self-Management Scores",
    subtitle = "Side-by-side model-predicted means overlaid across individual participant data profiles",
    x = "Study Timeline (Months)",
    y = "SDSCA Self-Management Score",
    color = "Study Allocation Group"
  ) +
  
  # Clean, publication-compliant classic background style
  theme_classic(base_family = "Times New Roman", base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, color = "grey30", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.background = element_rect(fill = "white", color = "grey80"),
    panel.grid.major.y = element_line(color = "grey95") # Soft horizontal grid accents
  )


# ==========================================
# 5. HIGH-RESOLUTION PLOT EXPORT
# ==========================================
# Export as an ultra high-res 300 DPI image optimized for peer-reviewed print
ggsave(
  filename = "SDSCA_Trajectory_Plot.png",
  plot = trajectory_plot,
  width = 10,
  height = 6.5,
  units = "in",
  dpi = 300
)

cat("\nSuccess! 'SDSCA_Trajectory_Plot.png' has been saved as a 300 DPI high-resolution image.\n")
```

    ## 
    ## Success! 'SDSCA_Trajectory_Plot.png' has been saved as a 300 DPI high-resolution image.

# 8. Effect of peer support groups on components of diabetes self management

``` r
group_var   <- "intervention/ control"
cluster_var <- "PSG group number"

# Clean up grouping variables and remove empty rows
psg_data <- psg_data %>%
  filter(!is.na(.data[[group_var]]), !is.na(.data[[cluster_var]])) %>%
  mutate(clean_group = trimws(as.character(.data[[group_var]])))

# Define the 6 domains, their baseline adjusters, and wide column prefixes
domains <- c("Diet", "Exercise", "Foot care", "Blood testing", "Medication adherence", "Non smoking")
baselines <- c("Diet0", "Exercise0", "Foot0", "Test0", "Med0", "Smoke0")
prefixes <- c("Diet", "Exercise", "Foot", "Test", "Med", "Smoke")

# Global baseline socio-demographic covariates vector
covariates <- c("Age", "Gender", "Religion", "Caste", "Marital_status", "Financial_status")


# ==========================================
# 2. LONG-FORMAT ANALYSIS LOOP PIPELINE
# ==========================================
# This master list will collect model outputs across all loops
compiled_models_list <- list()

for (i in 1:length(domains)) {
  
  current_domain   <- domains[i]
  current_baseline <- baselines[i]
  current_prefix   <- prefixes[i]
  
  cat("Processing Mixed Model for Domain:", current_domain, "...\n")
  
  # Identify the 8 follow-up columns dynamically (e.g., Diet1, Diet2... Diet8)
  domain_time_cols <- paste0(current_prefix, 1:8)
  
  # Isolate and reshape data for the current domain to long format
  loop_long <- psg_data %>%
    select(all_of(covariates), all_of(current_baseline), all_of(domain_time_cols), clean_group, all_of(cluster_var)) %>%
    pivot_longer(
      cols = all_of(domain_time_cols),
      names_to = "Time_Point",
      values_to = "Outcome_Score"
    ) %>%
    mutate(
      # Extract the trailing number from the string as our continuous Time variable (Months 1 to 8)
      Time = as.numeric(str_extract(Time_Point, "\\d+$"))
    )
  
  # Standardize model formula system labels
  names(loop_long)[names(loop_long) == "clean_group"]     <- "Group"
  names(loop_long)[names(loop_long) == cluster_var]      <- "ClusterID"
  names(loop_long)[names(loop_long) == current_baseline] <- "BaselineScore"
  
  # Fit the Linear Mixed Model
  model_fit <- lmer(
    Outcome_Score ~ Group * Time + BaselineScore + Age + Gender + Religion + Caste + 
                    Marital_status + Financial_status + (1 | ClusterID), 
    data = loop_long, REML = FALSE
  )
  
  # Extract Fixed Effects Summary Matrix Coefficients
  coef_matrix <- as.data.frame(summary(model_fit)$coefficients)
  coef_matrix$Parameter <- rownames(coef_matrix)
  
  # Rename the specialized 'BaselineScore' generic label to its true baseline column name
  coef_matrix$Parameter <- ifelse(coef_matrix$Parameter == "BaselineScore", current_baseline, coef_matrix$Parameter)
  
  # Format parameters into standardized Estimate (SE) configurations
  coef_formatted <- coef_matrix %>%
    mutate(
      Est_SE = sprintf("%.2f (%.2f)", Estimate, `Std. Error`),
      P_Val  = ifelse(`Pr(>|t|)` < 0.001, "<.001", sprintf("%.3f", `Pr(>|t|)`)),
      Combined_Output = paste0(Est_SE, " [p=", P_Val, "]")
    ) %>%
    select(Parameter, !!sym(current_domain) := Combined_Output)
  
  # Manual Variance Extraction for Fit Statistics (Bypasses performance package lockouts)
  var_components     <- as.data.frame(VarCorr(model_fit))
  rand_intercept_var <- var_components$vcov[var_components$grp == "ClusterID"]
  residual_var       <- var_components$vcov[var_components$grp == "Residual"]
  fixed_effects_var  <- var(predict(model_fit, re.form = NA))
  
  icc_val     <- rand_intercept_var / (rand_intercept_var + residual_var)
  marginal_r2 <- fixed_effects_var / (fixed_effects_var + rand_intercept_var + residual_var)
  cond_r2     <- (fixed_effects_var + rand_intercept_var) / (fixed_effects_var + rand_intercept_var + residual_var)
  
  fit_summary_rows <- data.frame(
    Parameter = c("Model Fit Metrics", 
                  "  Intraclass Correlation (ICC)", 
                  "  Marginal R-squared (R²m)", 
                  "  Conditional R-squared (R²c)"),
    Output_Val = c("", sprintf("%.3f", icc_val), sprintf("%.3f", marginal_r2), sprintf("%.3f", cond_r2)),
    stringsAsFactors = FALSE
  )
  names(fit_summary_rows)[2] <- current_domain
  
  # Append coefficients and fit structures together
  domain_final_df <- rbind(coef_formatted, fit_summary_rows)
  
  # Store current iteration dataframe to global compilation list
  compiled_models_list[[current_domain]] <- domain_final_df
}
```

    ## Processing Mixed Model for Domain: Diet ...
    ## Processing Mixed Model for Domain: Exercise ...
    ## Processing Mixed Model for Domain: Foot care ...
    ## Processing Mixed Model for Domain: Blood testing ...
    ## Processing Mixed Model for Domain: Medication adherence ...
    ## Processing Mixed Model for Domain: Non smoking ...

``` r
# ==========================================
# 3. SIDE-BY-SIDE MATRIX COMPILATION
# ==========================================
# Reduce list items into one comprehensive wide master dataframe
comprehensive_table <- Reduce(function(x, y) full_join(x, y, by = "Parameter"), compiled_models_list)

# Clean structural mismatch empty fragments cleanly
comprehensive_table[is.na(comprehensive_table)] <- "—"

# Order rows to keep fit statistics structured at the bottom margin
fit_subset  <- comprehensive_table %>% filter(str_detect(Parameter, "Model Fit|Correlation|R-squared"))
coef_subset <- comprehensive_table %>% filter(!Parameter %in% fit_subset$Parameter)
final_matrix_df <- rbind(coef_subset, fit_subset)


# ==========================================
# 4. GENERATE PUBLICATION READY FLEXTABLE
# ==========================================
ft_comprehensive <- flextable(final_matrix_df) %>%
  set_header_labels(Parameter = "Fixed Effect / Model Parameter") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 9, part = "all") %>% # Compact font size optimized for 7 columns wide layout
  bold(part = "header") %>%
  bold(i = which(final_matrix_df$Parameter == "Model Fit Metrics"), bold = TRUE) %>%
  align(j = 2:7, align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  
  # Standard Medical/Academic Journal 3-Line Border Configuration Layout
  border_remove() %>%
  hline_top(border = fp_border(width = 1.5), part = "header") %>%
  hline_bottom(border = fp_border(width = 1.5), part = "header") %>%
  hline_bottom(border = fp_border(width = 1.5), part = "body") %>%
  autofit() %>%
  
  # Academic footnotes
  add_footer_lines("Data cells are structured as Coefficient Estimate (Standard Error) [p-value].") %>%
  add_footer_lines("Models adapt adjustments for specific baseline scores matching the domain's parameter index vector row entries.") %>%
  add_footer_lines("All models specify random intercepts grouped nested within 'PSG group number' parameter constraints.") %>%
  font(fontname = "Times New Roman", part = "footer") %>%
  fontsize(size = 8, part = "footer")


# ==========================================
# 5. WRITE DIRECTLY TO MS WORD DOCUMENT
# ==========================================
title_style <- fp_text(font.size = 13, bold = TRUE, font.family = "Times New Roman")

doc_master <- read_docx() %>% 
  # Set page orientation to landscape layout to accommodate the wide columns beautifully
  body_end_section_landscape() %>%
  body_add_fpar(
    fpar(
      ftext("Table: Comprehensive Side-by-Side Evaluation of Multi-Domain Diabetes Self-Management Linear Mixed Models", prop = title_style),
      fp_p = fp_par(padding.bottom = 12)
    )
  ) %>%
  body_add_flextable(ft_comprehensive) %>%
  body_end_section_landscape()

print(doc_master, target = "Comprehensive_LMM_6_Domains_Report.docx")

cat("\nSUCCESS! Check your workspace for 'Comprehensive_LMM_6_Domains_Report.docx' (Landscape Oriented Document).\n")
```

    ## 
    ## SUCCESS! Check your workspace for 'Comprehensive_LMM_6_Domains_Report.docx' (Landscape Oriented Document).

# 9. Visualisation of effect of diabetes peer support on components of diabetes self management behaviours

``` r
group_var   <- "intervention/ control"
cluster_var <- "PSG group number"

# Assign a unique tracking ID to every participant row
psg_data <- psg_data %>%
  mutate(ParticipantID = row_number()) %>%
  filter(!is.na(.data[[group_var]]), !is.na(.data[[cluster_var]])) %>%
  mutate(clean_group = trimws(as.character(.data[[group_var]])))

# Map the structures of your 6 domains
domains  <- c("Diet", "Exercise", "Foot Care", "Blood Testing", "Medication Adherence", "Non Smoking")
prefixes <- c("Diet", "Exercise", "Foot", "Test", "Med", "Smoke")

# Master list to collect long-format chunks for each behavior
long_chunks_list <- list()

for (i in 1:length(domains)) {
  current_domain <- domains[i]
  current_prefix <- prefixes[i]
  
  # Identify baseline (0) and follow-up (1-8) columns
  baseline_col <- paste0(current_prefix, "0")
  followup_cols <- paste0(current_prefix, 1:8)
  all_domain_cols <- c(baseline_col, followup_cols)
  
  # Subset and shape this specific domain into long format
  domain_long <- psg_data %>%
    select(ParticipantID, clean_group, all_of(cluster_var), all_of(all_domain_cols)) %>%
    pivot_longer(
      cols = all_of(all_domain_cols),
      names_to = "Time_String",
      values_to = "Score"
    ) %>%
    mutate(
      # Extract the time point number (0 to 8)
      Time_Point = as.numeric(str_extract(Time_String, "\\d+$")),
      # Convert time point tokens into actual study month intervals (0 to 24 months)
      Month = case_when(
        Time_Point == 0 ~ 0,  Time_Point == 1 ~ 3,  Time_Point == 2 ~ 6,
        Time_Point == 3 ~ 9,  Time_Point == 4 ~ 12, Time_Point == 5 ~ 15,
        Time_Point == 6 ~ 18, Time_Point == 7 ~ 21, Time_Point == 8 ~ 24
      ),
      Behavior = current_domain
    ) %>%
    select(ParticipantID, Group = clean_group, ClusterID = all_of(cluster_var), Month, Behavior, Score)
  
  long_chunks_list[[current_domain]] <- domain_long
}

# Combine all behaviors into one master long-format data frame
master_long_data <- bind_rows(long_chunks_list)
master_long_data <- master_long_data %>% filter(!is.na(Score))

# Clean group string identities dynamically to prevent structural plot mapping bugs
unique_groups <- unique(master_long_data$Group)
int_label     <- unique_groups[str_detect(tolower(unique_groups), "int|peer|1")][1]
ctrl_label    <- unique_groups[!unique_groups %in% int_label][1]


# ==========================================
# 2. EXTRACT MODEL-PREDICTED MEANS PER DOMAIN
# ==========================================
predicted_means_list <- list()

for (current_domain in domains) {
  # Filter data for the specific behavior loop
  behavior_sub_data <- master_long_data %>% filter(Behavior == current_domain)
  
  # Fit the Linear Mixed Model
  lmm_fit <- lmer(Score ~ Group * Month + (1 | ClusterID), data = behavior_sub_data, REML = FALSE)
  
  # Compute predicted marginal means across the targeted 0-24 timeline grid
  means_df <- emmeans::emmeans(
    lmm_fit, 
    ~ Group | Month, 
    at = list(Month = c(0, 3, 6, 9, 12, 15, 18, 21, 24))
  ) %>% as.data.frame()
  
  means_df$Behavior <- current_domain
  predicted_means_list[[current_domain]] <- means_df
}
```

    ## Note: D.f. calculations have been disabled because the number of observations exceeds 3000.
    ## To enable adjustments, add the argument 'pbkrtest.limit = 3357' (or larger)
    ## [or, globally, 'set emm_options(pbkrtest.limit = 3357)' or larger];
    ## but be warned that this may result in large computation time and memory use.

    ## Note: D.f. calculations have been disabled because the number of observations exceeds 3000.
    ## To enable adjustments, add the argument 'lmerTest.limit = 3357' (or larger)
    ## [or, globally, 'set emm_options(lmerTest.limit = 3357)' or larger];
    ## but be warned that this may result in large computation time and memory use.

    ## Note: D.f. calculations have been disabled because the number of observations exceeds 3000.
    ## To enable adjustments, add the argument 'pbkrtest.limit = 3798' (or larger)
    ## [or, globally, 'set emm_options(pbkrtest.limit = 3798)' or larger];
    ## but be warned that this may result in large computation time and memory use.

    ## Note: D.f. calculations have been disabled because the number of observations exceeds 3000.
    ## To enable adjustments, add the argument 'lmerTest.limit = 3798' (or larger)
    ## [or, globally, 'set emm_options(lmerTest.limit = 3798)' or larger];
    ## but be warned that this may result in large computation time and memory use.

    ## Note: D.f. calculations have been disabled because the number of observations exceeds 3000.
    ## To enable adjustments, add the argument 'pbkrtest.limit = 3798' (or larger)
    ## [or, globally, 'set emm_options(pbkrtest.limit = 3798)' or larger];
    ## but be warned that this may result in large computation time and memory use.

    ## Note: D.f. calculations have been disabled because the number of observations exceeds 3000.
    ## To enable adjustments, add the argument 'lmerTest.limit = 3798' (or larger)
    ## [or, globally, 'set emm_options(lmerTest.limit = 3798)' or larger];
    ## but be warned that this may result in large computation time and memory use.

    ## Note: D.f. calculations have been disabled because the number of observations exceeds 3000.
    ## To enable adjustments, add the argument 'pbkrtest.limit = 3798' (or larger)
    ## [or, globally, 'set emm_options(pbkrtest.limit = 3798)' or larger];
    ## but be warned that this may result in large computation time and memory use.

    ## Note: D.f. calculations have been disabled because the number of observations exceeds 3000.
    ## To enable adjustments, add the argument 'lmerTest.limit = 3798' (or larger)
    ## [or, globally, 'set emm_options(lmerTest.limit = 3798)' or larger];
    ## but be warned that this may result in large computation time and memory use.

    ## Note: D.f. calculations have been disabled because the number of observations exceeds 3000.
    ## To enable adjustments, add the argument 'pbkrtest.limit = 3798' (or larger)
    ## [or, globally, 'set emm_options(pbkrtest.limit = 3798)' or larger];
    ## but be warned that this may result in large computation time and memory use.

    ## Note: D.f. calculations have been disabled because the number of observations exceeds 3000.
    ## To enable adjustments, add the argument 'lmerTest.limit = 3798' (or larger)
    ## [or, globally, 'set emm_options(lmerTest.limit = 3798)' or larger];
    ## but be warned that this may result in large computation time and memory use.

    ## Note: D.f. calculations have been disabled because the number of observations exceeds 3000.
    ## To enable adjustments, add the argument 'pbkrtest.limit = 3434' (or larger)
    ## [or, globally, 'set emm_options(pbkrtest.limit = 3434)' or larger];
    ## but be warned that this may result in large computation time and memory use.

    ## Note: D.f. calculations have been disabled because the number of observations exceeds 3000.
    ## To enable adjustments, add the argument 'lmerTest.limit = 3434' (or larger)
    ## [or, globally, 'set emm_options(lmerTest.limit = 3434)' or larger];
    ## but be warned that this may result in large computation time and memory use.

``` r
# Merge all predicted values together
master_predicted_means <- bind_rows(predicted_means_list)


# ==========================================
# 3. GENERATE FACETED TRAJECTORY PLOT
# ==========================================
faceted_plot <- ggplot() +
  # Layer 1: Background participant-level spaghetti lines (Light Grey)
  # Unique grouping ID is generated dynamically by combining ID + Behavior
  geom_line(data = master_long_data, 
            aes(x = Month, y = Score, group = interaction(ParticipantID, Behavior)), 
            color = "#DCDCDC", alpha = 0.3, linewidth = 0.4) +
  
  # Layer 2: Foreground aggregate group-level predicted mean lines
  geom_line(data = master_predicted_means, 
            aes(x = Month, y = emmean, color = Group, group = Group), 
            linewidth = 1.2) +
  
  # Layer 3: Assessment interval marker points
  geom_point(data = master_predicted_means, 
             aes(x = Month, y = emmean, color = Group), 
             size = 2.5) +
  
  # Layer 4: Model-predicted mean value text labels
  geom_text(data = master_predicted_means, 
            aes(x = Month, y = emmean, label = sprintf("%.1f", emmean)),
            vjust = -1.0, fontface = "bold", size = 2.6, show.legend = FALSE) +
  
  # Split the canvas into a 3x2 grid layout based on behavioral sub-domains
  facet_wrap(~Behavior, scales = "free_y", ncol = 2) +
  
  # Color Palette Mapping: Dark Green for Intervention, Dark Red for Control
  scale_color_manual(
    breaks = c(int_label, ctrl_label),
    values = c("#006400", "#8B0000")
  ) +
  
  # Axis Adjustments
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12, 15, 18, 21, 24)) +
  scale_y_continuous(expand = expansion(mult = c(0.12, 0.18))) + # Extra vertical spacing for numeric labels
  
  # Labels and Typography
  labs(
    title = "Longitudinal Trajectories of Self-Management Sub-Behaviors",
    subtitle = "Faceted evaluation grid showing model-predicted group means over individual participant profiles (Months 0–24)",
    x = "Study Timeline (Months)",
    y = "Calculated Domain Behavior Scores",
    color = "Randomized Allocation Arm"
  ) +
  
  # Publication-Ready Layout Theme Customization
  theme_bw(base_family = "Times New Roman", base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    plot.subtitle = element_text(size = 10, color = "grey30", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black", size = 9),
    strip.background = element_rect(fill = "#F5F5F5", color = "grey70"), # Clean facet header banners
    strip.text = element_text(face = "bold", size = 11, color = "#2C3E50"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.background = element_rect(fill = "white", color = "grey85"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank() # Clean grid style: hide minor and vertical accents
  )


# ==========================================
# 4. HIGH-RESOLUTION PLOT EXPORT
# ==========================================
# Save the final matrix grid as a crisp, print-optimized 300 DPI image canvas
ggsave(
  filename = "Faceted_Behavioral_Trajectories.png",
  plot = faceted_plot,
  width = 12,
  height = 14,
  units = "in",
  dpi = 300
)

cat("\nSuccess! 'Faceted_Behavioral_Trajectories.png' has been saved as an ultra-high resolution image.\n")
```

    ## 
    ## Success! 'Faceted_Behavioral_Trajectories.png' has been saved as an ultra-high resolution image.

# 10. Effect of peer support on diabetes distress

``` r
group_var   <- "intervention/ control"
cluster_var <- "PSG group number"

# Strip trailing spaces from strings and eliminate completely blank structural rows
psg_data <- psg_data %>%
  filter(!is.na(.data[[group_var]]), !is.na(.data[[cluster_var]])) %>%
  mutate(!!sym(group_var) := trimws(as.character(.data[[group_var]])))

# Map the 8 longitudinal tracking variables for Diabetes Distress
time_cols <- c("DDS1", "DDS2", "DDS3", "DDS4", "DDS5", "DDS6", "DDS7", "DDS8")


# ==========================================
# 2. RESHAPE WIDE TO LONG FORMAT
# ==========================================
# Mixed models require vertical structure alignment: collapsing columns to observation rows
data_long <- psg_data %>%
  pivot_longer(
    cols = all_of(time_cols),
    names_to = "Time_Point",
    values_to = "DDS_Score"
  ) %>%
  mutate(
    # Convert nominal column tokens to their corresponding continuous month intervals
    Time = case_when(
      Time_Point == "DDS1" ~ 3,  Time_Point == "DDS2" ~ 6,
      Time_Point == "DDS3" ~ 9,  Time_Point == "DDS4" ~ 12,
      Time_Point == "DDS5" ~ 15, Time_Point == "DDS6" ~ 18,
      Time_Point == "DDS7" ~ 21, Time_Point == "DDS8" ~ 24
    )
  )

# Standardize column labels to prevent system formula syntax bugs
names(data_long)[names(data_long) == group_var]   <- "Group"
names(data_long)[names(data_long) == cluster_var] <- "ClusterID"


# ==========================================
# 3. FIT LINEAR MIXED-EFFECTS MODELS (LMM)
# ==========================================
# A. Unconditional Null Model
null_model <- lmer(DDS_Score ~ 1 + (1 | ClusterID), data = data_long, REML = FALSE)

# B. Main Adjusted Model (Accounting for baseline covariate adjusters & interactions)
full_model <- lmer(
  DDS_Score ~ Group * Time + DDS0 + Age + Gender + Religion + Caste + 
              `Marital_status` + `Financial_status` + (1 | ClusterID), 
  data = data_long, 
  REML = FALSE
)


# ==========================================
# 4. ROBUST VARIANCE & COEFFICIENT EXTRACTION
# ==========================================
# Pull model parameters directly to avoid external package calculation freezes
variance_matrix <- as.data.frame(VarCorr(full_model))

rand_intercept_var <- variance_matrix$vcov[variance_matrix$grp == "ClusterID"]
residual_var       <- variance_matrix$vcov[variance_matrix$grp == "Residual"]
fixed_effects_var  <- var(predict(full_model, re.form = NA))

# Calculate structural fit indexes via standard formulas
icc_value    <- rand_intercept_var / (rand_intercept_var + residual_var)
marginal_r2  <- fixed_effects_var / (fixed_effects_var + rand_intercept_var + residual_var)
cond_r2      <- (fixed_effects_var + rand_intercept_var) / (fixed_effects_var + rand_intercept_var + residual_var)

# Extract Fixed Effects Summary Matrix Estimates
model_summary    <- summary(full_model)$coefficients
fixed_effects_df <- as.data.frame(model_summary)
fixed_effects_df <- cbind(Parameter = rownames(model_summary), fixed_effects_df)
rownames(fixed_effects_df) <- NULL

colnames(fixed_effects_df) <- c("Parameter", "Estimate", "Std_Error", "df", "t_value", "p_value")

# Clean formatting strings for peer-reviewed presentation
fixed_effects_df <- fixed_effects_df %>%
  mutate(
    `Estimate (SE)` = sprintf("%.2f (%.2f)", Estimate, Std_Error),
    `p-value` = ifelse(p_value < 0.001, "<.001", sprintf("%.3f", p_value))
  ) %>%
  select(Parameter, `Estimate (SE)`, `p-value`)


# ==========================================
# 5. COMPILE UNIFIED MATRIX GRID
# ==========================================
# Build custom rows tracking random variances and model fit summaries
fit_rows <- data.frame(
  Parameter = c("Random Effects & Model Fit Measures", 
                "  Intraclass Correlation Coefficient (ICC)", 
                "  Marginal R-squared (Variance from Fixed Variables)", 
                "  Conditional R-squared (Total Model Variance Captured)"),
  `Estimate (SE)` = c("", sprintf("%.3f", icc_value), sprintf("%.3f", marginal_r2), sprintf("%.3f", cond_r2)),
  `p-value` = c("", "", "", ""),
  stringsAsFactors = FALSE
)

colnames(fit_rows) <- colnames(fixed_effects_df)
final_table_df     <- rbind(fixed_effects_df, fit_rows)


# ==========================================
# 6. CONSTRUCT PUBLICATION READY FLEXTABLE
# ==========================================
ft_dds <- flextable(final_table_df) %>%
  set_header_labels(
    Parameter     = "Fixed Effect / Parameter Fit Model",
    `Estimate (SE)` = "Coefficient Estimate (SE)",
    `p-value`     = "p-value"
  ) %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  bold(i = which(final_table_df$Parameter == "Random Effects & Model Fit Measures"), bold = TRUE) %>%
  bold(part = "header") %>%
  align(j = 2:3, align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  
  # Standard Medical/Academic Journal 3-Line Border Format (APA Layout compliant)
  border_remove() %>%
  hline_top(border = fp_border(width = 1.5), part = "header") %>%
  hline_bottom(border = fp_border(width = 1.5), part = "header") %>%
  hline_bottom(border = fp_border(width = 1.5), part = "body") %>%
  autofit() %>%
  
  # Structural Footnotes
  add_footer_lines("Dependent Variable: Diabetes Distress Scale (DDS) score measured at 8 follow-up checks (Months 3 to 24).") %>%
  add_footer_lines("Model specifies adjustments for random intercept cluster groupings nested within 'PSG group number' units.") %>%
  add_footer_lines("Marginal R² checks fixed factors variance representation; Conditional R² indicates total model variance capture.") %>%
  font(fontname = "Times New Roman", part = "footer") %>%
  fontsize(size = 9, part = "footer")


# ==========================================
# 7. EXPORT DIRECTLY TO MICROSOFT WORD
# ==========================================
title_style <- fp_text(font.size = 14, bold = TRUE, font.family = "Times New Roman")

doc_dds <- read_docx() %>% 
  body_add_fpar(
    fpar(
      ftext("Table: Linear Mixed-Effects Model Predicting Longitudinal Diabetes Distress Scores", prop = title_style),
      fp_p = fp_par(padding.bottom = 12)
    )
  ) %>%
  body_add_flextable(ft_dds)

print(doc_dds, target = "DDS_Linear_Mixed_Model_Report.docx")

cat("\nDone! Check your workspace directory for 'DDS_Linear_Mixed_Model_Report.docx'.\n")
```

    ## 
    ## Done! Check your workspace directory for 'DDS_Linear_Mixed_Model_Report.docx'.

# 11. Visualisation of effect of diabetes peer support groups on diabetes distress

``` r
group_var   <- "intervention/ control"
cluster_var <- "PSG group number"

# Specify the wide column array tracking parameters
# Change "sdsca0" below to your baseline distress column name if named differently in Excel
baseline_col <- "DDS0" 
followup_cols <- c("DDS1", "DDS2", "DDS3", "DDS4", "DDS5", "DDS6", "DDS7", "DDS8")
all_time_cols <- c(baseline_col, followup_cols)

# Shape wide columns to a long format vertical matrix grid
data_long <- psg_data %>%
  select(ParticipantID, clean_group, all_of(cluster_var), all_of(all_time_cols)) %>%
  pivot_longer(
    cols = all_of(all_time_cols),
    names_to = "Time_String",
    values_to = "DDS_Score"
  ) %>%
  mutate(
    # Map raw field identifiers onto actual continuous timeline month values
    Month = case_when(
      Time_String == baseline_col ~ 0,
      Time_String == "DDS1"       ~ 3,  Time_String == "DDS2" ~ 6,
      Time_String == "DDS3"       ~ 9,  Time_String == "DDS4" ~ 12,
      Time_String == "DDS5"       ~ 15, Time_String == "DDS6" ~ 18,
      Time_String == "DDS7"       ~ 21, Time_String == "DDS8" ~ 24
    )
  ) %>%
  filter(!is.na(DDS_Score))

# Standardize column naming vectors for internal model syntax rules
names(data_long)[names(data_long) == "clean_group"] <- "Group"
names(data_long)[names(data_long) == cluster_var]  <- "ClusterID"

# Isolate allocations to avoid trailing spacing errors during rendering
unique_groups <- unique(data_long$Group)
int_label     <- unique_groups[str_detect(tolower(unique_groups), "int|peer|1")][1]
ctrl_label    <- unique_groups[!unique_groups %in% int_label][1]


# ==========================================
# 2. RUN LMM AND EXTRACT EXPLICIT MEANS
# ==========================================
# Fit a random intercept linear mixed model tracking Month changes
lmm_dds_fit <- lmer(DDS_Score ~ Group * Month + (1 | ClusterID), data = data_long, REML = FALSE)

# Compute cluster-adjusted predicted means across the 0-24 month timeline matrix
predicted_means <- emmeans::emmeans(
  lmm_dds_fit, 
  ~ Group | Month, 
  at = list(Month = c(0, 3, 6, 9, 12, 15, 18, 21, 24))
) %>% as.data.frame()
```

    ## Note: D.f. calculations have been disabled because the number of observations exceeds 3000.
    ## To enable adjustments, add the argument 'pbkrtest.limit = 3798' (or larger)
    ## [or, globally, 'set emm_options(pbkrtest.limit = 3798)' or larger];
    ## but be warned that this may result in large computation time and memory use.

    ## Note: D.f. calculations have been disabled because the number of observations exceeds 3000.
    ## To enable adjustments, add the argument 'lmerTest.limit = 3798' (or larger)
    ## [or, globally, 'set emm_options(lmerTest.limit = 3798)' or larger];
    ## but be warned that this may result in large computation time and memory use.

``` r
# ==========================================
# 3. GENERATE THE LONGITUDINAL TRAJECTORY PLOT
# ==========================================
dds_trajectory_plot <- ggplot() +
  # Layer 1: Background participant-level spaghetti lines (Light Grey)
  geom_line(data = data_long, 
            aes(x = Month, y = DDS_Score, group = ParticipantID), 
            color = "#DCDCDC", alpha = 0.35, linewidth = 0.5) +
  
  # Layer 2: Forefront model-predicted aggregate group trajectory paths
  geom_line(data = predicted_means, 
            aes(x = Month, y = emmean, color = Group, group = Group), 
            linewidth = 1.3) +
  
  # Layer 3: Assessment timeline tracking markers
  geom_point(data = predicted_means, 
             aes(x = Month, y = emmean, color = Group), 
             size = 3.0) +
  
  # Layer 4: Model value annotations (Displaying predicted means on the plot layout)
  geom_text(data = predicted_means, 
            aes(x = Month, y = emmean, label = sprintf("%.2f", emmean)),
            vjust = -1.2, fontface = "bold", size = 3.0, show.legend = FALSE) +
  
  # Color Palette Properties: Dark Green for Active Intervention, Dark Red for Control
  scale_color_manual(
    breaks = c(int_label, ctrl_label),
    values = c("#006400", "#8B0000")
  ) +
  
  # Layout Configurations
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12, 15, 18, 21, 24)) +
  scale_y_continuous(expand = expansion(mult = c(0.10, 0.15))) + # Prevent value labels clipping top bounds
  
  # Graph Titles and Labels
  labs(
    title = "Longitudinal Trajectory of Diabetes Distress Scale (DDS) Scores",
    subtitle = "Model-predicted group means compared over individual participant distress tracks (Months 0–24)",
    x = "Study Assessment Interval (Months)",
    y = "Calculated Diabetes Distress Scores",
    color = "Randomized Treatment Group"
  ) +
  
  # Publication-Compliant Classic Academic Theme Styling
  theme_classic(base_family = "Times New Roman", base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, color = "grey30", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.background = element_rect(fill = "white", color = "grey80"),
    panel.grid.major.y = element_line(color = "grey96") # Subtle horizontal grid tracking lines
  )


# ==========================================
# 4. HIGH-RESOLUTION PLOT EXPORT
# ==========================================
# Export as an print-ready 300 DPI high-resolution canvas file
ggsave(
  filename = "DDS_Longitudinal_Trajectories.png",
  plot = dds_trajectory_plot,
  width = 10,
  height = 6.5,
  units = "in",
  dpi = 300
)

cat("\nSuccess! High-resolution 'DDS_Longitudinal_Trajectories.png' generated and saved.\n")
```

    ## 
    ## Success! High-resolution 'DDS_Longitudinal_Trajectories.png' generated and saved.

# 12. Effect of peer support on clinical parameters

``` r
# ==========================================
# 0. CLEAR ACTIVE ENVIRONMENT AND CACHE
# ==========================================
rm(list = ls(all.names = TRUE)) 
gc()                            
```

    ##           used  (Mb) gc trigger  (Mb) limit (Mb) max used  (Mb)
    ## Ncells 3378095 180.5    5610454 299.7         NA  5610454 299.7
    ## Vcells 6036701  46.1   23342079 178.1      16384 23342078 178.1

``` r
psg_data <- read_excel("Clean.xlsx") 
```

    ## New names:
    ## • `In the last 3 months how much did you spend on Diabetes?Transport - Rs.` ->
    ##   `In the last 3 months how much did you spend on Diabetes?Transport -
    ##   Rs....72`
    ## • `In the last 3 months how much did you spend on Diabetes?Loss of wages - Rs.`
    ##   -> `In the last 3 months how much did you spend on Diabetes?Loss of wages -
    ##   Rs....73`
    ## • `In the last 3 months how much did you spend on Diabetes?Other cost - Rs.` ->
    ##   `In the last 3 months how much did you spend on Diabetes?Other cost -
    ##   Rs....74`
    ## • `Were you hospitalized in the last 3 months ?` -> `Were you hospitalized in
    ##   the last 3 months ?...75`
    ## • `If yes, Reason for hospitalization` -> `If yes, Reason for
    ##   hospitalization...76`
    ## • `In the last 3 months how much did you spend on Diabetes?Transport - Rs.` ->
    ##   `In the last 3 months how much did you spend on Diabetes?Transport -
    ##   Rs....215`
    ## • `In the last 3 months how much did you spend on Diabetes?Loss of wages - Rs.`
    ##   -> `In the last 3 months how much did you spend on Diabetes?Loss of wages -
    ##   Rs....216`
    ## • `In the last 3 months how much did you spend on Diabetes?Other cost - Rs.` ->
    ##   `In the last 3 months how much did you spend on Diabetes?Other cost -
    ##   Rs....217`
    ## • `Were you hospitalized in the last 3 months ?` -> `Were you hospitalized in
    ##   the last 3 months ?...218`
    ## • `If yes, Reason for hospitalization` -> `If yes, Reason for
    ##   hospitalization...219`

``` r
group_var   <- "intervention/ control"
cluster_var <- "PSG group number"

# Clean up empty rows and filter out missing tracking variables
psg_data <- psg_data %>%
  filter(!is.na(.data[[group_var]]), !is.na(.data[[cluster_var]])) %>%
  mutate(clean_group = trimws(as.character(.data[[group_var]])))

# Standardize column headers: strip hidden trailing spaces or backtick artifacts safely
names(psg_data) <- trimws(names(psg_data))
names(psg_data) <- gsub("`|'", "", names(psg_data)) 

all_cols <- names(psg_data)

# Helper function to find columns dynamically (handles spaces vs underscores)
find_col <- function(pattern, columns) {
  matched <- columns[str_detect(tolower(columns), tolower(pattern))]
  if(length(matched) == 0) return(NULL) else return(matched[1])
}

# --- FIX: STRICT WORD-BOUNDARY MATCH FOR AGE ---
# Using \\bage\\b ensures it matches "Age" or "Age (Years)" but completely IGNORES "Village"
age_name <- all_cols[str_detect(tolower(all_cols), "\\bage\\b")][1]

# Map other sociodemographic adjustments dynamically
gender_name  <- find_col("gender|sex", all_cols)
rel_name     <- find_col("religion", all_cols)
caste_name   <- find_col("caste", all_cols)
marital_name <- find_col("marital", all_cols)   
finance_name <- find_col("financial|income", all_cols) 

# Map physiological baselines dynamically (Explicitly accommodates spaces like "SBP 0")
sbp0_name    <- find_col("sbp\\s*0|sbp0|systolic\\s*0", all_cols)
dbp0_name    <- find_col("dbp\\s*0|dbp0|diastolic\\s*0", all_cols)
rbs0_name    <- find_col("rbs\\s*0|rbs0|glucose\\s*0|blood\\s*sugar\\s*0", all_cols)
wt0_name     <- find_col("wt\\s*0|wt0|weight\\s*0|weight0", all_cols)

# CRITICAL PROTECTION LAYER: Force clean continuous/factor parsing
# This converts any accidental text typos in numeric columns to NA, preventing row explosions.
psg_data <- psg_data %>%
  mutate(
    Age_clean       = as.numeric(as.character(.data[[age_name]])),
    Gender_clean    = as.factor(trimws(as.character(.data[[gender_name]]))),
    Religion_clean  = as.factor(trimws(as.character(.data[[rel_name]]))),
    Caste_clean     = as.factor(trimws(as.character(.data[[caste_name]]))),
    Marital_clean   = as.factor(trimws(as.character(.data[[marital_name]]))),
    Financial_clean = as.factor(trimws(as.character(.data[[finance_name]]))),
    
    SBP0_clean      = as.numeric(as.character(.data[[sbp0_name]])),
    DBP0_clean      = as.numeric(as.character(.data[[dbp0_name]])),
    RBS0_clean      = as.numeric(as.character(.data[[rbs0_name]])),
    Wt0_clean       = as.numeric(as.character(.data[[wt0_name]]))
  )
```

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `RBS0_clean = as.numeric(as.character(.data[["RBS 0"]]))`.
    ## Caused by warning:
    ## ! NAs introduced by coercion

``` r
# Verify that the strict Age filter successfully extracted a numeric vector
if (all(is.na(psg_data$Age_clean))) {
  stop("CRITICAL STOP: The selected Age column is still pulling text values. Please check column headers.")
}

cat("Successfully mapped numerical Age column to source name:", age_name, "\n")
```

    ## Successfully mapped numerical Age column to source name: Age

``` r
# Define outcomes for the model loop
outcomes <- c("Systolic BP", "Diastolic BP", "Random Blood Glucose", "Weight")

# Form internal covariate tracking vector (Notice sdsca0 is completely omitted)
internal_covariates <- c("Age_clean", "Gender_clean", "Religion_clean", "Caste_clean", 
                         "Marital_clean", "Financial_clean", "SBP0_clean", "DBP0_clean", 
                         "RBS0_clean", "Wt0_clean")


# ==========================================
# 2. LONG-FORMAT MODELING LOOP PIPELINE
# ==========================================
compiled_outputs_list <- list()

for (current_outcome in outcomes) {
  
  # REGEX BREAKDOWN: (?i)^SBP\\s*\\d+$
  # Matches the string prefix, allowing any trailing spaces or formatting variance smoothly (e.g. "SBP 1")
  if (current_outcome == "Systolic BP") {
    time_cols <- all_cols[str_detect(all_cols, "(?i)^sbp\\s*\\d+$")]
    time_cols <- time_cols[!str_detect(time_cols, "\\b0\\b|\\b00\\b")]
  } else if (current_outcome == "Diastolic BP") {
    time_cols <- all_cols[str_detect(all_cols, "(?i)^dbp\\s*\\d+$")]
    time_cols <- time_cols[!str_detect(time_cols, "\\b0\\b|\\b00\\b")]
  } else if (current_outcome == "Random Blood Glucose") {
    time_cols <- all_cols[str_detect(all_cols, "(?i)^rbs\\s*\\d+$")]
    time_cols <- time_cols[!str_detect(time_cols, "\\b0\\b|\\b00\\b")]
  } else if (current_outcome == "Weight") {
    time_cols <- all_cols[str_detect(all_cols, "(?i)^wt\\s*\\d+|^weight\\s*\\d+")]
    time_cols <- time_cols[!str_detect(time_cols, "\\b0\\b|\\b00\\b")]
    time_cols <- time_cols[str_detect(time_cols, "6|12|18|24")] # Keep target intervals
  }
  
  if(length(time_cols) == 0) {
    cat("⚠️ Warning: Could not find tracking columns for:", current_outcome, ". Skipping.\n")
    next
  }
  
  cat("Running mixed model for", current_outcome, "using columns:", paste(head(time_cols, 2), collapse=", "), "... \n")
  
  # DATA TYPE SAFEGUARD: Coerce follow-up columns to numeric to clear character vs double pivot conflicts
  loop_data_wide <- psg_data
  loop_data_wide[time_cols] <- lapply(loop_data_wide[time_cols], function(x) as.numeric(as.character(x)))
  
  # Reshape wide metrics to vertical data blocks
  loop_long <- loop_data_wide %>%
    select(all_of(internal_covariates), all_of(time_cols), clean_group, all_of(cluster_var)) %>%
    pivot_longer(cols = all_of(time_cols), names_to = "Time_String", values_to = "Measurement_Value") %>%
    mutate(Time = as.numeric(str_extract(Time_String, "\\d+$"))) %>%
    filter(!is.na(Measurement_Value))
  
  names(loop_long)[names(loop_long) == "clean_group"] <- "Group"
  names(loop_long)[names(loop_long) == cluster_var]  <- "ClusterID"
  
  # Fit the Linear Mixed Model
  model_fit <- lmer(
    Measurement_Value ~ Group * Time + SBP0_clean + DBP0_clean + RBS0_clean + Wt0_clean + 
                        Age_clean + Gender_clean + Religion_clean + Caste_clean + 
                        Marital_clean + Financial_clean + (1 | ClusterID), 
    data = loop_long, REML = FALSE
  )
  
  # Extract Coefficients Matrix
  coef_matrix <- as.data.frame(summary(model_fit)$coefficients)
  coef_matrix$Parameter <- rownames(coef_matrix)
  
  # Clean up parameter naming conventions for standard presentation strings
  coef_matrix$Parameter <- coef_matrix$Parameter %>%
    str_replace_all("Age_clean", "Age (Years)") %>%
    str_replace_all("Gender_clean", "Gender: ") %>%
    str_replace_all("Religion_clean", "Religion: ") %>%
    str_replace_all("Caste_clean", "Caste: ") %>%
    str_replace_all("Marital_clean", "Marital Status: ") %>%
    str_replace_all("Financial_clean", "Financial Status: ") %>%
    str_replace_all("SBP0_clean", "Baseline SBP (SBP 0)") %>%
    str_replace_all("DBP0_clean", "Baseline DBP (DBP 0)") %>%
    str_replace_all("RBS0_clean", "Baseline RBS (RBS 0)") %>%
    str_replace_all("Wt0_clean", "Baseline Weight (Wt 0)") %>%
    str_replace_all("Groupintervention", "Group (Intervention vs. Control)") %>%
    str_replace_all("Groupintervention:Time", "Group × Time Interaction")
  
  # Compress estimates and p-values into space-optimized cells
  coef_formatted <- coef_matrix %>%
    mutate(
      Est_SE = sprintf("%.2f (%.2f)", Estimate, `Std. Error`),
      P_Val  = ifelse(`Pr(>|t|)` < 0.001, "<.001", sprintf("%.3f", `Pr(>|t|)`)),
      Combined_Output = paste0(Est_SE, " [p=", P_Val, "]")
    ) %>%
    select(Parameter, !!sym(current_outcome) := Combined_Output)
  
  # Extract variance components directly to avoid package lockouts
  var_data           <- as.data.frame(VarCorr(model_fit))
  rand_intercept_var <- var_data$vcov[var_data$grp == "ClusterID"]
  residual_var       <- var_data$vcov[var_data$grp == "Residual"]
  fixed_effects_var  <- var(predict(model_fit, re.form = NA))
  
  icc_value    <- rand_intercept_var / (rand_intercept_var + residual_var)
  marginal_r2  <- fixed_effects_var / (fixed_effects_var + rand_intercept_var + residual_var)
  cond_r2      <- (fixed_effects_var + rand_intercept_var) / (fixed_effects_var + rand_intercept_var + residual_var)
  
  # Construct fit rows
  fit_summary_rows <- data.frame(
    Parameter = c("Random Effects & Model Fit Measures", 
                  "  Intraclass Correlation Coefficient (ICC)", 
                  "  Marginal R-squared (Fixed Variance Explained)", 
                  "  Conditional R-squared (Total Variance Explained)"),
    Output_Val = c("", sprintf("%.3f", icc_value), sprintf("%.3f", marginal_r2), sprintf("%.3f", cond_r2)),
    stringsAsFactors = FALSE
  )
  names(fit_summary_rows)[2] <- current_outcome
  
  outcome_final_df <- rbind(coef_formatted, fit_summary_rows)
  compiled_outputs_list[[current_outcome]] <- outcome_final_df
}
```

    ## Running mixed model for Systolic BP using columns: SBP 1, SBP 2 ...

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Running mixed model for Diastolic BP using columns: DBP 1, DBP 2 ...

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Running mixed model for Random Blood Glucose using columns: RBS 1, RBS 2 ...

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## ⚠️ Warning: Could not find tracking columns for: Weight . Skipping.

``` r
# ==========================================
# 3. SIDE-BY-SIDE MATRIX COMPILATION
# ==========================================
comprehensive_table <- Reduce(function(x, y) full_join(x, y, by = "Parameter"), compiled_outputs_list)
comprehensive_table[is.na(comprehensive_table)] <- "—"

# Order rows to keep fit statistics structured at the bottom margin
fit_subset  <- comprehensive_table %>% filter(str_detect(Parameter, "Model Fit|Correlation|R-squared"))
coef_subset <- comprehensive_table %>% filter(!Parameter %in% fit_subset$Parameter)
final_matrix_df <- rbind(coef_subset, fit_subset)


# ==========================================
# 4. GENERATE FLEXTABLE & EXPORT TO WORD
# ==========================================
# Dynamically calculate column bounds to protect against selection/alignment errors
total_cols <- ncol(final_matrix_df)

ft_clinical <- flextable(final_matrix_df) %>%
  set_header_labels(Parameter = "Fixed Effect / Model Parameter") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 9, part = "all") %>% 
  bold(part = "header") %>%
  bold(i = which(final_matrix_df$Parameter == "Random Effects & Model Fit Measures"), bold = TRUE) %>%
  
  # Dynamic alignment: spans cleanly from column 2 to the actual end of your matrix
  align(j = 2:total_cols, align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  
  border_remove() %>%
  hline_top(border = fp_border(width = 1.5), part = "header") %>%
  hline_bottom(border = fp_border(width = 1.5), part = "header") %>%
  hline_bottom(border = fp_border(width = 1.5), part = "body") %>%
  autofit() %>%
  add_footer_lines("Data cells are formatted as Coefficient Estimate (Standard Error) [p-value].") %>%
  add_footer_lines("Models adapt baseline adjustments across outcomes (SBP 0, DBP 0, RBS 0, Wt 0). All models specify random intercepts grouped nested within 'PSG group number' constraints.")

# Create and print out the Word Document
doc_clinical <- read_docx() %>% 
  body_end_section_landscape() # Forces a horizontal landscape section layout template

doc_clinical <- doc_clinical %>%
  body_add_fpar(fpar(ftext("Table: Longitudinal Mixed Models Across Clinical Biomarkers and Physiological Parameters", fp_text(font.size = 13, bold = TRUE, font.family = "Times New Roman")), fp_p = fp_par(padding.bottom = 12))) %>%
  body_add_flextable(ft_clinical) %>%
  body_end_section_landscape()

print(doc_clinical, target = "Clinical_Outcomes_LMM_Report.docx")
cat("\nProcess complete! Check your directory for 'Clinical_Outcomes_LMM_Report.docx'.\n")
```

    ## 
    ## Process complete! Check your directory for 'Clinical_Outcomes_LMM_Report.docx'.

# 13. Effect of peer support on weight

``` r
# ==========================================
# 0. CLEAR ACTIVE ENVIRONMENT AND CACHE
# ==========================================
rm(list = ls(all.names = TRUE)) 
gc()                            
```

    ##           used  (Mb) gc trigger  (Mb) limit (Mb) max used  (Mb)
    ## Ncells 3380540 180.6    5610454 299.7         NA  5610454 299.7
    ## Vcells 6043504  46.2   23342079 178.1      16384 23342078 178.1

``` r
# ==========================================
# 1. DATA IMPORT & RIGOROUS DATA-TYPE CLEANING
# ==========================================
# Reads your dataset from the working directory
psg_data <- read_excel("Clean.xlsx") 
```

    ## New names:
    ## • `In the last 3 months how much did you spend on Diabetes?Transport - Rs.` ->
    ##   `In the last 3 months how much did you spend on Diabetes?Transport -
    ##   Rs....72`
    ## • `In the last 3 months how much did you spend on Diabetes?Loss of wages - Rs.`
    ##   -> `In the last 3 months how much did you spend on Diabetes?Loss of wages -
    ##   Rs....73`
    ## • `In the last 3 months how much did you spend on Diabetes?Other cost - Rs.` ->
    ##   `In the last 3 months how much did you spend on Diabetes?Other cost -
    ##   Rs....74`
    ## • `Were you hospitalized in the last 3 months ?` -> `Were you hospitalized in
    ##   the last 3 months ?...75`
    ## • `If yes, Reason for hospitalization` -> `If yes, Reason for
    ##   hospitalization...76`
    ## • `In the last 3 months how much did you spend on Diabetes?Transport - Rs.` ->
    ##   `In the last 3 months how much did you spend on Diabetes?Transport -
    ##   Rs....215`
    ## • `In the last 3 months how much did you spend on Diabetes?Loss of wages - Rs.`
    ##   -> `In the last 3 months how much did you spend on Diabetes?Loss of wages -
    ##   Rs....216`
    ## • `In the last 3 months how much did you spend on Diabetes?Other cost - Rs.` ->
    ##   `In the last 3 months how much did you spend on Diabetes?Other cost -
    ##   Rs....217`
    ## • `Were you hospitalized in the last 3 months ?` -> `Were you hospitalized in
    ##   the last 3 months ?...218`
    ## • `If yes, Reason for hospitalization` -> `If yes, Reason for
    ##   hospitalization...219`

``` r
group_var   <- "intervention/ control"
cluster_var <- "PSG group number"

# Clean up empty rows and filter out missing tracking variables
psg_data <- psg_data %>%
  filter(!is.na(.data[[group_var]]), !is.na(.data[[cluster_var]])) %>%
  mutate(clean_group = trimws(as.character(.data[[group_var]])))

# Standardize column headers: strip hidden trailing spaces or backtick artifacts safely
names(psg_data) <- trimws(names(psg_data))
names(psg_data) <- gsub("`|'", "", names(psg_data)) 

all_cols <- names(psg_data)

# Helper function to find columns dynamically (handles spaces vs underscores)
find_col <- function(pattern, columns) {
  matched <- columns[str_detect(tolower(columns), tolower(pattern))]
  if(length(matched) == 0) return(NULL) else return(matched[1])
}

# --- FIXED: STRICT WORD-BOUNDARY MATCH FOR AGE ---
# Using \\bage\\b ensures it matches "Age" or "Age (Years)" but completely IGNORES "Village"
age_name <- all_cols[str_detect(tolower(all_cols), "\\bage\\b")][1]

# Map other sociodemographic adjustments dynamically
gender_name  <- find_col("gender|sex", all_cols)
rel_name     <- find_col("religion", all_cols)
caste_name   <- find_col("caste", all_cols)
marital_name <- find_col("marital", all_cols)   
finance_name <- find_col("financial|income", all_cols) 

# Map physiological baseline dynamically (Explicitly accommodates spaces like "Wt 0")
wt0_name     <- find_col("wt\\s*0|wt0|weight\\s*0|weight0", all_cols)

# CRITICAL PROTECTION LAYER: Force clean continuous/factor parsing
# This converts any accidental text typos in numeric columns to NA, preventing row explosions.
psg_data <- psg_data %>%
  mutate(
    Age_clean       = as.numeric(as.character(.data[[age_name]])),
    Gender_clean    = as.factor(trimws(as.character(.data[[gender_name]]))),
    Religion_clean  = as.factor(trimws(as.character(.data[[rel_name]]))),
    Caste_clean     = as.factor(trimws(as.character(.data[[caste_name]]))),
    Marital_clean   = as.factor(trimws(as.character(.data[[marital_name]]))),
    Financial_clean = as.factor(trimws(as.character(.data[[finance_name]]))),
    Wt0_clean       = as.numeric(as.character(.data[[wt0_name]]))
  )

# Verify that the strict Age filter successfully extracted a numeric vector
if (all(is.na(psg_data$Age_clean))) {
  stop("CRITICAL STOP: The selected Age column is still pulling text values. Please check column headers.")
}

cat("Successfully mapped numerical Age column to source name:", age_name, "\n")
```

    ## Successfully mapped numerical Age column to source name: Age

``` r
# Define outcomes and covariates for the model
current_outcome <- "Weight"
internal_covariates <- c("Age_clean", "Gender_clean", "Religion_clean", "Caste_clean", 
                         "Marital_clean", "Financial_clean", "Wt0_clean")

# Explicitly identify your 4 follow-up columns using the spacing pattern regex
time_cols <- all_cols[str_detect(all_cols, "(?i)^wt\\s*[1-4]$|^weight\\s*[1-4]$")]

if(length(time_cols) == 0) {
  stop("⚠️ Error: Could not find tracking columns for Weight (Wt 1 to Wt 4). Check naming syntax in Excel.")
}

cat("Running mixed model for Weight using columns:", paste(time_cols, collapse=", "), "... \n")
```

    ## Running mixed model for Weight using columns: Wt1, Wt2, Wt3, Wt4 ...

``` r
# DATA TYPE SAFEGUARD: Coerce follow-up columns to numeric to clear character vs double pivot conflicts
loop_data_wide <- psg_data
loop_data_wide[time_cols] <- lapply(loop_data_wide[time_cols], function(x) as.numeric(as.character(x)))

# ==========================================
# 2. LONG-FORMAT MODELING PIPELINE
# ==========================================
# Reshape wide metrics to vertical data blocks
loop_long <- loop_data_wide %>%
  select(all_of(internal_covariates), all_of(time_cols), clean_group, all_of(cluster_var)) %>%
  pivot_longer(cols = all_of(time_cols), names_to = "Time_String", values_to = "Measurement_Value") %>%
  mutate(
    # Programmatically capture the time point index (1, 2, 3, 4)
    Time_Point = as.numeric(str_extract(Time_String, "\\d+$")),
    # Convert those index tokens to actual continuous study month intervals (6, 12, 18, 24)
    Time = case_when(
      Time_Point == 1 ~ 6,
      Time_Point == 2 ~ 12,
      Time_Point == 3 ~ 18,
      Time_Point == 4 ~ 24
    )
  ) %>%
  filter(!is.na(Measurement_Value))

names(loop_long)[names(loop_long) == "clean_group"] <- "Group"
names(loop_long)[names(loop_long) == cluster_var]  <- "ClusterID"

# Fit the Linear Mixed Model
model_fit <- lmer(
  Measurement_Value ~ Group * Time + Wt0_clean + 
                      Age_clean + Gender_clean + Religion_clean + Caste_clean + 
                      Marital_clean + Financial_clean + (1 | ClusterID), 
  data = loop_long, REML = FALSE
)
```

    ## boundary (singular) fit: see help('isSingular')

``` r
# Extract Coefficients Matrix
coef_matrix <- as.data.frame(summary(model_fit)$coefficients)
coef_matrix$Parameter <- rownames(coef_matrix)

# Clean up parameter naming conventions for standard presentation strings
coef_matrix$Parameter <- coef_matrix$Parameter %>%
  str_replace_all("Age_clean", "Age (Years)") %>%
  str_replace_all("Gender_clean", "Gender: ") %>%
  str_replace_all("Religion_clean", "Religion: ") %>%
  str_replace_all("Caste_clean", "Caste: ") %>%
  str_replace_all("Marital_clean", "Marital Status: ") %>%
  str_replace_all("Financial_clean", "Financial Status: ") %>%
  str_replace_all("Wt0_clean", "Baseline Weight (Wt 0)") %>%
  str_replace_all("Groupintervention", "Group (Intervention vs. Control)") %>%
  str_replace_all("Groupintervention:Time", "Group × Time Interaction")

# Compress estimates and p-values into space-optimized cells
coef_formatted <- coef_matrix %>%
  mutate(
    Est_SE = sprintf("%.2f (%.2f)", Estimate, `Std. Error`),
    P_Val  = ifelse(`Pr(>|t|)` < 0.001, "<.001", sprintf("%.3f", `Pr(>|t|)`)),
    Combined_Output = paste0(Est_SE, " [p=", P_Val, "]")
  ) %>%
  select(Parameter, !!sym(current_outcome) := Combined_Output)

# Extract variance components directly to avoid package lockouts
var_data           <- as.data.frame(VarCorr(model_fit))
rand_intercept_var <- var_data$vcov[var_data$grp == "ClusterID"]
residual_var       <- var_data$vcov[var_data$grp == "Residual"]
fixed_effects_var  <- var(predict(model_fit, re.form = NA))

icc_value    <- rand_intercept_var / (rand_intercept_var + residual_var)
marginal_r2  <- fixed_effects_var / (fixed_effects_var + rand_intercept_var + residual_var)
cond_r2      <- (fixed_effects_var + rand_intercept_var) / (fixed_effects_var + rand_intercept_var + residual_var)

# Construct fit rows
fit_summary_rows <- data.frame(
  Parameter = c("Random Effects & Model Fit Measures", 
                "  Intraclass Correlation Coefficient (ICC)", 
                "  Marginal R-squared (Fixed Variance Explained)", 
                "  Conditional R-squared (Total Variance Explained)"),
  Output_Val = c("", sprintf("%.3f", icc_value), sprintf("%.3f", marginal_r2), sprintf("%.3f", cond_r2)),
  stringsAsFactors = FALSE
)
names(fit_summary_rows)[2] <- current_outcome

final_matrix_df <- rbind(coef_formatted, fit_summary_rows)

# ==========================================
# 3. GENERATE FLEXTABLE & EXPORT TO WORD
# ==========================================
ft_weight <- flextable(final_matrix_df) %>%
  set_header_labels(Parameter = "Fixed Effect / Model Parameter", Weight = "Weight Model Outcomes") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 10, part = "all") %>% 
  bold(part = "header") %>%
  bold(i = which(final_matrix_df$Parameter == "Random Effects & Model Fit Measures"), bold = TRUE) %>%
  align(j = 2, align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  
  # Professional APA style 3-line table border format
  border_remove() %>%
  hline_top(border = fp_border(width = 1.5), part = "header") %>%
  hline_bottom(border = fp_border(width = 1.5), part = "header") %>%
  hline_bottom(border = fp_border(width = 1.5), part = "body") %>%
  autofit() %>%
  add_footer_lines("Data cells are formatted as Coefficient Estimate (Standard Error) [p-value].") %>%
  add_footer_lines("Model specifies baseline adjustment for Wt 0. Random intercepts are grouped nested within 'PSG group number' clusters.")

# Save to Word Document (Portrait layout is fine since it's a single model table)
doc_weight <- read_docx() %>% 
  body_add_fpar(fpar(ftext("Table: Longitudinal Mixed-Effects Model Predicting Changes in Weight (kg)", fp_text(font.size = 13, bold = TRUE, font.family = "Times New Roman")), fp_p = fp_par(padding.bottom = 12))) %>%
  body_add_flextable(ft_weight)

print(doc_weight, target = "Weight_LMM_Report.docx")
cat("\nProcess complete! 'Weight_LMM_Report.docx' has been successfully created in your directory.\n")
```

    ## 
    ## Process complete! 'Weight_LMM_Report.docx' has been successfully created in your directory.

# 14. Visualisation of effect of peer support on clinical parameters

``` r
# Standardize headers and remove white spaces/backtick artifacts
names(psg_data) <- trimws(names(psg_data))
names(psg_data) <- gsub("`|'", "", names(psg_data)) 
all_cols <- names(psg_data)

psg_data <- psg_data %>%
  filter(!is.na(.data[[group_var]]), !is.na(.data[[cluster_var]])) %>%
  mutate(clean_group = trimws(as.character(.data[[group_var]])))

# Define your 3 key continuous clinical biomarkers and their column prefixes
biomarkers <- c("Systolic Blood Pressure", "Diastolic Blood Pressure", "Random Blood Glucose")
prefixes   <- c("SBP", "DBP", "RBS")

long_chunks_list <- list()

for (i in 1:length(biomarkers)) {
  current_bio    <- biomarkers[i]
  current_prefix <- prefixes[i]
  
  # Dynamically look for baseline (0) and follow-up tracking columns (1 to 24) with spaces
  baseline_col <- all_cols[str_detect(tolower(all_cols), paste0("^", tolower(current_prefix), "\\s*0$"))][1]
  followup_cols <- all_cols[str_detect(tolower(all_cols), paste0("^", tolower(current_prefix), "\\s*\\d+$"))]
  followup_cols <- followup_cols[!followup_cols %in% baseline_col]
  
  all_domain_cols <- c(baseline_col, followup_cols)
  
  # Ensure all matched columns are treated as numeric to prevent type conflicts
  psg_data[all_domain_cols] <- lapply(psg_data[all_domain_cols], function(x) as.numeric(as.character(x)))
  
  # Shape this specific biomarker array to vertical long format
  bio_long <- psg_data %>%
    select(clean_group, all_of(cluster_var), all_of(all_domain_cols)) %>%
    pivot_longer(cols = all_of(all_domain_cols), names_to = "Time_String", values_to = "Value") %>%
    mutate(
      Month = as.numeric(str_extract(Time_String, "\\d+$")),
      Biomarker = current_bio
    ) %>%
    select(Group = clean_group, ClusterID = all_of(cluster_var), Month, Biomarker, Value)
  
  long_chunks_list[[current_bio]] <- bio_long
}
```

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion
    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion
    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion
    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion
    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

``` r
# Bind all clinical parameters into a master data frame
master_long_data <- bind_rows(long_chunks_list) %>% filter(!is.na(Value))

# Isolate naming labels for group coloring safety
unique_groups <- unique(master_long_data$Group)
int_label     <- unique_groups[str_detect(tolower(unique_groups), "int|peer|1")][1]
ctrl_label    <- unique_groups[!unique_groups %in% int_label][1]

# ==========================================
# 2. RUN LMM & EXTRACT ADJUSTED MARGINAL MEANS
# ==========================================
predicted_means_list <- list()

for (current_bio in biomarkers) {
  sub_data <- master_long_data %>% filter(Biomarker == current_bio)
  
  # Fit your cluster-nested random intercept model tracking continuous months
  lmm_fit <- lmer(Value ~ Group * Month + (1 | ClusterID), data = sub_data, REML = FALSE)
  
  # Generate predicted margin means across standard tracking intervals (0 to 24 months)
  means_df <- emmeans::emmeans(lmm_fit, ~ Group | Month, at = list(Month = seq(0, 24, by = 3))) %>% 
    as.data.frame()
  
  means_df$Biomarker <- current_bio
  predicted_means_list[[current_bio]] <- means_df
}
```

    ## Note: D.f. calculations have been disabled because the number of observations exceeds 3000.
    ## To enable adjustments, add the argument 'pbkrtest.limit = 9236' (or larger)
    ## [or, globally, 'set emm_options(pbkrtest.limit = 9236)' or larger];
    ## but be warned that this may result in large computation time and memory use.

    ## Note: D.f. calculations have been disabled because the number of observations exceeds 3000.
    ## To enable adjustments, add the argument 'lmerTest.limit = 9236' (or larger)
    ## [or, globally, 'set emm_options(lmerTest.limit = 9236)' or larger];
    ## but be warned that this may result in large computation time and memory use.

    ## Note: D.f. calculations have been disabled because the number of observations exceeds 3000.
    ## To enable adjustments, add the argument 'pbkrtest.limit = 9237' (or larger)
    ## [or, globally, 'set emm_options(pbkrtest.limit = 9237)' or larger];
    ## but be warned that this may result in large computation time and memory use.

    ## Note: D.f. calculations have been disabled because the number of observations exceeds 3000.
    ## To enable adjustments, add the argument 'lmerTest.limit = 9237' (or larger)
    ## [or, globally, 'set emm_options(lmerTest.limit = 9237)' or larger];
    ## but be warned that this may result in large computation time and memory use.

    ## Note: D.f. calculations have been disabled because the number of observations exceeds 3000.
    ## To enable adjustments, add the argument 'pbkrtest.limit = 9233' (or larger)
    ## [or, globally, 'set emm_options(pbkrtest.limit = 9233)' or larger];
    ## but be warned that this may result in large computation time and memory use.

    ## Note: D.f. calculations have been disabled because the number of observations exceeds 3000.
    ## To enable adjustments, add the argument 'lmerTest.limit = 9233' (or larger)
    ## [or, globally, 'set emm_options(lmerTest.limit = 9233)' or larger];
    ## but be warned that this may result in large computation time and memory use.

``` r
master_predicted_means <- bind_rows(predicted_means_list)

# ==========================================
# 3. GENERATE FACETED TRAJECTORY PLOT
# ==========================================
clinical_plot <- ggplot(master_predicted_means, aes(x = Month, y = emmean, color = Group, group = Group)) +
  # Forefront aggregate model-predicted trend lines
  geom_line(linewidth = 1.2) +
  
  # 95% Confidence Interval error bands around your model predictions
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL, fill = Group), alpha = 0.12, color = NA) +
  
  # Clear assessment point markers
  geom_point(size = 2.5) +
  
  # Facet the canvas vertically per clinical biomarker, allowing individual y-axes
  facet_wrap(~Biomarker, scales = "free_y", ncol = 1) +
  
  # Set specific colors: Dark Green for Intervention, Dark Red for Control
  scale_color_manual(breaks = c(int_label, ctrl_label), values = c("#006400", "#8B0000")) +
  scale_fill_manual(breaks = c(int_label, ctrl_label), values = c("#006400", "#8B0000")) +
  
  # Timeline Axis Controls
  scale_x_continuous(breaks = seq(0, 24, by = 3)) +
  
  # Labels and Typography
  labs(
    title = "Effect of Peer Support Intervention on Clinical Parameters",
    subtitle = "Faceted trajectories showing covariate-adjusted mixed model estimates with 95% Confidence Intervals (Months 0–24)",
    x = "Timeline Since Enrollment (Months)",
    y = "Estimated Marginal Means (Model Adjusted)",
    color = "Treatment Allocation Arm",
    fill = "Treatment Allocation Arm"
  ) +
  
  # Clean Academic Theme Customization
  theme_bw(base_family = "Times New Roman", base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, color = "grey30", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    strip.background = element_rect(fill = "#F8F9FA", color = "grey70"), # Subtle facet banners
    strip.text = element_text(face = "bold", size = 11, color = "#2C3E50"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank() # Hide vertical lines to focus on longitudinal progression
  )

# ==========================================
# 4. HIGH-RESOLUTION GRAPH EXPORT
# ==========================================
# Export as a crisp, large-format 300 DPI canvas optimized for medical journals
ggsave(
  filename = "Peer_Support_Clinical_Trajectories.png",
  plot = clinical_plot,
  width = 9,
  height = 11,
  units = "in",
  dpi = 300
)

cat("\nSuccess! 'Peer_Support_Clinical_Trajectories.png' has been saved to your workspace layout.\n")
```

    ## 
    ## Success! 'Peer_Support_Clinical_Trajectories.png' has been saved to your workspace layout.

# 15. Trajectory plot for weight

``` r
group_var   <- "intervention/ control"
cluster_var <- "PSG group number"

# Standardize headers and remove white spaces/backtick artifacts
names(psg_data) <- trimws(names(psg_data))
names(psg_data) <- gsub("`|'", "", names(psg_data)) 
all_cols <- names(psg_data)

psg_data <- psg_data %>%
  filter(!is.na(.data[[group_var]]), !is.na(.data[[cluster_var]])) %>%
  mutate(clean_group = trimws(as.character(.data[[group_var]])))

# Identify baseline weight (0) and follow-up tracking columns (1 to 4) with spaces
baseline_col <- all_cols[str_detect(tolower(all_cols), "wt\\s*0|weight\\s*0")][1]
followup_cols <- all_cols[str_detect(tolower(all_cols), "(?i)^wt\\s*[1-4]$|^weight\\s*[1-4]$")]

all_weight_cols <- c(baseline_col, followup_cols)

# Ensure all matched columns are treated as numeric to prevent character type conflicts
psg_data[all_weight_cols] <- lapply(psg_data[all_weight_cols], function(x) as.numeric(as.character(x)))

# Shape wide weight columns into vertical long format data blocks
weight_long <- psg_data %>%
  select(clean_group, all_of(cluster_var), all_of(all_weight_cols)) %>%
  pivot_longer(cols = all_of(all_weight_cols), names_to = "Time_String", values_to = "Weight_Value") %>%
  mutate(
    # Programmatically capture the trailing digits (0, 1, 2, 3, 4)
    Time_Point = as.numeric(str_extract(Time_String, "\\d+$")),
    # Convert those index tokens to actual continuous study month intervals (0, 6, 12, 18, 24)
    Month = case_when(
      Time_Point == 0 ~ 0,
      Time_Point == 1 ~ 6,
      Time_Point == 2 ~ 12,
      Time_Point == 3 ~ 18,
      Time_Point == 4 ~ 24
    )
  ) %>%
  select(Group = clean_group, ClusterID = all_of(cluster_var), Month, Weight_Value) %>%
  filter(!is.na(Weight_Value))

# Isolate naming labels for group coloring safety
unique_groups <- unique(weight_long$Group)
int_label     <- unique_groups[str_detect(tolower(unique_groups), "int|peer|1")][1]
ctrl_label    <- unique_groups[!unique_groups %in% int_label][1]

# ==========================================
# 2. RUN LMM & EXTRACT ADJUSTED MARGINAL MEANS
# ==========================================
# Fit your cluster-nested random intercept model tracking continuous months for weight
lmm_weight_fit <- lmer(Weight_Value ~ Group * Month + (1 | ClusterID), data = weight_long, REML = FALSE)

# Generate predicted margin means across your evaluation intervals (0, 6, 12, 18, 24 months)
predicted_weight_means <- emmeans::emmeans(
  lmm_weight_fit, 
  ~ Group | Month, 
  at = list(Month = c(0, 6, 12, 18, 24))
) %>% as.data.frame()

# ==========================================
# 3. GENERATE THE LONGITUDINAL TRAJECTORY PLOT (Column Name Fix)
# ==========================================
weight_plot <- ggplot(predicted_weight_means, aes(x = Month, y = emmean, color = Group, group = Group)) +
  # Forefront aggregate model-predicted trend lines
  geom_line(linewidth = 1.3) +
  
  # FIX: Updated from asymp.LCL/UCL to lower.CL and upper.CL
  geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL, fill = Group), alpha = 0.12, color = NA) +
  
  # Clear assessment point markers
  geom_point(size = 3.0) +
  
  # Add value labels immediately above the points for manuscript clarity
  geom_text(aes(label = sprintf("%.1f kg", emmean)), 
            vjust = -1.2, fontface = "bold", size = 3.2, show.legend = FALSE) +
  
  # Set specific colors: Dark Green for Intervention, Dark Red for Control
  scale_color_manual(breaks = c(int_label, ctrl_label), values = c("#006400", "#8B0000")) +
  scale_fill_manual(breaks = c(int_label, ctrl_label), values = c("#006400", "#8B0000")) +
  
  # Timeline Axis Controls
  scale_x_continuous(breaks = c(0, 6, 12, 18, 24)) +
  scale_y_continuous(expand = expansion(mult = c(0.10, 0.15))) + # Extra room on top so text labels don't get cut off
  
  # Labels and Typography
  labs(
    title = "Longitudinal Trajectory of Weight Changes Over 24 Months",
    subtitle = "Model-adjusted marginal means with 95% Confidence Intervals mapped across study intervals",
    x = "Timeline Since Enrollment (Months)",
    y = "Estimated Weight (kg) [Model Adjusted]",
    color = "Treatment Allocation Arm",
    fill = "Treatment Allocation Arm"
  ) +
  
  # Clean Academic Theme Customization
  theme_classic(base_family = "Times New Roman", base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, color = "grey30", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.background = element_rect(fill = "white", color = "grey85"),
    panel.grid.major.y = element_line(color = "grey96") # Subtle horizontal grid tracking lines
  )

# ==========================================
# 4. HIGH-RESOLUTION GRAPH EXPORT
# ==========================================
# Export as a crisp 300 DPI image canvas optimized for medical journals
ggsave(
  filename = "Weight_Longitudinal_Trajectories.png",
  plot = weight_plot,
  width = 9,
  height = 6,
  units = "in",
  dpi = 300
)

cat("\nSuccess! High-resolution 'Weight_Longitudinal_Trajectories.png' generated and saved.\n")
```

    ## 
    ## Success! High-resolution 'Weight_Longitudinal_Trajectories.png' generated and saved.

# 16. HbA1c comparison

``` r
# ==========================================
# 0. CLEAR ACTIVE ENVIRONMENT AND CACHE
# ==========================================
rm(list = ls(all.names = TRUE)) 
gc()                            
```

    ##           used  (Mb) gc trigger  (Mb) limit (Mb) max used  (Mb)
    ## Ncells 3414825 182.4    5610454 299.7         NA  5610454 299.7
    ## Vcells 6081470  46.4   23342079 178.1      16384 23342078 178.1

``` r
# ==========================================
# 1. DATA IMPORT & RIGOROUS DATA-TYPE CLEANING
# ==========================================
# Reads your dataset from the working directory
psg_data <- read_excel("Clean.xlsx") 
```

    ## New names:
    ## • `In the last 3 months how much did you spend on Diabetes?Transport - Rs.` ->
    ##   `In the last 3 months how much did you spend on Diabetes?Transport -
    ##   Rs....72`
    ## • `In the last 3 months how much did you spend on Diabetes?Loss of wages - Rs.`
    ##   -> `In the last 3 months how much did you spend on Diabetes?Loss of wages -
    ##   Rs....73`
    ## • `In the last 3 months how much did you spend on Diabetes?Other cost - Rs.` ->
    ##   `In the last 3 months how much did you spend on Diabetes?Other cost -
    ##   Rs....74`
    ## • `Were you hospitalized in the last 3 months ?` -> `Were you hospitalized in
    ##   the last 3 months ?...75`
    ## • `If yes, Reason for hospitalization` -> `If yes, Reason for
    ##   hospitalization...76`
    ## • `In the last 3 months how much did you spend on Diabetes?Transport - Rs.` ->
    ##   `In the last 3 months how much did you spend on Diabetes?Transport -
    ##   Rs....215`
    ## • `In the last 3 months how much did you spend on Diabetes?Loss of wages - Rs.`
    ##   -> `In the last 3 months how much did you spend on Diabetes?Loss of wages -
    ##   Rs....216`
    ## • `In the last 3 months how much did you spend on Diabetes?Other cost - Rs.` ->
    ##   `In the last 3 months how much did you spend on Diabetes?Other cost -
    ##   Rs....217`
    ## • `Were you hospitalized in the last 3 months ?` -> `Were you hospitalized in
    ##   the last 3 months ?...218`
    ## • `If yes, Reason for hospitalization` -> `If yes, Reason for
    ##   hospitalization...219`

``` r
group_var   <- "intervention/ control"
cluster_var <- "PSG group number"

# Standardize text strings: strip out accidental space/backtick artifacts safely
names(psg_data) <- trimws(names(psg_data))
names(psg_data) <- gsub("`|'", "", names(psg_data)) 
all_cols <- names(psg_data)

# Filter structural missing elements and stabilize treatment assignments
psg_data <- psg_data %>%
  filter(!is.na(.data[[group_var]]), !is.na(.data[[cluster_var]])) %>%
  mutate(clean_group = trimws(as.character(.data[[group_var]])))

# Dynamic fuzzy matching to identify columns while avoiding name duplication bugs
find_col <- function(pattern, columns) {
  matched <- columns[str_detect(tolower(columns), tolower(pattern))]
  if(length(matched) == 0) return(NULL) else return(matched[1])
}

# Strict word-boundary selection maps "Age" correctly while ignoring "Village" names
age_name     <- all_cols[str_detect(tolower(all_cols), "\\bage\\b")][1]
gender_name  <- find_col("gender|sex", all_cols)
rel_name     <- find_col("religion", all_cols)
caste_name   <- find_col("caste", all_cols)
marital_name <- find_col("marital", all_cols)   
finance_name <- find_col("financial|income", all_cols) 

hba1c_0_name <- "hba1c 0"   # Replace with your exact baseline column name
hba1c_1_name <- "hba1c 1"   # Replace with your exact 24-month column name

# ==========================================
# 2. DATA-TYPE CONVERSION PROTECTION LAYER
# ==========================================
# Forcing continuous types fields to double floats clears character merge bugs
psg_data <- psg_data %>%
  mutate(
    Age_clean       = as.numeric(as.character(.data[[age_name]])),
    Gender_clean    = as.factor(trimws(as.character(.data[[gender_name]]))),
    Religion_clean  = as.factor(trimws(as.character(.data[[rel_name]]))),
    Caste_clean     = as.factor(trimws(as.character(.data[[caste_name]]))),
    Marital_clean   = as.factor(trimws(as.character(.data[[marital_name]]))),
    Financial_clean = as.factor(trimws(as.character(.data[[finance_name]]))),
    
    HbA1c_0_clean   = as.numeric(as.character(.data[[hba1c_0_name]])),
    HbA1c_1_clean   = as.numeric(as.character(.data[[hba1c_1_name]]))
  ) %>%
  filter(!is.na(HbA1c_0_clean), !is.na(HbA1c_1_clean))
```

    ## Warning: There were 2 warnings in `mutate()`.
    ## The first warning was:
    ## ℹ In argument: `HbA1c_0_clean = as.numeric(as.character(.data[["hba1c 0"]]))`.
    ## Caused by warning:
    ## ! NAs introduced by coercion
    ## ℹ Run `dplyr::last_dplyr_warnings()` to see the 1 remaining warning.

``` r
# Standardize system matrix formula handles
names(psg_data)[names(psg_data) == "clean_group"] <- "Group"
names(psg_data)[names(psg_data) == cluster_var]  <- "ClusterID"

# ==========================================
# 3. STATISTICAL MODEL ESTIMATION (LMM ANCOVA)
# ==========================================
# A. Unconditional Null Model
null_model <- lmer(HbA1c_1_clean ~ 1 + (1 | ClusterID), data = psg_data, REML = FALSE)
```

    ## boundary (singular) fit: see help('isSingular')

``` r
# B. Adjusted Mixed ANCOVA Model
ancova_model <- lmer(
  HbA1c_1_clean ~ Group + HbA1c_0_clean + Age_clean + Gender_clean + 
                  Religion_clean + Caste_clean + Marital_clean + Financial_clean + (1 | ClusterID),
  data = psg_data, REML = FALSE
)
```

    ## boundary (singular) fit: see help('isSingular')

``` r
# Extract Coefficients Summaries
coef_matrix <- as.data.frame(summary(ancova_model)$coefficients)
coef_matrix$Parameter <- rownames(coef_matrix)

# Format rows back onto professional scientific text labels
coef_matrix$Parameter <- coef_matrix$Parameter %>%
  str_replace_all("Age_clean", "Age (Years)") %>%
  str_replace_all("Gender_clean", "Gender: ") %>%
  str_replace_all("Religion_clean", "Religion: ") %>%
  str_replace_all("Caste_clean", "Caste: ") %>%
  str_replace_all("Marital_clean", "Marital Status: ") %>%
  str_replace_all("Financial_clean", "Financial Status: ") %>%
  str_replace_all("HbA1c_0_clean", "Baseline HbA1c (hba1c 0)") %>%
  str_replace_all("Groupintervention", "Group (Intervention vs. Control)")

coef_formatted <- coef_matrix %>%
  mutate(
    # 1. Combined Estimate and SE
    `Coefficient Estimate (SE)` = sprintf("%.2f (%.2f)", Estimate, `Std. Error`),
    
    # 2. Fixed: Changed 't.value' to 't value' 
    `t-value` = sprintf("%.2f", `t value`),
    
    # 3. Formatted p-value
    `p-value` = ifelse(`Pr(>|t|)` < 0.001, "<.001", sprintf("%.3f", `Pr(>|t|)`))
  ) %>%
  select(Parameter, `Coefficient Estimate (SE)`, `t-value`, `p-value`)

# ==========================================
# 4. ROBUST EXTRACTION OF FITNESS SCORING
# ==========================================
var_components     <- as.data.frame(VarCorr(ancova_model))
rand_intercept_var <- var_components$vcov[var_components$grp == "ClusterID"]
residual_var       <- var_components$vcov[var_components$grp == "Residual"]
fixed_effects_var  <- var(predict(ancova_model, re.form = NA))

icc_val  <- rand_intercept_var / (rand_intercept_var + residual_var)
r2_marg  <- fixed_effects_var / (fixed_effects_var + rand_intercept_var + residual_var)
r2_cond  <- (fixed_effects_var + rand_intercept_var) / (fixed_effects_var + rand_intercept_var + residual_var)

fit_metrics_rows <- data.frame(
  Parameter = c("Random Effects & Model Fit Measures", 
                "  Intraclass Correlation Coefficient (ICC)", 
                "  Marginal R-squared (Fixed Variance Explained)", 
                "  Conditional R-squared (Total Variance Explained)"),
  `Coefficient Estimate (SE)` = c("", sprintf("%.3f", icc_val), sprintf("%.3f", r2_marg), sprintf("%.3f", r2_cond)),
  `t-value` = c("", "", "", ""),
  `p-value` = c("", "", "", ""),
  check.names = FALSE, stringsAsFactors = FALSE
)

final_table_dataframe <- rbind(coef_formatted, fit_metrics_rows)

# ==========================================
# 5. MANUSCRIPT TABLE EXPORT
# ==========================================
ft_hba1c <- flextable(final_table_dataframe) %>%
  set_header_labels(Parameter = "Fixed Effect / Model Parameter") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  bold(part = "header") %>%
  bold(i = which(final_table_dataframe$Parameter == "Random Effects & Model Fit Measures"), bold = TRUE) %>%
  align(j = 2:4, align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  border_remove() %>%
  hline_top(border = fp_border(width = 1.5), part = "header") %>%
  hline_bottom(border = fp_border(width = 1.5), part = "header") %>%
  hline_bottom(border = fp_border(width = 1.5), part = "body") %>%
  autofit() %>%
  add_footer_lines("Note: Dependent Variable is HbA1c at 24 Months ('hba1c 1'). Model adjusts for baseline HbA1c.") %>%
  add_footer_lines("Model includes a random intercept for cluster membership nested within the 'PSG group number' parameter vector.")

doc_hba1c <- read_docx() %>%
  body_add_fpar(fpar(ftext("Table: HbA1c Outcomes Accounted for Cluster Nested Variance", fp_text(font.size = 12, bold = TRUE, font.family = "Times New Roman")))) %>%
  body_add_flextable(ft_hba1c)

print(doc_hba1c, target = "HbA1c_ANCOVA_Mixed_Model_Report.docx")

# ==========================================
# 6. HIGH-RESOLUTION PLOT VISUALIZATION
# ==========================================
# Extract adjusted post-hoc means using emmeans for your visualization layer
adj_means_hba1c <- as.data.frame(emmeans::emmeans(ancova_model, ~ Group))
```

    ## boundary (singular) fit: see help('isSingular')

``` r
# We also pull baseline averages directly to structure the Pre-Post Trajectory Line Plot
base_means <- psg_data %>% group_by(Group) %>% summarize(m = mean(HbA1c_0_clean, na.rm=TRUE))

# Isolate grouping values to prevent vector coloring swap errors
unique_groups <- unique(psg_data$Group)
int_label     <- unique_groups[str_detect(tolower(unique_groups), "int|peer|1")][1]
ctrl_label    <- unique_groups[!unique_groups %in% int_label][1]

plot_df <- data.frame(
  Group = c(ctrl_label, ctrl_label, int_label, int_label),
  Timeline = c(0, 24, 0, 24),
  HbA1c = c(base_means$m[base_means$Group==ctrl_label], adj_means_hba1c$emmean[adj_means_hba1c$Group==ctrl_label],
            base_means$m[base_means$Group==int_label], adj_means_hba1c$emmean[adj_means_hba1c$Group==int_label]),
  lower_cl = c(NA, adj_means_hba1c$lower.CL[adj_means_hba1c$Group==ctrl_label], 
               NA, adj_means_hba1c$lower.CL[adj_means_hba1c$Group==int_label]),
  upper_cl = c(NA, adj_means_hba1c$upper.CL[adj_means_hba1c$Group==ctrl_label], 
               NA, adj_means_hba1c$upper.CL[adj_means_hba1c$Group==int_label])
)

hba1c_trajectory_plot <- ggplot(plot_df, aes(x = Timeline, y = HbA1c, color = Group, group = Group)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 3.5, aes(shape = Group)) +
  geom_errorbar(aes(ymin = lower_cl, ymax = upper_cl), width = 0.6, linewidth = 0.8, na.rm = TRUE) +
  geom_text(aes(label = sprintf("%.2f%%", HbA1c)), vjust = ifelse(plot_df$Group == int_label, 1.8, -1.8), fontface = "bold", size = 3.5, show.legend = FALSE) +
  scale_color_manual(breaks = c(int_label, ctrl_label), values = c("#006400", "#8B0000")) +
  scale_x_continuous(breaks = c(0, 24), labels = c("Baseline\n(Month 0)", "Follow-Up\n(Month 24)")) +
  labs(
    title = "Longitudinal Trajectory of Glycated Hemoglobin (HbA1c) Levels",
    subtitle = "Adjusted mixed model marginal means tracking changes between study arms",
    x = "Study Evaluation Period",
    y = "Calculated Mean HbA1c (%)"
  ) +
  theme_classic(base_family = "Times New Roman", base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
    plot.subtitle = element_text(size = 10, color = "grey30", hjust = 0.5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold")
  )

ggsave("HbA1c_Longitudinal_Trajectories.png", plot = hba1c_trajectory_plot, width = 7, height = 5.5, dpi = 300)
cat("\nAll operations successfully completed!\n")
```

    ## 
    ## All operations successfully completed!
