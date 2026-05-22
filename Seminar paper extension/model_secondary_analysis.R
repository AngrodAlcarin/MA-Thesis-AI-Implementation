library(tidyverse)
library(gt)
library(stargazer)
library(webshot)

survey_data <- readRDS("Survey_Data/survey_results_final.rds")

survey_data <- survey_data %>%
  mutate(
    PEoU = PEoU1 + PEoU3,           
    PU   = PU1 + PU2 + PU3 + PU4,
    ATU  = ATU1 + ATU2 + ATU3 + ATU4,
    AU   = AU1 + AU2 + AU3 + AU4,
    SN   = SN3,
    T    = T1 + T2 + T4,
    E    = E1,

    Private_AI_Use_bin = ifelse(Private_AI_Use == "Ja",   1,
                                ifelse(Private_AI_Use == "Nein", 0, NA)),

    Age_f = factor(Age,
                   levels = c("<20 Jahre", "20-30 Jahre", "30-40 Jahre",
                              "40-50 Jahre", "50-60 Jahre", ">60 Jahre"),
                   ordered = TRUE),
    Tenure_f = factor(Years_of_Experience,
                      levels = c("<1 Jahr", "1-3 Jahre", "3-5 Jahre",
                                 "5-10 Jahre", "10-20 Jahre", ">20 Jahre"),
                      ordered = TRUE),
    Age_num    = as.numeric(Age_f),
    Tenure_num = as.numeric(Tenure_f)
  )

extract_path <- function(model, term, H_label, path_label) {
  s     <- summary(model)
  coefs <- s$coefficients
  
  est <- coefs[term, "Estimate"]
  t   <- coefs[term, "t value"]
  p   <- coefs[term, "Pr(>|t|)"]
  se  <- coefs[term, "Std. Error"]
  
  data.frame(
    Hypothesis     = H_label,
    Path           = path_label,
    Original_Value = round(est, 3),
    T_Statistic    = round(t,   3),
    P_Value        = round(p,   3),
    STDEV          = round(se,  3),
    Results        = dplyr::case_when(
      p < 0.05 & est > 0  ~ "Significant, positive",
      p < 0.05 & est < 0  ~ "Significant, negative",
      p >= 0.05 & est > 0 ~ "Insignificant, positive",
      p >= 0.05 & est < 0 ~ "Insignificant, negative"
    ),
    stringsAsFactors = FALSE
  )
}

# H1: Subjective Norms towards AI tools are positively associated with employees’ Perceived Usefulness of AI tools.

newmod_H1 <- lm(PU ~ SN, data = survey_data)
summary(newmod_H1)

# H2: Subjective Norms towards AI tools are positively associated with employees’ Perceived Ease of Use of AI tools.

newmod_H2 <- lm(PEoU ~ SN, data = survey_data)
summary(newmod_H2)

# H3: Trust of AI tools is positively associated with employees’ Perceived Usefulness of AI tools.

newmod_H3 <- lm(PU ~ T, data = survey_data)
summary(newmod_H3)

# H4: Trust of AI tools are positively associated with employees’ Perceived Ease of Use of AI tools.

newmod_H4 <- lm(PEoU ~ T, data = survey_data)
summary(newmod_H4)

# H5: Ethics towards AI tools are positively associated with employees’ Perceived Usefulness of AI tools.

newmod_H5 <- lm(PU ~ E, data = survey_data)
summary(newmod_H5)

# H6: Ethics towards AI tools are positively associated with employees’ Perceived Ease of Use of AI tools.

newmod_H6 <- lm(PEoU ~ E, data = survey_data)
summary(newmod_H6)


# H7: Mutual moderation of SN, Trust, and Ethics on Actual Use
#   H7a: SN moderates the relationship between Trust and AU
#   H7b: SN moderates the relationship between Ethics and AU
#   H7c: Trust moderates the relationship between Ethics and AU

# H7a: Trust × SN -> AU
newmod_H7a <- lm(AU ~ T * SN, data = survey_data)
summary(newmod_H7a)

# H7b: Ethics × SN -> AU
newmod_H7b <- lm(AU ~ E * SN, data = survey_data)
summary(newmod_H7b)

# H7c: Ethics × Trust -> AU
newmod_H7c <- lm(AU ~ E * T, data = survey_data)
summary(newmod_H7c)


# H8: Subjective Norms, Trust, and Ethics are negatively associated
#             with employees' age and length of tenure.

#Age trend regressions
newmod_H8_age_SN <- lm(SN ~ Age_num, data = survey_data)
newmod_H8_age_T  <- lm(T  ~ Age_num, data = survey_data)
newmod_H8_age_E  <- lm(E  ~ Age_num, data = survey_data)

summary(newmod_H8_age_SN)
summary(newmod_H8_age_T)
summary(newmod_H8_age_E)

#Tenure trend regressions
newmod_H8_ten_SN <- lm(SN ~ Tenure_num, data = survey_data)
newmod_H8_ten_T  <- lm(T  ~ Tenure_num, data = survey_data)
newmod_H8_ten_E  <- lm(E  ~ Tenure_num, data = survey_data)

summary(newmod_H8_ten_SN)
summary(newmod_H8_ten_T)
summary(newmod_H8_ten_E)


# Results tables

# Table S1: Path coefficients for H1-H7 (direct + interaction effects)

table_S1_df <- bind_rows(
  extract_path(newmod_H1,  "SN",   "H1",  "SN → PU"),
  extract_path(newmod_H2,  "SN", "H2",  "SN → PEoU"),
  extract_path(newmod_H3,  "T",   "H3",  "T → PU"),
  extract_path(newmod_H4,  "T", "H4",  "T → PEoU"),
  extract_path(newmod_H5,  "E",   "H5",  "E → PU"),
  extract_path(newmod_H6,  "E", "H6",  "E → PEoU"),
  extract_path(newmod_H7a, "T:SN", "H7a", "T × SN → AU"),
  extract_path(newmod_H7b, "E:SN", "H7b", "E × SN → AU"),
  extract_path(newmod_H7c, "E:T",  "H7c", "E × T → AU")
)

table_S1_df

table_S1 <- table_S1_df %>%
  gt() %>%
  tab_caption("Table S1. Secondary Analysis Path Coefficients (H1–H7).") %>%
  cols_label(
    Hypothesis     = "Hypothesis",
    Path           = "Path",
    Original_Value = "Original Value",
    T_Statistic    = "T-Statistics",
    P_Value        = "P-Value",
    STDEV          = "STDEV",
    Results        = "Results"
  ) %>%
  fmt_number(
    columns = c(Original_Value, T_Statistic, P_Value, STDEV),
    decimals = 3
  ) %>%
  tab_source_note(gt::md("Source: OLS regression on construct scores (N = 220).")) %>%
  tab_options(table.font.size = px(12))

table_S1

gtsave(
  table_S1,
  "Tables_Figures_Plots/TableS1_Secondary_Path_Coefficients.png",
  vwidth  = 8000,
  vheight = 4000
)

# Table S2: H8 path coefficients — demographic effects on SN, Trust, Ethics

table_S2_df <- bind_rows(
  extract_path(newmod_H8_age_SN,  "Age_num",    "H8a", "Age → SN"),
  extract_path(newmod_H8_age_T,   "Age_num",    "H8b", "Age → T"),
  extract_path(newmod_H8_age_E,   "Age_num",    "H8c", "Age → E"),
  extract_path(newmod_H8_ten_SN,  "Tenure_num", "H8d", "Tenure → SN"),
  extract_path(newmod_H8_ten_T,   "Tenure_num", "H8e", "Tenure → T"),
  extract_path(newmod_H8_ten_E,   "Tenure_num", "H8f", "Tenure → E")
)

table_S2_df

table_S2 <- table_S2_df %>%
  gt() %>%
  tab_caption("Table S2. H8 Path Coefficients — Demographic Effects on SN, Trust, and Ethics.") %>%
  cols_label(
    Hypothesis     = "Hypothesis",
    Path           = "Path",
    Original_Value = "Original Value",
    T_Statistic    = "T-Statistics",
    P_Value        = "P-Value",
    STDEV          = "STDEV",
    Results        = "Results"
  ) %>%
  fmt_number(
    columns = c(Original_Value, T_Statistic, P_Value, STDEV),
    decimals = 3
  ) %>%
  tab_source_note(gt::md("Source: OLS regression on construct scores (N = 220).")) %>%
  tab_options(table.font.size = px(12))

table_S2

gtsave(
  table_S2,
  "Tables_Figures_Plots/TableS2_H8_Demographic_Effects.png",
  vwidth  = 8000,
  vheight = 4000
)

# stargazer HTML outputs

stargazer(newmod_H1, newmod_H2,
          type = "html", title = "H1–H2: PU/PEoU → SN",
          column.labels = c("H1: PU→SN", "H2: PEoU→SN"),
          digits = 2, out = "Tables_Figures_Plots/secondary_H1H2.html")

stargazer(newmod_H3, newmod_H4,
          type = "html", title = "H3–H4: PU/PEoU → Trust",
          column.labels = c("H3: PU→T", "H4: PEoU→T"),
          digits = 2, out = "Tables_Figures_Plots/secondary_H3H4.html")

stargazer(newmod_H5, newmod_H6,
          type = "html", title = "H5–H6: PU/PEoU → Ethics",
          column.labels = c("H5: PU→E", "H6: PEoU→E"),
          digits = 2, out = "Tables_Figures_Plots/secondary_H5H6.html")

stargazer(newmod_H7a, newmod_H7b, newmod_H7c,
          type = "html", title = "H7: Mutual Moderation of SN, Trust, Ethics on AU",
          column.labels = c("H7a: T×SN→AU", "H7b: E×SN→AU", "H7c: E×T→AU"),
          digits = 2, out = "Tables_Figures_Plots/secondary_H7.html")

stargazer(newmod_H8_age_SN, newmod_H8_age_T, newmod_H8_age_E,
          newmod_H8_ten_SN, newmod_H8_ten_T, newmod_H8_ten_E,
          type = "html", title = "H8: Age and Tenure → SN, Trust, Ethics",
          column.labels = c("Age→SN", "Age→T", "Age→E",
                            "Tenure→SN", "Tenure→T", "Tenure→E"),
          digits = 2, out = "Tables_Figures_Plots/secondary_H8.html")

# Extended TAM model including secondary analysis hypotheses
tam_data_extended <- subset(
  survey_data,
  select = c(
    PEoU1, PEoU2, PEoU3,
    PU1, PU2, PU3, PU4,
    ATU1, ATU2, ATU3, ATU4,
    T1, T2, T3, T4,
    E1, E2, E4,
    SN1, SN3,
    AU1, AU2, AU3, AU4
  )
)

mm_extended <- constructs(
  reflective("PEoU", multi_items("PEoU", 1:3)),
  reflective("PU",   multi_items("PU",   1:4)),
  reflective("ATU",  multi_items("ATU",  1:4)),
  reflective("SN",   multi_items("SN",   c(1, 3))),
  reflective("T",    multi_items("T",    1:4)),
  reflective("E",    multi_items("E",    c(1, 2, 4))),
  reflective("AU",   multi_items("AU",   1:4)),
  
  # --- Primary analysis interaction terms (unchanged) ---
  interaction_term(iv = "ATU", moderator = "T",  method = two_stage),
  interaction_term(iv = "ATU", moderator = "E",  method = two_stage),
  interaction_term(iv = "SN",  moderator = "SN", method = two_stage),
  
  # --- Secondary analysis interaction terms (H7a, H7b, H7c) ---
  interaction_term(iv = "T",   moderator = "SN", method = two_stage),  # H7a
  interaction_term(iv = "E",   moderator = "SN", method = two_stage),  # H7b
  interaction_term(iv = "E",   moderator = "T",  method = two_stage)   # H7c
)


sm_extended <- relationships(
  paths(from = "PEoU",                          to = c("PU", "ATU")),
  paths(from = "PU",                            to = "ATU"),
  paths(from = c("PEoU", "SN", "T", "E",
                 "ATU",  "PU"),                 to = "AU"),
  paths(from = "SN", to = c("PU", "PEoU")),
  paths(from = "T",  to = c("PU", "PEoU")),
  paths(from = "E",  to = c("PU", "PEoU")),
  paths(from = c("T*SN", "E*SN", "E*T"),        to = "AU")
)

# Estimation and bootstrapping

pls_mod_extended <- estimate_pls(
  data              = tam_data_extended,
  measurement_model = mm_extended,
  structural_model  = sm_extended
)

boot_pls_extended <- bootstrap_model(pls_mod_extended, nboot = 2000)

# Path coefficients table
report_paths(boot_pls_extended)

# SmartPLS-style plot
plot(pls_mod_extended,
     title = "Extended TAM with Secondary Analysis Paths",
     theme = seminr_theme_smart())

# Save as PNG
model_plot_extended <- plot(pls_mod_extended,
                            title = "Extended TAM with Secondary Analysis Paths",
                            theme = seminr_theme_smart())
htmlwidgets::saveWidget(model_plot_extended, 
                        "Tables_Figures_Plots/model_extended2.html")
webshot(
  "Tables_Figures_Plots/model_extended.html",
  file    = "Tables_Figures_Plots/model_extended2.png",
  vwidth  = 900,
  vheight = 2400,
  zoom    = 5
)
