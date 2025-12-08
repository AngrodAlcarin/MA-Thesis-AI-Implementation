##install.packages(c("stargazer","seminr", "plspm", "psych", "lavaan", "semTools", "corrr","webshot2","gt","car"))
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(magrittr)
library(lubridate)
library(hms)
library(forcats)
library(crayon)
library(scales)
library(ggrepel)
library(stargazer)
library(seminr)
library(plspm)
library(psych)
library(lavaan)
library(semTools)
library(webshot2)
library(gt)
library(car)

survey_data <- readRDS("Survey_Data/survey_results_final.rds")

#stargazer summary of dataset
stargazer(survey_data, type = "html", title = "Summary Statistics of Survey Data", digits = 2, out = "Tables_Figures_Plots/survey_summary.html")

#H1: Perceived ease of use is positively associated with employees’ perceived usefulness of AI tools
h1_model <- lm(PU1+PU2+PU3+PU4~PEoU1+PEoU2+PEoU3, data = survey_data)
summary(h1_model)
stargazer(h1_model, type = "html", title = "H1 Regression Results", digits = 2, out = "h1_regression.html")
webshot("Tables_Figures_Plots/h1_regression.html", file="h1_regression.png", vwidth = 350,vheight=450,zoom=3)

#H2: Perceived usefulness is positively associated with employees’ positive attitude toward using AI tools.
h2_model <- lm(ATU1+ATU2+ATU3+ATU4~PU1+PU2+PU3+PU4, data = survey_data)
summary(h2_model)
stargazer(h2_model, type = "html", title = "H2 Regression Results", digits = 2, out = "h2_regression.html")
webshot("Tables_Figures_Plots/h2_regression.html", file="h2_regression.png",vwidth=370,vheight=500,zoom=3)

#H3: A positive attitude toward AI tools is positively associated with employees’ actual use in daily work.
h3_model <- lm(AU1+AU2+AU3+AU4~ATU1+ATU2+ATU3+ATU4, data = survey_data)
summary(h3_model)
stargazer(h3_model, type = "html", title = "H3 Regression Results", digits = 2, out = "h3_regression.html")
webshot("Tables_Figures_Plots/h3_regression.html", file="h3_regression.png",vwidth=350,vheight=500,zoom=3)

#H4: Perceived ease of use is positively associated with employees’ positive attitude toward AI tools.
h4_model <- lm(ATU1+ATU2+ATU3~PEoU1+PEoU2+PEoU3, data = survey_data)
summary(h4_model)
stargazer(h4_model, type = "html", title = "H4 Regression Results", digits = 2, out = "h4_regression.html")
webshot("Tables_Figures_Plots/h4_regression.html", file="h4_regression.png",vwidth=350,vheight=450,zoom=3)

#H5: Subjective norms are positively associated with employees’ actual use of AI tools.
h5_model <- lm(AU1+AU2+AU3+AU4~SN1+SN3, data = survey_data)
summary(h5_model)
stargazer(h5_model, type = "html", title = "H5 Regression Results", digits = 2, out = "h5_regression.html")
webshot("Tables_Figures_Plots/h5_regression.html", file="h5_regression.png",vwidth=345,vheight=300,zoom=3)

#H6.1: Subjective norms weaken the relationship between Attitude toward Use and employees’ Actual Use of AI tools.
h6.1_model <- lm(AU1+AU2+AU3+AU4~(SN1+SN3)*(ATU1+ATU2+ATU3), data = survey_data)
summary(h6.1_model)
stargazer(h6.1_model, type = "html", title = "H6.1 Regression Results", digits = 2, out = "h6.1_regression.html")
webshot("Tables_Figures_Plots/h6.1_regression.html", file="h6.1_regression.png",vwidth=350,vheight=450,zoom=3)

#H6.2: The relationship between subjective norms and actual use of AI tools by employees follows a non-linear (quadratic) pattern.
h6.2_model <- lm(AU1+AU2+AU3+AU4~(SN1 + I(SN1^2))+(SN3 + I(SN3^2)), data = survey_data)
summary(h6.2_model)
stargazer(h6.2_model, type = "html", title = "H6.2 Regression Results", digits = 2, out = "h6.2_regression.html")
webshot("Tables_Figures_Plots/h6.2_regression.html", file="h6.2_regression.png",vwidth=350,vheight=450,zoom=3)

#H7: Employee’s self-reported private use of AI tools like ChatGPT positively predict their Actual Use of AI tools in the workplace.
h7_model <- lm(AU1+AU2+AU3+AU4~Private_AI_Use, data = survey_data)
summary(h7_model)
stargazer(h7_model, type = "html", title = "H7 Regression Results", digits = 2, out = "h7_regression.html")
webshot("Tables_Figures_Plots/h7_regression.html", file="h7_regression.png",vwidth=360,vheight=240,zoom=3)

#H8: Employees’ trust in the AI tool positively predicts employees’ actual use of AI tools
h8_model <- lm(AU1+AU2+AU3+AU4~T1+T2+T3+T4, data = survey_data)
summary(h8_model)
stargazer(h8_model, type = "html", title = "H8 Regression Results", digits = 2, out = "h8_regression.html")
webshot("Tables_Figures_Plots/h8_regression.html", file="h8_regression.png",vwidth=350,vheight=450,zoom=3)

#H9: Trust strengthens the relationship between Attitude Toward Use and employees’ Actual Use of AI tools.
h9_model <- lm(AU1+AU2+AU3+AU4~(ATU1+ATU2+ATU3+ATU4)*(T1+T2+T3+T4), data = survey_data)
summary(h9_model)
stargazer(h9_model, type = "html", title = "H9 Regression Results", digits = 2, out = "h9_regression.html")
webshot("Tables_Figures_Plots/h9_regression.html", file="h9_regression.png",vwidth=350,vheight=450,zoom=3)

#H10: Ethics positively predict employees’ Actual Use in using AI tools.
h10_model <- lm(AU1+AU2+AU3+AU4~E1+E2+E4, data = survey_data)
summary(h10_model)
stargazer(h10_model, type = "html", title = "H10 Regression Results", digits = 2, out = "h10_regression.html")
webshot("Tables_Figures_Plots/h10_regression.html", file="h10_regression.png",vwidth=350,vheight=450,zoom=3)

#H11: Ethics strengthens the relationship between Attitude Toward Use and employees’ Actual Use in using AI tool.
h11_model <- lm(AU1+AU2+AU3+AU4~(ATU1+ATU2+ATU3+ATU4)*(E1+E2+E4), data = survey_data)
summary(h11_model)
stargazer(h11_model, type = "html", title = "H11 Regression Results", digits = 2, out = "h11_regression.html")
webshot("Tables_Figures_Plots/h11_regression.html", file="h11_regression.png",vwidth=350,vheight=450,zoom=3)

#H12: Perceived ease of use, perceived usefulness, positive attitude toward and trust in AI tools is negatively associated with employees’ age and length of tenure.
h12_model <- lm(PEoU1+PEoU2+PEoU3+PU1+PU2+PU3+PU4+ATU1+ATU2+ATU3+ATU4+T1+T2+T3+T4~Age+Years_of_Experience, data = survey_data)
summary(h12_model)
stargazer(h12_model, type = "html", title = "H12 Regression Results", digits = 2, out = "h12_regression.html")
webshot("Tables_Figures_Plots/h12_regression.html", file="h12_regression.png",vwidth=960,vheight=600,zoom=4)

#SPLS analysis
indicators <- c(
  paste0("PEoU", 1:3),
  paste0("PU",   1:4),
  paste0("ATU",  1:4),
  paste0("E",    c(1,2,4)),
  paste0("T",    1:4),
  paste0("SN",   c(1,3)),     
  paste0("AU",   1:4)
)
# Missing columns?
setdiff(indicators, names(survey_data))
# Which columns are list-columns?
which(sapply(survey_data[indicators], is.list))
# Quick glimpse of types
str(survey_data[indicators])

survey_data_clean <- survey_data |>
  mutate(across(all_of(indicators), ~{
    x <- .
    if (is.list(x)) x <- unlist(x, use.names = FALSE)
    x <- suppressWarnings(as.numeric(as.character(x)))
    x
  }))

survey_data_clean <- survey_data_clean |> select(all_of(indicators))

#add moderating variables
#clean data
tam_data <- subset(
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

#measurement model
mm <- constructs(
  reflective("PEoU", multi_items("PEoU", 1:3)),
  reflective("PU",   multi_items("PU",   1:4)),
  reflective("ATU",  multi_items("ATU",  1:4)),
  reflective("SN",   multi_items("SN",   c(1, 3))),
  reflective("T",    multi_items("T",    1:4)),
  reflective("E",    multi_items("E",    c(1, 2, 4))),
  reflective("AU",   multi_items("AU",   1:4)),
  interaction_term(iv = "ATU", moderator = "T", method = two_stage),
  interaction_term(iv = "ATU", moderator = "E", method = two_stage),
  interaction_term(iv = "SN",  moderator = "SN", method = two_stage)
)

#structural model
sm <- relationships(
  paths(from = "PEoU", to = c("PU", "ATU")),
  paths(from = "PU",   to = "ATU"),
  paths(from = c("PEoU","SN","T","E","ATU","PU"), to = "AU"),
  paths(from = c("ATU*T","ATU*E","SN*SN"), to = "AU")
)

#estimate + bootstrap
pls_mod  <- estimate_pls(data = tam_data,
                         measurement_model = mm,
                         structural_model  = sm)

boot_pls <- bootstrap_model(pls_mod, nboot = 2000)

#Path table (H1–H10)
report_paths(boot_pls)

# SmartPLS-style plot (blue constructs, yellow indicators)
plot(pls_mod,
     title = "TAM with moderators and quadratic effect",
     theme = seminr_theme_smart())
#save plot as png with white background
model_plot2 <- plot(pls_mod,
                    title = "TAM with moderators and quadratic effect",
                    theme = seminr_theme_smart())
webshot("Tables_Figures_Plots/model.html", file = "model_with_moderators.png",
                 vwidth = 1000, vheight = 500, zoom = 5)

load_tab <- as.data.frame(pls_mod$outer_loadings)
load_tab$Item <- rownames(load_tab)

#table 10, composite reliability and outer loadings of model
load_long <- load_tab |>
  pivot_longer(
    cols      = -Item,
    names_to  = "Construct",
    values_to = "Loading"
  ) |>
  filter(!is.na(Loading))

rel_ave_mat <- rhoC_AVE(pls_mod)
rel_ave_df  <- as.data.frame(rel_ave_mat)
rel_ave_df$Construct <- rownames(rel_ave_df)

#rename columns
colnames(rel_ave_df)[1:2] <- c("Composite_Reliability", "AVE")

table10_like <- load_long |>
  left_join(rel_ave_df, by = "Construct") |>
  arrange(Construct, Item)

keep_items <- c(
  "PEoU1", "PEoU2", "PEoU3",
  "PU1", "PU2", "PU3", "PU4",
  "ATU1", "ATU2", "ATU3", "ATU4",
  "E1", "E2", "E4",
  "T1", "T2", "T3", "T4",
  "SN1", "SN3",
  "AU1", "AU2", "AU3", "AU4"
)

table10_filtered <- table10_like %>%
  filter(Item %in% keep_items) %>%
  filter(Loading != 0) %>%
  arrange(Construct, Item)

item_meta <- tibble::tribble(
  ~Item,   ~Question,                                                                                                      ~order,
  # PEoU
  "PEoU1", "Meiner Meinung nach ist KI-Technologie einfach zu benutzen.",                                                 1,
  "PEoU2", "Ich glaube, KI-Technologien wie ChatGPT benötigen keine speziellen Fähigkeiten, 
  um sie zu benutzen.",         2,
  "PEoU3", "Es fällt mir leicht, die Funktionen von KI-Technologien wie ChatGPT zu erlernen.",                            3,
  
  # PU
  "PU1",   "Ich denke, dass die Nutzung von KI-Tools in meiner Arbeit nützlich ist.",                                      4,
  "PU2",   "Fast alle meine Arbeitstätigkeiten sind von KI-Tools unterstützt.",                                           5,
  "PU3",   "KI-Tools wie ChatGPT ermöglichen mir die schnellere Erledigung von Arbeitsaufträgen.",                        6,
  "PU4",   "Ich würde KI-Tools wie ChatGPT Freunden und Bekannten empfehlen, weil sie nützlich sind.",                    7,
  
  # ATU
  "ATU1",  "Für die Erledigung von Arbeitsaufträgen werde ich KI-Tools wie ChatGPT verwenden.",                           8,
  "ATU2",  "Die Existenz von KI-Tools wie ChatGPT ist essentiell in meiner Arbeitstätigkeit.",                            9,
  "ATU3",  "Es füllt mich mit Befriedigung, KI-Tools wie ChatGPT für meine Arbeitsbedürfnisse optimieren zu können.",    10,
  "ATU4",  "KI-Tools wie ChatGPT sind in der Lage, mein Potenzial bei der Arbeit zu optimieren.",                        11,
  
  # Ethics
  "E1",    "Ich denke nicht, dass die Verwendung von KI-Tools wie ChatGPT ethisch bedenklich ist.",                      12,
  "E2",    "Ich denke, es ist notwendig, mit meinen Vorgesetzten die Nutzung von KI-Tools wie ChatGPT zu besprechen.",  13,
  "E4",    "Ich weise die Nutzung von KI-Tools wie ChatGPT bei Präsentationen meiner Arbeit stets aus.",                 14,
  
  # Trust
  "T1",    "Ich habe Vertrauen in die Vorschläge von KI-Tools wie ChatGPT.",                                             15,
  "T2",    "Ich kann mich für die Erledigung von Arbeitsaufträgen auf KI-Tools wie ChatGPT verlassen.",                 16,
  "T3",    "Antworten oder Vorschläge von KI-Tools wie ChatGPT liefern die besten Resultate bei der Analyse verschiedener Datenquellen.", 17,
  "T4",    "KI-Tools wie ChatGPT können Probleme bei der Erfüllung von Arbeitsaufträgen lösen, bei denen ich Mühe habe.",18,
  
  # Subjective norms
  "SN1",   "Ich denke nicht, dass es ein Problem ist, sich auf KI-Tools wie ChatGPT zu verlassen.",                      19,
  "SN3",   "KI-Tools wie ChatGPT bei der Arbeit zu verwenden entspricht meinen persönlichen Werten.",                    20,
  
  # Actual use
  "AU1",   "Ich verwende KI-Tools wie ChatGPT, seit ich von den Vorteilen gehört habe.",                                 21,
  "AU2",   "Die Verwendung von KI-Tools wie ChatGPT ermöglicht mir die Optimierung meiner Arbeit.",                      22,
  "AU3",   "Ich würde die Verwendung von KI-Tools wie ChatGPT anderen empfehlen.",                                       23,
  "AU4",   "Arbeitsaufträge ohne die Verwendung von KI-Tools wie ChatGPT zu erfüllen, fühlt sich ineffizient an.",      24
)

item_meta_en <- tibble::tribble(
  ~Item,   ~Question,                                                                                                      ~order,
  # PEoU
  "PEoU1", "In my opinion, AI tools like ChatGPT are easy to use",                                                 1,
  "PEoU2", "In my opinion, AI tools like ChatGPT do not require any special skills to operate it",         2,
  "PEoU3", "I can easily learn the features of AI tools like ChatGPT as needed",                            3,
  
  # PU
  "PU1",   "In my opinion, AI tools like ChatGPT are beneficial in my day-to-day work",                                      4,
  "PU2",   "Currently, almost all of my work activities are assisted by AI tools like ChatGPT",                                           5,
  "PU3",   "AI tools like ChatGPT make it easier for me to complete my work assignments faster",                        6,
  "PU4",   "I would recommend AI tools like ChatGPT to my friends because of its great benefits",                    7,
  
  # ATU
  "ATU1",  "When I have a work assignment, I will choose to use AI tools like ChatGPT",                           8,
  "ATU2",  "The existence of AI tools like ChatGPT is essential in my day-to-day work",                            9,
  "ATU3",  "Being able to optimise AI tools like ChatGPT for my needs is a satisfying thing",    10,
  "ATU4",  "AI tools like ChatGPT were able to optimise my potential at work",                        11,
  
  # Ethics
  "E1",    "I do not think using AI tools like ChatGPT violates work ethics",                      12,
  "E2",    "I consider it necessary to consult with bosses first before using AI tools like ChatGPT",  13,
  "E4",    "I listed in any section, assignment/presentation/document related to the use of AI tools like ChatGPT",                 14,
  
  # Trust
  "T1",    "I believe in the results of AI tools like ChatGPT recommendations",                                             15,
  "T2",    "I can rely on AI tools like ChatGPT to answer/complete work assignments",                 16,
  "T3",    "Answers/Recommendations from AI tools like ChatGPT are the best results of analysis that considers various data", 17,
  "T4",    "AI tools like ChatGPT may be able to solve various work problems that I have",18,
  
  # Subjective norms
  "SN1",   "I do not think it is a problem to use AI tools like ChatGPT fully",                      19,
  "SN3",   "Using AI tools like ChatGPT at work is according to the values I believe in",                    20,
  
  # Actual use
  "AU1",   "I have been using AI tools like ChatGPT a lot since I learned about its benefits",                                 21,
  "AU2",   "Using AI tools like ChatGPT was able to optimise my work assignments",                      22,
  "AU3",   "I would recommend AI tools like ChatGPT to others in need",                                       23,
  "AU4",   "Doing tasks without using AI tools like ChatGPT feels less than optimal",      24
)

construct_titles <- tribble(
  ~Construct, ~Construct_Title,
  "PEoU", "Perceived Ease of Use",
  "PU",   "Perceived Usefulness",
  "ATU",  "Attitude toward using AI",
  "E",    "Ethics",
  "T",    "Trust",
  "SN",   "Subjective Norms",
  "AU",   "Actual Use of AI"
)


table10_layout <- table10_filtered %>%
  left_join(item_meta, by = "Item") %>%
  arrange(order) %>%
  select(Construct, Question, Item, Loading,
         Composite_Reliability, AVE, order) %>%   # keep `order` for now
  group_by(Construct) %>%
  mutate(
    Composite_Reliability = if_else(row_number() == 1, Composite_Reliability, NA_real_),
    AVE                   = if_else(row_number() == 1, AVE,                   NA_real_)
  ) %>%
  ungroup()

#Build table with construct titles and keep Construct temporarily
table10_for_gt <- table10_layout %>%
  left_join(construct_titles, by = "Construct") %>%
  arrange(order) %>%
  select(Construct_Title, Question, Item, Loading,
         Composite_Reliability, AVE)

#round to 3 decimals BEFORE sending to gt
table10_for_gt <- table10_for_gt %>%
  mutate(
    Loading               = round(Loading, 3),
    Composite_Reliability = round(Composite_Reliability, 3),
    AVE                   = round(AVE, 3)
  )

#Produce Mustofa-style grouped GT table
table10_for_gt_wrapped <- table10_for_gt %>%
  mutate(
    Question = str_wrap(Question, width = 70)
  )

gt_table <- table10_for_gt_wrapped %>%
  gt(groupname_col = "Construct_Title") %>%
  tab_header(title = "Table 10. Construct validity and reliability (Outer Model)") %>%
  cols_label(
    Question              = "Item",
    Item                  = "Code",
    Loading               = "Loading",
    Composite_Reliability = "CR",
    AVE                   = "AVE"
  ) %>%
  sub_missing(everything(), missing_text = "") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  cols_hide(columns = c(Construct_Title)) %>%
  cols_width(
    Question              ~ px(210),
    Item                  ~ px(20),
    Loading               ~ px(20),
    Composite_Reliability ~ px(20),
    AVE                   ~ px(20)
  ) %>%
  tab_options(
    table.width      = px(925),
    data_row.padding = px(4)
  )%>%
  tab_style(
    style = cell_text(color = "red"),
    locations = cells_body(
      columns = Loading,
      rows = Loading < 0.708
    ))%>%
      
    tab_style(
      style = cell_text(color = "orange"),
      locations = cells_body(
        columns = Loading,
        rows = Loading >= 0.6 & Loading < 0.708
        )
  )%>%
  tab_style(
    style = cell_text(color = "red"),
    locations = cells_body(
      columns = Composite_Reliability,
      rows = Composite_Reliability < 0.708
    )) %>% 
  tab_style(
    style = cell_text(color = "orange"),
    locations = cells_body(
      columns = Composite_Reliability,
      rows = Composite_Reliability >= 0.6 & Composite_Reliability < 0.708
    ))
gt_table

#save table as png
gtsave(
  gt_table,
  "Tables_Figures_Plots/Tabelle10_Konstruktgueltigkeit.png",
  vwidth  = 5000, 
  vheight = 1800
)

#table 11: Path coefficients
survey_data <- survey_data %>%
  mutate(
    PEoU = PEoU1 + PEoU3, #PEoU2 removed due to low loading
    PU   = PU1 + PU2 + PU3 + PU4,
    ATU  = ATU1 + ATU2 + ATU3 + ATU4,
    AU   = AU1 + AU2 + AU3 + AU4,
    SN   = SN3, #SN1 removed due to low loading
    T    = T1 + T2 + T4, #T3 removed due to low loading        
    E    = E1       #E2 and E4 removed due to low loading
  ) %>%
  mutate(
    Private_AI_Use_bin = ifelse(Private_AI_Use == "Ja", 1,
                                ifelse(Private_AI_Use == "Nein", 0, NA))
  )%>%
  mutate(
    Age_f = factor(Age,
                   levels = c("<20 Jahre", "20-30 Jahre", "30-40 Jahre",
                              "40-50 Jahre", "50-60 Jahre", ">60 Jahre"),
                   ordered = TRUE),
    Tenure_f = factor(Years_of_Experience,
                      levels = c("<1 Jahr","1-3 Jahre", "3-5 Jahre", "5-10 Jahre",
                                 "10-20 Jahre", ">20 Jahre"),
                      ordered = TRUE)
  )

# H1: PEoU → PU
mod_H1 <- lm(PU ~ PEoU, data = survey_data)

# H2: PU → ATU
mod_H2 <- lm(ATU ~ PU, data = survey_data)

# H3: ATU → AU
mod_H3 <- lm(AU ~ ATU, data = survey_data)

# H4: PEoU → ATU
mod_H4 <- lm(ATU ~ PEoU, data = survey_data)

# H5: SN → AU
mod_H5 <- lm(AU ~ SN, data = survey_data)

# H6.1: SN*ATU → AU
mod_H6.1 <- lm(AU ~ ATU * SN, data = survey_data)

# H6.2: quadratic SN → AU  (focus on curvature)
mod_H6.2 <- lm(AU ~ SN + I(SN^2), data = survey_data)

# H7: Private_AI_Use → AU
mod_H7 <- lm(AU ~ Private_AI_Use_bin, data = survey_data)

# H8: Trust → AU
mod_H8 <- lm(AU ~ T, data = survey_data)

# H9: Trust moderates ATU → AU (interaction term)
mod_H9 <- lm(AU ~ ATU * T, data = survey_data)

# H10: Ethics → AU
mod_H10 <- lm(AU ~ E, data = survey_data)

# H11: Ethics moderates ATU → AU
mod_H11 <- lm(AU ~ ATU * E, data = survey_data)

extract_path <- function(model, term, H_label, path_label) {
  s <- summary(model)
  coefs <- s$coefficients
  
  est <- coefs[term, "Estimate"]
  t   <- coefs[term, "t value"]
  p   <- coefs[term, "Pr(>|t|)"]
  se  <- coefs[term, "Std. Error"]
  
  data.frame(
    Hypothesis    = H_label,
    Path          = path_label,
    Original_Value = round(est, 3),
    T_Statistic    = round(t, 3),
    P_Value        = round(p, 3),
    STDEV          = round(se, 3),
    Results        = dplyr::case_when(
      p < 0.05 & est > 0 ~ "Significant, positive",
      p < 0.05 & est < 0 ~ "Significant, negative",
      p >= 0.05 & est > 0 ~ "Insignificant, positive",
      p >= 0.05 & est < 0 ~ "Insignificant, negative"
    ),
    stringsAsFactors = FALSE
  )
}

table11_df <- bind_rows(
  extract_path(mod_H1,  "PEoU",        "H1",  "PEoU → PU"),
  extract_path(mod_H2,  "PU",          "H2",  "PU → ATU"),
  extract_path(mod_H3,  "ATU",         "H3",  "ATU → AU"),
  extract_path(mod_H4,  "PEoU",        "H4",  "PEoU → ATU"),
  extract_path(mod_H5,  "SN",          "H5",  "SN → AU"),
  extract_path(mod_H6.1,  "ATU:SN",     "H6.1",  "SN x ATU → AU"),
  extract_path(mod_H6.2, "I(SN^2)", "H6.2", "SN² → AU"),
  extract_path(mod_H7,  "Private_AI_Use_bin", "H7", "Private AI use → AU"),
  extract_path(mod_H8,  "T",           "H8",  "Trust → AU"),
  extract_path(mod_H9,  "ATU:T",       "H9",  "Trust × ATU → AU"),
  extract_path(mod_H10, "E",           "H10", "Ethics → AU"),
  extract_path(mod_H11, "ATU:E",       "H11", "Ethics × ATU → AU")
)

table11_df

table11 <- table11_df %>%
  gt() %>%
  tab_caption("Table 11. Path coefficients.") %>%
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

table11
#save png
gtsave(
  table11,
  "Tables_Figures_Plots/Tabelle11_Path_Coefficients2.png",
  vwidth  = 8000, 
  vheight = 4000
)

#hyp12
#H12: Age regressions
mod_H12_1 <- lm(PEoU ~ Age_f, data = survey_data)
mod_H12_2 <- lm(PU   ~ Age_f, data = survey_data)
mod_H12_3 <- lm(ATU  ~ Age_f, data = survey_data)
mod_H12_4 <- lm(AU    ~ Age_f, data = survey_data)

#H12: Tenure regressions
mod_H12_5 <- lm(PEoU ~ Tenure_f, data = survey_data)
mod_H12_6 <- lm(PU   ~ Tenure_f, data = survey_data)
mod_H12_7 <- lm(ATU  ~ Tenure_f, data = survey_data)
mod_H12_8 <- lm(AU    ~ Tenure_f, data = survey_data)

survey_data <- survey_data %>%
  mutate(
    Age_num    = as.numeric(Age_f),
    Tenure_num = as.numeric(Tenure_f)
  )

# Linear trend regressions
mod_H12_age_PEOU  <- lm(PEoU ~ Age_num, data = survey_data)
mod_H12_age_PU    <- lm(PU   ~ Age_num, data = survey_data)
mod_H12_age_ATU   <- lm(ATU  ~ Age_num, data = survey_data)
mod_H12_age_AU     <- lm(AU    ~ Age_num, data = survey_data)

mod_H12_ten_PEOU  <- lm(PEoU ~ Tenure_num, data = survey_data)
mod_H12_ten_PU    <- lm(PU   ~ Tenure_num, data = survey_data)
mod_H12_ten_ATU   <- lm(ATU  ~ Tenure_num, data = survey_data)
mod_H12_ten_AU     <- lm(AU    ~ Tenure_num, data = survey_data)

#table 12: H12 regression results
table12_df <- bind_rows(
  extract_path(mod_H12_age_PEOU, "Age_num",    "H12a", "Age → PEoU"),
  extract_path(mod_H12_age_PU,   "Age_num",    "H12b", "Age → PU"),
  extract_path(mod_H12_age_ATU,  "Age_num",    "H12c", "Age → ATU"),
  extract_path(mod_H12_age_AU,    "Age_num",    "H12d", "Age → AU"),
  extract_path(mod_H12_ten_PEOU, "Tenure_num", "H12e", "Tenure → PEoU"),
  extract_path(mod_H12_ten_PU,   "Tenure_num", "H12f", "Tenure → PU"),
  extract_path(mod_H12_ten_ATU,  "Tenure_num", "H12g", "Tenure → ATU"),
  extract_path(mod_H12_ten_AU,    "Tenure_num", "H12h", "Tenure → AU")
)
table12_df
table12 <- table12_df %>%
  gt() %>%
  tab_caption("Table 12. H12 Path coefficients.") %>%
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
table12
#save png
gtsave(
  table12,
  "Tables_Figures_Plots/Tabelle12_H12_Regression_Results.png",
  vwidth  = 8000, 
  vheight = 4000
)
