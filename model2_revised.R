
measurement_model <- constructs(
  composite("PEoU", multi_items("PEoU", 1:3)),
  composite("PU",   multi_items("PU",   1:4)),
  composite("ATU",  multi_items("ATU",  1:4)),
  composite("E",    multi_items("E",    c(1,2,4))),
  composite("T",    multi_items("T",    1:4)),
  composite("SN",   multi_items("SN",   c(1,3))),
  composite("AU",   multi_items("AU",   1:4))
)

structural_model <- relationships(
  paths(from = "PEoU", to = c("PU", "ATU")),
  paths(from = "PU",   to = c("ATU", "AU")),
  paths(from = c("ATU","E","T","SN"), to = "AU")
)

pls_model <- estimate_pls(
  data              = survey_data_clean,
  measurement_model = measurement_model,
  structural_model  = structural_model,
  inner_weights     = path_weighting,
  missing           = mean_replacement,
  missing_value     = NA
)

summary(pls_model)
plot(summary(pls_model)$reliability)

boot <- bootstrap_model(pls_model, nboot = 5000)
summary(boot)

model_plot<-plot(pls_model, what = "paths", theme = seminr_theme_smart())
model_plot
save_plot(filename="model_plot.png",plot=model_plot)
#run model and table again, this time without PEoU2, T3, E2, E4, SN1 (loadings < 0.6)
mm2 <- constructs(
  reflective("PEoU", multi_items("PEoU", c(1,3))),
  reflective("PU",   multi_items("PU",   1:4)),
  reflective("ATU",  multi_items("ATU",  1:4)),
  reflective("E",    multi_items("E",    c(1,4))),
  reflective("T",    multi_items("T",    c(1,2,4))),
  reflective("SN",   multi_items("SN",   c(3))),
  reflective("AU",   multi_items("AU",   1:4)),
  interaction_term(iv = "ATU", moderator = "T", method = two_stage),
  interaction_term(iv = "ATU", moderator = "E", method = two_stage),
  interaction_term(iv = "SN",  moderator = "SN", method = two_stage)
)

#H1: Perceived ease of use is positively associated with employees’ perceived usefulness of AI tools
h1_model2 <- lm(PU1+PU2+PU3+PU4~PEoU1+PEoU3, data = survey_data)
summary(h1_model2)
stargazer(h1_model2, type = "html", title = "H1 Regression Results", digits = 2, out = "h1_regression2.html")
webshot("Tables_Figures_Plots/h1_regression2.html", file="h1_regression2.png", vwidth = 350,vheight=450,zoom=3)

#H2: Perceived usefulness is positively associated with employees’ positive attitude toward using AI tools.
h2_model2 <- lm(ATU1+ATU2+ATU3+ATU4~PU1+PU2+PU3+PU4, data = survey_data)
summary(h2_model2)
stargazer(h2_model2, type = "html", title = "H2 Regression Results", digits = 2, out = "h2_regression2.html")
webshot("Tables_Figures_Plots/h2_regression2.html", file="h2_regression2.png",vwidth=370,vheight=500,zoom=3)

#H3: A positive attitude toward AI tools is positively associated with employees’ actual use in daily work.
h3_model2 <- lm(AU1+AU2+AU3+AU4~ATU1+ATU2+ATU3+ATU4, data = survey_data)
summary(h3_model2)
stargazer(h3_model2, type = "html", title = "H3 Regression Results", digits = 2, out = "h3_regression2.html")
webshot("Tables_Figures_Plots/h3_regression2.html", file="h3_regression2.png",vwidth=350,vheight=500,zoom=3)

#H4: Perceived ease of use is positively associated with employees’ positive attitude toward AI tools.
h4_model2 <- lm(ATU1+ATU2+ATU3~PEoU1+PEoU3, data = survey_data)
summary(h4_model2)
stargazer(h4_model2, type = "html", title = "H4 Regression Results", digits = 2, out = "h4_regression2.html")
webshot("Tables_Figures_Plots/h4_regression2.html", file="h4_regression2.png",vwidth=350,vheight=450,zoom=3)

#H5: Subjective norms are positively associated with employees’ actual use of AI tools.
h5_model2 <- lm(AU1+AU2+AU3+AU4~SN3, data = survey_data)
summary(h5_model2)
stargazer(h5_model2, type = "html", title = "H5 Regression Results", digits = 2, out = "h5_regression2.html")
webshot("Tables_Figures_Plots/h5_regression2.html", file="h5_regression2.png",vwidth=345,vheight=300,zoom=3)

#H6: The relationship between subjective norms and actual use of AI tools by employees follows a non-linear (quadratic) pattern.
h6_model2 <- lm(AU1+AU2+AU3+AU4~SN3+I(SN3^2), data = survey_data)
summary(h6_model2)
stargazer(h6_model2, type = "html", title = "H6 Regression Results", digits = 2, out = "h6_regression2.html")
webshot("Tables_Figures_Plots/h6_regression2.html", file="h6_regression2.png",vwidth=350,vheight=450,zoom=3)

#H7: Employee’s self-reported private use of AI tools like ChatGPT positively predict their Actual Use of AI tools in the workplace.
h7_model2 <- lm(AU1+AU2+AU3+AU4~Private_AI_Use, data = survey_data)
summary(h7_model2)
stargazer(h7_model2, type = "html", title = "H7 Regression Results", digits = 2, out = "h7_regression2.html")
webshot("Tables_Figures_Plots/h7_regression2.html", file="h7_regression2.png",vwidth=360,vheight=240,zoom=3)

#H8: Employees’ trust in the AI tool positively predicts employees’ actual use of AI tools
h8_model2 <- lm(AU1+AU2+AU3+AU4~T1+T2+T4, data = survey_data)
summary(h8_model2)
stargazer(h8_model2, type = "html", title = "H8 Regression Results", digits = 2, out = "h8_regression2.html")
webshot("Tables_Figures_Plots/h8_regression2.html", file="h8_regression2.png",vwidth=350,vheight=450,zoom=3)

#H9: Trust strengthens the relationship between Attitude Toward Use and employees’ Actual Use of AI tools.
h9_model2 <- lm(AU1+AU2+AU3+AU4~ATU1+ATU2+ATU3+ATU4*T1+T2+T4, data = survey_data)
summary(h9_model2)
stargazer(h9_model2, type = "html", title = "H9 Regression Results", digits = 2, out = "h9_regression2.html")
webshot("Tables_Figures_Plots/h9_regression2.html", file="h9_regression2.png",vwidth=350,vheight=450,zoom=3)

#H10: Ethics positively predict employees’ Actual Use in using AI tools.
h10_model2 <- lm(AU1+AU2+AU3+AU4~E1+E4, data = survey_data)
summary(h10_model2)
stargazer(h10_model2, type = "html", title = "H10 Regression Results", digits = 2, out = "h10_regression2.html")
webshot("Tables_Figures_Plots/h10_regression2.html", file="h10_regression2.png",vwidth=350,vheight=450,zoom=3)

#H11: Ethics strengthens the relationship between Attitude Toward Use and employees’ Actual Use in using AI tool.
h11_model2 <- lm(AU1+AU2+AU3+AU4~ATU1+ATU2+ATU3+ATU4*E1+E4, data = survey_data)
summary(h11_model2)
stargazer(h11_model2, type = "html", title = "H11 Regression Results", digits = 2, out = "h11_regression2.html")
webshot("Tables_Figures_Plots/h11_regression2.html", file="h11_regression2.png",vwidth=350,vheight=450,zoom=3)

#H12: Perceived ease of use, perceived usefulness, positive attitude toward and trust in AI tools is negatively associated with employees’ age and length of tenure.
h12_model2 <- lm(PEoU1+PEoU3+PU1+PU2+PU3+PU4+ATU1+ATU2+ATU3+ATU4+T1+T2+T4~Age+Years_of_Experience, data = survey_data)
summary(h12_model2)
stargazer(h12_model2, type = "html", title = "H12 Regression Results", digits = 2, out = "h12_regression2.html")
webshot("Tables_Figures_Plots/h12_regression2.html", file="h12_regression2.png",vwidth=960,vheight=600,zoom=4)

#estimate + bootstrap mm2
pls_mod2  <- estimate_pls(data = tam_data,
                          measurement_model = mm2,
                          structural_model  = sm)
boot_pls2 <- bootstrap_model(pls_mod2, nboot = 2000)
summary(boot_pls2)

#Path table (H1–H10)
report_paths(boot_pls2)

#table 10 again, but for mm2
load_tab2 <- as.data.frame(pls_mod2$outer_loadings)
load_tab2$Item <- rownames(load_tab2)
#table 10, composite reliability and outer loadings of model
load_long2 <- load_tab2 |>
  pivot_longer(
    cols      = -Item,
    names_to  = "Construct",
    values_to = "Loading"
  ) |>
  filter(!is.na(Loading))
rel_ave_mat2 <- rhoC_AVE(pls_mod2)
rel_ave_df2  <- as.data.frame(rel_ave_mat2)
rel_ave_df2$Construct <- rownames(rel_ave_df2)
#rename columns
colnames(rel_ave_df2)[1:2] <- c("Composite_Reliability", "AVE")
table10_like2 <- load_long2 |>
  left_join(rel_ave_df2, by = "Construct") %>%
  arrange(Construct, Item)

keep_items2 <- c(
  "PEoU1", "PEoU3",
  "PU1", "PU2", "PU3", "PU4",
  "ATU1", "ATU2", "ATU3", "ATU4",
  "E1",
  "T1", "T2", "T4",
  "SN3",
  "AU1", "AU2", "AU3", "AU4"
)
table10_filtered2 <- table10_like2 %>%
  filter(Item %in% keep_items2) %>%
  filter(Loading != 0) %>%
  arrange(Construct, Item)
table10_layout2 <- table10_filtered2 %>%
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
table10_for_gt2 <- table10_layout2 %>%
  left_join(construct_titles, by = "Construct") %>%
  arrange(order) %>%
  select(Construct_Title, Question, Item, Loading,
         Composite_Reliability, AVE)
#round to 3 decimals BEFORE sending to gt
table10_for_gt2 <- table10_for_gt2 %>%
  mutate(
    Loading               = round(Loading, 3),
    Composite_Reliability = round(Composite_Reliability, 3),
    AVE                   = round(AVE, 3)
  )
#Produce Mustofa-style grouped GT table
table10_for_gt_wrapped2 <- table10_for_gt2 %>%
  mutate(
    Question = str_wrap(Question, width = 70)
  )
gt_table2 <- table10_for_gt_wrapped2 %>%
  gt(groupname_col = "Construct_Title") %>%
  tab_header(title = "Table 10. Construct validity and reliability (Outer Model) - Revised") %>%
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
gt_table2
#save table as png
gtsave(
  gt_table2,
  "Tables_Figures_Plots/Tabelle10_Konstruktgueltigkeit_Revised.png",
  vwidth  = 5000, 
  vheight = 1800
)