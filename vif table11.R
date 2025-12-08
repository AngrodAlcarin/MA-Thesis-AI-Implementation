
#table 11: collinearity statistics (VIF)
# Put every item into one big linear model
set.seed(123)  # for reproducibility
survey_data$dummy <- rnorm(nrow(survey_data))
#remove variables with loading < 0.6 from vif model, meaning PEoU2, T3, E2, E4, SN1
vif_model <- lm(
  dummy ~ ATU1 + ATU2 + ATU3 + ATU4 +
    AU1 + AU2 + AU3 + AU4 +
    PU1 + PU2 + PU3 + PU4 +
    PEoU1  + PEoU3 +
    SN1 +
    T1 + T2 + T4 +
    E1,
  data = survey_data
)

vif_values <- car::vif(vif_model)
vif_values

vif_df <- data.frame(
  Item = names(vif_values),
  VIF  = as.numeric(vif_values)
) %>%
  arrange(Item)
vif_df

half <- ceiling(nrow(vif_df) / 2)

left  <- vif_df[1:half, ]
right <- vif_df[(half+1):nrow(vif_df), ]
##add empty row to right if odd number of rows, to match left
if (nrow(left) > nrow(right)) {
  right <- bind_rows(right, data.frame(Item = NA, VIF = NA))
}
#replace "NA" with nothing
right[is.na(right)] <- ""
#make right numerical
right$VIF <- as.numeric(right$VIF)
vif_side_by_side <- bind_cols(left, right)
names(vif_side_by_side) <- c("Item1", "VIF1", "Item2", "VIF2")

table11 <- vif_side_by_side %>%
  gt() %>%
  cols_label(
    Item1 = "Items",
    VIF1  = "VIF",
    Item2 = "Items",
    VIF2  = "VIF"
  ) %>%
  fmt_number(columns = c(VIF1, VIF2), decimals = 3) %>%
  #replace NA with empty string
  sub_missing(everything(), missing_text = "") %>%
  tab_caption("Table 11. Collinearity statistics.") %>%
  tab_source_note(gt::md("Source: output RStudio using seminr package")) %>%
  tab_options(table.font.size = px(12))

table11
#save png
gtsave(
  table11,
  "Tables_Figures_Plots/Tabelle11_Collinearity_Statistics.png",
  vwidth  = 100000, 
  vheight = 4000
)