#install.packages(c("lubridate", "hms", "forcats", "crayon", "ggrepel", "gtsummary", "kableExtra", "webshot2"))
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
library(knitr)
library(kableExtra)
library(webshot2)
library(gtsummary)

survey_data <- readRDS("Survey_Data/survey_results_cleaned.rds")

#change columns 2 and 3 to datetime
time_cols <- c("Start.time", "Completion.time")

orders <- c("mdy HMS", "mdy HM",   # 09.18.25 12:33:04  / 09.18.25 12:33
            "mdY HMS", "mdY HM")   # 09.18.2025 12:33:04 / 09.18.2025 12:33

survey_data <- survey_data %>%
  mutate(across(all_of(time_cols),
                ~ with_tz(parse_date_time(.x, orders = orders, tz = "UTC"),
                          "Europe/Zurich")))

survey_data <- survey_data %>%
  mutate(fill_time = Completion.time - Start.time,
         fill_time_hms = as_hms(fill_time))  
#reorder columns to have 51 and 52 after 3
survey_data <- survey_data %>%
  select(1:3, fill_time, fill_time_hms, 4:50)

#round fill_time to minutes with two digits after the comma
survey_data <- survey_data %>%
  mutate(fill_time = round(as.numeric(fill_time, units = "mins"), 2))

#round fill_time_hms to format 07:30
survey_data <- survey_data %>%
  mutate(fill_time_hms = as_hms(round(as.numeric(fill_time_hms), digits = 0)))

#save rds
saveRDS(survey_data, "Survey_Data/survey_results_final.rds")

# Analyze the distribution of education levels
education_distribution <- survey_data %>%
  group_by(Education_Level) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

#sort the rows by percentage descending
education_distribution <- education_distribution %>%
  arrange(desc(Percentage)) %>%
  mutate(Education_Level = factor(Education_Level, levels = Education_Level))
print(education_distribution)

# Plot the distribution of education levels
#plot the labels of the education level labels inside the bars and rotated 90 degrees, the percentages at the bottom
# map each x tick (Education_Level) to a percentage string shown at the bottom
x_tick_labs <- setNames(paste0(round(education_distribution$Percentage, 1), "% (",
                               education_distribution$Count,")"), education_distribution$Education_Level)

# small constant vertical nudge (in y-units = percentage points)
nudge <- 0.5   # tweak to taste (e.g., 0.3–1.0)

ggplot(education_distribution, aes(x = Education_Level, y = Percentage)) +
  geom_col(aes(fill=factor(Education_Level))) +
  # put the education labels inside each bar near the bottom
  geom_text(aes(y = nudge, label = Education_Level),
            angle = 90, hjust = 0, vjust = 0.25) +
  # show the percentages as x-axis tick labels at the bottom
  scale_x_discrete(labels = x_tick_labs) +
  labs(title = "Distribution of Education Levels (n=220)",
       x = "Education", y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),legend.position="none")

ggsave("Tables_Figures_Plots/education_level_distribution.png", width = 8, height = 6)

#analyze the years of experience distribution
experience_distribution <- survey_data %>%
  group_by(Years_of_Experience) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)
#sort the rows by percentage descending
experience_distribution <- experience_distribution %>%
  arrange(desc(Percentage)) %>%
  mutate(Years_of_Experience = factor(Years_of_Experience, levels = Years_of_Experience))
print(experience_distribution)
# Plot the distribution of years of experience
# map each x tick (Years_of_Experience) to a percentage string shown at the bottom
x_tick_labs_exp <- setNames(paste0(round(experience_distribution$Percentage, 1), "% (",
                                   experience_distribution$Count,")"), experience_distribution$Years_of_Experience)
ggplot(experience_distribution, aes(x = Years_of_Experience, y = Percentage)) +
  geom_col(aes(fill=factor(Years_of_Experience))) +
  # put the experience labels inside each bar near the bottom
  geom_text(aes(y = nudge, label = Years_of_Experience),
            angle = 90, hjust = 0, vjust = 0.25) +
  # show the percentages as x-axis tick labels at the bottom
  scale_x_discrete(labels = x_tick_labs_exp) +
  labs(title = "Distribution of Years of Experience (n=220)",
       x = "Percentage", y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),legend.position="none")
ggsave("Tables_Figures_Plots/years_of_experience_distribution.png", width = 8, height = 6)

#analyze the gender distribution
gender_distribution <- survey_data %>%
  group_by(Gender) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)
#sort the rows by percentage descending
gender_distribution <- gender_distribution %>%
  arrange(desc(Percentage)) %>%
  mutate(Gender = factor(Gender, levels = Gender))
print(gender_distribution)
#plot the gender distribution as a pie chart
ggplot(gender_distribution, aes(x = "", y = Percentage, fill = Gender)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Distribution of Gender (n=220)") +
  theme_minimal()+
  theme(axis.text.x = element_blank(),axis.ticks = element_blank(), axis.title = element_blank(), 
        panel.grid = element_blank())+
  #put the percentages as text in the respective pie slices
  geom_text(aes(label = paste0(round(Percentage, 1), "% (", Count,")")),
            position = position_stack(vjust = 0.5))
ggsave("Tables_Figures_Plots/Gender_distribution_pie_chart.png", width = 8, height = 6)

#before I plot the fill times, I want to summarize them into groups, grouped by fill time.
#Group 1 contains fill times between 0 and 5 minutes, Group 2 between 5 and 10 minutes, Group 3 between 10 and 15 minutes,
#Group 4 between 15 and 30 minutes, Group 5 more than 30 minutes.
survey_data <- survey_data %>%
  mutate(fill_time_group = case_when(
    fill_time <= 5 ~ "0-5 minutes",
    fill_time > 5 & fill_time <= 10 ~ "5-10 minutes",
    fill_time > 10 & fill_time <= 15 ~ "10-15 minutes",
    fill_time > 15 & fill_time <= 30 ~ "15-30 minutes",
    fill_time > 30 ~ ">30 minutes"
  ))
#analyze the fill time group distribution
fill_time_group_distribution <- survey_data %>%
  group_by(fill_time_group) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)
#sort the rows by fill_time_group in the order defined above
fill_time_group_distribution <- fill_time_group_distribution %>%
  mutate(fill_time_group = factor(fill_time_group, levels = c("0-5 minutes", "5-10 minutes", "10-15 minutes",
                                                            "15-30 minutes", ">30 minutes"))) %>%
  arrange(fill_time_group)
print(fill_time_group_distribution)

# Plot the distribution of fill time groups
# map each x tick (fill_time_group) to a percentage string shown at the bottom
x_tick_labs_fill <- setNames(paste0(round(fill_time_group_distribution$Percentage, 1), "% (",
                                    fill_time_group_distribution$Count,")"), fill_time_group_distribution$fill_time_group)
ggplot(fill_time_group_distribution, aes(x = fill_time_group, y = Percentage)) +
  geom_col(aes(fill=factor(fill_time_group))) +
  # put the fill time group labels inside each bar near the bottom
  geom_text(aes(y = nudge, label = fill_time_group),
            angle = 90, hjust = 0, vjust = 0.25) +
  # show the percentages as x-axis tick labels at the bottom
  scale_x_discrete(labels = x_tick_labs_fill) +
  labs(title = "Distribution of Survey Fill Time Groups (n=220)",
       x = "Fill time", y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),legend.position="none")
ggsave("Tables_Figures_Plots/survey_fill_time_group_distribution.png", width = 8, height = 6)

#plot the distribution of Department
department_distribution <- survey_data %>%
  group_by(Department) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)
#sort the rows by percentage descending
department_distribution <- department_distribution %>%
  arrange(desc(Percentage)) %>%
  mutate(Department = factor(Department, levels = Department))
print(department_distribution)
# Plot the distribution of Department
# map each x tick (Department) to a percentage string shown at the bottom
x_tick_labs_dept <- setNames(paste0(round(department_distribution$Percentage, 1), "% (",
                                    department_distribution$Count,")"), department_distribution$Department)
ggplot(department_distribution, aes(x = Department, y = Percentage)) +
  geom_col(aes(fill=factor(Department))) +
  # put the Department labels inside each bar near the bottom
  geom_text(aes(y = nudge, label = Department),
            angle = 90, hjust = 0, vjust = 0.25) +
  # show the percentages as x-axis tick labels at the bottom
  scale_x_discrete(labels = x_tick_labs_dept) +
  labs(title = "Distribution of Departments (n=220)",
       x = "Department", y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),legend.position="none")
ggsave("Tables_Figures_Plots/department_distribution.png", width = 8, height = 6)
#show department distribution in a pie chart
ggplot(department_distribution, aes(x = "", y = Percentage, fill = Department)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Distribution of Departments (n=220)") +
  theme_minimal()+
  theme(axis.text.x = element_blank(),axis.ticks = element_blank(), axis.title = element_blank(), 
        panel.grid = element_blank())+
  #put the percentages as text in the respective pie slices
  geom_text(aes(label = paste0(round(Percentage, 1), "% (", Count,")")),
            position = position_stack(vjust = 0.55))
ggsave("Tables_Figures_Plots/department_distribution_pie_chart.png", width = 8, height = 6)

#plot distribution of age
age_distribution <- survey_data %>%
  group_by(Age) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)
#sort by age
age_distribution <- age_distribution %>%
  arrange(Age) %>%
  mutate(Age = factor(Age, levels = Age))
print(age_distribution)
# Plot the distribution of Age
# map each x tick (Age) to a percentage string shown at the bottom
x_tick_labs_age <- setNames(paste0(round(age_distribution$Percentage, 1), "% (",
                                   age_distribution$Count,")"), age_distribution$Age)
ggplot(age_distribution, aes(x = Age, y = Percentage)) +
  geom_col(aes(fill=factor(Age))) +
  # put the Age labels inside each bar near the bottom
  geom_text(aes(y = nudge, label = Age),
            angle = 90, hjust = 0, vjust = 0.25) +
  # show the percentages as x-axis tick labels at the bottom
  scale_x_discrete(labels = x_tick_labs_age) +
  labs(title = "Distribution of Age (n=220)",
       x = "Age", y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),legend.position="none")
ggsave("Tables_Figures_Plots/age_distribution.png", width = 8, height = 6)

#analyze the division distribution and remove the empty or NA ones
division_distribution <- survey_data %>%
  filter(!is.na(Division) & Division != "") %>%
  group_by(Division) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)
#sort the rows by percentage descending
division_distribution <- division_distribution %>%
  arrange(desc(Percentage)) %>%
  mutate(Division = factor(Division, levels = Division))
print(division_distribution)
# Plot the distribution of Division
# map each x tick (Division) to a percentage string shown at the bottom
x_tick_labs_div <- setNames(paste0(round(division_distribution$Percentage, 1), "% (",
                                   division_distribution$Count,")"), division_distribution$Division)
ggplot(division_distribution, aes(x = Division, y = Percentage)) +
  geom_col(aes(fill=factor(Division))) +
  # put the Division labels inside each bar near the bottom
  geom_text(aes(y = nudge, label = Division),
            angle = 90, hjust = 0, vjust = 0.25) +
  # show the percentages as x-axis tick labels at the bottom
  scale_x_discrete(labels = x_tick_labs_div) +
  labs(title = "Distribution of Divisions (n=135)",
       x = "Division", y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),legend.position="none")
ggsave("Tables_Figures_Plots/division_distribution.png", width = 8, height = 6)
#show division distribution in a pie chart
ggplot(division_distribution, aes(x = "", y = Percentage, fill = Division)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Distribution of Divisions (n=135)") +
  theme_minimal()+
  theme(axis.text.x = element_blank(),axis.ticks = element_blank(), axis.title = element_blank(), 
        panel.grid = element_blank())+
  #put the percentages as text in the respective pie slices and offset the text position for the Grid slice so it's readable
  geom_text(aes(label = paste0(round(Percentage, 1), "% (", Count,")")),
            position = position_stack(vjust = 0.5))
ggsave("Tables_Figures_Plots/division_distribution_pie_chart.png", width = 8, height = 6)
# Improved pie chart for Division distribution with better label placement for small percentage slices
#only do the repel labels for the small percentages below 5 percent
ggplot(division_distribution, aes(x = "", y = Percentage, fill = Division)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Distribution of Divisions (n=135)") +
  theme_minimal()+
  theme(axis.text.x = element_blank(),axis.ticks = element_blank(), axis.title = element_blank(), 
        panel.grid = element_blank())+
  geom_text(data = subset(division_distribution, Percentage >= 5),
            aes(label = paste0(round(Percentage, 1), "% (", Count,")")),
            position = position_stack(vjust = 0.85)) +
  geom_label_repel(data = subset(division_distribution, Percentage < 5),
                   aes(label = paste0(round(Percentage, 1), "% (", Count,")")),
                   nudge_x = 0.55,
                   nudge_y = -0.14,
                   show.legend = FALSE,
                   segment.color = 'grey50',
                   segment.size = 0.3)

ggsave("Tables_Figures_Plots/division_distribution_pie_chart_improved.png", width = 8, height = 6)

#analyze employment type
employment_type_distribution <- survey_data %>%
  group_by(Employment_Type) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)
#sort the rows by percentage descending
employment_type_distribution <- employment_type_distribution %>%
  arrange(desc(Percentage)) %>%
  mutate(Employment_Type = factor(Employment_Type, levels = Employment_Type))
print(employment_type_distribution)
#plot employment type distribution as a pie chart
ggplot(employment_type_distribution, aes(x = "", y = Percentage, fill = Employment_Type)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Distribution of Employment Types (n=220)") +
  theme_minimal()+
  theme(axis.text.x = element_blank(),axis.ticks = element_blank(), axis.title = element_blank(), 
        panel.grid = element_blank())+
  #put the percentages as text in the respective pie slices
  geom_text(aes(label = paste0(round(Percentage, 1), "% (", Count,")")),
            position = position_stack(vjust = 0.5))
ggsave("Tables_Figures_Plots/employment_type_distribution_pie_chart.png", width = 8, height = 6)

#analyze work type
work_type_distribution <- survey_data %>%
  group_by(Work_Type) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)
#sort the rows by percentage descending
work_type_distribution <- work_type_distribution %>%
  arrange(desc(Percentage)) %>%
  mutate(Work_Type = factor(Work_Type, levels = Work_Type))
print(work_type_distribution)
#plot work type distribution as a pie chart
ggplot(work_type_distribution, aes(x = "", y = Percentage, fill = Work_Type)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Distribution of Work Types (n=220)") +
  theme_minimal()+
  theme(axis.text.x = element_blank(),axis.ticks = element_blank(), axis.title = element_blank(), 
        panel.grid = element_blank())+
  #put the percentages as text in the respective pie slices
  geom_text(aes(label = paste0(round(Percentage, 1), "% (", Count,")")),
            position = position_stack(vjust = 0.65))
ggsave("Tables_Figures_Plots/work_type_distribution_pie_chart.png", width = 8, height = 6)

#analyze work location
work_location_distribution <- survey_data %>%
  group_by(Work_Location) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)
#sort the rows by percentage descending
work_location_distribution <- work_location_distribution %>%
  arrange(desc(Percentage)) %>%
  mutate(Work_Location = factor(Work_Location, levels = Work_Location))
print(work_location_distribution)
#plot work location distribution as a pie chart
ggplot(work_location_distribution, aes(x = "", y = Percentage, fill = Work_Location)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Distribution of Work Locations (n=220)") +
  theme_minimal()+
  theme(axis.text.x = element_blank(),axis.ticks = element_blank(), axis.title = element_blank(), 
        panel.grid = element_blank())+
  #put the percentages as text in the respective pie slices
  geom_text(aes(label = paste0(round(Percentage, 1), "% (", Count,")")),
            position = position_stack(vjust = 0.5))
ggsave("Tables_Figures_Plots/work_location_distribution_pie_chart.png", width = 8, height = 6)

#analyze Team_abbr
team_abbr_distribution <- survey_data %>%
  filter(!is.na(Team_Abbr) & Team_Abbr != "") %>%
  group_by(Team_Abbr) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)
#sort the rows by percentage descending
team_abbr_distribution <- team_abbr_distribution %>%
  arrange(desc(Percentage)) %>%
  mutate(Team_Abbr = factor(Team_Abbr, levels = Team_Abbr))
print(team_abbr_distribution)
#only keep teams with at least 3 represenations
team_abbr_distribution <- team_abbr_distribution %>%
  filter(Count >= 3)
print(team_abbr_distribution)
# Plot the distribution of Team_Abbr
# map each x tick (Team_Abbr) to a percentage string shown at the bottom
x_tick_labs_team <- setNames(paste0(round(team_abbr_distribution$Percentage, 1), "% (",
                                   team_abbr_distribution$Count,")"), team_abbr_distribution$Team_Abbr)
ggplot(team_abbr_distribution, aes(x = Team_Abbr, y = Percentage)) +
  geom_col(aes(fill=factor(Team_Abbr))) +
  # put the Team_Abbr labels inside each bar near the bottom
  geom_text(aes(y = nudge, label = Team_Abbr),
            angle = 90, hjust = 0, vjust = 0.25) +
  # show the percentages as x-axis tick labels at the bottom
  scale_x_discrete(labels = x_tick_labs_team) +
  labs(title = "Distribution of Teams (n=54)",
       x = "Team", y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),legend.position="none")

#analyze Team_Abbr3
team_abbr3_distribution <- survey_data %>%
  filter(!is.na(Team_Abbr3) & Team_Abbr3 != "") %>%
  group_by(Team_Abbr3) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)
#sort the rows by percentage descending
team_abbr3_distribution <- team_abbr3_distribution %>%
  arrange(desc(Percentage)) %>%
  mutate(Team_Abbr3 = factor(Team_Abbr3, levels = Team_Abbr3))
print(team_abbr3_distribution)
#only keep teams with at least 5 representations
team_abbr3_distribution <- team_abbr3_distribution %>%
  filter(Count >= 5)
print(team_abbr3_distribution)
# Plot the distribution of Team_Abbr
# map each x tick (Team_Abbr) to a percentage string shown at the bottom
x_tick_labs_team3 <- setNames(paste0(round(team_abbr3_distribution$Percentage, 1), "% (",
                                     team_abbr3_distribution$Count,")"), team_abbr3_distribution$Team_Abbr3)
ggplot(team_abbr3_distribution, aes(x = Team_Abbr3, y = Percentage)) +
  geom_col(aes(fill=factor(Team_Abbr3))) +
  # put the Team_Abbr labels inside each bar near the bottom
  geom_text(aes(y = nudge, label = Team_Abbr3),
            angle = 90, hjust = 0, vjust = 0.25) +
  # show the percentages as x-axis tick labels at the bottom
  scale_x_discrete(labels = x_tick_labs_team3) +
  labs(title = "Distribution of Teams (n=71)",
       x = "Team", y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),legend.position="none")
ggsave("Tables_Figures_Plots/team_abbr3_distribution.png", width = 8, height = 6)

#analyze Team_Abbr2
team_abbr2_distribution <- survey_data %>%
  filter(!is.na(Team_Abbr2) & Team_Abbr2 != "") %>%
  group_by(Team_Abbr2) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)
#sort the rows by percentage descending
team_abbr2_distribution <- team_abbr2_distribution %>%
  arrange(desc(Percentage)) %>%
  mutate(Team_Abbr2 = factor(Team_Abbr2, levels = Team_Abbr2))
print(team_abbr2_distribution)
#only keep teams with at least 5 representations
team_abbr2_distribution <- team_abbr2_distribution %>%
  filter(Count >= 5)
print(team_abbr2_distribution)
# Plot the distribution of Team_Abbr
# map each x tick (Team_Abbr) to a percentage string shown at the bottom
x_tick_labs_team2 <- setNames(paste0(round(team_abbr2_distribution$Percentage, 1), "% (",
                                     team_abbr2_distribution$Count,")"), team_abbr2_distribution$Team_Abbr2)
ggplot(team_abbr2_distribution, aes(x = Team_Abbr2, y = Percentage)) +
  geom_col(aes(fill=factor(Team_Abbr2))) +
  # put the Team_Abbr labels inside each bar near the bottom
  geom_text(aes(y = nudge, label = Team_Abbr2),
            angle = 90, hjust = 0, vjust = 0.25) +
  # show the percentages as x-axis tick labels at the bottom
  scale_x_discrete(labels = x_tick_labs_team2) +
  labs(title = "Distribution of Teams (n=71)",
       x = "Team", y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),legend.position="none")
ggsave("Tables_Figures_Plots/team_abbr2_distribution.png", width = 8, height = 6)

#analyze Team_Abbr1
team_abbr1_distribution <- survey_data %>%
  filter(!is.na(Team_Abbr1) & Team_Abbr1 != "") %>%
  group_by(Team_Abbr1) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)
#sort the rows by percentage descending
team_abbr1_distribution <- team_abbr1_distribution %>%
  arrange(desc(Percentage)) %>%
  mutate(Team_Abbr1 = factor(Team_Abbr1, levels = Team_Abbr1))
print(team_abbr1_distribution)
#only keep teams with at least 5 representations
team_abbr1_distribution <- team_abbr1_distribution %>%
  filter(Count >= 5)
print(team_abbr1_distribution)
# Plot the distribution of Team_Abbr
# map each x tick (Team_Abbr) to a percentage string shown at the bottom
x_tick_labs_team1 <- setNames(paste0(round(team_abbr1_distribution$Percentage, 1), "% (",
                                     team_abbr1_distribution$Count,")"), team_abbr1_distribution$Team_Abbr1)
ggplot(team_abbr1_distribution, aes(x = Team_Abbr1, y = Percentage)) +
  geom_col(aes(fill=factor(Team_Abbr1))) +
  # put the Team_Abbr labels inside each bar near the bottom
  geom_text(aes(y = nudge, label = Team_Abbr1),
            angle = 90, hjust = 0, vjust = 0.25) +
  # show the percentages as x-axis tick labels at the bottom
  scale_x_discrete(labels = x_tick_labs_team1) +
  labs(title = "Distribution of Teams (n=71)",
       x = "Team", y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),legend.position="none")
ggsave("Tables_Figures_Plots/team_abbr1_distribution.png", width = 8, height = 6)

##create table with demographic infos, with two columns, one for N and one for percent
survey_data <- survey_data %>%
  mutate(
    Age = factor(
      Age,
      levels = c("<20 Jahre", "20-30 Jahre", "30-40 Jahre", "40-50 Jahre", "50-60 Jahre", ">60 Jahre"),
      ordered = TRUE
    )
  )
tbl1<-tbl_summary(
  data = survey_data,
  include = c(Gender, Age),            
  type = all_categorical() ~ "categorical",
  statistic = all_categorical() ~ "{n} ({p}%)",
  missing = "no"
) |>
  modify_caption("**Table 1. Demographic Respondents' data.**") |>
  bold_labels()|>
  as_gt() |>
  gt::tab_source_note(gt::md("Source: Primary data collected from survey."))
tbl1
#save png
tbl1 %>%
  gt::gtsave("Tables_Figures_Plots/demographic_table1.png")

#same but for Education_Level and Years_of_Experience
survey_data <- survey_data %>%
  mutate(
    Years_of_Experience = factor(
      Years_of_Experience,
      levels = c("<1 Jahr", "1-3 Jahre", "3-5 Jahre", "5-10 Jahre", "10-20 Jahre", ">20 Jahre"),
      ordered = TRUE
    )
  )
tbl2<-tbl_summary(
  data = survey_data,
  include = c(Education_Level, Years_of_Experience),            
  type = all_categorical() ~ "categorical",
  statistic = all_categorical() ~ "{n} ({p}%)",
  missing = "no"
) |>
  modify_caption("**Table 2. Demographic Respondents' data.**") |>
  bold_labels()|>
  as_gt() |>
  gt::tab_source_note(gt::md("Source: Primary data collected from survey."))
tbl2
#save png
tbl2 %>%
  gt::gtsave("Tables_Figures_Plots/demographic_table2.png")

#same but for Work_Location, Work_type and Employment_Type
tbl3<-tbl_summary(
  data = survey_data,
  include = c(Work_Location, Work_Type, Employment_Type),            
  type = all_categorical() ~ "categorical",
  statistic = all_categorical() ~ "{n} ({p}%)",
  missing = "no"
) |>
  modify_caption("**Table 3. Demographic Respondents' data.**") |>
  bold_labels()|>
  as_gt() |>
  gt::tab_source_note(gt::md("Source: Primary data collected from survey."))
tbl3
#save png
tbl3 %>%
  gt::gtsave("Tables_Figures_Plots/demographic_table3.png")

#same but for department and division. Since division is a subgroup of only those that have Department
##Generation&Distribution, show Division as a subgroup of Department Generation&Distribution
tbl_dept <-
  tbl_summary(
    data = survey_data,
    include = Department,
    statistic = all_categorical() ~ "{n} ({p}%)",
    missing = "no",
    label = list(Department ~ "Department")
  ) %>% 
  add_n()
tbl_div <-
  survey_data %>%
  filter(Department == "Generation & Distribution") %>%
  tbl_summary(
    include = Division,
    statistic = all_categorical() ~ "{n} ({p}%)",
    missing = "no",
    # this becomes the small section header above the levels
    label = list(Division ~ "Division (within Generation & Distribution)")
  ) %>%
  add_n() %>%
  # indent the division levels so they appear "under" the department
  modify_table_body(~ .x %>%
                      mutate(label = ifelse(variable == "Division" & row_type == "level",
                                            paste0("   ", label), label))
  )
tbl4 <-
  tbl_stack(list(tbl_dept, tbl_div)) %>%
  modify_caption("**Table 4. Demographic Respondents' data.**") %>%
  bold_labels() %>%
  as_gt() %>%
  gt::tab_source_note(gt::md("Source: Primary data collected from survey."))

tbl4
#save png
tbl4 %>%
  gt::gtsave("Tables_Figures_Plots/demographic_table4.png")

#plot the private ai use
private_ai_usage_distribution <- survey_data %>%
  group_by(Private_AI_Use) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)
#sort the rows by percentage descending
private_ai_usage_distribution <- private_ai_usage_distribution %>%
  arrange(desc(Percentage)) %>%
  mutate(Private_AI_Use = factor(Private_AI_Use, levels = Private_AI_Use))
print(private_ai_usage_distribution)
# Plot the distribution of Private_AI_Use as a pie chart
ggplot(private_ai_usage_distribution, aes(x = "", y = Percentage, fill = Private_AI_Use)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Distribution of Private AI Usage (n=220)") +
  theme_minimal()+
  theme(axis.text.x = element_blank(),axis.ticks = element_blank(), axis.title = element_blank(), 
        panel.grid = element_blank())+
  #put the percentages as text in the respective pie slices
  geom_text(aes(label = paste0(round(Percentage, 1), "% (", Count,")")),
            position = position_stack(vjust = 0.55))
ggsave("Tables_Figures_Plots/private_ai_usage_distribution_pie_chart.png", width = 8, height = 6)


#unnest the Private_AI_Tools column and analyze the distribution
private_ai_tools_used <- survey_data %>%
  select(Private_AI_Tools) %>%
  filter(!is.na(Private_AI_Tools), Private_AI_Tools != "") %>%
  
  # distinguish single vs multi
  mutate(
    cleaned = case_when(
      # multi-select format: c("A", "B")
      str_detect(Private_AI_Tools, "^c\\(") ~
        # remove leading c( and trailing )
        str_replace_all(Private_AI_Tools, '^c\\(|\\)$', "") %>% 
        # remove escaped quotes
        str_replace_all('\\"', "") %>%
        str_replace_all('"', ""),
      
      # single-select: keep as-is but remove quotes
      TRUE ~ str_replace_all(Private_AI_Tools, '"', "")
    )
  ) %>%
  
  # split into multiple rows (multi-select values contain comma)
  separate_rows(cleaned, sep = ",\\s*") %>%
  
  mutate(cleaned = str_trim(cleaned)) %>%  # trim whitespace
  rename(Tool = cleaned)


#clean private ai tools
private_ai_tools_used$Tool <- as.character(private_ai_tools_used$Tool)
private_ai_tools_used$Tool<-gsub("Axpo ChatGPT","AxpoGPT",private_ai_tools_used$Tool, fixed = TRUE)
private_ai_tools_used$Tool<-gsub("AXPO ChatGPT","AxpoGPT",private_ai_tools_used$Tool, fixed = TRUE)
private_ai_tools_used$Tool<-gsub("Axpo GPT und LISE","AxpoGPT",private_ai_tools_used$Tool, fixed = TRUE)
private_ai_tools_used$Tool<-gsub("axpogpt","AxpoGPT",private_ai_tools_used$Tool, fixed = TRUE)
private_ai_tools_used$Tool<-gsub("Deepseek","DeepSeek",private_ai_tools_used$Tool, fixed = TRUE)
private_ai_tools_used$Tool<-gsub("Github Copilot","GitHub Copilot",private_ai_tools_used$Tool, fixed = TRUE)
private_ai_tools_used$Tool<-gsub("Lise ch","LISE",private_ai_tools_used$Tool, fixed = TRUE)
private_ai_tools_used$Tool<-gsub("Lumo (Proton)","Lumo",private_ai_tools_used$Tool, fixed = TRUE)
private_ai_tools_used$Tool<-gsub("Perplexity (zahlversion)","Perplexity",private_ai_tools_used$Tool, fixed = TRUE)
private_ai_tools_used$Tool<-gsub("Perplexity Pro","Perplexity",private_ai_tools_used$Tool, fixed = TRUE)
private_ai_tools_used$Tool<-gsub("Perplexity.ai und DeepL","Perplexity",private_ai_tools_used$Tool, fixed = TRUE)
private_ai_tools_used$Tool<-gsub("Local installed LLM e.g. Lama","Lama",private_ai_tools_used$Tool, fixed = TRUE)
private_ai_tools_used$Tool<-gsub("Meta AI (KI-Assistent bei Facebook und Whatsapp)","Meta AI",private_ai_tools_used$Tool, fixed = TRUE)
private_ai_tools_used$Tool<-gsub("NotebookLLM (Mindmap-Tool von Google)","NotebookLLM",private_ai_tools_used$Tool, fixed = TRUE)
#filter out single responses
private_ai_tools_used <- private_ai_tools_used %>%
  filter(!Tool %in% c("und viele andere die es noch gibt.","JetBrains AI Assistant","KI Musik-Tools","Lama","LeChat", 
                      "weil keine Kosten","Suno"))
  
private_ai_tools_summary <- private_ai_tools_used %>%
  group_by(Tool) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percentage = round(Count / sum(Count) * 100, 1)) %>%
  arrange(desc(Count))

private_ai_tools_summary
  
#gt table ai tools summary
tbl_private_ai_tools <- private_ai_tools_used %>%
  select(Tool) %>%
  tbl_summary(
    include   = Tool,
    type      = all_categorical() ~ "categorical",
    statistic = all_categorical() ~ "{n} ({p}%)",
    missing   = "no",
    label     = list(Tool ~ "Tool"),
    sort      = list(all_categorical() ~ "frequency")  
  ) %>%
  modify_caption("**Table 5. Private AI Tools Used by Respondents.**") %>%
  bold_labels() %>%
  as_gt()|>
  gt::tab_source_note(gt::md("Source: Primary data collected from survey."))

tbl_private_ai_tools
#save png
tbl_private_ai_tools %>%
  gt::gtsave("Tables_Figures_Plots/private_ai_tools_table.png")

#gtable for Private_AI_Frequency column from survey_data without the empty answer, 20 did say they do not use AI privately, so N should be 200
survey_data[,22]<-gsub("habe das im Rahmen meiner Masterarbeit benutzt. Seitdem nicht mehr","seltener",survey_data[,22])
freq_list <- c(
  "Täglich",
  "2-3 Mal pro Woche",
  "Einmal pro Woche",
  "2-3 Mal pro Monat",
  "Einmal pro Monat",
  "seltener"
)

survey_data <- survey_data %>% 
  mutate(
    Private_AI_Frequency = factor(
      Private_AI_Frequency,
      levels  = freq_list,
      ordered = TRUE
    )
  )

tbl_priv_freq <- tbl_summary(
  data = survey_data %>% filter(Private_AI_Use == "Ja"),
  include   = Private_AI_Frequency,
  type      = all_categorical() ~ "categorical",
  statistic = all_categorical() ~ "{n} ({p}%)",
  
  sort      = list(all_categorical() ~ "alphanumeric"),
  missing   = "no"
) |>
  modify_caption("**Table 6. Frequency of Private AI Use by Respondents.**") |>
  bold_labels() |>
  as_gt() |>
  gt::tab_source_note(gt::md("Source: Primary data collected from survey."))

tbl_priv_freq
#save png
tbl_priv_freq %>%
  gt::gtsave("Tables_Figures_Plots/private_ai_frequency_table.png")

#unnest the Work_AI_Tools column and analyze the distribution
work_ai_tools_used <- survey_data %>%
  select(Work_AI_Tools) %>%
  filter(!is.na(Work_AI_Tools), Work_AI_Tools != "") %>%
  
  # distinguish single vs multi
  mutate(
    cleaned = case_when(
      # multi-select format: c("A", "B")
      str_detect(Work_AI_Tools, "^c\\(") ~
        # remove leading c( and trailing )
        str_replace_all(Work_AI_Tools, '^c\\(|\\)$', "") %>% 
        # remove escaped quotes
        str_replace_all('\\"', "") %>%
        str_replace_all('"', ""),
      
      # single-select: keep as-is but remove quotes
      TRUE ~ str_replace_all(Work_AI_Tools, '"', "")
    )
  ) %>%
  # split into multiple rows (multi-select values contain comma)
  separate_rows(cleaned, sep = ",\\s*") %>%
  
  mutate(cleaned = str_trim(cleaned)) %>%  # trim whitespace
  rename(Tool = cleaned)

work_ai_tools_summary <- work_ai_tools_used %>%
  group_by(Tool) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)
#sort the rows by percentage descending
work_ai_tools_summary <- work_ai_tools_summary %>%
  arrange(desc(Percentage)) %>% 
  mutate(Tool = factor(Tool, levels = Tool))
print(work_ai_tools_summary)

#clean work ai tools
work_ai_tools_used$Tool<-gsub("Github Copilot","GitHub Copilot",work_ai_tools_used$Tool, fixed = TRUE)
work_ai_tools_used$Tool<-gsub("Perplexity Pro","Perplexity",work_ai_tools_used$Tool, fixed = TRUE)
work_ai_tools_used$Tool<-gsub("ChatGPT gratis","ChatGPT (gratis-Version)",work_ai_tools_used$Tool, fixed = TRUE)
work_ai_tools_used$Tool<-gsub("Lise (KI-Tool der Division Nuklear)","Lise",work_ai_tools_used$Tool, fixed = TRUE)
work_ai_tools_used$Tool<-gsub("NotebookLLM (Mindmap-Tool von Google)","NotebookLLM",work_ai_tools_used$Tool, fixed = TRUE)
#filter out single responses
work_ai_tools_used <- work_ai_tools_used %>%
  filter(!Tool %in% c("und alles Google related ist strengstens verboten bei der Axpo für die arbeit",
                      "JetBrains AI Assistant","Langdock"))

work_ai_tools_summary <- work_ai_tools_used %>%
  group_by(Tool) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

work_ai_tools_summary <- work_ai_tools_summary %>%
  arrange(desc(Percentage)) %>% 
  mutate(Tool = factor(Tool, levels = Tool))
print(work_ai_tools_summary)

#gt table work ai tools summary
tbl_work_ai_tools<-tbl_summary(
  data = work_ai_tools_used,
  include = Tool,            
  type = all_categorical() ~ "categorical",
  statistic = all_categorical() ~ "{n} ({p}%)",
  missing = "no",
  label = list(Tool ~ "Tool"),
  sort = list(all_categorical() ~ "frequency")  
) %>%
  modify_caption("**Table 7. Work AI Tools Used by Respondents.**") %>%
  bold_labels() %>%
  as_gt() |>
  gt::tab_source_note(gt::md("Source: Primary data collected from survey."))
tbl_work_ai_tools
#save png
tbl_work_ai_tools %>%
  gt::gtsave("Tables_Figures_Plots/work_ai_tools_table.png")

#gtable for Work_AI_Frequency column from survey_data without the empty answer
freq_list_work <- c(
  "Täglich",
  "2-3 Mal pro Woche",
  "Einmal pro Woche",
  "2-3 Mal pro Monat",
  "Einmal pro Monat",
  "seltener"
)
survey_data <- survey_data %>%
  mutate(
    Work_AI_Frequency = factor(
      Work_AI_Frequency,
      levels  = freq_list_work,
      ordered = TRUE
    )
  )
tbl_work_freq <- tbl_summary(
  data = survey_data %>% filter(Work_AI_Use == "Ja"),
  include   = Work_AI_Frequency,
  type      = all_categorical() ~ "categorical",
  statistic = all_categorical() ~ "{n} ({p}%)",
  
  sort      = list(all_categorical() ~ "alphanumeric"),
  missing   = "no"
) |>
  modify_caption("**Table 8. Frequency of Work AI Use by Respondents.**") |>
  bold_labels() |>
  as_gt() |>
  gt::tab_source_note(gt::md("Source: Primary data collected from survey."))
tbl_work_freq
#save png
tbl_work_freq %>%
  gt::gtsave("Tables_Figures_Plots/work_ai_frequency_table.png")

#unnest Work_AI_Purposes
work_ai_purposes <- survey_data %>%
  select(Work_AI_Purposes) %>%
  filter(!is.na(Work_AI_Purposes), Work_AI_Purposes != "") %>%
  
  # distinguish single vs multi
  mutate(
    cleaned = case_when(
      # multi-select format: c("A", "B")
      str_detect(Work_AI_Purposes, "^c\\(") ~
        # remove leading c( and trailing )
        str_replace_all(Work_AI_Purposes, '^c\\(|\\)$', "") %>% 
        # remove escaped quotes
        str_replace_all('\\"', "") %>%
        str_replace_all('"', ""),
      
      # single-select: keep as-is but remove quotes
      TRUE ~ str_replace_all(Work_AI_Purposes, '"', "")
    )
  ) %>%
  
  # split into multiple rows (multi-select values contain comma)
  separate_rows(cleaned, sep = ",\\s*") %>%
  
  mutate(cleaned = str_trim(cleaned)) %>%  # trim whitespace
  rename(Purpose = cleaned)

#clean work ai purposes
work_ai_purposes$Purpose<-gsub("Code","Coding",work_ai_purposes$Purpose, fixed = TRUE)
work_ai_purposes$Purpose<-gsub("Coding completion und Cod-Generierung","Coding",work_ai_purposes$Purpose, fixed = TRUE)
work_ai_purposes$Purpose<-gsub("Coding Generierung","Coding",work_ai_purposes$Purpose, fixed = TRUE)
work_ai_purposes$Purpose<-gsub("Coding Makros","Coding",work_ai_purposes$Purpose, fixed = TRUE)
work_ai_purposes$Purpose<-gsub("visuelles Coding","Coding",work_ai_purposes$Purpose, fixed = TRUE)
work_ai_purposes$Purpose<-gsub("Code completion und Cod-Generierung","Coding",work_ai_purposes$Purpose, fixed = TRUE)
work_ai_purposes$Purpose<-gsub("Code Generierung","Coding",work_ai_purposes$Purpose, fixed = TRUE)
work_ai_purposes$Purpose<-gsub("Coding (Javascript","Coding",work_ai_purposes$Purpose, fixed = TRUE)
work_ai_purposes$Purpose<-gsub("coding improvement","Coding",work_ai_purposes$Purpose, fixed = TRUE)
work_ai_purposes$Purpose<-gsub("Coding über github copliot","Coding",work_ai_purposes$Purpose, fixed = TRUE)
work_ai_purposes$Purpose<-gsub("Coding/ Debugging","Coding",work_ai_purposes$Purpose, fixed = TRUE)
work_ai_purposes$Purpose<-gsub("codinghilfe","Coding",work_ai_purposes$Purpose, fixed = TRUE)
work_ai_purposes$Purpose<-gsub("DAX)","Coding",work_ai_purposes$Purpose, fixed = TRUE)
work_ai_purposes$Purpose<-gsub("etc.)","Coding",work_ai_purposes$Purpose, fixed = TRUE)
work_ai_purposes$Purpose<-gsub("Programmieren","Coding",work_ai_purposes$Purpose, fixed = TRUE)
work_ai_purposes$Purpose<-gsub("Programmierung","Coding",work_ai_purposes$Purpose, fixed = TRUE)
work_ai_purposes$Purpose<-gsub("Programmierung Makros","Coding",work_ai_purposes$Purpose, fixed = TRUE)
work_ai_purposes$Purpose<-gsub("Pwerquery","Coding",work_ai_purposes$Purpose, fixed = TRUE)
work_ai_purposes$Purpose<-gsub("Quick Coding","Coding",work_ai_purposes$Purpose, fixed = TRUE)
work_ai_purposes$Purpose<-gsub("scripting","Coding",work_ai_purposes$Purpose, fixed = TRUE)
work_ai_purposes$Purpose<-gsub("Scripts für PowerBI / Komplexe Excel Formeln","Coding",work_ai_purposes$Purpose, fixed = TRUE)
work_ai_purposes$Purpose<-gsub("visuelles Programmieren","Coding",work_ai_purposes$Purpose, fixed = TRUE)
work_ai_purposes$Purpose<-gsub("Zusammenfassen von Verträgen","Paraphrasieren eines Textes",work_ai_purposes$Purpose, fixed = TRUE)
work_ai_purposes$Purpose<-gsub("Zusammenfassung","Paraphrasieren eines Textes",work_ai_purposes$Purpose, fixed = TRUE)
work_ai_purposes$Purpose<-gsub("Zusammenfassungen","Paraphrasieren eines Textes",work_ai_purposes$Purpose, fixed = TRUE)
work_ai_purposes$Purpose<-gsub("Zusammenfassungen Protokolle","Paraphrasieren eines Textes",work_ai_purposes$Purpose, fixed = TRUE)
work_ai_purposes$Purpose<-gsub("messages","Paraphrasieren eines Textes",work_ai_purposes$Purpose, fixed = TRUE)
work_ai_purposes$Purpose<-gsub("Paraphrasieren eines Textesen Protokolle","Paraphrasieren eines Textes",work_ai_purposes$Purpose, fixed = TRUE)
work_ai_purposes$Purpose<-gsub("Paraphrasieren eines Textesen","Paraphrasieren eines Textes",work_ai_purposes$Purpose, fixed = TRUE)
work_ai_purposes$Purpose<-gsub("Datenaufbereitung","Datenanalyse",work_ai_purposes$Purpose, fixed = TRUE)
work_ai_purposes$Purpose<-gsub("challenge me and simplify things (processes","Hilfesuche/Ratschläge",work_ai_purposes$Purpose, fixed = TRUE)
work_ai_purposes$Purpose<-gsub("Textanalyse","Textverbesserungen",work_ai_purposes$Purpose, fixed = TRUE)
work_ai_purposes$Purpose<-gsub("Strukturierung von Wissen","Hilfesuche/Ratschläge",work_ai_purposes$Purpose, fixed = TRUE)
work_ai_purposes$Purpose<-gsub("Generieren eines Bildes anhand einer Beschreibung","Generieren eines Bildes",work_ai_purposes$Purpose, fixed = TRUE)
work_ai_purposes$Purpose<-gsub("Coding","Programmieren",work_ai_purposes$Purpose, fixed = TRUE)
#filter out single responses
work_ai_purposes<-work_ai_purposes %>%
  filter(!Purpose %in% c("nichts von oben","Excel- und Wordvorlagen erstellen","Literatur-Suche","Terminplanung","presentations","oberflächliche Branchen-/Marktanalysen","Monatliche Zeitaufschreibing und Notizen für mein monatliches Bila mit Cheg anhand Tagebucheinträgen erstellen","pc probleme"))

work_ai_purposes_summary <- work_ai_purposes %>%
  group_by(Purpose) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)
#sort the rows by percentage descending
work_ai_purposes_summary <- work_ai_purposes_summary %>%
  arrange(desc(Percentage)) %>% 
  mutate(Purpose = factor(Purpose, levels = Purpose))
print(work_ai_purposes_summary)
#gt table work ai purposes summary
tbl_work_ai_purposes<-tbl_summary(
  data = work_ai_purposes,
  include = Purpose,            
  type = all_categorical() ~ "categorical",
  statistic = all_categorical() ~ "{n} ({p}%)",
  missing = "no",
  label = list(Purpose ~ "Purpose"),
  sort = list(all_categorical() ~ "frequency")  
) %>%
  modify_caption("**Table 9. Work AI Purposes by Respondents.**") %>%
  bold_labels() %>%
  as_gt() |>
  gt::tab_source_note(gt::md("Source: Primary data collected from survey."))
tbl_work_ai_purposes
#save png
tbl_work_ai_purposes %>%
  gt::gtsave("Tables_Figures_Plots/work_ai_purposes_table.png")

#save rds
saveRDS(survey_data, file = "Survey_Data/survey_data_final.rds")
