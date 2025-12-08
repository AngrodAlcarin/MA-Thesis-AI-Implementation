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

