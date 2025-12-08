##install.packages(c("dplyr", "tidyr", "stringr", "ggplot2"))
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(magrittr)

#load survey data
survey_data <- read.csv("Survey_Data/survey_results_precleaned.csv", header=TRUE, fileEncoding="utf-8",sep = ";")

#remove 4t, 5th and 7th columns
survey_data <- survey_data[ , -c(4,5,7)]
#in the 4th column, replace "English (United Kingdom)?" with "English"
survey_data[,4] <- gsub("English \\(United Kingdom\\)\\?", "English", survey_data[,4])
#show all different replies in the 7th column and how many replies for each
table(survey_data[,7])
survey_data[,7]<-gsub("2x EFZ in verschiedenen Berufen", "Berufslehre/EFZ",survey_data[,7])
survey_data[,7]<-gsub("Berufsprüfung Fachmann Sicherheit und Bewachung", "Eidg. Fachausweis",survey_data[,7])
survey_data[,7]<-gsub("Fachausweis", "Eidg. Fachausweis",survey_data[,7])
survey_data[,7]<-gsub("Berufsprüfung", "Eidg. Fachausweis",survey_data[,7])
survey_data[,7]<-gsub("Eidg. Eidg. Fachausweis", "Eidg. Fachausweis",survey_data[,7])
survey_data[,7]<-gsub("Doktorat Universität", "PHD",survey_data[,7])
survey_data[,7]<-gsub("Doktorat", "PHD",survey_data[,7])
survey_data[,7]<-gsub("PhD.", "PHD",survey_data[,7])
survey_data[,7]<-gsub("PhD", "PHD",survey_data[,7])
survey_data[,7]<-gsub("PHD an einer Universität", "PHD",survey_data[,7])
survey_data[,7]<-gsub("PHDan einer Universität", "PHD",survey_data[,7])
survey_data[,7]<-gsub("ETH Master", "Master an einer Universität",survey_data[,7])
survey_data[,7]<-gsub("Uni-Diplom", "Bachelor an einer Universität",survey_data[,7])
survey_data[,7]<-gsub("MBA", "Master an einer Universität",survey_data[,7])
survey_data[,7]<-gsub("Volksschule", "Obligatorische Schule",survey_data[,7])
survey_data[,7]<-gsub("Sekundarschule", "Obligatorische Schule",survey_data[,7])
survey_data[,7]<-gsub("Schulabschluss (In Ausbildung)", "Obligatorische Schule",survey_data[,7])
survey_data[,7]<-gsub("In Ausbildung KV EFZ", "Obligatorische Schule",survey_data[,7])

#change the entry at the 7th column with row Id 164 to "Obgligatorische Schule"
survey_data[163,7] <- "Obligatorische Schule"
survey_data[,7]<-gsub("obligatorische Schule", "Obligatorische Schule",survey_data[,7])
survey_data[,7]<-gsub("Oberstufe", "Obligatorische Schule",survey_data[,7])
table(survey_data[,7])

#column 9 cleanup
table(survey_data[,9])
#change column 10 to "Nuclear" for Ids 211,22,46
survey_data[c(211,217,22,46),10] <- "Nuclear"
survey_data[170,10] <- "Grid"
survey_data[,9]<-gsub("Axpo Power", "Generation & Distribution",survey_data[,9])
survey_data[,9]<-gsub("Beznau Stromproduktion", "Generation & Distribution",survey_data[,9])
survey_data[,9]<-gsub("Nuclear", "Generation & Distribution",survey_data[,9])
survey_data[,9]<-gsub("Generation & Distrubution", "Generation & Distribution",survey_data[,9])
survey_data[,9]<-gsub("Power", "Generation & Distribution",survey_data[,9])
survey_data[,9]<-gsub("Grid", "Generation & Distribution",survey_data[,9])
survey_data[,9]<-gsub("KBM-Q", "Generation & Distribution",survey_data[,9])
survey_data[,9]<-gsub("Berufsbildung", "Services",survey_data[,9])                      
survey_data[,9]<-gsub("Services aber arbeite jedes halbe Jahr in einer anderen Abteilung", "Services",survey_data[,9])                      
survey_data[,9]<-gsub("Group IT", "Services",survey_data[,9])
survey_data[,9]<-gsub("Finance", "Trading & Sales",survey_data[,9])
survey_data[,9]<-gsub("Lager", "Generation & Distribution",survey_data[,9])
survey_data[,9]<-gsub("COO", "Geschäftsleitung",survey_data[,9])
survey_data[,9]<-gsub("CFO", "Geschäftsleitung",survey_data[,9])
survey_data[141,9] <- "Geschäftsleitung"
table(survey_data[,9])

table(survey_data[,13])
survey_data[,13]<-gsub("Beznau ", "Beznau",survey_data[,13])
survey_data[,13]<-gsub("KKB ", "Beznau",survey_data[,13])
survey_data[,13]<-gsub("Beznau, Doettingen", "Beznau",survey_data[,13])
survey_data[,13]<-gsub("Frage unklar formuliert. Was: Büro, Wo: Beznau", "Beznau",survey_data[,13])
survey_data[,13]<-gsub("BeznauBeznau", "Beznau",survey_data[,13])
survey_data[,13]<-gsub("Kernkraftwerk Beznau", "Beznau",survey_data[,13])
survey_data[,13]<-gsub("Kernkraftwerk Beznau(Döttingen)", "Beznau",survey_data[,13])
survey_data[,13]<-gsub("KKB", "Beznau",survey_data[,13])
survey_data[,13]<-gsub("Beznau(Döttingen)", "Beznau",survey_data[,13])
survey_data[,13]<-gsub("Beznau(KKB)", "Beznau",survey_data[,13])
survey_data[,13]<-gsub("Kommandoraum", "Beznau",survey_data[,13])
survey_data[,13]<-gsub("kkb", "Beznau",survey_data[,13])
survey_data[,13]<-gsub("Zone und Büro", "Beznau",survey_data[,13])
survey_data[,13]<-gsub("Beznau, Döttingen", "Beznau",survey_data[,13])
survey_data[,13]<-gsub("Beznau(Beznau)", "Beznau",survey_data[,13])
survey_data[,13]<-gsub("zur Zeit Nukleartechnikerschule, ansonst Beznau", "Beznau",survey_data[,13])
survey_data[c(32,15),13] <- "Beznau"
survey_data[,13]<-gsub("Gebäude M", "Baden",survey_data[,13])
survey_data[,13]<-gsub("BAden", "Baden",survey_data[,13])
survey_data[,13]<-gsub("BADEN", "Baden",survey_data[,13])
survey_data[,13]<-gsub("Baden im D Gebäude", "Baden",survey_data[,13])
survey_data[,13]<-gsub("Hauptsächlich Baden", "Baden",survey_data[,13])
survey_data[,13]<-gsub("Baden, Rathausen, HO", "Baden",survey_data[,13])
survey_data[,13]<-gsub("Baden & Homeoffice", "Baden",survey_data[,13])
survey_data[,13]<-gsub("Baden / Beznau", "Baden",survey_data[,13])
survey_data[,13]<-gsub("Baden und Beznau", "Baden",survey_data[,13])
survey_data[,13]<-gsub("Baden, Benzau, Würenlingen, Leibstadt", "Baden",survey_data[,13])
survey_data[,13]<-gsub("Baden ", "Baden",survey_data[,13])

#for those who selected "Beznau" in column 13 but have not selected "Generation & Distribution" in column 9 and/or whose colum 10 is empty, change column 9 to "Generation & Distribution" and column 10 to "Nuclear"
beznau_ids <- which(survey_data[,13] == "Beznau")
for (id in beznau_ids) {
  if (survey_data[id,9] != "Generation & Distribution") {
    survey_data[id,9] <- "Generation & Distribution"
  }
  if (survey_data[id,10] == "") {
    survey_data[id,10] <- "Nuclear"
  }
}
table(survey_data[,13])

#for those who have selected "Services" in column 9 but are not empty in column 10, emtpy their column 10
services_ids <- which(survey_data[,9] == "Services")
for (id in services_ids) {
  if (survey_data[id,10] != "") {
    survey_data[id,10] <- ""
  }
}
table(survey_data[,9])

table(survey_data[,12])
survey_data[,12]<-gsub("Computer, Strategisch und Führung", "...am Computer",survey_data[,12])
survey_data[,12]<-gsub("Kommandoraum meistens noch mit Knöpfen, aber auch PC", "...beides etwa gleich",survey_data[,12])
survey_data[,12]<-gsub("Überwachung im Kommandoraum", "...beides etwa gleich",survey_data[,12])
table(survey_data[,12])

table(survey_data[,14])
#create a new column that contains the data from column 14, but only the first three digits of that code
survey_data$Teamkürzel <- substr(survey_data[,14], 1, 3)
table(survey_data$Teamkürzel)
#again a new one, this time only the first two digits
survey_data$Teamkürzel2<- substr(survey_data[,14], 1, 2)
table(survey_data$Teamkürzel2)
#now with one digit
survey_data$Teamkürzel1<- substr(survey_data[,14], 1, 1)
table(survey_data$Teamkürzel1)

#columns 16, 19 and 21 often contain multiple answers separated by a ";". 
#keep the columns as list
multi_cols <- c(16, 19, 21)

survey_data <- survey_data %>%
  mutate(
    across(
      all_of(multi_cols),
      \(v) {
        # split every cell on ";" -> list
        out <- str_split(v, "\\s*;\\s*")
        # clean each element of the list
        lapply(out, function(x) {
          # handle NA from str_split
          if (length(x) == 1 && is.na(x)) return(NA_character_)
          x <- str_trim(x)
          x <- x[x != ""]
          if (length(x) == 0) return(NA_character_)
          x
        })
      }
    )
  )

#colunms 22 to 45 are all likert scale from 1 to 7 with text. remove text, convert to numerical
likert_cols <- 22:45
survey_data <- survey_data %>%
  mutate(
    across(
      all_of(likert_cols),
      \(v) {
        # extract the leading number
        as.integer(str_extract(v, "^\\d+"))
      }
    )
  )
#check if all values in likert columns are now between 1 and 7 or NA
for (col in likert_cols) {
  if (any(!is.na(survey_data[[col]]) & (survey_data[[col]] < 1 | survey_data[[col]] > 7))) {
    stop(paste("Values out of range in column", col))
  }
}

#column 6: rename "Männlich" to "male" and "Weiblich" to "female"
survey_data[,6]<-gsub("Männlich", "male",survey_data[,6])
survey_data[,6]<-gsub("Weiblich", "female",survey_data[,6])

#rename columns for better readability
colnames(survey_data)[5] <- "Age"
colnames(survey_data)[6] <- "Gender"
colnames(survey_data)[7] <- "Education_Level"
colnames(survey_data)[8] <- "Years_of_Experience"
colnames(survey_data)[9] <- "Department"
colnames(survey_data)[10] <- "Division"
colnames(survey_data)[11] <- "Employment_Type"
colnames(survey_data)[12] <- "Work_Type"
colnames(survey_data)[13] <- "Work_Location"
colnames(survey_data)[14] <- "Team_Abbr"
colnames(survey_data)[15] <- "Private_AI_Use"
colnames(survey_data)[16] <- "Private_AI_Tools"
colnames(survey_data)[17] <- "Private_AI_Frequency"
colnames(survey_data)[18] <- "Work_AI_Use"
colnames(survey_data)[19] <- "Work_AI_Tools"
colnames(survey_data)[20] <- "Work_AI_Frequency"
colnames(survey_data)[21] <- "Work_AI_Purposes"

colnames(survey_data)[22] <- "PEoU1"
colnames(survey_data)[23] <- "PEoU2"
colnames(survey_data)[24] <- "PEoU3"
colnames(survey_data)[25] <- "PU1"
colnames(survey_data)[26] <- "PU2"
colnames(survey_data)[27] <- "PU3"
colnames(survey_data)[28] <- "PU4"
colnames(survey_data)[29] <- "ATU1"
colnames(survey_data)[30] <- "ATU2"
colnames(survey_data)[31] <- "ATU3"
colnames(survey_data)[32] <- "ATU4"
colnames(survey_data)[33] <- "E1"
colnames(survey_data)[34] <- "E2"
colnames(survey_data)[35] <- "E4"
colnames(survey_data)[36] <- "T1"
colnames(survey_data)[37] <- "T2"
colnames(survey_data)[38] <- "T3"
colnames(survey_data)[39] <- "T4"
colnames(survey_data)[40] <- "SN1"
colnames(survey_data)[41] <- "SN3"
colnames(survey_data)[42] <- "AU1"
colnames(survey_data)[43] <- "AU2"
colnames(survey_data)[44] <- "AU3"
colnames(survey_data)[45] <- "AU4"

colnames(survey_data)[46] <- "Comments"
colnames(survey_data)[47] <- "Email"

colnames(survey_data)[48] <- "Team_Abbr3"
colnames(survey_data)[49] <- "Team_Abbr2"
colnames(survey_data)[50] <- "Team_Abbr1"

#resort columns so 48, 49 and 50 are after 14
survey_data <- survey_data %>%
  select(1:14, Team_Abbr3, Team_Abbr2, Team_Abbr1, 15:47)

#save cleaned data
saveRDS(survey_data, "Survey_Data/survey_results_cleaned.rds")


