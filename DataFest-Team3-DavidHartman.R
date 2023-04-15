library(stringr)
library(stringi)
library(usmap)
library(ggplot2)
library(dplyr)

attorneys <- read.csv('attorneys.csv')
clients <- read.csv('clients.csv')
questions <- read.csv('questions.csv')

state_abbreviations <- state.abb

state_counts <- table(factor(questions$StateAbbr, levels = state_abbreviations))

mismatch_count <- sum(questions$TakenByAttorneyUno != questions$ClosedByAttorneyUno)

mismatchbystate <- table(factor((questions$TakenByAttorneyUno != questions$ClosedByAttorneyUno) , levels = state_abbreviations))

AttorneysByState <- table(factor(attorneys$StateAbbr, levels = state_abbreviations))

CompareTable <- state_counts/AttorneysByState

ClientsByState <- table(factor(clients$StateAbbr, levels = state_abbreviations))

CompareClientstoAttByState <- ClientsByState/AttorneysByState

avginterval_table <- questions %>%
  mutate(AskedOnUtc = as.POSIXct(AskedOnUtc, format = "%Y-%m-%d %H:%M:%OS"),
         ClosedOnUtc = as.POSIXct(ClosedOnUtc, format = "%Y-%m-%d %H:%M:%OS")) %>%
  filter(!is.na(ClosedOnUtc)) %>%  # filter out rows with NULL values in ClosedOnUtc
  group_by(StateAbbr) %>%
  summarize(avg_time_per_ticket = mean(ClosedOnUtc - AskedOnUtc))

mismatchbystate <- questions %>%
  filter(TakenByAttorneyUno != ClosedByAttorneyUno) %>%
  group_by(StateAbbr) %>%
  summarize(count = n())


StateData <- merge(AttorneysByState, ClientsByState, by.x = 1, by.y = 1) %>%
  merge(CompareClientstoAttByState, by = 1) %>%
  merge(CompareTable, by = 1) %>%
  merge(state_counts, by = 1, all.x = TRUE) %>%
  merge(mismatchbystate, by = 1, all.x = TRUE) %>%
  merge(avginterval_table, by = 1, all.x = TRUE)
colnames(StateData) = c("state","Attorneys","Clients",
                            "Clients/Attorney","Questions/Attorney",
                        "Total Questions",
                            "Mismatched Open/Close",
                        "Average Time per Ticket")

StateData$`Average Time per Ticket` <- StateData$`Average Time per Ticket`/86400
StateData$`Average Time per Ticket` <- gsub("secs", "days", StateData$`Average Time per Ticket`)

StateData <- StateData[(!is.nan(StateData$`Clients/Attorney`)), ]
rownames(StateData) <- NULL

top10_indices_clientratio <- order(-StateData$`Clients/Attorney`)[1:10]
top10_rows_clientratio <- StateData[top10_indices_clientratio, ]
top10_rows_clientratio

top10_indices_questionsratio <- order(-StateData$`Questions/Attorney`)[1:10]
top10_rows_questionratio <- StateData[top10_indices_questionsratio, ]
top10_rows_questionratio

barplot(top10_rows_clientratio$`Clients/Attorney`,
        names.arg = top10_rows_clientratio$state, 
        xlab = "States", ylab = "Clients/Attorney", 
        main = "Top 10 States: Clients/Attorney Ratio",
        las = 1,
        ylim = c(0,100),
        col = "lightblue")
grid(nx = NA, ny = NULL, lty = 1, col = "gray")

barplot(top10_rows_questionratio$`Questions/Attorney`,
        names.arg = top10_rows_questionratio$stateabbr, 
        xlab = "States", ylab = "Questions/Attorney", 
        main = "Top 10 States: Questions/Attorney Ratio",
        las = 1,
        ylim = c(0,60),
        col = "lightblue")
grid(nx = NA, ny = NULL, lty = 1, col = "gray")


plot_usmap(data = StateData, values = "Total Questions", color = "red") + 
  scale_fill_continuous(name = "Total Questions", label = scales::comma) + 
  theme(legend.position = "right")

plot_usmap(data = StateData, values = "Clients/Attorney", color = "red") + 
  scale_fill_continuous(name = "Clients/Attorney", label = scales::comma) + 
  theme(legend.position = "right")

plot_usmap(data = StateData, values = "Questions/Attorney", color = "red") + 
  scale_fill_continuous(name = "Questions/Attorney", label = scales::comma) + 
  theme(legend.position = "right")

plot_usmap(data = StateData, values = "Clients", color = "red") + 
  scale_fill_continuous(name = "Clients", label = scales::comma) + 
  theme(legend.position = "right")

plot_usmap(data = StateData, values = "Attorneys", color = "red") + 
  scale_fill_continuous(name = "Attorneys", label = scales::comma) + 
  theme(legend.position = "right")
        
