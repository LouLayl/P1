
library(tidyverse)
library(data.table)
library(ggplot2)
library(tibble)
setwd("~/Project1")

data = fread("healthcare-dataset-stroke-data.csv")
head(data)

# take out relevant data 
heart_hyper= data%>% 
  select("hypertension", "heart_disease", "stroke")


stroke_summary = heart_hyper%>%
  group_by(stroke)%>%
  summarise(
    total = n()
  )



# check for invalid values and empty values

heart_hyper %>%
  filter(!stroke %in% c(0, 1))
heart_hyper %>%
  filter(!hypertension %in% c(0,1))
heart_hyper %>%
  filter(!heart_disease %in% c(0,1))

nrow(heart_hyper)
totaln = nrow(heart_hyper)

# Grouped total stroke cases based on whether person has hypertension (1) or not (0)

hypertension = heart_hyper %>%
  group_by(hypertension)%>%
  summarise(
    totalstroke = sum(stroke)
  )%>%
  mutate(
    percentage = totalstroke/249
  )

# Grouped total stroke cases based on whether person has heart disease (1) or not (0)
heartdisease = heart_hyper %>%
  group_by(heart_disease)%>%
  summarise(
    totalstroke = sum(stroke)
  )%>%
  mutate(
    percentage = totalstroke/249
  )

head(data)

head(heart_hyper)

heart_hyper= data%>% 
  select("hypertension", "heart_disease", "stroke") %>% 
  mutate(
    heartdisease_stroke = if_else(stroke == 1 & heart_disease== 1, 1, 0),
    hypertension_stroke = if_else(stroke == 1 & hypertension== 1, 1, 0), 
    both_stroke = if_else(stroke == 1 & hypertension == 1 & heart_disease == 1, 1, 0)
  )

summary = heart_hyper%>% 
  summarise(
    heartdisease_stroke_total = sum(heartdisease_stroke),
    hypertension_stroke_total = sum(hypertension_stroke),
    both_total = sum(both_stroke)
  )

percentage_graph = data %>%
  select(hypertension, heart_disease, stroke)%>%
  group_by(hypertension, heart_disease)%>%
  summarise(
    stroke_count = sum(stroke),
    total = n()
  )%>% 
  mutate(percentage = stroke_count/total) %>%
  add_column(Group = c("Neither", "Heartdisease", "Hypertension", "both"))

row.names(percentage_graph)[1] <- "neither"
row.names(percentage_graph)[2] <- "heartdisease"
row.names(percentage_graph)[3] <- "hypertension"                    
row.names(percentage_graph)[4] <- "both"


        
  


percentage_graph  %>%
  ggplot(aes (x = Group, y = percentage, fill = Group)) +
  geom_bar(stat="identity")+
  scale_fill_brewer(palette="Set1")+
  ggtitle("Stroke Risk in Individuals with Heart Disease and Hypertension")



