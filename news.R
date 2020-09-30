library(tidyverse)
library(DataExplorer)
library(caret)
library(naniar)
library(textcat)

#### Fake News

## load in data 
fake.train <- read_csv("/Users/graceedriggs/Documents/Stat 495/Fake News/fake-news/train.csv")
fake.test <- read_csv("/Users/graceedriggs/Documents/Stat 495/Fake News/fake-news/test.csv")

fake <- bind_rows(fake.test, fake.train)
summary(fake)

## see how much data is missing
plot_missing(fake.train)

## replace the "nan" with NAs
fake.train <- fake.train %>% replace_with_na(replace = list(author = 'nan'))

fakeTrainComplete<- na.omit(fake.train) 
plot_missing(fakeTrainComplete)

## separate the different languages
fakeTrainComplete <- fakeTrainComplete %>%
  mutate(language = textcat(x = fakeTrainComplete$text))

table(fakeTrainComplete$language)
    ##what the heck. Weird random languages?
