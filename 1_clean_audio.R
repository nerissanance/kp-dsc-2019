#Cleaning file
#creates clean training and testing dataframes 
#Original code fromthe following source:

##########################
# Tutorial : PART 1 : Reading Audio File & Create Data-sets for modeling
# Author : Taposh Roy
# Email : taposh.d.roy@kp.org
# Date: Sept 25th 2019
# References :
# https://marce10.github.io/2017/06/06/Individual_sound_files_for_each_selection.html
# https://cran.r-project.org/web/packages/seewave/vignettes/seewave_IO.pdf
# Update : Oct 3rd 2019
# Saving the testing file fname column with ".wav" 
# Update : Oct 15th 2019
##########################

options(warn = -1)

##########################
#imports packages Needed
##########################

packages_needed <- c("h2o","readr", "tuneR","wrassp","audio","seewave","rgl","DataCombine","dplyr","tools","tidyverse","tfdatasets","keras","glmnet")
new.packages <- packages_needed[!(packages_needed %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(wrassp)
library(readr)
library(tuneR)
library(audio)
library(seewave)
library(DataCombine)
library (dplyr)
library (tools)
library(tidyverse)
library(tfdatasets)
library(keras)
library(glmnet)
library(h2o)
library(knitr)
library(rio)
#knit('/Users/p624626/Desktop/read_audio_ecg.Rmd', encoding = 'UTF-8')

##########################
# Paths for files
##########################
description <- read_csv('./data/data_description.csv')
head(description)
table(description$dataset)
test_data <- description[description$dataset=='test',]
tr_data <- description[description$dataset=='train',]

train_path_audio <- './data/train'
test_path_audio <- './data/test'

#########################
# Read training Data
#########################

tr_df <- as.data.frame(tr_data)
tr_df$fname <- str_split(tr_df$fname, ".wav", simplify = TRUE)
#apply(tr_df$response, 1, function(x) sum(x) )

#limit the response outcome to normal and abnormal
fix_response <- function(x) {
  if(x != "normal"){x ="1"} #abnormal =1
  if(x == "normal"){x ="0"} #normal =0
  x
}
tr_df$response <- lapply(tr_df$response, fix_response)
head(tr_df)

##########################
# Read test Data
##########################

test_df <- as.data.frame(test_data)
test_df$fname <- str_split(test_df$fname, ".wav", simplify = TRUE)
head(test_df)


##############################################################################
#create a Feature list
##############################################################################
name_list <- c("fname", "num_samples","sample_rate", "min_range","max_range","normal_min_ampl_range","normal_max_ampl_range","fmVals","bwVals")
df <- data.frame(matrix(ncol = 9, nrow = 0))
colnames(df) <- name_list

###########################################################################
#Create a function to obtain all features 
###########################################################################
get_audio_features <- function(x) {
  #tuneR
  tr <- readWave(x) # load file
  #print(t@left)
  ar <- read.AsspDataObj(x)
  
  #File Name
  fname <- file_path_sans_ext(basename(x))
  #add Feature Number of Samples
  num_samples <- numRecs.AsspDataObj(ar)
  # calculate formants and corresponding bandwidth values
  fmBwVals <- forest(x,toFile=F)
  fmVals <- fmBwVals$fm
  bwVals <- fmBwVals$bw
  #add Feature Sample Rate
  sample_rate <- tr@samp.rate
  left= tr@left
  #left
  range_audio = range(tr@left)
  #add Feature min_amplitude_range
  min_range =range_audio[1]
  #add Feature min_amplitude_range
  max_range =range_audio[2]
  
  normvalues=left/2^(tr@bit -1) 
  normal_range <- range(normvalues)
  #add Feature normalized_min_amplitude_range
  normal_min_ampl_range <- normal_range[1]
  #add Feature normalized_min_amplitude_range
  normal_max_ampl_range <- normal_range[2]
  
  
  mylist <- c(fname=fname,num_samples=num_samples,sample_rate=sample_rate, min_range=min_range, max_range=max_range, normal_min_ampl_range=normal_min_ampl_range, normal_max_ampl_range=normal_max_ampl_range,fmVals=fmVals,bwVals=bwVals)
  return(mylist)
}

#############################
# Create a function to Process all data 
#############################

process_data <- function(path_audio){
  files <- list.files(path=path_audio, pattern="*.wav", full.names=TRUE, recursive=FALSE)
  for(ii in files){
    l<- get_audio_features(ii)
    df[nrow(df) + 1,] <- l
  }
  return(df)
}

###############################################################################

#Train Audio Path
df = process_data(train_path_audio)
#Create Training Data 
training_df <- merge(df, tr_df, by="fname")
#drop dataset and subcat columns
training_df <- subset(training_df, select = -c(dataset, subcat))
training_df <- as.data.frame(apply(training_df,2,as.character))

#Test Audio Path
df2=process_data(test_path_audio)
#Create Training Data 
testing_df <- merge(df2, test_df, by="fname")
#drop dataset, response and subcat columns
testing_df <- subset(testing_df, select = -c(dataset, response,subcat))
testing_df$fname <- lapply(testing_df$fname, function(x) paste0(x, ".wav"))
testing_df$fname <- as.character(testing_df$fname)
#testing_df <- as.data.frame(apply(testing_df,1,as.character))
testing_df <- as.data.frame(testing_df)


#Saving Data Sets for re-use
write.csv(training_df, file = "./data/training_df.csv",row.names=FALSE)
write.csv(testing_df, file = "./data/testing_df.csv",row.names=FALSE)

###############################################################################
