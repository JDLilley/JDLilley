# Load the required libraries
library(dplyr)
library(tm)
library(ggplot2)
library(tidyr)
library(tidytext)
library(textclean)
library(stopwords)
library(text2vec)
library(proxy)
library(dendextend)
library(factoextra)
library(stringr)
library(stringdist)
library(cluster)
library(hansard)
library(rvest)
library(fmsb)
library(lubridate)
library(magick)
library(gtools)
library(gifski)
library(nortest)
library(SGmisc)

### Create Functions ###
# Process_text - Function to remove anything after ':' using regular expressions
remove_text_after_colon <- function(input_text) {
  result_text <- sub(":.*", "", input_text)
  return(result_text)
}
# Process_text - Rmv_punct
remove_punct <- function(text) {
  # Use gsub to remove all punctuation characters
  cleaned_text <- gsub("[[:punct:]]", " ", text)
  return(cleaned_text)
}
# Process_text - rmv_no.
remove_num <- function(text){ 
  # Use gsub to remove all numbers 
  result_text <- gsub("[0-9]", "", text)
  return(result_text)
}
# Process_text - rmv_stop
remove_stopwords <- function(sentences, stopwords) {
  cleaned_sentences <- character(length(sentences))
  for (i in 1:nrow(sentences)) {
    # Split the sentence into individual words
    words <- strsplit(sentences$processed_title[[i]], "\\s+")
    # Flatten the list of words
    words <- unlist(words)
    # Filter out stopwords
    words <- words[!words %in% stopwords]
    # Reconstruct the sentence
    cleaned_sentences[i] <- paste(words, collapse = " ")
  }
  return(cleaned_sentences)
}
# Process_text - Function to strip anything before the underscore
strip_before_underscore <- function(input_vector) {
  result <- sub(".*_", "", input_vector)
  return(result)
}
# Process_text - Rmv_freq
Freq_words <- function(df,n){
  df_tokens <- df %>% 
    group_by(processed_title) %>%
    mutate(word = processed_title %>% 
             str_split(" ")) %>%
    unnest(word) %>%
    ungroup()
  # Remove rows with empty strings in the 'word' column
  df_tokens_clean <-  filter(df_tokens, word != "")
  # This section also removes duplicate titles
  # Convert the data to a matrix, where each row represents a document (title) and each column represents a word count
  word_matrix <- df_tokens_clean %>%
    count(processed_title, word) %>%
    pivot_wider(names_from = word, values_from = n, values_fill = 0, names_glue = "word_{word}")
  word_matrix_prep <- word_matrix[1:nrow(word_matrix),2:length(word_matrix)]
  #Can we remove useless words like bill?
  # Plot frequent terms
  freq <- colSums(word_matrix_prep)
  freq <- subset(freq, freq>=n)
  Exess_words <- rownames(as.matrix(freq))
  Exess_words <- strip_before_underscore(Exess_words)
  Exess_words <- c(Exess_words,'lords', 'stage','report','clock', 'prog','programme', ' rd' , ' nd','third','reading','allotted' ,'motion','clause' ,'committee','read','new','forthwith','accordingly','main','standing','amendments','amendment',' th ')
  print(Exess_words)
  cleaned_sentences <- character(length(df))
  for (i in 1:nrow(df)) {
    # Split the sentence into individual words
    words <- strsplit(df$processed_title[[i]], "\\s+")
    # Flatten the list of words
    words <- unlist(words)
    # Filter out stopwords
    words <- words[!words %in% Exess_words]
    # Reconstruct the sentence
    cleaned_sentences[i] <- paste(words, collapse = " ")
  }
  return(cleaned_sentences)
}
# Process_text - 3 words
three_words <- function(sentences) {
  processed_sentences <- character(nrow(sentences))
  for (i in 1:nrow(df)) {
    # Split the sentence into individual words
    words <- strsplit(sentences$processed_title[i], "\\s+")
    
    # Check if the sentence is greater than 3 words long
    if (length(words[[1]]) > 3) {
      # Keep only the first 3 words
      processed_sentences[i] <- paste(words[[1]][1:3], collapse = " ")
    } else {
      # Keep the original sentence if it has 3 or fewer words
      processed_sentences[i] <- sentences$processed_title[i]
    }
  }
  
  return(processed_sentences)
}
# Process_text - EU
abbreviate_eu <- function(sentences) {
  processed_sentences <- character(nrow(sentences))
  for (i in 1:nrow(df)) {
    # Check if the title contains the phrase 'european union'
    if (grepl(" eu ", sentences$processed_title[i], ignore.case = TRUE)) {
      # Replace 'european union' with 'eu' in the title
      processed_sentences[i] <- gsub(" eu ", " european union ", sentences$processed_title[i], ignore.case = TRUE)
    } else {
      # Keep the original sentence if it has 3 or fewer words
      processed_sentences[i] <- sentences$processed_title[i]
    }
  }
  return(processed_sentences)
}
# Process_text - NHS 
abbreviate_NHS <- function(sentences) {
  processed_sentences <- character(nrow(sentences))
  for (i in 1:nrow(df)) {
    # Check if the title contains the phrase 'european union'
    if (grepl("nhigh speed rail", sentences$processed_title[i], ignore.case = TRUE)) {
      # Replace 'european union' with 'eu' in the title
      processed_sentences[i] <- gsub("nhigh speed rail", "NationalHealthService", sentences$processed_title[i], ignore.case = TRUE)
    } else {
      # Keep the original sentence if it has 3 or fewer words
      processed_sentences[i] <- sentences$processed_title[i]
    }
  }
  return(processed_sentences)
}
# Process_text - HighSpeed
abbreviate_HS <- function(sentences) {
  processed_sentences <- character(nrow(sentences))
  for (i in 1:nrow(df)) {
    # Check if the title contains the phrase 'european union'
    if (grepl("hs", sentences$processed_title[i], ignore.case = TRUE)) {
      # Replace 'european union' with 'eu' in the title
      processed_sentences[i] <- gsub("hs", "high speed rail", sentences$processed_title[i], ignore.case = TRUE)
    } else {
      # Keep the original sentence if it has 3 or fewer words
      processed_sentences[i] <- sentences$processed_title[i]
    }
  }
  return(processed_sentences)
}
# Process_text - Single 'l'
single_let <- function(sentences) {
  processed_sentences <- character(nrow(sentences))
  
  for (i in 1:nrow(sentences)) {
    # Split the titles into individual words
    words <- strsplit(sentences$processed_title[i], " ")
    # Filter out single-letter words
    filtered_words <- words[[1]][!(nchar(words[[1]]) == 1)]
    # Join the remaining words back into a single string
    processed_words <- paste(filtered_words, collapse = " ")
    # Store the processed title in the result vector
    processed_sentences[i] <- processed_words
    
  }  # Return the updated titles as a data frame
  
  return(processed_sentences)
}
# Process_text - Remove ' '
Remove_space <- function(sentences) {
  processed_sentences <- character(nrow(sentences))
  for (i in 1:nrow(sentences)) {
    if (substr(sentences$processed_title[i], 1, 1) == " ") {
      processed_sentences[i] <- substr( sentences$processed_title[i], 2, nchar(sentences$processed_title[i]))
    } else {
      # Keep the original sentence if it has 3 or fewer words
      processed_sentences[i] <- sentences$processed_title[i]
    }
  }
  return(processed_sentences)
}

# Remove Txt Before ':'
remove_text_before_colon <- function(input_text) {
  if (grepl(":", input_text)) {
    result_text <- sub(".*:", "", input_text)
    return(result_text)
  } else {
    return(input_text)
  }
}

# MP Data
check_add_mp <- function(df,division) {
  add_mp <- c()
  for (j in division$member_printed_value){
    j <- format(j)
    if (!(j %in%  colnames(df))){
      add_mp <- c(add_mp, j)
    }
  }
  return(add_mp)
}
# Remove the titles
format <- function(name){
  name <- gsub("Sir |Dame |Dr |Mr |Mrs |Miss |Ms ", "", name)
  name <- gsub("^\\s+", "", name)
  return(name)
}

#Visualisation
create_beautiful_radarchart_simple <- function(data, color = "#00AFBB", 
                                               vlabels = colnames(data), vlcex = 0.8, plwd = 2, plty = 1,
                                               caxislabels = NULL, title = paste(rownames(profile)[3],'vote profile 2006-2023',colapse = ''), ...){
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = plwd, plty = plty,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
  
}
# 5 plot comp
create_beautiful_radarchart_comp <- function(data, color = "#00AFBB", 
                                             vlabels = cat(colnames(data)), vlcex = 1.5,cex.main = 1,plwd = 4,plty = 1,
                                             caxislabels = NULL,cglwd = 5,cex.axis = 5, title = paste(rownames(profile)[3],'vote profile 2006-2023',colapse = ''), axis.text = element_text(face="bold"),...){
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pfcol = c("#99999980",NA),
    pcol = c(NA,"#00AFBB"), plty = plty, plwd = plwd,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = cglwd,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,cex.main = cex.main,
    caxislabels = caxislabels, 
    title = row.names(df_yearly)[i],cex.axis = cex.axis,axis.text = 4,
    
  )
  
}

# gif
create_beautiful_radarchart <- function(data, title,subtitle, color = "#00AFBB", 
                                        vlabels = colnames(data), vlcex = 1.2,
                                        caxislabels = NULL, ...){
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,cex.main = 3,
    caxislabels = caxislabels, title = title, ...
  )
  # Add legend
  legend(x = -0.25, y = 1.135, title = subtitle, NA, bty="n", title.cex = 2)
}

# Polar_bar
make_bar <- function(df,titles){ 
  ggplot(df, aes(x=seq(-1, 1, by = 0.1), y=counts.Freq)) + 
    geom_bar(stat="identity", fill=c("#203c3e", "#203c3e","#284b4d", "#284b4d", "#346164","#346164","#3f7478","#3f7478", "#4a898d",'#4c8d91',"#4a898d","#549da2","#549da2","#5eaeb4","#5eaeb4","#67bfc5","#67bfc5","#70d0d7","#70d0d7", "#78e0e7","#78e0e7")) +
    labs(x="Position on Issue", y="MP Count", title = titles) +
    ggtitle(titles) +
    theme(plot.title = element_text(hjust = 0.5, size = 20),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10))+
    coord_cartesian(ylim = c(0, 1000))
  
}


####################################


## Pull Hansard.API
HoC_API <- hansard_commons_divisions(start_date = "2006-01-01")
End_date <- Sys.Date()
API_Division <- select(HoC_API, about , title, date_value)

# Build link DebateAsText 
df <- API_Division %>% mutate(search_url = NA)

for (s in 1:nrow(df)) {
  date <- df[s,3]
  title <- substr(df[s,2], 1, 10)
  link <- sprintf("https://hansard.parliament.uk/search/Divisions?startDate=%s&endDate=%s&searchTerm=%s&house=Commons&includeCommitteeDivisions=False&partial=False&sortOrder=1",date,date,title)
  df[s,4] <- link}

df$file.name <- ''
write.csv(df, "/Users/jdlilley/Desktop/Data Science and Modelling/Dissertation/DataBase/Parlimentsearch.csv", row.names=FALSE)

#### Manual Debate as text file ####
# rip new files
# save new file name df
# Save as FileName_IDDiv


####################################

## Process Divisions
# Create df for 
setwd("~/Desktop/Data Science and Modelling/Dissertation/DataBase")
df <- read.csv('Parlimentsearch_text.csv')
df <- select(df, about , title, date_value, file.name)

# Remove rows with missing div
list_na <- c()
for (row in 1:nrow(df)) {
  if (df[row,4] == ''){
    list_na <- c(list_na, row)
  }
}
df <- df[-list_na, ]

# Get the list of English stop words
stop_words <- stopwords::stopwords("en")
# Apply the function to each row of the data.frame
df$processed_title <- sapply(df$title, remove_text_after_colon)
# Preprocess the titles - Convert to lowercase, remove punctuation, and numbers, remove stop words, remove frequant words
df$processed_title <- tolower(df$processed_title)
df$processed_title <- remove_num(df$processed_title)
df$processed_title <- remove_punct(df$processed_title)
df$processed_title <- remove_stopwords(df,stop_words)
df$processed_title <- Freq_words(df,300)
df$processed_title <- abbreviate_eu(df)
df$processed_title <- single_let(df)
df$processed_title <- abbreviate_HS(df)
df$processed_title <- abbreviate_NHS(df)
df$processed_title <- three_words(df)
df$processed_title <- Remove_space(df)

## Assign Clusters
# New titles to assign
subset_data <- as_tibble(unique(df$processed_title))
subset_data$cluster <- 'Miscellaneous'
# Assign Previous titles
Previous_clusters <- read.csv('LLM_cluster.csv')
for (i in 1:nrow(Previous_clusters)){
 No <- which(subset_data$value == Previous_clusters[i,1])
 subset_data$cluster[No] <-  Previous_clusters[i,2]
}

#### Manual Debate as text file ####




# Potential Help
word <- 'finance' # Word or phrase that can be assigned clearly
cat <- 'Parliamentary Procedures' # Cluster for assignment
for (i in grep(word, subset_data$value, ignore.case = TRUE, perl = FALSE, value = FALSE)){
  if (subset_data$cluster[i] == 'Miscellaneous'){
    subset_data$cluster[i] <- cat
  }}

# Write New file 
write.csv(subset_data, "/Users/jdlilley/Desktop/Data Science and Modelling/Dissertation/DataBase/LLM_cluster.csv", row.names=FALSE)





####################################

## Prompt Building
# Assign Clusters
df$cluster <- 'x'
for (i in subset_data$value){
  df$cluster[which(df$processed_title == i)] <- subset_data$cluster[which(subset_data$value == i)]
}
# Remove Non-Clustered
df <-  df[-which(df$cluster == 'Miscellaneous'),]
# Load file for + vs - 
prompt <-  read.csv('Prompt_clus.csv')

# Get the motion details for prompt
df$motion <- 'x'
for (i in 1:nrow(df)){
  df$motion[i] <- remove_text_before_colon(df$title[i])
  # Remove space at start
  if (substr(df$motion[i], 1, 1) == " ") {
    df$motion[i] <- substr(df$motion[i], 2, nchar(df$motion[i]))
  }
}

df$prompt <- 'x'
for (i in 1:nrow(df)){
  df$prompt[i] <- paste(c("Review the text file open in my current webpage. First, summarise the debate generally in 50 words. Next briefly summarise the arguments voting 'Ayes' (For), and 'Noes' (Against) in the division related to: '", df$title[i], "'. Finally this file is classified under '", prompt$cluster[i], "', I want you to consider if voting 'Aye' is, A: ", prompt$plus[which(prompt$cluster == df$cluster[i])]," , or B: ", prompt$minus[which(prompt$cluster == df$cluster[i])] , ". Please format your response strictly in the following manner: 'Summary - (50 words, about the text file)', 'For - (30-50 words, what it means to vote ‘Ayes’ in the division: ", df$motion[i],")', 'Against - (30-50 words, what it means to vote ‘Noes’ in the division: ",df$motion[i],")', 'Decision - (Justify if voting 'Aye' in the division: " , df$motion[i],",  is '",prompt$plus[which(prompt$cluster == df$cluster[i])],"', or '",prompt$minus[which(prompt$cluster == df$cluster[i])],"')' , 'Sentiment - (Based on your 'Decision', return either 'A', or 'B'. If voting 'Aye' is not associated with either, or associated with both A & B, return C)"),collapse = '')
}

#### Manual LLM response extraction ####




## Use:
manual_prompt <- select(df,about,prompt)





####################################

## Data Prep
# Extract sentiment
df$sentiment <- 'x'
df$For <- 'x'
df$Against <- 'x'
df$summary <- 'x'
df$why <- 'x'

setwd("~/Desktop/Data Science and Modelling/Dissertation/DataBase/LLM_DAT")
for (i in df$about){
  #build file name
  file <- paste(c(i,'.txt'),collapse = '')
  # Read in text file
  text <- paste(readLines(file),collapse = '')
  # Search for phrase 'Sentiment -' and return any words after that phrase
  df$sentiment[which(df$about == i)] <- gsub(".*Sentiment - ", "", text)
  df$summary[which(df$about == i)] <- gsub(".*Summary - (.*?)For - .*", "\\1", text, perl = TRUE)
  df$For[which(df$about == i)] <- gsub(".*For - (.*?)Against - .*", "\\1", text, perl = TRUE)
  df$Against[which(df$about == i)] <- gsub(".*Against - (.*?)Decision - .*", "\\1", text, perl = TRUE)
  df$why[which(df$about == i)] <- gsub(".*Decision - (.*?)Sentiment - .*", "\\1", text, perl = TRUE)
}

# check the second column & records any rows that contain just the letter 'C'
df <- df[-which(df$sentiment == 'C'), ]

## MP data
df$cluster[which(df$cluster ==  "European Union & Foreign Affairs\n")] <- "European Union & Foreign Affairs" 

# Pull hansard data
df_mp <- select(df,about,cluster,sentiment)
n <- 0
for (i in 1:nrow(df_mp)){
  # Get division info
  y <- commons_divisions(division_id = df_mp[i,1], summary = FALSE)
  n <- n+1
  print(n)
  Sys.sleep(0.05)
  # check if new MP
  add <- check_add_mp(df_mp,y)
  for (j in add) {
    df_mp[[j]] <- rep(0, nrow(df_mp))
  }
  # If voted assign Aye or Noe value to mp, row
  for (k in 1:nrow(y)){
    MP <- which(colnames(df_mp) == format(y$member_printed_value[k]))
    if (y$type[k]== 'Aye_Vote'){
      df_mp[i,MP] <- 1
    }
    if (y$type[k]== 'No_Vote'){
      df_mp[i,MP] <- -1
      
    }
  }
}

# Turn sentiment into 1 or -1
for (i in 1:nrow(df_mp)){
  if (df_mp$sentiment[i] == 'A'){
    df_mp$sentiment[i] <- 1
  }else{
    df_mp$sentiment[i] <- -1
  }
}

# Save and Load MP data 
setwd("~/Desktop/Data Science and Modelling/Dissertation/DataBase")
write.csv(df_mp, "/Users/jdlilley/Desktop/Data Science and Modelling/Dissertation/DataBase/df_mp.csv", row.names=FALSE)
df_mp <-  read.csv('df_mp.csv')
df_mp$sentiment <- as.integer(df_mp$sentiment)

## Radial df
df_radial <- as_tibble(colnames(df_mp[4:length(df_mp)]))
df_radial$"European Union & Foreign Affairs" <- 0
df_radial$"Parliamentary Procedures" <- 0
df_radial$"Environment & Energy" <- 0
df_radial$"Health & Healthcare"  <- 0
df_radial$"Welfare & Social Housing"  <- 0
df_radial$"Education & Learning"    <- 0
df_radial$"Economy & Financial Services" <- 0
df_radial$"Standards & Technology"  <- 0
df_radial$"Crime & Justice"  <- 0
df_radial$"Defence & Armed Forces" <- 0
df_radial$'Immigration & Borders' <- 0

# Create full Radial
df_mp_in <- df_mp
for (mp in df_radial$value){
  mp_n <- which(colnames(df_mp_in) == mp)
  for (clus in colnames(df_radial[2:12])){
    count <- 0
    for (div in 1:nrow(df_mp_in)){
      if (df_mp_in$cluster[div] == clus){
        count <- count + (df_mp_in[div,mp_n] * df_mp_in$sentiment[div])
      }
    }
    df_radial[which(df_radial$value == mp) ,which(colnames(df_radial) == clus)] <- count
  }
}

df_radial_norm <- df_radial
for (col in 2:12){
  for (row in 1:nrow(df_radial)){
    df_radial_norm[row,col] <- (df_radial[row,col] - min(df_radial[,col]))/(max(df_radial[,col])-min(df_radial[,col]))
  }
}

setwd("~/Desktop/Data Science and Modelling/Dissertation/DataBase")
write.csv(df_radial_norm, "/Users/jdlilley/Desktop/Data Science and Modelling/Dissertation/DataBase/df_rad_FULL.csv", row.names=FALSE)
df_rad_FULL <-  read.csv('df_rad_FULL.csv')
########### Visualisation ################

## Create DF
# Define the variable ranges: maximum and minimum
max_min <- data.frame(
  'European Union & Foreign Affairs' = c(1, 0), "Parliamentary Procedures" = c(1, 0), "Environment & Energy" = c(1,0),
  "Health & Healthcare" = c(1, 0), "Welfare & Social Housing" = c(1, 0), "Education & Learning" = c(1, 0),
  "Economy & Financial Services" = c(1, 0), "Standards & Technology" = c(1, 0), "Crime & Justice" = c(1, 0), "Defence & Armed Forces" = c(1,0),
  'Immigration & Borders' = c(1, 0)
)
rownames(max_min) <- c("Max", "Min")

## Create Simple visuals
for (i in df_rad_FULL$value){
  # set var + df
name <- i
mp <- which(df_rad_FULL$value == name)
profile <-  data.frame(df_rad_FULL[mp,])
rownames(profile) <- df_rad_FULL[mp,1]
profile <- profile[,2:12]
title <- paste0(rownames(profile)[3],'vote profile 2006-2023',colapse = '')

# Bind the variable ranges to the data
df_p <- rbind(max_min, profile)
profile <- df_p[c("Max", "Min", name), ]
colnames(profile) <- colnames(df_radial)[2:12]
profile <- profile %>% 
  rename("Standards &\nTechnology" = "Standards & Technology"  ,"Crime &\nJustice" = "Crime & Justice" , "Defence & Armed\nForces" = "Defence & Armed Forces", "Education &\nLearning" ="Education & Learning" ,"Economy &\nFinancial Services" =  "Economy & Financial Services" ,"Immigration\n& Borders" = "Immigration & Borders" ,"Welfare &\nSocial Housing"="Welfare & Social Housing","Health & \nHealthcare" ="Health & Healthcare","Environment\n& Energy"="Environment & Energy","European Union &\nForeign Affairs"="European Union & Foreign Affairs",'Parliamentary\nProcedures' = 'Parliamentary Procedures', )

# Create PNG
png(filename = paste0("/Users/jdlilley/Desktop/Data Science and Modelling/Dissertation/DataBase/2006+_LT_Diagram/",name,title,".png",collapse = ''), width = 1200, height = 1000)
create_beautiful_radarchart_simple(profile, caxislabels = c(0, 0.25, 0.5,0.75,1),vlcex = 1.8,cex.main = 4,plwd = 5)
dev.off()
}

# View Visual
create_beautiful_radarchart_simple(profile, caxislabels = c(0, 0.25, 0.5,0.75,1), vlcex = 0.6)


## Create 5-plot
#### Prep Data == Plot 5 Radar Plots of Politician #####
# df
Years <- year(as.Date(df$date_value[1], format = "%d/%m/%Y"))-year(as.Date(df$date_value[nrow(df)], format = "%d/%m/%Y"))
Num_graphs <- floor(Years/3.333333) ##### If Greater than 5 new radial added
n_div <- round(nrow(df)/5,0)
# assign df
for(i in 1:Num_graphs){
  df_name <- paste0('df_mp',i,collapse = '')
  if(i == 1){
    assign(df_name, df_mp[1:n_div,])
  }
  if (i > 1 && i < 5){
  assign(df_name, df_mp[(n_div*(i-1)):(n_div*i),])
  }
  if(i==5){
    assign(df_name, df_mp[(n_div*(i-1)):nrow(df_mp),])
  }
}
# Create Radial df for each graph
n <- 0
for (i in 1:Num_graphs){
  df_mp_in <- get(paste0('df_mp',i,collapse = ''))
  # Convert to mp issue count per mp
for (mp in df_radial$value){
  mp_n <- which(colnames(df_mp_in) == mp)
  for (clus in colnames(df_radial[2:12])){
    count <- 0
    for (div in 1:nrow(df_mp_in)){
      if (df_mp_in$cluster[div] == clus){
        count <- count + (df_mp_in[div,mp_n] * df_mp_in$sentiment[div])
      }
    }
    df_radial[which(df_radial$value == mp) ,which(colnames(df_radial) == clus)] <- count
  }
}

# Normalise the 
df_radial_norm <- df_radial
for (col in 2:12){
  for (row in 1:nrow(df_radial)){
    df_radial_norm[row,col] <- (df_radial[row,col] - min(df_radial[,col]))/(max(df_radial[,col])-min(df_radial[,col]))
  }
}
assign(paste0('df_rad_',i,collapse = ''),df_radial_norm)
n <- n+1
print(n)
}

# Year on year Radar DF
for (i in df_rad_FULL$value){
rownames(max_min) <- c("Max", "Min")
name <- i
mp <- which(df_rad_FULL$value == name)
profile <-  data.frame(df_rad_FULL[mp,])
rownames(profile) <- df_rad_FULL[mp,1]
profile <- profile[,2:12]

profile_5 <-  data.frame(df_rad_1[mp,])
rownames(profile_5) <- paste(df_rad_1[mp,1],'2021-2023',collapse = '')
profile_5 <- profile_5[,2:12]

profile_4 <-  data.frame(df_rad_2[mp,])
rownames(profile_4) <- paste(df_rad_2[mp,1],'2016-2021',collapse = '')
profile_4 <- profile_4[,2:12]

profile_3 <-  data.frame(df_rad_3[mp,])
rownames(profile_3) <- paste(df_rad_3[mp,1],'2012-2016',collapse = '')
profile_3 <- profile_3[,2:12]

profile_2 <-  data.frame(df_rad_4[mp,])
rownames(profile_2) <- paste(df_rad_4[mp,1],'2009-2012',collapse = '')
profile_2 <- profile_2[,2:12]

profile_1 <-  data.frame(df_rad_5[mp,])
rownames(profile_1) <- paste(df_rad_5[mp,1],'2006-2009',collapse = '')
profile_1 <- profile_1[,2:12]

# Bind the variable ranges to the data
df_yearly <- rbind(max_min, profile_1,profile_2,profile_3,profile_4,profile_5)
# profile <- df[c("Max", "Min", name), ]
colnames(df_yearly) <-  colnames(df_radial)[2:12]
df_yearly <- df_yearly %>% 
  rename("Standards &\nTechnology" = "Standards & Technology"  ,"Crime &\nJustice" = "Crime & Justice" , "Defence & Armed\nForces" = "Defence & Armed Forces", "Education &\nLearning" ="Education & Learning" ,"Economy &\nFinancial Services" =  "Economy & Financial Services" ,"Immigration\n& Borders" = "Immigration & Borders" ,"Welfare &\nSocial Housing"="Welfare & Social Housing","Health & \nHealthcare" ="Health & Healthcare","Environment\n& Energy"="Environment & Energy","European Union &\nForeign Affairs"="European Union & Foreign Affairs",'Parliamentary\nProcedures' = 'Parliamentary Procedures', )

#### Plot 5 Radar Plots of Politician #####
png(filename = paste0("/Users/jdlilley/Desktop/Data Science and Modelling/Dissertation/DataBase/3Year_LT_Diagram/",name,".png",collapse = ''), width = 1680, height = 1059)
opar <- par() 
# Define settings for plotting in a 3x4 grid, with appropriate margins:
par(mar = rep(1,4))
par(mfrow = c(2,3))
par(mai = c(0, 0, 0.5, 0))
# Produce a radar-chart for each student
for (i in 3:nrow(df_yearly)) {
  create_beautiful_radarchart_comp(df_yearly[c(1:2,i-1, i), ],vlcex = 1.8,cex.main = 3.5,cex.axis = 5,plwd = 6,plty = 1,caxislabels = c(0, 0.25, 0.5,0.75,1),cglwd = 2) #,caxislabels = c(0, 0.25, 0.5,0.75,1),axis.text = 2 
}
# Restore the standard par() settings
par <- par(opar) 
dev.off()
}

# ggsave(file= paste0("/Users/jdlilley/Desktop/Data Science and Modelling/Dissertation/DataBase/3Year_LT_Diagram/Comp_",name,".png",collapse = ''),plot = last_plot())



########### Animate Gif (radar plot) ################
## DF
# set var
end <- nrow(df_mp) - 250
start <- nrow(df_mp)
# How many divisions
jump <- 10
No <- 1
# Loop for every div
while (end > 0){
  df_mp_temp <- df_mp[start:end,]
  
  for (mp in df_radial$value){
    mp_n <- which(colnames(df_mp_temp) == mp)
    for (clus in colnames(df_radial[2:12])){
      count <- 0
      for (div in 1:nrow(df_mp_temp)){
        if (df_mp_temp$cluster[div] == clus){
          count <- count + (df_mp_temp[div,mp_n] * df_mp_temp$sentiment[div])
        }
      }
      df_radial[which(df_radial$value == mp) ,which(colnames(df_radial) == clus)] <- count
    }
  }
  
  df_radial_norm <- df_radial
  for (col in 2:12){
    for (row in 1:nrow(df_radial)){
      df_radial_norm[row,col] <- (df_radial[row,col] - min(df_radial[,col]))/(max(df_radial[,col])-min(df_radial[,col]))
    }
  }
  
  write.csv(df_radial_norm, paste0("/Users/jdlilley/Desktop/Data Science and Modelling/Dissertation/DataBase/mp_gif_df/df_animate_",No,".csv",collapse = ''), row.names=FALSE)
  start <- start - jump
  end <- end - jump
  No <- No+1
  Num <- No -1
  print(No)
}

# #### Get Year #####
# set df
date_div <- as_tibble(df$about)
date_div$date <- 'x'

## Extract date from title
for (i in 1:nrow(date_div)){
  filename <- df$file.name[i]
  date_div$date[i] <- stringr::str_extract(filename, "\\d{4}-\\d{2}-\\d{2}")
}

## Extract start & end date
# set var & df
No <- 1
file_date <- as_tibble(rep('x',Num))
colnames(file_date) <- 'file'
file_date$start <- 'x'
file_date$end <- 'x'
end <- nrow(df_mp) - 250
start <- nrow(df_mp)

for (i in 1:Num){
  file_date$file[i] <- paste0("df_animate_",i,".csv",collapse = '')
  file_date$start[i] <- date_div$date[start]
  file_date$end[i] <- date_div$date[end]
  start <- start - jump
  end <- end - jump
}
## Extract only year start / end date for each animation df
file_Year <- file_date
for (i in 1:Num){
  file_Year$start[i] <- year(as.Date(file_Year$start[i]))
  file_Year$end[i] <- year(as.Date(file_Year$end[i]))
}

### Create png
# Define the variable ranges: maximum and minimum
max_min <- data.frame(
  'European Union & Foreign Affairs' = c(1, 0), "Parliamentary Procedures" = c(1, 0), "Environment & Energy" = c(1,0),
  "Health & Healthcare" = c(1, 0), "Welfare & Social Housing" = c(1, 0), "Education & Learning" = c(1, 0),
  "Economy & Financial Services" = c(1, 0), "Standards & Technology" = c(1, 0), "Crime & Justice" = c(1, 0), "Defence & Armed Forces" = c(1,0),
  'Immigration & Borders' = c(1, 0)
)
rownames(max_min) <- c("Max", "Min")

# Loop through mp
for (mp in df_rad_FULL$value){
mp_n <- which(df_rad_FULL$value == mp)
# create png in temp file
setwd("~/Desktop/Data Science and Modelling/Dissertation/DataBase/mp_gif_df")
for (i in 1:Num){
  df_rad_year <- read.csv(paste0('df_animate_',i,'.csv',collapse = ''))
  profile <-  data.frame(df_rad_year[mp_n,])
  rownames(profile) <- mp
  
  # Bind the variable ranges to the data
  df_temp <- max_min
  df_temp[3,] <- profile[2:12]
  rownames(df_temp)[3] <- mp
  df_temp <- df_temp %>% 
    rename("Standards &\nTechnology" = "Standards...Technology"  ,"Crime &\nJustice" = "Crime...Justice" , "Defence & Armed\nForces" = "Defence...Armed.Forces", "Education &\nLearning" ="Education...Learning" ,"Economy &\nFinancial Services" =  "Economy...Financial.Services" ,"Immigration\n& Borders" = "Immigration...Borders" ,"Welfare &\nSocial Housing"="Welfare...Social.Housing","Health & \nHealthcare" ="Health...Healthcare","Environment\n& Energy"="Environment...Energy","European Union &\nForeign Affairs"="European.Union...Foreign.Affairs",'Parliamentary\nProcedures' = 'Parliamentary.Procedures', )

  title = paste(rownames(df_temp)[3],'voting profile',colapse = '')
  subtitle = paste(file_Year$start[which(file_Year$file == paste0('df_animate_',i,'.csv',collapse = ''))],'-',file_Year$end[which(file_Year$file == paste0('df_animate_',i,'.csv',collapse = ''))],collapse = '')
  
  # Saving the graph as a PNG file
  png(filename = paste0("/Users/jdlilley/Desktop/Data Science and Modelling/Dissertation/DataBase/temp_png/df_animate_",i,".png",collapse = ''), width = 1000, height = 850)
  create_beautiful_radarchart(df_temp,title,subtitle, caxislabels = c(0, 0.25, 0.5,0.75,1))
  dev.off()
}

## Create gif
setwd("~/Desktop/Data Science and Modelling/Dissertation/DataBase/temp_png")
png_files <- list.files()

# List all PNG files in the directory
png_files <- list.files(pattern = "*.png")
png_files <-  mixedsort(png_files)

gifski(png_files, paste0("/Users/jdlilley/Desktop/Data Science and Modelling/Dissertation/DataBase/mp_gif/",rownames(df_temp)[3],'.gif',collapse=''), width = 1000, height = 850, delay = 0.1)
}


########### Polarisation ################
## Animation Polar - Create df
# set var
end <- nrow(df_mp) - 250
start <- nrow(df_mp)
jump <- 10
No <- 1
# Loop for every div
while (end > 0){
  df_mp_temp <- df_mp[start:end,]
  columns <- c()
  for (i in 4:length(df_mp_temp)){
    if (sum(abs(df_mp_temp[,i])) == 0)
      columns <- c(columns, i)
  }
  df_mp_temp <- df_mp_temp[,-columns]
  #Create Radial
  # Overview df
  df_radial <- as_tibble(colnames(df_mp_temp[4:length(df_mp_temp)]))
  df_radial$"European Union & Foreign Affairs" <- 0
  df_radial$"Parliamentary Procedures" <- 0
  df_radial$"Environment & Energy" <- 0
  df_radial$"Health & Healthcare"  <- 0
  df_radial$"Welfare & Social Housing"  <- 0
  df_radial$"Education & Learning"    <- 0
  df_radial$"Economy & Financial Services" <- 0
  df_radial$"Standards & Technology"  <- 0
  df_radial$"Crime & Justice"  <- 0
  df_radial$"Defence & Armed Forces" <- 0
  df_radial$'Immigration & Borders' <- 0
  for (mp in df_radial$value){
    mp_n <- which(colnames(df_mp_temp) == mp)
    for (clus in colnames(df_radial[2:12])){
      count <- 0
      for (div in 1:nrow(df_mp_temp)){
        if (df_mp_temp$cluster[div] == clus){
          count <- count + (df_mp_temp[div,mp_n] * df_mp_temp$sentiment[div])
        }
      }
      df_radial[which(df_radial$value == mp) ,which(colnames(df_radial) == clus)] <- count
    }
  }
  df_radial_norm <- df_radial
  for (col in 2:12){
    for (row in 1:nrow(df_radial)){
      df_radial_norm[row,col] <- (df_radial[row,col] - min(df_radial[,col]))/(max(df_radial[,col])-min(df_radial[,col]))
    }
  }
  write.csv(df_radial_norm, paste0("/Users/jdlilley/Desktop/Data Science and Modelling/Dissertation/DataBase/Polar_df_5/df_animate_",No,".csv",collapse = ''), row.names=FALSE)
  start <- start - jump
  end <- end - jump
  No <- No+1
  Num <- No -1
  print(No)
}

## Create plots
setwd("~/Desktop/Data Science and Modelling/Dissertation/DataBase/Polar_df")
for (i in 1:Num){
  df_rad_year <- read.csv(paste0('df_animate_',i,'.csv',collapse = ''))
  df_BP <- data.frame(df_rad_year)
  df_bar_ <- c(df_BP[,2], df_BP[,3], df_BP[,4], df_BP[,5], df_BP[,6], df_BP[,7], df_BP[,8], df_BP[,9], df_BP[,10], df_BP[,11], df_BP[,12])
  df_bar <- as.double(df_bar_)
  df <- data.frame(counts = table(cut(t(data.frame(x = df_bar)), breaks=seq(0, 1, by=0.047619047619048))))
  titles = paste(file_Year$start[which(file_Year$file == paste0('df_animate_',i,'.csv',collapse = ''))],'-',file_Year$end[which(file_Year$file == paste0('df_animate_',i,'.csv',collapse = ''))],collapse = '')
  # Saving the graph as a PNG file
  make_bar(df,titles)
  ggsave(file=paste0("/Users/jdlilley/Desktop/Data Science and Modelling/Dissertation/DataBase/Polar_png/df_animate_",i,".png",collapse = ''))
}

## Create gif
# find files
setwd("~/Desktop/Data Science and Modelling/Dissertation/DataBase/Polar_png")
png_files <- list.files()
# List all PNG files in the directory
png_files <- list.files(pattern = "*.png")
# Order normally
png_files <-  mixedsort(png_files)
# create gif
gifski(png_files, "/Users/jdlilley/Desktop/Data Science and Modelling/Dissertation/DataBase/Polar_gif/Polar_2006_2023.gif",'.gif', width = 3374, height = 2117, delay = 0.45)


#### Polarisation indicator
setwd("~/Desktop/Data Science and Modelling/Dissertation/DataBase/Polar_df")
# Run AD test over df
AD_test <- c()
data.frame(AD_test)
for (i in 1:Num){
  df_rad_year <- read.csv(paste0('df_animate_',i,'.csv',collapse = ''))
  df_BP <- data.frame(df_rad_year)
  df_bar_ <- c(df_BP[,2], df_BP[,3], df_BP[,4], df_BP[,5], df_BP[,6], df_BP[,7], df_BP[,8], df_BP[,9], df_BP[,10], df_BP[,11], df_BP[,12])
  AD_test[i] <- as.double(ad.test(df_bar_)[1])
}
# set x-axis dates
file_date$mid <- as.Date(file_date[[i,2]])
for (i in 1:Num){
  startdate <- as.Date(file_date[[i,2]])
  enddate <- as.Date(file_date[[i,3]])
  file_date$mid[i] <- mid_date(startdate, enddate)
}
# create df
x_date <- file_date$mid
df <- data.frame(
  date = x_date,
  value = AD_test
)
ggplot(df, aes(x = date, y = value)) +
  geom_line() +
  xlab("Mean Year") +
  ylab("AD-test Scores")+
  ggtitle("Polarisation measured by AD test Normal Dist ")+
  theme(plot.title = element_text(hjust = 0.5))



