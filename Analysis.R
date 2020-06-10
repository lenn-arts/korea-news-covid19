# ---------------------------------------------
# KOREA NEWS COVID-19 RESPONSE EMOTIONALITY ANALYSIS 
# 
# Details:      We are analysing the articles of the English speaking newspaper in Korea (Korea Times, Korea Herald) from January 01 to May 23 2020 
#               containing the words "covid-19" or "coronavirus" for their positivity/negativitiy and their emotionality.
# Copyright:    © 2020 Lennart Schulze
# Author        Lennart Schulze, Ee Su-jin; Group 5, Big Data Analysis Class 2020-01, Sungkyunkwan University
# 
# imports:      see packages
# Usage:        source(*filename*)
# History:      1.1.0 2020-06-05
# --------------------------------------------- 



# Initialization
rm(list=ls())
getwd()

# ==== PACKAGES ====

# # java, rJava installation
# install.packages("multilinguer")
# install.packages("ggplot2")
# library(multilinguer)
# install_jdk()
# 
# # Install dependency packages
# install.packages(c('stringr', 'hash', 'tau', 'Sejong', 'RSQLite', 'devtools'), type = "binary")
# 
# # Install github version
# install.packages("remotes")
# remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))


library(ggplot2) # plotting
# library(rJava) 
# library(KoNLP)
library(dplyr) # data processing
library(plyr)
library(stringr) 
library(reshape2) # melting (necessary for plotting)
library(cowplot) # arrange multiple ggplots on same page



# ==== FILES ====

# - Coronavirus data
corona_growth<-read.csv('covid19_cases/kr_daily.csv', sep=',' , header = TRUE)

# - Lexicons

# Stanford SocialSent
pos_neg_lex=read.table("./lexicons/worldnews.tsv", header=F)
head(pos_neg_lex)

# NRC Emotion Intensity Lexicon
emo_lex=read.table("./lexicons/NRC-Emotion-Intensity-Lexicon-v1.txt", sep="\t", header=T)
head(emo_lex)


# - News articles 
# kh_virus <- read.csv("./articles/articles_koreaherald_coronavirus.csv", header = T, sep=";")
# kh_covid <- read.csv("./articles/articles_koreaherald_covid19.csv", header = T, sep=";")
# kh_virus <- read.csv("./articles/articles_koreaherald_coronavirus_date.csv", header = T)
# kh_covid <- read.csv("./articles/articles_koreaherald_covid19_date.csv", header = T)
# kt_virus <- read.csv("./articles/articles_koreatimes_coronavirus.csv", header = T, sep=";")
# kt_covid <- read.csv("./articles/articles_koreatimes_covid19.csv", header = T, sep=";")


# - Prepare sample for coding

# write.csv(head(kh_virus, n=50), file="./articles/sample1.csv")
sample1 <- read.csv("./articles/sample1.csv", header = T)
sample1 <- sample1[-1] # drop first column
# write.csv(head(kh_covid, n=50), file="./articles/sample2.csv")
sample2 <- read.csv("./articles/sample2.csv", header = T)
sample2 <- sample2[-1] # drop first column



# ==== PREPROCESSING ====

# - Date format change (for Korea Herald Articles)
date_transform = function(row){
  date = unlist(row["DATE"])
  date <- gsub(",","", date) %>% strsplit(" ")
  date <- unlist(date)
  year = date[3] # get year part of date string
  m = match(date[1], month.abb) # lookup month index from month.name constant
  m = sprintf("%02d", m) # extend to 2 digit
  day = date[2]  # get day part date string
  day = sprintf("%02d", as.numeric(day)) # extend to 2 digit
  date = paste(year, m, day, sep="-")
  row["DATE"] = date
  return(row)
}

# - Write transformed data (only once)
# sample <- t(apply(sample, 1, date_transform))
# write.csv(t(apply(kh_virus, 1, date_transform)), file="./articles/articles_koreaherald_coronavirus_date.csv", row.names=F, sep=";")
# write.csv(t(apply(kh_covid, 1, date_transform)), file="./articles/articles_koreaherald_covid19_date.csv", row.names=F, sep=";")
# kh_virus <- read.csv("./articles/articles_koreaherald_coronavirus_date.csv", header = T)
# kh_covid <- read.csv("./articles/articles_koreaherald_covid19_date.csv", header = T)


# - Aggreagate articles from all sources in single data frame
articles = rbind(sample1, sample2)  # sample
# articles = rbind(kh_virus, kh_covid, kt_virus, kt_covid)  # real data
articles = distinct(articles) # eliminate copies (articles can contain both 'coronvirus', and 'covid-19')
articles$DATE = as.Date(articles$DATE) # read dates (strings) as Date type -> necessary for sorting
articles <- articles[order(articles$DATE), ] # order by date
rownames(articles)<-NULL # row ids to default
articles <- articles[,c(1, 6)] # project only date and content
cat("number of articles: ",nrow(articles))



# - Group articles per day
# toString(subset(articles, DATE=="2020-05-20")[,2])
# aggregates <- articles %>%  group_by(DATE) %>%  summarise(TXT = toString(CONTENT)) %>% ungroup()

aggregates <- aggregate(CONTENT~DATE, articles, toString) # aggregate articles per day on date column, concetanating string with "," seperator
tail(aggregates$DATE)
aggregates <- aggregates[order(aggregates$DATE), ] # order by date
tail(aggregates$DATE)


# - Prepare results data frame, copying the dates
results = data.frame(DATE=aggregates[1])
emotions = c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust")
results[c("POS", "NEG", "OBJ", emotions)]=NA



# ==== ANALYSIS ====

sentiment_scores = function(articles, .progress='none') { 
  # generate scores for each row in articles tables (represents one date)
  values_list = apply(articles,1,
                 function(row, r=results) 
                 { 
                   # PREPROCESSING OF TEXT
                   
                   # translate punctuation into spaces (sometimes full-stops used without space between sentences)
                   date = row[[1]]
                   article = row[2]
                   cat(date,"\n")
                   # select only latin sign words (excludes numbers (even '-19' in covid), control signs) (keeps the spaces for separation)
                   article = gsub("[^A-z|\\s]", " ", article)
                   # transform words to lowercase 
                   article = tolower(article)
                   # # remove digits
                   #article = gsub('\\d+', " ", article) 
                   article = gsub("[[:punct:]]", " ", article)
                   # delete words that have only 1 character
                   article = gsub("(\\s.\\s){1}", " ", article) 
                   # remove unnecessary spaces 
                   article = gsub("[ \t]{2,}", " ", article) 
                   article = gsub("^\\s+|\\s+$", "", article)
                  
                   # # remove control characters 
                   # article = gsub("[[:cntrl:]]", "", article) 
                   # # remove html links 
                   # article = gsub("http\\w+", " ", article) 
                   
                   # get words list
                   word.list = str_split(article , "\\s+") 
                   typeof(word.list)
                   words = unlist(word.list)
                   
                   
                   # ANALYSIS FOR 1) POSITITVY/NEGATIVITY, 2) EMOTIONS
                   pos_score = 0
                   neg_score = 0
                   pos_count = 0
                   neg_count = 0
                   
                   emo_values = vector(mode="numeric", length = length(emotions)) # values for emotion scores
                   
                   # iterate over all words from aggreagated articles text of the current date
                   for (word in words){
                      pos_index = which(pos_neg_lex[,1]==word)
                      if (length(pos_index) > 0){
                        val = pos_neg_lex[pos_index, 2]
                        #print(val)
                        if (val >= 0){
                          pos_score = pos_score + val
                          pos_count = pos_count + 1
                        }
                        else if (val<0){
                          neg_score = neg_score + (-val)
                          neg_count = neg_count + 1
                        }
                      }
                      emo_occurances = which(emo_lex[,1]==word) # find row ids of all occurances of the word in emo lexicon
                      for (i in range(length(emo_occurances))){ # for each occurance (usually 8), add the score for the respective emotion
                        emo_index = emo_occurances[i]  # restore row id
                        val = emo_lex[emo_index, 3]     # get value for emotion by row id
                        category = emo_lex[emo_index, 2]  # find type of emotion by row id
                        # print(word)
                        # print(match(category, emotions))
                        emo_values[match(category, emotions)] = emo_values[match(category, emotions)] + val # add to respective value in emotions vector
                      }
                   }
                   objectivity = 1 - ((pos_count+neg_count)/length(words)) # objectivity = inverse ratio of subjective words to text length
                   positivity = pos_score/length(words)
                   negativity = neg_score/length(words)
                   pos_values = c(positivity, negativity, objectivity)
                   print(c(positivity, negativity, objectivity))
                   
                   print(emo_values)
                   print(emo_values/length(words))
                   emo_values = emo_values/length(words)
                   
                   values = c(pos_values, emo_values)
                   return(values)
                 })
  # matrix with scores for each day
  return(t(values_list)) 
} 

# ---> TURN ON to calculate (takes very long, thus saved in file)
# sentiments=sentiment_scores(aggregates)
# str(sentiments)
# results[,-1] = as.data.frame(sentiments) # assign values to result data frame (keeping date column)
# 
# write.csv(results, file="./articles/results.csv", row.names=F, sep=";")
results = read.csv(file="./articles/results.csv", row.names = NULL, header=T)
tail(results)
# cat("number of articles processed: ", nrow(num_articles))


# ==== PLOT ====

# for draw the number of confirmed cases increased compared to the previous day
# in this case for similar index set 1 person = 0.0001

# calculate new cases per day
corona_growth_offset<- c(0, corona_growth[-length(corona_growth$confirmed), "confirmed"])  # shift confirmed_case column by 1
corona_growth["new_cases"]=corona_growth$confirmed-corona_growth_offset  # ... to calculate new infections per day

results<-cbind(results, corona_growth$growth)
colnames(results)[13]<-"corona_growth"
results<-results[,c(1,13,2:12)] # for emphasize corona_growth's color, change sequence
# head(results)


# - Create auxilary variables
pos_melt = melt(results[,c(1,3:5)], id=c("DATE"))
emo_melt = melt(results[,c(1,6:13)], id=c("DATE")) # melt values into single column data frame

max_case = max(corona_growth$new_cases)
max_pos = max(pos_melt$value)
max_emo = max(emo_melt$value)

# - Positivity / Negativity
p1 = ggplot()+
  geom_line(pos_melt, mapping = aes(x=as.Date(DATE), y=value, colour = variable, group=variable)) +
  geom_line(corona_growth, lwd=1, mapping = aes(x=as.Date(DATE), y=new_cases/(max_case/max_pos), linetype="daily new cases")) +
  # geom_line(corona_growth, lwd=1, mapping = aes(x=as.Date(DATE), y=growth*2, linetype="confirmed cases growth %")) +
  # geom_line(corona_growth, lwd=1, mapping = aes(x=as.Date(DATE), y=confirmed*6, linetype="confirmed cases growth")) +
  
  theme_bw() +
  scale_color_brewer(palette='Paired') + # color palette used
  scale_linetype_manual(
    name="COVID-19", # legend title
    values=c(5,1)) + # assign linetype style (where linetype set in geom_line(), in order of appearance)
  scale_y_continuous(
    name = "Index", # 1st axis
    sec.axis = sec_axis(~.*(max_case/max_pos), name="Cases")  # 2nd y axis (scaling + name)
  ) + 
  # theme(axis.text.x=element_text(angle = -90, vjust = 1, size = 6, hjust = 1)) +
  ggtitle("Positivity/Negativity in Korean News Articles on COVID-19") +
  labs(color='Sentiment') + # legend title emotions
  xlab("Date") +
  ylab("Index")  # +  facet_wrap(vars(variable))

# - Emotionality
p2 = ggplot()+
  geom_line(emo_melt, mapping = aes(x=as.Date(DATE), y=value, colour = variable, group=variable)) + 
  geom_line(corona_growth, lwd=1, mapping = aes(x=as.Date(DATE), y=new_cases/(max_case/max_emo), linetype="daily new cases"), size=20) +
  theme_bw() +
  scale_color_brewer(palette='Paired') + # color palette used
  scale_linetype_manual(
    name="COVID-19", # legend title
    values=c(5,1)) + # assign linetype style (where linetype set in geom_line(), in order of appearance)
  scale_y_continuous(
    name = "Index", # 1st y axis name
    sec.axis = sec_axis(~.*(max_case/max_emo), name="Cases")  # 2nd y axis (scaling + name)
  ) + 
  ggtitle("Emotionality in Korean News Articles on COVID-19") +
  labs(color='Emotion') + # legend title emotions
  xlab("Date") +
  ylab("Index")  +  facet_wrap(vars(variable))

p1

# - Plot two graphs together
# plot_grid(p1, p2, align="v", ncol=1)


# ==== CORRELATION ====
head(results[c(-1,-2)])
head(corona_growth$new_cases)
nrow(corona_growth)
cor = cor(results[c(-1,-2)], corona_growth$new_cases)
cor = as.data.frame(cor)
colnames(cor) = "cor"
cor["p-value"]<-(sapply(results[c(-1,-2)], function(y) {
  return(cor.test(y, corona_growth$new_cases)$p.value)}
))
cor
nrow(results)

