# ---------------------------------------------
# KOREA NEWS COVID-19 RESPONSE EMOTIONALITY ANALYSIS 
# 
# Details:      We are analysing the articles of the English speaking newspaper in Korea (Korea Times, Korea Herald) from January 01 to May 23 2020 
#               containing the words "covid-19" or "coronavirus" for their positivity/negativitiy and their emotionality.
# Copyright:    © 2020 Lennart Schulze
# Author        Lennart Schulze, Group 5, Big Data Analysis Class 2020-01, Sungkyunkwan University
# 
# imports:      see packages
# Usage:        source(*filename*)
# History:      1.0.0 2020-05-30
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
# library(KoNLP)
library(dplyr) # data processing
library(plyr)
library(stringr) 
library(reshape2) # melting (necessary for plotting)



# ==== FILES ====

# - lexicons

# Stanford SocialSent
pos_neg_lex=read.table("./lexicons/worldnews.tsv", header=F)
head(pos_neg_lex)

# NRC Emotion Intensity Lexicon
emo_lex=read.table("./lexicons/NRC-Emotion-Intensity-Lexicon-v1.txt", sep="\t", header=T)


# - news articles 

kh_virus <- read.csv("./articles/articles_koreaherald_coronavirus_date.csv", header = T)
kh_covid <- read.csv("./articles/articles_koreaherald_covid19_date.csv", header = T)
kt_virus <- read.csv("./articles/articles_koreatimes_coronavirus.csv", header = T, sep=";")
kt_covid <- read.csv("./articles/articles_koreatimes_covid19.csv", header = T, sep=";")


# - prepare sample for coding

# write.csv(head(kh_virus, n=50), file="./articles/sample1.csv")
sample1 <- read.csv("./articles/sample1.csv", header = T)
sample1 <- sample1[-1] # drop first column

# write.csv(head(kh_covid, n=50), file="./articles/sample2.csv")
sample2 <- read.csv("./articles/sample2.csv", header = T)
sample2 <- sample2[-1] # drop first column



# ==== PREPROCESSING ====

# - date format change (for Korea Herald Articles)
date_transform = function(row){
  date = unlist(row["DATE"])
  date <- gsub(",","", date) %>% strsplit(" ")
  date <- unlist(date)
  year = date[3]
  m = match(date[1], month.abb) # lookup index from month.name constant
  m = sprintf("%02d", m) # extend to 2 digit
  day = date[2] 
  day = sprintf("%02d", as.numeric(day)) # extend to 2 digit
  date = paste(year, m, day, sep="-")
  row["DATE"] = date
  return(row)
}

# - write transformed data (only once)
# sample <- t(apply(sample, 1, date_transform))
# write.csv(t(apply(kh_virus, 1, date_transform)), file="./articles/articles_koreaherald_coronavirus_date.csv", row.names=F, sep=";")
# write.csv(t(apply(kh_covid, 1, date_transform)), file="./articles/articles_koreaherald_covid19_date.csv", row.names=F, sep=";")


# - aggreagate articles from all sources in single data frame
articles = rbind(kh_virus, kh_covid, kt_virus, kt_covid)
articles = distinct(articles) # eliminate copies (articles can contain both 'coronvirus', and 'covid-19')
articles <- articles[order(articles$DATE), ] # order by date
rownames(articles)<-NULL # row ids to default
articles <- articles[,c(1, 6)] # project only date and content
nrow(articles)


# - group articles per day
# toString(subset(articles, DATE=="2020-05-20")[,2])
# aggregates <- articles %>%  group_by(DATE) %>%  summarise(TXT = toString(CONTENT)) %>% ungroup()

aggregates <- aggregate(CONTENT~DATE, articles, toString) # aggregate articles per day on date column, concetanating string with "," seperator
# print(aggregates)


# - prepare results data frame, copying the dates
results = data.frame(DATE=aggregates[1])
emotions = c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust")
results[c("POS", "NEG", "OBJ", emotions)]=NA



# ==== ANALYSIS ====

# function score.sentiment 
score.sentiment = function(articles, .progress='none') { 
  # generate scores for each row in articles tables (represents one date)
  values_list = apply(articles,1,
                 function(row, r=results) 
                 { 
                   # PREPROCESSING OF TEXT
                   
                   # translate punctuation to spaces (sometimes full-stops used without space between sentences)
                   date = row[[1]]
                   article = row[2]
                   cat(date,"\n")
                   # select only latin sign words (excludes numbers (even '-19' in covid), control signs) (keeps the spaces for separation)
                   article = gsub("[^A-z|\\s]", " ", article)
                   # lowercase 
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
                   
                   emo_values = vector(mode="numeric", length = length(emotions)) 
                   
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
                      emo_occurances = which(emo_lex[,1]==word)
                      for (i in range(length(emo_occurances))){
                        emo_index = emo_occurances[i]
                        val = emo_lex[emo_index, 3]
                        category = emo_lex[emo_index, 2]
                        # print(word)
                        # print(match(category, emotions))
                        emo_values[match(category, emotions)] = emo_values[match(category, emotions)] + val
                      }
                   }
                   objectivity = 1 - ((pos_count+neg_count)/length(words)) # objectivity = ratio of subjective words to text length
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
words=score.sentiment(aggregates)
str(words)
results[,-1] = as.data.frame(words) # assign values to result data frame


write.csv(results, file="./articles/results_kh.csv", row.names=F, sep=";")
results = read.csv(file="./articles/results_kh.csv", row.names = NULL, header=T)
print(results)



# ==== PLOT ====

results_melt = melt(results[c(1,2:4)], id=c("DATE")) # melt values into single column data frame
head(results_melt)
ggplot(results_melt) + geom_line(aes(x=as.Date(DATE), y=value, colour=variable)) + 
  theme_bw() +
  scale_color_brewer(palette="Paired") +
  # theme(
  #   axis.text.x=element_text(angle = -90, vjust = 1, size = 6, hjust = 1)
  # ) +
  ggtitle("Emotionality in Korean News Articles on COVID-19") +
  xlab("Date") +
  ylab("Index")

  
  

# ==== OLD ====

#pos.words=scan("positive-words.txt", what="character", comment.char = ";")
#neg.words=scan("negative-words.txt", what="character", comment.char = ";")

#pratice


# #step1) Start file input code
# 
# ##When you want to put it as a txt file
# ##Example execution with obama.txt
# 
# conn=file("obama.txt",open="r")
# line=readLines(conn)
# for (i in 1:length(line)){
#   print(line[i])
# }
# close(conn)
# 
# #Notice before executing the sentence below: If the amount of txt is too large, the function to extract English words is when the pc's ram is low.
# #It may take a long time and may go down.
# 
# X2<-sapply(line, extractNoun,USE.NAMES = T)
# # X2<-sapply(line, extractNoun,USE.NAMES = F)
# 
# #unlist
# c_vector<-unlist(X2)
# 
# #step2) Preprocessing code
# #Special character and blank processing
# c_vector<-str_replace_all(c_vector,"[^[:alpha:]]","")
# 
# #Save only non-blank
# c_vector<-c_vector[c_vector!=""]
# 
# #step3) Check the frequency of desired articles and non-verbs (원하는 관사와 비동사 빈도 확인)
# ###a, the, is, am, are, be
# # table (c_vector) dataframe conversion
# X3<-as.data.frame(table(c_vector))
# X3[X3$c=="a"|X3$c=="is"|X3$c=="the"|X3$c=="am"|X3$c=="are"|X3$c=="be",]
# 
# 
# # [Preprocessing still in progress...]
# ###a, the, is, am, are
# #Convert the articles or non-verbs you want to remove to blanks. (제거하고 싶은 관사나 비동사를 공백으로 변환함)
# c_vector<-gsub("is","",c_vector)
# c_vector<-gsub("the","",c_vector)
# c_vector<-gsub("am","",c_vector)
# c_vector<-gsub("are","",c_vector)
# 
# # The Number of Rows/Columns
# NROW(c_vector)
# 
# #Extract only 2 digits or more (2자리이상만 추출)
# c_vector<-Filter(function(x){nchar(x)>2},c_vector)
# 
# 
# # Save as txt (txt로 저장)
# write(c_vector,"ObamaFilter.txt")
# H1<-read.table("ObamaFilter.txt")
# 
# #End of preprocessing (전처리 종료)
# str(H1)
# wordcount<-table(H1)
# H1
# #step4) Emotion score calculation (감정점수 계산)
# results <- score.sentiment(H1$V1, pos.words, neg.words)
# H1$score<-results$score
# H1_mean<-mean(H1$score)
# View(H1_mean)
# 
# 
# H1_mean
# #for example) 0.02071823
# #obama
# 
# #H1_mean = Score of line sentence (H1_mean = line 문장의 감정점수)
# #H1_mean이 감정점수라고 볼 수 있음 (H1_mean이 감정점수라고 볼 수 있음)
# #1 point per 1 positive word-1 point per 1 negative word (ex: 긍정단어 1개당 1점 부정단어 1개당 -1점)
# #The maximum value is between -1 and 1. (최대값은 -1 ~ 1 사이의 값을 가지게 됨)
# 
# 
# # rm(list=ls())
# #Initialization for resource recovery (리소스 회복을 위한 초기화)
# 
# 
# ##Start trump example
# 
# conn=file("trump.txt",open="r")
# line=readLines(conn)
# for (i in 1:length(line)){
#   print(line[i])
# }
# close(conn)
# 
# 
# 
# X2<-sapply(line, extractNoun,USE.NAMES = T)
# 
# 
# c<-unlist(X2)
# 
# 
# 
# X3<-as.data.frame(table(c))
# X3[X3$c=="a"|X3$c=="is"|X3$c=="the"|X3$c=="am"|X3$c=="are"|X3$c=="be",]
# 
# 
# ###a, the, is, am, are, be
# 
# 
# 
# c<-Filter(function(x){nchar(x)>2},c)
# 
# #Special character and blank processing (특수문자 공백처리)
# #res<-str_replace_all(c,"[^[:alpha:]]","")
# res<-c
# #Save only non-blank (공백이 아닌것만 저장)
# res<-res[res!=""]
# 
# ###a, the, is, am, are
# #Convert the articles or non-verbs you want to remove to blanks (제거하고 싶은 관사나 비동사를 공백으로 변환함)
# res<-gsub("is","",res)
# res<-gsub("the","",res)
# res<-gsub("am","",res)
# res<-gsub("are","",res)
# 
# NROW(res)
# 
# # If unnecessary words are blanked out and saved as txt,
# # Comes in with no spaces.
# #불필요한 단어를 공백처리하고 txt로 저장후 불러들이면 
# # 공백이 없어진채로 들어오게됨.
# write(res,"Trump_temp.txt")
# H1_Trump<-read.table("Trump_temp.txt")
# 
# 
# str(H1_Trump)
# wordcountTrump<-table(H1_Trump)
# View(H1_Trump)
# 
# 
# #Repeat the score.sentiment function above.
# #위에 있는 score.sentiment 함수를 다시 수행하세요.
# results_Trump <- score.sentiment(H1_Trump$V1, pos.words, neg.words)
# H1_Trump$score<-results_Trump$score
# H1_Tmean<-mean(H1_Trump$score)
# View(H1_Tmean)
# 
# H1_Tmean
# #0.04276069
# #trump
# 
# 
# obama_trump_score<-c(H1_mean,H1_Tmean)
# 
# windows()
# barplot(obama_trump_score, col=rainbow(2), 
#         xlab = "Speaker (Red: Obama, Blue: Trump)", ylab = "Speech affirmation")
# title(main = "Positive comparison (Obama vs. Trump), 긍정도 비교(Obama vs. Trump)", font = 4)
# 
# #Comparing Obama and Trump's speech, we can see that Trump mentioned more positive words.
# #오바마와 트럼프의 연설문을 비교한 결과 트럼프가 긍정적인단어를 더 많이 언급한 것을 알 수 있음.
# 
# #If you replace it with other celebrities like # steve.txt, bill.txt, etc., you can analyze the same result positively.
# #steve.txt, bill.txt 등과 같이 다른 유명 인사문으로 바꿔서 넣으면 동일한 결과를 긍정도 분석가능합니다.
# 
# #[Team Project][Due date: June. 1st, Monday] Big Data Analysis about "Coronavirus"]
# #Find the coronavirus data among articles in the New York Times site and save it as a txt file and compare the frequency of positive and negative words with respect to the coronavirus.
# 
