#Initialization for resource recovery (리소스 회복을 위한 초기화)
rm(list=ls())

getwd()

############ Old packages often don't work as R continues to upgrade
# install.packages("KoNLP")
# install.packages("https://cran.r-project.org/src/contrib/Archive/KoNLP/KoNLP_0.80.2.tar.gz", repos=NULL, type="source", INSTALL_opts=c('--no-lock'))

# java, rJava installation
install.packages("multilinguer")
install.packages("ggplot2")
library(multilinguer)
install_jdk()

# Install dependency packages
install.packages(c('stringr', 'hash', 'tau', 'Sejong', 'RSQLite', 'devtools'), type = "binary")

# Install github version
install.packages("remotes")
remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))




library(ggplot2)
library(KoNLP)
library(plyr)
library(stringr)



# function score.sentiment 
score.sentiment = function(sentences, pos.words, neg.words, .progress='none') 
{ 
  # create simple array of scores with laply 
  scores = laply(sentences, 
                 function(sentence, pos.words, neg.words) 
                 { 
                   # remove punctuation 
                   sentence = gsub("[[:punct:]]", "", sentence) 
                   # remove control characters 
                   sentence = gsub("[[:cntrl:]]", "", sentence) 
                   # remove digits? 
                   sentence = gsub('\\d+', '', sentence) 
                   # remove html links 
                   sentence = gsub("http\\w+", "", sentence) 
                   # remove unnecessary spaces 
                   sentence = gsub("[ \t]{2,}", "", sentence) 
                   sentence = gsub("^\\s+|\\s+$", "", sentence)
                   # define error handling function when trying tolower 
                   tryTolower = function(x) 
                   { 
                     # create missing value 
                     y = NA 
                     # tryCatch error 
                     try_error = tryCatch(tolower(x), error=function(e) e) 
                     # if not an error 
                     if (!inherits(try_error, "error")) 
                       y = tolower(x) 
                     # result 
                     return(y) 
                   } 
                   # use tryTolower with sapply  
                   sentence = sapply(sentence, tryTolower) 
                   # split sentence into words with str_split (stringr package) 
                   word.list = str_split(sentence, "\\s+") 
                   words = unlist(word.list)
                   # compare words to the dictionaries of positive & negative terms 
                   pos.matches = match(words, pos.words) 
                   neg.matches = match(words, neg.words)
                   # get the position of the matched term or NA 
                   # we just want a TRUE/FALSE 
                   pos.matches = !is.na(pos.matches) 
                   neg.matches = !is.na(neg.matches)
                   # final score 
                   score = sum(pos.matches) - sum(neg.matches) 
                   return(score) 
                 }, pos.words, neg.words, .progress=.progress )
  # data frame with scores for each sentence 
  scores.df = data.frame(text=sentences, score=scores) 
  return(scores.df) 
} 

pos.words=scan("positive-words.txt", what="character", comment.char = ";")
neg.words=scan("negative-words.txt", what="character", comment.char = ";")



#pratice


#step1) Start file input code

##When you want to put it as a txt file
##Example execution with obama.txt

conn=file("obama.txt",open="r")
line=readLines(conn)
for (i in 1:length(line)){
  print(line[i])
}
close(conn)

#Notice before executing the sentence below: If the amount of txt is too large, the function to extract English words is when the pc's ram is low.
#It may take a long time and may go down.

X2<-sapply(line, extractNoun,USE.NAMES = T)
# X2<-sapply(line, extractNoun,USE.NAMES = F)

#unlist
c_vector<-unlist(X2)

#step2) Preprocessing code
#Special character and blank processing
c_vector<-str_replace_all(c_vector,"[^[:alpha:]]","")

#Save only non-blank
c_vector<-c_vector[c_vector!=""]

#step3) Check the frequency of desired articles and non-verbs (원하는 관사와 비동사 빈도 확인)
###a, the, is, am, are, be
# table (c_vector) dataframe conversion
X3<-as.data.frame(table(c_vector))
X3[X3$c=="a"|X3$c=="is"|X3$c=="the"|X3$c=="am"|X3$c=="are"|X3$c=="be",]


# [Preprocessing still in progress...]
###a, the, is, am, are
#Convert the articles or non-verbs you want to remove to blanks. (제거하고 싶은 관사나 비동사를 공백으로 변환함)
c_vector<-gsub("is","",c_vector)
c_vector<-gsub("the","",c_vector)
c_vector<-gsub("am","",c_vector)
c_vector<-gsub("are","",c_vector)

# The Number of Rows/Columns
NROW(c_vector)

#Extract only 2 digits or more (2자리이상만 추출)
c_vector<-Filter(function(x){nchar(x)>2},c_vector)


# Save as txt (txt로 저장)
write(c_vector,"ObamaFilter.txt")
H1<-read.table("ObamaFilter.txt")

#End of preprocessing (전처리 종료)
str(H1)
wordcount<-table(H1)
H1
#step4) Emotion score calculation (감정점수 계산)
results <- score.sentiment(H1$V1, pos.words, neg.words)
H1$score<-results$score
H1_mean<-mean(H1$score)
View(H1_mean)


H1_mean
#for example) 0.02071823
#obama

#H1_mean = Score of line sentence (H1_mean = line 문장의 감정점수)
#H1_mean이 감정점수라고 볼 수 있음 (H1_mean이 감정점수라고 볼 수 있음)
#1 point per 1 positive word-1 point per 1 negative word (ex: 긍정단어 1개당 1점 부정단어 1개당 -1점)
#The maximum value is between -1 and 1. (최대값은 -1 ~ 1 사이의 값을 가지게 됨)


# rm(list=ls())
#Initialization for resource recovery (리소스 회복을 위한 초기화)


##Start trump example

conn=file("trump.txt",open="r")
line=readLines(conn)
for (i in 1:length(line)){
  print(line[i])
}
close(conn)



X2<-sapply(line, extractNoun,USE.NAMES = T)


c<-unlist(X2)



X3<-as.data.frame(table(c))
X3[X3$c=="a"|X3$c=="is"|X3$c=="the"|X3$c=="am"|X3$c=="are"|X3$c=="be",]


###a, the, is, am, are, be



c<-Filter(function(x){nchar(x)>2},c)

#Special character and blank processing (특수문자 공백처리)
#res<-str_replace_all(c,"[^[:alpha:]]","")
res<-c
#Save only non-blank (공백이 아닌것만 저장)
res<-res[res!=""]

###a, the, is, am, are
#Convert the articles or non-verbs you want to remove to blanks (제거하고 싶은 관사나 비동사를 공백으로 변환함)
res<-gsub("is","",res)
res<-gsub("the","",res)
res<-gsub("am","",res)
res<-gsub("are","",res)

NROW(res)

# If unnecessary words are blanked out and saved as txt,
# Comes in with no spaces.
#불필요한 단어를 공백처리하고 txt로 저장후 불러들이면 
# 공백이 없어진채로 들어오게됨.
write(res,"Trump_temp.txt")
H1_Trump<-read.table("Trump_temp.txt")


str(H1_Trump)
wordcountTrump<-table(H1_Trump)
View(H1_Trump)


#Repeat the score.sentiment function above.
#위에 있는 score.sentiment 함수를 다시 수행하세요.
results_Trump <- score.sentiment(H1_Trump$V1, pos.words, neg.words)
H1_Trump$score<-results_Trump$score
H1_Tmean<-mean(H1_Trump$score)
View(H1_Tmean)

H1_Tmean
#0.04276069
#trump


obama_trump_score<-c(H1_mean,H1_Tmean)

windows()
barplot(obama_trump_score, col=rainbow(2), 
        xlab = "Speaker (Red: Obama, Blue: Trump)", ylab = "Speech affirmation")
title(main = "Positive comparison (Obama vs. Trump), 긍정도 비교(Obama vs. Trump)", font = 4)

#Comparing Obama and Trump's speech, we can see that Trump mentioned more positive words.
#오바마와 트럼프의 연설문을 비교한 결과 트럼프가 긍정적인단어를 더 많이 언급한 것을 알 수 있음.

#If you replace it with other celebrities like # steve.txt, bill.txt, etc., you can analyze the same result positively.
#steve.txt, bill.txt 등과 같이 다른 유명 인사문으로 바꿔서 넣으면 동일한 결과를 긍정도 분석가능합니다.

#[Team Project][Due date: June. 1st, Monday] Big Data Analysis about "Coronavirus"]
#Find the coronavirus data among articles in the New York Times site and save it as a txt file and compare the frequency of positive and negative words with respect to the coronavirus.

