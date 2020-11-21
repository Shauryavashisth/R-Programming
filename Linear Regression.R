df=read.csv("D://Study//BA//March-April Classes//Par-Linear-Regression//datafiles//OnlineNewsPopularity.csv",stringsAsFactors = FALSE)
library(ggplot2)
library(GGally)
library(Hmisc)
library(car)
View(df)
str(df)
summary(df)

#Missing value Treatment

df$n_tokens_content=impute(as.numeric(df$n_tokens_content,mean))
boxplot(df$n_tokens_content)
summary(df$n_tokens_content)

#Need The Treatment?                      #Done?                  #Perfect?                   #Col Num
boxplot(df$n_tokens_title)                #yes                    #YES                          2
boxplot(df$n_tokens_content)              #yes                    #YES                          3
boxplot(df$n_unique_tokens)               #yes                    #YES                          4
boxplot(df$n_non_stop_unique_tokens)      #Yes                    #YES                          5
boxplot(df$num_hrefs)                     #yes                    #YES                          6
boxplot(df$num_self_hrefs)                #yes                    #YES                          7
boxplot(df$num_imgs)                      #yes                    #YES                          8
boxplot(df$num_videos)                    #yes                    #YES                          9
boxplot(df$average_token_length)          #yes                    #YES                          10
boxplot(df$num_keywords)                  #yes                    #YES                          11
boxplot(df$data_channel_is_lifestyle)     #yes                    #YES(Problem)       0,1       12
boxplot(df$data_channel_is_entertainment) #yes                    #YES(Problem)       0,1       13
boxplot(df$data_channel_is_bus)           #yes                    #YES(Problem)       0,1       14
boxplot(df$data_channel_is_socmed)        #yes                    #YES(Problem)       0,1       15
boxplot(df$data_channel_is_tech)          #yes                    #YES(Problem)       0,1       16
boxplot(df$self_reference_avg_sharess)    #yes                    #YES                          17
boxplot(df$is_weekend)                    #yes                    #YES(Problem)       0,1       18
boxplot(df$global_sentiment_polarity)     #yes                    #YES                          19
boxplot(df$global_rate_positive_words)    #yes                    #Yes                          20
boxplot(df$global_rate_negative_words)    #yes                    #Yes                          21
boxplot(df$avg_positive_polarity)         #yes                    #YES                          22
boxplot(df$shares)                        #yes                    #YES                          23
boxplot(df$data_channel_is_lifestyles)    #yes                    #YES(Problem)       0,1       24

#df$n_tokens_title

x=df$n_tokens_title
qnt=quantile(x,probs = c(.25,.75),na.rm = T)
caps=quantile(x,probs = c(0.05,.95),na.rm = T)
H=1.5*IQR(x,na.rm = T)
x[x<(qnt[1]-H)]=caps[1]
x[x>(qnt[2]+H)]=caps[2]
df$n_tokens_title=x
boxplot(df$n_tokens_title)

#df$n_tokens_content                                             

x=df$n_tokens_content
qnt=quantile(x,probs = c(.25,.75),na.rm = T)
caps=quantile(x,probs = c(0.05,.95),na.rm = T)
H=1.5*IQR(x,na.rm = T)
x[x<(qnt[1]-H)]=caps[1]
x[x>(qnt[2]+H)]=caps[2]
df$n_tokens_content=x
boxplot(df$n_tokens_content)

#df$n_unique_tokens

x=df$n_unique_tokens
qnt=quantile(x,probs = c(.25,.75),na.rm = T)
caps=quantile(x,probs = c(0.05,.95),na.rm = T)
H=1.5*IQR(x,na.rm = T)
x[x<(qnt[1]-H)]=caps[1]
x[x>(qnt[2]+H)]=caps[2]
df$n_unique_tokens=x
boxplot(df$n_unique_tokens)

#df$n_non_stop_unique_tokens

x=df$n_non_stop_unique_tokens
qnt=quantile(x,probs = c(.25,.75),na.rm = T)
caps=quantile(x,probs = c(0.05,.95),na.rm = T)
H=1.5*IQR(x,na.rm = T)
x[x<(qnt[1]-H)]=caps[1]
x[x>(qnt[2]+H)]=caps[2]
df$n_non_stop_unique_tokens=x
boxplot(df$n_non_stop_unique_tokens)

#df$num_hrefs

x=df$num_hrefs
qnt=quantile(x,probs = c(.25,.75),na.rm = T)
caps=quantile(x,probs = c(0.05,.95),na.rm = T)
H=1.5*IQR(x,na.rm = T)
x[x<(qnt[1]-H)]=caps[1]
x[x>(qnt[2]+H)]=caps[2]
df$num_hrefs=x
boxplot(df$num_hrefs)

#df$num_self_hrefs

x=df$num_self_hrefs
qnt=quantile(x,probs = c(.25,.75),na.rm = T)
caps=quantile(x,probs = c(0.05,.90),na.rm = T)
H=1.5*IQR(x,na.rm = T)
x[x<(qnt[1]-H)]=caps[1]
x[x>(qnt[2]+H)]=caps[2]
df$num_self_hrefs=x
boxplot(df$num_self_hrefs)

#df$num_imgs

x=df$num_imgs
qnt=quantile(x,probs = c(.25,.75),na.rm = T)
caps=quantile(x,probs = c(0.05,.80),na.rm = T)
H=1.5*IQR(x,na.rm = T)
x[x<(qnt[1]-H)]=caps[1]
x[x>(qnt[2]+H)]=caps[2]
df$num_imgs=x
boxplot(df$num_imgs)

#df$num_videos

x=df$num_videos
qnt=quantile(x,probs = c(.25,.75),na.rm = T)
caps=quantile(x,probs = c(0.05,.90),na.rm = T)
H=1.5*IQR(x,na.rm = T)
x[x<(qnt[1]-H)]=caps[1]
x[x>(qnt[2]+H)]=caps[2]
df$num_videos=x
boxplot(df$num_videos)

#df$average_token_length

x=df$average_token_length
qnt=quantile(x,probs = c(.25,.75),na.rm = T)
caps=quantile(x,probs = c(0.05,.95),na.rm = T)
H=1.5*IQR(x,na.rm = T)
x[x<(qnt[1]-H)]=caps[1]
x[x>(qnt[2]+H)]=caps[2]
df$average_token_length=x
boxplot(df$average_token_length)

#df$num_keywords

x=df$num_keywords
qnt=quantile(x,probs = c(.25,.75),na.rm = T)
caps=quantile(x,probs = c(0.05,.95),na.rm = T)
H=1.5*IQR(x,na.rm = T)
x[x<(qnt[1]-H)]=caps[1]
x[x>(qnt[2]+H)]=caps[2]
df$num_keywords=x
boxplot(df$num_keywords)

#df$data_channel_is_lifestyle                                             #problem

x=df$data_channel_is_lifestyle
qnt=quantile(x,probs = c(.25,.75),na.rm = T)
caps=quantile(x,probs = c(0.05,.95),na.rm = T)
H=1.5*IQR(x,na.rm = T)
x[x<(qnt[1]-H)]=caps[1]
x[x>(qnt[2]+H)]=caps[2]
df$data_channel_is_lifestyles=x
boxplot(df$data_channel_is_lifestyle)

#df$data_channel_is_entertainment                                         #problem

x=df$data_channel_is_entertainment
qnt=quantile(x,probs = c(.25,.75),na.rm = T)
caps=quantile(x,probs = c(0.05,.95),na.rm = T)
H=1.5*IQR(x,na.rm = T)
x[x<(qnt[1]-H)]=caps[1]
x[x>(qnt[2]+H)]=caps[2]
df$data_channel_is_entertainment=x
boxplot(df$data_channel_is_entertainment)

#df$data_channel_is_bus                                                   #problem

x=df$data_channel_is_bus
qnt=quantile(x,probs = c(.25,.75),na.rm = T)
caps=quantile(x,probs = c(0.05,.95),na.rm = T)
H=1.5*IQR(x,na.rm = T)
x[x<(qnt[1]-H)]=caps[1]
x[x>(qnt[2]+H)]=caps[2]
df$data_channel_is_bus=x
boxplot(df$data_channel_is_bus)

#df$data_channel_is_socmed                                                #problem

x=df$data_channel_is_socmed
qnt=quantile(x,probs = c(.25,.75),na.rm = T)
caps=quantile(x,probs = c(0.05,.95),na.rm = T)
H=1.5*IQR(x,na.rm = T)
x[x<(qnt[1]-H)]=caps[1]
x[x>(qnt[2]+H)]=caps[2]
df$data_channel_is_socmed=x
boxplot(df$data_channel_is_socmed)

#df$data_channel_is_tech                                                  #problem

x=df$data_channel_is_tech
qnt=quantile(x,probs = c(.25,.75),na.rm = T)
caps=quantile(x,probs = c(0.05,.95),na.rm = T)
H=1.5*IQR(x,na.rm = T)
x[x<(qnt[1]-H)]=caps[1]
x[x>(qnt[2]+H)]=caps[2]
df$data_channel_is_tech=x
boxplot(df$data_channel_is_tech)

#df$self_reference_avg_sharess

x=df$self_reference_avg_sharess
qnt=quantile(x,probs = c(.25,.75),na.rm = T)
caps=quantile(x,probs = c(0.05,.85),na.rm = T)
H=1.5*IQR(x,na.rm = T)
x[x<(qnt[1]-H)]=caps[1]
x[x>(qnt[2]+H)]=caps[2]
df$self_reference_avg_sharess=x
boxplot(df$self_reference_avg_sharess)

#df$is_weekend                                                            #problem

x=df$is_weekend
qnt=quantile(x,probs = c(.25,.75),na.rm = T)
caps=quantile(x,probs = c(0.05,.95),na.rm = T)
H=1.5*IQR(x,na.rm = T)
x[x<(qnt[1]-H)]=caps[1]
x[x>(qnt[2]+H)]=caps[2]
df$is_weekend=x
boxplot(df$is_weekend)

#df$global_sentiment_polarity

x=df$global_sentiment_polarity
qnt=quantile(x,probs = c(.25,.75),na.rm = T)
caps=quantile(x,probs = c(0.05,.95),na.rm = T)
H=1.5*IQR(x,na.rm = T)
x[x<(qnt[1]-H)]=caps[1]
x[x>(qnt[2]+H)]=caps[2]
df$global_sentiment_polarity=x
boxplot(df$global_sentiment_polarity)

#df$global_rate_positive_words

x=df$global_rate_positive_words
qnt=quantile(x,probs = c(.25,.75),na.rm = T)
caps=quantile(x,probs = c(0.05,.95),na.rm = T)
H=1.5*IQR(x,na.rm = T)
x[x<(qnt[1]-H)]=caps[1]
x[x>(qnt[2]+H)]=caps[2]
df$global_rate_positive_words=x
boxplot(df$global_rate_positive_words)

#df$global_rate_negative_words

x=df$global_rate_negative_words
qnt=quantile(x,probs = c(.25,.75),na.rm = T)
caps=quantile(x,probs = c(0.05,.95),na.rm = T)
H=1.5*IQR(x,na.rm = T)
x[x<(qnt[1]-H)]=caps[1]
x[x>(qnt[2]+H)]=caps[2]
df$global_rate_negative_words=x
boxplot(df$global_rate_negative_words)

#df$avg_positive_polarity

x=df$avg_positive_polarity
qnt=quantile(x,probs = c(.25,.75),na.rm = T)
caps=quantile(x,probs = c(0.05,.95),na.rm = T)
H=1.5*IQR(x,na.rm = T)
x[x<(qnt[1]-H)]=caps[1]
x[x>(qnt[2]+H)]=caps[2]
df$avg_positive_polarity=x
boxplot(df$avg_positive_polarity)

#df$shares

x=df$shares
qnt=quantile(x,probs = c(.25,.75),na.rm = T)
caps=quantile(x,probs = c(0.05,.85),na.rm = T)
H=1.5*IQR(x,na.rm = T)
x[x<(qnt[1]-H)]=caps[1]
x[x>(qnt[2]+H)]=caps[2]
df$shares=x
boxplot(df$shares)

#df$data_channel_is_lifestyles                                            #problem

x=df$data_channel_is_lifestyles
qnt=quantile(x,probs = c(.25,.75),na.rm = T)
caps=quantile(x,probs = c(0.05,.95),na.rm = T)
H=1.5*IQR(x,na.rm = T)
x[x<(qnt[1]-H)]=caps[1]
x[x>(qnt[2]+H)]=caps[2]
df$data_channel_is_lifestyles=x
boxplot(df$data_channel_is_lifestyles)

ggpairs(data=df[ ,-c(1)])

                    cormat=cor(df[ ,-c(1)])                  
                    View(cormat)                                        
                    getwd()                                            
                    setwd("C://Users//Shaurya//Desktop")               
                    write.csv(cormat,file = 'c:/Users/Shaurya/cor.csv')

model=lm(shares~.,data = df[ ,-c(1,4,5,22,19,24)])
summary(model)
vif(model)

#train test split

samplesize = 0.75 * nrow(df)
set.seed(123)
trainsample=sample(seq_len(nrow(df)), size = samplesize)
train=df[trainsample, ]
test=df[-trainsample, ]


#model BUILDING
linearMod=lm(shares ~ ., data=train[,-c(1,3,4,5,19,22,24)])
summary(linearMod)

#prediction

prediction=predict(linearMod, test)  

#comparing

comparing1=data.frame(cbind(actuals=train$shares, predicteds=prediction))
goodorno=cor(comparing1)

#MAPE

mape=mean((abs((comparing1$predicteds - comparing1$actuals))/comparing1$actuals),na.rm = TRUE)
mape
