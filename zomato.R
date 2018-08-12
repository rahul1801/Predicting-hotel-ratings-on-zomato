data = zomato
data_1 = data
library(ggplot2)
library(caret)
names(data)
qplot(`Aggregate rating`,data = data,binwidth = 0.1)
summary(data$`Aggregate rating`)
str(data)
data = subset(data,select = -`Restaurant ID`)
data = subset(data,select = -`Restaurant Name`)
ggplot(aes(data$City,y = data$`Aggregate rating`),geom = 'boxplot')
by(data$`Aggregate rating`,data$City,summary)
summary(data$City)
data$City = factor(data$City)
table(is.na(data$`Aggregate rating`))
data = na.omit(data)
levels(data$City)
data$City =  factor(data$City,exclude = c("Noida\"","NewDelhi\"","103.83916499999999","77.375287"))
data = subset(data,select = -`Locality Verbose`)
data = subset(data,select = -`Address`)
data$rat = (data$`Aggregate rating`*data$Votes)/(data$`Aggregate rating`+data$Votes)
summary(data$rat)
data = subset(data,select = -`Votes`)
data$`Price range` = factor(data$`Price range`)
qplot(x = `Price range`,data = data)+facet_wrap(~`Aggregate rating`)
ggplot(aes(rat,`Aggregate rating`),data = data)+geom_point()
names(data)
data$`Has Table booking` = factor(data$`Has Table booking`)
data$`Has Online delivery` = factor(data$`Has Online delivery`)
data$`Is delivering now` = factor(data$`Is delivering now`)
data = subset(data,select = -`Switch to order menu`)
data = subset(data,select = -`Rating color`)
data$`Rating text` = factor(data$`Rating text`)
data$Currency = factor(data$Currency)
data_current = data
data = subset(data,select = -`Cuisines`)
samplesize = 0.70 * nrow(data)
set.seed(80)
index = sample( seq_len ( nrow ( data ) ), size = samplesize )
data_train = data[index, ]
data_test = data[-index, ]
str(data_train)
#trctrl = trainControl(method = "repeatedcv",number = 10,repeats = 3)
model = train(`Aggregate rating`~.,data=data_train,method = "lm")
plot(model)
test_predict = predict(model,newdata = data_test)
test_predict = as.data.frame(test_predict)
c =sum(0.5*(1/9536)*(test_predict - data$`Aggregate rating`)*(test_predict - data$`Aggregate rating`))
?str_sub
library(stringr)
#str_locate((data$Locality,",")| (data$Locality,"$"))[,1]
str_sub(test_data$Name, str_locate(test_data$Name, ","))
summary(data$Locality)
data_1 = data
data$Locality = substr(data$Locality,1,ifelse(is.na(str_locate(data$Locality,",")[,1]),str_locate(data$Locality,"$"),str_locate(data$Locality,",")[,1]-1))
data$Locality = factor(data$Locality)
ggplot(x = Locality,y = `Aggregate rating`,data = data,geom = 'boxplot')
by(data$`Aggregate rating`,data$Locality,summary)
?strsplit
data_2 = data
data_2$Cuisines = strsplit(data$Cuisines,",")
summary(data_2$Cuisines)
data_3 = data
s = strsplit(data_3$Cuisines, split = ",")
a = data.frame(V1 = rep(data_3$Cuisines, sapply(s, length)), V2 = unlist(s))
a = factor(a)
levels(a)
a$V2 = factor(a$V2)
levels(a$V2)


n.speakers <- 15
n.records = 30
i <- c(rep(1,3), rep(0, n.speakers-2))
i
x.matrix <- sapply(1:n.records, function(j) sample(i, n.speakers))
x.matrix
x <- as.data.frame(t(x.matrix))
colnames(x) <- colnames <- lapply(1:n.speakers, function(i) sprintf("Speaker%d",i))
colnames(x)
x$y <- rnorm(n.records)
x$
formula <- paste("y ~", paste(colnames, collapse="+"))
formula
a$V3 = 1:19696
library(tidyr)
b = spread(a,V2,V3)
a = subset(a,select = -V1)
b = levels(a$V2)
b

df=data.frame(rbind(1:length(b))) 
df
names(df) = b
df
data_4 = rbind(data_3,df)
m = strsplit(data$Cuisines,",")
m[[1]]
m
df[1]
df1 = df
df[]
length(data_2$Cuisines[[1]])
df = as.character(df)
df
df1 = for (i in m) {
  ifelse()
}
df[1] = as.character(df[1])
df[1]
colnames(df)[1]
#n1 = c(2, 3, 5) 
# n2 = c("aa", "bb", "cc", "dd", "ee") 
 #n3 = c(TRUE, FALSE, TRUE, FALSE, FALSE) 
 #n4 = list(n1,n2,n3, 3)
#n4 
#n4[1]
#n4[[2]][1]
v = ifelse(colnames(df)[1]==" Afghani","T","F")
length(df)
for (i in 1:length(df)) {
  g[i] = colnames(df)[i]
}
length(g)
v = 131:length(g)
length(v)
for(i in v){
  print(g[v])
}
o[131]
o[249]
colnames(df)[131]
v
o
data_2$Cuisines[[1]][1]
df

df2 = df
data_4 = data_2
df2[1]
colnames(df2)[1]
length(data_4$Cuisines[[1]])
for(i in 1:130){
  rm(colnames(df2)[i])
}
df = df[,-c(1:130)]
data_4$Cuisines[[1]][1]
colnames(df)[1]
length(df)
  insertRows(df2,r = 2,0)
?insertRows  
install.packages("rowr")  
library(rowr)
dim(df2)
df2[2,] = 0
df2[3:9536,] = 0
df2[1,] = 0
data_2 = merge(data_2,df2,by=data_2$ID)
data_6$ID = c(1:9536)
df2$ID = c(1:9536)
data_3 = data_2$df2[1]
data_5 = data_4
z = df2[1]
data_4$df2 = df2[2]
data_4 = subset(data_4,select = -df2)
data_6 = data_5
install.packages("gtools")
library(gtools)
data_5 = smartbind(data_5,df2)
library(dplyr)
library(plyr)
?arrange
names(data_6)
data_7 = data_6
data_6 = subset(data_6,select = -`df2`)
data_6 <- merge(data_6,df2, by.x=data_6$ID,by.y=df2$ID)
data_6 = rbind.fill(data_6,df2)
data_6 = data_6[1:9536,]
data_6$`Cuisines`[[1]][2]
data_8 = data_6
data_8[[2]][1]
ifelse(data_8$Cuisines[[1]][1])
data_8$Cuisines[[1]]
v1 = 17:135
a = c(1,2)
a = data_10$Cuisines
w = colnames(data_10)[17:135]
w[1]
a[[1]]
for (j in 1:119) {
    for (i in 1:length(a[[1]])) {
      print(data_10$Cuisines[[1]][i])
      print(colnames(data_10)[j])
      b = ifelse(strcmpi(a[[1]][i],w[j]),1,0)
    }
}

df[[1]][1]
colnames(data_8)[16]
data_8[[17]][1] = ifelse(data_8$Cuisines[[1]][1]==colnames(data_8)[17],1,0)
data_8[[17]][1]

data_8$Cuisines[[1]][1]
colnames(data_8)[135]
for(i in 1:length(data_8$Cuisines[[1]])){
print(data_8$Cuisines[1][i])
}
data_8$Cuisines
data_9$Cuisines[[1]] = str_trim(data_9$Cuisines[[1]],side = "left")
data_9$Cuisines[[1]]
data_9 = data_6
data_10 = data_6
v2 = 1:9536
for(i in v2){
  data_10$Cuisines[[i]] = str_trim(data_10$Cuisines[[i]],side = "left")
}
for(i in 1:length(data_10$Cuisines[[1]])){
  print(data_10$Cuisines[[1]][i])
}
data_10$Cuisines[[1]][1]
data_10$Cuisines[[1]][2]
data_10$Cuisines[[1]][3]
install.packages("pracma")
library(pracma)
a = ifelse(strcmpi(data_10$Cuisines[[1]],colnames(data_10)),1,0)
data_11 = data
data_11 = subset(data_11,select = -`Cuisines`)
str(data_11)
data_11$Currency = factor(data_11$Currency)
data_11$`Country Code` = factor(data_11$`Country Code`)
