
# First we need to set the directory of the file's location in your computer and then read it so that R can download the data
setwd("E:/DATA ANALYTICS JOURNEY/R Edvancer/PROJECT 5 BANKING") 

bf_train = read.csv("bank-full_train.csv",sep = ",",header = T)
bf_test = read.csv("bank-full_test.csv",sep=",",header = T)

# Library that can be used for this model
library(dplyr)
library(randomForest)

# Structure of the training dataset
glimpse(bf_train)

# You will need same set of vars on both train and test,its easier to manage that if you combine train and test
# in the beginning and then separate them once you are done with data preparation
# We'll fill test's response column with NAs.
bf_test$y = NA
bf_train$data = 'train'
bf_test$data = 'test'

all = rbind(bf_train,bf_test)

apply(all,2,function(x) length(unique(x)))

# We remove ID and poutcome as they are not useful in predictions
all = all %>% 
  select(-ID,-poutcome)

# Next we'll create dummy variables for remaining categorical variables
CreateDummies=function(data,var,freq_cutoff=100){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    name=gsub("/","_",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
} 

char_logical = sapply(all,is.character)
cat_cols = names(all)[char_logical]
cat_cols

cat_cols = cat_cols[!(cat_cols %in% c('data','y'))]
cat_cols

# we are using frequency cutoff as 50, there is no magic number here,
# lower cutoffs will simply result in more number of dummy variables
for(col in cat_cols){
  all=CreateDummies(all,col,50)
}

# Remove NA values from the data except the response column 
for(col in names(all))
{
  if(sum(is.na(all[,col]))>0 & !(col %in% c("data","y")))
  {
    all[is.na(all[,col]),col]=mean(all[all$data=='train',col],na.rm=T)
  }
}

head(all)

bf_train = all %>% filter(data == 'train') %>% select(-data) 
bf_test= all %>% filter(data == 'test') %>% select(-y, -data) 

any(is.na(bf_train))
any(is.na(bf_test))

# Export Training and Test data set for future use
write.csv(bf_train, "bf_train_clean.csv", row.names = F)
write.csv(bf_test, "bf_test_clean.csv", row.names = F)

library(randomForest)
fit = randomForest(as.factor(y )~ ., data = bf_train) 

# Make predictions on test and submit 
test.predictions = predict(fit, newdata = bf_test)
write.csv(test.predictions,file = "Umakant_Project_5.csv", row.names = F)

