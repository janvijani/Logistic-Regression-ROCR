summary(bank_marketing)
summary(bank_marketing$job)
# blue-collar , housemaid , technician = blue-collar
# admin. , management  = desk_jobs
# retired , student, unemployed,unknown  = no_income 

bank_marketing$job<-as.character(bank_marketing$job)
bank_marketing$job[bank_marketing$job=="blue-collar"|
                     bank_marketing$job=="housemaid"|
                     bank_marketing$job=="technician"]<-"blue-collar"

bank_marketing$job[bank_marketing$job=="admin."|
                     bank_marketing$job=="management"]<-"desk_job"

bank_marketing$job[bank_marketing$job=="retired"|
                     bank_marketing$job=="student"|
                     bank_marketing$job=="unemployed"|
                     bank_marketing$job=="unknown"]<-"no_income"

bank_marketing$job[bank_marketing$job=="entrepreneur"|
                     bank_marketing$job=="self-employed"]<-"self-employed"

bank_marketing$job<-as.factor(bank_marketing$job)
summary(bank_marketing$job)

bank_marketing$default<-as.character(bank_marketing$default)
bank_marketing$default[bank_marketing$default=="unknown"]<-"no"
bank_marketing$default<-as.factor(bank_marketing$default)
bank_marketing$default<-NULL

bank_marketing$housing<-as.character(bank_marketing$housing)
bank_marketing$housing[bank_marketing$housing=="unknown"]<-"no"
bank_marketing$housing<-as.factor(bank_marketing$housing)

bank_marketing$loan<-as.character(bank_marketing$loan)
bank_marketing$loan[bank_marketing$loan=="unknown"]<-"no"
bank_marketing$loan<-as.factor(bank_marketing$loan)

summary(bank_marketing)
quantile(bank_marketing$campaign,probs = seq(0.9,1.0,0.01))

# as 91% of customers have value for campaign [no of times customer is contacted]
# either 5 or less than that ; so any value > 5 will be converted as 5

bank_marketing$campaign[bank_marketing$campaign>5]<-5
summary(bank_marketing$campaign)
bank_marketing$campaign<-as.factor(bank_marketing$campaign)

summary(bank_marketing$campaign)
head(bank_marketing$campaign,20)

bank_marketing$marital<-as.character(bank_marketing$marital)
bank_marketing$marital[bank_marketing$marital=="unknown"]<-"married"
bank_marketing$marital<-as.factor(bank_marketing$marital)
summary(bank_marketing)

quantile(bank_marketing$duration,probs = seq(0.9,1.0,0.01))
bank_marketing$duration[bank_marketing$duration>600]<-600
summary(bank_marketing$duration)
head(bank_marketing$duration,20)

head(bank_marketing$pdays,50)
# as most of the observations have value 999 ; the column has became almost
# constant ; so removing the column 


#ROCR doesnot need upsampling or downsampling 
#here, we are trying to find out threshold other than 0.5 to see if it affects our o/p

#hold cross valudation 

#set.seed helps us generate the same set of random observations each time 
nrow(bank_marketing)
index<-sample(41188,0.75*41188)
head(index)
train_bank_marketing<- bank_marketing[index,]
test_bank_marketing<- bank_marketing[-index,]
 
dim(train_bank_marketing) #  30891    20
dim(test_bank_marketing)  # 10297    20
View(train_bank_marketing)

# build model using train data 
null_bank_marketing_model<-glm(y~1,data = train_bank_marketing,family = "binomial")
full_bank_marketing_model<-glm(y~.,data = train_bank_marketing,family = "binomial")
step(null_bank_marketing_model,direction = "forward",
     scope=list(lower=null_bank_marketing_model,upper=full_bank_marketing_model))

bank_marketing_model<-glm(formula = y ~ duration + nr.employed + month + poutcome + 
                            emp.var.rate + cons.price.idx + euribor3m + contact + education + 
                            pdays + campaign + job + day_of_week + cons.conf.idx + previous, 
                          family = "binomial", data = train_bank_marketing)

# generate probabilities of being "1" for every observation 
pred_prob_training<-predict(bank_marketing_model,train_bank_marketing,type="response")
head(pred_prob_training)

library(ROCR)  
pred<-prediction(pred_prob_training, train_bank_marketing$y)
perf<-performance(pred,"tpr","tnr")
plot.new()
plot(perf,colorize=T,print.cutoffs.at=seq(0.1,0.2,0.03))


pred_class<-ifelse(pred_prob_training>0.15,"yes","no")
evaluation_matrix<-table(actual= train_bank_marketing$y,predicted=pred_class)
evaluation_matrix
acc_train<-(23725+2969)/nrow(train_bank_marketing);acc_train #0.86
sen_train<- 2969/(2969+441);sen_train # 0.86
spe_train<-23725/(23725+3756);spe_train #0.87

pred_prob_test<-predict(bank_marketing_model,test_bank_marketing,
                        type="response")
pred_class_test<-ifelse(pred_prob_test>=0.15,"yes","no")
table(actual=test_bank_marketing$y,predicted=pred_class_test)
acc_test<- (7823+1105)/nrow(test_bank_marketing);acc_test #0.86
sen_test<- 1105/(125+1105);sen_test # 0.89
spe_test<- 7823/(7823+1244);spe_test #0.89

#Titanic Dataset 

titanic<-read.csv("/Users/janvijani/Desktop/pg diploma/databases/titanic.csv",stringsAsFactors = T,
                  na.strings = c("","","NA"))
summary(titanic)
titanic$PassengerId<-NULL
titanic$Name<-NULL
titanic$Ticket<-NULL
titanic$Cabin<-NULL

#here, we want to convert multiple columns into factor format. So we use the following code: 

vect1<-c("Survived","Pclass","SibSp","Parch")
titanic[,vect1]<-lapply(titanic[,vect1],as.factor)
summary(titanic)

#impute age and embarked using their central tendencies 
titanic$Age[is.na(titanic$Age)]<-median(titanic$Age,na.rm = T)
titanic$Embarked[is.na(titanic$Embarked)]<-"S"
colSums(is.na(titanic))

#CHECKING DIMENTIONS OF DEPENDENT VARIABLES I.E. SURVIVED 
table(titanic$Survived)

#performaing upsampling
library(caret)
up_titanic<-upSample(titanic,titanic$Survived)
table(up_titanic$Survived)
summary(up_titanic)

#performing logistic regression
#performing hold out cross validation.... divide data into 75% training and 25% testing data

index<-sample(nrow(up_titanic),0.75*nrow(up_titanic))
titanic_train<-up_titanic[index,]
titanic_test<-up_titanic[-index,]

# build model on training data using step wise regression 
# build model on training data using step wise regression 
titanic_train$Class<-NULL

#if you get an erroe called glm.fit: algorithm did not converge 
# it means thaT THERE is an extra variable which is a replica of dependent variable but is a part of independent vars
#IN THIS CASE, IT WAS "CLASS"
#this happened when we upsampled our dATA
#so, we remove the class variable 

null_titanic_model<-glm(Survived~1,data = titanic_train,family = "binomial")
full_titanic_model<-glm(Survived~.,data = titanic_train,family = "binomial")
step(null_titanic_model,direction = "forward", 
     scope=list(lower=null_titanic_model,upper=full_titanic_model))

titanic_model<-glm(formula = Survived ~ Sex + Pclass + Age + SibSp + Fare, family = "binomial", 
                   data = titanic_train)
null_titanic_model<-glm(Survived~1,data = titanic_train,family = "binomial")
full_titanic_model<-glm(Survived~.,data = titanic_train,family = "binomial")
step(null_titanic_model,direction = "forward",
     scope=list(lower=null_titanic_model,upper=full_titanic_model))

titanic_model<-glm(formula = Survived ~ Sex + Pclass + Age + SibSp + Fare, family = "binomial", 
                   data = titanic_train)

# generate probabilities 
pred_prob_train_titanic<-predict(titanic_model,titanic_train,type = "response")
# threshold = 0.5 
pred_Survived<-ifelse(pred_prob_train_titanic>=0.5,"1","0")
pred_Survived<-as.factor(pred_Survived)
# generate evaluation matrix 
table(actual=titanic_train$Survived,predicted=pred_Survived)

acc_train<-(315+330)/nrow(titanic_train);acc_train # 0.78
sen_train<- 315/(315+80);sen_train # 0.79
spe_train<-330 /(330+98);spe_train #0.77

# implement model on test data 
pred_prob_test_titanic<-predict(titanic_model,titanic_test,type = "response")
pred_Survived_test<-ifelse(pred_prob_test_titanic>=0.5,"1","0")
pred_Survived_test<-as.factor(pred_Survived_test)
table(actual=titanic_test$Survived,predicted=pred_Survived_test)
acc_test<-(125+92)/nrow(titanic_test);acc_test # 0.78
sen_test<-125/(125+29);sen_test # 0.81
spe_test<-92/(29+92);spe_test # 0.76
summary(titanic_model)


