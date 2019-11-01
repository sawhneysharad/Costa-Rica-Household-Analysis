#Libaries required for excution
require(ggplot2)
require(dplyr)
require(plyr)
require(tidyr)
require(tidyverse)
require(outliers)
require(psych)
library(ggplot2)
library(dplyr)
library(plyr)
library(tidyr)
library(tidyverse)
library(outliers)
library(psych)
library(corrplot)
library(ggcorrplot)
library(MASS)
library(ordinal)
library(ROCR)
library(pROC)
library(Matrix)
install.packages("xgboost")
library(xgboost)
library(magrittr)



#for correlation******
install.packages("faraway")
library(faraway)
#*****************Step 1: Imprting data into R studio*******
#

myData1 <- read.csv(file.choose(), sep=",", header = TRUE )
myData1$Id<-NULL
myData1$idhogar<-NULL
myData1$SQBage<-NULL
myData1$agesq<-NULL
myData1$SQBescolari<-NULL
myData1$SQBhogar_total<-NULL
myData1$SQBedjefe<-NULL
myData1$SQBhogar_nin<-NULL
myData1$SQBovercrowding<-NULL
myData1$SQBdependency<-NULL
myData1$SQBmeaned<-NULL
myData1$dependency<-NULL
myData1$edjefe<-NULL
myData1$edjefa<-NULL
head(myData1)
str(myData1)
# Changing catogorical varibles in Factors
#Factors<-c(3,5:8,24:95,105:113,116:132)
#myData1[,Factors]<-lapply(myData1[,Factors],factor)
str(myData1)
#changing column names to more understanble way
names(myData1)[names(myData1)=="rez_esc"] <- "YearsBehindSchool"
names(myData1)[names(myData1)=="v18q1"] <- "numberoftabletshouseholdholds" 
names(myData1)[names(myData1)=="v2a1"] <- "MonthlyRent"
names(myData1)[names(myData1)=="meaneduc"] <- "AverageAdultEducation"
names(myData1)[names(myData1)=="SQBmeaned"] <- "SquaremeanofAdulteducation"
colnames(myData1)
#Changing lables of target varible
#myData1$Target[myData1$Target == 1] ='Extreme Poverty'
#myData1$Target[myData1$Target == 2] ='Moderate Poverty'
#myData1$Target[myData1$Target == 3] ='Vulnerable Household'
#myData1$Target[myData1$Target == 4] ='Non-Vulnerable Household'
#myData1$Target

#***************Step 2: Data cleaning ********************

#finding missing valules in data set
missingvalues<-myData1 %>% summarise_each(funs(sum(is.na(.))*100/n()))
missingvalues<- gather(missingvalues,key = "feature",value ="missing_pct")
missingvalues
#showing varibles which have more than 0.01% of missing values.
missingfeatures<-filter(missingvalues,missing_pct > 0.01)
missingfeatures
#visavlizing the Missing values in graphical form
missingfeatures %>% ggplot(aes(x=reorder(feature,-missing_pct),y=missing_pct)) + geom_bar(stat = "identity",fill="red") +
  coord_flip()+theme_bw()
# Handling the missing values 
# Methods 1 :
#Removing Non -significant varibles/More missing values varible from Data set
myData1$YearsBehindSchool <- NULL
ncol(myData1)
#Method 2:
# Substittuing the Mean or zero values in place NA values in data set
myData1$MonthlyRent[is.na(myData1$MonthlyRent)]<-mean(myData1$MonthlyRent,na.rm = T)
myData1$AverageAdultEducation[is.na(myData1$AverageAdultEducation)]<-mean(myData1$AverageAdultEducation,na.rm = T)
#myData1$SquaremeanofAdulteducation[is.na(myData1$SquaremeanofAdulteducation)]<-mean(myData1$SquaremeanofAdulteducation,na.rm = T)
myData1$numberoftabletshouseholdholds[is.na(myData1$numberoftabletshouseholdholds)]<-0
myData1$numberoftabletshouseholdholds
#Checking the outliers in by using BoxPlot
ggplot(myData1,aes(x= "MonthlyRent", y=MonthlyRent)) + geom_boxplot()
#finding count outliers using below Z- score value method
#get the z Score for variable
Outlierscore<-scores(myData1$MonthlyRent)
is_outlier<-Outlierscore > 3 | Outlierscore< -3
# add outlier column to data set 
myData1$is_outlier<-is_outlier
#creating data frame with outliers
data_v2_Outlier <- myData1[Outlierscore > 3 | Outlierscore< -3,]
head(data_v2_Outlier)
#counting the number outliers
nrow(data_v2_Outlier)
# we have around 142 values which are outliers 
# Handling Outliers
#Method 1
# we are  treating outliers and inliners sepately
data_v2_Outlier <- myData1[Outlierscore > 3 | Outlierscore< -3,]
data_v2_Outlier$Target
nrow(data_v2_Outlier)
data_v2_Outlier_rm <- myData1[Outlierscore < 3 | Outlierscore> -3,]
#from the outlier data frrame we can conclude that most of the values falls under non-vulnerable household
data_v2_Outlier$Target
#Method 2
# Removing the outliers from the data set
DataNew<-myData1[myData1$is_outlier == F,]
DataNew$is_outlier <- NULL
ncol(DataNew)
head(DataNew)

#**************Step3 :Explortory Data Analysis ********
# Descriptive Stastics
summary(DataNew)
#Analyzing Target clasess (Predicting variable)
DataNew %>% ggplot(aes(Target)) +  geom_bar(color='Black',fill="blue")+ 
  xlab("Poverty Designation") + ylab("Poverty Designation Count")

#Analyzing how Number of rooms in house effecting poverty prediction
DataNew %>% ggplot(aes(as.factor(rooms))) + geom_bar(aes(fill= as.factor(Target)),color="Black")+ labs(x="Number of rooms",y="Count")

# Finding the relation between number of males younger than 12 and poverty level 
DataNew %>%
  ggplot(aes(x = as.factor(Target)))+
  geom_bar(colour = "grey19", fill = "blue", alpha = 0.6)+
  facet_wrap(~as.factor(r4h1), scales = "free", ncol = 3)+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))+
  theme(strip.text = element_text(size = 9, face = "bold"))+
  labs(title = "Relation between number of males younger than 12 and Household Type", x = "Type of Household", y = "Count")

#***********Correlation*******
Corr<-cor(myData1)
library(corrplot)
library(ggcorrplot)
corrplot(Corr, method = "circle")
str(myData1)
#****

#sanitario
myData1$sanitario2[myData1$sanitario2==1]<-2
myData1$sanitario3[myData1$sanitario3==1]<-3
myData1$sanitario4[myData1$sanitario4==1]<-4
myData1$sanitario5[myData1$sanitario5==1]<-5
myData1$sanitario6[myData1$sanitario6==1]<-6
sanitariow<-c("sanitario1","sanitario2","sanitario3","sanitario5","sanitario6")
myData1$sanitario<-rowSums(myData1[,sanitariow])
myData1$sanitario1<-NULL
myData1$sanitario2<-NULL
myData1$sanitario3<-NULL
myData1$sanitario4<-NULL
myData1$sanitario5<-NULL
myData1$sanitario6<-NULL
myData1$sanitario <-as.ordered(myData1$sanitario)
#OutsideMaterial
myData1$paredfibras[myData1$paredfibras==1]<-2
myData1$pareddes[myData1$pareddes==1]<-3
myData1$paredblolad[myData1$paredblolad==1]<-4
myData1$paredmad[myData1$paredmad==1]<-5
myData1$paredpreb[myData1$paredpreb==1]<-6
myData1$paredzinc[myData1$paredzinc==1]<-7
myData1$paredzocalo[myData1$paredzocalo==1]<-8
OutsideMAtR<-c("paredother","paredfibras","pareddes","paredblolad","paredmad","paredpreb","paredzinc","paredzocalo")
myData1$OutsideMaterial<-rowSums(myData1[,OutsideMAtR])
myData1$paredother<-NULL
myData1$paredfibras<-NULL
myData1$pareddes<-NULL
myData1$paredblolad<-NULL
myData1$paredmad<-NULL
myData1$paredpreb<-NULL
myData1$paredzinc<-NULL
myData1$paredzocalo<-NULL
myData1$OutsideMaterial <-as.ordered(myData1$OutsideMaterial)
#Water
myData1$abastaguafuera[myData1$abastaguafuera==1]<-2
myData1$abastaguadentro[myData1$abastaguadentro==1]<-3
WaterR<-c("abastaguafuera","abastaguadentro","abastaguano")
myData1$Water<-rowSums(myData1[,WaterR])
str(myData1$Water)
myData1$abastaguano<-NULL
myData1$abastaguafuera<-NULL
myData1$abastaguadentro<-NULL
myData1$Water <-as.ordered(myData1$Water)
#Electricity
myData1$public[myData1$public==1]<-2
myData1$coopele[myData1$coopele==1]<-3
myData1$planpri[myData1$planpri==1]<-4
ElectrcityR<-c("public","coopele","planpri","noelec")
myData1$Electricity<-rowSums(myData1[,ElectrcityR])
str(myData1$Electricity)
myData1$public<-NULL
myData1$coopele<-NULL
myData1$planpri<-NULL
myData1$noelec<-NULL
myData1$Electricity <-as.ordered(myData1$Electricity)
#Walls
myData1$epared2[myData1$epared2==1]<-2
myData1$epared3[myData1$epared3==1]<-3
WallsR<-c("epared1","epared2","epared3")
myData1$Walls<-rowSums(myData1[,WallsR])
str(myData1$Walls)
myData1$epared1<-NULL
myData1$epared2<-NULL
myData1$epared3<-NULL
myData1$Walls <-as.ordered(myData1$Walls)
#Roof
myData1$etecho2[myData1$etecho2==1]<-2
myData1$etecho3[myData1$etecho3==1]<-3
RoofR<-c("etecho1","etecho2","etecho3")
myData1$Roof<-rowSums(myData1[,RoofR])
str(myData1$Roof)
myData1$etecho1<-NULL
myData1$etecho2<-NULL
myData1$etecho3<-NULL
myData1$Roof <-as.ordered(myData1$Roof)
#FloorCon
myData1$eviv2[myData1$eviv2==1]<-2
myData1$eviv3[myData1$eviv3==1]<-3
FloorR<-c("eviv1","eviv2","eviv3")
myData1$FloorCon<-rowSums(myData1[,FloorR])
str(myData1$Roof)
myData1$eviv1<-NULL
myData1$eviv2<-NULL
myData1$eviv3<-NULL
myData1$FloorCon <-as.ordered(myData1$FloorCon)

#RoofMaterial
myData1$techocane[myData1$techocane==1]<-2
myData1$techoentrepiso[myData1$techoentrepiso==1]<-3
myData1$techozinc[myData1$techozinc==1]<-4
RoofmatR<-c("techootro","techocane","techoentrepiso","techozinc")
myData1$RoofMaterial<-rowSums(myData1[,RoofmatR])
myData1$techootro<-NULL
myData1$techocane<-NULL
myData1$techoentrepiso<-NULL
myData1$techozinc<-NULL
myData1$RoofMaterial <-as.ordered(myData1$RoofMaterial)
#FloorMaterial
myData1$pisonatur[myData1$pisonatur==1]<-2
myData1$pisoother[myData1$pisoother==1]<-3
myData1$pisocemento[myData1$pisocemento==1]<-4
myData1$pisomoscer[myData1$pisomoscer==1]<-5
floorMateriaR<-c("pisonotiene","pisonatur","pisoother","pisocemento","pisomoscer")
myData1$FloorMaterial<-rowSums(myData1[,floorMateriaR])
myData1$pisonotiene<-NULL
myData1$pisonatur<-NULL
myData1$pisoother<-NULL
myData1$pisocemento<-NULL
myData1$pisomoscer<-NULL
myData1$FloorMaterial <-as.ordered(myData1$FloorMaterial)
#Energysource
myData1$energcocinar4[myData1$energcocinar4==1]<-2
myData1$energcocinar3[myData1$energcocinar3==1]<-3
myData1$energcocinar2[myData1$energcocinar2==1]<-4
EnergysouR<-c("energcocinar1","energcocinar4","energcocinar3","energcocinar2")
myData1$Energysource<-rowSums(myData1[,EnergysouR])
myData1$energcocinar1<-NULL
myData1$energcocinar4<-NULL
myData1$energcocinar3<-NULL
myData1$energcocinar2<-NULL
myData1$Energysource <-as.ordered(myData1$Energysource)
#Rubbish
myData1$elimbasu5[myData1$elimbasu5==1]<-2
myData1$elimbasu3[myData1$elimbasu3==1]<-3
myData1$elimbasu4[myData1$elimbasu4==1]<-4
myData1$elimbasu2[myData1$elimbasu2==1]<-5
myData1$elimbasu1[myData1$elimbasu1==1]<-6
DisposalR<-c("elimbasu6","elimbasu5","elimbasu3","elimbasu4","elimbasu2","elimbasu1")
myData1$Disposal<-rowSums(myData1[,DisposalR])
myData1$elimbasu6<-NULL
myData1$elimbasu5<-NULL
myData1$elimbasu3<-NULL
myData1$elimbasu4<-NULL
myData1$elimbasu2<-NULL
myData1$elimbasu1<-NULL
myData1$Disposal <-as.ordered(myData1$Disposal)
#Education
myData1$instlevel2[myData1$instlevel2==1]<-2
myData1$instlevel3[myData1$instlevel3==1]<-3
myData1$instlevel4[myData1$instlevel4==1]<-4
myData1$instlevel5[myData1$instlevel5==1]<-5
myData1$instlevel6[myData1$instlevel6==1]<-6
myData1$instlevel7[myData1$instlevel7==1]<-7
myData1$instlevel8[myData1$instlevel8==1]<-8
myData1$instlevel9[myData1$instlevel9==1]<-9
EducationR<-c("instlevel1","instlevel2","instlevel3","instlevel4","instlevel5","instlevel6","instlevel7","instlevel8","instlevel9")
myData1$Education<-rowSums(myData1[,EducationR])
myData1$instlevel1<-NULL
myData1$instlevel2<-NULL
myData1$instlevel3<-NULL
myData1$instlevel4<-NULL
myData1$instlevel5<-NULL
myData1$instlevel6<-NULL
myData1$instlevel7<-NULL
myData1$instlevel8<-NULL
myData1$instlevel9<-NULL
myData1$Education <-as.ordered(myData1$Education)

#House
myData1$tipovivi4[myData1$tipovivi4==1]<-2
myData1$tipovivi3[myData1$tipovivi3==1]<-3
myData1$tipovivi2[myData1$tipovivi2==1]<-4
myData1$tipovivi1[myData1$tipovivi1==1]<-4
HouseR<-c("tipovivi5","tipovivi4","tipovivi3","tipovivi2","tipovivi1")
myData1$House<-rowSums(myData1[,HouseR])
myData1$tipovivi5<-NULL
myData1$tipovivi4<-NULL
myData1$tipovivi3<-NULL
myData1$tipovivi2<-NULL
myData1$tipovivi1<-NULL
myData1$House <-as.ordered(myData1$House)

myData1$computer <-as.factor(myData1$computer)
myData1$television <-as.factor(myData1$television)
myData1$mobilephone <-as.factor(myData1$mobilephone)
myData1$male <-as.factor(myData1$male)
myData1$Target <-as.ordered(myData1$Target)
#***********Dataset Paritiotion*****
set.seed(101)
ind<-sample(2,nrow(myData1), replace = TRUE,prob = c(0.8,0.2))
Traindata<- myData1[ind==1,]
Testdata<- myData1[ind==2,]
nrow(Traindata)
nrow(Testdata)
nrow(myData1)
set.seed(101)
head(Traindata$Target)

#*************PCA Analysis***************
library(caret)
Prici<-preProcess(x=Traindata,method='pca',pcaComp = 2)
(training_set<-predict(Prici,Traindata))
training_set<-training_set[c("PC1","PC2","Target")]
head(training_set)
(Testing_set<-predict(Prici,Testdata))
Testing_set<-training_set[c("PC1","PC2","Target")]
head(Testing_set)

#***************STEP 4: Desigining the Model **************
#Model<- clm(Target~v18q+escolari+ sanitario+OutsideMaterial+Water+Electricity+Walls+Roof+FloorCon+RoofMaterial+FloorMaterial+ Disposal+ Energysource+Education+ House+rooms+dis+male+hogar_nin+hogar_total+r4h3+r4m3+overcrowding+age+ hacapo+v14a+refrig+r4h3+r4m3+area1+computer+television+mobilephone+parentesco1+parentesco2+parentesco3+estadocivil1+estadocivil2+estadocivil3+estadocivil4+estadocivil6+estadocivil7,data = myData1)
#summary(Model)
#Model1<- polr(Target~.,data = training_set,Hess = TRUE,method= "logistic")
#summary(Model1)

Model1<- polr(Target~v18q+escolari+ sanitario+OutsideMaterial+Water+Electricity+Walls+Roof+FloorCon+RoofMaterial+FloorMaterial+ Disposal+ Energysource+Education+ House+rooms+dis+male+hogar_nin+hogar_total+r4h3+r4m3+overcrowding+age+ hacapo+v14a+refrig+r4h3+r4m3+area1+computer+television+mobilephone+parentesco1+parentesco2+parentesco3+estadocivil1+estadocivil2+estadocivil3+estadocivil4+estadocivil6+estadocivil7,data = Traindata,Hess = TRUE,method= "logistic")
summary(Model1)
#***** Model Selection *************
Stepmodel<- step(Model1,direction = "both")
Step<-stepAIC(Model1,trace= FALSE)
Step$anova

Model2<- polr(Target~v18q + escolari + sanitario + OutsideMaterial + Water + Electricity + Walls + Roof + FloorCon + RoofMaterial + FloorMaterial + Disposal + Energysource + House + rooms + dis + male + hogar_nin + hogar_total + r4h3 + r4m3 + age + hacapo + area1 + computer + television + mobilephone + parentesco3 + estadocivil1 + estadocivil4 + estadocivil6,data = Traindata,Hess = TRUE,method= "logistic")
summary(Model2)

#*********** P-value Calculation********
(ctable<- coef(summary(Model1)))
p<- pnorm(abs(ctable[,"t value"]),lower.tail = FALSE)*2
(ctable<- cbind(ctable,"p value" =p))

#*****Prediction**********
pred<-predict(Model1,Testdata,type = "class")
head(pred)
tab<-table(pred,Testdata$Target)
tab
Accuracey<-sum(diag(tab))/sum(tab)
Accuracey
table(Testdata$Target)
print(pred,digits = 3)

# Model Performance 
pred<-predict(Model1,Testdata,type = 'probs')
head(pred)
head(Testdata$Target)
#******************
library(factoextra)
res.pca<-prcomp(myData1[sapply(myData1,function(x) !is.factor(x))],scale. = TRUE)

fviz_eig(res.pca)
fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             select.var = list(contrib = 30), # top 30 contributing
             repel = TRUE     # Avoid text overlapping
)


#***************STEP 5: Optimizing the Model **************

# Step 5.1 : Creating the Matrix for Training and testing data
trainm<-sparse.model.matrix(Target~v18q+escolari+ sanitario+OutsideMaterial+Water+Electricity+Walls+Roof+FloorCon+RoofMaterial+FloorMaterial+ Disposal+ Energysource+Education+ House+rooms+dis+male+hogar_nin+hogar_total+r4h3+r4m3+overcrowding+age+ hacapo+v14a+refrig+r4h3+r4m3+area1+computer+television+mobilephone+parentesco1+parentesco2+parentesco3+estadocivil1+estadocivil2+estadocivil3+estadocivil4+estadocivil6+estadocivil7, data=Traindata)
head(trainm)
train_label <-Traindata[,"Target"]
nc<-length(unique(train_label))

train_matrix<-xgb.DMatrix(data = as.matrix(trainm),label = as.integer(train_label)-1)

testm<-sparse.model.matrix(Target~v18q+escolari+ sanitario+OutsideMaterial+Water+Electricity+Walls+Roof+FloorCon+RoofMaterial+FloorMaterial+ Disposal+ Energysource+Education+ House+rooms+dis+male+hogar_nin+hogar_total+r4h3+r4m3+overcrowding+age+ hacapo+v14a+refrig+r4h3+r4m3+area1+computer+television+mobilephone+parentesco1+parentesco2+parentesco3+estadocivil1+estadocivil2+estadocivil3+estadocivil4+estadocivil6+estadocivil7, data=Testdata)
head(testm)
test_label <-Testdata[,"Target"]
test_matrix<-xgb.DMatrix(data = as.matrix(testm),label = test_label)

#*******Step 5.2 : Creating the Parametres for Model *********** 
xgb_params = list(
  objective = 'multi:softprob',
  num_class = 4,
  eta = 0.1)
xgb_params
#******* Step 5.3 : Buliding the Model ********
#******Step 5.3.1 :Tuning The model *********
cv = xgb.cv(
  data = train_matrix, 
  nfold = 5, 
  nrounds = 15000, 
  verbose = FALSE, 
  maximize = FALSE,
  early_stopping_rounds = 25, 
  params = xgb_params,
  tree_method = "hist",
  nthread = 4
)
cv
#*********Step 5.3.2 :extreme Gradient Boosting Model training ****
bst_model<-xgb.train(params = xgb_params,
                     data =train_matrix,
                     nrounds = round(which.min(cv$evaluation_log$test_merror_mean)*1.1,0), 
                     verbose = FALSE, 
                     maximize = FALSE,
                     seed=333,
                     tree_method = "hist"
)

bst_model
#************Step 5.3.3 :Predcting the target varible from the learnt model**********
pred<-matrix(predict(bst_model,test_matrix), ncol = 4, byrow = TRUE)
library("stats")
library("datasets")
pred1<-prediction(bst_model,test_matrix,type = "prob")
head(pred1)
pred
confusionMatrix(factor(apply(pred,1,which.max)), factor(Testdata$Target))
predvalues<-matrix(pred,nrow = nc,ncol=length(pred)/nc) %>%
  t() %>%
  data.frame() %>%
  mutate(label = test_label,max_prob=max.col(.,"last")-1)
head(predvalues)
table(prediction=predvalues$max_prob, Actual =predvalues$label)

#****Step 5.4 : View feature importance or influence from the learnt model***********
importance_matrix<-xgb.importance(model = bst_model)
xgb.plot.importance(importance_matrix[1:20,])

