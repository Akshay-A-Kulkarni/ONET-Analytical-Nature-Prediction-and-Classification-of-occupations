
# Introduction


The ONET Database: A Primary Source of Occupational Information

The ONET database has a wide variety of worker and job oriented data categories. The ONET Content Model provides the framework that identifies and organizes this important information about work. The O*NET-SOC Occupation Taxonomy covers work performed in the U.S. economy and defines the set of occupations for which data is collected.

The ONet is now the primary source of occupational information. It is sponsored by ETA through a grant to the North Carolina Department of Commerce. Thus, it is a rich database with a sizable ampunt of information of very high quality.

This is script deals with the task of classifying a given occupation from the the ONET database by implementing a model to estimate a probability of an occupation being *Analytical* in nature. After spending some time looking around on the ONET site, Relevant tables that contain possible promising features were identified and data was aqucisitoned, cleaned and transformed to create a feature matrix on which modelling was done to predict an occupation being analytical. 


This RMD script contains:

* Feature Analysis and manipulation.
* Training the model on manually pre-labled data
* Prediction


Checking and Loading required libraries

```{r}
  
#Initialization

packages <- c("tidyverse","readxl","scales","mice","randomForest","MASS","caret","klaR","writexl","modelr","kernlab") # add any packages that need to be installed to this vec.

checkPackage <- function(package_vec){                 # defining a custom function for checking packages.
                            for (p in package_vec){
                                if(p %in% rownames(installed.packages()) == FALSE){
                                    cat(paste(p,"Package is not found/installed on this machine,                  installing the required package... \n"))
                                    install.packages(p,dependencies = TRUE) # Installing with dependancies
                                } else {
                                cat(paste("[",p,"]","is present. \n"))
                              }
                            }
                          }
                          



checkPackage(packages) # running check 




# Load packages

library('MASS')
library('readxl') # Reading data
library('scales') # visualization
library('dplyr') # data manipulation (already loaded with Tidyverse)
library('mice') # needed for possible imputation
library('randomForest') # RF classification algorithm
library('tidyverse') # Data manipulation + Visualization
library('klaR') # for partimat function for plotting discriminant analysis plots
library('writexl') # writing to xlsx format
library('modelr') # for partitioning function
library('caret')
library('kernlab') #Gaussian Process Classification Function
```

# Data collation and Feature Selection:

Before any feature selection can begin setting a base definition about what constitutes as an ANALYTICAL and what aspects might influence the nature of an occupation will be good.

Solutions can be reached by clear-cut, methodical approaches or more creative and lateral angles, depending on the objective. Both ways of solving a problem require analytical skills.
Analytical skills might sound technical, but we use these skills in everyday work when detecting patterns, brainstorming, observing, interpreting data, integrating new information, theorizing, and making decisions based on multiple factors and options available.

therefore features that could capture the analytical nature of an occupation would have descriptions strongly correlating to aspects in :

* Communication.
* Creativity.
* Critical Thinking & Pattern analysis.
* Information Processing.
* Analytics
* Research.
 
 etc.
 
Therefore after systematically observing all tables int the ONET Content model and correlating features by using the Content Model Reference, Various variables from multiple tables such as Ablities, Skills, Work Activities , Interests were identified and further reduced to avoid repeated or correlating features. 

Thus the final feature list was identified as below :

 
SOC Code  |  Selected Feature                                   |  Description
----------|-----------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------
1.A.1.b.2 |	Originality                                         |	The ability to come up with unusual or clever ideas about a given topic or situation, or to develop creative ways to solve a problem.
1.A.1.b.3 |	Problem Sensitivity                                 |	The ability to tell when something is wrong or is likely to go wrong. It does not involve solving the problem, only recognizing there is a problem.
1.A.1.b.5 |	Inductive Reasoning                                 |	The ability to combine pieces of information to form general rules or conclusions (includes finding a relationship among seemingly unrelated events).
1.C.7.a	  | Innovation	                                        | Job requires creativity and alternative thinking to develop new ideas for and answers to work-related problems.
1.C.7.b	  | Analytical Thinking                                 | Job requires analyzing information and using logic to address work-related issues and problems.
2.A.2.a	  | Critical Thinking	                                  | Using logic and reasoning to identify the strengths and weaknesses of alternative solutions, conclusions or approaches to problems.
2.A.2.b	  | Active Learning	                                    | Understanding the implications of new information for both current and future problem-solving and decision-making.
2.B.2.i	  | Complex Problem Solving                             |	Identifying complex problems and reviewing related information to develop and evaluate options and implement solutions.
4.A.2.a.1 |	Judging the Qualities of Things, Services, or People|	Assessing the value, importance, or quality of things or people.
4.A.2.a.4	| Analyzing Data or Information                       |	Identifying the underlying principles, reasons, or facts of information by breaking down information or data into separate parts.
4.A.2.b.1	| Making Decisions and Solving Problems	              | Analyzing information and evaluating results to choose the best solution and solve problems.
4.A.2.b.3	| Updating and Using Relevant Knowledge             	| Keeping up-to-date technically and applying new knowledge to your job.
4.A.2.b.4	| Developing Objectives and Strategies              	| Establishing long-range objectives and specifying the strategies and actions to achieve them.

 
12 variables have been identified that describe analytical attributes. These features do a pretty good job of encompassing the analytical nature of an occupation title from various aspects or facets mentioned in our base definition/assumption.

```
# Loading data (make sure the tables are in the same folder as the rmd)
 
script_folder <-  getwd() # Retrieving the path from where the rmd is being accessed (To eliminate locating files and paths, if run from the Task folder since default behaviour for getwd in a rmd is to give the working directory of the rmd not the global setting)


occupation_rawdata <- read_excel(paste(getwd(),"/Occupation Data.xlsx",sep = "")) # loading Occupation table

skills_rawdata <-  read_excel(paste(getwd(),"/Skills.xlsx",sep = ""))  # loading Skills table

scale_ref <-  read_excel(paste(getwd(),"/Scales Reference.xlsx",sep = "")) # loading Scales ref table

blsdata <-  read_excel(paste(getwd(),"/national_M2017_dl.xlsx",sep = "")) # national BLS data for general EDA

abilities_rawdata <-  read_excel(paste(getwd(),"/Abilities.xlsx",sep = "")) # loading abilities table

workactivities_rawdata <- read_excel(paste(getwd(),"/Work Activities.xlsx",sep = "")) # loading work activity table

interests_rawdata <- read_excel(paste(getwd(),"/Interests.xlsx",sep = "")) # loading interests table

workstyles_rawdata <- read_excel(paste(getwd(),"/Work Styles.xlsx",sep = ""))

CMR <-  read_excel(paste(getwd(),"/Content Model Reference.xlsx",sep = "")) # Content model Reference details

blsdatamay18 <- blsdata %>% mutate(OCC_CODE = paste(OCC_CODE,".00",sep="")) # BLS data for possible EDA





str(skills_rawdata)



library(tidyverse)

skills <- skills_rawdata %>% select(`O*NET-SOC Code`,Title,`Element Name`,`Scale ID`,`Data Value`)%>%
                              filter(`Scale ID` == "LV")%>%  
                                spread(key = `Element Name`, value = `Data Value`)


abilities <- abilities_rawdata %>% select(`O*NET-SOC Code`,Title,`Element Name`,`Scale ID`,`Data Value`)%>%                                  filter(`Scale ID` == "LV")%>%  
                                  spread(key = `Element Name`, value = `Data Value`)

activities <- workactivities_rawdata %>% select(`O*NET-SOC Code`,Title,`Element Name`,`Scale ID`,`Data Value`)%>%                                  filter(`Scale ID` == "LV")%>%  
                             spread(key = `Element Name`, value = `Data Value`)

interests <- interests_rawdata %>% select(`O*NET-SOC Code`,Title,`Element Name`,`Scale ID`,`Data Value`)%>%                                  filter(`Scale ID` == "OI")%>%  
                             spread(key = `Element Name`, value = `Data Value`)

styles <- workstyles_rawdata %>% select(`O*NET-SOC Code`,Title,`Element Name`,`Scale ID`,`Data Value`)%>%                                  filter(`Scale ID` == "IM")%>%  
                             spread(key = `Element Name`, value = `Data Value`)





merged_features_data <- skills %>% left_join(abilities,by =c("O*NET-SOC Code","Title")) %>% left_join(activities,by =c("O*NET-SOC Code","Title"))  %>% left_join(styles,by =c("O*NET-SOC Code","Title"))  # Joining tables with SOC code as the primary key (title is redundant but kept.)



#-------------------------------------------------------------------------------------------------

features <-  c("O*NET-SOC Code","Title",
               "Originality",
               "Problem Sensitivity",
               "Inductive Reasoning",
               "Innovation",
               "Analytical Thinking",
               "Critical Thinking",
               "Active Learning",
               "Complex Problem Solving",
               "Judging the Qualities of Things, Services, or People",
               "Making Decisions and Solving Problems",
               "Updating and Using Relevant Knowledge",
               "Developing Objectives and Strategies")  # Defining a vector of chosen features 





main_features <- merged_features_data %>% dplyr::select(one_of(features)) # picking cols/features of interest. 


head(main_features,10)
write_csv(main_features,paste(getwd(),"/TrainingFeatures.csv",sep=""))

```

Thus we have obtained a clean feature matrix with each row forming a feature vector $\hat{x}$ $\in$ ${\rm I\!R}^{12}$ for $n$ = 967 occupation titles


# Generating Lables for Training

Our feature matrix is missing the binary response variable  $y$ $\in$ ${0,1}$ which denotes whether a particular job is `Analytical`

Therefore  the response variable was manually generated by hand-labeling.
The subjective assessment was done with randomly sampling occupations and comparing them to a psuedo-rubric consisting the base assumption/definition and the attributes that are indicative of the analytical nature of an occupation.

I surmised that the procedure would best carried out together with a cohort of MSDS students (a group of 5 Friends) at Northeastern University in order to  mitigate the unavoidable subjective bias in the data which was natrually bound to arise.
(hand-labelled 167 occupations, assigning 1 if analytical, and 0 if not).
The resulting DF contained the response variable `ANALYTICAL` $\in$ ${0,1}$ otherwise `NA` for unlableled points.


## Loading Labled data

```{r}

Labled_original_data <-  read_csv(paste(getwd(),"/LabledTrainingFeatures.csv",sep = ""), na = "NA")
model_data <- Labled_original_data
names(model_data)<-str_replace_all(names(model_data), c(" " = ".", "," = ""))# renaming cols and removing spaces and commas

str(model_data)
```

# Modelling.

## Randomforest.

As a non-parametric modelling technique `randomForest` theoretically should and does perform well on classifcation problems and since its easy to implement its a great way to build a preliminary model to test initial accuracy and gather more information on importance of the selected features.

```{r}

# RandomForest Analysis

rfdata <- model_data %>% drop_na() # doriiping NA rows basically gives us the part of the data that we manually labled.

set.seed(55)  # for reproducibitlity

partitions <- resample_partition(rfdata,c(train = 0.7,test=0.3)) # partitioning labeled data for training & test 

train_df <- as.tibble(partitions$train) %>% dplyr::select(-`O*NET-SOC.Code`,-`Title`) # removing title and code from training

test_df <- as.tibble(partitions$test)  # coercing to tibble if train/test need to be viewed.

rf1 <- randomForest(as.factor(ANALYTICAL) ~ . , data = train_df, mtry=12 , ntree = 250, keep.forest=TRUE) 
rf1
plot(rf1)
importance(rf1)

# Calculating predictions from train/test of labled data for randomForest

model_test_pred <- test_df %>% mutate("Prediction" = format(predict(rf1,test_df),scientific = FALSE))

model_test_pred_classwise <- test_df %>% mutate("Pred Prob for (0)" = format(predict(rf1,test_df,type = "prob")[,1],scientific = FALSE),
                                   "Pred Prob for (1)" = format(predict(rf1,test_df,type = "prob")[,2],scientific = FALSE)) # adding class probabilities 



model_test_pred <- subset(model_test_pred, select=c(`O*NET-SOC.Code`, `Title`,`ANALYTICAL`,`Prediction`,`Originality`:`Developing.Objectives.and.Strategies`)) # reordering columns

model_test_pred_classwise <- subset(model_test_pred_classwise, select=c(`O*NET-SOC.Code`, `Title`,`ANALYTICAL`,`Pred Prob for (0)`,`Pred Prob for (1)`,`Originality`:`Developing.Objectives.and.Strategies`)) # reordering columns




# Displaying Confusion Matrix

Cmatrix1 <- confusionMatrix(as.factor(model_test_pred$Prediction),as.factor(model_test_pred$ANALYTICAL), dnn = c("Prediction", "Reference"),positive='1')
Cmatrix1
cat("The Accuracy is",Cmatrix1$overall["Accuracy"])




# Predicting values for original partially-labled data 

model_full_pred <- model_data %>% mutate("Prediction" = format(predict(rf1,model_data),scientific = FALSE)) # predicting on unlabled data
 
model_full_pred_classwise <- model_data %>% mutate("Pred Prob for (0)" = format(predict(rf1,model_data,type = "prob")[,1],scientific = FALSE),
                                   "Pred Prob for (1)" = format(predict(rf1,model_data,type = "prob")[,2],scientific = FALSE)) # adding class probabilities 



model_full_pred <- subset(model_full_pred, select=c(`O*NET-SOC.Code`, `Title`,`ANALYTICAL`,`Prediction`,`Originality`:`Developing.Objectives.and.Strategies`)) # reordering columns

model_full_pred_classwise <- subset(model_full_pred_classwise, select=c(`O*NET-SOC.Code`, `Title`,`ANALYTICAL`,`Pred Prob for (0)`,`Pred Prob for (1)`,`Originality`:`Developing.Objectives.and.Strategies`)) # reordering columns


write_xlsx(model_full_pred,paste(getwd(),"/rfPredResult.xlsx",sep=""))
write_xlsx(model_full_pred_classwise,paste(getwd(),"/rfPredResult(classwise).xlsx",sep=""))

```


The model obtained an 88% accuracy against the test data and the importance function indicates that the `Complex Problem Solving` feature/attribute is the most influential variable in the model
indicated by its highest meandecreaseGINI where the scale is irrelevant: only the relative values matter.

(note: the results with predictions and class probs are saved in the cwd)

## Quadratic Discriminant Analysis.

```{r Quadratic Discriminant Analysis.}
# Quadratic Discriminant Analysis.

qdadata <- model_data %>% drop_na()

qda.model <- qda(ANALYTICAL ~ . , train_df)
qda.model

partimat(as.factor(ANALYTICAL) ~ Problem.Sensitivity+Analytical.Thinking+Complex.Problem.Solving+Inductive.Reasoning, data = train_df, method="qda")


qda.test <- predict(qda.model,test_df)
test_df$qda <- qda.test$class


Cmatrix2 <- confusionMatrix(as.factor(test_df$qda),as.factor(test_df$ANALYTICAL), dnn = c("Prediction", "Reference"))
Cmatrix2
cat("The Accuracy is",Cmatrix2$overall["Accuracy"])


# Predicting values for original partially-labled data 

model_full_pred2 <- model_data %>% mutate("Prediction" = (predict(qda.model,model_data))$class) # predicting on unlabled data


model_full_pred2 <- subset(model_full_pred2, select=c(`O*NET-SOC.Code`, `Title`,`ANALYTICAL`,`Prediction`,`Originality`:`Developing.Objectives.and.Strategies`)) # reordering columns


write_xlsx(model_full_pred2,paste(getwd(),"/QDAPredResult.xlsx",sep=""))


```
The model obtained an 92% accuracy which is an improvement over our rF model.
The `partimat()` fucntion provides a multiple figure array which shows the classification of observations based on classification methods (lda, qda, rpart, naiveBayes, rda, sknn and svmlight) for every combination of two variables. Moreover, the classification boundaries are displayed and the apparent error rates are given in each title.


## Gaussian Process Classifier.

Now we get to the main part of this excercise and implement a `Gaussian Process Classifier`.

```{r}

# Gaussian Process Classifier.

gpc_model <- gausspr(as.factor(ANALYTICAL) ~ ., data = train_df, type= 'classification', kernel="anovadot",
          kpar="automatic", var=1, variance.model = FALSE, tol=0.0005,
          cross=0, fit=TRUE, na.action = na.omit)



(gpc_model)

gpc_data_test <- as.tibble(test_df) 

gpc_data_test$Pred <- predict(gpc_model,gpc_data_test[,-3]) # need to remove the response variable from test set before predicting.

# class probabilities 

Cmatrix3 <- confusionMatrix(gpc_data_test$Pred,as.factor(gpc_data_test$ANALYTICAL), dnn = c("Prediction", "Reference"))
Cmatrix3
cat("The Accuracy is",Cmatrix3$overall["Accuracy"])


# Predicting values for original partially-labled data 

model_full_pred3 <- model_data %>% mutate("Prediction" = format(predict(rf1,model_data),scientific = FALSE)) # predicting on unlabled data
 
model_full_pred_classwise <- model_data %>% mutate("Pred Prob for (0)" = format(predict(rf1,model_data,type = "prob")[,1],scientific = FALSE),
                                   "Pred Prob for (1)" = format(predict(rf1,model_data,type = "prob")[,2],scientific = FALSE)) # adding class probabilities 



model_full_pred3 <- subset(model_full_pred3, select=c(`O*NET-SOC.Code`, `Title`,`ANALYTICAL`,`Prediction`,`Originality`:`Developing.Objectives.and.Strategies`)) # reordering columns



write_xlsx(model_full_pred3,paste(getwd(),"/GPCPredResult.xlsx",sep=""))

ggplot(data = filter(model_full_pred3, !is.na(model_full_pred3$Prediction)))+
  geom_bar(aes(x=Prediction,fill = as.factor(Prediction)))



```


The gaussian classifier is far outperforming our other models with 96% accuracy while there should be more analysis done before we can concretely state anything but it does confirm Gaussian Process as a well known powerful and highly flexible classifier. 

The Gaussian process (GP) directly captures the model uncertainty.e.g for regression GP directly gives you a distribution for the prediction value, rather than just one value as the prediction. This uncertainty is not directly captured in neural networks.

It :

*Can learn the kernel parameters automatically from data, no matter how flexible we wish to make the kernel.
*Can learn the regularization parameter C without cross-validation.
*Can incorporate interpretable noise models and priors over functions, and can sample from prior to get intuitions about the model assumptions.
*We can combine automatic feature selection with learning using ARD.


# Conclusions:

ONET as a database has proven be a very informative and resourceful for occupation classification. It contains rich and detailed feature information on occupations and serves as a great repository  fo analyses.

##Q2b.       Evaluation of the results. Do they make sense? What occupations came up as analytical that surprised you? What occupations did you expect to be analytical and didn't come up? What other sources of data could be included to make the model more accurate? 

Answer - Most of the occupations are in accordance with our base definition and are predicted resonably accurately,and the barlpot indicates there are more non-analytical jobs than analytical in the ONET database, and Surprisingly jobs such as "Brodcast News Analyst" & "Cytogenetic Technician" are marked non analytical while "Nurse Midwives" are analytical which is a strech thus there could be some correlation or features we could be missing.

##Q2c.       The assumptions you made and anything you would consider going back and changing in the model. 
Answer - The models can be tuned further and maybe a different choice of kernel can be used for GPC but i would invest more time into feature analysis, selection and generation to improve the models.



```{r Further prediction analysis}

# some data analysis

edadata <-  model_full_pred3

feature_level_ref <-  read_excel(paste(getwd(),"/Level Scale Anchors.xlsx",sep = "")) 

feature_level_ref <-  feature_level_ref %>% filter(`Element Name` %in%(features))

edadata$Prediction <- as.integer(edadata$Prediction)


inaccurate <- edadata %>% filter(ANALYTICAL!= Prediction & !is.na(ANALYTICAL))
inaccurate

x <- ggplot(data = filter(edadata, !is.na(model_full_pred3$Prediction)))+
  geom_bar(aes(x=Prediction,fill = as.factor(Prediction)))

library(plotly) # comment it out if needed

inaccurate_pred <- inaccurate %>% inner_join(occupation_rawdata, by = c("O*NET-SOC.Code"="O*NET-SOC Code","Title"))

select(inaccurate_pred,Title,Complex.Problem.Solving)

test <- inaccurate %>% gather("Feature", "Value", 5:16) # reshaping the df for plotting 


ggplot(data= filter(test, Feature == "Complex.Problem.Solving"),aes(x=Title,y=Value))+  
         geom_bar(stat = "identity",color="orange")+
        aes(stringr::str_wrap(Title, 15)) + xlab(NULL) +
        ylab("Complex Problem Solving")

ggplot(data= filter(test, Feature == "Making.Decisions.and.Solving.Problems"),aes(x=Title,y=Value))+
         geom_bar(stat = "identity",fill="orange")+
        aes(stringr::str_wrap(Title, 15)) + xlab(NULL) +
        ylab("Making.Decisions.and.Solving.Problems")

#plotting all feature values 

ggplot(dplyr::group_by(test,Title), aes(y=Value, x=Feature ,color=Feature)) + 
  geom_bar(stat="identity",show.legend = FALSE)+
  facet_wrap(.~Title)+
  coord_flip()






```


```{r}

install.packages("inTrees")
library(inTrees)
library(randomForest) 
d
```
