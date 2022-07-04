###################################################################################################################
#################### Make reusable Confusion Matrix function ####################### Model accuracy check
###################################################################################################################

my_confusion_matrix <- function(cf_table) {
  true_positive <- cf_table[4]
  true_negative <- cf_table[1]
  false_positive <- cf_table[2]
  false_negative <- cf_table[3]
  accuracy <- (true_positive + true_negative) / (true_positive + true_negative + false_positive + false_negative)
  sensitivity_recall <- true_positive / (true_positive + false_negative) 
  specificity_selectivity <- true_negative / (true_negative + false_positive)
  precision <- true_positive / (true_positive + false_positive) 
  neg_pred_value <- true_negative/(true_negative + false_negative)
  print(cf_table)
  my_list <- list(sprintf("%1.0f = True Positive (TP), Hit", true_positive),
                  sprintf("%1.0f = True Negative (TN), Rejection", true_negative),
                  sprintf("%1.0f = False Positive (FP), Type 1 Error", false_positive),
                  sprintf("%1.0f = False Negative (FN), Type 2 Error", false_negative),
                  sprintf("%1.4f = Accuracy (TP+TN/(TP+TN+FP+FN))", accuracy), 
                  sprintf("%1.4f = Sensitivity, Recall, Hit Rate, True Positive Rate (How many positives did the model get right? TP/(TP+FN))", sensitivity_recall),
                  sprintf("%1.4f = Specificity, Selectivity, True Negative Rate (How many negatives did the model get right? TN/(TN+FP))", specificity_selectivity),                   
                  sprintf("%1.4f = Precision, Positive Predictive Value (How good are the model's positive predictions? TP/(TP+FP))", precision),
                  sprintf("%1.4f = Negative Predictive Value (How good are the model's negative predictions? TN/(TN+FN)", neg_pred_value)
  )
  return(my_list)
}

###################################################################################################################
################################################# Simple Sampling #################################################
###################################################################################################################
data.df <- datda.frame(original_dataset)

sample(1:nrow(data.df), nrow(original_dataset) * 0.75,
       replace = FALSE)  -> sample.index

head(original_dataset[sample.index, ])

############# Regression Data ###############
# trd data: you can find in Illinois Course 4 week 1 "tecaRegressionData.rds"

trd %>%
  tidyr::pivot_longer(cols = 7:13, names_to = 'parent', values_to = 'pctSales') %>%
  dplyr::mutate(parent = abbreviate(parent, 10)) %>%
  ggplot2::ggplot(aes(x = parent, y = pctSales)) +
  ggplot2::geom_boxplot() +
  ggplot2::theme_minimal()

trd %>%
  tidyr::pivot_longer(cols = 7:13, names_to = 'parent', values_to = 'pctSales') %>%
  dplyr::mutate(parent = abbreviate(parent, 6)) %>%
  ggplot2::ggplot(aes(x = parent, y = pctSales)) +
  ggplot2::geom_boxplot() +
  # facet_grid(~lat + long)
  ggplot2::facet_wrap(facets = vars(lat, long), nrow = 2) +
  ggplot2::theme_minimal()


###################################################################################################################
#################################################### Correlation ##################################################
###################################################################################################################
# trd data: you can find in Illinois Course 4 week 1 "tecaRegressionData.rds"

ggplot(trd, aes(x = Fuel_py1, y = totalRevenue)) +
  geom_point()

ggplot(trd, aes(x = Pop_py1, y = totalRevenue)) +
  geom_point()

cor(trd[,c('totalRevenue', 'Fuel_py1')])
cor(trd[,c('totalRevenue', 'Fuel_py1', 'Pop_py1')])

ctrd <- cor(trd %>% dplyr::select(where(is.numeric)))
ctrd

corrplot::corrplot(ctrd,
                   method = 'color', # I also like color, pie and ellipse
                   order = 'hclust', # Orders the variables so that ones that behave similarly are placed next to each other
                   addCoef.col = 'black',
                   number.cex = .6) # Lower values decrease the size of the numbers in the cells


graphics::pairs(trd %>%  # graphics is base r package
        dplyr::select(where(is.numeric)), cex = .1) # See the help documentation for adding in coefficients and histograms.


# for a powerful correlation chart as well
df %>% 
  dplyr::select_if(is.numeric) %>% 
  PerformanceAnalytics::chart.Correlation()

# for a correlation chart (similar function with corrplot)
df %>% 
  dplyr::select_if(is.numeric) %>% 
  GGally::ggcorr(label = TRUE)

# for a correlation pair chart
df %>% 
  dplyr::select_if(is.numeric) %>% 
  GGally::ggpairs()


###########  Using GGally::ggpairs  : Creating Correlation chart at once  (Customizing options included below)
GGally::ggpairs(df)

my_scatter<-function(data,mapping){
  ggplot(data=data,mapping=mapping)+
    geom_jitter(color="red")
}
####Write your own function for the density plot
my_density<-function(data,mapping){
  ggplot(data=data,mapping=mapping)+
    geom_density(alpha=.05,
                 fill="red")
}
#####substitute your functions for the functions that ggpairs() uses to draw the figures

###########  Using GGally::ggpairs  : Creating Correlation chart at once  (Customizing options included below)
## All of your data should be numeric
GGally::ggpairs(df)

my_scatter <- function(data, mapping){
  ggplot(data = data, mapping = mapping)+
    geom_jitter(color = "lightblue")
}
####Write your own function for the density plot
my_density<- function(data, mapping){
  ggplot(data = data, mapping = mapping)+
    geom_density(alpha = 0.05,
                 fill="lightblue")
}
#####substitute your functions for the functions that ggpairs() uses to draw the figures
GGally::ggpairs(df,
                lower=list(continuous = my_scatter),
                diag=list(continuous = my_density))


# correlation plot using ggcorrplot::ggcorrplot
df<- cces %>% select("educ","pid7","pew_religimp")
r<-cor(df,use="complete.obs")
ggcorrplot(r)


###################################################################################################################
#################################################### Linear Regression ############################################
###################################################################################################################

# trd data: you can find in Illinois Course 4 week 1 "tecaRegressionData.rds"

ggplot2::ggplot(trd, aes(x = Fuel_py1, y = totalRevenue)) +
  ggplot2::geom_point() +
  ggplot2::stat_smooth(method = 'lm') +   # this is how you add "lm" smooth line
  ggplot2::theme_minimal()

lm1 <- lm(totalRevenue ~ Fuel_py1, data = trd)
lm1
summary(lm1)  # Adjusted R-squared is the parameter for accuracy. 

ggplot2::ggplot(trd, aes(x = Fuel_py1, y = totalRevenue)) +
  ggplot2::geom_point() +
  ggplot2::expand_limits(x = c(0,1)) +
  ggplot2::stat_smooth(method = 'lm', fullrange = T)


lm2 <- lm(totalRevenue ~ Juicetonics_py1, data = trd)
summary(lm2)

trd %>% 
  tidyr::pivot_longer(cols = c(Fuel_py1, Juicetonics_py1), names_to = 'parent_name', values_to = 'pctRev_py1') %>% 
  ggplot2::ggplot(aes(x = pctRev_py1, y = totalRevenue)) +
  ggplot2::geom_point() +
  ggplot2::stat_smooth(aes(color = parent_name), method = 'lm', fullrange = T, se = F) +
  ggplot2::theme_minimal()

# How to plot linear regression model ----
sjPlot::plot_model(lm2, show.values = TRUE, value.offset = 0.2)

# or you can just simply
par(mfrow = c(2, 2))
plot(model)  # model <- lm(y ~ x, data = data)


########### Linear regression visualization using visreg ############
# this is make possible for you to visualize the categorical values to the viz as well. 
# sample data
library(mosaicData)
data(SaratogaHouses, package = "mosaicData")

houses_lm <- lm(price ~ lotSize + age + landValue + livingArea + bedrooms + bathrooms + waterfront,
                data = SaratogaHouses)

# continouse viz
visreg::visreg(houses_lm, "livingArea", gg = TRUE)

# categorical viz
visreg::visreg(houses_lm, "waterfront", gg = TRUE) +
  ggplot2::scale_y_continuous(label = scales::dollar) +
  ggplot2::labs(title = "Relationship between price and location",
                subtitle = "controlling for lot size, age, land value, bedrooms and bathrooms",
                caption = "source: Saratoga Housing Data (2006)",
                y = "Home Price",
                x = "Waterfront")


###################################################################################################################
############################################# Residuals and Predictions ###########################################
###################################################################################################################

# trd data: you can find in Illinois Course 4 week 1 "tecaRegressionData.rds"

lm1 <- lm(totalRevenue ~ Fuel_py1, data = trd)
summary(lm1)

resids <- trd %>%
  dplyr::select(Fuel_py1, totalRevenue) %>%
  dplyr::mutate(fittedRevenue = -11510 + 35097*Fuel_py1,  # y = mx + b -> y = m(coefficient)x + b(intercept)
         residuals = totalRevenue - fittedRevenue)        # input - y
head(resids)

# Create a dataframe with residuals and identifying information
resids2 <- trd %>%
  dplyr::select(site_name, quarter, Fuel_py1, totalRevenue)

resids2$fittedRevenue <- lm1$fitted.values
resids2$residuals <- lm1$residuals


# Get the five best performing store/quarter combinations
best <- resids2 %>%
  dplyr::arrange(desc(residuals)) %>%
  .[1:5,]
# Get the five worst performing store/quarter combinations
worst <- resids2 %>%
  dplyr::arrange(residuals) %>%
  .[1:5,] %>%
  arrange(desc(residuals))

# Combine the five best and worst into one dataframe and display them
bestWorst <- dplyr::bind_rows(best,worst)
bestWorst

# Create a dataframe of new observations
newObservations <- data.frame(storeName = c('1', '2', '3', '4', '5'),  # Create or bring new store to predict
                              Fuel_py1 = c(.3, .35, .4, .5, .55))      # New stores input value (x)
# Add a new column of predicted values
newObservations$predictedRevenue <- predict(lm1, newObservations)      # add predict based on lm model


###################################################################################################################
################################################ Multiple Regression ##############################################
###################################################################################################################

# trd data: you can find in Illinois Course 4 week 1 "tecaRegressionData.rds"
lm1 <- lm(totalRevenue ~ Fuel_py1, data = trd)
lm2 <- lm(totalRevenue ~ Juicetonics_py1, data = trd)
jtools::export_summs(lm1, lm2) # Create a nice looking table for comparing the regression results

jtools::plot_summs(lm1, lm2)

# Let's combind multiple models in one plot
lm3 <- lm(totalRevenue ~ Fuel_py1 + Juicetonics_py1, data = trd)
summary(lm3)
jtools::export_summs(lm1, lm2, lm3)  # Accuracy got higher with two combinded model

ctrd <- cor(trd %>% 
              dplyr::select(where(is.numeric)))
corrplot::corrplot(ctrd,
         method = 'color', # I also like color, pie and ellipse
         order = 'hclust', # Orders the variables so that ones that behave similarly are placed next to each other
         addCoef.col = 'black',
         number.cex = .6) # Lower values decrease the size of the numbers in the cells

jtools::plot_summs(lm1, lm2, lm3)


lm4 = lm(totalRevenue ~ Fuel_py1 + Juicetonics_py1 + ColdDispensedBeverage_py1 + Lottery_py1, data = trd)
summary(lm4)

lm5 = lm(totalRevenue ~ Fuel_py1 + Juicetonics_py1 + ColdDispensedBeverage_py1, data = trd)
summary(lm5)

jtools::plot_summs(lm1, lm2, lm3, lm4, lm5)  # With this plot, we can identify what model is the highest accurate



# The methodology of multinominal case
# sample
multi.model <- nnet::multinom(lab ~ x2 + x1, data = data, trace = FALSE)


###################################################################################################################
################################################### Dummy Variables ###############################################
###################################################################################################################

# creating 0, 1 data all at once dummy   (this is super good)
diamonds %>% 
  DataExplorer::dummify()


# trd data: you can find in Illinois Course 4 week 1 "tecaRegressionData.rds"

data.frame('quarterNoYear' = c('First', 'Second', 'Third', 'Fourth'), 
           'quarterNoYearSecond' = c(0,1,0,0),
           'quarterNoYearThird' = c(0,0,1,0),
           'quarterNoYearFourth' = c(0,0,0,1))


lm6 <- lm(totalRevenue ~ quarterNoYear, data = trd)
summary(lm6)

trd %>%
  dplyr::group_by(quarterNoYear) %>%
  dplyr::summarize(meanRevenue = mean(totalRevenue)) %>%
  dplyr::ungroup()


lm7 <- lm(totalRevenue ~ Fuel_py1 + Juicetonics_py1 + ColdDispensedBeverage_py1 + quarterNoYear, data = trd)
summary(lm7)


###################################################################################################################
############################################## Logistics Regression Model #########################################
###################################################################################################################

# creating 0, 1 data all at once dummy   (this is super good)
diamonds %>% 
  DataExplorer::dummify()



#data: you can find in Illinois Course 4 week 2 "logistics1.rds"

loyal_table <- table(logistics1$loyalty)
loyal_table[2]/(loyal_table[1]+loyal_table[2])


contrasts(logistics1$loyalty)   # Assign dummy variable 0 or 1 to category



# divide to two partition (one for train, one for test)  # Split data
set.seed(77) 
partition <- caret::createDataPartition(y = logistics1$loyalty, p=.80, list=FALSE) 
data_train <- logistics1[partition,]
data_test <- logistics1[-partition,]
print(nrow(data_train)/(nrow(data_test)+nrow(data_train)))

# Train logistic regression model  
model_train <- glm(loyalty ~ category, family = binomial, data = data_train)
summary(model_train)

# Predict the probabilities of each row on the training data
predict_train <- predict(model_train, newdata = data_train, type='response')
summary(predict_train)
data_train$prediction <- predict_train
head(data_train)

# Confusion/classification matrix for training data
table1 <- table(predict_train>0.5, data_train$loyalty) #prediction on left and truth on top

# Predict and evaluate the model on the test data
table2 <- table(predict_test>.5, data_test$loyalty) #prediction on left and truth on top

# Predict and evaluate with test data
predict_test <- predict(model_train, newdata = data_test, type='response')
summary(predict_test)
data_test$prediction <- predict_test
head(data_test)
table2 <- table(predict_test>.5, data_test$loyalty)




#### from here... business Science ###  023_ppscore_vs_correlationfunnel
# How to get categorical data to model
# two methods introduced.. try both. decision is on you
#########################################

# ppsr package
# * Make the ppscore ----

# data example is from Business Science R tips lesson 023_ppscore_vs_corrlationalfunnel.R
# Categotical variables can be correlated with this method. 

# below code does is -> get pps scord, select right alrotithm for you, define model_type for you (very good one)
library(correlationfunnel)
churn_ppsr_score <- customer_churn_tbl %>%
  dplyr::select(-customerID) %>%
  ppsr::score_predictors(y = 'Churn', do_parallel = TRUE) %>%
  dplyr::as_tibble()

churn_ppsr_score %>% glimpse()

# Now let's visualize above code - you will see what columns or factors are related to your "y" (super good)
customer_churn_tbl %>%
  dplyr::select(-customerID) %>%
  ppsr::visualize_pps(y = "Churn", do_parallel = TRUE)

# and draw correlation graph 
customer_churn_tbl %>%
  dplyr::select(-customerID) %>%
  ppsr::visualize_pps(do_parallel = TRUE) +
  ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))



######### another way with correlationfunnel package
# How to get your categotical data turn into 0, 1 data all at once using correlationfunnel::binarize()
customer_churn_tbl %>%
  dplyr::select(-customerID) %>%
  dplyr::mutate(TotalCharges = ifelse(is.na(TotalCharges), 0, TotalCharges)) %>%
  correlationfunnel::binarize() -> customer_churn_binned_tbl

# How to get your correlation funnel plot
customer_churn_binned_tbl %>%
  correlate(target = Churn__Yes) %>%
  plot_correlation_funnel() +
  geom_point(size = 3, color = "#2c3e50") 


######### other methods with recipe package
recipe_spec <- recipes::recipe(Churn ~ ., data = customer_churn_tbl) %>%
  recipes::step_rm(customerID) %>%
  recipes::step_dummy(all_nominal(), -Churn)

recipe_spec %>% 
  recipes::prep() %>% 
  recipes::juice() %>% 
  dplyr::glimpse()

# Need to study how those code works above.. (not in the library yet..)

wflw_fit_xgb <- workflows::workflow() %>%
  workflows::add_model(boost_tree(mode = "classification") %>% set_engine("xgboost")) %>%
  workflows::add_recipe(recipe_spec) %>%
  parsnip::fit(customer_churn_tbl)

wflw_fit_xgb$fit$fit$fit %>% vip::vip()



##### Logistics Regresion with visreg package (glm visualization)
# sample data
data(CPS85, package = "mosaicData")
cps85_glm <- glm(married ~ sex + age + race + sector,
                 family = "binomial",
                 data = CPS85)

visreg::visreg(cps85_glm,
               "age",
               gg = TRUE,
               scale = "response") +
  ggplot2::labs(title = "Relationship of age and marital status",
                subtitle = "controlling for sex, race, and job sector",
                caption = "source: Current Population Survey 1995",
                x = "Age",
                y = "Prob(Married)")

# Same plot for glm, but how to face wrap? 

visreg::visreg(cps85_glm,
               "age",
               gg = TRUE,
               scale = "response",
               by = "sex") +
  ggplot2::labs(title = "Relationship of age and marital status",
                subtitle = "controlling for sex, race, and job sector",
                caption = "source: Current Population Survey 1995",
                x = "Age",
                y = "Prob(Married)") 



#####################################################################################################################
###################################################### tieymodels ###################################################
#####################################################################################################################

# From  here, Using tidymodels packages
############# Continuous Variable Prediction ##
# example data: iris

# Step 0: Example of Continuous Variable Prediction
set.seed(1234)
split_iris <- rsample::initial_split(iris)  # (prop = )defalut is 3/4 which I think is better

training_iris <- rsample::training(split_iris)
testing_iris  <- rsample::testing(split_iris)


# step1: Specifty variables with the recipe() function
first_recipe <- training_iris %>% 
  recipes::recipe(Sepal.Length ~ Sepal.Width + Species)


first_recipe <- recipes::recipe(training_iris) %>%
  recipes::update_role(Sepal.Length, new_role = "outcome") %>% 
  recipes::update_role(Sepal.Width, new_role = "predictor") %>% 
  recipes::update_role(Species, new_role = "predictor")
  
summary(first_recipe)

# step2: Specify the preprocessing steps with step *() functions
first_recipe %>% 
  recipes::step_dummy(Species, one_hot = TRUE) -> first_recipe

# step3: Example of optionally performing the preprocessing to see how it influences the data
first_recipe %>% 
  recipes::prep(verbose = TRUE, retain = TRUE) -> prepped_rec

names(prepped_rec)
prepped_rec$var_info

prepped_rec %>% 
  recipes::bake(new_data = NULL) -> preproc_train

preproc_train %>% 
  dplyr::glimpse()


prepped_rec %>% 
  recipes::bake(new_data = testing_iris) -> baked_test_pm

baked_test_pm %>% 
  dplyr::glimpse()


# step4: Example of specifying the model with parsnip
Lin_reg_model <- parsnip::linear_reg() %>% 
  parsnip::set_engine("lm") %>% 
  parsnip::set_mode("regression") 

# step5: Example of fitting the model
iris_reg_wflow <- workflows::workflow() %>% 
  workflows::add_recipe(first_recipe) %>% 
  workflows::add_model(Lin_reg_model)

iris_reg_wflow_fit <- parsnip::fit(iris_reg_wflow, data = training_iris)
iris_reg_wflow_fit

# step6: Example of assessing the model performance
wf_fit <- iris_reg_wflow_fit %>% 
  workflows::extract_fit_parsnip()

head(wf_fit$fit$fitted.values)

predict(iris_reg_wflow_fit, new_data = training_iris)

wf_fitted_values <- broom::augment(wf_fit$fit, data = preproc_train) %>% 
  dplyr::select(Sepal.Length, .fitted:.std.resid)

yardstick::rmse(wf_fitted_values,
                truth = Sepal.Length,
                estimate = .fitted)

wf_fitted_values %>% 
  ggplot2::ggplot(mapping = aes(x = Sepal.Length, y = .fitted)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = "lm") +
  ggplot2::labs(x = "True Sepal Length", y = "Predicted Sepal Length")

overallfit <- iris_reg_wflow %>% 
  tune::last_fit(split_iris)

workflowsets::collect_metrics(overallfit)



############# Categorical Variable Prediction ##

# Step 0: Example of Categorical Variable Prediction
# example data: iris

set.seed(1234)
split_iris <- rsample::initial_split(iris, strata = Species)  # (prop = )defalut is 3/4 which I think is better

training_iris <- rsample::training(split_iris)
testing_iris  <- rsample::testing(split_iris)

dplyr::count(training_iris, Species)
dplyr::count(testing_iris, Species)


# Step 1: set multiple cases
vfold_iris <- rsample::vfold_cv(data = training_iris,
                                v = 4)

dplyr::pull(vfold_iris, splits)

# Step 2: Cross validation
first_fold <- vfold_iris$splits[[1]]
as.data.frame(first_fold, data = "analysis") %>% head()   # training set of this fold
as.data.frame(first_fold, data = "assessment") %>% head() # test set of this fold

# Step 3: Creating categorical variables as the outcome
cat_recipe <- training_iris %>% recipes::recipe(Species ~ .)


# Step 4: Specify our model
cat_model <- parsnip::decision_tree() %>% 
             parsnip::set_mode("classification") %>% 
             parsnip::set_engine("rpart")

cat_model

# Step 5: We will make a workflow for this!





###################################################################################################################
################################################ K-Nearest Model ##################################################
###################################################################################################################

#data: you can find in Illinois Course 4 week 3 "knn_input.rds"

# Explore the target feature
freq <- table(knn_input$high_gpm)
freq[2]/(freq[1]+freq[2])
contrasts(knn_input$high_gpm)

# Partition the data
set.seed(77)
partition <- caret::createDataPartition(y=knn_input$high_gpm, p=.80, list=FALSE)
data_train <- knn_input[partition, ]
data_test <- knn_input[-partition, ]

# Separate the target variable
X_train <- data_train %>% dplyr::select(-high_gpm)
X_test <-  data_test %>% dplyr::select(-high_gpm) 
y_train <- data_train$high_gpm
y_test <- data_test$high_gpm

# z-score standardization
X_train <- scale(X_train)
X_test <- scale(X_test)

# Double check sizes
nrow(X_train)/(nrow(X_test)+nrow(X_train))
dim(X_train)
length(y_train)

# Run the model
knn1 <- class::knn(train = X_train, test = X_test, cl = y_train, k = 141)  # how you choose k number is based on your decision/ domain knowledge


# Chcking the accuracy of the model 
caret::confusionMatrix(knn1, y_test, positive='high')

# Evaluate the data 
data_test$prediction <- knn1
data_test <- data_test %>% 
               dplyr::mutate(correct = high_gpm == prediction)


###################################################################################################################
################################################# Decision Tree Model #############################################
###################################################################################################################

#data: you can find in Illinois Course 4 week 3 "tree_input.rds"

# Explore the target feature # Assign dummy variables
freq <- table(tree_input$high_gpm)
freq[2]/(freq[1]+freq[2])
contrasts(tree_input$high_gpm)

# Run the tree model 
model_tree <- tree::tree(high_gpm ~ ., data_train)

# Predict the tree model on the holdout testing data ----
predict_tree <- predict(model_tree, data_test, type='class') 

# Summarize the results from our model
summary(model_tree)


# Plot the tree 
plot(model_tree)
text(model_tree, all=TRUE, cex=.80, font=2, digits=2, pretty=0)


###################################################################################################################
############################################### K-means Cluster model #############################################
###################################################################################################################

#data: you can find in Illinois Course 4 week 4 "clustering_input2.rds"

# K-means clustering model - # Z-score standardization ----
clustering_input2 <- as.data.frame(scale(clustering_input2))

# Run the model of K-means
set.seed(777)  # You can pick any number here
clusters <- kmeans(clustering_input2, centers=7, iter.max=20, nstart=1)

# Getting the table of K-means clustering model
clusters$centers -> cluster_center
clusters$size -> cluster_size
data.frame(cluster_size) -> cluster_size

cbind(cluster_center, cluster_size) -> cluster_table

# Add cluster to the original a more complete dataframe and add labels
clustering_input1 <- read_rds('clustering_input1.rds') 

clustering_input1$cluster <- clusters$cluster

clustering_input1 %<>%
  dplyr::mutate(cluster_labels = dplyr::case_when(cluster == 1 ~ 'everything',
                                                  cluster == 2 ~ 'fountain soda/coffee',
                                                  cluster == 3 ~ 'gas only',
                                                  cluster == 4 ~ 'cigarettes',
                                                  cluster == 5 ~ 'meals',
                                                  cluster == 6 ~ 'snacks',
                                                  cluster == 7 ~ 'cooler'))


# Visualize the clustering
factoextra::fviz_cluster(clusters, 
                         clustering_input2,  
                         geom = "point", 
                         show.clust.cent = FALSE, 
                         palette = "jco", 
                         ggtheme = theme_classic()) -> cluster_viz



# Party package, decision tree plot ----
party::ctree(y_outcome ~ x_input, #this could be "." for everything
             data = dataset) -> tree
plot(tree)

###################################################################################################################
#################################################### DBSCAN Algorithm #############################################
###################################################################################################################

#data: you can find in Illinois Course 4 week 4 "clustering_input2.rds"
# good for identify and ignore outliers

# Z-score standardization
clustering_input2 <- as.data.frame(scale(clustering_input2))

# Trim the data to a more manageable size
set.seed(777)
clustering_input2_s <- clustering_input2[sample(nrow(clustering_input2), 15000), ]

# run the model 
dbscan::kNNdistplot(clustering_input2_s, k = 7)  # This is your choice for K 
abline(h = 4)   # This is your decision to choose this epsilon number

set.seed(777)
clusters_db <- dbscan(clustering_input2_s, eps = 4, minPts = 7)

# Count clusters
table(clusters_db$cluster)

# Visualize the clusters
factoextra::fviz_cluster(clusters_db, clustering_input2_s,  geom = "point", show.clust.cent = FALSE, palette = "jco", 
                         ggtheme = theme_classic())    # black dots are outliers

# Examine clusters
clustering_input2_s$cluster <- clusters_db$cluster   # 0 is outliers - black dots
clustering_look <- clustering_input2_s %>% 
  dplyr::group_by(cluster) %>% 
  dplyr::summarize(n=n(), across('area.fresh':'refill.yes', mean))

clustering_look


###################################################################################################################
################################################# Regularization Technic ##########################################
###################################################################################################################

lm.lasso <- lasso2::l1ce(mpg ~ ., data = mtcars)
summary(lm.lasso$coefficients)

# how to get right variable for machine learning regression model (Important!!) ----
# for linear regression: use l1ce
# for logistics linear regression: use gl1ce

lm.lasso <- lasso2::l1ce(mpg ~ ., data = mtcars)
summary(lm.lasso)$coefficients

lm.lasso2 <- lasso2::l1ce(mpg ~ cyl + hp + wt + am + carb, data = mtcars)
summary(lm.lasso2)$coefficients

lm.lasso3 <- lasso2::l1ce(mpg ~ cyl + hp + wt, data = mtcars)
summary(lm.lasso3)$coefficients

lm.lasso4 <- lasso2::l1ce(mpg ~ cyl + wt, data = mtcars)
summary(lm.lasso4)$coefficients




###################################################################################################################
######################################################## test #####################################################
###################################################################################################################

# t-test
t.test(dataset, mu = 12)




###################################################################################################################
####################################################  neural network  #############################################
###################################################################################################################

# example data (single data)
set.seed(123)
AND <- c(rep(0, 3), 1)
binary.data <- data.frame(expand.grid(c(0, 1), c(0, 1)), AND)

net <- neuralnet::neuralnet(AND ~ Var1 + Var2, data = binary.data,
                            hidden = 0, err.fct = "ce", linear.output = FALSE)
plot(net, rep = "best")



# example data (multiple data)
set.seed(123)
AND <- c(rep(0, 3), 1)
OR <- c(0, rep(1, 7))
binary.data_2 <- data.frame(expand.grid(c(0, 1), c(0, 1), c(0, 1)), AND, OR)

net_2 <- neuralnet::neuralnet(AND + OR ~ Var1 + Var2 + Var3, data = binary.data_2,
                            hidden = 0, err.fct = "ce", linear.output = FALSE)

plot(net_2, rep = "best")


