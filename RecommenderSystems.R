
# read and observe the data

ratings = read.csv("product_ratings_data.csv")
dim(ratings) # tall format 

library(tidyverse)
glimpse(ratings)

# to establish recommendation systems, I need to mutate the format of the data
library(recommenderlab)
data_matrix = as(ratings,Class = 'realRatingMatrix')
data_matrix # with 362105 ratings in total

as(data_matrix,'matrix') # to observe ratings with a matrix format

# expoloring the data
colMeans(data_matrix)
rowMeans(data_matrix)
rowSds(data_matrix) # varience in ratings urge me to standardize the data

# but I am gonna move forward without without standardization, "for now"

# lets build a prediction
set.seed(1031)
split = sample(nrow(data_matrix),size = 0.9*nrow(data_matrix))
train = data_matrix[split,]
test = data_matrix[-split,]

dim(train)
class(train)
as(train,'matrix')

# How many products (prod) did user 20150 (u20150) rate?
nratings(train['u20150',]) # 44

# How many user ratings did product 25 (prod_25) receive?
colCounts(train[,'prod_25']) # 3745

#  summaries for the train dataset
summary(getRatings(train))
summary(colCounts(train))
summary(rowCounts(train))

# product specific summaries
summary(getRatings(train[,'prod_100']))

# try again with normalized train data #
train_norm = normalize(train,method='center',row=TRUE)
summary(getRatings(train_norm[,'prod_100']))

  # and assess user similarities (cosine similarity function)
similarity(normalize(train)[1:5,],method = 'cosine')

# There are 3 major models used for recommender systems 


          ### USER BASED COLLABORATIVE FILTERING MODEL(ubcf) ###
recommenderRegistry$get_entry("UBCF", type ="realRatingMatrix")

recom_ubcf = Recommender(train, method='UBCF', 
             parameter=list(method='cosine',nn=25, normalize='center'))

pred_ubcf_topN = predict(recom_ubcf,newdata=test,method='topNList',n=5) # n=5 represents top5 recommendation
getList(pred_ubcf_topN)[1:5]

# user specific evaluations
pred_ubcf = predict(recom_ubcf,newdata=test,type='ratings')
as(test,'matrix')['u10139',] # actual
as(pred_ubcf,'matrix')['u10139',] # prediction


          ### ITEM BASED COLLABORATIVE FILTERING MODEL(ibcf) ###
recommenderRegistry$get_entries(data='realRatingMatrix')$IBCF_realRatingMatrix

recom_ibcf = Recommender(train, method='IBCF', 
                         parameter=list(k=30, method='cosine',
                                        normalize='center'))

pred_ibcf_topN = predict(recom_ibcf,newdata=test,method='topNList',n=5)
getList(pred_ibcf_topN)[1:5]

# user specific evaluations
pred_ibcf = predict(recom_ibcf,newdata=test,type='ratings')
as(test,'matrix')['u10139',] # actual
as(pred_ibcf,'matrix')['u10139',] # prediction


#Recommenderlab package has some handy built-in functions to evaluate different 
#recommender models. Here, I am going to create an evaluation scheme from data_matrix,
#not the train dataset. evaluationScheme() handles splits, k-fold cross validation 
#and bootstrapping under the hood.

set.seed(1031)
es = evaluationScheme(data_matrix,method='split',train = 0.8, given=30)

# use ibcf model
recom = Recommender(getData(es,'train'),method='IBCF')
pred_ibcf = predict(recom,newdata = getData(es,'known'),type='ratings')
accuracy_ibcf = calcPredictionAccuracy(x = pred_ibcf,data = getData(es,'unknown'))
accuracy_ibcf # RMSE 1.30

# use ubcf model
recom = Recommender(getData(es,'train'),method='UBCF')
pred_ubcf = predict(recom,newdata = getData(es,'known'),type='ratings')
accuracy_ubcf = calcPredictionAccuracy(x = pred_ubcf,data = getData(es,'unknown'))
accuracy_ubcf # RMSE 1.18

# use ubcf model with nn=100
ev = evaluate(x = es,method='UBCF',parameter=list(method='cosine',nn=100), type='ratings')
avg(ev) # RMSE 1.16 (performed better with higher nn -default nn is 25)

# use the method "Popular" non-personalized model 
es = evaluationScheme(ratings_matrix,method='split',train = 0.8, given=30)
recom3 = Recommender(getData(es,'train'),method='POPULAR')
pred_ubcf2 = predict(recom3,newdata = getData(es,'known'),type='ratings')
accuracy_ubcf2 = calcPredictionAccuracy(x = pred_ubcf2,data = getData(es,'unknown'))
accuracy_ubcf2




