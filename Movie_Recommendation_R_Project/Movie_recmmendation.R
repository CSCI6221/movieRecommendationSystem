library(data.table)
library(reshape2)
library(recommenderlab)
library(ggplot2)                       

movies <- read.csv("Dataset/movies.csv",stringsAsFactors=FALSE)
ratings <- read.csv("Dataset/ratings.csv")

moviesGenres <- as.data.frame(tstrsplit(as.data.frame(movies$genres, stringsAsFactors=FALSE)[,1], '[|]', 
                                        type.convert=TRUE), 
                              stringsAsFactors=FALSE) 
colnames(moviesGenres) <- c(1:10)
genres <- c("Action", "Adventure", "Animation", "Children", 
            "Comedy", "Crime","Documentary", "Drama", "Fantasy",
            "Film_Noir", "Horror", "Musical", "Mystery","Romance",
            "Sci_Fi", "Thriller", "War", "Western")
genreMatrix <- matrix(0,10330,18)
genreMatrix[1,] <- genres
colnames(genreMatrix) <- genres

for (index in 1:nrow(moviesGenres)) {
  for (col in 1:ncol(moviesGenres)) {
    genre_colm = which(genreMatrix[1,] == moviesGenres[index,col]) 
    genreMatrix[index+1,genre_colm] <- 1
  }
}
genreMatrix2 <- as.data.frame(genreMatrix[-1,], stringsAsFactors=FALSE) 
for (col in 1:ncol(genreMatrix2)) {
  genreMatrix2[,col] <- as.integer(genreMatrix2[,col]) 
} 


matrixOfRatings <- dcast(ratings, userId~movieId, value.var = "rating", na.rm=FALSE)
matrixOfRatings <- as.matrix(matrixOfRatings[,-1]) 
matrixOfRatings <- as(matrixOfRatings, "realRatingMatrix")


usefulRatings <- matrixOfRatings[rowCounts(matrixOfRatings) > 50,
                                 colCounts(matrixOfRatings) > 50]

sampleDataSet<- sample(x = c(TRUE, FALSE),
                       size = nrow(usefulRatings),
                       replace = TRUE,
                       prob = c(0.8, 0.2))
trainingSet <- usefulRatings[sampleDataSet, ]
testingSet <- usefulRatings[!sampleDataSet, ]
recommendationModel <- Recommender(data = trainingSet,
                                   method = "IBCF",
                                   parameter = "cosine")


BestRecommendations <- 10 

predictedMovies <- predict(object = recommendationModel,
                           newdata = testingSet,
                           n = BestRecommendations)

r <- testingSet

dimnames(r)
rowCounts(r) ## number of ratings per user
colCounts(r) ## number of ratings per item
colMeans(r) ## average item rating
nratings(r) ## total number of ratings
hasRating(r) ## user-item combinations with ratings



testUser <- predictedMovies@items[[1]] 

movies_For_testUser <- predictedMovies@itemLabels[testUser]
testUser2 <- movies_For_testUser

for (index in 1:10){
  testUser2[index] <- as.character(subset(movies,
                                          movies$movieId == movies_For_testUser[index])$title)
}
testUser2


#evaluate the model

evaluationData <- evaluationScheme(data = usefulRatings, method = "cross-validation", k = 4,
                                   given = 8, goodRating = 3)

train <- getData(evaluationData, "train")
known <- getData(evaluationData, "known")
unknown <- getData(evaluationData, "unknown")
selectedModel <- "IBCF"

evaluation_model <- Recommender(data = getData(evaluationData,"train"), method = selectedModel,
                                parameter = NULL)

predictions <- predict(object = evaluation_model, newdata = getData(evaluationData, "known"),
                       n = 10, type = "ratings")

ModelAccuracy <- calcPredictionAccuracy(x = predictions, data = getData(evaluationData, "unknown"),
                                        byUser = TRUE)

calcPredictionAccuracy(x = predictions, data = getData(evaluationData, "unknown"),
                       byUser = FALSE)
AccuracyResults <- evaluate(x = evaluationData, method = selectedModel, n = seq(10,100,10))

class(AccuracyResults)
plot(AccuracyResults, annotate = TRUE, main = "ROC curve")
plot(AccuracyResults, "prec/rec", annotate = TRUE, main = "Precision-Recall")

models <- list(IBCF_cos = list(name = "IBCF", param = list(method = "cosine")),
               IBCF_cor = list(name = "IBCF", param = list(method = "pearson")),
               UBCF_cos = list(name = "UBCF", param = list(method = "cosine")),
               random = list(name = "RANDOM", param = NULL))

recommendations <- c(1, 5, seq(10, 100,10))
allModels_results <- evaluate(x = evaluationData, method = models, n = recommendations)
plot(allModels_results, annotate = 1, legend = "topleft")
title("ROC curves")

plot(allModels_results, "prec/rec", annotate = 1, legend = "bottomright")
title("Precision-Recall")

