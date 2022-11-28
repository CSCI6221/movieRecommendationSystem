library(proxy)
library(recommenderlab)
library(reshape2)
library(rlang)
movies <- read.csv("Dataset/movies.csv", header = TRUE, stringsAsFactors=FALSE)
ratings <- read.csv("Dataset/ratings.xls", header = TRUE)
movies2 <- movies[-which((movies$movieId %in% ratings$movieId) == FALSE),]

 #input = "'71 (2014)"
 #input2 = "August Rush (2007)"
 #input3 = "Carrie (2013)"
 #selectage = "under18"

movieRecommendation <- function(input,input2,input3,selectage) {
  
  if (selectage == "under18"){
    selectage <- 1
  }else if(selectage == "18-24"){
    selectage <- 18
  }else if(selectage == "25-34"){
    selectage <- 25
  }else if(selectage == "35-44"){
    selectage <- 35
  }else if(selectage == "45-49"){
    selectage <- 45
  }else if(selectage == "50-55"){
    selectage <- 18 
  }else {
    selectage <- 56
  }
 

  ratings <- ratings[ratings[,4] == selectage,] #filter dataset based on age
  
  firstRow <- which(movies2[,2] == input)
  secondRow <- which(movies2[,2] == input2)
  thirdRow <- which(movies2[,2] == input3)

  
  
  ratingmatrix1 <- dcast(ratings, userId~movieId, value.var = "rating", na.rm=FALSE)
  ratingmatrix1 <- ratingmatrix1[,-1]

  length(ratingmatrix1)
  
 userData <- matrix(NA,length(ratingmatrix1))
 userData <- t(userData)
 length(userData) 
 
 
 colnames(userData) <- colnames(ratingmatrix1)
 
 indexofchoice1 = which(colnames(userData) == firstRow)
 indexofchoice2 = which(colnames(userData) == secondRow)
 indexofchoice3 = which(colnames(userData) == thirdRow)
 

 if(is_empty(indexofchoice1)) {
   choice1 = matrix(c(1), nrow=1, ncol=1 )
   colnames(choice1) <- firstRow 
   userData = cbind(userData,choice1)
   userData[firstRow] <- 5
   
   makeup1 = matrix(NA, nrow=nrow(ratingmatrix1), ncol=1 )
   colnames(makeup1) <- firstRow
   ratingmatrix1 = cbind(makeup1,ratingmatrix1)
   
 } else{
   userData[indexofchoice1] <- 5
 }
 

 if(is_empty(indexofchoice2)) {
   choice2 = matrix(c(1), nrow=1, ncol=1)
   colnames(choice2) <- secondRow 
   userData = cbind(userData,choice2)
   userData[secondRow] <- 4
   
   makeup2 = matrix(NA, nrow=nrow(ratingmatrix1))
   colnames(makeup2) <- secondRow
   ratingmatrix1 = cbind(makeup2,ratingmatrix1)
   
 } else{
   userData[indexofchoice2] <- 4
 }

 if(is_empty(indexofchoice3)) {
   choice3 = matrix(c(1), nrow=1, ncol=1)
   colnames(choice3) <- thirdRow
   userData = cbind(userData,choice3)
   userData[thirdRow] <- 3
   
   makeup3 = matrix(NA, nrow=nrow(ratingmatrix1), ncol=1)
   colnames(makeup3) <- thirdRow
   ratingmatrix1 = cbind(makeup3,ratingmatrix1)
   
 } else{
   userData[indexofchoice3] <- 3
 }

  ratingmatrix2 <- rbind(userData,ratingmatrix1)
  ratingmatrix2 <- as.matrix(ratingmatrix2)
  
  ratingmatrix2 <- as(ratingmatrix2, "realRatingMatrix")
  
  #Create  Model
  recommendationModel <- Recommender(ratingmatrix2, method = "UBCF",param=list(method="Cosine",nn=30))
  predicted <- predict(recommendationModel, ratingmatrix2[1], n=10)
  predicted2 <- as(predicted, "list")
  noResult <- data.frame(matrix(NA,1))
  recommendationResult <- data.frame(matrix(NA,10))
  if (as.character(predicted2[1])=='character(0)'){
    noResult[1,1] <- "There is no enough data to recommend a movie. Try agin!"
    colnames(noResult) <- "No results"
    return(noResult) 
  } else {
    for (i in c(1:10)){
      recommendationResult[i,1] <- as.character(subset(movies, 
                                               movies$movieId == as.integer(predicted2[[1]][i]))$title)
    }
  colnames(recommendationResult) <- "Recommended Movies:"
  return(recommendationResult)
  }
}


