


# linear discriminant analysis

library(MASS)

mat = read.table("New Text Document.txt" , sep = "\t" , header = T)


find.acuracy = function(train , test , algorithm) {
  
  model = algorithm(class ~ . , train)
  pr = predict(model , test)
  return(sum(pr$class == test$class) / nrow(test))
  
}


data.partition = function(data , algorithm){
  
  l1 = which(data$class == 1)
  l2 = which(data$class == 0)
  data = data[c(l1 , l2) , ]
  rownames(data) = 1:length(data[,1])
  n1 = round(min(length(l1),length(l2)) * 0.75 , 0)
  N = nrow(mat)
  trainset = sapply(unique(mat$class) , function(x) sample(which(mat$class == x) , n1))
  trainset = sort(as.numeric(trainset))
  testset = setdiff(seq(N) , trainset)
  find.acuracy(data[trainset , ] , data[testset , ] , algorithm)
  
}

results = sapply(1:1000 , function(x) data.partition(mat,lda))
mean(results)





# Support Vector Machine

library(e1071)

model.svm = svm(class ~ . , mat[trainset,] , kernel = "linear")
predict.svm = predict(model.svm , mat[testset , 1:(ncol(mat)-1)])
cat("SVM accuracy is:" , sum(predict.svm == mat[testset,"class"]) / length(testset))




# Randome forest

library(randomForest)

model.RF = randomForest(class ~ . , mat[trainset,])
predict.RF = predict(model.RF , mat[testset , 1:(ncol(mat)-1)])
cat("RF accuracy is:" , sum(predict.RF == mat[testset,"class"]) / length(testset))

                 
                 
