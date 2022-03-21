getwd()
a= matrix(data= c(1:4),nrow=2)
b=matrix(data= c(1:4),nrow=2)
c=matrix(data= c(1,2),nrow=1)
aa = apply(a,1,function(x) {t(x)%*%x})



pre_processing = function(A,B,K){
  if(!is.matrix(A)){
    A = as.matrix(A)  
  }
  if(!is.matrix(B)){
    B = as.matrix(B)
  }
  
  knn = list();
  knn[["features"]] = A
  knn[["response"]] = B
  knn[["k"]] = K
  
  return(knn)
  
}
testA=matrix(c(1:14),nrow = 2)
testB=matrix(c(12:24),nrow = 2)
w=12
g=pre_processing(A = testA,B = testB,K = w)


#D ^ 2 <A, B> = [|A| ^ 2 %*% t(1)] + [1 %*% t(|B| ^ 2)] - [2 * A %*% t(B)]
calculate_distance = function(A,B){
  
  nA = nrow(A)
  nB = nrow(B)
  
  a = apply(A,1, function(row){t(row)%*%row})
  b = apply(B,1 ,function(row){t(row)%*%row})
  
 sqrt(
  matrix(a,nA,1)%*% matrix(rep(1,nB),nrow = 1,ncol = nB)+
  matrix(rep(1,nA),nrow = nA,ncol = 1)%*%matrix(b,1,nB)-
  2*A%*%t(B)  
 )
  
}



knn_regression = function(train_data,response_data,test_data,K_value){
  
  result = pre_processing(train_data,response_data,K_value)
  distance = calculate_distance(result$features,as.matrix(test_data))
  answer = matrix(NA,nrow(test),1)
  (t(apply(distance, 2, rank)<=K_value)%*%result$response)/K_value
}

knn_regression(train$x,train$y,train$x,3)



