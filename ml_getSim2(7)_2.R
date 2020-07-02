
##################################

siCnt <- 7
mat <- as.matrix(trainingRatingMat)
userLen <- c()
rMed <- median(mat, na.rm = TRUE)
maxRating <- 5  
minRating <- 0  

Get.similarity.names <- function() {
  nameList <- c("Cos", "ACos","Pea", "CPC", "MSD", "Jac", "aJac")
  return ( nameList )
}

##################################
### Cal.all.default.similarity ###
##################################

mat <- trainingRatingMat

Cal.all.default.similarity <- function( mat ){
  mat <<- as.matrix(mat)
  
  userLen <<- nrow(mat)
  # rMed <<- median(mat, na.rm = TRUE)
  
  siMat1 <- matrix(data = NA, ncol = userLen, nrow=userLen)  # Cos
  siMat2 <- matrix(data = NA, ncol = userLen, nrow=userLen)  # Adjusted Cos
  siMat3 <- matrix(data = NA, ncol = userLen, nrow=userLen)  # Pearson
  siMat4 <- matrix(data = NA, ncol = userLen, nrow=userLen)  # Constrined Pearson C
  siMat5 <- matrix(data = NA, ncol = userLen, nrow=userLen)  # MSD  
  siMat6 <- matrix(data = NA, ncol = userLen, nrow=userLen)  # Jaccard
  siMat7 <- matrix(data = NA, ncol = userLen, nrow=userLen)  # aJaccard
  
  tmp <- list()
  for( i in 1:(userLen-1) ) {  # 610
    cat(i, "(getSim)\t")
    print( Sys.time() )
    
    tmp <- apply(mat[i:userLen,], 1, function(x) { Cal.similarity(mat[i,], x) } )
    tmp <- matrix(unlist(tmp), ncol = siCnt, byrow = TRUE)
    
    siMat1[, i] <- c(rep(NA,i-1), unlist(tmp[,1]))
    siMat2[, i] <- c(rep(NA,i-1), unlist(tmp[,2]))
    siMat3[, i] <- c(rep(NA,i-1), unlist(tmp[,3]))
    siMat4[, i] <- c(rep(NA,i-1), unlist(tmp[,4]))
    siMat5[, i] <- c(rep(NA,i-1), unlist(tmp[,5]))
    siMat6[, i] <- c(rep(NA,i-1), unlist(tmp[,6]))
    siMat7[, i] <- c(rep(NA,i-1), unlist(tmp[,7]))
  }
  
  siMat1 <- round(siMat1, 5)
  siMat2 <- round(siMat2, 5)
  siMat3 <- round(siMat3, 5)
  siMat4 <- round(siMat4, 5)
  siMat5 <- round(siMat5, 5)
  siMat6 <- round(siMat6, 5)
  siMat7 <- round(siMat7, 5)
  
  siMat1[upper.tri(siMat1)] <- t(siMat1)[upper.tri(t(siMat1), diag=FALSE)]
  siMat2[upper.tri(siMat2)] <- t(siMat2)[upper.tri(t(siMat2), diag=FALSE)]
  siMat3[upper.tri(siMat3)] <- t(siMat3)[upper.tri(t(siMat3), diag=FALSE)]
  siMat4[upper.tri(siMat4)] <- t(siMat4)[upper.tri(t(siMat4), diag=FALSE)]
  siMat5[upper.tri(siMat5)] <- t(siMat5)[upper.tri(t(siMat5), diag=FALSE)]
  siMat6[upper.tri(siMat6)] <- t(siMat6)[upper.tri(t(siMat6), diag=FALSE)]
  siMat7[upper.tri(siMat7)] <- t(siMat7)[upper.tri(t(siMat7), diag=FALSE)]
  
  siMatList = list( siMat1, siMat2, siMat3, siMat4,  siMat5 , siMat6 , siMat7 )
  
  # save( siMatList , file="C:/Users/haji/Desktop/200518/DB/ml_small_N100_siMatList_1.RData")
  
  # load(  file="C:/Users/haji/Desktop/200518/DB/ml_small_N10_siMatList_1.RData"  ) #siMatList
  
  return ( siMatList )
}

#################################
### default similarity ###
##################################

Cal.similarity <- function(a, b) {
  result <- list( Cos.similarity(a, b),
                  ACos.similarity(a, b),
                  Pea.similarity(a, b),
                  Cpc.similarity(a, b),
                  Msd.similarity(a, b),
                  Jac.similarity(a, b),
                  AJac.similarity(a, b) )
  return ( result )
}


# . Cosine 
Cos.similarity <- function(a, b) {
  tmp1 = sum(a * b, na.rm = T)
  tmp2 = sum(a * a, na.rm = T)
  tmp3 = sum(b * b, na.rm = T)
  
  if( (tmp1 * tmp2 * tmp3) != 0 ){
    return (  tmp1/(sqrt(tmp2*tmp3))   )
    #return ( list("similarity" = tmp1/(sqrt(tmp2*tmp3)) , "cnt" = cnt1 )   )
  }else{
    return ( 0 )
  }
}


# . Adjust Cosine 
ACos.similarity <- function(a, b) {
  avgA = mean(a, na.rm=TRUE)
  avgB = mean(b, na.rm=TRUE)
  
  tmp1 = sum((a - avgA) * (b - avgB), na.rm = T)
  tmp2 = sum((a - avgA) * (a - avgA), na.rm = T) 
  tmp3 = sum((b - avgB) * (b - avgB), na.rm = T) 

  if( (tmp1 * tmp2 * tmp3) != 0 ){
    return (  tmp1/(sqrt(tmp2*tmp3))   )
    #return ( list("similarity" = tmp1/(sqrt(tmp2*tmp3)) , "cnt" = cnt1 )   )
  }else{
    return ( 0 )
  }
}


# . pearson
Pea.similarity <- function(a, b) {
  avgA = mean(a[a&b], na.rm=TRUE)
  avgB = mean(b[a&b], na.rm=TRUE)
  
  tmp1 = sum((a - avgA) * (b - avgB), na.rm = T)
  tmp2 = sum((a - avgA) * (a - avgA), na.rm = T) 
  tmp3 = sum((b - avgB) * (b - avgB), na.rm = T) 
  
  if(  (tmp1 * tmp2 * tmp3) != 0 ){
    return ( tmp1/(sqrt(tmp2*tmp3)) )
  }else{
    return ( 0 )
  }
  # return ( cor(a, b, method = "pearson") )
}


# . Constrained Pearson Correlation (CPC)
Cpc.similarity <- function(a, b) {
  tmp1 = sum((a - rMed) * (b - rMed), na.rm = T)
  tmp2 = sum((a - rMed) * (a - rMed), na.rm = T) 
  tmp3 = sum((b - rMed) * (b - rMed), na.rm = T) 
  
  if(  (tmp1 * tmp2 * tmp3) != 0 ){
    return ( tmp1/(sqrt(tmp2*tmp3)) )
  }else{
    return ( 0 )
  }
}


# . Mean Squared Distance (MSD)
Msd.similarity <- function(a, b) {
  # standardized values [0..1] 
  a <- a/maxRating
  b <- b/maxRating
  
  result <- (a-b)*(a-b)
  cntCommon <- sum( (!is.na(a) & !is.na(b)) )
  
  if(  cntCommon > 0  ){
    return( 1 - sum(result, na.rm = T)/cntCommon )   
  }else{
    return ( 0 )
  }
}


# . Jaccard
Jac.similarity <- function(a, b){
  cntA <- sum(!is.na(a))
  cntB <- sum(!is.na(b))
  cntCommon <- sum( (!is.na(a) & !is.na(b)) )
  
  if( (cntA+cntB-cntCommon)==0 ){
    return ( 0 )
  }else{
    return ( cntCommon / (cntA+cntB-cntCommon) )
  }
}


# . AJaccard
AJac.similarity <- function(a, b){
  lBd <- 2
  hBd <- 5
  
  lA <- (a <= lBd) 
  lB <- (b <= lBd)
  mA <- (a > lBd & a < hBd )
  mB <- (b > lBd & b < hBd )
  hA <- (a >= hBd)
  hB <- (b >= hBd)
  
  result <- c()
  result[1] <- sum(lA&lB,na.rm = T)/sum(lA|lB,na.rm = T)
  result[2] <- sum(mA&mB,na.rm = T)/(sum(!is.na(lA))+sum(!is.na(lB)))
  result[3] <- sum(hA&hB,na.rm = T)/sum(hA|hB,na.rm = T)
  
  return (  mean(result, na.rm = T) )
}

