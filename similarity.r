Cal.similarity <- function(a, b) {
  result <- c( Cos.similarity(a,b),
               Cor.similarity(a,b),
               Cpc.similarity (a,b),
               Msd.similarity(a,b),
               Jaccard.similarity(a,b),
               # NHSM.similarity(a,b),
               AJac.similarity(a,b) )
  return ( unlist(result) )
}

Get.similarity.names <- function() {
  nameList <- c("Cos", "Pearson", "CPC", "MSD", "Jaccard", "aJaccard")
  return ( nameList )
}

# . Cosine 
Cos.similarity <- function(a, b) {
  cnt1 <- 0 #동일 개채를 평가한 것의 갯수
  tmp1 <-0
  tmp2 <-0
  tmp3 <-0
  for(i in 1:length(a)){
    if( !is.na(a[i]) && !is.na(b[i]) ){
      tmp1 <- tmp1 + a[i]*b[i]
      tmp2 <- tmp2 + a[i]*a[i]
      tmp3 <- tmp3 + b[i]*b[i]
      cnt1 <- cnt1+1
    }
  }
  
  if( cnt1 > 0 ){
    return (  tmp1/(sqrt(tmp2*tmp3))   )
    #return ( list("similarity" = tmp1/(sqrt(tmp2*tmp3)) , "cnt" = cnt1 )   )
  }else{
    return ( 0 )
  }
}

# . pearson
Cor.similarity <- function(a, b) {
  cnt1 <- 0 #동일 개채를 평가한 것의 갯수
  tmp1 <-0
  tmp2 <-0
  tmp3 <-0
  rMed <-3
  for(i in 1:length(a)){
    if( !is.na(a[i]) && !is.na(b[i]) ){
      tmp1 <- tmp1 + (a[i]-rMed)*(b[i]-rMed)
      tmp2 <- tmp2 + (a[i]-rMed)*(a[i]-rMed)
      tmp3 <- tmp3 + (b[i]-rMed)*(b[i]-rMed)
      cnt1 <- cnt1+1
    }
  }
  
  if( cnt1 > 0 && (tmp1*tmp2*tmp3)!=0 ){
    return ( tmp1/(sqrt(tmp2*tmp3)) )
  }else{
    return ( 0 )
  }
  # return ( cor(a, b, method = "pearson") )
}

# . Constrained Pearson Correlation (CPC)
Cpc.similarity <- function(a, b) {
  cnt1 <- 0 #동일 개채를 평가한 것의 갯수
  tmp1 <-0
  tmp2 <-0
  tmp3 <-0
  rMed <-3
  for(i in 1:length(a)){
    if( !is.na(a[i]) && !is.na(b[i]) ){
      tmp1 <- tmp1 + (a[i]-rMed)*(b[i]-rMed)
      tmp2 <- tmp2 + (a[i]-rMed)*(a[i]-rMed)
      tmp3 <- tmp3 + (b[i]-rMed)*(b[i]-rMed)
      cnt1 <- cnt1+1
    }
  }
  
  if( cnt1 > 0 && (tmp1*tmp2*tmp3)!=0 ){
    return ( tmp1/(sqrt(tmp2*tmp3)) )
  }else{
    return ( 0 )
  }
}

# . Mean Squared Distance (MSD)
Msd.similarity <- function(a, b) {
  # standardized values [0..1] 
  a <- a/5 
  b <- b/5 
  
  result <- (a-b)*(a-b)
  cntCommon <- sum( (!is.na(a) & !is.na(b)) )
  
  if(  cntCommon > 0  ){
    return( 1 - sum(result, na.rm = T)/cntCommon )   
  }else{
    return ( 0 )
  }
}

# . Jaccard
Jaccard.similarity <- function(a, b){
  cntA <- sum(!is.na(a))
  cntB <- sum(!is.na(b))
  cntCommon <- sum( (!is.na(a) & !is.na(b)) )
  
  if( (cntA+cntB-cntCommon)==0 ){
    return ( 0 )
  }else{
    return ( cntCommon / (cntA+cntB-cntCommon) )
  }
}

#  NHSM(New Heuristic Similarity Model)
NHSM.similarity <- function(a, b, tmpMat) {
  library(robustbase) #rowMedians
  Cal.PSS <- function(a, b, tmpMat){
    proximimity <- 1
    Significance <- 1 
    s1ingularity <- 1 
    
    simPSS <-  
      ( 1 - 1/(1 + exp(-abs(a-b)) ) ) *  
      ( 1/(1 + exp( -(abs(a-colMedians(tmpMat,na.rm=TRUE))*abs(b-colMedians(tmpMat, na.rm=TRUE))) )) ) *
      ( 1 - 1/(1 + exp(- abs((a+b)/2 - mean(tmpMat, na.rm=TRUE)) )) )
    simPSS <- sum(simPSS, na.rm=T)
    
    if (simPSS == 0){
      return ( 1 )
    } else {
      return ( simPSS )  
    }
  }
  Cal.Jaccard2 <- function(a, b){
    cntA <- sum(!is.na(a))
    cntB <- sum(!is.na(b))
    cntCommon <- sum( (!is.na(a) & !is.na(b)) )
    
    if (cntA == 0 || cntB == 0 || cntCommon ==0 ){
      return ( 1 )
    } else {
      return ( abs( cntCommon/(cntA*cntB) ) )
    }
  }
  Cal.URP <- function(a, b){
    urp <- 1 - 1/( 1+ exp(-abs(mean(a, na.rm=TRUE)-mean(b, na.rm=TRUE))*abs(sd(a, na.rm=TRUE)-sd(b, na.rm=TRUE))))
    if (urp == 0){
      return ( 1 )
    } else {
      return ( urp )  
    }
  }
  
  pss <- Cal.PSS(a,b, tmpMat) 
  jaccard <- Cal.Jaccard2(a,b)
  urp <-  Cal.URP(a,b)
  
  return (  pss * jaccard  * urp  )  
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

