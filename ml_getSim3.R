
Cal.all.similarity <- function(a, b) {
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


# . Mean Squared Distance (MSD)
# Msd.similarity <- function(a, b) {
#   cntCommon <- sum( (!is.na(a) & !is.na(b)) )
#   result <-   sum((a-b)*(a-b), na.rm = T)/ cntCommon
# 
#   if(  cntCommon > 0  ){
#     return( 1/(result+1) )
#   }else{
#     return ( 0 )
#   }
# }

# MSD1	0.6694724	0.6794398	0.6848078 0.683653
# MSD2	0.6737505	0.6762215	0.6818461 0.680256


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

