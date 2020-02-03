## similarity1

1. Cosine - Cos.similarity(a,b) </br>
2. Pearson - Cor.similarity(a,b) </br>
3. Constrained Pearson - Cpc.similarity (a,b) </br>
4. MSD(Mean Square Distance) - Msd.similarity(a,b) </br>
5. Jaccard - Jaccard.similarity(a,b) </br>
( NHSM.similarity(a,b) ) </br>
6. A Jaccard - AJac.similarity(a,b) </br>

</br>

**Predicted Rating** </br>
![image](https://user-images.githubusercontent.com/31869418/73638062-04151b00-46ad-11ea-8e28-a6decd66d51c.png) </br>

<hr>

![image](https://user-images.githubusercontent.com/31869418/73638064-05dede80-46ad-11ea-9e83-29f60897e78b.png) </br>

![image](https://user-images.githubusercontent.com/31869418/73638066-07a8a200-46ad-11ea-83b1-cab55c24ed8d.png) </br>

![image](https://user-images.githubusercontent.com/31869418/73638069-09726580-46ad-11ea-8ff6-4e40a1c18886.png) </br>

![image](https://user-images.githubusercontent.com/31869418/73638073-0aa39280-46ad-11ea-9ee0-a19262176ed7.png) </br>

![image](https://user-images.githubusercontent.com/31869418/73638080-0d05ec80-46ad-11ea-9c37-e41aa662afde.png) </br>

![image](https://user-images.githubusercontent.com/31869418/73638086-0ecfb000-46ad-11ea-8c38-bad20b9ff261.png) </br>


<hr>

### 성능 평가 기준 </br>
**MAE(Mean Absolute Error)** : 평균 절대 오차 <br>
$$
MAE=1|R^|∑r^(u,i)∈R^|r(u,i)−r^(u,i)|
$$

<hr>

### 코드 </br>
***main.r***

    Cal.all.similarity <- function() 
전체 사용자의 모든 메소드에 대한 유사도 추출 <br>
각 메소드별로 matrix(사용자x사용자)형태로 전체 유사도가 저장 <br>
return ( siMat <<- list( siMat1, siMat2, siMat3, siMat4 , siMat5, siMat6 ) ) List 에 저장 <br><br>
      
    Get.similarity <- function( userId )
UserID 기준으로 전체 사용자의 메소드별 유사도 추출 ( 행 : 사용자, 열 : 메소드 ) <br>
*TODO*  6개의 유사도 병합 방법 <br><br>

    Predicted.rating <- function(userId, movieId)
예측 평가치 추출 <br>
( 원래 mathod는 평가치가 평가치를 반환하지만, 성능 평가를 위하여 해당부분 제거  ) <br><br>

    Predict.all.ratings <- function()
성능 평가를 위하여, 평가치가 있는 항목만 평가치 예측<br>
실제 평가치와 예측 평가치를 비교하여 유사도 성능 비교<br><

<hr>





