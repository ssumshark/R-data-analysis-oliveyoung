## 올리브영 공식 홈페이지 데이터 분석
### Data analysis based on Oliveyoung official homepage

Writer : **[Sumin Woo](mailto:wsm9764@naver.com)**

E-Business, Ajou Univ.

<img width="296" alt="r-logo" src="https://user-images.githubusercontent.com/75171481/100538706-c5240780-3274-11eb-97fa-32a78e4126d2.png"> 

## Overview
| wordcloud by brand name | heatmap by category|
| ------ | ------ |
| <img width="512" alt="wordcloud" src="https://user-images.githubusercontent.com/75171481/100538903-f3eead80-3275-11eb-8298-80e03956d1ec.PNG"> | <img width="512" alt="heatmap" src="https://user-images.githubusercontent.com/75171481/100538878-b68a2000-3275-11eb-9856-cf00b4ee150a.PNG"> | 

|  network between brand & category | chi-square testing|
| ------ | ------ |
|<img width="512" alt="network" src="https://user-images.githubusercontent.com/75171481/100538913-02d56000-3276-11eb-9a0f-ffa1344f33bd.PNG">|<img width="512" alt="chi-square" src="https://user-images.githubusercontent.com/75171481/100538923-0ff24f00-3276-11eb-8671-43bda56cece7.PNG">|

## Driven Insight
- Since Olive Young is a company where many brands are located, it was amazing to see **different brands** in different items such as skincare, makeup, body care, and hair care.
- There were brands that offered products across various categories, but we could see that there were many brands that had only **one category**.
- I thought Olive Young was a company only for women, but there were more products **for men** than I thought, and if there are beauty products that men need, it would be good to visit Olive Young.
- Like Daiso, Olive Young was selling **various beauty items** and it was confirmed that the number of products was not small. Therefore, if Daiso's price is so low that you don't want to buy beauty accessories, you can stop by Olive Young.
- There have been many small beauty brands and **start-up beauty brands**, and Olive Young is likely to remain a place to give opportunities to these brands in the future.

## Getting started
### Install dependencies

#### needed packages in R

- rvest
- gridExtra
- wordcloud
- wordcloud2
- RColorBrewer
- igraph


## Raw Data Link
- [Oliveyoung official homepage](https://www.oliveyoung.co.kr/store/main/main.do?oy=0&chlNo=6&chlDtlNo=11&gclid=Cj0KCQiAqo3-BRDoARIsAE5vnaL1KPTvWcPly7_lPU4uRrZHrDaduVJ6DCcoWkgY79FFGQpwHz2JAskaAmCpEALw_wcB)

## Preprocessd Data Link
- [beauty_item_Data](https://drive.google.com/file/d/110TJdLL_ic1YtgsID5dAUcrHCxpNsQxF/view?usp=sharing)
- [body_care_Data](https://drive.google.com/file/d/1eae4hHZXC2ji4ZkOuEfgXHVbsrzDUdcd/view?usp=sharing)
- [facial_care_Data](https://drive.google.com/file/d/1-4RF7pbXT-kVZ2WheqrPjkHP2n6h5b_L/view?usp=sharing)
- [for_man_goods_Data](https://drive.google.com/file/d/1jK4xa8sdktloLBeL1x6Hn1PLjMSnXfOM/view?usp=sharing)
- [hair_care_Data](https://drive.google.com/file/d/1dkovW6HtWW7l_snD__2BQdXVjJM83bqE/view?usp=sharing)
- [item_consist_Data](https://drive.google.com/file/d/1ec-70upeOQOO2a342F96Jt_plFMI4UZz/view?usp=sharing)
- [makeup_goods_Data](https://drive.google.com/file/d/1AOS2uLD8-76q2-Ljy2x24SMDJnH9IlSm/view?usp=sharing)
- [perfume_goods_Data](https://drive.google.com/file/d/1vZ3O3ocxX2I_Wcnb5SKJlyXcgl7JNYn-/view?usp=sharing)
- [skin_care_goods_Data](https://drive.google.com/file/d/1_PF2pMpkcLjDV5OzvIUhFSrnwHdI-o9Y/view?usp=sharing)


