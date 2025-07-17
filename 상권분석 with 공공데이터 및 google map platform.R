library(ggplot2)
library(ggmap)
library(readxl)
library(dplyr)
setwd("C:\\Users\\DESKTOP\\Desktop\\work_new_R_2025")
register_google(key = "AIzaSyAaU3_jgZzYP7yT9iS9ElneCMBWSnSSxxw")

getwd()



#데이터준비  - 공공데이터 포털

files <- c("202403","202409","202503")
columns <- c( "상가업소번호", "상호명", "상권업종대분류명", "상권업종중분류명", "상권업종소분류명", "시군구명", "행정동명", "경도", "위도")
ds.total <- NULL
for (i in 1:length(files)) {
  filename <- paste("seoul_", files[i], ".csv", sep="")
  cat("read ", filename, "...\n") # 읽을 파일 이름 출력
  ds <- read.csv(filename) # 엑셀 파일 읽기
  ds <- data.frame(ds) # 데이터프레임으로 변환
  ds <- ds[,columns] # 분석에 필요한 변수만 추출
  ds$수집연월 <- rep(files[i], nrow(ds)) # 데이터 수집 시점
  ds.total <- rbind(ds.total,ds) # 데이터 통합
}
head(ds.total)
tail(ds.total)

# 2.데이터 탐색
str(ds.total)
unique(ds.total$수집연월) # 수집연월
unique(ds.total$상권업종대분류명) # 상권업종 대분류
unique(ds.total$상권업종중분류명) # 상권업종 중분류
unique(ds.total$상권업종소분류명)


# 2-1 2017년도 서울시 업종별 점포수_ 상권업종대분류명에 따른 분류
# NA 포함여부 확인
sum(is.na(ds.total))

# 202503 수집 데이터만 추출
ds.202503 <- subset(ds.total, ds.total$수집연월== 202503)
dim(ds.202503)

# 업종별 점포수(대분류)
store.level_1 <- aggregate(ds.202503[,1],
                           by=list(대분류=ds.202503$상권업종대분류명),
                           FUN=length)
store.level_1
names(store.level_1)[2] = c("count")

#store.level_1 = ds.202503%>% 
#group_by(상권업종대분류명)%>% summarise(count=n())
#store.level_1

#시각화

ggplot(store.level_1, aes(x=reorder(대분류, -count), y=count)) +
  geom_bar(stat="identity", width=0.6, fill="steelblue") + 
  ggtitle("업종별 점포수") + 
  theme(plot.title = element_text(color="black", size=14, 
                                  face="bold"), 
        axis.text.x = element_text(size=7, angle = 45))

# 2-2 2025년도 서울시 업종별 점포수_ 구별
# 구별 점포수
store.region <- aggregate(ds.202503[,1],
                          by=list(구이름=ds.202503$시군구명),
                          FUN=length)
store.region
names(store.region)[2] = c("count")
ggplot(store.region, aes(x=구이름, y=count)) +
  geom_bar(stat="identity", width=0.7, fill="steelblue") +
  ggtitle("구별 점포수") +
  theme(plot.title = 
          element_text(color="black", size=14, face="bold"),
        axis.text.x = element_text(angle = 45))
head(ds.202503)

# 2-3 지도 위에 구별 점포수 표시
store.region.loc <- aggregate(ds.202503[,c("경도","위도")],
                              by=list(구이름=ds.202503$시군구명),
                              FUN=mean)

store.region <- data.frame(store.region, store.region.loc[,2:3])

register_google(key = "AIzaSyC8s3TLTpwZ5tMWFSfGgE9Ip24qbd62_f4")
cen <- c(mean(store.region$경도),mean(store.region$위도))
map <- get_googlemap(center=cen, # 마커 없는 지도 가져오기
                     maptype="roadmap",
                     size=c(640,640),
                     zoom=11)
gmap <- ggmap(map) # 지도를 저장
gmap+geom_point(data=store.region, aes(x=경도,y=위도,size=count),
                alpha=0.5, col="red") +
  scale_size_continuous(range = c(1, 15))+ # 원의 크기 조절
  geom_text(data=store.region, # 지도위에 텍스트 표시
            aes(x=경도,y=위도), # 텍스트 위치 (= 구의 좌표)
            size=3, # 텍스트 크기
            label=store.region$구이름) # 텍스트 내용

# 2-4 점포수가 많은 상위 10개 동 확인
store.dong <- aggregate(ds.202503[,1],
                        by=list(동이름=ds.202503$행정동명),
                        FUN=length)
names(store.dong)[2] = c("count")
head(store.dong)
store.dong <- store.dong[order(store.dong$count, decreasing=T),]
dong.top10 <- store.dong[1:10,]
dong.top10

#시각화
ggplot(dong.top10, aes(x=reorder(동이름, -count), y=count)) +
  geom_bar(stat="identity", width=0.7, fill="steelblue") +
  ggtitle("점포수 많은 상위 10개동") +
  theme(plot.title = element_text(color="black", size=14, face="bold"),
        axis.text.x = element_text(angle = 45))

# 3.기간별 분석
# 3-1 수집 연월별 업종별 점포수 변화
store.change <- aggregate(ds.total[,1],
                          by=list(연월=ds.total$수집연월,
                                  업종대분류=ds.total$상권업종대분류명),
                          FUN=length)
names(store.change)[3] = c("count")
head(store.change)

#시각화
ggplot(store.change, aes(x = 연월, y = count, colour = 업종대분류, group = 업종대분류)) +
  geom_line() +
  geom_point(size = 6, shape = 19, alpha = 0.5) +
  ggtitle("업종별 점포수 변화(대분류)") +
  ylab("점포수") +
  scale_x_discrete(labels = files) +
  theme(plot.title = element_text(color = "black", size = 14, face = "bold"))

# 3-2 구별 점포수의 변화를 분석하시오 

store.gu <- aggregate(ds.total[,1],
                      by=list(연월=ds.total$수집연월,
                              구이름=ds.total$시군구명),
                      FUN=length)
names(store.gu)[3] <- c("count")

#시각화
ggplot(store.gu, aes(x = 연월, y = count, colour = 구이름, group = 구이름)) +
  geom_line() +
  geom_point(size = 6, shape = 19, alpha = 0.5) +
  ggtitle("구별 점포수 변화(대분류)") +
  ylab("점포수") +
  scale_x_discrete(labels = files) +
  theme(plot.title = element_text(color = "black", size = 14, face = "bold"))

# 3-3 2024년 3월과 2025년 3월 두기간의 점포수 변화가 큰 상위 10개의 행정동확인

store.tmp <- aggregate(ds.total[,1],
                       by=list(연월=ds.total$수집연월,
                               동이름=ds.total$행정동명),
                       FUN=length)
names(store.tmp)[3] <- c("count")
store.dong.202403 <- store.tmp[store.tmp$연월==202403,]
names(store.dong.202403)[3] <- c("cnt_2024")
store.dong.202503 <- store.tmp[store.tmp$연월==202503,]
names(store.dong.202503)[3] <- c("cnt_2025")
store.diff <- merge(store.dong.202403[,2:3],store.dong.202503[,2:3])
head(store.diff)

store.diff$diff <- abs(store.diff$cnt_2024-store.diff$cnt_2025)
store.diff <- store.diff[order(by=store.diff$diff,decreasing=T),]
top10 <- store.diff[1:10,1]
top10

store.change <- subset(store.tmp, store.tmp$동이름 %in% top10)
ggplot(store.change, aes(x = 연월, y = count, colour = 동이름, group = 동이름)) +
  geom_line() +
  geom_point(size = 6, shape = 19, alpha = 0.5) +
  ggtitle("점포수 변화 Top 10 동") +
  ylab("점포수") +
  scale_x_discrete(labels = files) +
  theme(plot.title = element_text(color = "black", size = 14, face = "bold"))


# 4상권분석
ds.yeoksam <- subset(ds.total, ds.total$수집연월==202503&
                       ds.total$행정동명 == "역삼1동")
head(ds.yeoksam)
unique(ds.yeoksam$상권업종소분류명)
cen <- c(mean(ds.yeoksam$경도),mean(ds.yeoksam$위도))
ds.change <- aggregate(ds.yeoksam[,1],
                         by=list(업종=ds.yeoksam$상권업종대분류명),
                         FUN=length)
names(ds.change)[2] = c("count")
ds.change <- ds.change[order(by = ds.change$count , decreasing = T),]
top10 <- ds.change[1:3,]
top10
ds.yeoksam.top <- subset(ds.yeoksam,ds.yeoksam$상권업종대분류명 %in% top10$업종)
head(ds.yeoksam)


map <- get_googlemap(center = cen,
                     maptype="roadmap",
                     size=c(640,640),
                     zoom=15)
#Source : https://maps.googleapis.com/maps/api/staticmap?center=37.50022,127.035814&zoom=15&size=640x640&scale=2&maptype=roadmap&key=xxx-DrgE2qfrE9urxjSpPOA

gmap <- ggmap(map) # 지도를 저장
gmap+geom_point(data=ds.yeoksam.top, aes(x=경도,y=위도,color=상권업종대분류명),
                size=1.3,alpha=0.4) +
  labs(x = "Longitude", y="Latitude",
       title="역삼1동 업종별 점포",color="업종")
#역삼1동 커피 상권 분석  

ds.yeoksam.coffee <- subset(ds.yeoksam, ds.yeoksam$상권업종소분류명=="카페")
ds.yeoksam.coffee
gmap + geom_point(data = ds.yeoksam.coffee, aes(x = 경도, y = 위도),
                  size = 1.8, alpha = 0.5, color = "chocolate4")+
  labs(x = "Longitude", y="Latitude",
       title="역삼1동 커피점")


# 두 시점에서 '카페' 점포만 필터링
coffee_202403 <- subset(ds.total,행정동명 == "역삼1동"&수집연월 == 202403& 상권업종소분류명 == "카페")
coffee_202503 <- subset(ds.total, 행정동명 == "역삼1동"&수집연월 == 202503& 상권업종소분류명 == "카페")
head(coffee_202503)
# 새로 생긴 카페
new_cafes <- subset(coffee_202503, !(상가업소번호 %in% coffee_202403$상가업소번호))
nrow(new_cafes)
nrow(closed_cafes)
# 사라진 카페
closed_cafes <- subset(coffee_202403, !(상가업소번호 %in% coffee_202503$상가업소번호))

# 중심 좌표 설정
cen <- c(mean(coffee_202503$경도), mean(coffee_202503$위도))

# 지도 가져오기
map <- get_googlemap(center = cen, maptype = "roadmap", zoom = 15, size = c(640, 640))
gmap <- ggmap(map)

# 시각화
gmap +
  geom_point(data = new_cafes, aes(x = 경도, y = 위도),
             size = 3, alpha = 0.6, color = "forestgreen") +  # 새로 생긴 카페 (초록)
  geom_point(data = closed_cafes, aes(x = 경도, y = 위도),
             size = 3, alpha = 0.6, color = "red") +  # 사라진 카페 (빨강)
  labs(title = "역삼1동 1년간 카페 상권 변동",
       subtitle = "사라진 지점:126 , 새로 생긴 지점 : 159",
       x = "Longitude", y = "Latitude") +
  theme(plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 12))



