library(readxl)
library(dplyr)
jeju <- read_xlsx("jeju.xlsx")
jeju$기준연월=gsub("2022","",jeju$기준연월)
jeju$country=as.factor(jeju$country)
str(jeju)

jeju$season=ifelse(jeju$기준연월%in% c("03","04","05"),"spring",
                   ifelse(jeju$기준연월%in% c("06","07","08"),"summer",
                          ifelse(jeju$기준연월%in% c("09","10","11"),"automn","winter")))
print(jeju)
#country - "1"은 내국인 , "2"는 외국인

#1.계절별 데이터 통계
jeju.cost=jeju%>%group_by(season)%>%summarise(Mean=mean(cost),
                                              Max=max(cost),
                                              Min=min(cost),
                                              n=n(),
                                              Sd=sd(cost))
jeju.cost
str(jeju)
#2.계절별 cost를 boxplot 시각화
boxplot(cost~season,data=jeju,
        frame=FALSE,col=c("blue","yellow","red"),
        horizontal = TRUE)


#3.계절별 여행 비용 아노바 분석
seoson.aov=aov(cost~season,data = jeju)
summary(seoson.aov)
TukeyHSD(seoson.aov)
par(mfrow=c(2,2))
plot(seoson.aov)

#3-2 계절별 + 내&외국인별 여행비용 아노바 분석
seoson.aov2=aov(cost~season+country,data = jeju)
summary(seoson.aov2)
TukeyHSD(seoson.aov2)
par(mfrow=c(2,2))
plot(seoson.aov2)

#정규성이 없는 것으로 판단 따라서 kruskal.test()을 사용해야함 


#1.업종별 데이터 통계
jeju.cost=jeju%>%group_by(업종대분류명)%>%summarise(Mean=mean(cost),
                                              Max=max(cost),
                                              Min=min(cost),
                                              n=n(),
                                              Sd=sd(cost))
jeju.cost
#2.업종별 cost를 boxplot 시각화
boxplot(cost~업종대분류명,data=jeju,
        frame=FALSE,col=c("blue","yellow","red"),
        horizontal = TRUE)

#3.업종별 costf를 아노바 분석석
jeju.aov=aov(cost~업종대분류명,data = jeju)
summary(jeju.aov)
TukeyHSD(jeju.aov)

par(mfrow=c(2,2))
plot(jeju.aov)


mean_value <- mean(jeju$cost)
std_value <- sd(jeju$cost)
threshold <- 2 * std_value  # Threshold for outlier detection

new_jeju <- jeju[jeju$cost > (mean_value - threshold) & jeju$cost < (mean_value + threshold), ]
new_jeju.aov <- aov(cost ~ 업종대분류명, data = new_jeju)

# Plotting after outlier removal
par(mfrow = c(2, 2))
plot(new_jeju.aov)

#업종별 내&외국인별 cost 아노바 분석
jeju.aov2=aov(cost~업종대분류명+country,data=jeju)
summary(jeju.aov2)
plot(jeju.aov2)

#두 개의 아노바 비교

anova_result <- anova(jeju.aov, jeju.aov2)

