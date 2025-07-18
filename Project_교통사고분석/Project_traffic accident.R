library(tidyverse)
library(readxl)
library(conflicted)
library(dplyr)
library(lubridate)
accident=read_csv("death.csv",locale=locale("ko",encoding="CP949"))
cars_pops=read_excel("cars_pops.xlsx",skip=1)
accident
cars_pops

acc_wk=accident%>%mutate(date=substr(발생년월일시,1,8),
                         hour=substr(발생년월일시,9,10))%>%
  mutate(date=as.Date(date,format="%Y%m%d"))
acc_wk
acc_wk=acc_wk%>%
  mutate(
    day_night=factor(주야,levels=c("주간","야간")),
    days=factor(요일,levels=c("월","화","수","목","금","토","일")),
    acc_cat=factor(사고유형_대분류,levels=c("차대사람","차량단독","차대차",
                                     "철길건널목")),
    road_cat=factor(도로형태_대분류,levels=c("단일로","교차로","주차장",
                                      "철길건널목","불명","기타","기타/불명")))
#기존값과 새로운 값이 잘 매칭되었는지 확인
table(acc_wk$주야,acc_wk$day_night)
#데이터 분석에 불필요한 열 제거
acc_wk=acc_wk%>%select(-c(발생년,발생년월일시,주야,요일,사상자수,부상신고자수,발생위치X_UTMK))
names(acc_wk)
#cars_pops 데이터셋 정리
names(cars_pops)
names(cars_pops)=c("region","cpp","reg_cars","pops")
names(cars_pops);cars_pops$region
#공백문자 제거
cars_pops=cars_pops%>%mutate(region=str_replace_all(cars_pops$region,"　　　",""))
cars_pops$region
#separate사용하여 별도의 열로 분리
cars_pops_wk=cars_pops%>%
  separate(
    region,into=c("regionCode","regionName"),sep=" "
  );cars_pops_wk
# 결합하여 정보확장-시도명 결합
table(acc_wk$발생지시도)
cars_pops_wk%>%dplyr::filter(str_length(regionCode)==2)%>%select(regionCode,regionName)
#단축어 사전 생성
cars_pops_wk%>%dplyr::filter(str_length(regionCode)==2)%>%
  dplyr::filter(regionCode!="00")%>%
  pull(regionName)
cars_pops_wk
abbr_tab=tibble(
  fullName=cars_pops_wk%>%
    dplyr::filter(str_length(regionCode)==2)%>%
    dplyr::filter(regionCode!="00")%>%
    pull(regionName),
  abbrName=c("서울","부산","대구","인천","광주","대전",
             "울산","세종","경기","강원","충북","충남",
             "전북","전남","경북","경남","제주"))
abbr_tab

#시도별 행정 분류 코드 열 추가하기
abbr_tab=abbr_tab%>%left_join(cars_pops_wk%>%
                                select(regionCode,regionName),
                              by=c("fullName"="regionName"))
abbr_tab
cars_pops_wk=cars_pops_wk%>%mutate(sidoCode=substr(regionCode,1,2))
#숫자형 데이터 요약
library(tidyverse)
library(summarytools)
library(dplyr)
library(conflicted)
acc_wk=readRDS("acc_wk.rds")
cars_pops_wk=readRDS("cars_pops_wk.rds")
abbr_tab=readRDS("abbr_tab.rds")

#숫자형 데이터 요약
library(tidyverse)
library(summarytools)
library(dplyr)
library(conflicted)
acc_wk=readRDS("acc_wk.rds")
cars_pops_wk=readRDS("cars_pops_wk.rds")
abbr_tab=readRDS("abbr_tab.rds")

#acc_wk 데이터셋 중 숫자형 데이터 요약
descr(acc_wk,
      stats = "common",
      round.digits = 1)
descr(acc_wk%>%select(사망자수,경상자수,중상자수),
      stats=c("mean","sd","min","q1","med","q3","max"),
      round.digits = 1)
#acc_wk데이터셋을 주간과 야간으로 그룹화
stby(acc_wk%>%select(사망자수,경상자수,중상자수),
     INDICES=acc_wk$day_night,
     FUN=descr,
     stats=c("mean","sd","min","q1","med","q3","max"))
#범주형 데이터 요약
freq(acc_wk%>%select_if(is.factor))
#출력하지 않도록 설정
freq(acc_wk%>%select_if(is.factor),
     order="freq",
     totals=FALSE,
     cumul=FALSE,
     report.nas = FALSE,
     heading=FALSE)
#summarytools-ctable( ) -> 교차표 작성
ctable(x=acc_wk$days,
       y=acc_wk$day_night)
#교통사고 사망자 수의 변화를 연도별로 확인하기 
format(acc_wk$date,"%Y")
#그룹 및 요약
acc_wk%>%
  mutate(years=format(acc_wk$date,"%Y"))%>%
  group_by(years)%>%
  summarise(
    n.death=sum(사망자수, na.rm=TRUE)
  )
#2020년 교통사고 사망자 수를 시도별로 확인
acc_wk_2020_sido=acc_wk%>%
  mutate(years=format(acc_wk$date,"%Y"))%>%
  dplyr::filter(years=="2020")%>%
  group_by(발생지시도)%>%
  summarise(
    n.death=sum(사망자수,na.rm=TRUE)
  )%>%
  arrange(desc(n.death));acc_wk_2020_sido
#2020년 지역별 자동차 등록 대수와 등록인 수 확인
pops_sido_2020=cars_pops_wk%>%
  dplyr::filter(str_length(regionCode)==2)%>%
  dplyr::filter(regionCode!="00")%>%
  select(-regionCode,-cpp)
#acc_wk_2020_sido 과 pops_sido_2020 데이터셋 결합
pops_sido_2020
acc_pops_2020=acc_wk_2020_sido%>%
  left_join(abbr_tab,by=c("발생지시도"="abbrName"))%>%
  left_join(pops_sido_2020,by=c("regionCode"="sidoCode"))%>%
  select(-fullName)

acc_pops_2020=acc_pops_2020%>%
  mutate(dperht=100000*n.death/pops)%>%
  select(regionCode,regionName,n.death,pops,dperht)
acc_pops_2020


#숫자형 데이터 요약
library(tidyverse)
library(summarytools)
library(dplyr)
library(conflicted)
acc_wk=readRDS("acc_wk.rds")
cars_pops_wk=readRDS("cars_pops_wk.rds")
abbr_tab=readRDS("abbr_tab.rds")

acc_wk_cslt=acc_wk%>%
  select(ends_with("수"))%>%
  pivot_longer(cols=everything())
acc_wk_cslt
#데이터셋을 인명 피해 정도에 따라 상자 도표 그리고,
#몇 가지 요소 사용자화
acc_wk_cslt %>%
  ggplot()+
  geom_boxplot(aes(name,value,color=name))+
  scale_color_manual("상해정도",values = c("red","blue","green"))+
  theme_minimal()+
  theme(
    text=element_text(family = "NSK",size=12),
    axis.title=element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
#이전 그림을 더 세분화하여 데이터셋이 사고 유형 대분류별로 취하는 모양
acc_wk%>%
  select(ends_with("수"),acc_cat)%>%
  pivot_longer(cols=ends_with("수"))%>%
  dplyr::filter(acc_cat !="철길건널목")%>%
  ggplot()+
  geom_boxplot(aes(name,value,color=name))+
  scale_color_manual("상해정도",values = c("red","blue","green"))+
  facet_wrap(vars(acc_cat),ncol=3)+
  theme_minimal()+
  theme(
    text=element_text(family="NSK",size=12),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
#acc_wk$발생지시도 열: 교통사고가 발생한 시도
sido_ta_death=acc_wk%>%group_by(발생지시도)%>%
  summarise(
    n.deaths=sum(사망자수)
  )
sido_ta_death%>%
  ggplot()+
  geom_bar(aes(x=발생지시도,y=n.deaths),fill=NA,color="#00abff",stat="identity")+
  geom_hline(yintercept = 0)+
  theme_minimal()+
  theme(
    text = element_text(family = "NSK",size=12),
    axis.title=element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

pops_2021=read_csv("2021_pops_sido.csv",locale=locale("ko",encoding="CP949"))
sido_ta_death2=sido_ta_death%>%
  left_join(pops_2021,by=c("발생지시도"="sido_abbr"))
sido_ta_death2
sido_ta_death2%>%
  select(행정구역,총인구수,n.deaths)%>%
  ggplot()+
  geom_point(aes(총인구수,n.deaths))
#시각화 보완 완성 코드
library(ggrepel)
library(ggthemes)
sido_ta_death2%>%
  select(행정구역,총인구수,n.deaths)%>%
  mutate(pops=총인구수/10000)%>%
  ggplot()+
  geom_point(aes(pops,n.deaths,color=행정구역),size=3,show.legend=FALSE)+
  geom_text_repel(aes(pops,n.deaths,label=행정구역),size=3)+
  xlab("인구(십만명)")+ylab("사망자수")+
  scale_x_log10()+
  theme_wsj()+
  theme(
    text=element_text(family="NSK"),
    axis.title=element_text(family="NSK",size=10),
    axis.text=element_text(family="NSK",size=7),
    panel.grid.major.y=element_line(color="gray")
  )
