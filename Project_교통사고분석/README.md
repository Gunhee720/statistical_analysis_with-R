# 🚦 R 기반 교통사고 데이터 분석 프로젝트

> 대한민국 교통사고 사망자 수 및 자동차 등록 현황 데이터를 활용한 통계 분석 및 시각화 프로젝트

---

## 📌 프로젝트 개요

이 프로젝트는 **도로교통공단의 교통사고 사망자 데이터**와 **통계청의 자동차 등록현황 데이터**를 활용하여  
교통사고 발생의 특성을 분석하고, R을 이용해 시각화한 데이터 분석 사례입니다.  
지역별, 요일별, 사고 유형별, 인구 대비 사고율 등의 다양한 지표를 통해 **교통안전정책 수립에 기여할 수 있는 통찰을 도출**하는 것이 목적입니다.

---

## 📁 데이터 출처

- **교통사고 사망자 수**: 도로교통공단 TAAS 오픈 API  
  - https://taas.koroad.or.kr/api/main.do
- **자동차 등록 대수 및 인구**: 통계청 (KOSIS)  
  - https://kosis.kr

---

## 📊 사용 데이터

- `death.csv`: 전국 사망 교통사고 정보 (2012~2021년)
- `cars_pops.xlsx`: 지역별 자동차 등록 대수 및 주민등록 인구 (2003~2022년)
- `2021_pops_sido.csv`: 2021년 행정구역별 주민등록 인구

  <img width="1309" height="523" alt="image" src="https://github.com/user-attachments/assets/087bcc7d-063d-4f77-8809-e3439e06782f" />


---

## 🔧 주요 기술 스택

- **언어**: R
- **라이브러리**: `tidyverse`, `readxl`, `lubridate`, `summarytools`, `ggplot2`, `ggrepel`, `ggthemes`, `conflicted`
- **형식**: tidy data format, long/short format 변환, `rds` 저장/로드 활용

---

## 🧩 데이터 처리 과정

### 📥 1. 데이터 수집 및 전처리

- 날짜 및 시간 분리 (`발생년월일시` → `date`, `hour`)
- 사고 유형, 도로 형태, 요일 등 범주형 팩터로 변환
- 시도 및 행정구역 정보 정리 및 축약명 매핑
- 문자열 특수공백 제거 및 열 이름 정규화
- 두 데이터셋(`accident`, `cars_pops`)을 **시도 단위로 결합**

---

### 📐 2. 데이터 요약

- **숫자형 데이터**: 사망자수, 경상자수, 중상자수의 평균, 중앙값, 표준편차 등
- **범주형 데이터**: 사고 유형, 도로 형태, 요일, 주야 여부에 따른 빈도 요약
- **연도별 변화**: 사망자 수 추이 분석
- **2020년 기준 분석**: 시도별 사고 발생 및 자동차 등록 현황

---

### 📊 3. 시각화

- 상해 정도별 피해자 수 **상자 그림**

  <img width="436" height="385" alt="image" src="https://github.com/user-attachments/assets/f0c82a69-2d9b-4be3-9807-a82ae87f80f0" />

- 사고 유형별 피해 정도 **Facet Boxplot**

  <img width="634" height="339" alt="image" src="https://github.com/user-attachments/assets/9c66b66e-415e-4375-9854-171af2fee9e3" />

- 시도별 사망자 수 **막대그래프**

  <img width="554" height="333" alt="image" src="https://github.com/user-attachments/assets/1f23a550-0efe-40e7-acb9-48ad309c4e58" />


예시 코드:

```r
ggplot(acc_pops_2020, aes(x = pops/10000, y = n.death)) +
  geom_point(aes(color = regionName), size = 3) +
  geom_text_repel(aes(label = regionName), size = 3) +
  xlab("인구 (십만명)") + ylab("사망자수") +
  scale_x_log10() +
  theme_minimal()

