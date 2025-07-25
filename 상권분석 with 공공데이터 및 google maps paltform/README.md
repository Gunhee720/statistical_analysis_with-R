# 서울시 상권분석 with 공공데이터 및 Google Map Platform
# 프로젝트 개요
이 프로젝트는 서울시 상권 데이터를 기반으로 주요 상권의 변화 및 밀집도를 분석하고, Google Map API를 활용해 시각화하는 R 기반 상권 분석 도구입니다.
특히, 역삼1동 지역을 중심으로 특정 업종(예: 카페)의 시계열 변화 및 신규/폐업 현황을 시각적으로 파악할 수 있도록 구현했습니다.

# 🛠️ 사용 기술
언어: R

라이브러리:

ggplot2: 데이터 시각화

ggmap: 구글 지도 시각화

dplyr: 데이터 처리

readxl: 엑셀 파일 불러오기

API: Google Map Static API

# 📂 데이터 출처
서울시 공공데이터포털의 상가업소 정보

수집 기간: 2024년 3월, 2024년 9월, 2025년 3월

주요 컬럼: 업종 분류, 행정동명, 위도/경도, 수집연월 등

# 코드로 구현한 기능

| 기능                | 설명                        |
| ----------------- | ------------------------- |
| ✅ 업종별 점포수 집계      | 대분류 기준으로 서울시 전체 업종 분포 시각화 |
| ✅ 구/행정동 단위 분석     | 구별 및 행정동별 점포수 비교          |
| ✅ 시계열 분석          | 업종별/지역별 점포 수 변화 추이        |
| ✅ 특정 지역 분석 (역삼1동) | 업종별 밀도 분석 및 구글 지도 시각화     |
| ✅ 신규/폐업 업소 분석     | 1년간 카페 업소의 개폐업 현황 표시      |


# 서울시 업종별 점포수를 구별로 시각화 with google maps platform

서울시 전역의 상권 데이터를 분석한 결과, 강남구, 서초구, 송파구 등에서 **점포 수가 특히 많은 것으로 나타남.
이를 Google Maps를 활용해 시각적으로 표현함으로써, 지역별 상권 밀집도를 한눈에 파악할 수 있도록 활용
<img width="953" height="766" alt="스크린샷 2025-07-17 100806" src="https://github.com/user-attachments/assets/1a0894d7-d092-4760-bd0e-13cc804cdd6b" />

# 2024년 3월 ~ 2025년 3월 사이, 점포 수 변화가 가장 큰 TOP 10 행정동

### 1년간의 상권 데이터를 비교한 결과, 신사동, 가산동, 반포3동이 가장 큰 점포 수 변화를 보였고 , 이는 해당 지역의 상권 재편 또는 트렌드 변화 가능성을 시사

<img width="1243" height="801" alt="스크린샷 2025-07-17 104610" src="https://github.com/user-attachments/assets/848b34c3-da21-4ec4-b5c7-423f52d6bc91" />

# 역삼동 업종별 점포 with google maps paltform
<img width="925" height="771" alt="스크린샷 2025-07-17 112110" src="https://github.com/user-attachments/assets/c37f4b55-388a-4d63-8fc6-940db8ae7107" />

### 역삼1동의 전체 업종 데이터를 분석한 결과, 업종 수가 매우 다양하여 시각적 복잡성이 높았음 따라서 점포 수 기준 상위 3개 업종만을 추출해 보다 명확하게 상권 분포를 시각화

<img width="947" height="817" alt="스크린샷 2025-07-17 112203" src="https://github.com/user-attachments/assets/a412f270-be26-4c16-b568-65591dc598df" />

# ☕ 역삼1동 카페 상권의 1년간 변화 현황 

<img width="848" height="827" alt="스크린샷 2025-07-17 114613" src="https://github.com/user-attachments/assets/9cbbf27d-29a4-46c6-a5cd-7fab2d69bd8a" />

2024년 3월부터 2025년 3월까지의 데이터를 비교한 결과,  
**역삼1동 지역의 카페 점포 수에 큰 변동이 있었음**.

- 📉 **폐업한 카페**: 126곳 발생함  
- 📈 **신규 개업한 카페**: 159곳 생김  

카페 점포의 유입과 이탈이 활발하게 일어났으며,  
이는 **역삼1동 내 카페 시장의 경쟁 심화** 또는 **소비 트렌드 변화**를 반영한 결과로 해석됨.

