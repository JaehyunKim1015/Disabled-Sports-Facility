library(sf)
library(dplyr)
library(tidyverse)
library(sf)
library(tmap)

setwd("C:/Users/USER/Desktop/final/data2")
seoul_map <- st_read("C:/Users/USER/Desktop/final/data2/SGG_SEOUL/SGG_SEOUL.shp", options = "ENCODING=CP949")  # EUC-KR 또는 CP949로 한글 인코딩 처리
hypertension_data <- read.csv("장애인_고혈압.csv", fileEncoding = "CP949")  # 또는 "EUC-KR"
seoul_map <- seoul_map %>%
  mutate(SGG_NM = str_replace_all(SGG_NM, " ", ""), SGG_NM = str_to_lower(SGG_NM))

hypertension_data <- hypertension_data %>%
  rename(SGG_NM= 구) %>%  # 실제 열 이름에 따라 수정
  mutate(SGG_NM = str_replace_all(SGG_NM, " ", ""), SGG_NM = str_to_lower(SGG_NM))

hypertension_data <- hypertension_data %>%
  rename(
    total_population = 대상인원,
    hypertension_cases = 유병인원
  )

hypertension_data <- hypertension_data %>%
  select(SGG_NM, total_population, hypertension_cases)

seoul_merged <- left_join(seoul_map, hypertension_data, by = "SGG_NM")
library(SpatialEpi)

# 4. spatialEpi용 데이터 구성
cases <- as.numeric(seoul_merged$hypertension_cases)
pop <- as.numeric(seoul_merged$total_population)            # 숫자로 변환
e <- expected(cases = cases,
              population = pop, n.strata = 1)
seoul_merged$sir <- cases/ pop

library(spdep)
## 3. 인접 리스트 생성 (queen contiguity)
nb <- poly2nb(seoul_merged, queen = TRUE)
lw <- nb2listw(nb, style = "W")
moran.test(seoul_merged$sir, lw)

##밖으로 추출##
# 1. 저장할 디렉토리 경로 설정
out_dir <- "C:/Users/USER/Desktop"
dir.create(out_dir, showWarnings = FALSE)  # 디렉토리가 없으면 생성

# 2. shapefile로 저장 (인코딩: CP949)
st_write(seoul_merged,
         dsn = out_dir,
         layer = "seoul_merged",
         driver = "ESRI Shapefile",
         layer_options = "ENCODING=CP949",
         delete_layer = TRUE)
library(tmap)

# SIR 단계구분도
tm_shape(seoul_merged) +
  tm_fill("sir",
          style = "quantile",
          palette = "Reds",
          title = "SIR (장애인 고혈압)") +
  tm_borders() +
  tm_text("SGG_NM", size = 0.7, col = "black") +  # 구 이름 표시
  tm_layout(title = "서울시 장애인 고혈압 SIR",
            legend.outside = TRUE)


