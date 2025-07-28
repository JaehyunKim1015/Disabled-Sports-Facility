library(sf)
library(tidyverse)
library(stringr)

# 1. shapefile 불러오기
setwd("C:/Users/USER/Desktop/final/BND_SIGUNGU_PG")
map <- st_read("BND_SIGUNGU_PG.shp", options = "ENCODING=CP949")

# 2. 부천시 3개 구 통합 후 삽입
map_updated <- map %>%
  filter(!str_detect(SIGUNGU_NM, "원미구|소사구|오정구")) %>%
  bind_rows(
    map %>%
      filter(str_detect(SIGUNGU_NM, "원미구|소사구|오정구")) %>%
      summarise(geometry = st_union(geometry)) %>%
      mutate(SIGUNGU_NM = "부천시", SIGUNGU_CD = "31050")
  )

setwd("C:/Users/USER/Desktop/final/data2")
hypertension_data <- read.csv("장애인_고혈압_1.csv", fileEncoding = "CP949")

library(dplyr)
library(stringr)

data_ready <- hypertension_data %>%
  rename(
    SIGUNGU_CD = Code,
    total_population = Pop,
    hypertension_cases = Morb
  ) %>%
  mutate(
    SIGUNGU_CD = as.character(SIGUNGU_CD),
    total_population = as.numeric(gsub("[^0-9]", "", total_population)),
    hypertension_cases = as.numeric(gsub("[^0-9]", "", hypertension_cases))
  ) %>%
  filter(!is.na(total_population) & !is.na(hypertension_cases))  # NA 제거

library(SpatialEpi)
library(sf)

map_updated <- map_updated %>%
  mutate(SIGUNGU_CD = as.character(SIGUNGU_CD))

# 병합
merged <- left_join(map_updated, data_ready, by = "SIGUNGU_CD")

# 기대 유병자 수 계산
cases <- merged$hypertension_cases
pop <- merged$total_population
expected_cases <- expected(cases = cases, population = pop, n.strata = 1)

# SIR 추가
merged <- merged %>%
  mutate(
    expected = expected_cases,
    sir = hypertension_cases / expected
  )

library(tmap)

tm_shape(merged) +
  tm_fill("sir", style = "quantile", palette = "Reds", title = "SIR (장애인 고혈압)") +
  tm_borders() +
  tm_layout(title = "전국 시군구 장애인 고혈압 SIR", legend.outside = TRUE)

library(spdep)
str(merged)

# 필요한 열만 남기고 정리
merged_clean <- merged %>%
  select(-Gu, -total_population, -hypertension_cases, -expected, -Ratio, -BASE_DATE)

# 1. 인접 리스트 생성 (Queen contiguity)
nb <- poly2nb(merged_clean, queen = TRUE)
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

# 2. Moran's I 테스트
moran_result <- moran.test(merged$sir, lw, zero.policy = TRUE)

# 3. 결과 출력
print(moran_result)

##밖으로 추출##
# 1. 저장할 디렉토리 경로 설정
out_dir <- "C:/Users/USER/Desktop/seoul_merged"
dir.create(out_dir, showWarnings = FALSE)  # 디렉토리가 없으면 생성

# 2. shapefile로 저장 (인코딩: CP949)
st_write(merged,
         dsn = out_dir,
         layer = "merged",
         driver = "ESRI Shapefile",
         layer_options = "ENCODING=CP949",
         delete_layer = TRUE)

