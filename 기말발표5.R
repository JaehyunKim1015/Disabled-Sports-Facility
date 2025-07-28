library(sf)
library(tidyverse)
library(stringr)

#=============================종속변수수=====================
setwd("C:/Users/USER/Desktop/final/data2")
hypertension_data <- read.csv("장애인_고혈압_2.csv", fileEncoding = "CP949")

#  shapefile 불러오기
setwd("C:/Users/USER/Desktop/final/BND_SIGUNGU_PG")
map <- st_read("BND_SIGUNGU_PG.shp", options = "ENCODING=CP949")

# 수도권 코드로 시작하는 시군구만 필터
map_cap <- map %>%
  filter(str_starts(SIGUNGU_CD, "11") |
           str_starts(SIGUNGU_CD, "23") |
           str_starts(SIGUNGU_CD, "31"))

# 부천시 3개 구 병합 후 삽입
map_updated <- map_cap %>%
  filter(!str_detect(SIGUNGU_NM, "원미구|소사구|오정구")) %>%
  bind_rows(
    map_cap %>%
      filter(str_detect(SIGUNGU_NM, "원미구|소사구|오정구")) %>%
      summarise(geometry = st_union(geometry)) %>%
      mutate(SIGUNGU_NM = "부천시", SIGUNGU_CD = "31050")
  )

# 코드형 일치
map_updated <- map_updated %>%
  mutate(SIGUNGU_CD = as.character(SIGUNGU_CD))

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
  filter(!is.na(total_population), !is.na(hypertension_cases))

# 병합
merged <- left_join(map_updated, data_ready, by = "SIGUNGU_CD")

library(SpatialEpi)
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

#===============SIR 시각화=====================
library(tmap)

tm_shape(merged) +
  tm_fill("sir", style = "quantile", palette = "Reds", title = "SIR (장애인 고혈압)") +
  tm_borders() +
  tm_layout(
    title = "전국 시군구 장애인 고혈압 SIR",
    title.position = c("right", "top"),  # ←오른른왼쪽 정렬
    legend.outside = TRUE
  )

merged_clean <- merged %>%
  select(-Gu, -Ratio, -BASE_DATE)

####Moran's I
library(spdep)
# 1. 인접 리스트 생성 (Queen contiguity)
nb <- poly2nb(merged_clean, queen = TRUE)
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

# 2. Moran's I 테스트
moran_result <- moran.test(merged_clean$sir, lw, zero.policy = TRUE)

# 3. 결과 출력
print(moran_result)

#### 장애인 체육시설
centroids <- st_centroid(merged_clean)
setwd("C:/Users/USER/Desktop/final/data2/sd_ex")
ex <- st_read("a.shp", options = "ENCODING=UTF-8")

# 좌표계 통일 (둘 다 meter 기반 좌표계로 맞춰야 함, 예: UTM-K 5181)
centroids <- st_transform(centroids, crs = 5181)
ex <- st_transform(ex, crs = 5181)

# 시군구 중심점마다 체육시설까지 거리 계산 (m 단위 → km)
dist_matrix <- st_distance(centroids, ex)
min_dist_km <- apply(dist_matrix, 1, min) / 1000  # 단위 변환: m → km
?st_distance
# 결과 병합
merged_clean$min_distance_km <- min_dist_km

library(tmap)

library(tmap)

tm_shape(merged_clean) +
  tm_fill("min_distance_km",
          style = "quantile",
          palette = c("#edf8fb", "#b2e2e2", "#66c2a4", "#2ca25f", "#006d2c"),
          title = "체육시설 최소거리 (km)") +
  tm_borders() +
  tm_layout(
    title = "시군구별 장애인 체육시설 접근성",
    title.position = c("right", "top"),
    legend.outside = TRUE
  )

# 결과 확인
head(merged_clean[, c("SIGUNGU_NM", "min_distance_km")])

###체육지도사 비율
setwd("C:/Users/USER/Desktop/final/data2")
lead <- read.csv("장애인_지도사.csv", fileEncoding = "CP949")

lead <- lead %>%
  mutate(Code = as.character(Code))

merged_clean <- merged_clean %>%
  left_join(
    lead %>% select(Code, Ratio), 
    by = c("SIGUNGU_CD" = "Code")
  )

tm_shape(merged_clean) +
  tm_fill("Ratio",
          style = "quantile",      # 분위수 기준 구간
          palette = "Blues",
          title = "장애인 체육지도사 비율 (%)") +
  tm_borders() +
  tm_layout(
    title = "지역별 장애인 체육지도사 비율",
    title.position = c("right", "top"),   # 오른쪽 상단 정렬
    title.size = 1.2,
    legend.outside = TRUE
  )

####INLA
library(INLA)
# 인접 리스트 생성
nb <- poly2nb(merged_clean, queen = TRUE)

# INLA용 graph file로 저장
nb2INLA("map.adj", nb)

# 그래프 불러오기
g <- inla.read.graph("map.adj")

merged_clean$idx <- 1:nrow(merged_clean)
merged_clean$log_sir <- log(merged_clean$sir)
formula <- log_sir ~ Ratio + min_distance_km +
  f(idx, model = "bym", graph = g)

result <- inla(formula,
               family = "gaussian",
               data = merged_clean,
               control.predictor = list(compute = TRUE),
               control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE))
summary(result)   

formula_bym2 <- log_sir ~ Ratio + min_distance_km +
  f(idx, model = "bym2", graph = g)

result_bym2 <- inla(formula_bym2,
                    family = "gaussian",
                    data = merged_clean,
                    control.predictor = list(compute = TRUE),
                    control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE))
summary(result_bym2)

# Posterior RR 값 추출 (exp of linear predictor)
merged_clean$RR <- exp(result_bym2$summary.fitted.values$mean)

# 시각화
tm_shape(merged_clean) +
  tm_fill("RR",
          style = "quantile",
          palette = "YlOrRd",
          title = "Posterior RR") +
  tm_borders() +
  tm_layout(title = "Posterior Relative Risk (BYM2 모델)",
            title.position = c("right", "top"),
            legend.outside = TRUE)
###초과확률률
lambda <- result$summary.fitted.values$mean  # Posterior mean of fitted values
exceed_prob <- 1 - ppois(q = merged_clean$sir, lambda = lambda)
merged_clean$exceed_prob <- exceed_prob
library(tmap)

tm_shape(merged_clean) +
  tm_fill("exceed_prob",
          palette = "Reds",
          style = "quantile",
          title = "Exceedance Prob. (RR > 1)") +
  tm_borders() +
  tm_layout(
    title = "Exceedance Probability",
    title.position = c("right", "top"),
    legend.outside = TRUE
  )
str(merged_clean)

###Carbayes
library(CARBayes)
library(spdep)
library(sf)
library(dplyr)

# sf 객체 → SpatialPolygons로 변환
merged_sp <- as_Spatial(st_geometry(merged_clean))

# 인접 리스트 생성
nb <- poly2nb(merged_sp)

# 인접 행렬 (binary)
W <- nb2mat(nb, style = "B", zero.policy = TRUE)

which(rowSums(W) == 0)
isolated <- which(rowSums(W) == 0)
# 고립지역 제거
W_clean <- W[-isolated, -isolated]
data_car_clean <- data_car[-isolated, ]

set.seed(123)  # 재현성 확보

data_car <- merged_clean %>%
  sf::st_drop_geometry() %>%
  dplyr::select(hypertension_cases, expected, Ratio, min_distance_km)

data_car$log_expected <- log(data_car$expected)
merged_clean$SIGUNGU_NM[which(rowSums(W) == 0)]
model_car <- S.CARleroux(
  formula = hypertension_cases ~ offset(log_expected) + Ratio + min_distance_km,
  data = data_car_clean,
  family = "poisson",
  W = W_clean,
  burnin = 2000,
  n.sample = 10000,
  thin = 10)
summary(model_car)
model_car$summary.results
print(model_car$summary.results)
model_car$modelfit
