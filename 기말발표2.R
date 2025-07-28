setwd("C:/Users/USER/Desktop/final/data/sigungu_final")
library(sf)
library(INLA)
library(spdep)
library(dplyr)

# 2. 저장된 shapefile 불러오기
sigungu <- st_read("sigungu_final_selected.shp")
str(sigungu)

# 4. 공간 인접행렬 생성
nb <- poly2nb(sigungu)
nb2INLA("sigungu.adj", nb)
g <- inla.read.graph("sigungu.adj")

# 6. INLA 모델 공식 정의 및 실행
formula <- log_SIR ~ duration + senior_ratio + worker_ratio +
  f(idx, model = "bym", graph = g)

formula <- log_SIR ~ f(idx, model = "bym", graph = g, hyper = prior)

result <- inla(
  formula,
  family = "gaussian",
  data = sigungu,
  control.predictor = list(compute = TRUE),
  control.compute = list(dic = TRUE, waic = TRUE)
)

result <- inla(
  formula,
  family = "gaussian",
  data = sigungu,
  control.predictor = list(compute = TRUE))


# 7. 결과 확인
summary(result)

##
prior = list(
  precision = list(prior = "pc.prec", param = c(0.5, 0.01)), # 0.5보다 클 확률이 1%
  phi       = list(prior = "pc", param = c(0.5, 2/3))         # 구조적 효과가 0.5보다 작을 확률이 2/3
)

