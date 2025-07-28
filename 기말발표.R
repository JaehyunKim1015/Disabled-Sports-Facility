setwd("C:/Users/USER/Desktop/final")
library(sf)
library(dplyr)
library(stringr)

####지도 모양 다듬기###
# 셰이프파일 불러오기 (shp, dbf 등 같은 폴더에 있어야 함)
sigungu <- st_read("C:/Users/USER/Desktop/final/BND_SIGUNGU_PG/BND_SIGUNGU_PG.shp", options = "ENCODING=CP949")

# 2. 병합 대상 시와 시군구 코드 리스트
merge_info <- data.frame(
  city_name = c("수원시", "성남시", "안양시", "부천시", "안산시", "고양시",
                "용인시", "청주시", "천안시", "전주시", "포항시", "창원시"),
  SIGUNGU_CD = c("31010", "31020", "31040", "31050", "31090", "31100",
                 "31190", "33040", "34010", "35010", "37010", "38110"),
  stringsAsFactors = FALSE
)

# 3. 시 이름 추출: 예) "수원시 장안구" → "수원시"
sigungu <- sigungu %>%
  mutate(city_name = gsub("시.*", "시", SIGUNGU_NM),  # 기본 시 이름 추출
         city_name = ifelse(SIGUNGU_CD == "31150", "시흥시", city_name))  # 예외 처리

# 4. 병합 대상 시만 선택하여 병합
sigungu_merged <- sigungu %>%
  filter(city_name %in% merge_info$city_name) %>%
  group_by(city_name) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

# 5. 병합된 시에 시군구 코드 추가 (`SIGUNGU_CD`)
sigungu_merged <- sigungu_merged %>%
  left_join(merge_info, by = "city_name") %>%
  select(city_name, SIGUNGU_CD, geometry)

# 6. 병합하지 않은 나머지 시군구는 그대로 유지 (기존 코드 → SIGUNGU_CD로 변경)
sigungu_others <- sigungu %>%
  filter(!city_name %in% merge_info$city_name) %>%
  rename(SIGUNGU_CD = SIGUNGU_CD) %>%
  select(city_name, SIGUNGU_CD, geometry)

# 7. 병합된 시 + 나머지 모두 결합
sigungu_final <- bind_rows(sigungu_merged, sigungu_others)

# 8. 시각화 확인
plot(sigungu_final["city_name"], main = "병합 완료 + SIGUNGU_CD 포함")

##밖으로 추출##
# 1. 저장할 디렉토리 경로 설정
out_dir <- "C:/Users/USER/Desktop/sigungu_final"
dir.create(out_dir, showWarnings = FALSE)  # 디렉토리가 없으면 생성

# 2. shapefile로 저장 (인코딩: CP949)
st_write(sigungu_final,
         dsn = out_dir,
         layer = "SIGUNGU_MERGED_WITH_CODE",
         driver = "ESRI Shapefile",
         layer_options = "ENCODING=CP949",
         delete_layer = TRUE)

####데이터###
# CSV 파일들
setwd("C:/Users/USER/Desktop/final/data")
morb   <- read.csv("2023_morb.csv")      # 온열질환자 수
pop    <- read.csv("2023_pop.csv")       # 전체 인구
pop2   <- read.csv("2023_pop2.csv")      # 노인 인구
worker <- read.csv("2023_worker.csv")    # 야외 노동자 수
temp_raw <- read.csv("2023_temp.csv")
green<-read.csv("2023_green.csv")

#interpolation
library(sf)
library(gstat)
library(sp)

##Kriging
# 2. sf → sp 변환 (좌표: 경도 y, 위도 x)
temp_sf <- st_as_sf(temp_raw, coords = c("y", "x"), crs = 4326) %>%
  st_transform(5181)  # UTM-K

# 3. 관측 데이터 sp 객체로 변환
temp_sp <- as(temp_sf, "Spatial")

# 4. 시군구 중심점 계산
sigungu <- st_transform(sigungu_final, 5181)
centroids <- st_centroid(sigungu)
centroids_sp <- as(centroids, "Spatial")

# 5. Variogram 모델 만들기
vgm_model <- variogram(duration ~ 1, data = temp_sp)
fit_model <- fit.variogram(vgm_model, model = vgm("Sph"))  # 구형 모형(Spherical)
plot(vgm_model, fit_model, main = "Semivariogram (duration ~ 거리)")
nugget <- fit_model$psill[1]
sill   <- sum(fit_model$psill)   # nugget + partial sill
range  <- fit_model$range[2]     # range는 두 번째 항목에서 나옴

# 6. Ordinary Kriging 수행
kriging_result <- krige(formula = duration ~ 1,
                        locations = temp_sp,
                        newdata = centroids_sp,
                        model = fit_model)

# 7. 결과를 시군구에 결합
centroids$duration <- kriging_result$var1.pred

sigungu_with_duration <- sigungu %>%
  mutate(SIGUNGU_CD = as.character(SIGUNGU_CD)) %>%
  left_join(st_drop_geometry(centroids)[, c("SIGUNGU_CD", "duration")], by = "SIGUNGU_CD")

# 8. 결과 시각화
plot(sigungu_with_duration["duration"], main = "폭염일수 (Kriging 보간)")

##온열환자 sIR구하기
library(SpatialEpi)
cases <- as.numeric(morb$patient)
pop$total <- gsub(",", "", pop$total)          # 콤마 제거
pop$total <- trimws(pop$total)                # 앞뒤 공백 제거
pop$total <- as.numeric(pop$total)            # 숫자로 변환
E <- expected(cases = morb$patient,
              population = pop$total, n.strata = 1)
morb$E<- E
morb$SIR <- morb$patient / morb$E

# 공통 코드 변환 (정합성 확보)
pop$code    <- as.character(pop$sigungu_cd)
pop2$code   <- as.character(pop2$sigungu_cd)
worker$code <- as.character(worker$sigungu_cd)

# 숫자형으로 변환 (콤마 제거 + 숫자 변환)
pop$total      <- as.numeric(gsub(",", "", pop$total))
pop2$old_t     <- as.numeric(gsub(",", "", pop2$old_t))
worker$outside <- as.numeric(gsub(",", "", worker$outside))

# 병합
merged_data <- pop %>%
  left_join(pop2, by = "code", suffix = c("_pop", "_pop2")) %>%
  left_join(worker, by = "code")

# 변수 이름 추정: total 인구(pop), senior(노인 수), outdoor_worker(노동자 수)
# 필요시 실제 열 이름으로 수정

# 비율 계산
merged_data <- merged_data %>%
  mutate(
    senior_ratio = old_t / total,
    worker_ratio = outside / total
  )

str(merged_data)

merged_data_clean <- merged_data %>%
  select(province, region, code, senior_ratio, worker_ratio)

# 필요한 패키지
library(sf)
library(tmap)
library(dplyr)
library(ggplot2)

# 좌표계 일치
sigungu_map <- st_transform(sigungu_final, crs = 5181)

# 2. 병합: morb의 지역 코드 변수는 'sgg_code', 지도는 'SIGUNGU_CD'라고 가정
sigungu_map <- sigungu_map %>%
  mutate(SIGUNGU_CD = as.character(SIGUNGU_CD)) %>%
  left_join(morb %>% mutate(sgg_code = as.character(sgg_code)),
            by = c("SIGUNGU_CD" = "sgg_code"))

# 2. SIR 단계구분도 (tmap v4 스타일)
tm_shape(sigungu_map) +
  tm_polygons(
    fill = "SIR",
    fill.scale = tm_scale_intervals(
      style = "quantile",
      values = "brewer.reds"
    ),
    fill.legend = tm_legend(title = "SIR (표준화발생비)"),
    col = "black",        # 테두리 색
    col_alpha = 0.3       # 테두리 투명도
  ) +
  tm_title("시군구별 온열질환 SIR 지도") +
  tm_layout(legend.outside = TRUE)

# 1. SIR > 1인 지역만 추출
sir_high <- sigungu_map %>% filter(SIR > 1) %>%
  arrange(desc(SIR)) 
sir_high

# 1. 저장할 폴더 경로 (예: 현재 작업 디렉토리 하위의 "exported_data" 폴더)
dir.create("exported_data", showWarnings = FALSE)

# 2. shapefile 저장 (CP949 인코딩으로 한글 깨짐 방지)
st_write(sigungu_map,
         dsn = "exported_data/sigungu_map.shp",
         delete_layer = TRUE,
         layer_options = "ENCODING=CP949")
getwd()
library(sf)
library(dplyr)

# 1. 코드 정리
morb$code <- as.character(morb$sgg_code)  # morb에서 sgg_code가 시군구 코드
merged_data_clean$code <- as.character(merged_data_clean$code)
sigungu_with_duration$SIGUNGU_CD <- as.character(sigungu_with_duration$SIGUNGU_CD)

# 2. morb + merged_data 병합
morb_merged <- morb %>%
  left_join(merged_data_clean %>% select(code, senior_ratio, worker_ratio), by = "code")

# 3. 위 결과를 공간데이터(sigungu_with_duration)에 병합
sigungu_final_merged <- sigungu_with_duration %>%
  left_join(morb_merged, by = c("SIGUNGU_CD" = "code"))

str(sigungu_final_merged)

sigungu_final_selected <- sigungu_final_merged %>%
  select(province, city_name, SIGUNGU_CD, geometry,
         duration, SIR, senior_ratio, worker_ratio)

str(sigungu_final_selected)

# 1. 저장할 폴더 경로 (예: 현재 작업 디렉토리 하위의 "exported_data" 폴더)
dir.create("exported_data", showWarnings = FALSE)

# 2. shapefile 저장 (CP949 인코딩으로 한글 깨짐 방지)
st_write(sigungu_final_selected,
         dsn = "sigungu_final_selected.shp",
         delete_layer = TRUE,
         layer_options = "ENCODING=CP949")
getwd()

###INLA
library(INLA)
library(spdep)

sigungu_final_selected <- sigungu_final_selected %>%
  mutate(log_SIR = log(SIR))

# 지역 인덱스 생성
sigungu_final_selected$idx <- as.numeric(as.factor(sigungu_final_selected$SIGUNGU_CD))

# 공간 인접 리스트 생성
nb <- poly2nb(sigungu_final_selected)

# INLA용 인접 행렬 저장
nb2INLA("sigungu_sir.adj", nb)

# 그래프 객체 생성
g <- inla.read.graph("sigungu_sir.adj")

formula_log_sir <- log_SIR ~ duration + senior_ratio + worker_ratio +
  f(idx, model = "bym", graph = g)
