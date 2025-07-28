# 📦 필요한 패키지 설치 & 로딩
library(readxl)
library(sf)
library(dplyr)
library(spdep)
library(SpatialEpi)

setwd()
# 1. 데이터 불러오기
data <- read.csv("C:/Users/USER/Desktop/2023_seoul.csv")  # 관찰값 (Observed cases)
seoul_map <- st_read(
  "C:/Users/USER/Desktop/SGG_SEOUL/SGG_SEOUL.shp",
  options = "ENCODING=CP949"  # 또는 "ENCODING=EUC-KR"
)  # 서울시 shp

# 4. spatialEpi용 데이터 구성
cases <- as.numeric(data$mort)
pop <- gsub(",", "", data$total)          # 콤마 제거
pop <- trimws(pop)                # 앞뒤 공백 제거
pop <- as.numeric(pop)            # 숫자로 변환
E <- expected(cases = cases,
              population = pop, n.strata = 1)

data$SMR<-cases/E

data <- data %>%
  mutate(code = as.character(code))

seoul_map <- seoul_map %>%
  mutate(ADM_SECT_C = as.character(ADM_SECT_C))

data <- data %>%
  mutate(code = as.character(code))

seoul_map <- seoul_map %>%
  mutate(COL_ADM_SE = trimws(COL_ADM_SE))

dat <- data %>%
  mutate(code = trimws(code))

seoul_map_merged <- seoul_map %>%
  left_join(
    cbind(data, seoul_map),
    by = c("SGG_NM" = "region")  # 실제 코드명 확인 후 수정
  )

str(seoul_map_merged)

# 1. 제거할 컬럼명 지정 (.y 컬럼 제거)
cols_to_remove <- c("ADM_SECT_C.y", "SGG_NM.y", "SGG_OID.y", "COL_ADM_SE.y", "GID.y", "geometry.y")
# 2. 제거
seoul_map_cleaned <- seoul_map_merged[, !(names(seoul_map_merged) %in% cols_to_remove)]

# 3. geometry 재설정 (.x를 geometry로 설정)
seoul_map_cleaned <- st_set_geometry(seoul_map_cleaned, "geometry.x")
names(seoul_map_cleaned)[names(seoul_map_cleaned) == "geometry.x"] <- "geometry"

# .x로 끝나는 컬럼명에서 .x 제거
names(seoul_map_cleaned) <- gsub("\\.x$", "", names(seoul_map_cleaned))

## 3. 인접 리스트 생성 (queen contiguity)
nb <- poly2nb(seoul_map_merged, queen = TRUE)
lw <- nb2listw(nb, style = "W")
moran.test(seoul_map_merged$SMR, lw)

##밖으로 추출##
# 1. 저장할 디렉토리 경로 설정
out_dir <- "C:/Users/USER/Desktop"
dir.create(out_dir, showWarnings = FALSE)  # 디렉토리가 없으면 생성

# 2. shapefile로 저장 (인코딩: CP949)
st_write(seoul_map_merged,
         dsn = out_dir,
         layer = "seoul_merged_2",
         driver = "ESRI Shapefile",
         layer_options = "ENCODING=CP949",
         delete_layer = TRUE)
