# 0. 팩키지 -------------
library(tidyverse)
library(magick)
library(opencv)
library(gt)
library(readxl)

fs::dir_create("data/speakers_face")
fs::dir_create("data/speakers_mask")

# 1. 발표자 -------
# https://statkclee.github.io/data-science/ds-rconf-profile.html

speakers <- read_excel("data/발표자_대쉬보드.xlsx")

speakers %>% 
  ## ISO2 국기 -----------------------------------------
  mutate(iso2 = ifelse(국가 == "한국", "kr", "us")) %>% 
  mutate(flag_URL = glue::glue('data/worldflags/{iso2}.png')) %>% 
  ## 발표자 사진  -----------------------------------------
  mutate(파일명 = glue::glue("{fs::path_ext_remove(파일명)}_face_mask.png")) %>% 
  mutate(profile_photo = glue::glue('data/speakers_mask/{파일명}')) %>% 
  ## 표에 표시할 칼럼  -----------------------------------------
  select(flag_URL, profile_photo, 발표자명, 소속, 발표제목)  %>% 
  ## ISO2 국기 표에 삽입 -----------------------------------------
  gt() %>% 
    gt::text_transform(
      # Apply a function to a column
      locations = cells_body(columns = c(flag_URL)),
      fn = function(x) {
        # Return an image of set dimensions
        web_image(
          url = x,
          height = 12
        )
      }
    ) %>% 
    # Hide column header flag_URL and reduce width
    cols_width(columns = c(flag_URL) ~ px(50)) %>% 
    cols_label(flag_URL = "") %>% 
    ## 발표자 사진 표에 삽입 -----------------------------------------
  text_transform(
    locations = cells_body(columns = c(profile_photo)),
    fn = function(x) {
      web_image(
        url = x,
        height = 50
      )
    }
  ) %>% 
    cols_width(columns = c(profile_photo) ~ px(50)) %>% 
    cols_label(profile_photo = "")
  
