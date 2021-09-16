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

speakers_tbl <- speakers %>% 
  ## ISO2 국기 -----------------------------------------
  mutate(iso2 = ifelse(국가 == "한국", "kr", "us")) %>% 
  mutate(flag_URL = glue::glue('data/worldflags/{iso2}.png')) %>% 
  ## 발표자 사진  -----------------------------------------
  mutate(파일명 = glue::glue("{fs::path_ext_remove(파일명)}_face_mask.png")) %>% 
  mutate(profile_photo = glue::glue('data/speakers_mask/{파일명}')) %>% 
  ## 표에 표시할 칼럼  -----------------------------------------
  select(flag_URL, profile_photo, 발표자명, 소속, 발표자소개) %>% 
  ## 어수행 오류 -----
  filter(!str_detect(발표자명, "어수행|박상훈|이민호"))

speakers_tbl_gt <- speakers_tbl %>% 
  ## ISO2 국기 표에 삽입 -----------------------------------------
  gt() %>% 
  gt::text_transform(  
    locations = cells_body(columns = flag_URL),    
    fn = function(x) {
      # loop over the elements of the column
      map_chr(x, ~ local_image(
        filename = .x,
        height = 20
      ))
    }) %>%   
  # Hide column header flag_URL and reduce width
    cols_width(columns = c(flag_URL) ~ px(50)) %>% 
    cols_label(flag_URL = "") %>% 
    ## 발표자 사진 표에 삽입 -----------------------------------------
  text_transform(
    locations = cells_body(columns = c(profile_photo)),
    fn = function(x) {
      map_chr(x, ~ local_image(
        filename = .x,
        height = 50
      ))
    }) %>% 
    cols_width(columns = c(profile_photo) ~ px(100)) %>% 
    cols_label(profile_photo = "")
  

speakers_tbl_gt %>% 
  tab_header(
    title = md("**&#x2600; 한국 R 컨퍼런스 발표자 &#x2600;**"),
    subtitle = md("*NLP, 웹앱, 예측, 재현가능 과학, 산업현장, 사회과학 ...*")
  ) %>% 
  tab_source_note(
    source_note = md("한국 R 컨퍼런스: <https://use-r.kr/>")
  ) %>% 
  tab_options(
    heading.background.color = "#e8fc03",
    heading.title.font.size = "32px",
    column_labels.background.color = "#a5fc03",
    column_labels.font.weight = "bold",
    stub.background.color = "#bcbddc",
    stub.font.weight = "bold"
  ) %>% 
  cols_align(
    align = "center",
    columns = everything()
  )  
