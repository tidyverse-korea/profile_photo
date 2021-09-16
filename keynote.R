# 0. 팩키지 -------------
library(tidyverse)
library(magick)
library(opencv)
library(gt)
library(readxl)

# 1. 발표자 -------
# https://statkclee.github.io/data-science/ds-rconf-profile.html

keynote <- tribble(~"구분", ~"시간", ~"발표자명", ~"소속", ~"발표제목", ~"국가", ~"파일명",
                   "오프닝", "10:00~10:15", "OOO", "OOOO", "OOOOO", "한국", "opening_speaker.png",
                   "키노트", "10:15~11:00", "Julia Silge", "RStudio", "NLP and text modeling with tidymodels", "미국", "julia_silge.png",
                   "키노트", "11:00~11:45",  "유충현", "한화생명", "`dlookr` - AudoEDA", "한국", "choonghyun_ryu.png")

keynote_tbl <- keynote %>% 
  ## ISO2 국기 -----------------------------------------
  mutate(iso2 = ifelse(국가 == "한국", "kr", "us")) %>% 
  mutate(flag_URL = glue::glue('data/worldflags/{iso2}.png')) %>% 
  ## 발표자 사진  -----------------------------------------
  mutate(파일명 = ifelse(str_detect(파일명, "datarize"), 
                                   glue::glue("{fs::path_ext_remove(파일명)}_face_mask.gif"),
                                   glue::glue("{fs::path_ext_remove(파일명)}_face_mask.png"))) %>% 
  mutate(profile_photo = glue::glue('data/speakers_mask/{파일명}')) %>% 
  ## 표에 표시할 칼럼  -----------------------------------------
  select(구분, 시간, flag_URL, profile_photo, 발표자명, 소속, 발표제목) 

keynote_tbl_gt <- keynote_tbl %>% 
  ## ISO2 국기 표에 삽입 -----------------------------------------
  gt(rowname_col = "시간", groupname_col = "구분") %>% 
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
  

keynote_tbl_gt %>% 
  tab_header(
    title = md("**&#x2600; 한국 R 컨퍼런스 키노트 발표 &#x2600;**"),
    subtitle = md("*Make R Great Again!!!*")
  ) %>% 
  tab_source_note(
    source_note = md("**한국 R 컨퍼런스**: 발표내용은 조율중이며 변경될 수 있습니다.")
  ) %>% 
  tab_options(
    heading.background.color = "#1E61B0", # R logo 파란색
    heading.title.font.size = "32px",
    column_labels.background.color = "#F7F7F7", # R logo 회색 
    column_labels.font.weight = "bold",
    stub.background.color = "#ffffff",
    stub.font.weight = "bold"
  ) %>% 
  cols_align(
    align = "center",
    columns = c(flag_URL, profile_photo, 발표자명, 소속)
  ) %>%
  cols_align(
    align = "center",
    columns = 발표제목
  ) %>%  
  cols_width(
    flag_URL ~ px(50),
    profile_photo ~ px(100),
    발표자명 ~ px(100),
    소속 ~ px(150),
    발표제목 ~ px(500)
  ) %>% 
  gt::fmt_markdown(columns = `발표제목`) %>% 
  tab_style(
    style = list(
      cell_fill("#3764B0"),
      cell_text(color = "white", weight = "bold",
                align = "left",
                size = px(25))
    ),
    locations = cells_row_groups())
