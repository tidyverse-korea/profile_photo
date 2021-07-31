## Not Working ~~~~

# 0. 팩키지 -------------
library(tidyverse)
library(magick)
library(opencv)
library(gt)
library(readxl)

# 1. 발표자 -------
# https://statkclee.github.io/data-science/ds-rconf-profile.html

keynote_bio <- tribble(~"발표자명", ~"파일명", ~"약력",
                      "Julia Silge", "julia_silge.png", "Julia Silge is a data scientist and software engineer at RStudio PBC where she works on open source modeling tools. She is an author, an international keynote speaker, and a real-world practitioner focusing on data analysis and machine learning practice. Julia loves text analysis, making beautiful charts, and communicating about technical topics with diverse audiences.",
                      "유충현", "choonghyun_ryu.png", "Korea R User Group 창설을 주도하셨고 초대 대표 역임하셨으며 2011년, 2012년 1회/2회 Korea R User Conference를 이끄셨고, “R을 이용한 통계학의 이해” 외 5종 저술을 통해 한글 R/Tidyverse 대중화에 크게 기여를 하셨으며, CRAN에 R 팩키지 dlookr, alookr 를 기여하였다. 현재는 Seoul R Meetup, Tidyverse Korea 운영위원으로 R/Tidyverse 보급과 선진화에 헌신적인 기여를 하고 있다.")
                   
keynote_bio_tbl <- keynote_bio %>% 
  ## 발표자 사진  -----------------------------------------
  mutate(파일명 = ifelse(str_detect(파일명, "datarize"), 
                                   glue::glue("{fs::path_ext_remove(파일명)}_face_mask.gif"),
                                   glue::glue("{fs::path_ext_remove(파일명)}_face_mask.png"))) %>% 
  mutate(profile_photo = glue::glue('data/speakers_mask/{파일명}')) %>% 
  ## 표에 표시할 칼럼  -----------------------------------------
  select(profile_photo, 약력) %>% 
  t(.) %>% 
  as_tibble() %>% 
  set_names(c("Julia Silge", "유충현"))

keynote_bio_gt <- keynote_bio_tbl %>% 
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
