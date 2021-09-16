# 0. 팩키지 -------------
library(tidyverse)
library(magick)
library(opencv)

fs::dir_create("data/speakers_face")
fs::dir_create("data/speakers_mask")

# 1. 얼굴 추출 -------
## 참고: https://statkclee.github.io/deep-learning/r-face-conference.html

## 1.1. 얼굴 추출 함수 -------

extract_face <- function(raw_image) {
  # 1. 얼굴인식 좌표 추출 --------------
  figure_face_info <- ocv_read(raw_image) %>% 
    ocv_facemask()
  
  ## 데이터프레임 자료구조 변환
  face_tbl <- attr(figure_face_info, "faces") %>% 
    as_tibble()
  
  # 2. 이미지 영역 표시 -------------
  figure_img <- image_read(raw_image)
  
  # ## 영역 표시 -------------------------
  # figure_draw <- image_draw(figure_img)
  # 
  # rect(xleft   = face_tbl$x - face_tbl$radius, 
  #      xright  = face_tbl$x + face_tbl$radius,
  #      ybottom = face_tbl$y - face_tbl$radius,
  #      ytop    = face_tbl$y + face_tbl$radius, border = "green", lwd = 5)
  # 
  # dev.off()
  
  # 3. 이미지 잘라내기 -------------
  figure_crop <- figure_img %>% 
    image_crop(geometry_area(x_off = face_tbl$x - face_tbl$radius * 1.5, 
                             y_off = face_tbl$y - face_tbl$radius * 1.5,
                             width = face_tbl$radius  * 2 * 1.5, 
                             height = face_tbl$radius * 2 * 1.5))
  
  # 4. 이미지 저장하기 -------------
  processed_filename <- fs::path_file(raw_image) %>% fs::path_ext_remove(.)
  
  figure_crop %>% 
    image_write(path =  glue::glue("data/speakers_face/{processed_filename}_face.png"))
  
  # return(figure_crop)
}

# extract_face('data/speakers/julia_silge.jpg')

extract_face('data/speakers/jinhwan_kim.jpg')

## 1.2. 발표자 얼굴 전체 변환 -------

speakers_fs <- fs::dir_ls("data/speakers/")

safely_extract_face <- safely(extract_face, otherwise = NA_real_)

walk(speakers_fs, safely_extract_face)

# 2. 졸업앨범 사진 -------
## 참고: https://statkclee.github.io/art/art-masked-profile.html

## 2.1. 졸업앨범 사진 함수 -------

circular_mask <- function(raw_image) {
  
  img <- magick::image_read(raw_image) %>% 
    image_resize(200)
  
  ii <- magick::image_info(img)
  ii_min <- min(ii$width, ii$height)
  cropped_img <- magick::image_crop(img, geometry=paste0(ii_min, "x", ii_min, "+0+0"), repage=TRUE)
  
  fig <- magick::image_draw(image_blank(ii_min, ii_min))
  symbols(ii_min/2, ii_min/2, circles=(ii_min/2)-3, bg='black', inches=FALSE, add=TRUE)
  dev.off()
  
  im2 <- magick::image_composite(cropped_img, fig, operator='copyopacity')
  
  created_img <- magick::image_background(im2, 'white')

  # 졸업앨범 내보내기  
  processed_filename <- fs::path_file(raw_image) %>% fs::path_ext_remove(.)
  
  created_img %>% 
    image_write(path = glue::glue("data/speakers_mask/{fs::path_ext_remove(processed_filename)}_mask.png"))
  
  # return(created_img)
}

circular_mask("data/speakers_face/julia_silge_face.png")

## 2.2. 전체 졸업앨범 자동화 -------

speakers_face_fs <- fs::dir_ls("data/speakers_face/")

safely_circular_mask <- safely(circular_mask, otherwise = NA_real_)

walk(speakers_face_fs, safely_circular_mask)

# 3. 데이터라이즈 GIF -------

sanghyun_img <- image_read("data/speakers_mask/kimsanghyun_face_mask.png")
minho_img <- image_read("data/speakers_mask/leeminho_face_mask.png")

image_join(c(minho_img, sanghyun_img)) %>%
  image_animate(fps=1) %>%
  image_write("data/speakers_mask//datarize_face_mask.gif")

# 4. 데이터라이즈 김상현님 얼굴 추출 문제 -------------


