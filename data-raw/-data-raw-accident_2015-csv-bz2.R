## code to prepare `./data-raw/accident_2015.csv.bz2` dataset goes here

usethis::use_data(./data-raw/accident_2015.csv.bz2, overwrite = TRUE)

d2015 <- fars_read(./data-raw/accident_2015.csv.bz2)
use_data(d2015)
