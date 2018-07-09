us_blkgrps <- stringr::str_pad(us_block_groups, width = 12,
                               side = "left", pad = "0")

fips_table <- data.frame(`block group` = us_blkgrps,
                         tract = stringr::str_sub(us_blkgrps, 1, 11),
                         county = stringr::str_sub(us_blkgrps, 1, 5),
                         state = stringr::str_sub(us_blkgrps, 1, 2),
                         short_county = stringr::str_sub(us_blkgrps, 3, 5),

                         check.names = FALSE,
                         stringsAsFactors = FALSE)
