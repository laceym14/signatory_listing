library(dplyr)
library(googlesheets)
library(lubridate)
library(snakecase)
library(stringr)

fils <- gs_ls()

meta <- gs_title("border_wall_signatories")
dat <- gs_read(meta)

# De-duplicate
dedup <- distinct(dat, first_name, last_name, email, zipcode, .keep_all = TRUE)

# Simplify category
dedup$category <- if_else(dedup$category == "A general supporter", 
                          "supporter", 
                          "scientist")

# Fix some ASCII errors
tmp <- dedup %>%
  mutate_all(funs(str_replace_all(., "√©", "é"))) %>%
  mutate_all(funs(str_replace_all(., "√®", "è"))) %>%
  mutate_all(funs(str_replace_all(., "√´", "ë"))) %>%
  mutate_all(funs(str_replace_all(., "√™", "ê"))) %>%
  mutate_all(funs(str_replace_all(., "√â", "É"))) %>%
  mutate_all(funs(str_replace_all(., "√≠", "í"))) %>%
  mutate_all(funs(str_replace_all(., "√ç", "í"))) %>%
  mutate_all(funs(str_replace_all(., "√±", "ñ"))) %>%
  mutate_all(funs(str_replace_all(., "√ß", "ç"))) %>%
  mutate_all(funs(str_replace_all(., "√≥", "ó"))) %>%
  mutate_all(funs(str_replace_all(., "√ì", "ó"))) %>%
  mutate_all(funs(str_replace_all(., "√∂", "ö"))) %>%
  mutate_all(funs(str_replace_all(., "√≤", "ò"))) %>%
  mutate_all(funs(str_replace_all(., "√∏", "ø"))) %>%
  mutate_all(funs(str_replace_all(., "√∫", "ú"))) %>%
  mutate_all(funs(str_replace_all(., "√º", "ü"))) %>%
  mutate_all(funs(str_replace_all(., "√Å", "Á"))) %>%
  mutate_all(funs(str_replace_all(., "√†", "à"))) %>%
  mutate_all(funs(str_replace_all(., "√¢", "à"))) %>%
  mutate_all(funs(str_replace_all(., "√°", "á"))) %>%
  mutate_all(funs(str_replace_all(., "√§", "ä"))) %>%
  mutate_all(funs(str_replace_all(., "√£", "ã")))

# Split date, time
tmp$date <- str_split(tmp$date_time, " ") %>% 
  lapply(`[[`, 1) %>%
  unlist() %>% as.Date("%m/%d/%y")

tmp$time <- str_split(tmp$date_time, " ") %>% 
  lapply(`[[`, 2) %>%
  unlist()

# clean up names
tmp$first_name <- lapply(tmp$first_name, function(x){
  allcap <- toupper(x)
  if_else(allcap == x, to_sentence_case(x), x)
})
tmp$last_name <- lapply(tmp$last_name, function(x){
  allcap <- toupper(x)
  if_else(allcap == x, to_sentence_case(x), x)
})
tmp$city <- lapply(tmp$city, function(x){
  allcap <- toupper(x)
  if_else(allcap == x, to_upper_camel_case(x, sep_out = " "), x)
})

# homogenize states; not worrying about abbreviated provinces, etc. for now...
st_trns <- state.name[match(tmp$state, state.abb)]
long_state <- if_else(is.na(st_trns), tmp$state, st_trns)
long_state <- lapply(long_state, function(x){
  if_else(x == "DC", "District of Columbia", x)
}) %>% unlist()
long_state <- lapply(long_state, function(x){
  if_else(nchar(x) == 2, state.name[match(toupper(x), state.abb)], x)
}) %>% unlist()
long_state <- lapply(long_state, function(x){
  if_else(nchar(x) <= 4, to_upper_camel_case(x, sep_out = " "), x)
}) %>% unlist()
long_state <- lapply(long_state, function(x){
  if_else(nchar(x) > 4, to_upper_camel_case(tolower(x), sep_out = " "), x)
}) %>% unlist()
tmp$state <- long_state

# Degree
ph_pat <- "PhD|DVM|DPhil|Post-doc|Postdoc|Ph D|MD|Dr|Doctor|Doctorado"
ms_pat <- "Master|MS|MA|MBA|MPH" 
degree_short <- lapply(tmp$degree, function(x){
  x <- str_replace_all(x, "\\.", "")
  phd <- length(grep(x, pattern = ph_pat, ignore.case = TRUE)) > 0
  mas <- length(grep(x, pattern = ms_pat, ignore.case = TRUE)) > 0
  if_else(phd, "Doctorate", if_else(
      mas, "Master", "Other"))
}) %>% unlist()
table(degree_short) %>% sort(decreasing = TRUE) %>% head(25)
table(degree_short) %>% sort(decreasing = TRUE) %>% sum()
tmp$degree_short <- degree_short

# Make a new file to match current sign-on
final <- select(tmp, c(2,4:10,12:14,11))
rio::export(final, "signatories_2019-07-24.xlsx")
