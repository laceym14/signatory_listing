#! /usr/local/bin/lr

print("********************************************")
print(date())
print("********************************************")

library(digest)
library(dplyr)
library(ggplot2)
library(googlesheets)
library(htmlwidgets)
library(lubridate)
library(plotly)
library(snakecase)
library(stringr)

# token <- gs_auth(cache = FALSE)
# gd_token()
# saveRDS(token, file = "googlesheets_token.rds")
gs_auth(token = "googlesheets_token.rds")

# 1. Get the data from GSheet
fils <- gs_ls()
meta <- gs_key("1yKMbAP2ATw7whC6Q563qSbLi1vkzCgbdmqcO-hP4EBQ")
dat <- gs_read(meta)
md5 <- digest(dat)

# 2. Stop if no update from last check
last_md5 <- readLines("last_md5.txt")
if(md5 == last_md5) { stop(paste("No changes.", date())) }

# 2b. Write the new md5
print("Changes detected.")
writeLines(md5, con = "last_md5.txt")

# 3. Prep the new data
names(dat) <- c("date_time", "category", "first_name", "last_name", "city", "state",
                "zipcode", "country", "degree", "institution", "position", "email")

dat$category <- if_else(dat$category == "A general supporter", 
                        "supporter", 
                        "scientist")
dat$first_name <- lapply(dat$first_name, function(x){
  allcap <- toupper(x)
  if_else(allcap == x, to_sentence_case(x), x)
}) %>% unlist()
dat$last_name <- lapply(dat$last_name, function(x){
  allcap <- toupper(x)
  if_else(allcap == x, to_sentence_case(x), x)
}) %>% unlist()
dat$city <- lapply(dat$city, function(x){
  allcap <- toupper(x)
  if_else(allcap == x, to_upper_camel_case(x, sep_out = " "), x)
}) %>% unlist()
dat$zipcode <- as.character(dat$zipcode)

# 4. Bind old and new data then clean
old <- readRDS("original_signatories_2019-07-24_clean.rds")
upd <- bind_rows(old, dat)
upd <- distinct(upd, first_name, last_name, email, zipcode, .keep_all = TRUE)

# 5. Create the HTML for the cards
sci <- filter(upd, category == "scientist")
n_sci <- length(sci$date_time)
sup <- filter(upd, category != "scientist")
n_sup <- length(sup$date_time)

sci_names <- paste0(sci$first_name, " ", sci$last_name)
sci_cards <- paste0("<div class='sig-card'>",
                "<div class='sig-container'>",
                "<h4>", sci_names, "</h4>",
                "<p style='font-size:smaller'><em>", sci$institution, "</em></p>",
                "<p style='font-size:smaller'>", sci$city, ", ", sci$state, "<br>", 
                sci$country, "</p>",
                "</div></div>") %>% paste(collapse = "\n")
sci_card_set <- paste0(
  "<div id='Scientists' class='tabcontent'>",
  "<h2 style='text-align:center; margin-bottom:1.5rem'>", 
  n_sci, " Scientist signatories</h2>",
  "<div class='sig-flex-container'>",
  sci_cards,
  "</div></div>", collapse = "\n"
)

sup_names <- paste0(sup$first_name, " ", sup$last_name)
sup_cards <- paste0("<div class='sig-card'>",
                    "<div class='sig-container'>",
                    "<h4>", sup_names, "</h4>",
                    "<p style='font-size:smaller'><em>", sup$institution, "</em></p>",
                    "<p style='font-size:smaller'>", sup$city, ", ", sup$state, "<br>", 
                    sup$country, "</p>",
                    "</div></div>") %>% paste(collapse = "\n")
sup_card_set <- paste0(
  "<div id='Supporters' class='tabcontent'>",
  "<h2 style='text-align:center; margin-bottom:1.5rem'>", 
  n_sup, " Supporter signatories</h2>",
  "<div class='sig-flex-container'>",
  sup_cards,
  "</div></div>", collapse = "\n"
)

# 5. Create some summary info
n_sig <- length(upd$date_time)

sum_card_set <- paste0(
  "<div id='Summary' class='tabcontent'>",
  "<div class='sig-flex-container' style='height:2000px'>",
  "<iframe src='https://defenders-cci.org/app/microapps/border_microapp/' width='900' frameBorder='0'></iframe>",
  "</div></div>"
)

html_pre <- readLines("prefix.html") %>% paste(collapse = "\n")
html_post <- readLines("suffix.html") %>% paste(collapse = "\n")

# Write the HTML to file
path <- "/home/jacobmalcom/site_root/sign-on/border-wall/signatories/index.html"
writeLines(paste0(html_pre, sci_card_set, sup_card_set, sum_card_set,
                  html_post), con = path)
saveRDS(upd, "current_signatories.rds")
