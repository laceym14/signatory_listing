library(digest)
library(dplyr)
library(googlesheets)

fils <- gs_ls()

meta <- gs_title("signatories_2019-07-24")
dat <- gs_read(meta)
md5 <- digest(dat)

current_md5 <- "notarealmd5"
current_md5 <- readLines(
  con = "/home/jacobmalcom/cronjobs/border_signatories/current_md5"
)

#if digests match, terminate the program
if(current_md5 == md5) { break }

sci <- filter(dat, category == "scientist")
n_sci <- length(sci$date_time)
sup <- filter(dat, category != "scientist")
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

sum_card_set <- paste0(
  "<div id='Summary' class='tabcontent'>",
  "<div class='sig-flex-container'>",
  "<h1>Coming soon!</h1>",
  "</div></div>"
)

html_pre <- readLines("prefix.html") %>% paste(collapse = "\n")
html_post <- readLines("suffix.html") %>% paste(collapse = "\n")

# Writing in the project dir for this demo, but just change con to the
# absolute path for signatories page 
writeLines(paste0(html_pre, sci_card_set, sup_card_set, sum_card_set,
                  html_post), con = "index.html")

