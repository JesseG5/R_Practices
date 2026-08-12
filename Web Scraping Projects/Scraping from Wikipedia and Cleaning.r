# I am first going to try to scrape from the wikipedia on Japan, first I will need to make sure to seperate the paragraphs from the rest of the text using .mw-parser-output p in html_elements()
# Then I will make sure to get rid of the sourcing because when it is drawn up using cat() it will appear as normal text and when trying to pull each sentence with specifically numbers that will interfere with the dataset
# Then I will pull the first ten sentences that have numbers in them
# Then I will make another list which specifically pulls all the percentages
# I need to start off by downloading the html from wikipedia, that part is the only one I am very unsure on and will definetly make AI do it until I can memorize the setup for it

#I memorized this script to download html

library(httr2)
library(xml2)

url <- "https://en.wikipedia.org/wiki/Japan"

html_doc <- request(url) |>
req_user_agent("Mozilla 5.0 (Windows 10.0; Win64; x64) Academic Research Bot(Indiana University: Jesgonz@iu.edu)") |>
req_perform() |>
resp_body_html()

write_html(html_doc, "WikionJapan.html")



# Now that we have saved it as that we can say our url we are pulling from is "WikionJapan.html"

url <- "WikionJapan.html"