# get factchecks:

library(rvest)
library(tidyverse)
library(lubridate)
library(RSelenium)

# get dpa factchecks ####
dpa <- "https://dpa-factchecking.com/germany/"
dpa_baseURL <- "https://dpa-factchecking.com"

dpa_factcheck <- read_html("articles/disinformation/dpa-factchecking.html")



links_dpa <- paste0(dpa_baseURL,
                    dpa_factcheck %>% html_nodes(".content a") %>% html_attr('href')
                    )

factchecks_dpa <- dpa_factcheck %>% html_element(".content") %>% html_table() %>% 
  rename("date" = "X1",
         "title" = "X2") %>% 
  bind_cols(links = links_dpa) %>% 
  mutate(date = as.POSIXct(date, format='%d/%m/%Y, %I:%M %p')) %>% 
  filter(between(date, as.POSIXct("2021-01-01"), as.POSIXct("2021-12-31")))

scholz_disinfo <- factchecks_dpa %>% filter(str_detect(title, "Scholz")) # really zero?
laschet_disinfo <- factchecks_dpa %>% filter(str_detect(title, "Laschet"))
baerbock_disinfo <- factchecks_dpa %>% filter(str_detect(title, "Baerbock"))

# handcoding if these were present on RT (current archive, things could have been deleted!) ####

  # Baerbock Ostdeutsche/Brandenburgerin: used sometime afterwards (https://de.rt.com/meinung/121919-brandenburgerin-baerbock-bestaunt-barnim-und/)
    # on the part of her "Wehrmacht-Gro�vater", 4 days after earliest(?) fb post (https://de.rt.com/meinung/117986-baerbock-auf-schultern-ihres-wehrmacht-gro%C3%9Fvaters/)
      # more on that, biased (non)coverage, plus repeating all previous personal debates (https://de.rt.com/meinung/117815-ein-politisches-manifest-doch-baerbocks-transatlantischer-auftritt-verhallt/)
  # Baerbock CV: explicitly denying russian influence, criticising biased factchecking (https://de.rt.com/meinung/117768-debatte-um-baerbocks-hochschul-abschluss/)
    # related: Baerbock CV: "V�lkerrechtlerin" (https://de.rt.com/inland/117681-traegt-kanzlerkandidatin-baerbock-ihren-titel-als-voelkerrechtlerin-rechtmaessig/)

# conspiracy narrative on Baerbock and INSM (https://de.rt.com/meinung/119508-scheingefechte-furs-kuschelhaeschen-insm-campact/) LOL
# causa Promotionsstipendium: (https://de.rt.com/inland/120507-baerbocks-promotionsstipendium-waren-die-zahlungen-nicht-gerechtfertigt/)

# RT on RT criticism etc.: plausible deniability, "Tagesschau immerhin nimmt den wirklich entscheidenden und wichtigen Satz aus einem der unz�hligen vertraulichen Berichte �ber RT DE mit: Die Berichterstattung von RT DE k�nne zwar insgesamt nicht als Desinformation bezeichnet werden, aber sie versuche "eher unterschwellig, beil�ufig, ohne aufsehenerregende plakative Anklagen den Vertrauensverlust in Institutionen zu f�rdern". (https://de.rt.com/meinung/119380-vertrauliche-berichte-ueber-rt-kontraste-tagesspiegel-dienste/)
# RT on biased coverage of politicians (https://de.rt.com/meinung/123954-grune-hui-rest-pfui-medienlieblinge-wahlkampf/)
# on YT prohibiting misinformation on election processes (https://de.rt.com/international/124904-youtube-verbietet-zweifel-am-ordnungsgemassen/)
# criticising Correctiv factchecks on RT (https://de.rt.com/inland/124844-faktenchecker-unterstellen-rt-de-vermittlung-eines-falschen-eindrucks-vom-wahlverfahren/)
# furthering apathy: partys similar, no meaning (https://de.rt.com/inland/124907-interview-alexander-rahr-parteien-stehen/)
# "Politik sollte jedem offen stehen, der sich daf�r begeistern kann, in einer Gruppierung von machthungrigen Soziopathen die Spitze zu erklimmen. Dazu ist jede L�ge, jede Halbwahrheit, jede Nebelkerze erlaubt." (https://de.rt.com/meinung/118825-verteidiger-ehre-annalena-baerbock/)
# on russian/turkish influence operations against greens (https://de.rt.com/inland/118650-cem-oezdemir-russland-und-tuerkei-betreiben-schmutzkampagnen-gegen-baerbock/)
# not much personalized, but strongly biased conspiracy beliefs (soros), common misrepresentation of ARD-volo survey (https://de.rt.com/meinung/124211-es-ist-kult-klima-gender-migration/)
# predicting pro-Baerbock mainstream media (https://de.rt.com/meinung/116267-annalena-baerbock-kanzlerin-medien/)
# comment by spokesperson of russian foreign ministry: self-image (https://de.rt.com/meinung/123605-lassen-sie-uns-mal-ueber-deutsche-welle-sprechen/)
# rebuttal on Politico report on RT contents (https://de.rt.com/meinung/123491-prompt-nach-ubernahme-durch-springer-magazin-politico-entdeckt-feindbild-rt-de/)
# Dugin on russian view on election, loads of conspiracy, and Baerbock-Soros connection (https://de.rt.com/inland/124923-alexander-dugin-ueber-ergebnisse-der-bundestagswahl/)


# very much on plagiarism
# interestingly: BILD/Springer as central opponent/competitor!



# get correctiv factchecks ####
correctiv_searchURL <- "https://correctiv.org/?s="

system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
rD <- rsDriver(browser = "chrome",
               chromever = "99.0.4844.51",
               verbose = F,
               javascript = T,
               nativeEvents = T,
               extraCapabilities = eCap)
remDr <- rD[["client"]]

page_source <- F
i = 1
for (i in 1:length(candidate)) {
  search <- paste0(correctiv_searchURL,
                   candidate[i])
  
  remDr$navigate(search)
  
  page_source_prev <- remDr$getPageSource()[[1]]
  
  while (page_source_prev != page_source) {
    
    page_source_prev <- remDr$getPageSource()[[1]]
    
    webElemEnd <- tryCatch({remDr$findElement("css", "body")},
                           error = function(e){NULL})
    try(webElemEnd$sendKeysToElement(list(key = "end")))
  
  randsleep <- sample(seq(sleepmin, sleepmax, by = 0.001), 1)
  Sys.sleep(randsleep)
  
  page_source <- remDr$getPageSource()[[1]]
  }
  
  writeLines(page_source, paste0("articles/disinformation/correctiv", candidate[i], ".txt"), useBytes = T)

} 

# get mimikama factchecks ####

# not really necessary, either less relevant or already covered:
# https://www.mimikama.at/?s=Baerbock

# on the other hand: some Scholz Fakes! (https://www.mimikama.at/?s=Scholz)

# for Laschet mostly not new: https://www.mimikama.at/?s=Laschet


