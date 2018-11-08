library(tidyverse)
Sortiment_hela <- read_csv("Class_files/systembolaget2018-10-08.csv")

Sortiment_hela %>% pull(Alkoholhalt) %>% head


Sortiment_hela <- Sortiment_hela %>% 
  mutate(Alkoholhalt = as.numeric(gsub("%", "", Alkoholhalt))/100)

View(filter(Sortiment_hela, Varugrupp == "Röda"))

Sortiment_hela <- Sortiment_hela %>%
  mutate(Varugrupp = ifelse(Varugrupp=="Röda","Rött vin",ifelse(Varugrupp=="Vita","Vitt vin",Varugrupp)))

MaxPriceName <- Sortiment_hela %>%
  filter(PrisPerLiter == max(PrisPerLiter)) %>%
  select(Namn)

library(knitr)

Sortiment_ord <- Sortiment_hela %>%
  filter(SortimentText == "Ordinarie sortiment")

mostExpensive <- Sortiment_ord %>%
  arrange(desc(PrisPerLiter)) %>%
  select(Artikelid, Namn, Volymiml, Varugrupp, Alkoholhalt, PrisPerLiter) %>% 
  head(10)

kable(mostExpensive)

library(ggplot2)

ggplot(Sortiment_ord, aes(x = Alkoholhalt, y = PrisPerLiter, color = Varugrupp)) +
  geom_point() +
  scale_y_log10()

ggplot(Sortiment_ord, aes(Varugrupp, PrisPerLiter)) +
  geom_point() +
  scale_y_log10() +
  coord_flip()

wines <- Sortiment_ord %>% 
  filter(Varugrupp %in% c("Vitt vin", "Rött vin", "Rosévin", "Mousserande vin"),
         Argang %in% c(2010:2017))

ggplot(wines, aes(Argang, PrisPerLiter, color = Varugrupp)) +
  geom_point()

ggplot(wines, aes(Argang, PrisPerLiter)) +
  geom_point() +
  facet_wrap(~ Varugrupp)

SIFF_data <- read_csv("Class_files/Film_events_2018-11-07.csv")

soldOutFilms <- SIFF_data %>% 
  filter(eventTicketStatus == "soldout") %>% 
  select(eventName_en, eventDate, eventTime)

filmsPerVenue <- SIFF_data %>% 
  select(venueName,eventName_en) %>% 
  unique() %>% 
  count(venueName) %>% 
  arrange(desc(n))

soldoutRatio <- SIFF_data %>% 
  select(eventDate, eventTicketStatus) %>% 
  mutate(soldOut = ifelse(eventTicketStatus == "soldout", 1, 0)) %>% 
  group_by(eventDate) %>% 
  summarise(n_soldout = sum(soldOut), n = n()) %>% 
  mutate(soRatio = n_soldout / n)

ggplot(soldoutRatio, aes(eventDate, soRatio)) +
  geom_point()
