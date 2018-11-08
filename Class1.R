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

Sortiment_ord <- Sortiment_hela %>%
  filter(SortimentText == "Ordinarie sortiment")