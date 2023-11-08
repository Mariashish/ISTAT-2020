library(ggplot2)
library(plotly)
library(tidyverse)
require(magrittr)
require(dplyr)
library(scales)
library(sf)
library(tmap)
library(lme4)
library(lmerTest)
library(gridExtra)


#INITIALIZERS
load("Dati/istat2020_agg.RData")
percorso_shapefile <- "Dati/shapefile/"
italy_shape <- st_read(percorso_shapefile) 
italy_shape$reg_code <- as.character(italy_shape$reg_code)

ISTAT2020_agg %<>% select(-where(anyNA))


#corrispondenza <- c(
#  "PIEMONTE", "VALLE D'AOSTA", "LOMBARDIA", "TRENTINO A. A.", "VENETO",
#  "FRIULI V. G", "LIGURIA", "EMILIA ROMAGNA", "TOSCANA", "UMBRIA",
#  "MARCHE", "LAZIO", "ABRUZZO", "MOLISE", "CAMPANIA", "PUGLIA", "BASILICATA",
#  "CALABRIA", "SICILIA", "SARDEGNA"
#)
#ISTAT2020_agg$regione_famiglia <- corrispondenza[ISTAT2020_agg$regione_famiglia]


glimpse(ISTAT2020_agg)
summary(ISTAT2020_agg)



ISTAT2020_agg %<>% drop_na()
view(ISTAT2020_agg)

ISTAT2020_agg <- ISTAT2020_agg %>% filter(tot_spese_mensili != 0)
view(ISTAT2020_agg)


#--------------------------------------------------------------------------------------------------------------------------------------

#RAPPORTI ISTRUZIONE E ALCOLICI E TABACCHI RISPETTO ALLA SPESA TOTALE

SpesaAlcoliciTabacchiSuTot <- ISTAT2020_agg %>% group_by(regione_famiglia) %>% summarise(across(c(tot_spese_mensili, istruzione, alcolici_tabacchi), mean))
view(SpesaAlcoliciTabacchiSuTot)


SpesaAlcoliciTabacchiSuTot$tot_spese_mensili <- round(SpesaAlcoliciTabacchiSuTot$tot_spese_mensili, digits = 2)
SpesaAlcoliciTabacchiSuTot$istruzione <- round(SpesaAlcoliciTabacchiSuTot$istruzione, digits = 2)

SpesaAlcoliciTabacchiSuTot$alcolici_tabacchi <- round(SpesaAlcoliciTabacchiSuTot$alcolici_tabacchi, digits = 2)
SpesaAlcoliciTabacchiSuTot$RapportoAlcolTabacchiSpesaTot <- round((SpesaAlcoliciTabacchiSuTot$alcolici_tabacchi/SpesaAlcoliciTabacchiSuTot$tot_spese_mensili) * 100, digits = 2)
SpesaAlcoliciTabacchiSuTot$RapportoIstruzioneSpesaTot <- round((SpesaAlcoliciTabacchiSuTot$istruzione/SpesaAlcoliciTabacchiSuTot$tot_spese_mensili) * 100, digits = 2)


SpesaAlcoliciTabacchiSuTot$regione_famiglia <- as.numeric(SpesaAlcoliciTabacchiSuTot$regione_famiglia)


PSpesaAlcolTabacchi <- ggplot(SpesaAlcoliciTabacchiSuTot, aes(x = regione_famiglia, y = RapportoAlcolTabacchiSpesaTot, fill = regione_famiglia)) +
  geom_bar(stat = "identity") +
  labs(title = "Rapporto Alcolici e Tabacchi/Spesa Totale per Regione in Percentuale (%)")
#print(PSpesaAlcolTabacchi)

ggplotly(PSpesaAlcolTabacchi)


PIstruzione <- ggplot(SpesaAlcoliciTabacchiSuTot, aes(x = regione_famiglia, y = RapportoIstruzioneSpesaTot, fill = regione_famiglia)) +
  geom_bar(stat = "identity") +
  labs(title = "Rapporto Istruzione/Spesa Totale in Percentuale (%)")
#print(PIstruzione)

ggplotly(PIstruzione)


grid.arrange(PIstruzione, PSpesaAlcolTabacchi, ncol = 2)



SpesaAlcoliciTabacchiSuTot$regione_famiglia <- as.character(SpesaAlcoliciTabacchiSuTot$regione_famiglia)

AlcoliciTabacchiShape <- left_join(italy_shape, SpesaAlcoliciTabacchiSuTot, by = c("reg_code" = "regione_famiglia"))

MappaSpesaAlcolTabacchi <- tm_shape(AlcoliciTabacchiShape) + tm_borders() + tm_fill(col = "RapportoAlcolTabacchiSpesaTot", style = "jenks", palette = "Reds", title = "Spesa in Alcolici e Tabacchi per Regione in Percentuale (%)")

MappaSpesaAlcolTabacchi <- MappaSpesaAlcolTabacchi + tm_text(text = c("RapportoAlcolTabacchiSpesaTot"), size = 1, root = 2)

MappaSpesaAlcolTabacchi <- MappaSpesaAlcolTabacchi + tm_layout(legend.show = FALSE)

MappaSpesaAlcolTabacchi <- MappaSpesaAlcolTabacchi + tm_legend(legend.position = c("left", "bottom"))

tmap_mode("plot")
tmap_leaflet(MappaSpesaAlcolTabacchi)



SpesaAlcoliciTabacchiSuTot$regione_famiglia <- as.character(SpesaAlcoliciTabacchiSuTot$regione_famiglia)

IstruzioneShape <- left_join(italy_shape, SpesaAlcoliciTabacchiSuTot, by = c("reg_code" = "regione_famiglia"))

MappaIstruzione <- tm_shape(IstruzioneShape) + tm_borders() + tm_fill(col = "RapportoIstruzioneSpesaTot", style = "jenks", palette = "Greens", title = "Rapporto Istruzione/Spesa Totale per Regione in Percentuale (%)")

MappaIstruzione <- MappaIstruzione + tm_text(text = c("RapportoIstruzioneSpesaTot"), size = 1, root = 2)

MappaIstruzione <- MappaIstruzione + tm_layout(legend.show = FALSE)

MappaIstruzione <- MappaIstruzione + tm_legend(legend.position = c("left", "bottom"))

tmap_mode("plot")
tmap_leaflet(MappaIstruzione)

#--------------------------------------------------------------------------------------------------------------------------------------

#REGRESSIONE LINEARE SEMPLICE

regATSanita <- lm(alcolici_tabacchi~sanità, data = ISTAT2020_agg)
summary(regATSanita)
print(regATSanita)


ISTAT2020_agg$condizione_vita <- as.factor(ISTAT2020_agg$condizione_vita)
#REGRESSIONE LINEARE SEMPLICE CON EFFETTI CASUALI
modSpeseMensili <- lmer(tot_spese_mensili~n_comp_fam + condizione_vita + (1|regione_famiglia), data = ISTAT2020_agg)
summary(modSpeseMensili)

#GRAFICO REGRESSIONE LINEARE SEMPLICE CON EFFETTI CASUALI
modSpeseMensiliPlt <- ggplot(data = ISTAT2020_agg, aes(x = n_comp_fam, y = tot_spese_mensili, color = regione_famiglia)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "red") +
  labs(title = "Regressione Lineare Semplice",
       x = "Numero di Componenti della Famiglia",
       y = "Spese Mensili Totali") +
  theme_minimal() +
  theme(legend.position = "right")

ggplotly(modSpeseMensiliPlt)
#print(modSpeseMensiliPlt)



#MODELLO SOLO INTERCETTA

SpeseMensiliIOM<-lmer(tot_spese_mensili~1 + (1|n_comp_fam), data=ISTAT2020_agg)
summary(SpeseMensiliIOM)



#MODELLO BINOMIALE

ISTAT2020_agg[ISTAT2020_agg$sesso_1 == 1,]$sesso_1 <- 0
ISTAT2020_agg[ISTAT2020_agg$sesso_1 == 2,]$sesso_1 <- 1

ISTAT2020_agg$tot_spese_mensili <- scale(ISTAT2020_agg$tot_spese_mensili) #Standardizzazione ad una normale con media 0 e varianza 1


bm <- glm(soglia_poverta_rel~tot_spese_mensili + n_comp_fam, data=ISTAT2020_agg, family=binomial)
summary(bm)

modBinomPlt <- ggplot(data = ISTAT2020_agg, aes(x = n_comp_fam, y = soglia_poverta_rel)) +
  geom_point(size = 3) +
  geom_smooth(method = "glm", se=FALSE, method.args=list(family="binomial"), linetype = "dashed", color = "red") +
  labs(title = "Modello Binomiale",
       x = "Numero compoenenti famiglia",
       y = "Soglia di Povertà Relativa") +
  theme_minimal() +
  theme(legend.position = "right")  # Posiziona la legenda a destra

ggplotly(modBinomPlt)
#print(modSpeseMensiliPlt)





#MODELLO POISSON

pm = glm(sanità~alcolici_tabacchi + trasporti + ristorazione, family=poisson, data = ISTAT2020_agg)
summary(pm)


