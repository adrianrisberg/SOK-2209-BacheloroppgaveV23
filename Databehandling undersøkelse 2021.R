# Velger norsk språk
Sys.setlocale(locale="no_NO")

# Setter riktig 'working directory' 
setwd("C:/Users/adria/OneDrive/Dokumenter/6 Semester/SOK-2209 - Bachelor")

# Laster inn nødvendige pakker
library(tidyverse)
library(ggplot2)
library(grid)
library(zoo)
library(fastDummies)

# Laster inn datasettet
NSD <- read.csv("Livskvalitetsundersøkelsen 2021/NSD2995.csv")

# Plukker ut interessante variabler
a <- NSD %>% 
  select(sysselsatt, aldgrupp, alder, kjoenn, sivilstand, lonn, h_lonn, saminnt, 
         h_wsaminnt, wies, lavinntekt, A1a, SKH7, H38, A4, C2, O1, h50c, barn, sysselsatt)


# Datawrangling. Velger ut hvilke variabler vi skal bruke, samt kategoriserer for alder
a2 <- a%>%
  select(saminnt, A1a, lavinntekt, sysselsatt, kjoenn, sivilstand, barn, aldgrupp, alder, h50c) %>% 
  filter(A1a <= 10) %>% 
  mutate(aldersgruppe = case_when(alder >= 65 ~ "Gammel",
                                  alder >= 18 & alder <= 30 ~ "Ung",
                                  alder >= 31 & alder <= 64 ~ "Middelaldrende")) %>% 
  filter(alder >= 18)

# Skaper dummy-variabler ut fra eksisterende, kategoriske variabler
a2 <- dummy_cols(a2, select_columns = "kjoenn")
  
a2 <- dummy_cols(a2, select_columns = "aldersgruppe") %>% 
  rename("mann" = "kjoenn_1") %>% 
  subset(select = -c(kjoenn, kjoenn_2))

a2 <- dummy_cols(a2, select_columns = "sivilstand") %>% 
  subset(select = -c(sivilstand, sivilstand_1, sivilstand_3, sivilstand_4, sivilstand_5,
                     sivilstand_8)) %>% 
  mutate(partner = case_when(sivilstand_2 == 1 & sivilstand_6 == 1 ~ 1,
                             sivilstand_2 == 0 & sivilstand_6 == 1 ~ 1,
                             sivilstand_2 == 1 & sivilstand_6 == 0 ~ 1,
                             sivilstand_2 == 0 & sivilstand_6 == 0 ~ 0))


a2 <- dummy_cols(a2, select_columns = "barn") %>% 
  rename("forelder" = "barn_1") %>% 
  subset(select = -c(barn, barn_2, barn_8, barn_9))

a2 <- dummy_cols(a2, select_columns = "sysselsatt") %>% 
  rename("arbeidsaktiv" = "sysselsatt_1") %>% 
  subset(select = -c(sysselsatt_2))

a3 <- dummy_cols(a2, select_columns = "h50c") %>% 
  rename("helseproblemer" = "h50c_1") %>% 
  subset(select = -c(h50c_2, h50c_8, h50c_9)) %>% 
  filter(saminnt > 0) %>% 
  mutate(log_saminnt = log(saminnt)) %>% 
  mutate(inntekt = saminnt/100000)

# Deskriptiv statistikk
library(vtable)

navn <- c("Lykke", "Samlet inntekt/100 000", "Ung", "Middelaldrende", "Gammel", "Mann",
          "Partner", "Forelder", "Arbeidsaktiv", "Helseproblemer")

st(a3, labels = navn,
   vars = c("A1a", "inntekt", "aldersgruppe_Ung", "aldersgruppe_Middelaldrende",
            "aldersgruppe_Gammel", "mann", "partner", "forelder", "arbeidsaktiv",
            "helseproblemer"),
   summ = c('notNA(x)','mean(x)','sd(x)','min(x)','max(x)'),
   digits = 4,
   summ.names = list(
     c('Antall observasjoner','Gjennomsnitt','Standardavvik',
       'Minimum','Maksimum')))



#OLS-modell med flere variabler
ols2 <- lm(A1a ~ inntekt + I(inntekt^2) + aldersgruppe_Ung +
            aldersgruppe_Gammel + inntekt:aldersgruppe_Ung +
            inntekt:aldersgruppe_Gammel + mann + partner + forelder + arbeidsaktiv +
            helseproblemer, data = a3)

tab_model(ols2,
          encoding = "UTF-8",
          dv.labels = "Effekt på lykke",
          digits = 3,
          string.pred = "Variabler",
          string.est = "Estimat",
          string.ci = "Konfidensintervall",
          string.p = "P-verdi",
          string.intercept = "Konstantledd",
          CSS = list(
            css.depvarhead = 'color: red;',
            css.centeralign = 'text-align: left;', 
            css.firsttablecol = 'font-weight: bold;', 
            css.summary = 'color: blue;', 'font-weight:bold;',
            css.table = 'border:2px solid red;'))

tab_model(ols2, file= "OLS2.html",
          encoding = "UTF-8",
          dv.labels = "Effekt på lykke",
          digits = 3,
          string.pred = "Variabler",
          string.est = "Estimat",
          string.ci = "Konfidensintervall",
          string.p = "P-verdi",
          string.intercept = "(Konstantledd)",
          CSS = list(
            css.depvarhead = 'color: red;',
            css.centeralign = 'text-align: left;', 
            css.firsttablecol = 'font-weight: bold;', 
            css.summary = 'color: blue;', 'font-weight:bold;',
            css.table = 'border:2px solid red;'))

# Figurer med predikerte verdier
fit <- lm(A1a ~ aldersgruppe + inntekt + alder + inntekt:aldersgruppe, data = a3)

set_theme(base = theme_light())

plot_model(fit, type = "pred", terms = c("inntekt", "aldersgruppe"),
           title = "Predikerte verdier for lykke fordelt på inntekt",
           axis.title = c("Inntekt / 100 000", "Lykke")) + 
  scale_color_manual(values=c("red", "blue", "green")) +
  scale_fill_manual(values=c("red", "blue", "green"))

plot_model(fit, type = "pred", terms = c("inntekt"),
           title = "Predikert effekt av inntekt på lykke",
           axis.title = c("Inntekt / 100 000", "Lykke"))

fit2 <- lm(A1a ~ aldersgruppe + inntekt + alder +
             inntekt:aldersgruppe_Ung + aldersgruppe_Ung + aldersgruppe_Middelaldrende + aldersgruppe_Gammel +
             inntekt:aldersgruppe_Gammel + inntekt:aldersgruppe, data = a3)

plot_model(fit2, type = "pred", terms = c("aldersgruppe"),
           title = "Predikerte verdier for lykke basert på aldersgruppe",
           axis.title = c("Aldersgruppe", "Lykke"))

# Plotter samme graf i ggplot for å ha større muligheter for visuell tilpasning
library(ggeffects)
library(ggrepel)

df <- ggpredict(fit2, terms = c("aldersgruppe"))

ggplot(df, aes(x, predicted)) +
  geom_point(aes(color = x), size = 3.5, alpha = 0.8) +
  geom_errorbar(aes(ymin=predicted-std.error, ymax=predicted+std.error), width=.05) +
  geom_line(aes(x=x, y=predicted, group = 1), linetype = "dashed") +
  scale_color_manual(values=c("red", "blue", "green")) +
  geom_text_repel(aes(label = round(predicted, 2)), size = 3, nudge_x = 0.2) +
  labs(title = "Predikerte verdier for lykke",
       subtitle = "For hver aldersgruppe",
       x = "Aldersgruppe",
       y = "Lykke",
       color = "Aldersgruppe") +
  theme_light()
