# Utfordring 3.1
# Oppgave 1.

# Fjerner alt fra Global Envirement så ingenting kan klusse til 
# koden senere.
rm(list = ls())

# Laster inn nødvendige pakker.
library(OECD)   
library(ineq)
library(httr)
library(dplyr)
library(ggpubr)
library(readxl) 
library(rjstat)
library(ggrepel) 
library(rstatix)
library(cowplot)
library(janitor)
library(ggplot2)
library(gglorenz)
library(tidyverse)  
library(gridExtra)
library(hrbrthemes)
library(PxWebApiData)

# Setter lokalet til no-No for å få norsk språk (for å få øæå).
Sys.setlocale(locale="no_NO")

# Setter arbeidsplassen.
setwd("~/")

# Laster in URL-en fra ssb.
URL <- "https://data.ssb.no/api/v0/no/table/11155/" 

# Laster in data-en fra ssb.
data <- '{
  "query": [
    {
      "code": "Kjonn",
      "selection": {
        "filter": "item",
        "values": [
          "0"
        ]
      }
    },
    {
      "code": "Alder",
      "selection": {
        "filter": "item",
        "values": [
          "20-64",
          "15-24"
        ]
      }
    },
    {
      "code": "UtdNivaa",
      "selection": {
        "filter": "item",
        "values": [
          "TOT"
        ]
      }
    },
    {
      "code": "ContentsCode",
      "selection": {
        "filter": "item",
        "values": [
          "ArbLedigProsent"
        ]
      }
    }
  ],
  "response": {
    "format": "json-stat2"
  }
}'

# Slår sammen URL og data til tmp med POST funksjonen.
tmp <- POST(URL, body = data, encode = "json", verbose())

# Gjør om til tibble og rydder navnene.
un.empl <- fromJSONstat(content(tmp, "text")) %>% 
  clean_names() %>% 
  as_tibble()

rm(data, URL, tmp)

# Endrer på kolonne navnene.
names(un.empl) <- c('Kjønn', 'Alder', 'Utdanningsnivå', 
                    'Statistikkvariabel', 'År', 'Value')

# Lager to dataset, en med ung alder og en med gammel alder.
un.empl.young <- filter(un.empl, Alder == "15-24 år")
un.empl.old <- filter(un.empl, Alder == "20-64 år")


# Plot 1.
un.empl %>% 
  group_by(År, Alder) %>% 
  summarize(Verdi=mean(Value)) %>% 
  ggplot(aes(x=År,y=Verdi,color=Alder, group=Alder)) +
  geom_line(aes(group=Alder), size=1) +
  geom_point(size=2.5)+
  labs(x = "År" , y ="Arbeidsledige i prosent %")  + 
  theme(legend.position="none")+
  geom_label_repel(data=un.empl %>% 
                     group_by(Alder) %>% 
                     filter(Value==min(Value)), 
                   aes(År, Value, 
                       fill = factor(Alder), 
                       label = sprintf('%s', Alder)), 
                   color = "black", 
                   fill = "white")

# Grafen viser antall arbeidsledige i prosent  i alderen 15-24 og voksne 
# i alderen 20-64 (begge kjønn, alle utdanningsnivåer). Her trengte vi 
# egentlig bare å hente den siste informasjon om arbeidsledighet blant 
# ungdommer og voksne (et tall fra hver). Det ble likevel vissuelt finere 
# og ta med alle obeservasjonene fra år 2006 - 2020.

# Grafen viser enkelt forklart at 3 % av de i alderen 20-64 var 
# arbeidsledig i 2006 og litt over 9 % i alderen 15-24. 
# Så av 10 personer i alderen 15-24 ville 1 av de "garantert" vært 
# arbeidsledig. De voksne er mindre arbeidsledige enn de unge fordi de mest 
# sannsynlig er kommet ut i arbeidsmarkedet. 

# I et perfekt arbeidsmarked, hvor lønnssatsen bestemmes
# i bransjen, i stedet for den enkelte bedrift, er hver bedrift en 
# lønnstaker. Dette betyr at den reelle likevektslønnen bestemmes av 
# markedet og arbeidstilbudet til et enkelt firma er helt fleksibelt i 
# henhold til  markedsrenten.

# Et perfekt arbeidsmarked har følgende kjennetegn:
# (1) Et stort antall bedrifter som konkurrerer om å ansette en bestemt 
# type arbeidskraft.
# (2) Mange mennesker med homogene ferdigheter som uavhengig leverer sine 
# arbeidstjenester.
# (3) Lønnsmottakende atferd.
# (4) Perfekt, kostnadsfri informasjon og arbeidsmobilitet.

# Ulempen med et perfekt arbeidsmarked er at det er ingen sjanse til å 
# oppnå maksimal fortjeneste på grunn av det store antallet andre firmaer 
# som selger de samme produktene.

# Minstelønnen bestemmes av skjæringspunktet mellom etterspørsel og tilbud. 
# Når lønnen i et bestemt marked er etablert, tar individuelle firmaer i 
# perfekt konkurranse den som gitt. Fordi hvert firma er en pristaker, 
# står det overfor en horisontal tilbudskurve for arbeidskraft til 
# markedslønn.

# Minstelønn er essensielt for å regulere klasseforskjeller fordi 
# det forhindrer at folk blir byttet ut.

# Det er viktig å huske på at perfekte markeder er teoretiske og kan ikke 
# eksistere i den virkelige verden, alle virkelige markeder er ufullkomne.
# Landbruksmarkeder er nok det nærmeste man kommer et perfekt marked pga 
# deres like varer, tjenester og priser. Markedet er preget av fri inn- og 
# utgang, med produsenter forpliktet til å være pristakere.


# Oppgave 2
# Skript for UTF 3.
# Vi ønsker å lage en graf som viser korrelasjonen mellom minimum
# lønn og arbeidsledighet. Vi må søke etter data i OECDs dataramme
# om disse emnene.
# Søk i datasett for minstelønn og arbeidsledighetsstatistikk.
dsets<-get_datasets()
search_dataset("wage",dsets)
search_dataset("unemployment",dsets)

# Data om minstelønn er tilgjengelig i "MIN2AVE".
# Data om arbeidsledighet er tilgjengelig i "MIG_NUP_RATES_GENDER".

# Minwage.
minwage <- get_dataset("MIN2AVE",
                       filter = "USA+CAN+FRA+GBR+DEU+NZL", 
                       pre_formatted = TRUE)

# Velge år og minstelønn som andel av medianlønn.
minwage2019 <- subset(minwage, Time < 2019 & Time >2007 & SERIES=="MEDIAN")
minwage2007_2019 <- subset(minwage2019, Time>2007)

# UnEpl.
unempl <- get_dataset("MIG_NUP_RATES_GENDER",
                      filter = "USA+CAN+FRA+GBR+DEU+NZL", 
                      pre_formatted = TRUE)

# Velger år, arbeidsledigheten for personer født i landet, og begge kjønn.
unempl2019 <- subset(unempl, Time<2019 & RATE=="U_RATE" & BIRTH=="NB" & GENDER=="TOT")
unempl2007_2019 <- subset(unempl2019, Time>2007)

# Kombinering av datasett – vi må slå sammen etter både land og år for å få 
# riktig nummer på rett sted.
minwage_unempl <-left_join(minwage2007_2019, unempl2007_2019, by=c("COUNTRY","Time"))

# Fjerner land med manglende data.
complete_minwage_unempl <- na.omit(minwage_unempl)

# Transformerer minstelønn og arbeidsledighet til numeriske variabler.
# MinWage er mellom 0 og 1, jeg vil transformere den til mellom 0 og 100 
# senere, så jeg kaller den MinWage_0 her
complete_minwage_unempl$MinWage_0 <- as.numeric(complete_minwage_unempl$ObsValue.x) 
complete_minwage_unempl$UnEmpl <- as.numeric(complete_minwage_unempl$ObsValue.y)

# Transformering av minstelønn til prosent.
complete_minwage_unempl$MinWage <- complete_minwage_unempl$MinWage_0 * 100

# Fjerner land canada.
complete_minwage_unempl <- subset(complete_minwage_unempl, COUNTRY!="CAN")

# Plot 2.
minwage_plot1 <- ggplot(data=complete_minwage_unempl,
                        aes(x=UnEmpl,y=MinWage, 
                            group=COUNTRY, color=COUNTRY)) +
  geom_line(aes(group=COUNTRY), size=1) +
  geom_point(size=2.5)+
  labs(x = "UnEmpl" , y ="MinWage")  + 
  theme(legend.position="none")+
  geom_label_repel(data=complete_minwage_unempl %>% 
                     group_by(COUNTRY) %>% 
                     filter(UnEmpl==min(UnEmpl)), 
                   aes(UnEmpl, MinWage, 
                       fill = factor(COUNTRY), 
                       label = sprintf('%s', COUNTRY)), 
                   color = "black", 
                   fill = "white") 
minwage_plot1

# Grafen viser sammenhengen mellom minstelønn som prosent av medianlønn 
# og arbeidsledighet i prosent i Frankrike (FRA), Tyskland (DEU), USA (USA),
# Storbritannia (GBR), og Nya Zeeland (NZL) for perioden 2008-2018. 
# (Canada (CAN) er ikke med i grafen). Min Wage eller gjennomsnittslønnen 
# som det heter på Norsk er de ansatte mottar for det samme arbeidet utført
# i en gitt tidsperiode. Ved å legge sammen all lønnen til den ansatte i en
# enkelt jobb eller bransje og dele den summen på antall ansatte, får vi 
# gjennomsnittslønnen. Mens "UnEmlp" er arbeidsledighet og oppstår når 
# det er større tilbud etter arbeidskraft enn det som etterspørres.

# USA som kommer dårligst ut på grafen har lav medianlønn og høy 
# arbeidsledighet. 

# Uten minstelønnen ville flere kommet i arbeid. Dette er grunnleggende 
# tilbud og etterspørsel, at dersom prisen øker vil etterspørselen 
# gå ned. I denne sammenhengen menes det at dersom laveste tillatte lønn 
# settes høyere vil etterspørselen etter denne type arbeidskraft gå ned. 
# Kort fortalt betyr det at hvis minimumslønnen stiger vil arbeidsledigheten
# også stige.

# De som hadde tjent på at minstelønnen ble borte er arbeidstakere 


# Oppgave 3
# GBR (Storbritannia) som ligger i midten på grafen har gjennomsnittslig 
# medianlønn og gjennomsnittslig høy arbeidsledighet. Det er flere faktorer 
# som har spillt inn for at GBR ligger så "bra" ann som de gjør. 
# Den første faktoren er:

# Finnanskrisen som startet i 2008 (samme startperiode som grafen) rammet 
# først og fremst USA, men etterhvert resten av verden, finnanskrisen 
# oppsto ved at prisen på gjeldspapirer ble for høy, og banker lånte 
# dermed ut altfor mye penger både til middelklassen, men også til folk 
# som ikke kunne betale tilbake. Etter at den amerikanske storbanken 
# Lehman Brothers gikk konkurs 15. september 2008 forsvant mye av tilliten 
# mellom bankene og dette var en av grunnene som sendte arbeidsledigheten 
# i USA til 10,0 % i oktober 2008. Dette påvirket såklart alle landene på 
# grafen.

# En annen faktor er at lav arbeidsledighet speiler høyere sysselsetning

# I 2018 har GBR 4 % i arbeidsledighet, som vil si en nedgang på over 
# -1.5 % på 10 år. På grunnlag av at "år" variabelen mangler fra grafen er 
# det vanskelig å henvise til når noe skjedde. Så når det står at GBR har 
# 4 % arbeidsledige vet vi ikke hvilken år det er snakk om med mindre man 
# går inn og ser på datasettet / tabellen. (kommer nærmene inn på dette i 
# neste oppgave).


# Oppgave 4
# Grafen kan gjerne gi et profesjonelt inntrykk. Det er sant at USA har 
# større arbeidsledighet enn de andre oppgitte landene. Sammenhengen 
# mellom landene er imidlertid mer tvilsom.
# En bedre måte å fremstille det samme på ville vært å ta med variabelen 
# "år". Altså å lage to grafer, en som viser minstelønn som prosent av 
# medianlønn sammenlignet med år. Og en som viser arbeidsledighet i 
# prosent sammenlignet med år. For uten "år" variabelen får vi kun vite 
# sammenhengen mellom det ene og det andre, uten å se på når det skjedde.

# Istede for bare å gi forslag på en bedre metode å teste hvordan 
# minstelønn påvirker arbeidsledighet og sysselsetting skal det fremvises 
# grafisk.
minwage_plot2 <- ggplot(data=complete_minwage_unempl,
                        aes(x=Time,y=MinWage, # Endrer x-aksen til Time (år)
                            group=COUNTRY, color=COUNTRY)) +
  geom_line(aes(group=COUNTRY), size=1) +
  geom_point(size=2.5)+
  labs(x = "År" , y ="Minstelønn i prosent %")  + 
  theme(legend.position="none")+
  geom_label_repel(data=complete_minwage_unempl %>% 
                     group_by(COUNTRY) %>% 
                     filter(Time==min(Time)), 
                   aes(Time, MinWage, 
                       fill = factor(COUNTRY), 
                       label = sprintf('%s', COUNTRY)), 
                   color = "black", 
                   fill = "white") 
minwage_plot2

# Denne grafen viser sammenhengen mellom minstelønn som prosent av 
# medianlønn og år for de samme landene som over. Nå er det lettere og se 
# USA`s konstante nedgang i minstelønn fra år 2010 - 2018. Frankrike
# fortsatt sterk i toppen sammen med new-zealand.

minwage_plot3 <- ggplot(data=complete_minwage_unempl,
                        aes(x=Time,y=UnEmpl, # Endrer x-aksen til time (år)
                            group=COUNTRY, color=COUNTRY)) +
  geom_line(aes(group=COUNTRY), size=1) +
  geom_point(size=2.5)+
  labs(x = "År" , y ="Arbeidsledige i prosent %")  + 
  theme(legend.position="none")+
  geom_label_repel(data=complete_minwage_unempl %>% 
                     group_by(COUNTRY) %>% 
                     filter(Time==min(Time)), 
                   aes(Time, UnEmpl, 
                       fill = factor(COUNTRY), 
                       label = sprintf('%s', COUNTRY)), 
                   color = "black", 
                   fill = "white") 
minwage_plot3

# Denne grafen viser sammenhengen mellom arbeidsledighet og år for de samme
# landene som over. Det vi kan se nå er at grafen "beveger" seg mer og har 
# tydeligere opp og nedganger. I det første plottet vi så på (minwage_plot1) 
# så det ut som USA kom dårligst ut på arbeidslediget, men nå ligger de på 
# gjennomsnittet.

# Kort oppsummert så ser vi at det er lettere og "tyde" grafer hvis vi får 
# oppgitt hvilket år observasjonene er tatt. 



# Utfordring 2
# Oppgave 1

# Store deler av livet går ut på å finne en utdanning som passer best for 
# seg selv. Veien man velger bestemmer hvilket yrke man ønsker å jobbe 
# innenfor og det burde helst være noe man trives med. Men hadde det vært 
# like lett å utdanne seg til det man ønsker om utdanningen ikke var gratis?

# Norge hadde nok ikke vært like populært å bo i hadde det ikke vært for 
# tilbudet om gratis grunnskole og videregående opplæring. En av grunnene 
# til at utdanning er gratis er for at alle skal ha samme tilbud, 
# uansett bakgrunn. Det er også blitt forsket på om det er en klar 
# sammenheng mellom utdanning og økonomisk utvikling. 
# Ifølge utdanning.no har land  med en høyt utdannet befolkning høyere 
# gjennomsnittlig BNP (Thomassen 2013: 1).

# Norge er i topp når det kommer til sysselsetting i verden, etter irland.
# Med sysselsetting mener vi mennesker i yrkesaktiv alder i arbeid. 

# Utdanning er ikke billig, så den første ulempene med offentlig 
# finnansiert utdanning er økt skatt, siden det må finansieres på et vis. 
# For det andre er vi veldig opptatt av at folk skal 
# tjene likt i Norge. Som fører til at enkelte velger å ta en "lettere" 
# utdanning for samme gevinsten.
# En ulempe som mange sikkert har opplevd når klasserommene fylles til 
# randen er at lærerne kanskje ikke kan bry seg nok om hver elev eller 
# bruke nok tid med hver enkelt. Det er ikke tid til å lære styrker og s
# vakheter til hver elev. Så kun de flinkeste og raskeste i klassen får 
# en fordel. 

# Kort oppsummert så har vi funnet ut at gratis utdanning fører til høyere 
# BNP og økonmisk utvikling. 
# Så selv om vi har offentlig finnansiert utdanning og individene selv 
# tjener på sine investeringer i humankapital
# (Våres kunnskaper og ferdigheter, som forbedres gjennom utdanning og 
# opplæring). Så vil alle tjene på det etter en stund. Vi ville aldri 
# hatt offentlig finnansiert utdanning om det ikke var lønnsomt i det lange 
# løp.


# Kilder:
# https://www.utdanningsforbundet.no/var-politikk/publikasjoner/2013/utdanning-og-utvikling-temanotat-32013/

