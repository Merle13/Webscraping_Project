GKV1 = read.csv("German_HealthInsurance.csv", stringsAsFactors = FALSE)

library(dplyr)

# 
# Regions = GKV1 %>% select(., GKVname, Baden_Württemberg, Bayern, Berlin, Brandenburg, 
#                           Bremen, Hamburg, Hessen, MVP, Niedersachsen, NRW, 
#                           Rheinland_Pfalz, Saarland, Sachsen, Sachsen_Anhalt, 
#                           Schleswig_Holtstein, Thüringen)
# 
# Abroad = GKV1 %>% select(., EmerencyService_Abroad, Travel_vacc)
# 
# Basics = GKV1 %>% select(., Doc_search, hospital_search, medical_phoneInfo, online_branch, 
#                          reciept_online, reha_search, service_hotline, appointment_help, 
#                          careReminderService)
# 
# Bonus = GKV1 %>% select(., BMI, yearly_dentist, sport_ensigh, gym, sport_club,
#                         non_smoker, teeth_clean, Health_checkUp, skinCancer,Cancer_screening,
#                         prevention, Child_UG, pharmacy, Generika, adjuvant)
# 
# Naturpathy = GKV1 %>% select(., alter_cancerTherapy, alter_cancerMed, 
#                              AnthroposophyMedTherapy, AnthroposophyMedMed,
#                              Ayurveda, Chelattherapie, Autohaemotherapy,
#                              Feldenkrais, HomoeopathyTherapy, HomoeopathyMed,
#                              Irisdiagnostic, Lighttherapy, Osteopathy, Physiotherapy,
#                              Reflexmassage, Shiatsu, TCM)
GKV1 = GKV1 %>% rename(., Reflexmassage = Replexmassage)


#Name_Rate = GKV1 %>% select(.,GKVname, Premium_rate)


#Name_Rate$Premium_rate = gsub("[[:space:]]", "", Name_Rate$Premium_rate)
#Name_Rate$Premium_rate = gsub("%", "", Name_Rate$Premium_rate)


#as.numeric(as.matrix(Name_Rate$Premium_rate)[1,])
#  
# Name_Rate$Premium_rate = as.numeric(gsub(",", ".", Name_Rate$Premium_rate))
# Name_Rate$Premium_rate = Name_Rate$Premium_rate / 100



# 
# Name_Rate %>% summarise(., mean(Premium_rate))
# Name_Rate %>% summarise(., max(Premium_rate), 
#                         mean(Premium_rate), 
#                         min(Premium_rate))

# only the columns selected that I will work with for this project 
workGKV = GKV1 %>% select(., Name = GKVname, Premium_rate, EmerencyService_Abroad, BMI, Ayurveda, Cancer_screening,
                          Check_up = Health_checkUp, HomoeopathyMed, HomoeopathyTherapy, Osteopathy, service_hotline,
                          Reflexmassage, TCM, alter_cancerMed, alter_cancerTherapy, gym, non_smoker,
                          skinCancer, sport_club, yearly_dentist, Baden_Württemberg, Bayern, Berlin, Brandenburg, 
                          Bremen, Hamburg, Hessen, Mecklenburg_Vorpommern = MVP, Niedersachsen, Nordrhein_Westfalen = NRW,
                          Rheinland_Pfalz, Saarland, Sachsen, Sachsen_Anhalt, Schleswig_Holtstein, Thüringen)

#convers German values "ja/nein" into TRUE FALSE
ValueConverter = function(x) {
  if(x == "ja") {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#applied the ValueConverter on the selected columns
workGKV = workGKV %>% mutate(., EmerencyService_Abroad = sapply(EmerencyService_Abroad, ValueConverter)) %>%
  mutate(., BMI = sapply(BMI, ValueConverter)) %>%
  mutate(., Cancer_screening = sapply(Cancer_screening, ValueConverter)) %>%
  mutate(., Check_up = sapply(Check_up, ValueConverter)) %>%
  mutate(., gym = sapply(gym, ValueConverter)) %>%
  mutate(., non_smoker = sapply(non_smoker, ValueConverter)) %>%
  mutate(., skinCancer = sapply(skinCancer, ValueConverter)) %>%
  mutate(., sport_club = sapply(sport_club, ValueConverter)) %>%
  mutate(., yearly_dentist = sapply(yearly_dentist, ValueConverter))

# adding a column with the percentate as a number to do calculations with later on
workGKV = workGKV %>% mutate(., Rate = Premium_rate)
workGKV$Rate = gsub("[[:space:]]", "", workGKV$Rate)
workGKV$Rate = gsub("%", "", workGKV$Rate)
workGKV$Rate = as.numeric(gsub(",", ".", workGKV$Rate))
workGKV$Rate = workGKV$Rate / 100

# turning longer string values into True false  
workGKV$Ayurveda = ifelse(grepl("Ja", workGKV$Ayurveda), TRUE, FALSE)
workGKV$HomoeopathyMed = ifelse(grepl("Ja", workGKV$HomoeopathyMed), TRUE, FALSE)
workGKV$HomoeopathyTherapy = ifelse(grepl("Ja", workGKV$HomoeopathyTherapy), TRUE, FALSE)
workGKV$Osteopathy = ifelse(grepl("Ja", workGKV$Osteopathy), TRUE, FALSE)
workGKV$Reflexmassage = ifelse(grepl("Ja", workGKV$Reflexmassage), TRUE, FALSE)
workGKV$TCM = ifelse(grepl("Ja", workGKV$TCM), TRUE, FALSE)  
workGKV$alter_cancerMed = ifelse(grepl("Ja", workGKV$alter_cancerMed), TRUE, FALSE)
workGKV$alter_cancerTherapy = ifelse(grepl("Ja", workGKV$alter_cancerTherapy), TRUE, FALSE)


saveRDS(workGKV, "workGKV.RDS")


# change row values accroding to income input 
workGKV %>% mutate(., Rate = (Rate - 0.073) * input$income)


#boxplot
library(ggplot2)
raten = ggplot(data = workGKV, aes(x = Rate)) + geom_bar(aes(fill = Rate))
raten
workGKV$Name


workGKV$member = c(27179, 1063700, 22345, 130000, 541894, 49709, 97200,
                   89540, 23833, 33000, 25883, 33086, 119789, 120019, 
                   23622, 500000, 716124, 225000, 10200, 1800000, 5800000,
                   654350, 3300000, 577100, 180000, 2871836, 72625, 410216,
                   230005, 498000, 14504, 1045117, 326221, 121312, 31616,
                   58165, 1633784, 68772, 4288, 669145, 179006, 58850, 100566,
                   4499138, 9351721, 1629695, 1200000, 412162, 4267552,
                   27011, 172886, 29700, 158400,295145, 2843547, 1758787, 
                   220292, 9937314, 240701, 248411, 2620833, 14034, 53538,
                   658896, 84907, 23054, 70624, 3167707, 88920, 431913, 
                   73072, 555779, 54000, 46926, 760565)

#scatter plot für alle KKs Mitglieder xachse/ Beitrag yachse

scatter = ggplot(data = workGKV, aes(x = member, y = Rate)) + geom_point()
scatter

#nur die KKs die bundesweit agieren
bundesweit = workGKV %>% filter(., Baden_Württemberg == "True", Bayern == "True",
                                Berlin == "True", Brandenburg == "True",
                                Bremen == "True", Hamburg == "True",
                                Hessen == "True", Mecklenburg_Vorpommern == "True",
                                Niedersachsen == "True", Nordrhein_Westfalen == "True",
                                Rheinland_Pfalz == "True", Saarland == "True",
                                Sachsen == "True", Sachsen_Anhalt == "True",
                                Schleswig_Holtstein == "True", Thüringen == "True")

# mean max und min der Beiträge für 
mean(bundesweit$Rate)
mean(workGKV$Rate)
max(workGKV$Rate)
max(bundesweit$Rate)
min(bundesweit$Rate)

bundesscatter = ggplot(data = bundesweit, aes(x = member, y = Rate)) + geom_point()
bundesscatter



# functions to convert True to the conuty name to then rbind to new data frame called Region
badenWü = function(x) {
  if(x == "True") {
    return("Baden Württemberg")
  } else {
    return(FALSE)
  }
}

bay = function(x) {
  if(x == "True") {
    return("Bayern")
  } else {
    return(FALSE)
  }
}

ber = function(x) {
  if(x == "True") {
    return("Berlin")
  } else {
    return(FALSE)
  }
}

brand = function(x) {
  if(x == "True") {
    return("Brandenburg")
  } else {
    return(FALSE)
  }
}
brem = function(x) {
  if(x == "True") {
    return("Bremen")
  } else {
    return(FALSE)
  }
}

ham = function(x) {
  if(x == "True") {
    return("Hamburg")
  } else {
    return(FALSE)
  }
}

hes = function(x) {
  if(x == "True") {
    return("Hessen")
  } else {
    return(FALSE)
  }
}

meckP = function(x) {
  if(x == "True") {
    return("Mecklenburg Vorpommern")
  } else {
    return(FALSE)
  }
}

nieder = function(x) {
  if(x == "True") {
    return("Niedersachsen")
  } else {
    return(FALSE)
  }
}

nrw = function(x) {
  if(x == "True") {
    return("Nordrhein-Westfalen")
  } else {
    return(FALSE)
  }
}

rhP = function(x) {
  if(x == "True") {
    return("Rheinland-Pfalz")
  } else {
    return(FALSE)
  }
}

sar = function(x) {
  if(x == "True") {
    return("Saarland")
  } else {
    return(FALSE)
  }
}

sax = function(x) {
  if(x == "True") {
    return("Sachsen")
  } else {
    return(FALSE)
  }
}

saxan = function(x) {
  if(x == "True") {
    return("Sachsen-Anhalt")
  } else {
    return(FALSE)
  }
}

sh = function(x) {
  if(x == "True") {
    return("Schleswig-Holstein")
  } else {
    return(FALSE)
  }
}

tür = function(x) {
  if(x == "True") {
    return("Thüringen")
  } else {
    return(FALSE)
  }
}

#extracting each region (16)

BaWü = workGKV %>% select(Baden_Württemberg, Rate) %>%
  filter(., Baden_Württemberg == "True") %>%
  mutate(., Baden_Württemberg = sapply(Baden_Württemberg, badenWü)) %>%
  rename(., Region = Baden_Württemberg)

Bay = workGKV %>% select(Bayern, Rate) %>%
  filter(., Bayern == "True") %>%
  mutate(., Bayern = sapply(Bayern, bay)) %>%
  rename(., Region = Bayern)

Ber = workGKV %>% select(Berlin, Rate) %>%
  filter(., Berlin == "True") %>%
  mutate(., Berlin = sapply(Berlin, ber)) %>%
  rename(., Region = Berlin)

BrandB = workGKV %>% select(Brandenburg, Rate) %>%
  filter(., Brandenburg == "True") %>%
  mutate(., Brandenburg = sapply(Brandenburg, brand)) %>%
  rename(., Region = Brandenburg)

Brem = workGKV %>% select(Bremen, Rate) %>%
  filter(., Bremen == "True") %>%
  mutate(., Bremen = sapply(Bremen, brem)) %>%
  rename(., Region = Bremen)

HH = workGKV %>% select(Hamburg, Rate) %>%
  filter(., Hamburg == "True") %>%
  mutate(., Hamburg = sapply(Hamburg, ham)) %>%
  rename(., Region = Hamburg)

Hesse = workGKV %>% select(Hessen, Rate) %>%
  filter(., Hessen == "True") %>%
  mutate(., Hessen = sapply(Hessen, hes)) %>%
  rename(., Region = Hessen)

MVP = workGKV %>% select(Mecklenburg_Vorpommern, Rate) %>%
  filter(., Mecklenburg_Vorpommern == "True") %>%
  mutate(., Mecklenburg_Vorpommern = sapply(Mecklenburg_Vorpommern, meckP)) %>%
  rename(., Region = Mecklenburg_Vorpommern)

Nieder = workGKV %>% select(Niedersachsen, Rate) %>%
  filter(., Niedersachsen == "True") %>%
  mutate(., Niedersachsen = sapply(Niedersachsen, nieder)) %>%
  rename(., Region = Niedersachsen)

NRW = workGKV %>% select(Nordrhein_Westfalen, Rate) %>%
  filter(., Nordrhein_Westfalen == "True") %>%
  mutate(., Nordrhein_Westfalen = sapply(Nordrhein_Westfalen, nrw)) %>%
  rename(., Region = Nordrhein_Westfalen)
 
RP = workGKV %>% select(Rheinland_Pfalz, Rate) %>%
  filter(., Rheinland_Pfalz == "True") %>%
  mutate(., Rheinland_Pfalz = sapply(Rheinland_Pfalz, rhP)) %>%
  rename(., Region = Rheinland_Pfalz)

Saar = workGKV %>% select(Saarland, Rate) %>%
  filter(., Saarland == "True") %>%
  mutate(., Saarland = sapply(Saarland, sar)) %>%
  rename(., Region = Saarland)

Sachs = workGKV %>% select(Sachsen, Rate) %>%
  filter(., Sachsen == "True") %>%
  mutate(., Sachsen = sapply(Sachsen, sax)) %>%
  rename(., Region = Sachsen)

SachA = workGKV %>% select(Sachsen_Anhalt, Rate) %>%
  filter(., Sachsen_Anhalt == "True") %>%
  mutate(., Sachsen_Anhalt = sapply(Sachsen_Anhalt, saxan)) %>%
  rename(., Region = Sachsen_Anhalt)

SH = workGKV %>% select(Schleswig_Holtstein, Rate) %>%
  filter(., Schleswig_Holtstein == "True") %>%
  mutate(., Schleswig_Holtstein = sapply(Schleswig_Holtstein, sh)) %>%
  rename(., Region = Schleswig_Holtstein)

Thü = workGKV %>% select(Thüringen, Rate) %>%
  filter(., Thüringen == "True") %>%
  mutate(., Thüringen = sapply(Thüringen, tür)) %>%
  rename(., Region = Thüringen)

Regions = rbind(BaWü, Bay, Ber, BrandB, Brem, HH, Hesse, MVP, Nieder, NRW, RP, Saar, Sachs, SachA, SH, Thü)


#boxplot 

box_all = ggplot(data = Regions, 
                 aes(x =reorder(Region, Rate, median), 
                     y = Rate)) + 
  geom_boxplot()
box_all

Mean = round(mean(workGKV$Rate), 3)

OverUnder = function(x) {
  if(x < Mean) {
    return("under")
  } else if(x == Mean) {
    return("mean")
  } else {
    return("over")
  }
}
  
  
  
workGKV = workGKV %>% mutate(., overUnderMean = sapply(Rate, OverUnder))




# plotly box

#lyBox = plot_ly(ggplot2::Regions, y = ~Region, color = ~Region, type = "box")



# which insurance covers most

rowSums(workGKV)


Bonus = workGKV %>% select(., BMI, Cancer_screening, Check_up,
                           gym, non_smoker, skinCancer, yearly_dentist)
workGKV$BonusRows = rowSums(Bonus)

Natur = workGKV %>% select(., Ayurveda, HomoeopathyTherapy,
                           Osteopathy, Reflexmassage, TCM)
workGKV$Natur = rowSums(Natur)

colSums(Bonus)

colSums(Natur) 

workGKV %>% filter(., Ayurveda == TRUE)

min(workGKV$member)
workGKV %>% filter(., member == 4288)

max(workGKV$member)

max(workGKV$BonusRows)
workGKV %>% filter(., BonusRows == 7 & Natur == 5) %>% select(., Name)


min(workGKV$BonusRows)
min(workGKV$Natur)

workGKV %>% filter(., BonusRows == 0 & Natur == 0) %>% select(., Name)

max(workGKV$Rate)
min(workGKV$Rate)

workGKV %>% filter(., Rate == 0.163) %>% select(., Name)
workGKV %>% filter(., Rate == 0.146) %>% select(., Name)


#########################################################################################


library(plotly)
pal = c("green", "yellow", "red")
pal = setNames(pal, c("over", "mean", "under"))
lyScatter = plot_ly(data = workGKV, x = ~member, y = ~Rate,
                    text = ~paste("Insurance :", Name, "<br>Rate :", Premium_rate,
                                  "<br>Member Count :", member),
                    color = ~Rate, colors = pal)
lyScatter
