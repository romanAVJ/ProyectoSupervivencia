
library(readr)
library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(purrr)
library(sf)
library(compareBars)
library(tidyr)
library(survival)
library(survminer)
library(plotly)
library(stringr)
library(rebus)


# devtools::install_github("daranzolin/compareBars")

options(scipen=999)

########################################################################################################################

# ruta donde estan guardados los datos del link de --Pobreza Urbana en México, 2015 (AGEB urbana) - CONEVAL--
# archivo "Base de datos de pobreza AGEB segun entidad federativa 2015.xlsx"
coneval_data<-"C:/Users/ilan2/Documents/proyectos/ITAM/Analisis_de_supervivencia/Base_de_datos_de_pobreza_AGEB_segun_entidad_federativa_2015/Base de datos de pobreza AGEB segun entidad federativa 2015.xlsx"

entidades<-coneval_data %>% 
  excel_sheets

pobrezaurbana<-entidades[entidades!="Índice"] %>% 
  set_names(entidades[entidades!="Índice"] %>% 
              str_to_lower %>% 
              str_replace_all(" ", "_") %>% 
              str_replace_all("á", "a") %>% 
              str_replace_all("é", "e") %>% 
              str_replace_all("í", "i") %>% 
              str_replace_all("ó", "o") %>% 
              str_replace_all("ú", "u")) %>% 
  map_df(~read_excel(coneval_data, sheet = .x, skip = 4) %>% 
        rename_all(list(~str_to_lower(.) %>% 
                          str_replace_all(" ", "SPACE") %>% 
                          str_replace_all("[^A-Za-z0-9]", "") %>% 
                          str_replace_all("SPACE", "_"))
        ) %>% 
        filter(!str_detect(clave_de_entidad, "[:alpha:]"  )), .id = "nom_ent") %>% 
  select(NOM_ENT = nom_ent,
         CVE_ENT    = clave_de_entidad,
         #nom_mpio   = municipio,
         CVEGEO     = clave_de_ageb,
         r_pobreza  = rango_de_pobreza_) %>% 
  mutate(r_pobreza  = str_c("R_",r_pobreza) %>% 
                      str_replace_all("\\(|\\[|\\]","") %>% 
                      str_replace_all(" |,", "_") %>% 
                      str_replace_all(one_or_more("_"),"_") %>% 
                      str_replace_all("R_No_disponible", "NoDisponible")%>% 
                      str_replace_all("R_Sin_viviendas_particulares_habitadas", "SinVivHabitadas")%>% 
                      str_replace_all("R_Una_vivienda_particular_habitada", "UnaVivHabitada"))

# ruta donde estan guardados los datos del link de --Marco Geoestadistico Integrado 2019 - INEGI--
# archivo "00mun.shp"
municipios<-"C:/Users/ilan2/Documents/proyectos/mg_sep2019_integrado/mg_sep2019_integrado/conjunto_de_datos/00mun.shp" %>% 
  st_read(stringsAsFactors=FALSE,
          options = "ENCODING=latin1") %>% 
  st_drop_geometry %>% 
  select(-CVEGEO) %>% 
  set_names(c("CVE_ENT","CVE_MUN","NOM_MUN")) %>% 
  #pegamos nombres de las entidades
  left_join(pobrezaurbana %>% select(CVE_ENT, NOM_ENT) %>% distinct, by = "CVE_ENT")

# ruta donde estan guardados los datos del link de --Marco Geoestadistico Integrado 2019 - INEGI--
# archivo "00a.shp"
agebs<-"C:/Users/ilan2/Documents/proyectos/mg_sep2019_integrado/mg_sep2019_integrado/conjunto_de_datos/00a.shp" %>% 
  st_read(stringsAsFactors=FALSE) %>% 
  st_transform(4326) %>% 
  select(-CVE_LOC, -CVE_AGEB, -Shape_Leng, -Shape_Area, -OBJECTID) %>% 
  left_join(municipios, by=c("CVE_ENT", "CVE_MUN")) %>% 
  mutate(ENTMUN = str_c(CVE_ENT, CVE_MUN)) %>% 
  left_join(pobrezaurbana %>% select(-CVE_ENT, -NOM_ENT), by="CVEGEO") %>% 
  # Ponemos no disponible en todas lo que no tenga rango de pobreza
  mutate(r_pobreza = ifelse(is.na(r_pobreza),"NoDisponible",r_pobreza))

rm(municipios, pobrezaurbana, coneval_data, entidades)

########################################################################################################################

# st_crs(agebs)

########################################################################################################################

# ruta donde esta guardado el archivo del link --Customers Data--
# archivo "customers.rds"
clientes<-read_rds("C:/Users/ilan2/Documents/proyectos/ITAM/Analisis_de_supervivencia/Data/customers.rds")

clientes_en_mora<-clientes %>% 
  select(inegi_id, trnsx_id, fecha, fecha_de_vencimiento, dias_en_mora) %>% 
  filter(fecha == as.Date("2020-03-15")) %>% 
  filter(dias_en_mora>=1) %>% 
  pull(inegi_id) %>% 
  unique

localizacion<-clientes %>% 
  select(inegi_id, latitude, longitude) %>% 
  distinct

transacciones<-clientes %>% 
  select(inegi_id, trnsx_id, monto_usado, fecha, fecha_afiliacion) %>% 
  filter(monto_usado>0) %>% 
  group_by(inegi_id) %>% 
  summarise(NumTransx = n(),
            fecha_afiliacion = max(fecha_afiliacion),
            PrimrTrnsx = min(fecha),
            UltmTrnsx = max(fecha)) %>% 
  ungroup %>% 
  mutate(DiasObsrvdos = UltmTrnsx - PrimrTrnsx +1) %>% 
  mutate(diffAbandono = as.Date("2020-03-15") - UltmTrnsx) %>% 
  mutate(Abandono = ifelse(diffAbandono >= 90, 1, 0) ) %>% 
  mutate(en_mora = ifelse(inegi_id %in% clientes_en_mora, 0, 1) ) %>% 
  select(-diffAbandono) %>% 
  left_join(localizacion, by="inegi_id") %>% 
  filter(latitude>0) %>% # Restriccion cuentas solo en MX
  mutate(latitude = na_if(latitude, "NULL") %>% na_if(0),
         longitude = na_if(longitude, "NULL") %>% na_if(0)) %>%
  filter(latitude>0) %>% # Restriccion cuentas solo en MX
  filter(!(is.na(latitude))) %>%
  filter(!(is.na(longitude))) %>% 
  filter(latitude!=0) %>% 
  filter(longitude!=0) %>% 
  mutate(lat = latitude,
         lon = longitude) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_join(agebs) %>% 
  st_drop_geometry %>% 
  mutate(indx_dummie = 1) %>% 
  spread(r_pobreza, indx_dummie, fill = "0")

# validar cruce
# transacciones %>% glimpse
# transacciones$CVEGEO %>% is.na %>% sum

rm(localizacion, clientes, clientes_en_mora, agebs)

########################################################################################################################

transacciones %>% names

mpios_robustos<-transacciones %>% 
  group_by(ENTMUN, NOM_MUN) %>% 
  summarise(Freq = n(),
            FecMin = min(PrimrTrnsx),
            FecMax = max(PrimrTrnsx),
            FecMedian = median(PrimrTrnsx),
            MaxDiasObs = max(DiasObsrvdos)) %>% 
  ungroup %>% 
  filter(MaxDiasObs>=300) %>% 
  filter(Freq >= 120)

# quantile(conteos_mpios$Freq, probs = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.85, 0.9))

#########################################################################################################################

Conteos<-transacciones %>% 
  filter(ENTMUN %in% mpios_robustos$ENTMUN) %>% 
  count(ENTMUN, NOM_MUN, Abandono, name="freq") %>% 
  group_by(ENTMUN) %>% 
  mutate(porcentaje = round(freq/sum(freq),2) ) %>% 
  ungroup

p1<-Conteos %>% 
  ggplot(aes(x=factor(Abandono), y=porcentaje, fill=factor(Abandono) )) +
  geom_col()+
  facet_wrap(~NOM_MUN)
p1

p2<-Conteos %>% 
  select(-freq) %>% 
  spread(Abandono, porcentaje) %>% 
  # mutate(NOMGEO = factor(NOMGEO)) %>% 
  set_names(c("ENTMUN","NOM_MUN","stay","leave")) %>% 
  arrange(stay) %>% 
  compareBars(NOM_MUN, 
              stay, 
              leave,
              xLabel = "Porcentaje",
              yLabel = "Municipio",
              titleLabel = "Abandono",
              subtitleLabel = "2016-2020",
              fontFamily = "Arial",
              # compareVarFill1 = "pink",
              # compareVarFill2 = "green",
              orientation = "horizontal")
p2

########################################################################################################################

Abandonos<-transacciones %>% 
  filter(Abandono==1) 

Abandonos %>% names

conteo_nas<-Abandonos %>% 
  count(Ambito, R_No_disponible, name="Freq")


########################################################################################################################
# Kaplan Meier
km_wrong <- survfit(Surv(DiasObsrvdos) ~ 1, data = Abandonos)
# Estimate the survivor function from this dataset via kaplan-meier.
km <- survfit(Surv(DiasObsrvdos, en_mora) ~ 1, data = Abandonos)
# Plot the two and compare
ggsurvplot_combine(list(correct = km, wrong = km_wrong))
########################################################################################################################

# Weibull model
wb <- survreg(Surv(DiasObsrvdos, en_mora) ~ 1, data = Abandonos)
# 60% de los clientes abandonan antes de el día:
predict(wb, type = "quantile", p = 1 - 0.4, newdata = data.frame(1))

# Retrieve survival curve from model probabilities 
surv <- seq(.99, .01, by = -.01)
# Get time for each probability
t <- predict(wb, type = "quantile", p = 1 - surv, newdata = data.frame(1))
# Create data frame with the information
surv_wb <- data.frame(time = t, surv = surv, upper = NA, lower = NA, std.err = NA)
# Plot
ggsurvplot_df(fit = surv_wb, surv.geom = geom_line)


########################################################################################################################

Abandonos %>% names

# Weibull model
wbmod_1 <- survreg(Surv(DiasObsrvdos, en_mora) ~ Ambito, data = Abandonos)
coef(wbmod_1)

wbmod_2 <- survreg(Surv(DiasObsrvdos, en_mora) ~ NumTransx, data = Abandonos)
coef(wbmod_2)

########################################################################################################################

# Retrieve survival curve from model
# surv_seq <- seq(.99, .01, by = -.01)

# t_urbana <- predict(wbmod_1,
#                     type = "quantile", 
#                     p = 1 - surv_seq,
#                     newdata = data.frame(Ambito = "Urbana"))
# 
# t_rural <- predict(wbmod_1,
#                    type = "quantile", 
#                    p = 1 - surv_seq,
#                    newdata = data.frame(Ambito = "Rural"))

########################################################################################################################

# Compute Cox models

cxmod_1 <- coxph(Surv(DiasObsrvdos, en_mora) ~ Ambito, data = Abandonos)
coef(cxmod_1)

cxmod_2 <- coxph(Surv(DiasObsrvdos, en_mora) ~ NumTransx, data = Abandonos)
coef(cxmod_2)


###########################################################################################################################

# Cox model
cxmod <- coxph(
  Surv(DiasObsrvdos, en_mora) ~ Ambito + NumTransx + R_0_18 + R_18_34 + R_34_50 + R_50_70 + R_70_100 + SinVivHabitadas + UnaVivHabitada, 
  data = Abandonos)
coef(cxmod)

# Cox model
cxmod <- coxph(Surv(DiasObsrvdos, en_mora) ~ Ambito + NumTransx, data = Abandonos)
coef(cxmod)


DatosParaMostrar<-data.frame(Ambito = c("Urbana","Rural", "Urbana","Rural") %>% factor,
                            NumTransx = c(1, 1, 10,10)
                            # R_0_18 = c(1, 1) %>% factor,
                            # R_18_34 = c(1, 1) %>% factor,
                            # R_34_50 = c(1, 1) %>% factor,
                            # R_50_70 = c(1, 1) %>% factor,
                            # R_70_100 = c(1, 1) %>% factor,
                            # SinVivHabitadas = c(1, 1) %>% factor,
                            # UnaVivHabitada = c(1, 1) %>% factor
                            )
DatosParaMostrar %>% glimpse

# Imaginary patients
# newdat <- expand.grid(
#   Ambito = levels(Abandonos$Ambito),
#   NumTransx = quantile(Abandonos$NumTransx, probs = c(0.25, 0.5, 0.75)))
# rownames(newdat) <- letters[1:6]

# Inspect newdat
# newdat

# Compute survival curves
cxsf <- survfit(cxmod, 
                data = Abandonos, 
                newdata = DatosParaMostrar, 
                conf.type = "none")
head(cxsf$surv)

# Compute survival curves
#cxsf <- survfit(cxmod, data = Abandonos, newdata = newdat, conf.type = "none")

str(cxsf)

# Look at first 6 rows of cxsf$surv and time points
head(cxsf$surv)
head(cxsf$time)

# Compute data.frame needed for plotting
surv_cxmod0 <- surv_summary(cxsf)

# Look at the first few lines
head(surv_cxmod0)

# Get a character vector of patient letters (patient IDs)
pid <- as.character(surv_cxmod0$strata)

# Multiple of the rows in newdat so that it fits with surv_cxmod0
m_newdat <- DatosParaMostrar[pid, ]

# Add patient info to data.frame
surv_cxmod <- cbind(surv_cxmod0, m_newdat)
head(surv_cxmod)


# Plot
pSurvival<-ggsurvplot_df(surv_cxmod, 
              linetype = "Ambito", 
              color = "NumTransx", 
              legend.title = NULL, 
              censor = FALSE)

ggplotly(pSurvival)
