
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)

########################################################################################################################

clientes<-read_rds("C:/Users/ilan2/Documents/proyectos/ITAM/Analisis_de_supervivencia/Data/customers.rds")

clientes_en_mora<-clientes %>% 
  select(inegi_id, trnsx_id, fecha, fecha_de_vencimiento, dias_en_mora) %>% 
  filter(fecha == as.Date("2020-03-15")) %>% 
  filter(dias_en_mora>=1) %>% 
  pull(inegi_id) %>% 
  unique

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
  mutate(en_mora = ifelse(inegi_id %in% clientes_en_mora, 1, 0) ) %>% 
  mutate(Abandono = ifelse(en_mora==1, 0, Abandono) ) %>%
  select(-diffAbandono)

########################################################################################################################

p1<-transacciones %>% 
  # count(DiasObsrvdos, en_mora, name="freq") %>% 
  # ggplot(aes(x=DiasObsrvdos, y=freq)) +
  # geom_bar() +
  ggplot(aes(x=DiasObsrvdos)) +
  geom_histogram(bins=40) +
  facet_grid(Abandono~en_mora) +
  theme_bw()
p1
