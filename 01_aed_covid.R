

# -------------------------------------------------------------------------
# URL Descarga
# https://www.datos.gov.co/Salud-y-Protecci-n-Social/Casos-positivos-de-COVID-19-en-Colombia/gt2j-8ykr/data
# -------------------------------------------------------------------------

# Cargar librerias --------------------------------------------------------
library(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, RColorBrewer)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Cargar tabla descargada del INS -----------------------------------------
tbl <- read_csv('./tbl/Casos_positivos_de_COVID-19_en_Colombia.csv')
shp <- st_read('./shapefiles/MGN_MPIO_POLITICO.shp')
dpt <- st_read('./shapefiles/MGN_DPTO_POLITICO.shp')
shp <- shp %>% mutate(MPIO_CCNCT = as.numeric(MPIO_CCNCT))

# Seleccion de columnas de interes
tbl <- tbl[,1:8]
names(tbl) <- c('id', 'fecha', 'codigo', 'ciudad', 'dpto', 'atencion', 'edad', 'sexo')

# Grafico de cantidad de recuperados versus no recuperados ----------------
unique(tbl$atencion)
gg_1 <- tbl %>% 
  group_by(atencion) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  arrange(desc(count)) %>%
  mutate(atencion = factor(atencion, levels = atencion)) %>% 
  ggplot(data = ., aes(x = atencion, y = count)) +
  geom_col() +
  ggtitle(label = 'Estado de las personas contagiadas por COVID - 19 en Colombia') +
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = 'bold', hjust = 0.5),
        axis.text = element_text(size = 12)) +
  labs(x = '',
       y = 'Cantidad personas',
       caption = 'Adaptado del INS - Agosto 22 2020') +
  geom_text(aes(label = format(count, big.mark = "\\.", scientific = FALSE)), position=position_dodge(width=0.9), vjust = -0.25) +
  scale_y_continuous(labels = function(count) format(count, big.mark = "\\.", scientific = FALSE))

ggsave(plot = gg_1, filename = './png/graphics/estado_personas.png', units = 'in', width = 10, height = 7, dpi = 300)

# Organizando los datos para el mapa --------------------------------------
smm <- tbl %>% 
  group_by(codigo, atencion) %>% 
  dplyr::summarise(count = n()) %>% 
  ungroup() %>% 
  spread(atencion, count) %>% 
  setNames(c('codigo', 'casa', 'fallecido', 'hospital', 'uci', 'na', 'recuperado'))
smm$norecuperado <- smm$casa + smm$hospital + smm$uci
smm <- smm %>% dplyr::select(codigo, recuperado, norecuperado, na)
smm[is.na(smm)] <- 0
smm$codigo <- as.numeric(smm$codigo)
smm <- smm %>% 
  gather(var, value, -codigo)
smm <- smm %>% 
  group_by(codigo) %>% 
  mutate(porc = value / sum(value, na.rm = T) * 100) %>% 
  ungroup() 
smm$porc <- ifelse(is.nan(smm$porc), 0, smm$porc)

smm_value <- smm %>% dplyr::select(-porc) %>% spread(var, value)
smm_value <- smm_value %>% setNames(c('codigo', 'val_na', 'val_nr', 'val_r'))
smm_porcn <- smm %>% dplyr::select(-value) %>% spread(var, porc)
smm_porcn <- smm_porcn %>% setNames(c('codigo', 'prc_na', 'prc_nr', 'prc_r'))
smm <- inner_join(smm_value, smm_porcn, by = 'codigo')
shp <- inner_join(shp, smm, by = c('MPIO_CCNCT' = 'codigo'))

# Haciendo el mapa de recuperados valores en porcentaje
g1 <- ggplot() +
  geom_sf(data = shp, aes(fill = prc_r)) +
  geom_sf(data = dpt, fill = NA, size = 1.1) +
  scale_fill_gradientn(name = 'Porcentaje personas\n(%)',
                       colours = RColorBrewer::brewer.pal(n = 8, name = 'RdYlGn'),
                       na.value = 'white') +
  ggtitle(label = 'Porcentaje de personas recuperadas\nde COVID-19 en Colombia') +
  coord_sf() +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = 'bold'),
        legend.position = 'bottom',
        legend.key.width = unit(5, 'line')) +
  labs(caption = 'Adaptado del INS - Agosto 22 2020')

ggsave(plot = g1, filename = './png/maps/porcentaje_recuperadas.png',
       units = 'in', width = 10, height = 15, dpi = 300)

summary(smm$prc_r)

prc_10 <- smm %>% filter(prc_r < quantile(smm$prc_r, 0.1))

dpts <- inner_join(prc_10, shp %>% as.data.frame() %>% dplyr::select(MPIO_CCNCT, MPIO_CNMBR, DPTO_CNMBR), by = c('codigo' = 'MPIO_CCNCT'))
dpts %>% group_by(DPTO_CNMBR) %>% summarise(count = n()) %>% ungroup() %>% arrange(desc(count))
