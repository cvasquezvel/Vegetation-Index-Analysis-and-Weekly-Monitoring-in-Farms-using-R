# Cambiar el directorio de trabajo ----

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# setwd("C:/Users/Dell/Documents/Danper/4. Capacitaciones")
getwd()

# CARGAR LIBRERIAS ------------------------------------------------

# devtools::install_github("rsbivand/sp@evolution")
# Sys.setenv("SP_EVOLUTION_STATUS"=2)

# remotes::install_version("rgeos", version = "0.6-4")

# remotes::install_version("rgeos")
# remotes::install_version("rgdal")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(gstat, raster, sf, tidyverse, #rgdal,
               sp, readxl, rgeos, terra,
               dplyr,
               paletteer, plotly, gganimate, magick, spatialEco)

# install.packages('BiocManager')

# library(gstat)
# library(raster)
# library(sf)
# library(tidyverse)
# library(rgdal)
# library(sp)
# library(readxl)
# library(rgeos)
# library(dplyr)
# library(paletteer)
# library(plotly)
# library(writexl)
# library(gganimate)
# library(magick)
# library(spatialEco)



# RUTA DE SALIDA ----------------------------------------------------------

SEM = data.frame(SEM=c(2#,3
                       # 5,7,8,9,10,11,12,14,16,18,19,20,21,23,24,26,25,26,
                       # 27,29,41,42,45,46,47,49
                       ))


AÑO = 2023
FUNDO = data.frame(FUNDO = c(#"VICTORIA", "AGROMORIN","SANTO DOMINGO"
                              # "CASA VERDE"
                             "COMPOSITAN"
                             ))

INDICES = NULL
raster_df_cons = NULL
for (f in FUNDO$FUNDO) {
  raster_df_cons0 = NULL
for (S in SEM$SEM) {

  RUTA = paste0("E:/Danper/Danper/13. GIS/Indices/INDICES VIGOR/",AÑO, 
                "/", "SEMANA ", 2)
  setwd(paste0(RUTA))
  LISTA = data.frame(raster = (list.files("1. RASTER/",full.names = F))) %>%
    dplyr::mutate(FUNDO = str_replace (raster, paste0("_SEM",S,".tif"), ""))
  
    f = f
    fun = LISTA %>% dplyr::filter(FUNDO == f) %>% dplyr::select("FUNDO")

    if (count(fun) == 0) {
      
      print(paste("NO HAY VUELO PARA", f, "EN LA SEMANA", S))
    }else{
      
      # CARGAR RASTER ------------------------------------------------
      setwd(paste0(RUTA,"/1. RASTER"))
      # EX1 <- stack(paste0(RUTA,"/1. RASTER/", f,"_SEM", S, ".tif"))
      EX1 <- stack("COMPOSITAN_SEM2.tif")
      plot(EX1)
      
      
      # CARGAR ARCHIVO VECTORIAL ------------------------------------------------
      
      fundo = read_sf("FUNDOS_DANPER.shp")
      # fundo = fundo %>% dplyr::filter(FUNDO %in% f )
      fundo = fundo %>% dplyr::filter(FUNDO %in% "COMPOSITAN" )

# REESTRUCTURAR EL FUNDO --------------------------------------------------

      clip = as_Spatial(fundo)

      clip1 <- gUnaryUnion(clip, id = clip@data$LOTE)
      
      row.names(clip1) <- as.character(1:length(clip1))
      
      LOTE <- (unique(clip$LOTE))
      clip_LOTE = as.data.frame(clip)
      LOTE <- as.data.frame(LOTE) %>% left_join(clip_LOTE, 
                                                c("LOTE")) %>% 
        dplyr::select("MODULO", "TURNO",  "LOTE", "VARIEDAD")
      
      clip1 <- SpatialPolygonsDataFrame(clip, LOTE)
      
      plot(clip1)
      

      EX1 =  raster::mask(EX1, fundo)
      names(EX1)<- c("blue", "green", "red","nir")
      nir <- EX1$nir
      red <- EX1$red
      green <- EX1$green
      blue <- EX1$blue
      
      # VHI ----
      # El Índice de Humedad de Vegetación (VHI) se calcula a partir de los valores de la banda NDVI y la banda de humedad de los satélites meteorológicos NOAA. La ecuación para calcular el VHI es la siguiente:
      # VHI <- (NDVI * Humedad) / 100
      # Donde NDVI es el valor de NDVI para una fecha dada y Humedad es el valor de humedad para una fecha dada.
      
      SI1= sqrt(green * red)
      plot(SI1, main = "SI1")
      # 
      SI2 = sqrt(green^2+red^2 +nir^2)
      plot(SI2, main = "SI2")
      # 
      SI3 = sqrt(green^2+red^2)
      plot(SI3, main = "SI3")
      # 
      INT1 = (green + red)/2
      plot(INT1, main = "INT1")
      # 
      INT2 = (green + red + nir)/2
      plot(INT2, main = "INT2")
      # 
      BI = sqrt(green+ nir^2)
      plot(BI, main = "BI")
      
      NDVI = (nir-red)/(nir+red)
      plot(NDVI, main = "NDVI")
      # 
      # b0 = -35.2
      # b1 = 0.7
      # LST = (b0 + b1 * NDVI) / (1 - b1)
      # plot(LST, main = "TEMPERATURA 1")
      # 
      # a0 = 29.7
      # a1 = 0.7
      # a2 = 0.4
      EVI =2.5 * (nir/10000 - red/10000) / (nir/10000 + 2.4* red /10000 + 1)
      # LST1 = a0 + a1 * NDVI + a2 * EVI
      # plot(LST1, main = "TEMPERATURA 2")
      # 
      # par(mfrow=c(1,2))
      # plot(LST, main = "TEMPERATURA 2")
      # plot(NDVI, main = "NDVI")
      # 
      # a0 = 17.33
      # a1 = 0.05
      # a2 = 0.99
      # LST1 = a0 + a1 * NDVI + a2 * (red/1000)
      # plot(LST1, main = "TEMPERATURA 1")
      
      
      # a0 = 17.34
      # a1 = 0.04
      # a2 = 0.99
      # 
      # LST2 = a0 + a1 * EVI + a2 * B1
      # plot(LST2, main = "TEMPERATURA 1")
      # 
      SAVI = ((nir-red)/(nir+red+0.5))*(1+0.5)
      # a0 = 20.33
      # a1 = 0.03
      # a2 = 0.99
      # LST2 = a0 + a1 * SAVI + a2 * red/1000
      # plot(LST2, main = "TEMPERATURA ")
      
      MSAVI = (2 * nir + 1 - sqrt ((2 * nir + 1)^2 - 8 * (nir  - red))) / 2
      GNDVI = (nir-green) /(nir+green)
      
      NDWI = (green - nir) / (green + nir)
      plot(NDWI, main ="NDWI ", sub= "Detección de tierras agrícolas inundadas;
     asignación de inundaciones en el campo; detección de tierras de cultivo de
     regadío; asignación de humedales.")
      
      # ET = -15.67 + 0.85 * NDVI + 0.03 * red
      # plot(ET, main = "ET")
      # 
      # ET = -13.07 + 0.82 * EVI + 0.03 * red
      # plot(ET, main = "ET")
      # 
      # ET = -12.47 + 0.80 * SAVI + 0.03 * red
      # plot(ET, main = "ET")
      # 
      # ET = -10.58 + 0.78 * NDVI + 0.04 * EVI + 0.02 * MSAVI + 0.03 * red
      # plot(ET, main = "ET")
      # 
      # ET = -11.08 + 0.80 * NDVI + 0.04 * EVI + 0.03 * GNDVI + 0.02 * red
      # plot(ET, main = "ET")
      # 
      # ET = -9.98 + 0.82 * NDVI + 0.03 * EVI + 0.03 * NDWI + 0.02 * B4
      # plot(ET, main = "ET")
      
      ET = (0.0003 * NDVI^3 + 0.0094 * NDVI^2 + 0.0401 * NDVI + 0.0147)*100
      plot(ET, main = "ET")
      
      # par(mfrow = c(2,2))
      
      # ET = 0.0005 * NDVI^3 + 0.0096 * NDVI^2 + 0.0461 * NDVI + 0.0126
      # plot(ET, main = "ET")
      # 
      # ET = 0.0004 * NDVI^3 + 0.0095 * NDVI^2 + 0.0412 * NDVI + 0.0004 * EVI^3 +
      #   0.0083 * EVI^2 + 0.0321 * EVI + 0.0003 * NDWI^3 + 0.0078 * NDWI^2 + 
      #   0.0301 * NDWI + 0.0124
      # plot(ET, main = "ET")
      # 
      # ET = 0.0007 * NDVI^3 + 0.0089 * NDVI^2 + 0.0572 * NDVI + 0.0109
      # plot(ET, main = "ET")
      
      # Índices de estrés hídricos
      
      # El Índice de Estrés Hídrico Vegetativo (VHI) se calcula utilizando la siguiente fórmula:
        VHI = (NDVI - NDWI) / (NDVI + NDWI)
        # SI = (NDVI - NDWI) / (NDVI + NDWI)
        plot(VHI, main = "VHI")
        ggplot(raster::as.data.frame(VHI, xy = T) %>% na.omit()) +
          geom_raster(data = , aes(x = x, y = y, fill = layer)) +
          paletteer::scale_fill_paletteer_c("ggthemes::Red-Green-Gold Diverging",
                                            breaks = seq(-60, 60, by = 10),
                                            limits = c(-60, 60),
                                            labels = as.character(seq(-60, 60, by = 10)))+
          theme_test()
        # El Índice de Estrés Hídrico Normalizado (NSI) se calcula utilizando la siguiente fórmula:
        NSI = (NDVI - NDWI) / (NDVI + NDWI)
        plot(NSI, main = "NSI")
        ggplot(raster::as.data.frame(NSI, xy = T) %>% na.omit()) +
          geom_raster(data = , aes(x = x, y = y, fill = layer)) +
          paletteer::scale_fill_paletteer_c("ggthemes::Red-Green-Gold Diverging",
                                            breaks = seq(-100, 100, by = 40),
                                            limits = c(-100, 100),
                                            labels = as.character(seq(-100, 100, by = 40)))+
          theme_test()
      # El Índice de Estrés Hídrico de Transmitancia (TSI) se calcula utilizando la siguiente fórmula:
        TSI = NDWI / NDVI
        plot(TSI, main = "TSI")
      # El Índice de Estrés Térmico Vegetativo (TVI) se calcula utilizando la siguiente fórmula:
        TVI = (LST - Tair) / (LST + Tair)
      
      # El Índice de Estrés Térmico Normalizado (NTI) se calcula utilizando la siguiente fórmula:
        NTI = (LST - Tair) / (LST + Tair) * 100
      
      # Donde:
      # NDVI = Índice de Vegetación de Diferencia Normalizada
      # NDWI = Índice de Vegetación de Diferencia de Aguas
      # LST = Temperatura de la superficie del suelo
      # Tair = Temperatura del aire
      
        # El Índice de Estrés Térmico de Transmitancia (TTI) se calcula utilizando la siguiente fórmula:
          TTI = (NIR - Tair) / (NIR + Tair)
        
        # El Índice de Estrés Hídrico de Transmitancia Normalizado (NTSI) se calcula utilizando la siguiente fórmula:
          NTSI = (NDWI - NDVI) / (NDWI + NDVI) / 10000000
          plot(NTSI, main = "NTSI")
        # El Índice de Estrés Térmico de Transmitancia Normalizado (NTTI) se calcula utilizando la siguiente fórmula:
          NTTI = (TTI - Tair) / (TTI + Tair) * 100
        
        # El Índice de Vegetación de Transparencia Verde (TVI) se calcula utilizando la reflectancia en las bandas verde y roja de una imagen multiespectral. La ecuación para calcular el TVI es la siguiente:
          
          TVI = (green - red) / (green + red)
          plot(TVI, main = "TVI")
        # Donde G es la reflectancia en la banda verde y R es la reflectancia en la banda roja. El TVI es un índice de vigor de vegetación que se utiliza para estimar la cantidad de clorofila presente en las hojas de una planta. Valores altos de TVI indican una alta concentración de clorofila y, por lo tanto, una alta productividad vegetal, mientras que valores bajos indican una baja concentración de clorofila y una baja productividad vegetal.
        
        # Es importante mencionar que el TVI es un índice de vegetación normalizado, y su valor varía entre -1 y 1, típicamente los valores negativos indican suelo desnudo y los valores positivos indican vegetación.
        
          # Índice de Vegetación de Diferencia Normalizada de Aguas (NDWI2): Este índice se basa en la relación entre las bandas azul y roja del espectro electromagnético, y se utiliza para medir la cantidad de agua en los tejidos vegetales y detectar áreas de sequía. 
          NDWI2 = (blue - nir) / (blue + nir)
          plot(NDWI2, main = "NDWI2")
          
          # Índice de Estrés Hídrico Vegetativo (VHI): Este índice se basa en la relación entre NDVI y NDWI, y se utiliza para medir el estrés hídrico en las plantas. Fórmula: 
          # VHI = (NDVI - NDWI) / (NDVI + NDWI)
          # plot(VHI, main = "VHI")
          
          # Índice de Estrés Hídrico Normalizado (NSI): Este índice se basa en la relación entre NDVI y NDWI, y se utiliza para medir el estrés hídrico en las plantas en términos normalizados. Fórmula: 
          # NSI = (NDVI - NDWI) / (NDVI + NDWI) * 100
          # plot(NSI, main = "NSI")
          
          # Índice de Estrés Hídrico de Transmitancia (TSI): Este índice se 
          # basa en la relación entre NDWI y NDVI, y se utiliza para medir la
          # disponibilidad de agua en el suelo y la capacidad de la vegetación
          # para retener agua. 
          TSI = NDWI / NDVI
          # TSI = NDWI2 / NDVI
          plot(TSI, main = "TSI")
          ggplot(raster::as.data.frame(TSI, xy = T) %>% na.omit() %>%
                   dplyr::filter(layer > -2)) +
            geom_raster(data = , aes(x = x, y = y, fill = layer)) +
            paletteer::scale_fill_paletteer_c("ggthemes::Red-Green-Gold Diverging",
                                              breaks = seq(-2, -0.6639892, by = 1),
                                              limits = c(-2, -0.6639892),
                                              labels = as.character(seq(-2, -0.6639892, by = 1)))+
            theme_test()
          # Índice de Estrés Térmico Vegetativo (TVI): Este índice se basa en la relación entre LST y Tair, y se utiliza para medir el estrés térmico en las plantas. Fórmula: 
          # TVI = (LST - Tair) / (LST + Tair)
          # plot(TVI, main = "TVI")
          
          Biomasa = 0.0005 * NDVI + 0.0011 * TVI - 0.0005
          plot(Biomasa, main = "Biomasa")
          Biomasa = 0.0032 * NDVI + 0.0014 * TVI - 0.0009
          plot(Biomasa, main = "Biomasa")
          Biomasa = 0.0021 * NDVI + 0.0001 * TVI + 0.8779
          plot(Biomasa, main = "Biomasa")
          Biomasa = -0.1343 * NDVI + 0.9185 * TVI + 0.0304
          plot(Biomasa, main = "Biomasa")
          
          a = 0.3; b = 0.3; c = 0.3; d = 0.1
          LAI = a * (NDVI - NDWI) + b * (EVI - NDWI) + c * (SAVI - NDWI) + d
          plot(LAI, main = "LAI")
          
          # e = 0.4; f = 0.3; g = 0.2; h = 0.1
          # LAI = e * (NDVI - NDWI) + f * (EVI - NDWI) + g * (MSAVI - NDWI) + h
          # plot(LAI, main = "LAI")
          # 
          # i = 0.5; j = 0.3; k = 0.1; l = 0.1
          # LAI = i * (NDVI - NDWI) + j * (EVI - NDWI) + k * (GNDVI - NDWI) + l
          # plot(LAI, main = "LAI")
          # 
          # m = 0.4; n = 0.3; o = 0.2; p = 0.1
          # LAI = m * (NDVI - NDWI) + n * (EVI - NDWI) + o * (NGBVI - NDWI) + p
          # plot(LAI, main = "LAI")
      
          # El Índice de fotosíntesis relacionada con la reflexión (PRI) es un índice de vigor de plantas que mide la relación entre la luz reflectida en el espectro verde y rojo. Se calcula utilizando la siguiente ecuación:
            # TVI = PRI
            # PRI = (green - red) / (green + red)
            # plot(PRI, main = "PRI")
          Nitrogen = 0.67 * NDVI + 0.57 * TVI - 0.13
          Nitrogen = (0.67 * MSAVI + 0.57 * TVI - 0.13)*10
          # Nitrogen = 5.68 * NDVI - 0.08
          # Nitrogen = 11.77 * TVI - 0.44
          # Nitrogen = 0.0002 * NDVI - 0.0003 * TVI + 0.65
          Nitrogen = (0.0051 * (NDVI)^2 + 0.0189 * (NDVI) + 0.0388 * (TVI))*200
          Nitrogen = (0.0051 * (MSAVI)^2 + 0.0189 * (MSAVI) + 0.0388 * (TVI))*200
          
          # Nitrogen = (1.09 * (NDVI) - 0.02 * (TVI) - 0.03 * (GNDVI))*5
          plot(Nitrogen, main = "Nitrogen (mg/kg)")
          ggplot(raster::as.data.frame(Nitrogen, xy = T) %>% na.omit()) +
            geom_raster(data = , aes(x = x, y = y, fill = layer)) +
            paletteer::scale_fill_paletteer_c("ggthemes::Red-Green-Gold Diverging",
                                              breaks = seq(-0, 2, by = 1),
                                              limits = c(-0, 2),
                                              labels = as.character(seq(0, 2, by = 1)))+
            theme_test()
          # CO2 = 0.001 * NDVI + 0.01
          # CO2 = 0.001 * NDVI + 0.002 * TSI + 0.03
          CO2 = (0.002 * NDVI + 0.003 * TSI + 0.02)#*100
          plot(CO2, main = "CO2")
          
          puntos_bandas = rasterToPoints(EX1, xy= T) %>% as.data.frame() #%>% 
        # mutate(SEMANA = S)
      puntos_sbandas = as(EX1,"SpatialPoints")

      puntos <- spatialEco::point.in.poly(puntos_sbandas, fundo) %>%
        as.data.frame() %>% rename(x = "coords.x1", y = "coords.x2") %>%
        left_join(puntos_bandas, c("x", "y"))
      
      

      raster_df_cons0 = rbind(puntos, raster_df_cons0) 
      print(paste("SEMANA",S, f))
    }

  raster_df_cons = rbind(raster_df_cons, raster_df_cons0)

  

  
  INDICES = raster_df_cons %>%
    dplyr::mutate(SAVI = ((nir-red)/(nir+red+0.5))*(1+0.5),
                  MSAVI = (2 * nir + 1 - sqrt ((2 * nir + 1)^2 - 8 * (nir  - red))) / 2,
                  NDVI = (nir-red)/(nir+red),
                  GNDVI = (nir-green) /(nir+green),
                  SIPI = (nir - blue) / (nir - red),
                  EVI =2.5 * (nir/10000 - red/10000) / (nir/10000 + 2.4* red /10000 + 1),
                  LAI = 3.618 *  EVI - 0.118,
                  KC_NDVI = SAVI* 1.1875 + 0.04,
                  KC_MSAVI = MSAVI* 1.1875 + 0.04,
                  KC_NDVI = NDVI* 1.1875 + 0.04,
                  KC_GNDVI = GNDVI* 1.1875 + 0.04,
                  SI1= sqrt(green/1000 * red/1000),
                  SI2 = sqrt((green/1000)^2+(red/1000)^2 +(nir/1000)^2),
                  SI3 = sqrt((green/1000)^2+(red/1000)^2),
                  INT1 = ((green/1000) + (red/1000))/2,
                  INT2 = ((green/1000) + (red/1000) + (nir/1000))/2,
                  BI = sqrt((green/1000)+ (nir/1000)^2)
                  # R_NDVI = ifelse(NDVI<=quantile(NDVI, 0.20),1,
                  #                ifelse(NDVI<=quantile(NDVI, 0.40), 2,
                  #                       ifelse(NDVI<=quantile(NDVI, 0.60), 3,
                  #                              ifelse(NDVI<=quantile(NDVI, 0.80), 4,
                  #                                     ifelse(NDVI<=quantile(NDVI, 1), 5))))),
                  # R_SAVI = ifelse(SAVI<=quantile(SAVI, 0.20),1,
                  #                ifelse(SAVI<=quantile(SAVI, 0.40), 2,
                  #                       ifelse(SAVI<=quantile(SAVI, 0.60), 3,
                  #                              ifelse(SAVI<=quantile(SAVI, 0.80), 4,
                  #                                     ifelse(SAVI<=quantile(SAVI, 1), 5))))),
                  # R_MSAVI = ifelse(MSAVI<=quantile(MSAVI, 0.20),1,
                  #                 ifelse(MSAVI<=quantile(MSAVI, 0.40), 2,
                  #                        ifelse(MSAVI<=quantile(MSAVI, 0.60), 3,
                  #                               ifelse(MSAVI<=quantile(MSAVI, 0.80), 4,
                  #                                      ifelse(MSAVI<=quantile(MSAVI, 1), 5))))),
                  # R_GNDVI = ifelse(GNDVI<=quantile(GNDVI, 0.20),1,
                  #                   ifelse(GNDVI<=quantile(GNDVI, 0.40), 2,
                  #                          ifelse(GNDVI<=quantile(GNDVI, 0.60), 3,
                  #                                 ifelse(GNDVI<=quantile(GNDVI, 0.80), 4,
                  #                                        ifelse(GNDVI<=quantile(GNDVI, 1), 5)))))
                  )



    # VIGOR_df_TRANS <- INDICES %>%
    #   gather(INDICE, VALOR, 17:26)
    
  # write_xlsx(df, path  = paste0("C:/Users/lcornejo/PROYECCIONES/INDICES VIGOR/GIF/",
  #                               "datos.xlsx"))
    sem_unico = data.frame( semanas =  unique(INDICES$SEMANA)) %>%
      arrange(semanas)
    
    
    # Utilizar ifelse() para evaluar la condición de longitud de caracter igual a 1
    sem_unico$semanas <- ifelse(nchar(sem_unico$semana) == 1, 
                                paste0("0", sem_unico$semana),sem_unico$semana)
}
}
    
    #shp_df <- st_as_sf(fundo)
    

    
table(sem_unico)
for ( t in sem_unico$semanas) {
  
  df = INDICES %>% dplyr::filter(SEMANA %in% as.numeric(t), FUNDO %in% f)
  pl = ggplot() +
    geom_raster(data = df , aes(x = x, y = y, fill = NDVI)) +
    ggtitle(paste("NDVI", f, "SEM", t, "-", AÑO )) +
    scale_fill_paletteer_c("ggthemes::Red-Green-Gold Diverging",
                           breaks = seq(0, 0.8, by = 0.2),
                           limits = c(0, 0.8),
                           labels = as.character(seq(0, 0.8, by = 0.2)))+
    theme_bw()+
    labs(x= "", y = "")+ theme(plot.title = element_text(hjust = 0.5),
                                        axis.line.x = element_blank(), 
                                        axis.text.x = element_blank(), 
                                        axis.ticks.x = element_blank(),
                                        axis.line.y = element_blank(),
                                        axis.text.y = element_blank(),
                                        axis.ticks.y = element_blank())
  
    
    

  pl
  # write_xlsx(df, path  = paste0("C:/Users/lcornejo/PROYECCIONES/INDICES VIGOR/GIF/",
  #                               "datos.xlsx"))
  
  
 
  ggsave(pl, filename = paste0("C:/Users/lcornejo/PROYECCIONES/INDICES VIGOR/",
  AÑO, "/GIF/",  f, "/", f, "_SEM", t, ".png"),
         width = 12,height=16,
         dpi = 300)
  print(t)
  
  
  list.files(path = paste0("C:/Users/lcornejo/PROYECCIONES/INDICES VIGOR/",
                           AÑO, "/GIF/",  f),
             pattern = ".png", full.names = T) %>% 
    map(image_read) %>% # lee las imagenes del directorio
    image_join() %>% # une las imagenes
    image_animate(fps=1) %>% # genera la animacion
    image_write(paste0("C:/Users/lcornejo/PROYECCIONES/INDICES VIGOR/",
                       AÑO, "/GIF/",  f, "/", f, "_", AÑO,".gif")) # la guarda en un fichero .gif
}
}







