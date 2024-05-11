#Script preparacion:

# 0.Ajustes iniciales:
rm(list=ls()) #CODIGO PARA LIMPIAR EL ENVIRONMENT, SI QUIERES ELIMINAR SOLO 1 ESCRIBELO DENTRO DEL PARENTESIS,ASÍ:rm(objeto_a_eliminar)
options(scipen=999) #Desactivar notacion cientifica (los e10,e15,etc)

setwd("C:/Users/Javi/Desktop/Trabajo.3.Rstudio")

# 1.Paquetes:
install.packages("pacman")
pacman::p_load(haven,
               sjlabelled,
               dplyr, #Manipulacion de datos
               stargazer, #Tablas
               sjmisc, # Tablas
               summarytools, # Tablas
               kableExtra, #Tablas
               sjPlot, #Tablas y gráficos
               corrplot, # Correlaciones
               sessioninfo, # Información de la sesión de trabajo
               ggplot2,  # Para la mayoría de los gráficos
               mice)   #Para imputar NA's

#2. Cargamos base de datos:

load("C:/Users/Javi/Desktop/Trabajo.3.Rstudio/input/Latinobarometro_2023_Esp_Rdata_v1_0.rdata")

names(Latinobarometro_2023_Esp_v1_0)
dim(Latinobarometro_2023_Esp_v1_0)

#3. Seleccion y manipulación de variables:
find_var(data = find_var(data = Latinobarometro_2023_Esp_v1_0,"idenpa"),"idenpa")

#Base con variables a utilizar
proc_data <- select(Latinobarometro_2023_Esp_v1_0, idenpa, P19N, P32INN, P33N.D, P33N.C, P33N.A)

proc_data <- Latinobarometro_2023_Esp_v1_0 %>% select(idenpa, #país
                                                      
                                                      P19N, #manera de pensar respecto a una sociedad diversa
                                                      
                                                      P32INN, #manera de pensar respecto a llegada de inmigrantes 
                                                      
                                                      P33N.D, #grado de acuerdo: inmigrantes mejoran nuestra sociedad con ideas y cultura
                                                      
                                                      P33N.C, #grado de acuerdo: inmigrantes aumentan el crimen         
                                                      
                                                      P33N.A) #grado de acuerdo: inmigrantes son buenos para la economia del país


#Comprobar
names(proc_data)  
summary(proc_data)
sjlabelled::get_label(proc_data)

frq(proc_data$idenpa)
frq(proc_data$P19N)
frq(proc_data$P32INN)
frq(proc_data$P33N.D)
frq(proc_data$P33N.C)
frq(proc_data$P33N.A)

#FILTRAMOS:
proc_data <- proc_data %>%
  filter(idenpa==152)  

#Tratamos los casos perdidos:
proc_data <- proc_data %>% set_na(., na = -5)
proc_data <- na.omit(proc_data)

#RENOMBRAMOS:
proc_data <- proc_data %>% rename("soc_diversa"=P19N, 
                                  "inmi_ideycul"=P33N.D, 
                                  "inmi_crim"=P33N.C, 
                                  "inmi_eco"=P33N.A,
                                  "inmi_llega"=P32INN)
sjlabelled::get_label(proc_data)

#Removemos base anterior
remove(Latinobarometro_2023_Esp_v1_0)

#Guardar base nueva
save(proc_data,file = "input/proc_data.RData")

#Comprobamos:
names(proc_data)

view_df(proc_data)

frq(proc_data$idenpa)
frq(proc_data$soc_diversa)
frq(proc_data$inmi_llega)
frq(proc_data$inmi_ideycul)
frq(proc_data$inmi_crim)
frq(proc_data$inmi_eco)

# 4.Descriptivos:
summarytools::dfSummary(proc_data, plain.ascii = FALSE)
view(dfSummary(proc_data, headings=FALSE))

# 4.1 Gráficos
graf_soc_diversa <- ggplot(proc_data, aes(x = soc_diversa)) +
  geom_bar(fill = "pink") + 
  labs (title = "Percepción de la diversidad social")

graf_inmi_llega <- ggplot(proc_data, aes(x = inmi_llega)) +
  geom_bar(fill = "pink") + 
  labs (title = "Percepción de llegada de inmigrantes")

graf_inmi_ideycul <- ggplot(proc_data, aes(x = inmi_ideycul)) +
  geom_bar(fill = "pink") + 
  labs (title = "Interculturalidad")

graf_inmi_crim <- ggplot(proc_data, aes(x = inmi_crim)) +
  geom_bar(fill = "pink") + 
  labs (title = "Aumento inmigración-crimen")

graf_inmi_eco <- ggplot(proc_data, aes(x = inmi_eco)) +
  geom_bar(fill = "pink") + 
  labs (title = "Impacto económico de los inmigrantes")

# Guardamos gráficos:
ggsave(graf_inmi_crim, file="output/graf_inmi_crim.png")
ggsave(graf_inmi_eco, file="output/graf_inmi_eco.png")
ggsave(graf_inmi_ideycul, file="output/graf_inmi_ideycul.png")
ggsave(graf_inmi_llega, file="output/graf_inmi_llega.png")
ggsave(graf_soc_diversa, file="output/graf_soc_diversa.png")

#5. Asociación de variables: 
dim(proc_data)
sjmisc::descr(proc_data,
              show = c("label","range", "mean", "sd", "NA.prc", "n")) %>%
  kable(.,"markdown")

# 5.1.Correlación
#proc_data <- mutate_all(proc_data, as.numeric)  ### ESTE CODIGO ES PARA PASAR TODAS LAS VARIABLES DE LA BASE A "NUMERIC"

cor_proc_data <- cor(proc_data,
                     use = "complete.obs")
cor_proc_data

sjPlot::tab_corr(proc_data, 
                 triangle = "lower")

corrplot.mixed(cor_proc_data)

# 5.2 Confiabilidad del conjunto:
## Alpha de Cronbach:
psych::alpha(proc_data, check.keys = TRUE)

##Alpha de Cronbach recodificado:
psych::alpha(dplyr::select(proc_data, inmi_ideycul, inmi_crim, inmi_eco),check.keys = TRUE)

#CONSTRUCCION DE ESCALA:

proc_data <- proc_data %>% 
  rowwise() %>% 
  mutate(percepcion_impacto_integral_inmigrantes = sum(inmi_ideycul, inmi_crim, inmi_eco))
summary(proc_data$percepcion_impacto_integral_inmigrantes)


ggplot(proc_data, aes(x = percepcion_impacto_integral_inmigrantes)) +
  geom_histogram(binwidth = 0.6, colour = "black", fill = "yellow") +  
  theme_bw() +
  xlab("Percepcion del impacto integral de inmigrantes") +
  ylab("Frecuencia")
