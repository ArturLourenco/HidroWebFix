# 1 Limpar Console e Memória -------------------------------------------------------------------

gc(TRUE) #garbage colector da RAM
rm(list = ls()) #limpar memoria das variáveis globais
dev.off() # limpar os plots
cat("\014") #limpar console


# 2 Carregar bibliotecas, arquivos externos e definir pasta de trabalho -------------------------------------------------------------

list.of.packages <-
  c(
    "colorRamps",
    "ggplot2",
    "zoo",
    "RColorBrewer",
    "ggforce",
    "scales",
    "hydroTSM",
    "tidyverse",
    "readr",
    "stringr",
    "lubridate",
    "padr",
    "xml2"
  ) # lista de pacotes utilizados

new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])] # checar se há algum pacote novo

if (length(new.packages))
  install.packages(new.packages) # instala os pacotes novos

lapply(list.of.packages, library, character.only = TRUE) # carrega os pacotes necessários

setwd("G:\\My Drive\\R\\Hidroweb_Data_Process") #define a pasta de trabalho troque \ por / ou \\

# 3 Importar Dados -------------------------------------------------------------

file_option<- 1 # 1 for web and 2 for zip file 

# 3.1 Importar direto do webservice da hidroweb ---------------------------

if (file_option == 1) {

streamflow_station_number<- 37340000 

html_raw<- read_html(paste('http://telemetriaws1.ana.gov.br/ServiceANA.asmx/HidroSerieHistorica?codEstacao=',streamflow_station_number,'&dataInicio=&dataFim=&tipoDados=3&nivelConsistencia=',sep = ''))

streamflow_html_node<- xml_children(xml_find_all(html_raw,'.//documentelement'))

streamflow_list<- as_list(streamflow_html_node)

streamflow_list<- lapply(streamflow_list, lapply, function(x) if (length(x) == 0) {list(NA)} else {x})

streamflow_list<- lapply(streamflow_list,unlist)

streamflow_df<- do.call(rbind.data.frame,streamflow_list)

colnames(streamflow_df)<- names(streamflow_list[[1]])

streamflow_df$datahora<- as.Date(streamflow_df$datahora)

streamflow_df<- streamflow_df %>% rename(datahora = data)

} else {
  
# 3.2 Importar de arquivo zipado gerado no site da hidroweb ---------------

file_dir<- 'G:\\My Drive\\R\\Hidroweb_Data_Process\\Medicoes_convencionais.zip'

temp_data <- tempfile("Medicoes_convencionais",fileext=c(".zip"))

file.copy(file_dir,temp_data)

unzip(temp_data,exdir = tempdir())

variables<- unzip(temp_data,list = TRUE)

streamflow_file_name<- variables$Name[which(str_locate(variables$Name, pattern = 'vazoes') > 0)[1]]

streamflow_station_number<- parse_number(streamflow_file_name)

streamflow_file_name_dir<- paste(tempdir(),streamflow_file_name,sep = '\\')

streamflow_df<- read_delim(streamflow_file_name_dir, delim = ";", 
                           locale = locale("br",decimal_mark = ',',date_format = "%d/%m/%Y"),skip = 13)

# unlink(tempdir(), recursive = TRUE) #deletar a pasta temporária

}

# 3.1 Pré-tratamento dos dados --------------------------------------------

streamflow_df_long<- streamflow_df %>%
  set_names(~ str_to_lower(.)) %>% 
  select(-ends_with('status')) %>% 
  pivot_longer(cols = starts_with('vazao'), 
               values_drop_na = FALSE) %>%  
  mutate(dia = stringr::str_remove_all(name,"vazao")) %>%
  mutate(ano = year(data), mes = month(data)) %>% 
  unite(data,ano, mes, dia, sep = '-') %>%
  mutate(data = as.Date(data)) %>%
  filter(!is.na(data)) %>% 
  dplyr::rename(v_m3_s = value) %>% 
  select(data,v_m3_s) %>% 
  pad()
    

# 4 Tratamento -------------------------------------------------------------

monthnames_br <-c('Jan','Fev','Mar','Abr','Mai','Jun','Jul','Ago','Set','Out','Nov','Dez')

streamflow_daily_mean <- streamflow_df_long %>%
  group_by(dia_mes = format(data, "%m-%d")) %>% 
  summarise(f_mean = mean(v_m3_s,na.rm = TRUE)) %>%
  mutate(mes=substr(.$dia_mes, 1, 2),dia=as.factor(substr(.$dia_mes, 4, 5))) %>% 
  mutate(data=as.Date.character(paste("2020",mes, dia,sep = "-") )) %>% #apenas para organizar os valores
  arrange(data)

streamflow_daily_max <- streamflow_df_long %>%
  group_by(dia_mes = format(data, "%m-%d")) %>% 
  summarise(f_max = max(v_m3_s,na.rm = TRUE)) %>%
  mutate(mes=substr(.$dia_mes, 1, 2),dia=as.factor(substr(.$dia_mes, 4, 5))) %>% 
  mutate(data=as.Date.character(paste("2020",mes, dia,sep = "-") )) %>% #apenas para organizar os valores
  arrange(data)

streamflow_daily_with_flow <- streamflow_df_long %>%
  filter(v_m3_s>0) %>% 
  group_by(dia_mes = format(data, "%m-%d")) %>% 
  summarise(days_q = n()) %>%
  mutate(mes=substr(.$dia_mes, 1, 2),dia=as.factor(substr(.$dia_mes, 4, 5))) %>% 
  mutate(data=as.Date.character(paste("2020",mes, dia,sep = "-") )) %>% #apenas para organizar os valores
  arrange(data)

# 5 Gráficos --------------------------------------------------------------


# Vazão diária média ------------------------------------------------------

ggplot(data = streamflow_daily_mean, aes(x = mes, y = dia, fill = f_mean)) +
  geom_tile(color = "Black", size = .7) +
  scale_y_discrete(name = "Dias", breaks = unique(streamflow_daily_mean$dia)[seq(1, 31, 2)]) +
  scale_x_discrete(name = "Meses", labels = monthnames_br) +
  # geom_text(aes(Mes, Dia, label = Valor), color = "black", size = 4) +
  scale_fill_gradientn(colours = brewer.pal(9,'Blues'), name = 'Q(m³/s)') +
  theme_minimal() +
  theme(
    # text=element_text(size=16, family="A"),
    text=element_text(size=12),
    # axis.title.x = element_blank(),
    # axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank()
  ) +
  coord_equal(ratio = 0.2) +
  labs(title = "Matriz de ??? na Estação Fluviométrica ???",
       subtitle = "Valor médio diário da série histórica 19??-20??",
       caption = "Fonte dos dados: ANA, 2021.")


# Vazão diária máxima -----------------------------------------------------

ggplot(data = streamflow_daily_max, aes(x = mes, y = dia, fill = f_max)) +
  geom_tile(color = "Black", size = .7) +
  scale_y_discrete(name = "Dias", breaks = unique(streamflow_daily_mean$dia)[seq(1, 31, 2)]) +
  scale_x_discrete(name = "Meses", labels = monthnames_br) +
  # geom_text(aes(Mes, Dia, label = Valor), color = "black", size = 4) +
  scale_fill_gradientn(colours = brewer.pal(9,'Reds'), name = 'Q(m³/s)') +
  theme_minimal() +
  theme(
    # text=element_text(size=16, family="A"),
    text=element_text(size=12),
    # axis.title.x = element_blank(),
    # axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank()
  ) +
  coord_equal(ratio = 0.2) +
  labs(title = "Matriz de ??? na Estação Fluviométrica ???",
       subtitle = "Valor máximo diário da série histórica 19??-20??",
       caption = "Fonte dos dados: ANA, 2021.")


# Dias com vazão por ano --------------------------------------------------

ggplot(data = streamflow_daily_with_flow, aes(x = mes, y = dia, fill = days_q)) +
  geom_tile(color = "Black", size = .7) +
  scale_y_discrete(name = "Dias", breaks = unique(streamflow_daily_mean$dia)[seq(1, 31, 2)]) +
  scale_x_discrete(name = "Meses", labels = monthnames_br) +
  # geom_text(aes(Mes, Dia, label = Valor), color = "black", size = 4) +
  scale_fill_gradientn(colours = brewer.pal(9,'Reds'), name = 'Dias') +
  theme_minimal() +
  theme(
    # text=element_text(size=16, family="A"),
    text=element_text(size=12),
    # axis.title.x = element_blank(),
    # axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank()
  ) +
  coord_equal(ratio = 0.2) +
  labs(title = "Matriz de ??? na Estação Fluviométrica ???",
       subtitle = "Valor máximo diário da série histórica 19??-20??",
       caption = "Fonte dos dados: ANA, 2021.")

