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
    "ggrepel",
    "sf",
    "rgeos",
    "ggforce",
    "rworldmap",
    "rworldxtra",
    "scales",
    "openair",
    "raster",
    "rgdal",
    "rasterVis",
    "ggspatial",
    "reshape2",
    "cowplot",
    "googleway",
    "hydroTSM",
    "tidyverse",
    "padr",
    "openxlsx"
  ) # lista de pacotes utilizados

new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])] # checar se há algum pacote novo

if (length(new.packages))
  install.packages(new.packages) # instala os pacotes novos

lapply(list.of.packages, library, character.only = TRUE) # carrega os pacotes necessários

setwd("G:/My Drive/Orientações/Graduação/Especialização/Ajuda Alunos de Outros Orientadores") #define a pasta de trabalho

# 4 Importar Dados -------------------------------------------------------------

wb <- loadWorkbook("chuva_agua_branca.xlsx") # colocar o nome do arquivo do excel
wb2 <- createWorkbook("Report")

lst = read.xlsx(wb, sheet = 1,detectDates = TRUE,startRow = 11) #mudar startrow de acordo com sua planilha  
Data<- lst

# 4.1 Pré-tratamento dos dados --------------------------------------------

df<- Data[,c(-1,-35,-36)]

df_long<- df %>%
  pivot_longer(cols = starts_with('dia.'), 
               values_drop_na = FALSE) %>% 
  mutate(name = stringr::str_remove_all(name,"dia.")) %>%
  unite(Date, Ano, Mês, name, sep = '-') %>%
  mutate(Date = as.Date(Date))  %>%
  filter(!is.na(Date))
  
df_full<- pad(df_long) # add missing dates if needed

dfzoo<- read.zoo(df_full)

dfzoo<- dfzoo[index(dfzoo) <= as.Date("2019-12-31")]

# 5 Modelagem -------------------------------------------------------------


# 5.1 Funções -------------------------------------------------------------

countday<- function(x){sum((x==0),na.rm = TRUE)}
rain10c<- function(x){sum((x>=10),na.rm = TRUE)}
rain20c<- function(x){sum((x>=20),na.rm = TRUE)}
ndatac<- function(x){sum(is.na(x))}


# 5.2 Tratamento dos dados -------------------------------------------------

# ysumzoo<- daily2annual(dfzoo, FUN= sum) # totais anuais
dfzoomonth<- daily2monthly(dfzoo, FUN= sum) # totais mensais
Datamatrix <- matrix(dfzoomonth, ncol=12, byrow=TRUE)
# Dataframe<- fortify.zoo(dfzoo)
colnames(Datamatrix) <- c('Jan','Fev','Mar','Abr','Mai','Jun','Jul','Ago','Set','Out','Nov','Dez');
rownames(Datamatrix) <- unique(format(time(dfzoomonth), "%Y"))
Datamatrix<- cbind(Datamatrix,rowSums(Datamatrix))
colnames(Datamatrix) <- c('Jan','Fev','Mar','Abr','Mai','Jun','Jul','Ago','Set','Out','Nov','Dez','Totais Anuais');
Datamatrix<-rbind(Datamatrix,colMeans(Datamatrix))
rownames(Datamatrix)[nrow(Datamatrix)]<-"Média"
# Dataframemon<- daily2monthly(Dataframe, FUN= sum,out.type = "db") # totais mensais
# colnames(ysumframe)<- c('Ano','Valor','Jan','Fev','Mar','Abr','Mai','Jun','Jul','Ago','Set','Out','Nov','Dez');
# colnames(ysumframe)<- c('Valor')

rainydaysbymonth <-
  aggregate(dfzoo,
            format(time(dfzoo), "%m/%Y"),
            countday)
rainydaysbymonth<-data.frame('rdays'=rainydaysbymonth,'date'=as.Date(paste('01/',index(rainydaysbymonth),sep = ''),format = "%d/%m/%Y"))
rainydaysbymonth<-rainydaysbymonth[order(rainydaysbymonth$date),]
rainydaysbymonthmatrix<- matrix(rainydaysbymonth$rdays, ncol=12, byrow=TRUE)
rownames(rainydaysbymonthmatrix) <- unique(format(time(dfzoomonth), "%Y"))
rainydaysbymonthmatrix<- cbind(rainydaysbymonthmatrix,rowSums(rainydaysbymonthmatrix))
colnames(rainydaysbymonthmatrix) <- c('Jan','Fev','Mar','Abr','Mai','Jun','Jul','Ago','Set','Out','Nov','Dez','Total');

rain10 <-
  aggregate(dfzoo,
            format(time(dfzoo), "%m/%Y"),
            rain10c)
rain10<-data.frame('rdays'=rain10,'date'=as.Date(paste('01/',index(rain10),sep = ''),format = "%d/%m/%Y"))
rain10<-rain10[order(rain10$date),]
rain10<- matrix(rain10$rdays, ncol=12, byrow=TRUE)
rownames(rain10) <- unique(format(time(dfzoomonth), "%Y"))
rain10<- cbind(rain10,rowSums(rain10))
colnames(rain10) <- c('Jan','Fev','Mar','Abr','Mai','Jun','Jul','Ago','Set','Out','Nov','Dez','Total');


rain20 <-
  aggregate(dfzoo,
            format(time(dfzoo), "%m/%Y"),
            rain20c)
rain20<-data.frame('rdays'=rain20,'date'=as.Date(paste('01/',index(rain20),sep = ''),format = "%d/%m/%Y"))
rain20<-rain20[order(rain20$date),]
rain20<- matrix(rain20$rdays, ncol=12, byrow=TRUE)
rownames(rain20) <- unique(format(time(dfzoomonth), "%Y"))
rain20<- cbind(rain20,rowSums(rain20))
colnames(rain20) <- c('Jan','Fev','Mar','Abr','Mai','Jun','Jul','Ago','Set','Out','Nov','Dez','Total');

nodatacount <-
  aggregate(dfzoo,
            format(time(dfzoo), "%m/%Y"),
            ndatac)
nodatacount<-data.frame('rdays'=nodatacount,'date'=as.Date(paste('01/',index(nodatacount),sep = ''),format = "%d/%m/%Y"))
nodatacount<-nodatacount[order(nodatacount$date),]
nodatacount<- matrix(nodatacount$rdays, ncol=12, byrow=TRUE)
rownames(nodatacount) <- unique(format(time(dfzoomonth), "%Y"))
nodatacount<- cbind(nodatacount,rowSums(nodatacount))
colnames(nodatacount) <- c('Jan','Fev','Mar','Abr','Mai','Jun','Jul','Ago','Set','Out','Nov','Dez','Total');

rain_max_annual <-
  aggregate(dfzoo,
            format(time(dfzoo), "%Y"),
            max,na.rm=TRUE, na.action=NULL)
rain_max_annual<-data.frame('rdays'=rain_max_annual,'date'=as.Date(paste('31/12/',index(rain_max_annual),sep = ''),format = "%d/%m/%Y"))
rain_max_annual<-rain_max_annual[order(rain_max_annual$date),]


# 6 Gráficos --------------------------------------------------------------

dfzoomonth<- daily2monthly(dfzoo, FUN= sum) # totais mensais
Datamatrix <- matrix(dfzoomonth, ncol=12, byrow=TRUE)
monthnames <- c('Jan','Fev','Mar','Abr','Mai','Jun','Jul','Ago','Set','Out','Nov','Dez');
colnames(Datamatrix) <- monthnames
rownames(Datamatrix) <- unique(format(time(dfzoomonth), "%Y"))
print(matrixplot(Datamatrix, ColorRamp="Precipitation", main="Precipitação Mensal Água Branca, [mm/mês]"))


dwimatrix<- dwi(dfzoo, out.unit="mpy", dates=1)
matrixplot(as.matrix(dwimatrix), ColorRamp = "PCPAnomaly", var.type="Days", main="Número de dias com informação por mês e ano")

mm<- aggregate(daily2monthly(dfzoo, FUN=sum, na.rm=TRUE), 
               format(time(daily2monthly(dfzoo, FUN=sum, na.rm=TRUE)), "%m"), mean) # Calcular as médias mensais da série

md<- aggregate(dfzoo, format(time(dfzoo), "%d"), mean, na.rm = TRUE) # Calcular a médias dos dias de cada mês

ma<- daily2annual(dfzoo, FUN=sum, na.rm=TRUE) # Calcular os volumes anuais

mean(daily2annual(dfzoo, FUN=sum, na.rm=TRUE)) # Calcula média anual


mdm<- aggregate(dfzoo, format(time(dfzoo), "%d/%m"), mean, na.rm = TRUE) # Calcular a médias dos dias por mês

a<- as.data.frame(mdm)
a$names <- rownames(a)
a<- cbind(a,substr(a[,2],4,5),substr(a[,2],1,2))
colnames(a)<- c("Valor","Dia_Mes","Mes","Dia")
b<- dcast(a,Dia ~ Mes,value.var = "Valor")
c<-b[,2:ncol(b)]
colnames(c)<- monthnames
matrixplot(as.matrix(c), var.type="Days", main="Média de Precipitação Diária em mm, 1994--2019")

mdm<- aggregate(dfzoo, format(time(dfzoo), "%d/%m"), max, na.rm = TRUE) # Calcular a médias dos dias por mês

a<- as.data.frame(mdm)
a$names <- rownames(a)
a<- cbind(a,substr(a[,2],4,5),substr(a[,2],1,2))
colnames(a)<- c("Valor","Dia_Mes","Mes","Dia")
b<- dcast(a,Dia ~ Mes,value.var = "Valor")
c<-b[,2:ncol(b)]
colnames(c)<- monthnames
matrixplot(as.matrix(c), var.type="Days", main="Máxima de Precipitação diária em mm, 1994--2019")

mdm<- aggregate(dfzoo, format(time(dfzoo), "%d/%m"), min, na.rm = TRUE) # Calcular a médias dos dias por mês

a<- as.data.frame(mdm)
a$names <- rownames(a)
a<- cbind(a,substr(a[,2],4,5),substr(a[,2],1,2))
colnames(a)<- c("Valor","Dia_Mes","Mes","Dia")
b<- dcast(a,Dia ~ Mes,value.var = "Valor")
c<-b[,2:ncol(b)]
colnames(c)<- monthnames
matrixplot(as.matrix(c), var.type="Days", main="Mínima de Precipitação diária em mm, 1994--2019")

countday<- function(x){sum((x>0),na.rm = TRUE)}

mdmcount<- aggregate(dfzoo, format(time(dfzoo), "%d/%m"), countday)

a<- as.data.frame(mdmcount)
a$names <- rownames(a)
a<- cbind(a,substr(a[,2],4,5),substr(a[,2],1,2))
colnames(a)<- c("Valor","Dia_Mes","Mes","Dia")
b<- dcast(a,Dia ~ Mes,value.var = "Valor")
c<-b[,2:ncol(b)]
colnames(c)<- monthnames
matrixplot(as.matrix(c),ColorRamp = "PCPAnomaly", var.type="Days", main="Número de Dias com Chuva, 1994--2019")

mdmmax<- aggregate(dfzoo, format(time(dfzoo), "%d/%m"), max, na.rm = TRUE)
a<- as.data.frame(mdmmax)
a<- a[order(a$mdmmax,decreasing = TRUE), ,drop = FALSE]
a<- cbind(a,index(a))
a$names <- rownames(a)
# a<- cbind(a,Freq = a$`index(a)`/ (nrow(a)+1))
a<- cbind(a,Freq = a$`index(a)`/ (105+1))
a<- cbind(a,TR = (1/a$Freq) )
a<- cbind(a,Mes = substr(a[,3],4,5),Dia = substr(a[,3],1,2))
colnames(a)<- c("Valor","Ind","Dia_Mes","Freq","TR","Mes","Dia")
b<- dcast(a,Dia ~ Mes,value.var = "TR")
c<-b[,2:ncol(b)]
colnames(c)<- monthnames
matrixplot(as.matrix(c),ColorRamp = "TEMPAnomaly2", var.type="Days", main="Tempo de Retorno em anos para chuvas máximas, 1994--2019")


mdm<- aggregate(dfzoo, format(time(dfzoo), "%m/%Y"), sum, na.rm = TRUE) # Calcular a chuva mensal
a<- as.data.frame(mdm)
a$names <- rownames(a)
a<- cbind(a,substr(a[,2],4,7),substr(a[,2],1,2))
colnames(a)<- c("Valor","Dia_Ano","Ano","Mes")
b<- dcast(a,Ano ~ Mes,value.var = "Valor")
c<-b[,2:ncol(b)]
colnames(c)<- monthnames
rownames(c)<- as.vector(unique(a$Ano))
matrixplot(as.matrix(c[(nrow(c)-10):nrow(c),]), var.type="Days", main="Precipitação Mensal (mm), 2009--2019")
e<-rowSums(c)

# 7 Exortar Dados ---------------------------------------------------------

sheet<- as.character(lst[[1]][1]);
sheetData<- paste(as.character(lst[[1]][1]),"Data")
addWorksheet(wb2,sheetName = sheetData)
addWorksheet(wb2,sheetName = sheet)
# x$Valor[is.na(x$Valor)]<--99.0; # substituir valores NA por -99
writeData(wb2,sheet = sheetData,x = fortify(dfzoo), rowNames = TRUE)

writeData(wb2,sheet = sheet,x = "Série", rowNames = FALSE,startCol = 1,startRow = 2);
writeData(wb2,sheet = sheet,x = "=====", rowNames = FALSE,startCol = 1,startRow = 3);
writeData(wb2,sheet = sheet,x = "Série:", rowNames = FALSE,startCol = 1,startRow = 4);
writeData(wb2,sheet = sheet,x = sheet, rowNames = FALSE,startCol = 2,startRow = 4);
writeData(wb2,sheet = sheet,x = "Totais Mensais", rowNames = FALSE,startCol = 14,startRow = 8);
writeData(wb2,sheet = sheet,x = e, rowNames = TRUE,startCol = 14,startRow = 9);
writeData(wb2,sheet = sheet,x = "Maior Precip. Diária", rowNames = FALSE,startCol = 15,startRow = 8);
writeData(wb2,sheet = sheet,x = rain_max_annual$rdays, rowNames = TRUE,startCol = 15,startRow = 9);
writeData(wb2,sheet = sheet,x = "N. dias sem dados", rowNames = FALSE,startCol = 16,startRow = 8);
writeData(wb2,sheet = sheet,x = nodatacount[,13], rowNames = TRUE,startCol = 16,startRow = 9);
writeData(wb2,sheet = sheet,x = "==============", rowNames = FALSE,startCol = 1,startRow = 7);
writeData(wb2,sheet = sheet,x = Datamatrix, rowNames = TRUE,startCol = 1,startRow = 8);
writeData(wb2,sheet = sheet,x = "Ano", rowNames = FALSE,startCol = 1,startRow = 8);

writeData(wb2,sheet = sheet,x = "Dias sem Chuva", rowNames = FALSE,startCol = 1,startRow = 8 + nrow(Datamatrix) + 2);
writeData(wb2,sheet = sheet,x = "==============", rowNames = FALSE,startCol = 1,startRow = 8 + nrow(Datamatrix) + 3);
writeData(wb2,sheet = sheet,x = rainydaysbymonthmatrix, rowNames = TRUE,startCol = 1,startRow = 8 + nrow(Datamatrix) + 4);
writeData(wb2,sheet = sheet,x = "Ano", rowNames = FALSE,startCol = 1,startRow = 8 + nrow(Datamatrix) + 5);

nr<- 8 + nrow(Datamatrix) + 5

writeData(wb2,sheet = sheet,x = "Dias com Chuva >= 10 mm", rowNames = FALSE,startCol = 1,startRow = (nr) + nrow(rainydaysbymonthmatrix) + 2);
writeData(wb2,sheet = sheet,x = "==============", rowNames = FALSE,startCol = 1,startRow = nr + nrow(rainydaysbymonthmatrix) + 3);
writeData(wb2,sheet = sheet,x = rain10, rowNames = TRUE,startCol = 1,startRow = nr + nrow(rainydaysbymonthmatrix) + 4);
writeData(wb2,sheet = sheet,x = "Ano", rowNames = FALSE,startCol = 1,startRow = nr + nrow(rainydaysbymonthmatrix) + 5);

nr<- nr + nrow(rainydaysbymonthmatrix) + nrow(rain10) + 5

writeData(wb2,sheet = sheet,x = "Dias com Chuva >= 20 mm", rowNames = FALSE,startCol = 1,startRow = nr + 2);
writeData(wb2,sheet = sheet,x = "==============", rowNames = FALSE,startCol = 1,startRow = nr + 3);
writeData(wb2,sheet = sheet,x = rain20, rowNames = TRUE,startCol = 1,startRow = nr + 4);
writeData(wb2,sheet = sheet,x = "Ano", rowNames = FALSE,startCol = 1,startRow = nr + 5);

nr<- nr + nrow(rain20) + 5

statdata<- smry(Datamatrix[1:nrow(Datamatrix)-1,ncol(Datamatrix)])
colnames(statdata)<- "Estatística Totais Anuais"
rownames(statdata)<- c("Min","Primeiro Quartil","Média","Mediana","Terceiro Quartil","Max","Inter-Quartil","Desvio Padrão","Coeficiente de Variação","Coeficiente de Assimetria","Coeficiente de Kurtose","Tamanho da Série", "Número de NA´s")
writeData(wb2,sheet = sheet,x = statdata, rowNames = TRUE,startCol = 1,startRow = nr + 2);

saveWorkbook(wb2, file = paste(getwd(),'Report_data.xlsx',sep='/'), overwrite = TRUE) # nome do arquivo de saída, ou seja, em uma coluna;

