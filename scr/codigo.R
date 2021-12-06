#Santiago Prieto 201814818
#R 4.1.1
#Taller A -Task3
rm(list = ls()) 
if(!require(pacman)) install.packages("pacman") ; require(pacman)
p_load(tidyverse,viridis,sf,leaflet, rio, skimr,ggsn, broom, stargazer, modelsummary,
       margins,rockchalk, htmltools, rvest, ggmap)

#PUNTO1-MAPAS#

via = st_read("data/input/VIAS.shp")

puntos = st_read("data/input/MGN_URB_TOPONIMIA.shp")

#1.1.2
c_medico = puntos %>% filter(CSIMBOL %in% c("021001", "021002", "021003"))

#1.1.3
c_poblado = import("data/input/c poblado (2017).rds") %>%  filter(cod_dane >= 54001, cod_dane < 55000)

depto = import("data/input/dp deptos (2017).rds") %>% filter(name_dpto=="NORTE DE SANTANDER")

mapmuse = import("data/input/victimas_map-muse.rds")

#1.2






#PUNTO 2-REGRESIONES#

#2.1:importo la base de datos, estimo el modelo ylo guardo en el objeto llamado ols
mapmusereg = readRDS("data/input/victimas_map-muse.rds")

mapmusereg = mapmusereg %>% mutate(estado = ifelse(estado=='Muerto',1,0))

ols = lm(estado ~ as.factor(tipo_accidente) + as.factor(year)  
         + as.factor(month)+ as.factor(condicion)+ as.factor(genero)
         + as.factor(actividad), data = mapmusereg) 

ols %>% summary()

#2.2: para exportar en imagen las estimaciones primero limpio para que no quede tan cargada la imagen y luego la exporto con ggsave
des = ols$coefficients %>% names()
menos = grep("cod|year", des)
des = des[-menos]
grafico22 = modelplot(ols, coef_map=des) + labs(title = "Efecto en probabilidad de morir por una mina")
grafico22
ggsave("views/coef_plot_ols.jpeg", grafico22)

#2.3: estimo logit y probit
logit = glm(estado ~ as.factor(tipo_accidente) 
            + as.factor(year)  
            + as.factor(month)
            + as.factor(genero)
            + as.factor(condicion)
            + as.factor(actividad),              
            data = mapmusereg, 
            family = binomial(link="logit")) 


probit = glm(estado ~ as.factor(tipo_accidente) 
             + as.factor(year)  
             + as.factor(month)
             + as.factor(genero)
             + as.factor(condicion)
             + as.factor(actividad),              
             data = mapmusereg, 
             family = binomial(link="probit")) 

#2.4: imprimo los resultados de las 3 estimaciones en una tabla
varbls = list('Logit' = logit , 'Probit' = probit , "OLS" = ols)
tabla24 = outreg(varbls, type="html")

#2.5:
logitmarginal = margins(logit)
probitmarginal = margins(probit)




#PUNTO 3-WEB-SCRAPING#

#3.1:
browseURL(url = 'https://es.wikipedia.org/wiki/Departamentos_de_Colombia',browser = getOption('browser'))

wiki = "https://es.wikipedia.org/wiki/Departamentos_de_Colombia"
wiki = read_html(wiki)
class(wiki)

#3.2: extraigo el titulo de la pagina y lo proyecto
titulo = wiki %>% html_nodes(xpath = '//*[@id="firstHeading"]') %>% html_text()
titulo

#3.3: extraigo la informción de los departamentos en tabla
departamentos = html_wiki %>% html_nodes('table') 
departamentos

departamentos[4] %>% html_table(header = T,fill=T)

tablafinaldepartamentos = departamentos[4] %>% html_table(header = T,fill=T)  %>% as.data.frame()

