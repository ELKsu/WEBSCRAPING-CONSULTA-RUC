################################################################################
#                         WEBSCRAPING CONSULTA RUC  
#-------------------------------------------------------------------------------
# Autor:  Luis Casusol
# email:  luis.casusol.ortega@gmail.com
# Github: https://github.com/ELKsu
################################################################################

#-------------------------------------------------------------------------------
# Configuración previa
# Seguir los pasos del siguiente link de Github:
# https://github.com/ELKsu/WEBSCRAPING-CONSULTA-RUC
#-------------------------------------------------------------------------------

#Previamente se debe instalar
#install.packages("RSelenium")
#install.packages("rvest")

#Ejecutar para escoger la version de tu chromedriver
binman::list_versions("chromedriver")

library(RSelenium)
library(rvest)
library(stringr)

driver <- rsDriver(browser = "chrome",
                   chromever = '102.0.5005.61',
                   verbose = FALSE,
                   port = 4444L)
drive_remote <- driver$client

url = 'https://e-consultaruc.sunat.gob.pe/cl-ti-itmrconsruc/FrameCriterioBusquedaWeb.jsp'
drive_remote$navigate(url)

tabla_empresas_final = data.frame()
tabla_trabajadores_final = data.frame()
tabla_establecimientos_final = data.frame()
tabla_representantes_final = data.frame()

ruc = read.csv("D:/4. WEBSCRAPING/R/SUNAT/rucs.csv", sep="")
ruc$ruc = as.character(ruc$ruc)
ruc = as.matrix(ruc)

for (num in 1:length(ruc)) {

  drive_remote$findElement( using = "id", 'txtRuc')$clearElement()
  
  drive_remote$findElement( using = "id", 'txtRuc')$sendKeysToElement(list(ruc[num]))
  drive_remote$findElement(using = "id", 'btnAceptar')$clickElement()
  
  Sys.sleep(3)
  
  html <- drive_remote$getPageSource()[[1]]
  
  webpage <- read_html(html)
  
  tipo_data <- webpage %>%html_nodes(".col-sm-5")%>%html_text2()
  data <- webpage %>%html_nodes(".col-sm-7")%>%html_text2()
  
  tipo_data_1 <- webpage %>%html_nodes(".col-sm-3")%>% html_nodes("h4.list-group-item-heading") %>% html_text2()
  data_1 <- webpage %>%html_nodes(".col-sm-3")%>% html_nodes("p.list-group-item-text")%>%html_text2()

  id = ruc[num]
  tipo_data <- c(tipo_data,tipo_data_1)
  data <- c(data,data_1)
  
  if (length(tipo_data)==18) {
    documento = "Tipo de Documento:"
    tipo_data = c(tipo_data[1:2],documento,tipo_data[3:18])
    documento = ""
    data = c(data[1:2],documento,data[3:18])
  }
  
  tabla_empresa = data.frame(id,tipo_data,data)

  tabla_empresa = reshape(data = tabla_empresa, idvar = 'id', timevar = 'tipo_data', v.names = 'data', direction = 'wide')

  colnames (tabla_empresa) <- c("RUC", "NOMBRE_COMERCIAL", "TIPO_CONTRIBUYENTE","TIPO_DOCUMENTO",
                             "NOMBRE_COMERCIAL","ESTADO", "CONDICION","DOMICILIO", 
                             "SISTEMA_CONTABILIDAD","ACTIVIDAD_ECONOMICA",
                             "COMPROBANTE_PAGO","SISTEMA_EMISION_ELECTRONICA",
                             "AÑO_EMISION_ELECTRONICA","COMPROBANTES_ELECTRONICOS",
                             "PLE","PADRONES","FECHA_INSCRIPCION",
                             "FECHA_INICIO_ACTIVIDADES","SISTEMA_EMISION_COMPROBANTE",
                             "COMERCIO_EXTERIOR")
  
  tabla_empresa$NOMBRE_COMERCIAL <- str_replace(tabla_empresa$NOMBRE_COMERCIAL, ruc[num], "")
  tabla_empresa$NOMBRE_COMERCIAL <- str_replace(tabla_empresa$NOMBRE_COMERCIAL, " - ", "")
  
  tabla_empresas_final = rbind(tabla_empresas_final,tabla_empresa)
  
  webElem <- drive_remote$findElement("css", "body")
  
  for (x in 1:40) {
    webElem$sendKeysToElement(list(key = "down_arrow"))
  }

  for (x in 1:40) {
    webElem$sendKeysToElement(list(key = "up_arrow"))
  }
  
  #Trabajadores
  drive_remote$findElement(using = "name","formNumTrabajd")$findChildElement(using = "tag name","button")$clickElement()
  
  Sys.sleep(5)
  
  html_trabajadores <- drive_remote$getPageSource()[[1]]
  
  webpage_trabajadores <- read_html(html_trabajadores)
  
  tabla <- (webpage_trabajadores %>%
              html_nodes("table.table") %>%
              html_table())
  
  #Si la info de trabajadores no es vacio 
  if (length(tabla) != 0){
    tabla <- tabla[[1]]
    tabla_trab = data.frame(id,tabla)
    
    tabla_trabajadores_final = rbind(tabla_trabajadores_final,tabla_trab)
  }
  
  drive_remote$goBack()
  Sys.sleep(3)

  #Representantes
  result1 <- tryCatch({suppressMessages({
    drive_remote$findElement(using = "name","formRepLeg")$findChildElement(using = "tag name","button")$clickElement()
    })
  }, 
  error = function(e) {
    NA_character_
  }
  )
  
  Sys.sleep(5)

  if (is.null(result1)) {
    html_representantes <- drive_remote$getPageSource()[[1]]
    
    webpage_representantes <- read_html(html_representantes)
    
    tabla <- (webpage_representantes %>%
                html_nodes("table.table") %>%
                html_table())
    
    if (length(tabla) != 0){
      tabla <- tabla[[1]]
      tabla_repre = data.frame(id,tabla)
      
      colnames (tabla_repre) <- c("RUC", "DOCUMENTO", "NRO_DOCUMENTO", "NOMBRE", "CARGO", "FECHA")
      
      tabla_representantes_final = rbind(tabla_representantes_final,tabla_repre)
    }
    
    drive_remote$goBack()
    Sys.sleep(3)
    
    }  
  
  #Establecimientos 
  result2 <- tryCatch({suppressMessages({
    drive_remote$findElement(using = "name","formLocAnex")$findChildElement(using = "tag name","button")$clickElement()
  })
  }, 
  error = function(e) {
    NA_character_
  }
  )
  Sys.sleep(5)
  
  if (is.null(result2)) {
    
    html_establecimientos <- drive_remote$getPageSource()[[1]]
    
    webpage_establecimientos <- read_html(html_establecimientos)
    
    tabla <- (webpage_establecimientos %>%
                html_nodes("table.table") %>%
                html_table())
    
    if (length(tabla) != 0){
      
      xpag = drive_remote$findElement(using = "xpath",'/html/body/div/table[2]/tbody/tr/td[2]')$getElementText()
      n_pag = as.numeric(unlist(regmatches(xpag, gregexpr("[[:digit:]]+", xpag))))
      
      if (length(n_pag)>1){
        tabla_estab = data.frame()
        
        for (num in 1:length(n_pag)) {
          skip = drive_remote$findElement(using = "xpath",'/html/body/div/table[2]/tbody/tr/td[3]')
          Sys.sleep(2)
          
          html_establecimientos <- drive_remote$getPageSource()[[1]]
          
          webpage_establecimientos <- read_html(html_establecimientos)
          
          tabla <- (webpage_establecimientos %>%
                      html_nodes("table.table") %>%
                      html_table())
          
          tabla <- tabla[[1]]
          tabla = cbind(id,tabla)
          
          tabla_estab = rbind(tabla_estab,tabla)
          
          if (skip$getElementText() == 'Siguiente'){
            skip$clickElement()
          } 
        }
        
        tabla_establecimientos_final = rbind(tabla_establecimientos_final,tabla_estab)
        
        for (num in 1:length(n_pag)) {
          drive_remote$goBack()
        }
      } else {
        tabla <- tabla[[1]]
        tabla_estab = data.frame(id,tabla)
        colnames (tabla_estab) <- c("RUC", "CODIGO", "TIPO_ESTABLECIMIENTO", "DIRRECION", "ACTIVIDAD_ECONOMICA")
        
        tabla_establecimientos_final = rbind(tabla_establecimientos_final,tabla_estab)
      }
    }
    
    
  }
    
  drive_remote$goBack()
  Sys.sleep(5)
  
  drive_remote$refresh

  drive_remote$navigate(url)
  
}

drive_remote$close()

system("taskkill /im java.exe /f")

write.csv(tabla_empresas_final,"D:\\4. WEBSCRAPING\\R\\SUNAT\\BD_empresas.csv", row.names = FALSE)
write.csv(tabla_trabajadores_final,"D:\\4. WEBSCRAPING\\R\\SUNAT\\BD_trabajadores.csv", row.names = FALSE)
write.csv(tabla_establecimientos_final,"D:\\4. WEBSCRAPING\\R\\SUNAT\\BD_sedes.csv", row.names = FALSE)
write.csv(tabla_representantes_final,"D:\\4. WEBSCRAPING\\R\\SUNAT\\BD_representantes.csv", row.names = FALSE)
