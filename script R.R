#importar base de datos y libreria#
setwd("C:/Users/isma/Desktop/proyectoIsaac")#modificar la ruta donde tengan el archivo rawTimes.csv
library(tidyverse)
library(lubridate)
RegisterData <-
  read_csv("rawTimes.csv", locale = locale(encoding =  "windows-1252")) #nativo de windows para tildes
#*LIMPIAR LOS DATOS*#
#FUNCION#
#idea Eliminar 1er elemento si la lista tiene mas de 1 elemento usando funcion propia
delete <-
  function(x) {
    #tomar el ultimo valor con last() y ponerlo en primer indice [[, 1
    if (length(x[]) > 1) {
      #print(last(x[]))
      x[[1]] = last(x[])
    } else{
      x[1]
    }
  }

#FILTRAR DATOS#
data2021 <-
  RegisterData %>% filter(
    Campus == "ULV",
    Facultad != "NM",
    Facultad != "POSTGRADO",
    HoraRegistro < as.POSIXct("2022-01-01")
  ) %>% arrange(by_group = Matricula)
data2022 <-
  RegisterData %>% filter(
    Campus == "ULV",
    Facultad != "NM",
    Facultad != "POSTGRADO",
    HoraRegistro > as.POSIXct("2022-01-01")
  ) %>% arrange(by_group = Matricula)

#Ordenar datos *arreglar problema de formato - listo*
proof2021 <- data2021 %>%
  select(-...1) %>%
  pivot_wider(names_from = Proceso,
              values_from = HoraRegistro,
              values_fn = delete)


proof2022 <- data2022 %>%
  select(-...1) %>%
  pivot_wider(names_from = Proceso,
              values_from = HoraRegistro,
              values_fn = delete)

#Renombrar
proof2021 <-
  proof2021 %>% rename(
    Proceso1 = `Solicitud de Carga Acad閙ica`,
    Proceso2 = `Asignaci髇 de Carga Acad閙ica`,
    Proceso3 = `Solicitud de C醠culo de Cobro`,
    Proceso4 = `Asignaci髇 de C醠culo de Cobro`,
    Proceso5 = `Pago Deposito/Tranferencia`,
    Proceso6 = `Confirmaci髇 Pago Deposito`,
    Proceso6bis = `Confirmaci髇 de Pago en L韓ea`,
    Proceso7 = `Inscripci髇 Finalizada`
  )

proof2022 <-
  proof2022 %>% rename(
    Proceso0 = `Asignaci髇 de Carga Acad閙ica Autom醫ica`,
    Proceso1 = `Solicitud de Carga Acad閙ica`,
    Proceso2 = `Asignaci髇 de Carga Acad閙ica`,
    Proceso3 = `Solicitud de C醠culo de Cobro`,
    Proceso4 = `Asignaci髇 de C醠culo de Cobro`,
    Proceso5 = `Pago Deposito/Tranferencia`,
    Proceso6 = `Confirmaci髇 Pago Deposito`,
    Proceso6bis = `Confirmaci髇 de Pago en L韓ea`,
    Proceso7 = `Inscripci髇 Finalizada`
  )


#Limpiar datos, excluir los que no van a servir#
proof2021<-proof2021%>%
  filter(
    !is.na(Proceso7),
    !is.na(Proceso1),
    !is.na(Proceso5)
  )

proof2022<-proof2022%>%
  filter(
    !is.na(Proceso7),
    !is.na(Proceso1),
    !is.na(Proceso5)
  )

#* operaciones de resta de procesos *#
proof<-proof2021%>%
  mutate(TiempoCargaAcademica <- seconds_to_period(difftime(Proceso2,Proceso1,units = "secs")),
         TiempoCalculoCobro <- seconds_to_period(difftime(Proceso4,Proceso3,units = "secs")),
         TiempoPago <- seconds_to_period(difftime(if_else(is.na(Proceso6),Proceso6bis,Proceso6),Proceso5,units = "secs")),
         TiempoTotal <- seconds_to_period(difftime(Proceso7,Proceso1,units = "secs")))
proof2<-proof2022%>%
  mutate(TiempoCargaAcademica = seconds_to_period(difftime(Proceso2,if_else(is.na(pri)),units = "secs")),
         TiempoCalculoCobro = seconds_to_period(difftime(Proceso4,Proceso3,units = "secs")),
         TiempoPago = seconds_to_period(difftime(if_else(is.na(Proceso6),Proceso6bis,Proceso6),Proceso5,units = "secs")),
         TiempoTotal = seconds_to_period(difftime(Proceso7,Proceso1,units = "secs")))

#Pruebas#
proof <- proof2021[5:12]
proof <- proof2022[5:13]

#Esto no sirve#
data.frame(lapply(proof$`Confirmaci髇 Pago Deposito`, delete))

#*Con esto ya casi jala*#
proof <- proof %>%
  mutate(`Asignaci髇 de Carga Acad閙ica` = lapply(`Asignaci髇 de Carga Acad閙ica`, delete))

#arreglar formato de tiempo
proof2021 = type.convert(proof2021)
proof2022 = type.convert(proof2022)


  



    
# proof <- proof2021 %>% # no sirve, toma el ultimo valor y lo replica
#   mutate(`Asignaci贸n de Carga Acad茅mica` = last(x))
#
# proof <-
#   proof2021 %>% #solo consigue tomar el primer valor de cada registro
#   mutate(`Confirmaci贸n Pago Deposito` = sapply(`Confirmaci贸n Pago Deposito`, `[[`, 1))
#
# proof <- proof2021 %>% #no encuentro como aplicar
#   mutate(`Confirmaci贸n Pago Deposito` = apply(`Confirmaci贸n Pago Deposito`, 1, `[[`, 1))