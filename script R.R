#importar base de datos y libreria#
library("tidyverse")
library("qcc")
#library("lubridate")
RegisterData <-
  read_csv("rawTimes.csv", locale = locale(encoding =  "windows-1252")) #native de windows para tildes
#*LIMPIAR LOS DATOS*#

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

#Trasponer proceso con su hora de registro *arreglar problema de formato - listo*
test2021 <- data2021 %>%
  select(-...1) %>%
  pivot_wider(names_from = Proceso,
              values_from = HoraRegistro,
              values_fn = last)


test2022 <- data2022 %>%
  select(-...1) %>%
  pivot_wider(names_from = Proceso,
              values_from = HoraRegistro,
              values_fn = last)

#Renombrar
test2021 <-
  test2021 %>% rename(
    Proceso1 = `Solicitud de Carga Académica`,
    Proceso2 = `Asignación de Carga Académica`,
    Proceso3 = `Solicitud de Cálculo de Cobro`,
    Proceso4 = `Asignación de Cálculo de Cobro`,
    Proceso5 = `Pago Deposito/Tranferencia`,
    Proceso6 = `Confirmación Pago Deposito`,
    Proceso6bis = `Confirmación de Pago en Línea`,
    Proceso7 = `Inscripción Finalizada`
  )

test2022 <-
  test2022 %>% rename(
    Proceso0 = `Asignación de Carga Académica Automática`,
    Proceso1 = `Solicitud de Carga Académica`,
    Proceso2 = `Asignación de Carga Académica`,
    Proceso3 = `Solicitud de Cálculo de Cobro`,
    Proceso4 = `Asignación de Cálculo de Cobro`,
    Proceso5 = `Pago Deposito/Tranferencia`,
    Proceso6 = `Confirmación Pago Deposito`,
    Proceso6bis = `Confirmación de Pago en Línea`,
    Proceso7 = `Inscripción Finalizada`
  )


#Limpiar datos, excluir los que no van a servir#
test2021<-test2021%>%
  filter(
    !is.na(Proceso7),
    !is.na(Proceso1),
    !is.na(Proceso5),
    !is.na(Proceso3)
  )

test2022<-test2022%>%
  filter(
    !is.na(Proceso7),
    !is.na(Proceso1),
    !is.na(Proceso5),
    !is.na(Proceso3)
  )

#* operaciones *#
#en horas#
test2021 <- test2021 %>%
  mutate(
    TACA = round(difftime(Proceso2, Proceso1, units = "hours"),2),
    TSCC = round(difftime(Proceso3,Proceso2,units = "hours"),2),
    TACC = round(difftime(Proceso4, Proceso3, units = "hours"),2),
    TCPD = round(difftime(if_else(is.na(Proceso6bis), Proceso6, Proceso6bis), Proceso5, units = "hours"),2),
    TIF  = round(difftime(Proceso7,if_else(is.na(Proceso6bis),Proceso6,Proceso6bis),units = "hours"),2),
    TiempoTotal = round(difftime(Proceso7, Proceso1, units = "hours"),2)
  )

test2022 <- test2022 %>%
  mutate(
    TACA = round(if_else(is.na(Proceso0),difftime(Proceso2, Proceso1, units = "hours"), as.difftime(0, units = "hours")),2),
    TSCC = round(difftime(Proceso3,if_else(is.na(Proceso2),Proceso0,Proceso2),units = "hours"),2),
    TACC = round(difftime(Proceso4, Proceso3, units = "hours"),2),
    TCPD = round(difftime(if_else(is.na(Proceso6bis), Proceso6, Proceso6bis), Proceso5, units = "hours"),2),
    TIF  = round(difftime(Proceso7,if_else(is.na(Proceso6),Proceso6bis,Proceso6),units = "hours"),2),
    TiempoTotal = round(difftime(Proceso7, Proceso1, units = "hours"),2)
  )

test2021<-type.convert(test2021)
test2022<-type.convert(test2022)
#obtener medias-estandar#
estadistica2021 <-
  test2021 %>% summarise(
    TACAm = mean(abs(TACA)),TACAsd = sd(abs(TACA)),
    TSCCm = mean(abs(TSCC)),TSCCsd = sd(abs(TSCC)),
    TACCm = mean(abs(TACC)),TACCsd = sd(abs(TACC)),
    TCPDm = mean(abs(TCPD)),TCPDsd = sd(abs(TCPD)), 
    TIFm = mean(abs(TIF)),TIFsd = sd(abs(TIF)),
    TTmedia = mean(abs(TiempoTotal)), TTsd = sd(abs(TiempoTotal))
  )

estadistica2022 <-
  test2022 %>% summarise(
    TACAm = mean(abs(TACA)),TACAsd = sd(abs(TACA)),
    TSCCm = mean(abs(TSCC)),TSCCsd = sd(abs(TSCC)),
    TACCm = mean(abs(TACC)),TACCsd = sd(abs(TACC)),
    TCPDm = mean(abs(TCPD)),TCPDsd = sd(abs(TCPD)), 
    TIFm = mean(abs(TIF)),TIFsd = sd(abs(TIF)),
    TTmedia = mean(abs(TiempoTotal)), TTsd = sd(abs(TiempoTotal))
  )

#*  HISTOGRAMA *#
hist(test2021$TiempoTotal, breaks = 20)
hist(test2022$TiempoTotal, breaks = 30)

#* PARETOS 2021, 2022 *#

#caso por caso
test2021 <-
  test2021 %>% mutate(Frecuencia = ifelse(pmax(TACA, TSCC, TACC, TCPD, TIF) ==TACA,"TACA",ifelse(pmax(TSCC, TACC, TCPD, TIF) ==TSCC,"TSCC",ifelse(pmax(TACC, TCPD, TIF) ==TACC,"TACC",ifelse(pmax(TCPD, TIF)==TCPD,"TCPD","TIF")))))
tabla = table(as.factor(test2021$Frecuencia))
pareto.chart(tabla,col=rainbow(length(tabla)),main = "Inscripción 2021", ylab = "Estudiantes")
rm(tabla)

test2022 <-
  test2022 %>% mutate(Frecuencia = ifelse(pmax(TACA, TSCC, TACC, TCPD, TIF) ==TACA,"TACA",ifelse(pmax(TSCC, TACC, TCPD, TIF) ==TSCC,"TSCC",ifelse(pmax(TACC, TCPD, TIF) ==TACC,"TACC",ifelse(pmax(TCPD, TIF)==TCPD,"TCPD","TIF")))))
tabla = table(as.factor(test2022$Frecuencia))
pareto.chart(tabla,col=rainbow(length(tabla)),main = "Inscripción 2022", ylab = "Estudiantes")
rm(tabla)

#por horas
tabla<-estadistica2021%>%select(ends_with("m"))
tabla<-tabla%>%pivot_longer(cols = starts_with("T"),names_to = "Proceso")
x <- round(tabla$value)
names(x)<-tabla$Proceso
pareto.chart(x, col = rainbow(length(x)),main = "Inscripción 2021", ylab = "Horas")
rm(tabla,x)


tabla<-estadistica2022%>%select(ends_with("m"))
tabla<-tabla%>%pivot_longer(cols = starts_with("T"),names_to = "Proceso")
x <- round(tabla$value)
names(x)<-tabla$Proceso
pareto.chart(x, col = rainbow(length(x)),main = "Inscripción 2022", ylab = "Horas")
rm(tabla,x)

#Personas que se tardaron más de 168 horas
pruebamayor2021 <- test2021 %>%
  filter(TiempoTotal>168) %>% 
  arrange(by_group = Matricula)

pruebamayor2022 <- test2022 %>%
  filter(TiempoTotal>168) %>% 
  arrange(by_group = Matricula)

