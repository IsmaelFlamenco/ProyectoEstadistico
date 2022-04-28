#importar base de datos y libreria#
library("tidyverse")
library("lubridate")
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
#* en formato
# test2021 <- test2021 %>%
#   mutate(
#     TiempoCargaAcademica = seconds_to_period(difftime(Proceso2, Proceso1, units = "secs")),
#     TiempoCalculoCobro = seconds_to_period(difftime(Proceso4, Proceso3, units = "secs")),
#     TiempoPago = seconds_to_period(difftime(if_else(is.na(Proceso6), Proceso6bis, Proceso6), Proceso5, units = "secs")),
#     TiempoConfirmacionInscripcion  = seconds_to_period(difftime(Proceso7,if_else(is.na(Proceso6),Proceso6bis,Proceso6),units = "secs")),
#     TiempoTotal = seconds_to_period(difftime(Proceso7, Proceso1, units = "secs"))
#   )
# 
# test2022 <- test2022 %>%
#   mutate(
#     TiempoCargaAcademica = seconds_to_period(if_else(is.na(Proceso0),difftime(Proceso2, Proceso1, units = "secs"), as.difftime(0, units = "secs"))),
#     TiempoCalculoCobro = seconds_to_period(difftime(Proceso4, Proceso3, units = "secs")),
#     TiempoPago = seconds_to_period(difftime(if_else(is.na(Proceso6bis), Proceso6, Proceso6bis), Proceso5, units = "secs")),
#     TiempoConfirmacionInscripcion  = seconds_to_period(difftime(Proceso7,if_else(is.na(Proceso6),Proceso6bis,Proceso6),units = "secs")),
#     TiempoTotal = seconds_to_period(difftime(Proceso7, Proceso1, units = "secs"))
#   )

#*en horas*#
test2021 <- test2021 %>%
  mutate(
    TACA = round(difftime(Proceso2, Proceso1, units = "hours"),3),
    TSCC = round(difftime(Proceso3,Proceso2,units = "hours"),3),
    TACC = round(difftime(Proceso4, Proceso3, units = "hours"),3),
    TCPD = round(difftime(if_else(is.na(Proceso6bis), Proceso6, Proceso6bis), Proceso5, units = "hours"),3),
    TIF  = round(difftime(Proceso7,if_else(is.na(Proceso6),Proceso6bis,Proceso6),units = "hours"),3),
    TiempoTotal = round(difftime(Proceso7, Proceso1, units = "hours"),3)
  )

test2022 <- test2022 %>%
  mutate(
    TACA = round(if_else(is.na(Proceso0),difftime(Proceso2, Proceso1, units = "hours"), as.difftime(0, units = "hours")),3),
    TSCC = round(difftime(Proceso3,if_else(is.na(Proceso2),Proceso0,Proceso2),units = "hours"),3),
    TACC = round(difftime(Proceso4, Proceso3, units = "hours"),3),
    TCPD = round(difftime(if_else(is.na(Proceso6bis), Proceso6, Proceso6bis), Proceso5, units = "hours"),3),
    TIF  = round(difftime(Proceso7,if_else(is.na(Proceso6),Proceso6bis,Proceso6),units = "hours"),3),
    #TiempoTotal = round(difftime(Proceso7, Proceso1, units = "hours"),3)
  )
#Prueba graficar pareto#

#*idea, usar pivot_longer o crear dos colunmas para poner en una los nombres de 
#*los procesos y en la siguiente su valor en mediana y standard deviation*#
test<-type.convert(test2021[13:18]) 
test <-
  test %>% summarise(
    TACA = mean(abs(TACA)),
    TSCC = mean(abs(TSCC)),
    TACC = mean(abs(TACC)),
    TCPD = mean(abs(TCPD)),
    TIF = mean(abs(TIF)),
    #TiempoTotal = mean(abs(TiempoTotal))
  )

info2021<-test%>%pivot_longer(cols = starts_with("T"),names_to = "Proceso")
rm(test)
test<-type.convert(test2022[14:18]) 
test <-
  test %>% summarise(
    TACA = mean(abs(TACA)),
    TSCC = mean(abs(TSCC)),
    TACC = mean(abs(TACC)),
    TCPD = mean(abs(TCPD)),
    TIF = mean(abs(TIF)),
    #TiempoTotal = mean(abs(TiempoTotal))
  )

info2022<-test%>%pivot_longer(cols = starts_with("T"),names_to = "Proceso")
rm(test)
#agrupar
info2021<-info2021%>%arrange(desc(value))
info2022<-info2022%>%arrange(desc(value))
#Gráficas pendiente#
barplot(
  info2021$value, #datos
  main = "Inscripción 2021",
  xlab = "Pasos",
  ylab = "HORAS",
  col = rainbow(6),
  names.arg = info2021$Proceso,
  beside = TRUE
)
barplot(
  info2022$value, #datos
  main = "Inscripción 2022",
  xlab = "Pasos",
  ylab = "HORAS",
  col = rainbow(6),
  names.arg = info2022$Proceso,
  beside = TRUE
)
#*cual proceso te toma mas tiempo
#par matricula cual proceso es mas largo
#pareto
#nuevo campo en el registro y evaluar el mayor
#ejemplo pareto
tabla = table(info2021$Proceso)
pareto.chart(tabla,col=rainbow(length(tabla)),main = "Inscripción")