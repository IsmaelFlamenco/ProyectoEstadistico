#importar base de datos y libreria#
library("tidyverse")
library("lubridate")
RegisterData <-
  read_csv("rawTimes.csv", locale = locale(encoding =  "windows-1252")) #nativo de windows para tildes
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

#Ordenar datos *arreglar problema de formato - listo*
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
    !is.na(Proceso5)
  )

test2022<-test2022%>%
  filter(
    !is.na(Proceso7),
    !is.na(Proceso1),
    !is.na(Proceso5)
  )

#* operaciones *#
test2021 <- test2021 %>%
  mutate(
    TiempoCargaAcademica = seconds_to_period(difftime(Proceso2, Proceso1, units = "secs")),
    TiempoCalculoCobro = seconds_to_period(difftime(Proceso4, Proceso3, units = "secs")),
    TiempoPago = seconds_to_period(difftime(if_else(is.na(Proceso6), Proceso6bis, Proceso6), Proceso5, units = "secs")),
    TiempoTotal = seconds_to_period(difftime(Proceso7, Proceso1, units = "secs"))
  )

test2022 <- test2022 %>%
  mutate(
    TiempoCargaAcademica = seconds_to_period(if_else(is.na(Proceso0),difftime(Proceso2, Proceso1, units = "secs"), as.difftime(0, units = "secs"))),
    TiempoCalculoCobro = seconds_to_period(difftime(Proceso4, Proceso3, units = "secs")),
    TiempoPago = seconds_to_period(difftime(if_else(is.na(Proceso6bis), Proceso6, Proceso6bis), Proceso5, units = "secs")),
    TiempoTotal = seconds_to_period(difftime(Proceso7, Proceso1, units = "secs"))
  )

#Gráficas pendiente#
plot(proof)
