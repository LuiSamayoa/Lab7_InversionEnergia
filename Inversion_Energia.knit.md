---
title: "Inversion_Energia"
author: "Luis Javier Samayoa"
date: "11/10/2021"
output: html_document
---

###Arreglos de la base de Datos




```r
data <- read_csv("c1.csv")
```

```
## New names:
## * `` -> ...23
## * `` -> ...24
## * `` -> ...25
## * `` -> ...26
## * `` -> ...27
## * ...
```

```
## Rows: 263725 Columns: 28
```

```
## -- Column specification --------------------------------------------------------
## Delimiter: ","
## chr (17): Fecha, Camion_5, Pickup, Moto, Cod, factura, directoCamion_5, dire...
## dbl  (5): ID, origen, Lat, Long, height
## lgl  (6): ...23, ...24, ...25, ...26, ...27, ...28
```

```
## 
## i Use `spec()` to retrieve the full column specification for this data.
## i Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
names(data)[3]<-"Camion"
names(data)[11]<-"directoCamion"
names(data)[14]<-"fijoCamion"
```



```r
data <- data %>% 
  pivot_longer(c("Camion","Pickup","Moto"),names_to = "Transporte", values_to = "Costo_Total")

data <- data %>% 
  pivot_longer(c("directoCamion","directoPickup","directoMoto"),names_to = "direto_tipo", values_to = "Costo_Directo")

data <- data %>% 
  pivot_longer(c("fijoCamion","fijoPickup","fijoMoto"),names_to = "fijo_tipo", values_to = "Costo_Fijo")
```



```r
data[data== "Q-"]<-NA

data <- data[!is.na(data$Costo_Total),]
data <- data[!is.na(data$Costo_Directo),]
data <- data[!is.na(data$Costo_Fijo),]

data<-data %>% 
  select(-direto_tipo,-fijo_tipo,-...23,-...24,-...25,-...26,-...27,-...28)
```


```r
data<- data %>% 
  pivot_longer(c("5-30","30-45","45-75","75-120","120+"),names_to = "Distancia", values_to = "X" )
```


```r
data <- data[!is.na(data$X),]

data<-data %>% 
  select(-X)
```


```r
data2 <- data
```



```r
data2$factura <- str_remove(data2$factura, "Q")
data2$Costo_Directo<-str_remove(data2$Costo_Directo, "Q")
data2$Costo_Fijo<-str_remove(data2$Costo_Fijo, "Q")
data2$Costo_Total<-str_remove(data2$Costo_Total, "Q")
```


```r
data2$factura <- as.numeric(data2$factura)
data2$Costo_Directo<-as.numeric(data2$Costo_Directo)
data2$Costo_Fijo<-as.numeric(data2$Costo_Fijo)
data2$Costo_Total<-as.numeric(data2$Costo_Total)
```


```r
data2$Fecha<- dmy(data2$Fecha)
```


### Numeros de ID


```r
no_id<-data2 %>% 
  count(ID)
```

###clase de trabajo


```r
tipo_cod<-data2 %>% 
  count(Cod)
```

###Numero de Centros de Distribucion


```r
No_CD<-data2 %>%
  count(origen)
```

###Margen

```r
data2$Ganancia <- data2$factura-data2$Costo_Total

data2$Margen <- data2$Ganancia/data2$factura

data2$Margen <- formattable::percent(data2$Margen)
```

###Montos Totales

```r
Totales <- data2 %>% 
  summarise(Facturacion_Ttl=sum(factura),Ganancia_Ttl=sum(Ganancia),CD_Ttl = sum(Costo_Directo),CF_Ttl = sum(Costo_Fijo), CostoTtl=sum(Costo_Total))

Totales$MargenTtl<-Totales$Ganancia_Ttl / Totales$Facturacion_Ttl

Totales$MargenTtl<-formattable::percent(Totales$MargenTtl)
```


###Datos Por Mes

```r
Fact_Mensual <- data2 %>% 
  group_by(month(Fecha)) %>% 
  summarise(FacturaMensual=sum(factura),GananciaMensual=sum(Ganancia), CT_mensual=sum(Costo_Total),CF_Mensual=sum(Costo_Fijo),CD_Mensual=sum(Costo_Directo))

Fact_Mensual$Margen_Mensual<-Fact_Mensual$GananciaMensual / Fact_Mensual$FacturaMensual

Fact_Mensual$Margen_Mensual<-formattable::percent(Fact_Mensual$Margen_Mensual)
```


```r
Fact_Mensual %>%
  tail(12) %>% 
  ggplot( aes(x=`month(Fecha)`, y=FacturaMensual)) +
    geom_line( color="grey") +
    geom_point(shape=21, color="black", fill="#5fb8de", size=6) +
    xlab("MES")+
    ylab("Ingreso Mensual")+
    scale_x_continuous(breaks = seq(1,12, by = 1))+
    ggtitle("Ingresos Mensuales")
```

<img src="Inversion_Energia_files/figure-html/unnamed-chunk-16-1.png" width="672" />


```r
Fact_Mensual %>%
  tail(12) %>% 
  ggplot( aes(x=`month(Fecha)`, y=GananciaMensual)) +
    geom_line( color="grey") +
    geom_point(shape=21, color="black", fill="#5fb8de", size=6) +
    xlab("MES")+
    ylab("Ganancia Mensual")+
    scale_x_continuous(breaks = seq(1,12, by = 1))+
    ggtitle("Ingresos Mensuales")
```

<img src="Inversion_Energia_files/figure-html/unnamed-chunk-17-1.png" width="672" />


```r
Fact_Mensual %>%
  tail(12) %>% 
  ggplot( aes(x=`month(Fecha)`, y=CT_mensual)) +
    geom_line( color="grey") +
    geom_point(shape=21, color="black", fill="#5fb8de", size=6) +
    xlab("MES")+
    ylab("Costo Total Mensual")+
    scale_x_continuous(breaks = seq(1,12, by = 1))+
    ggtitle("Costos Mensuales")
```

<img src="Inversion_Energia_files/figure-html/unnamed-chunk-18-1.png" width="672" />

###Tabla por trabajo

```r
Trabajo <- (data2 %>%
                 group_by(Cod)%>%
                 summarise(costof=sum(Costo_Fijo),costod=sum(Costo_Directo),Fact_Trabajo=sum(factura))) 

Trabajo$CT_TipTrabajo<-Trabajo$costod + Trabajo$costof

Trabajo$G_TipTrabajo<- Trabajo$Fact_Trabajo - Trabajo$CT_TipTrabajo

Trabajo$Margen_TipTrabajo<- Trabajo$G_TipTrabajo / Trabajo$Fact_Trabajo

Trabajo$Margen_TipTrabajo <- formattable::percent(Trabajo$Margen_TipTrabajo,2)
```

###Cod por centro

```r
data2$origen<-as.character(data2$origen)

Cod_Centro <- data2 %>%
  group_by(origen) %>% 
  summarise(Cod) %>% 
  count(Cod)
```

```
## `summarise()` has grouped output by 'origen'. You can override using the `.groups` argument.
```


```r
Centro_Cod_mes<-data2 %>% 
  group_by(month(Fecha)) %>% 
  summarise(Cod) %>% 
  count(Cod)
```

```
## `summarise()` has grouped output by 'month(Fecha)'. You can override using the `.groups` argument.
```



```r
Centro_Cod_mes %>% 
  ggplot(aes(x=`month(Fecha)`, y = n, color = Cod))+
  scale_x_continuous(breaks = seq(1,12, by = 1))+
  geom_line()+
  theme_ipsum()+
  xlab("Mes")+
  ylab("Numero de Servicios")+
  ggtitle("Servicios Mensuales")
```

```
## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family not
## found in Windows font database

## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family not
## found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family not
## found in Windows font database

## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family not
## found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

<img src="Inversion_Energia_files/figure-html/unnamed-chunk-22-1.png" width="672" />





```r
ggplot(Cod_Centro, aes(fill=Cod, y=Cod_Centro$n, x=Cod_Centro$origen)) + 
  geom_bar(position="stack", stat="identity")+
  xlab("Origen")+
  ylab("Numero de servicios")+
  ggtitle("Servicios por Centro")
```

<img src="Inversion_Energia_files/figure-html/unnamed-chunk-23-1.png" width="672" />


###Transporte

```r
Trans <- data2 %>%
  group_by(Transporte) %>% 
  summarise(Cod) %>% 
  count(Cod) %>% 
  arrange(-n)
```

```
## `summarise()` has grouped output by 'Transporte'. You can override using the `.groups` argument.
```


ggplot(Trans, aes(fill=Transporte, x = Cod, y = n ))+
  geom_bar(position="stack", stat="identity")+
   xlab("Servicio")+
  ylab("Numero de Servicios")+
  ggtitle("Serivicios y Tranporte")



###Postes

```r
Mant_Postes<- data2 %>% 
  group_by(ID) %>% 
  summarise(Manteniminetos=n())

Postes_Mes<-data2 %>% 
  group_by(month(Fecha)) %>% 
  summarise(Postes=n())
```


ggplot(Postes_Mes, aes( x = Postes_Mes$`month(Fecha)`, y = Postes_Mes$Postes ))+
  geom_bar(stat="identity", color="#eb7a34", fill="#eb7a34")+
  scale_x_continuous(breaks = seq(1,12, by = 1))+
   xlab("Mes")+
  ylab("Postes Atendidos")+
  ggtitle("Postes Atendidos por Mes")



###Pareto 

```r
Cod_Fact<-data2 %>% 
  group_by(Cod) %>% 
  summarise(Fact=sum(factura)) %>% 
  arrange(Fact)

Cod_Fact$Acumulado <- cumsum(Cod_Fact$Fact)

Cod_Fact$Porcentaje <- Cod_Fact$Acumulado/max(Cod_Fact$Acumulado)

Cod_Fact$Porcentaje<-formattable::percent(Cod_Fact$Porcentaje)
```




library(ggQC)

ggplot(Cod_Fact, aes(x=Cod_Fact$Cod, y=Cod_Fact$Fact)) +
 stat_pareto(point.color = "red",
             point.size = 3,
             line.color = "black",
             #size.line = 1,
             bars.fill = c("blue", "orange"),)



```r
Trayecto_centro <- data2 %>% 
  group_by(origen) %>% 
  summarise(Distancia) %>% 
  count(Distancia)
```

```
## `summarise()` has grouped output by 'origen'. You can override using the `.groups` argument.
```



ggplot(Trayecto_centro, aes(fill=Distancia, x = origen, y = n ))+
  geom_bar(position="stack", stat="identity")+
   xlab("Origen")+
  ylab("Numero de Servicios")+
  ggtitle("Distancia de servicios")


