
#######################################################
#Klucz API Natalia: "lJg9TVrDZHlWXUEuzOkXv6PUfvzE1UMT"
#######################################################
#######################################################
#Klucz API Emilia: "qTWnrgxjXF9wriXvotOVPTK0rE25AfTd"
#######################################################
#######################################################
#Klucz API Zuzia: "AZJdLwwDebSXMDIGXZbfUZ1yBVtQoZsz"
#######################################################
#Klucz ostatecznego ratunku: "D8rexBz8zSNh3MGCn3bJLRkeMlsaV6DQ"

#Utworzenie struktury danych, która będzie przechowywała dane o zanieczyszczeniach 
#pochodzące z czujników airy

#Pobieranie potrzebnych bibliotek
#install.packages("httr") 
library(httr)
#install.packages("jsonlite")
library(jsonlite) 

#Pobranie danych w odległości 15km od ratusza w krakowie
r <- GET("https://airapi.airly.eu/v2/installations/nearest?lat=50.0617022&lng=19.9373569&maxDistanceKM=15&maxResults=-1", 
         add_headers(apikey = "qTWnrgxjXF9wriXvotOVPTK0rE25AfTd", Accept = "application/json")
)

#Przejście do listy
jsonRespText<-content(r,as="text")
dane<-fromJSON(jsonRespText)
save(dane,file=("dane_dzien_pora.Rdata"))

#Utworzenie ramki danych zawierającą id oraz lokalizację czujników (długość i szerokość 
#geograficzna, wysokości n.p.m.)
longitude<- dane$location$longitude
latitude<- dane$location$latitude
id<-dane$id
elevation<-dane$elevation

dane<-data.frame(longitude,latitude,id, elevation)

#Utworzenie obiektu przestrzennego ppp

#Załadowanie potrzebnych bibliotek
#install.packages("sp")
library(sp)
#install.packages("spatstat")
library(spatstat)

#utworzenie obiektu data_UTM
data_spat<-data.frame(lon=dane$longitude, lat=dane$latitude, elev=dane$elevation, id=dane$id)
#określenie koordynat
coordinates(data_spat) <- ~lon+lat
#określenie układu
proj4string(data_spat) <- CRS("+proj=longlat +datum=WGS84")
data_spat
data_UTM <- spTransform(data_spat, CRS("+proj=utm +zone=34
+datum=WGS84")) #konwersja do UTM

#utworzenie obiektu krakowUTM
#install.packages("sf")
library(sf)
dzielnice<-st_read("dzielnice_Krakowa/dzielnice_Krakowa.shp") 
dzielniceWGS84<-st_transform(dzielnice,crs = 4326) 
krakowWGS84<-st_union(dzielniceWGS84) 
krakowUTM<-st_transform(krakowWGS84,CRS("+proj=utm +zone=34
+datum=WGS84"))

#utworzenie obiektu ppp z danymi id czujnika i wysokościowymi
XY<-coordinates(data_UTM)
data_ppp_id<-ppp(x=XY[,1],y=XY[,2],marks=data.frame(elev=data_UTM$elev,id=data_UTM$id),window=as.owin(krakowUTM))

#pobranie danych z serwisu airly dla wybranych czujników znajdujących się w obrębie Krakowa
n_id<-length(data_ppp_id$marks$id)
id<-data_ppp_id$marks$id

#Stworzenie listy, która będzie przechowywała dane dla poszczególnych czujników
list_data <- vector(mode = "list", length = n_id)

for (i in seq(1,n_id)) {
  
  #Stworzenie ciągu znaków określajacy adres, pod kótrym znajdują się pomiary z czujnika
  str<-paste("https://airapi.airly.eu/v2/measurements/installation?installationId=",id[i],sep="")
  #Pobranie danych z adresu
  r <- GET(url=str,add_headers(apikey = "D8rexBz8zSNh3MGCn3bJLRkeMlsaV6DQ", Accept = "application/json"))
  #Przejście z formatu r na json i z json na tekst
  jsonRespText<-content(r,as="text")
  inst<-fromJSON(jsonRespText)
  
  list_data[[i]]<-inst 
  
}

#na wszelki
save(list_data,file="list_data.Rdata") 

#Wybranie potrzebnych danych, czyli aktualnego stężenia pyłu PM2.5 

#Utworzenie zmiennej która będzie przechowywać wartości stężenia pyłu
current<-rep(NA,n_id)

#Wyodbrębienie danych PM2.5 z czujników
for (i in seq(1,n_id)) {
  print(i)
  logic<-(list_data[[i]]$current$values$name=="PM25")
  if (sum(logic)==1)
    current[i]<-list_data[[i]]$current$values[logic,2]
}

temperature<-rep(NA,n_id)

#Wyodbrębienie danych temperaturowych z czujników
for (i in seq(1,n_id)) {
  print(i)
  logic<-(list_data[[i]]$current$values$name=="TEMPERATURE")
  if (sum(logic)==1)
    temperature[i]<-list_data[[i]]$current$values[logic,2]
}

#Przekształcenie obiektu data_ppp_id do obiektu spdf (do obsługi autoKrige())
#install.packages("https://cran.r-project.org/src/contrib/Archive/maptools/maptools_1.1-8.tar.gz", repo=NULL,type="source")> library(maptools) 
library(maptools)

data_spdf<-as.SpatialPointsDataFrame.ppp(data_ppp_id)
coordinates(data_spdf)

#dla PM2.5
data_spdf$current<-current
miss <- is.na(data_spdf$current)
#install.packages("automap")
library(automap)

#Standard error KRIGING
pm25_ste <- autoKrige(current ~ 1, input_data = data_spdf[!miss,], model="Ste")
plot(pm25_ste$krige_output[1],main="PM 2.5")
points(data_ppp_id[!miss,],pch="*",col="White")
plot(Window(data_ppp_id),add=TRUE)
plot(pm25_ste)

#mat KRIGING
pm25_mat <- autoKrige(current ~ 1, input_data = data_spdf[!miss,], model="Mat")
plot(pm25_mat$krige_output[1],main="PM 2.5")
points(data_ppp_id[!miss,],pch="*",col="White")
plot(Window(data_ppp_id),add=TRUE)
plot(pm25_mat)

#spherical KRIGING
pm25_sph <- autoKrige(current ~ 1, input_data = data_spdf[!miss,], model="Sph")
plot(pm25_sph$krige_output[1],main="PM 2.5")
points(data_ppp_id[!miss,],pch="*",col="White")
plot(Window(data_ppp_id),add=TRUE)
plot(pm25_sph)





#ste KRIGING
temp_ste <- autoKrige(temperature ~ 1, input_data = data_spdf[!misss,], model="Ste")
plot(temp_ste$krige_output[1],main="Temperatura")
points(data_ppp_id[!misss,],pch="*",col="White")
plot(Window(data_ppp_id),add=TRUE)
plot(temp_ste)

#mat KRIGING
temp_mat <- autoKrige(temperature ~ 1, input_data = data_spdf[!misss,], model="Mat")
plot(temp_mat$krige_output[1],main="Temperatura")
points(data_ppp_id[!misss,],pch="*",col="White")
plot(Window(data_ppp_id),add=TRUE)
plot(temp_mat)

#sph KRIGING
temp_sph <- autoKrige(temperature ~ 1, input_data = data_spdf[!misss,], model="Sph")
plot(temp_sph$krige_output[1],main="Temperatura")
points(data_ppp_id[!misss,],pch="*",col="White")
plot(Window(data_ppp_id),add=TRUE)
plot(temp_sph)


######bardzo ładna mapka######

bound<-st_as_sf(krakowUTM)
plot(bound)

#macierz na współrzędne punktów konturu Krakowa
coord<-as.data.frame(st_coordinates(krakowUTM))

#Tworzymy siatkę - prostokąt obejmujący obszar Krakowa
#Określamy współrzędne naroży prostokąta
left_down<-c( min(coord$X), min(coord$Y))
right_up<-c( max(coord$X), max(coord$Y))

#Ustalenie rozmiaru oczka siatki w [m]
size<-c(100,100)

#Obliczenie liczby oczek siatki przypadających na dl. i szer. prostokąta
points<- (right_up-left_down)/size
num_points<-ceiling(points)

#Utworzenie siatki
grid <- GridTopology(left_down, size,num_points)

#Konwersja siatki do odpowiedniego formatu
gridpoints <- SpatialPoints(grid, proj4string = CRS("+proj=utm +zone=34
+datum=WGS84"))
plot(gridpoints)

#Przycięcie siatki do konturu
#install.packages("tmaptools")
library(tmaptools)
#konwersja do formatu w którym działa crop_shape()
g<-st_as_sf(gridpoints)
cg<-crop_shape(g,bound,polygon = TRUE)
#konwersja do st
spgrid <- SpatialPixels(as_Spatial(cg)) 
plot(spgrid)

#dla PM2.5
#narysowana mapka

#Standard error KRIGING
pm25_ste <- autoKrige(current ~ 1, input_data = data_spdf[!miss,],new_data=spgrid, model="Ste")
plot(pm25_ste$krige_output[1],main="PM 2.5")
points(data_ppp_id[!miss,],pch="*",col="White")
plot(Window(data_ppp_id),add=TRUE)
plot(pm25_ste)

#mat KRIGING
pm25_mat <- autoKrige(current ~ 1, input_data = data_spdf[!miss,],new_data=spgrid, model="Mat")
plot(pm25_mat$krige_output[1],main="PM 2.5")
points(data_ppp_id[!miss,],pch="*",col="White")
plot(Window(data_ppp_id),add=TRUE)
plot(pm25_mat)

#spherical KRIGING
pm25_sph <- autoKrige(current ~ 1, input_data = data_spdf[!miss,],new_data=spgrid, model="Sph")
plot(pm25_sph$krige_output[1],main="PM 2.5")
points(data_ppp_id[!miss,],pch="*",col="White")
plot(Window(data_ppp_id),add=TRUE)
plot(pm25_sph)


#dla temperatury
data_spdf$temperature<-temperature
misss <- is.na(data_spdf$temperature)
#ste KRIGING
temp_ste <- autoKrige(temperature ~ 1, input_data = data_spdf[!misss,],new_data=spgrid, model="Ste")
plot(temp_ste$krige_output[1],main="Temperatura")
points(data_ppp_id[!misss,],pch="*",col="White")
plot(Window(data_ppp_id),add=TRUE)
plot(temp_ste)

#mat KRIGING
temp_mat <- autoKrige(temperature ~ 1, input_data = data_spdf[!misss,],new_data=spgrid, model="Mat")
plot(temp_mat$krige_output[1],main="Temperatura")
points(data_ppp_id[!misss,],pch="*",col="White")
plot(Window(data_ppp_id),add=TRUE)
plot(temp_mat)

#sph KRIGING
temp_sph <- autoKrige(temperature ~ 1, input_data = data_spdf[!misss,],new_data=spgrid, model="Sph")
plot(temp_sph$krige_output[1],main="Temperatura")
points(data_ppp_id[!misss,],pch="*",col="White")
plot(Window(data_ppp_id),add=TRUE)
plot(temp_sph)
