
source('C:/Users/geret.depiper/Documents/R/dbconnect_info.R')

library(plyr)
library(rgdal)
library(rgeos)
library(sf)

library(odbc)
library(dbplyr)
library(dplyr)

library(ecodata)

con <-dbConnect(drv=odbc::odbc(),dsn="sole",Pwd=password, Uid=username)
EPUs <- ecodata::epu_sf

START.YEAR = 1996
END.YEAR = 2018
VTR <- NULL
for (YEAR in START.YEAR:END.YEAR) {
sq = paste0("SELECT distinct
	TO_CHAR(t.datelnd1,'YYYY') year,
		t.datelnd1 as dateland,
		t.datesail as datesail,
		(g.CLATDEG+g.CLATMIN/60+NVL(g.CLATSEC,0)/3600) as LAT,
	-(g.CLONDEG+g.CLONMIN/60+NVL(g.CLONSEC,0)/3600) as LON,
		g.serial_num,
		t.permit,
		t.hullnum
		from vtr.veslog",YEAR,"g g, vtr.veslog",YEAR,"t t WHERE 
		(t.tripcatg=1 or t.tripcatg=4) and g.tripid = t.tripid;",sep="")

TEMP = odbc::dbGetQuery(con ,sq)
VTR <- rbind(VTR,TEMP)  
rm(TEMP,sq)
}

save(VTR, file="X:/gdepiper/EcoAP/Ecosystem_Synthesis/RAW_VTR.Rdata")

sq1 = "SELECT DISTINCT len, gtons, vhp, hull_id, vp_num,ap_year from permit.vps_Vessel where
ap_year > 1995;"
VESSEL  = odbc::dbGetQuery(con ,sq1)

save(VESSEL, file="X:/gdepiper/EcoAP/Ecosystem_Synthesis/RAW_VESSEL.Rdata")

VESSEL <- aggregate(cbind(LEN,GTONS,VHP)~HULL_ID+VP_NUM+AP_YEAR,FUN=mean,data=VESSEL)

VTR_VESSEL <- merge(VTR,VESSEL, by.x=c('PERMIT','HULLNUM','YEAR'),
                    by.y=c('VP_NUM','HULL_ID','AP_YEAR'), all.x=TRUE, all.y=FALSE)

VTR_VESSEL$TRIPLENGTH <- as.numeric(VTR_VESSEL$DATELAND-VTR_VESSEL$DATESAIL, units="hours")

VTR_VESSEL <- VTR_VESSEL %>% filter(!is.na(LAT)) %>% filter(!is.na(LON))


#-------------------------------
# CONVERT TO SPATIAL POINTS DF
#-------------------------------

crs = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")

coordinates(VTR_VESSEL) <- c('LON','LAT')

VTR_VESSEL <- sf::st_as_sf(VTR_VESSEL,coords=c("LON","LAT"))

EPUs = sf::st_transform(EPUs, crs = crs)

# POINTS IN POLYGONS OPERATION

VTR_VESSEL@proj4string = crs

VTR_VESSEL = st_join(VTR_VESSEL, EPUs)

EPU_EFFORT <- aggregate(TRIPLENGTH~YEAR+EPU,data=
                          VTR_VESSEL[which(!is.na(VTR_VESSEL$EPU)),],FUN=sum)
save(EPU_EFFORT, file="X:/gdepiper/EcoAP/Ecosystem_Synthesis/EPU_Effort.Rdata")