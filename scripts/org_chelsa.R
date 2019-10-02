#### Sort CHELSA future projections

library(stringr)

mainpath <- "D:/Projects/truffle_sdm/env_data/CHELSA/future/"

raw_list <- list.files("D:/Projects/truffle_sdm/env_data/CHELSA/www.wsl.ch/",
                       pattern="tif$",
                       full.names = TRUE,
                       recursive = TRUE)

get_info <- function(x){
    path <- str_extract(x,"D:.*(?=CHELSA)")
    mod <- str_extract(x,"(?<=mon_).*?(?=_rcp)")
    rcp <- str_extract(x,"rcp[0-9][0-9]")
    bioclim <- as.numeric(str_extract(x,"(?<=nc_)[0-9]{1,2}"))
    yrs <- str_extract(x,"(?<=_)20.*(?=_V)")
    
    pt1 <- paste0(mainpath,yrs,"/")
    pt2 <- paste0(pt1,rcp,"/")
    pt3 <- paste0(pt2,mod,"/")
    print(pt1)
    print(pt2)
    print(pt3)
  
    fname <- paste0(pt3,"CHELSA_",rcp,"_",yrs,"_",mod,"_bioclim_",sprintf("%02d",bioclim),".tif")
    print(fname)
    
    if(!dir.exists(pt1)) dir.create(pt1)
    if(!dir.exists(pt2)) dir.create(pt2)
    if(!dir.exists(pt3)) dir.create(pt3)
    
    cp <- file.copy(x,fname,overwrite = TRUE)
    if(cp) file.remove(x)
   
}

lapply(raw_list,get_info)
