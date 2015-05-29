### generate tags for subsetting data ####

grain.tags <- list("perennial W/1 grain")
wheat.tags <- list("perennial W/1 wheat","perennial W/1 triticum")
pp.tags <- list('perennial W/1 pigeonpea','perennial W/1 "pigeon pea"','perennial W/1 cajanus','"long duration" W/1 pigeonpea',
                '"long duration" W/1 "pigeon pea"','"long duration" W/1 cajanus','ratoon\\* W/5 pigeonpea','ratoon\\* W/5 "pigeon pea"','ratoon\\* W/5 cajanus')
rice.tags <- list("perennial W/1 rice","perennial W/1 oryza","ratoon\\* W/5 rice","ratoon\\* W/5 oryza")
sorghum.tags <- list("perennial W/1 sorghum","ratoon\\* W/5 sorghum")
rye.tags <- list("perennial W/1 'rye' AND NOT 'ryegrass' AND NOT 'rye-grass' AND NOT 'rye grass'", "perennial W/1 secale")

all.tags <- lapply(ls(pattern="*tags"), function(x) get(x))

crop.names <- c("grain","pp","rice","rye","sorghum","wheat")
names(all.tags) <- crop.names

rm(grain.tags,wheat.tags,pp.tags,rice.tags,sorghum.tags,rye.tags)
