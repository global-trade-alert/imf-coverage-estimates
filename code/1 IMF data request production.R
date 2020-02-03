library("gtalibrary")
library("xlsx")
library("data.table")

rm(list = ls())
gta_setwd()


## setup
project.folder="0 projects/42 IMF coverage estimates/"
output.folder="submitted 200204/"
output.path=paste0(project.folder, output.folder)

## IMF specs
# Time period: annual, from end 2009 to end 2019.
years=c(2009:2019)
production.start="2008-11-01"
production.end="2019-12-31"


# Indicators:
# import tariffs (import coverage),
# trade defense (import coverage),
# non-tariff import restrictions (import coverage),
# localization requirements (import coverage),
# export restrictions (export coverage), and
# FDI measures (number of measures).

imf.instruments=read.csv(paste0(project.folder, "help files/imf instrument classification.csv"),sep=";", stringsAsFactors = F) ## see Accompanying note in help files
names(imf.instruments)=c("imf.label","intervention.type")

inward.types=c("Import tariffs", "Trade defense","Non-tariff import restrictions", "Localization requirements", "Non-tariff import restrictions")
outward.types=c("Export restrictions")

## further requirements (see Accompanying note in help files)
gta.evaluation="Red"


# Economies: world as a whole, and each G20 separately.
individual.countries=country.names$name[country.names$is.g20==1]
individual.countries.un=country.names$un_code[country.names$is.g20==1]

#################################
## FDI measures (number of measures)
#################################

gta_data_slicer(gta.evaluation = gta.evaluation,
                keep.implementation.na = F,
                intervention.types = imf.instruments$intervention.type[imf.instruments$imf.label=="FDI measures"],
                keep.type = T,
                implementation.period = c(production.start, production.end))

fdi=data.frame()

for(yr in years){
  removal.date=paste(yr,"-12-31",sep="")
  
  fdi.set=subset(master.sliced, 
                 date.implemented<=removal.date &
                (date.removed>=removal.date | is.na(date.removed)))
  
  fdi=rbind(fdi,
            data.frame(country="World total",
                       year=yr,
                       count=length(unique(fdi.set$intervention.id))))
  
  if(length(individual.countries.un)>0){
    
    fdi.yr=aggregate(intervention.id ~ implementing.jurisdiction, subset(fdi.set,
                                                                         i.un %in% individual.countries.un), function(x) length(unique(x)))
    fdi.yr$year=yr
    names(fdi.yr)=c("country", "count","year")
    fdi=rbind(fdi, fdi.yr)
  }
  
  
  rm(fdi.set)
}


rm(master.sliced)
fdi=reshape(fdi, idvar="country", timevar="year", direction="wide")
names(fdi)=gsub("count\\.","", names(fdi))
fdi=fdi[,c("country",years)]
fdi[is.na(fdi)]=0

fdi=fdi[order(fdi$country),]

names(fdi)=c("Implementing country",paste("Number of discriminatory FDI-related interventions in force at the end of ",years,sep=""))

xlsx::write.xlsx(fdi, file=paste0(output.path, "GTA data for the IMF - incl. FDI.xlsx"), row.names = F, sheetName="FDI")


#################################
# import tariffs (import coverage),
# trade defense (import coverage),
# non-tariff import restrictions (import coverage),
# localization requirements (import coverage),
#################################

import.coverage=expand.grid(importer=unique(fdi$`Implementing country`), level=inward.types, stringsAsFactors = F)

## all inward instruments
imf.types=imf.instruments$intervention_type[imf.instruments$aggregate %in% inward.types]

ic.coverage=data.frame()

## per group
for(agg in inward.types){
  print(paste("starting", agg))
  
  imf.types=imf.instruments$intervention.type[imf.instruments$imf.label==agg]
  
  ## World total
  gta_trade_coverage(gta.evaluation = gta.evaluation,
                     affected.flows = "inward",
                     intervention.types=imf.types,
                     keep.type = T,
                     coverage.period = c(min(years),max(years)))
  
  coverage=trade.coverage.estimates[,c(1, seq(4,ncol(trade.coverage.estimates),1))]
  coverage$`Importing country` ="World total"
  rm(trade.coverage.estimates)
  names(coverage)=gsub("Trade coverage estimate for ","",names(coverage))
  coverage$level=agg
  coverage=coverage[,c("Importing country","level",years)]
  names(coverage)=c("importer","level",paste("share.",years,sep=""))
  
  ic.coverage=rbind(ic.coverage, coverage)
  rm(coverage)
  
  print("finished world estimate")
  
  ## individual countries
  if(length(individual.countries.un)>0){
    
    gta_trade_coverage(gta.evaluation = gta.evaluation,
                       affected.flows = "inward",
                       intervention.types=imf.types,
                       keep.type = T,
                       importers=individual.countries,
                       keep.importers=T,
                       group.importers=F,
                       coverage.period = c(min(years),max(years)))
    
    coverage=trade.coverage.estimates[,c(1, seq(4,ncol(trade.coverage.estimates),1))]
    rm(trade.coverage.estimates)
    names(coverage)=gsub("Trade coverage estimate for ","",names(coverage))
    coverage$level=agg
    coverage=coverage[,c("Importing country","level",years)]
    names(coverage)=c("importer","level",paste("share.",years,sep=""))
    
    ic.coverage=unique(rbind(ic.coverage, coverage))
    rm(coverage)
    
  }
  
  print(paste("finished", agg))
}
names(ic.coverage)=c("importer", "level", paste("Share of own imports affected by own import restrictions in ",years,sep=""))

import.coverage=merge(import.coverage, ic.coverage, by=c("importer","level"), all.x=T)
import.coverage[is.na(import.coverage)]=0
import.coverage=import.coverage[order(import.coverage$importer),]

xlsx::write.xlsx(import.coverage, file=paste0(output.path, "GTA data for the IMF - incl. FDI.xlsx"), row.names = F, sheetName="Import coverage", append=T)



## export coverage
ec.coverage=data.frame()

for(agg in outward.types){
  
  imf.types=imf.instruments$intervention.type[imf.instruments$imf.label==agg]
  
  
  ## World total
  gta_trade_coverage(gta.evaluation = gta.evaluation,
                     affected.flows="outward",
                     intervention.types=imf.types,
                     keep.type = T,
                     implementer.role = "exporter",
                     coverage.period = c(min(years),max(years)))
  
  
  coverage=trade.coverage.estimates[,c(2, seq(4,ncol(trade.coverage.estimates),1))]
  coverage$`Exporting country` ="World total"
  rm(trade.coverage.estimates)
  names(coverage)=gsub("Trade coverage estimate for ","",names(coverage))
  coverage$level=agg
  coverage=coverage[,c("Exporting country","level",years)]
  names(coverage)=c("exporter","level",paste("share.",years,sep=""))
  
  ec.coverage=rbind(ec.coverage, coverage)
  rm(coverage)
  
  
  ## individual countries
  if(length(individual.countries.un)>0){
    
    gta_trade_coverage(gta.evaluation = gta.evaluation,
                       intervention.types=imf.types,
                       keep.type = T,
                       exporters=individual.countries,
                       keep.exporters = T,
                       group.exporters=F,
                       affected.flows="outward",
                       implementer.role = "exporter",
                       coverage.period = c(min(years),max(years)))
    
    coverage=trade.coverage.estimates[,c(2, seq(4,ncol(trade.coverage.estimates),1))]
    rm(trade.coverage.estimates)
    names(coverage)=gsub("Trade coverage estimate for ","",names(coverage))
    coverage$level=agg
    coverage=coverage[,c("Exporting country","level",years)]
    names(coverage)=c("exporter","level",paste("share.",years,sep=""))
    
    ec.coverage=unique(rbind(ec.coverage, coverage))
    rm(coverage)
    
  }
  
  print(agg)
}
names(ec.coverage)=c("exporter", "level", paste("Share of own exports affected by export restrictions in ",years,sep=""))

export.coverage=ec.coverage
export.coverage[is.na(export.coverage)]=0
export.coverage=export.coverage[order(export.coverage$exporter),]

xlsx::write.xlsx(export.coverage, file=paste0(output.path, "GTA data for the IMF - incl. FDI.xlsx"), row.names = F, sheetName="Export coverage", append=T)
