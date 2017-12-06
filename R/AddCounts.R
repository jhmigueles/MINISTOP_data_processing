AddCounts <- function(metadatadir, NFdir=c(), LFEdir=c(),
                      f0=1, f1=length(dir(metadatadir, pattern = "*.RData")), desiredtz = "Europe/London"){
  #Function to import ActiGraph's activity counts (i.e., NFCounts and LFECounts) to the GGIR data frame (M$metashort)
  #produced from the function g.part1 in which all the research-derived metrics are calculated.
  #This function allowed us to process all the metrics with the GGIR functions.
  #Metadatadir = directory where files exported from g.part1 are stored ("./output_/meta/basic")
  #NFdir = directory where the ActiGraph's normal filtered counts "csv." files are stored.
  #LFEdir = directory where the ActiGraph's LFE filtered counts "csv." files are stored.

        #Names of the metafiles, normal filtered counts and LFE counts files
        if (length(dir(paste(metadatadir, "meta/basic/", sep = ""), pattern = "*.RData"))==0) {print("Invalid metadatadir"); metafiles = NA}
        else {metafiles = dir(paste(metadatadir, "meta/basic/", sep = ""), pattern = "*.RData")}
        
        if (length(NFdir)==0) {print("No NF files"); NFfiles = NA}
        else {NFfiles = dir(NFdir, pattern = "*.csv")}
        
        if (length(LFEdir)==0) {print("No LFE files"); LFEfiles = NA}
        else {LFEfiles = dir(LFEdir, pattern = "*.csv")}
        
        POSIXtime2iso8601 = function (x, tz) 
        {
                POStime = as.POSIXlt(x, tz=c())
                POStime_z = chartime2iso8601(as.character(POStime), tz)
                return(POStime_z)
        }
        
        for (i in 1:length(metafiles[f0:f1])) { #Loop through the files
                print(i)
                load(paste0(metadatadir, "meta/basic/", metafiles[i]))
                
                if (is.na(NFfiles)==FALSE & is.na(LFEfiles)==FALSE) { #NF files and LFE files available
                        NFCounts = read.csv(paste0(NFdir, NFfiles[i]))
                        LFECounts = read.csv(paste0(LFEdir, LFEfiles[i]))
                        
                        if(length(which(colnames(NFCounts)=="vectormagnitude"))==0 &
                           length(which(colnames(LFECounts)=="vectormagnitude"))==0 &
                           length(which(colnames(NFCounts)=="steps"))==0 &
                           length(which(colnames(LFECounts)=="steps"))==0) {
                                print("No counts nor steps data in the csv files")
                        } 
                        else if(length(which(colnames(NFCounts)=="vectormagnitude"))==1 &
                                length(which(colnames(LFECounts)=="vectormagnitude"))==1 &
                                length(which(colnames(NFCounts)=="steps"))==0 &
                                length(which(colnames(LFECounts)=="steps"))==0){
                                Counts = data.frame(timestamp = POSIXtime2iso8601(as.POSIXct(LFECounts[,"timestamp"]), tz = desiredtz),
                                                    NFCounts = as.numeric(NFCounts[,"vectormagnitude"])/1000, #divided into 1000, so when multiply in the following parts I can set cut-points in normal units (same treatment than the rest of the metrics)
                                                    LFECounts = as.numeric(LFECounts[,"vectormagnitude"])/1000)
                        } 
                        else if(length(which(colnames(NFCounts)=="vectormagnitude"))==0 &
                                length(which(colnames(LFECounts)=="vectormagnitude"))==0 &
                                length(which(colnames(NFCounts)=="steps"))==1 &
                                length(which(colnames(LFECounts)=="steps"))==1){
                                Counts = data.frame(timestamp = POSIXtime2iso8601(as.POSIXct(LFECounts[,"timestamp"]), tz = desiredtz),
                                                    NFSteps = as.numeric(NFCounts[,"steps"])/1000, #divided into 1000, so when multiply in the following parts I can set cut-points in normal units (same treatment than the rest of the metrics)
                                                    LFESteps = as.numeric(LFECounts[,"steps"])/1000)
                        }
                        else if(length(which(colnames(NFCounts)=="vectormagnitude"))==1 &
                                length(which(colnames(LFECounts)=="vectormagnitude"))==1 &
                                length(which(colnames(NFCounts)=="steps"))==1 &
                                length(which(colnames(LFECounts)=="steps"))==1){
                                Counts = data.frame(timestamp = POSIXtime2iso8601(as.POSIXct(LFECounts[,"timestamp"]), tz = desiredtz),
                                                    NFCounts = as.numeric(NFCounts[,"vectormagnitude"])/1000, #divided into 1000, so when multiply in the following parts I can set cut-points in normal units (same treatment than the rest of the metrics)
                                                    LFECounts = as.numeric(LFECounts[,"vectormagnitude"])/1000,
                                                    NFSteps = as.numeric(NFCounts[,"steps"])/1000, #divided into 1000, so when multiply in the following parts I can set cut-points in normal units (same treatment than the rest of the metrics)
                                                    LFESteps = as.numeric(LFECounts[,"steps"])/1000)
                        }
                }
                else if (is.na(NFfiles)==FALSE & is.na(LFEfiles)==TRUE) { #Only NF files available
                        NFCounts = read.csv(paste0(NFdir, NFfiles[i]))
                        
                        if(length(which(colnames(NFCounts)=="vectormagnitude"))==0 &
                           length(which(colnames(NFCounts)=="steps"))==0 &
                           length(which(colnames(NFCounts)=="axis1"))==0){
                                print("No counts nor steps data in the csv files")
                        } 
                        else if(length(which(colnames(NFCounts)=="vectormagnitude"))==1 &
                                length(which(colnames(NFCounts)=="axis1"))==0 &
                                length(which(colnames(NFCounts)=="steps"))==0){
                                Counts = data.frame(timestamp = POSIXtime2iso8601(as.POSIXct(NFCounts[,"timestamp"]), tz = desiredtz),
                                                    NFCounts = as.numeric(NFCounts[,"vectormagnitude"])/1000)
                        }
                        else if(length(which(colnames(NFCounts)=="vectormagnitude"))==1 &
                                length(which(colnames(NFCounts)=="axis1"))==1 &
                                length(which(colnames(NFCounts)=="steps"))==0){
                                Counts = data.frame(timestamp = POSIXtime2iso8601(as.POSIXct(NFCounts[,"timestamp"]), tz = desiredtz),
                                                    NFCounts = as.numeric(NFCounts[,"vectormagnitude"])/1000,
                                                    VACounts = as.numeric(NFCounts[,"axis1"])/1000)
                        }
                        else if(length(which(colnames(NFCounts)=="vectormagnitude"))==0 &
                                length(which(colnames(NFCounts)=="steps"))==1){
                                Counts = data.frame(timestamp = POSIXtime2iso8601(as.POSIXct(NFCounts[,"timestamp"]), tz = desiredtz),
                                                    NFSteps = as.numeric(NFCounts[,"steps"])/1000)
                        }
                        else if(length(which(colnames(NFCounts)=="vectormagnitude"))==1 &
                                length(which(colnames(NFCounts)=="axis1"))==0 &
                                length(which(colnames(NFCounts)=="steps"))==1){
                                Counts = data.frame(timestamp = POSIXtime2iso8601(as.POSIXct(NFCounts[,"timestamp"]), tz = desiredtz),
                                                    NFCounts = as.numeric(NFCounts[,"vectormagnitude"])/1000,
                                                    NFSteps = as.numeric(NFCounts[,"steps"])/1000)
                        }
                        
                }
                else if (is.na(NFfiles)==TRUE & is.na(LFEfiles)==FALSE){ #Only LFE files available
                        LFECounts = read.csv(paste0(LFEdir, LFEfiles[i]))
                        
                        if(length(which(colnames(LFECounts)=="vectormagnitude"))==0 &
                           length(which(colnames(LFECounts)=="steps"))==0){
                                print("No counts nor steps data in the csv files")
                        } 
                        else if(length(which(colnames(LFECounts)=="vectormagnitude"))==1 &
                                length(which(colnames(LFECounts)=="steps"))==0){
                                Counts = data.frame(timestamp = POSIXtime2iso8601(as.POSIXct(LFECounts[,"timestamp"]), tz = desiredtz),
                                                    LFECounts = as.numeric(LFECounts[,"vectormagnitude"])/1000)
                        }
                        else if(length(which(colnames(LFECounts)=="vectormagnitude"))==0 &
                                length(which(colnames(LFECounts)=="steps"))==1){
                                Counts = data.frame(timestamp = POSIXtime2iso8601(as.POSIXct(LFECounts[,"timestamp"]), tz = desiredtz),
                                                    LFESteps = as.numeric(LFECounts[,"steps"])/1000)
                        }
                        else if(length(which(colnames(LFECounts)=="vectormagnitude"))==1 &
                                length(which(colnames(LFECounts)=="steps"))==1){
                                Counts = data.frame(timestamp = POSIXtime2iso8601(as.POSIXct(LFECounts[,"timestamp"]), tz = desiredtz),
                                                    LFECounts = as.numeric(LFECounts[,"vectormagnitude"])/1000,
                                                    LFESteps = as.numeric(LFECounts[,"steps"])/1000)
                        }
                }
                
                #M$metashort$timestamp.tmp = substr(M$metashort$timestamp, 1, nchar(M$metashort$timestamp)-8)
                #Counts$timestamp.tmp = substr(Counts$timestamp, 1, nchar(Counts$timestamp)-4)

                M$metashort = merge(M$metashort, Counts, all.x = TRUE, all.y = FALSE, by="timestamp") #Merging datasets
                #M$metashort = M$metashort[,-c("timestamp.tmp")]
                
                if (dir.exists(paste(metadatadir, "meta/basic2/", sep=""))==FALSE){ 
                        dir.create(paste(metadatadir, "meta/basic2/", sep=""))
                }
                save(M, I, C, filename_dir, filefoldername, 
                     file = paste(metadatadir, "meta/basic2/meta_", 
                                  filename_dir, ".RData", sep = "")) #Saving datasets
        }  
}

