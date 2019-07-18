if (!('dplyr' %in% installed.packages()[,"Package"]))  install.packages('dplyr')
if (!('readr' %in% installed.packages()[,"Package"]))  install.packages('readr')
if (!('here' %in% installed.packages()[,"Package"]))  install.packages('here')

library(dplyr)
library(readr)
library(here)

local_files <- list.files(pattern = ".txt")

# remove files not necessary for merge

if (length(which(grepl("package_info",local_files))) > 0) local_files = local_files[-which(grepl("package_info",local_files))]
if (length(which(grepl("fmriresults01",local_files))) > 0) local_files = local_files[-which(grepl("fmriresults01",local_files))]
if (length(which(grepl("genomics_sample03",local_files))) > 0) local_files = local_files[-which(grepl("genomics_sample03",local_files))]
if (length(which(grepl("aurora01",local_files))) > 0) local_files = local_files[-which(grepl("aurora01",local_files))]
if (length(which(grepl("omics_experiments",local_files))) > 0) local_files = local_files[-which(grepl("omics_experiments",local_files))]
if (length(which(grepl("errors",local_files))) > 0) local_files = local_files[-which(grepl("errors",local_files))]
if (length(which(grepl("ABDC_MID_task.txt",local_files))) > 0) local_files = local_files[-which(grepl("ABDC_MID_task.txt",local_files))]
if (length(which(grepl("ABCD_SST.txt",local_files))) > 0) local_files = local_files[-which(grepl("ABCD_SST.txt",local_files))]
if (length(which(grepl("ABCD_rest.txt",local_files))) > 0) local_files = local_files[-which(grepl("ABCD_rest.txt",local_files))]
if (length(which(grepl("ABCD_NBACK.txt", local_files))) > 0) local_files = local_files[-which(grepl("ABCD_NBACK.txt", local_files))]

csv_files <- lapply(local_files, function(i){
  tryCatch({
    print(paste("Import: ", gsub("*.txt$|.txt", "", i)))
    a <- read_tsv(i, col_names = TRUE, col_types = NULL, quote = "", skip_empty_rows = FALSE, comment = "")
    a = as.data.frame(sapply(a, function(x) gsub("\"", "", x)))
    names(a) = as.list(sapply(names(a), function(x) gsub("\"","",x)))
    # Drop first row - contains header information, which is already present in the Data Dictionary
    a = a[-1,] 
    a = droplevels(a)
    # Drop columns introduced by NDA, they are not required in the resulting table.
    a = a[,!(names(a) %in% c("collection_id", "collection_title", "promoted_subjectkey", "subjectkey", "study_cohort_name"))]
    a
  }, error = function(e) {
    print(e)
    read.table(file = i, sep = '\t', header = TRUE)
  })
})

# Sometimes the "eventname" column shared in many instruments is called "visit". 
# In freesqc01 both columns exist and are different:

for (p in 1:length(csv_files)) {
  dt = csv_files[[p]]
  if ("visit" %in% names(dt)){
    print(p)
    dt$eventname = dt$visit
  }
  csv_files[[p]] = dt
}


# There are some other columns that appear in more than one instrument. 
# The last merge step would introduce duplicate columns if they remain in the data. 
# Remove interview_age and interview_date from all instrument but keeping lt01 as anchor.

rm.vars=c("visit","interview_age","interview_date","gender")

for (p in 1:length(csv_files)) {
  dt = csv_files[[p]]
  inst_name = gsub("_id", "", colnames(dt)[1])
  if (inst_name=="abcd_midabwdp201"){
    
    #both "abcd_midabwdp201" and "abcd_midabwdp01" have the same variables (same values), delete one;
    dt = dt[,!(names(dt) %in% c("tfmri_mid_all_antic.large.vs.small.reward_beta_cort.destrieux_g.front.inf.orbital.rh",rm.vars))]
    
  } else if (inst_name == "abcd_dmdtifp201"){ 
    #both abcd_dmdtifp101 and abcd_dmdtifp201 have the same variable, delete one;
    dt = dt[,!(names(dt) %in% c("dmri_dtifull_visitid",rm.vars))]
  } else if (inst_name != "abcd_lt01"){
    dt = dt[,!(names(dt) %in% rm.vars)] 
  }
  csv_files[[p]] = dt
}

# As a final step, re-calculate the levels in each table. 
# Information that has been removed in previous steps could have changed the factor information in each table.

for (p in 1:length(csv_files)) {
  dt = csv_files[[p]]
  dt = droplevels(dt)
  csv_files[[p]] = dt
}

# spreadsheet created from NIMH web API

release_names_nda <- read.csv("abcd_instruments_v2.csv", row.names = NULL)

# make a folder to put your files in

if(!file.exists(here("2.0-ABCD-Release"))){
  dir.create(here("2.0-ABCD-Release"))
}
# convert .txt files to .csv

sapply(csv_files, function(i){
  short_name = gsub("_id", "", colnames(i)[1])
  category = paste0(release_names_nda$category[match(short_name, release_names_nda$shortName)])
  title = paste0(release_names_nda$title[match(short_name, release_names_nda$shortName)])
  if(short_name %in% release_names_nda$shortName){
    file_name = gsub("/", "-", title)
  } else {
    file_name = short_name
  }
  tryCatch({
    print(paste("Writing", short_name, ":", file_name))
    if(!file.exists(here("2.0-ABCD-Release",category))){
      dir.create(here("2.0-ABCD-Release", category))
    }
    write.csv(i, 
              file = here("2.0-ABCD-Release", category, paste0(file_name, ".csv")),
              row.names = FALSE)
  }, error = function(e){
    print(e)
  })
})

# and save in native r format

if(!file.exists(here("2.0-ABCD-Release-R-format"))){
  dir.create(here("2.0-ABCD-Release-R-format"))
}

sapply(csv_files, function(i){
  short_name = gsub("_id", "", colnames(i)[1])
  title = paste0(release_names_nda$title[match(short_name, release_names_nda$shortName)])
  if(short_name %in% release_names_nda$shortName){
    file_name = gsub("/", "-", title)
  } else {
    file_name = short_name
  }
  tryCatch({
    print(paste("Writing in native R format", short_name, ":", file_name))
    saveRDS(i, 
            file = here("2.0-ABCD-Release-R-format", paste0(file_name, ".Rds")))
  }, error = function(e){
    print(e)
  })
})
