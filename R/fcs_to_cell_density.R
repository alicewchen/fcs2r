#Open packages
#' @name fcs_to_cell_density
#' @import flowCore
#' @import utils
#' @export fcs_to_cell_density
####Input:
#1. Folder containing only .fcs files you need
    #.fcs files must have file names in the following format:
    #      Plate number(##)-Well-Well position([A-Z]##)..\.fcs
        #Example 1: 01-Well-A1.fcs
        #Example 2: 01-Well-A1_Live alga.fcs
#2. Full folder path containing .fcs files
#3. Full output file path including .txt file name

####Output (.txt, tab-delimited)
#Well_id, Cell_density, plate

fcs_to_cell_density<-function(folder_path_containing_fcs, output_filepath=NULL){

  currentwd<-getwd()

  #create dataframe to store extracted .fcs data
  out.file <- data.frame(matrix(ncol=3, nrow=0))
  columns<-c("Well_id","Cell_density", "plate")
  colnames(out.file)<-columns

  #list all file names ending with .fcs within folder containing .fcs files
  setwd(folder_path_containing_fcs)
  file.names <- dir(pattern=".fcs")

  for(i in 1:length(file.names)){

    file<-file.names[i]
    f<-flowCore::read.FCS(file.names[i])
    total_events<-as.numeric(f@description$`$TOT`)
    volume<-as.numeric(f@description$`$VOL`)/1000
    cell_density<-total_events/volume
    plate<-as.numeric(strsplit(strsplit(file,"_")[[1]][1],"-")[[1]][1])
    well_id<-strsplit(strsplit(file,"_")[[1]][1],"-")[[1]][3]
    df<-cbind(well_id, cell_density, plate)
    out.file<-rbind(out.file,df)

  }

  setwd(currentwd)
  if (is.null(output_filepath) == FALSE){
    utils::write.table(out.file, output_filepath, quote=FALSE, sep="\t")
  }

  return(out.file)
}

