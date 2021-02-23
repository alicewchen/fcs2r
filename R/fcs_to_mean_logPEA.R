##############################
#  Extract mean FL2 from fcs #
##############################
#' @name fcs_to_mean_logPEA
#' @import flowCore
#' @import utils
#' @export fcs_to_mean_logPEA
####Input:
#1. Folder containing only .fcs files you need
    #.fcs files must have file names in the following format:
    #      Plate number(##)-Well-Well position([A-Z]##)..\.fcs
    #Example 1: 01-Well-A1.fcs
    #Example 2: 01-Well-A1_Live alga.fcs
#2. Full folder path containing .fcs files
#3. Full output file path including .txt file name

####Output (.txt, tab-delimited)
#Well_id, mean_PEA, plate

fcs_to_mean_logPEA<-function(folder_path_containing_fcs, output_filepath){

  currentwd<-getwd()

  #create dataframe to store extracted .fcs data
  out.file <- data.frame(matrix(ncol=3, nrow=0))
  columns<-c("Well_id","mean_PE_A", "Plate")
  colnames(out.file)<-columns

  #list all file names ending with .fcs within folder containing .fcs files
  setwd(folder_path_containing_fcs)
  file.names <- dir(pattern =".fcs")

  for(i in 1:length(file.names)){

    file<-file.names[i]
    f<-flowCore::read.FCS(file)
    plate<-as.numeric(strsplit(strsplit(file,"_")[[1]][1],"-")[[1]][1])
    well_id<-strsplit(strsplit(file,"_")[[1]][1],"-")[[1]][3]
    mean_PE_A<-(mean(log10(f@exprs[,8])))
    df<-cbind(well_id, mean_PE_A, plate)
    out.file<-rbind(out.file,df)

  }
  setwd(currentwd)
  utils::write.table(out.file, output_filepath, quote=FALSE, sep="\t")
  return(out.file)
}
