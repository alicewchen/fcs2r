#------------------------------
#  Extract mean FSC-A and SSC-A from fcs
#------------------------------
#' @name fcs_to_mean_fsc_ssc
#' @aliases fcs_to_mean_fsc_ssc
#' @title Export mean FSC-A and SSC-A from multiple \code{.fcs} files
#' @description This function calculates the mean mean FSC-A and SSC-A each \code{.fcs} file within a folder and returns all the values as a dataframe.
#' @template input-arg
#' @template common-note
#' @return Returns a dataframe with the 3 columns: \code{Well_id}, \code{mean_FSC},\code{mean_SSC},\code{Plate}
#' @import flowCore
#' @import utils
#' @export fcs_to_mean_fsc_ssc
#' @examples
#' library(fcs2r)
#' wd<- getwd()
#' fcs_folder <- paste(system.file(package = "fcs2r"),"/extdata/fcs_data/", sep ="")
#' df <- fcs_to_mean_fsc_scc(fcs_folder) #save output as dataframe
#' fcs_to_mean_mean_fsc_ssc(fcs_folder, "test.txt") #export as text file

fcs_to_mean_fsc_ssc<-function(folder_path_containing_fcs, output_filepath = NULL){

  currentwd<-getwd()

  #create dataframe to store extracted .fcs data
  out.file <- data.frame(matrix(ncol=4, nrow=0))
  columns<-c("Well_id","mean_FSC", "mean_SSC", "Plate")
  colnames(out.file)<-columns

  #list all file names ending with .fcs within folder containing .fcs files
  setwd(folder_path_containing_fcs)
  file.names <- dir(pattern=".fcs")

  for(i in 1:length(file.names)){

    file<-file.names[i]
    f<-flowCore::read.FCS(file)
    plate<-as.numeric(strsplit(strsplit(file,"_")[[1]][1],"-")[[1]][1])
    well_id<-strsplit(strsplit(file,"_")[[1]][1],"-")[[1]][3]
    index_fsc <- match("FSC-A", attributes(f@exprs)$dimnames[[2]])
    index_ssc <- match("SSC-A", attributes(f@exprs)$dimnames[[2]])
    mean_FSC<-(mean(f@exprs[,index_fsc]))
    mean_SSC<-(mean(f@exprs[,index_ssc]))
    df<-cbind(well_id, mean_FSC, mean_SSC, plate)
    out.file<-rbind(out.file,df)

  }
  setwd(currentwd)
  if (missing(output_filepath)==FALSE) {
    utils::write.table(out.file, output_filepath, quote=FALSE, sep="\t")}
  return(out.file)
}
