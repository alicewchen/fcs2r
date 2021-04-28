#------------------------------
#  Extract mean log10(FL2) from fcs
#------------------------------
#' @name fcs_to_mean_logPEA
#' @aliases fcs_to_mean_logPEA
#' @title Export mean log10(PE-A) (FL2-A) from multiple \code{.fcs} files
#' @description This function calculates the mean log PE-A of each \code{.fcs} file within a folder and returns all the values as a dataframe.
#' @template input-arg
#' @template common-note
#' @return Returns a dataframe with the 3 columns: \code{Well_id}, \code{mean_PE_A},\code{Plate}
#' @import flowCore
#' @import utils
#' @export fcs_to_mean_logPEA
#' @examples
#' library(fcs2r)
#' wd<- getwd()
#' fcs_folder <- paste(system.file(package = "fcs2r"),"/extdata/fcs_data/", sep ="")
#' df <- fcs_to_mean_logPEA(fcs_folder) #save output as dataframe
#' fcs_to_mean_logPEA(fcs_folder, "test.txt") #export as text file


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
    index <- match("FL2-A", attributes(f@exprs)$dimnames[[2]])
    mean_PE_A<-(mean(log10(f@exprs[,index])))
    df<-cbind(well_id, mean_PE_A, plate)
    out.file<-rbind(out.file,df)

  }
  setwd(currentwd)
  if (missing(output_filepath)==FALSE) {
    utils::write.table(out.file, output_filepath, quote=FALSE, sep="\t")}
  return(out.file)
}
