#------------------------------
#  Extract median FL2 from fcs
#------------------------------
#' @name fcs_to_median_PEA
#' @aliases fcs_to_median_PEA
#' @title Export median PE-A (FL2-A) from multiple `.fcs` files
#' @description This function calculates the median PE-A of each `.fcs` file within a folder and returns all the values as a dataframe.
#' @template input-arg
#' @template common-note
#' @return Returns a dataframe with the 3 columns: `Well_id`,`median_PE_A`,`Plate`
#' @import flowCore
#' @import utils
#' @importFrom stats median
#' @export fcs_to_median_PEA
#' @examples
#' library(fcs2r)
#' wd<- getwd()
#' fcs_folder <- paste(system.file(package = "fcs2r"),"/extdata/fcs_data/", sep ="")
#' df <- fcs_to_median_PEA(fcs_folder) #save output as dataframe
#' fcs_to_median_PEA(fcs_folder, "test.txt") #export as text file


fcs_to_median_PEA<-function(folder_path_containing_fcs, output_filepath=NULL){

  currentwd<-getwd()

  #create dataframe to store extracted .fcs data
  out.file <- data.frame(matrix(ncol=3, nrow=0))
  columns<-c("Well_id","mean_PE_A", "Plate")
  colnames(out.file)<-columns

  #list all file names ending with .fcs within folder containing .fcs files
  setwd(folder_path_containing_fcs)
  file.names <- dir(pattern=".fcs")

  for(i in 1:length(file.names)){

    file<-file.names[i]
    f<-flowCore::read.FCS(file)
    plate<-as.numeric(strsplit(strsplit(file,"_")[[1]][1],"-")[[1]][1])
    well_id<-strsplit(strsplit(file,"_")[[1]][1],"-")[[1]][3]
    index <- match("FL2-A", attributes(f@exprs)$dimnames[[2]])
    mean_PE_A<-(stats::median(f@exprs[,index]))
    df<-cbind(well_id, mean_PE_A, plate)
    out.file<-rbind(out.file,df)

  }
  setwd(currentwd)
  if (is.null(output_filepath)) {
    utils::write.table(out.file, output_filepath, quote=FALSE, sep="\t")}
  return(out.file)
}



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

fcs_to_mean_PEA<-function(folder_path_containing_fcs, output_filepath){

  currentwd<-getwd()

  #create dataframe to store extracted .fcs data
  out.file <- data.frame(matrix(ncol=3, nrow=0))
  columns<-c("Well_id","mean_PE_A", "Plate")
  colnames(out.file)<-columns

  #list all file names ending with .fcs within folder containing .fcs files
  setwd(folder_path_containing_fcs)
  file.names <- dir(pattern=".fcs")

  for(i in 1:length(file.names)){

    file<-file.names[i]
    f<-flowCore::read.FCS(file)
    plate<-as.numeric(strsplit(strsplit(file,"_")[[1]][1],"-")[[1]][1])
    well_id<-strsplit(strsplit(file,"_")[[1]][1],"-")[[1]][3]
    index <- match("FL2-A", attributes(f@exprs)$dimnames[[2]])
    mean_PE_A<-(median(f@exprs[,index]))
    df<-cbind(well_id, mean_PE_A, plate)
    out.file<-rbind(out.file,df)

  }
  setwd(currentwd)
  if (missing(output_filepath)==FALSE) {
    utils::write.table(out.file, output_filepath, quote=FALSE, sep="\t")}
  return(out.file)
}
