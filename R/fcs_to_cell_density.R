#------------------------------
#  Extract mean FL2 from fcs
#------------------------------
#' @name fcs_to_cell_density
#' @aliases fcs_to_cell_density
#' @title Export cell density from multiple \code{.fcs} files
#' @description This function calculates the cell density of each \code{.fcs} file within a folder and returns all the values as a dataframe.
#' \eqn{Cell density (cells per uL) = the total events recorded/the total sample volume}.
#' @template input-arg
#' @template common-note
#' @return Returns a dataframe with the 3 columns: \code{Well_id}, \code{cell density},\code{Plate}
#' @import flowCore
#' @import utils
#' @examples
#' library(fcs2r)
#' wd<- getwd()
#' fcs_folder <- paste(system.file(package = "fcs2r"),"/extdata/fcs_data/", sep ="")
#' df <- fcs_to_cell_density(fcs_folder) #save output as dataframe
#' fcs_to_cell_density(fcs_folder, "test.txt") #export as text file

#' @export
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
  if (missing(output_filepath)==FALSE) {
    utils::write.table(out.file, output_filepath, quote=FALSE, sep="\t")}
  return(out.file)
}

