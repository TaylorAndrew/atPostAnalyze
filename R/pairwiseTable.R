#' pairwiseTable
#'
#' pairwiseTable provides a table explicating all pairwise comparisons made, along with their pairwise number found in the multGroup function.
#'
#' @param levels A Vector of levels to use for the pairwise table
#' @param printOutput If TRUE, pairwise table will be printed to a word document
#' @param documentName If printOutput==T, the name of the output word document
#'
#' @return
#' @export
#'
#' @examples
#' levels<-c("White", "Black", "Other")
#' pairwiseTable(levels)
pairwiseTable <- function(levels,
                          printOutput = FALSE,
                          documentName = "PairwiseTable.doc") {
  l <- t(combn(levels,2))
  l <- data.frame(num = 1:length(l[,1]),l)
  if (printOutput == TRUE) {
    quick.print <- function(PrintType = "Table",
                            table = FALSE,
                            filename = "RPrintout.doc",
                            tableintro = "Table X",
                            row.names = FALSE,
                            fontsize = 10,
                            column.margins = 1,
                            plotfunc = FALSE,
                            plotargs = FALSE,
                            plotwidth = 6,
                            plotheight = 6,
                            resolution = 300) {
      #Get package and load
      get.packages <- function(list) {
        new.packages <- list[!(list %in% installed.packages()[,"Package"])]
        old.packages <-
          list[(list %in% installed.packages()[,"Package"])]
        if (length(old.packages)) {
          #       print(paste(rbind(old.packages, "is already installed.")))
        }
        if (length(new.packages)) {
          #       print(paste(rbind(new.packages, "not yet installed. Attempting to install now")))
          install.packages(new.packages)
        }
        lapply(list,function(x) {
          library(x,character.only = TRUE)
        })
        #     print(paste(rbind(list, "has been loaded")))
      }
      get.packages("rtf")


      if (PrintType == "Table") {
        type <- class(table)
        tabtypes <- c("table", "data.frame", "matrix")
        match <- type %in% tabtypes
        if (match != 1)
          print("Error: You have specified a non-Table-Type Object as a Table-Type Object")
        else {
          #Write to rtf
          rtf <- RTF(
            filename, width = 8.5, height = 11, font.size = 10,
            omi = c(1, 1, 1, 1)
          )

          addParagraph(rtf, tableintro)

          len <- length(table[1,])
          columnwid <- rep(column.margins,len)
          if (row.names == TRUE)
            columnwid <- c(columnwid,column.margins)
          addTable(
            rtf, table, font.size = fontsize, row.names = row.names, NA.string = "-",
            col.widths = columnwid
          )
        }
        if (PrintType == "Plot") {
          rtf <- RTF(
            filename, width = 8.5, height = 11, font.size = 10,
            omi = c(1, 1, 1, 1)
          )

          addParagraph(rtf, tableintro)
          addPlot(
            rtf,plot.fun = plotfunc,width = plotwidth,height = plotheight,res = resolution, plotargs
          )
        }
        done(rtf)
      }
    }
    quick.print(
      PrintType = "Table",
      table = l,
      filename = documentName,
      tableintro = "Table X",
      row.names = FALSE,
      fontsize = 10,
      column.margins = 1,
      plotfunc = FALSE,
      plotargs = FALSE,
      plotwidth = 6,
      plotheight = 6,
      resolution = 300
    )
  }
  return(l)
}