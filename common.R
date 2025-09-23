#turn off messages and warnings and make it so output isn't prefixed by anything,
#default is to put "##" in front of all output for some reason
#also set tidy to true so code is wrapped properly 
knitr::opts_chunk$set(message=FALSE, warning=FALSE, comment = "", cache = F, fig.align = "center")
options(width = 200)
options(readr.show_col_types = FALSE)
packagesUsed = c(
  "DT",
  "ggplot2",
  "tidyverse",
  "stringr",
  'knitr',
  'rmarkdown',
  "fastmatch",
  "rwantshue",
  "ComplexHeatmap",
  "ggthemes", 
  "here", 
  "reticulate"
)

suppressMessages(lapply(packagesUsed, require, character.only = TRUE))


# commonly used functions
`%!in%` <- Negate(`%in%`)
is.notna <- function(x) {
  return(!is.na(x))
}
scheme <- iwanthue(seed = 42, force_init = TRUE) 


# ggplot themes 
transparentBackground = theme(
  panel.background = element_rect(fill='transparent'), #transparent panel bg
  plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
  panel.grid.major = element_blank(), #remove major gridlines
  panel.grid.minor = element_blank(), #remove minor gridlines
  legend.background = element_rect(color = NA, fill='transparent'), #transparent legend bg
  legend.box.background = element_rect(color = NA, fill='transparent') #transparent legend panel
)
sofonias_theme_noTransparentBackground = theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank() )+
  theme(axis.line.x = element_line(color="black", linewidth = 0.3),axis.line.y =
          element_line(color="black", linewidth = 0.3))+
  theme(text=element_text(size=12, family="Helvetica"))+
  theme(axis.text.y = element_text(size=12))+
  theme(axis.text.x = element_text(size=12)) +
  theme(legend.position = "bottom") + 
  theme(plot.title = element_text(hjust = 0.5))
sofonias_theme = theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank() )+
  theme(axis.line.x = element_line(color="black", linewidth = 0.3),axis.line.y =
          element_line(color="black", linewidth = 0.3))+
  theme(text=element_text(size=12, family="Helvetica"))+
  theme(axis.text.y = element_text(size=12))+
  theme(axis.text.x = element_text(size=12)) +
  theme(legend.position = "bottom") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  transparentBackground

sofonias_theme_xRotate_noTransparentBackground = sofonias_theme + 
  theme(axis.text.x = element_text(size=12, angle = -90, vjust = 0.5, hjust = 0)) 
sofonias_theme_xRotate = sofonias_theme + 
  theme(axis.text.x = element_text(size=12, angle = -90, vjust = 0.5, hjust = 0)) + 
  transparentBackground


# color schemes
colorPalette_08 = c("#2271B2","#F748A5","#359B73","#F0E442","#D55E00","#3DB7E9","#E69F00","#000000")
colorPalette_12 = c("#E20134","#FF6E3A","#008DF9","#8400CD","#FFC33B","#9F0162","#009F81","#FF5AAF","#00FCCF","#00C2F9","#FFB2FD","#A40122")
colorPalette_15 = c("#F60239","#003C86","#EF0096","#9400E6","#009FFA","#008169","#68023F","#00DCB5","#FFCFE2","#FF71FD","#7CFFFA","#6A0213","#008607","#00E307","#FFDC3D")

palettes <- ggthemes_data[["tableau"]][["color-palettes"]][["regular"]]


createColorListFromDf <- function(df, colorPalette = colorPalette_12, iwanthudSeed = rnorm(1) * 100){
  colorList = list()
  for (dfColname in colnames(df)) {
    levels = sort(unique(df[[dfColname]]))
    scheme <- iwanthue(seed = iwanthudSeed, force_init = TRUE)
    if (length(levels) <= length(colorPalette_12)) {
      levelsCols = colorPalette_12[1:length(levels)]
    } else{
      levelsCols = scheme$hex(length(levels))
    }
    names(levelsCols) = levels
    colorList[[dfColname]] = levelsCols
  }
  return(colorList)
}

# quarto utils 
create_dt <- function(x) {
  DT::datatable(
    x,
    extensions = 'Buttons',
    options = list(
      dom = 'Blfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
      lengthMenu = list(c(10, 25, 50, -1),
                        c(10, 25, 50, "All"))
    ),
    filter = "top"
  )
}

createDownloadLink <-function(fnp, linkName = ""){
  relToHere = sub(here(),"",normalizePath(getwd()), fixed = T)
  depth = stringr::str_count(relToHere, "/")
  prefix = ""
  if(depth > 0){
    prefix = paste0(paste0(rep("..",depth), collapse = "/"), "/")
  }
  if ("" == linkName) {
    linkName =  basename(fnp)
  }
  return(paste0("[", linkName, "]", "(", paste0(prefix, gsub(paste0(here(),"/"),"",normalizePath(fnp), fixed = T)), "){.downloadLink .btn .btn-info}" ) )
}

createImgLink <-function(fnp, linkName = ""){
  relToHere = sub(here(), "", normalizePath(getwd()), fixed = T)
  depth = stringr::str_count(relToHere, "/")
  prefix = ""
  if(depth > 0){
    prefix = paste0(paste0(rep("..",depth), collapse = "/"), "/")
  }
  if ("" == linkName) {
    linkName =  basename(fnp)
  }
  return(paste0("![", linkName, "]", "(", paste0(prefix, gsub(paste0(here(),"/"),"",normalizePath(fnp), fixed = T)), ")" ) )
}


cleanUpFnp <- function(fnp) {
  cleanedFnp = gsub("\\/+", "/", fnp)
  cleanedFnpSplit = unlist(strsplit(cleanedFnp, split = "/"))
  cleanedFnpSplitDotDotIdx = (1:length(cleanedFnpSplit))[cleanedFnpSplit == ".."]
  return(cleanedFnp)
}


create_tabsetOfHtmlWidgets <- function(htmlObjectsList) {
  zz <- textConnection("foo", "w")
  sink(zz)
  cat(":::::: {.panel-tabset}\n")
  iwalk(htmlObjectsList, ~ {
    cat('## ', .y, '\n\n')
    tempList = list()
    tempList[["item"]] = .x
    print(htmltools::tagList(tempList))
    cat('\n\n')
  })
  cat("::::::\n")
  sink()
  close(zz)
  paste0(foo, collapse = "\n")
}

create_tabsetOfGgplotObjects <- function(ggplotObjectsList) {
  # zz <- textConnection("foo", "w")
  # sink(zz)
  cat("\n\n::: {.panel-tabset}\n\n")
  iwalk(ggplotObjectsList, ~ {
    cat('## ', .y, '\n\n')
    print(.x)
    # tempList = list()
    # tempList[["item"]] = .x
    # grid::grid.draw(ggplot2::ggplotGrob(.x))
    cat('\n\n')
  })
  cat(":::\n")
  # sink()
  # close(zz)
  # paste0(foo, collapse = "\n")
}


genColorsForValues <- function(values) {
  uniqValues = unique(values)
  outColors = c()
  if (length(uniqValues) <= 8) {
    outColors = colorPalette_08[1:length(uniqValues)]
  } else if (length(uniqValues) <= 12) {
    outColors = colorPalette_12[1:length(uniqValues)]
  } else if (length(uniqValues) <= 15) {
    outColors = colorPalette_15[1:length(uniqValues)]
  } else{
    outColors = scheme$hex(length(uniqValues))
  }
  names(outColors) = uniqValues
  return (outColors)
}

writeFasta<-function(data, filename, openMode = "w"){
  if(stringr::str_ends(filename, ".gz")){
    fileConn<-gzfile(filename, open = openMode)
  }else{
    fileConn<-file(filename, open = openMode)
  }
  for (rowNum in 1:nrow(data)){
    cat(paste(">", data[[rowNum,"name"]], sep = ""), file = fileConn, sep = "\n")
    cat(data[[rowNum,"seq"]], file = fileConn, sep = "\n")
  }
  close(fileConn)
}


#' Check sharing and unqiue values between two vectors
#'
#' @param vectorA the first vector
#' @param vectorB the second vector
#'
#' @returns a list with 4 vectors, "only_in_vectorA" unique to vectorA, "only_in_vectorB" unique to vectorB, "shared_samples" shared between both vectorA and vectorB, "all" all values by combinng vectorA and vectorB 
set_decompose <- function(vectorA, vectorB){
  ret = list()
  # Find unique and shared samples
  ret[["only_in_vectorA"]] <- setdiff(vectorA, vectorB)  # Samples only in vectorA
  ret[["only_in_vectorB"]] <- setdiff(vectorB, vectorA)  # Samples only in vectorB
  ret[["shared_samples"]] <- intersect(vectorA, vectorB) # Samples shared between vectorA and vectorB
  ret[["all"]] <- union(vectorA, vectorB) # All samples between vectorA and vectorB
  return(ret)
}

