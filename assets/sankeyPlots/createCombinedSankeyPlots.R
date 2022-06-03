#sapply(list.files("modules", recursive = TRUE, full.names = TRUE), source)
#sapply(list.files("scripts", recursive = TRUE, full.names = TRUE), source)
source("assets/stratification/utils.R", local = TRUE)

library(png)
library(tm)
library(grid)
library(gridExtra)
library(measurements)

source("assets/sankeyPlots/createSankeyPlot.R", local = TRUE)

# Usage:
#
# plot_parameters is a list of lists while each list represents a single plot.
#  key: file name of the single plot
#  value: single plot specification
# The single plot specification contains the following fields:
#  title: string that represents the question
#  question_list: list of questionnaire_id -> question_id pairs that shall be
#                 shown in the sankey plot
#
# legend_mm_height sets space that is removed from the total height.
#
# file_name is the file name for the combined sankey plots
#
# half_width is a boolean whether the image should be for one column

getSankeyResultDirectory <- function(create = FALSE) {
  result_directory <- file.path(getResultDirectory(), "sankey")
  if (create & !dir.exists(result_directory)) {
    dir.create(result_directory)
  }
  return(result_directory)
}

getSingleSankeyResultDirectory <- function(create = FALSE) {
  single_result_directory <- file.path(getSankeyResultDirectory(), "single")
  if (create & !dir.exists(single_result_directory)) {
    dir.create(single_result_directory)
  }
  return(single_result_directory)
}

getOutputImageParams <- function(plotNumber, title, legend_mm_height, half_width, verbose = FALSE) {
  # From https://genomemedicine.biomedcentral.com/submission-guidelines/preparing-your-manuscript#preparing+figures
  # Figures in the final PDF version:
  # * width of 85 mm for half page width figure
  # * width of 170 mm for full page width figure
  # * maximum height of 225 mm for figure and legend
  # * image resolution of approximately 300 dpi (dots per inch) at the final size
  # HEIGHT and WIDTH in px
  # RESOLUTION in PPI with POINT_SIZE = 1 (so should be same as DPI)

  max_rows <- 6 # set so that plots do not get too small and fit on page
  
  if (!is.na(title)) {
    legend_mm_height <- legend_mm_height + 30
  }
  max_mm_width <- 170
  columns <- 2
  if (half_width) {
    max_mm_width <- 85
    columns <- 1
  }
  rows <- ceiling(plotNumber / columns)
  max_inch_width <- conv_unit(max_mm_width, "mm", "inch")
  
  # Calculate image height so that contained plots are of equal height
  if (rows > max_rows & verbose) {
    print(paste("[WARNING] Too many plots for one page; max rows set to ", max_rows, " but rendering ", rows, " rows"))
  }
  heightRatio <- (1 / max_rows) * rows
  
  max_mm_height <- 225 * heightRatio
  max_mm_height <- max_mm_height - legend_mm_height
  max_inch_height <- conv_unit(max_mm_height, "mm", "inch")
  image_resolution <- 300
  point_size <- 1
  height <- image_resolution * max_inch_height
  width <- image_resolution * max_inch_width
  return(list(
    "columns" = columns,
    "rows" = rows,
    "width" = width,
    "height" = height,
    "clean.inch.width" = max_inch_width,
    "clean.inch.height" = max_inch_height,
    "resolution" = image_resolution,
    "point.size" = point_size,
    "title.fontsize" = 8
  ))
}

combinePlots <- function(inputFileNames, outputFileName, title = NA, legend_mm_height = 0, half_width = FALSE, isPdf = FALSE) {
  outputImageParams <- getOutputImageParams(length(inputFileNames), title, legend_mm_height, half_width, verbose = TRUE)
  grobs <- list()
  for (index in 1:length(inputFileNames)) {
    inputFileName <- inputFileNames[index]
    if (isPdf) {
      grobs[[index]] <- rasterGrob(readPDF(inputFileName))
    } else {
      grobs[[index]] <- rasterGrob(readPNG(inputFileName))
    }
  }
  raster_parameters <- list()
  raster_parameters[["ncol"]] <- outputImageParams[["columns"]]
  raster_parameters[["nrow"]] <- outputImageParams[["rows"]]
  raster_parameters[["grobs"]] <- grobs
  if (!is.na(title)) {
    raster_parameters[["top"]] <- textGrob(paste0("\n", title, "\n"), gp = gpar(fontsize = outputImageParams[["title.fontsize"]], fontface = 'bold'))
  }
  
  if (isPdf) {
    pdfParams <- list(
      "file" = paste0(outputFileName, ".pdf"),
      "width" = outputImageParams[["clean.inch.width"]],
      "height" = outputImageParams[["clean.inch.height"]]
    )
    do.call(pdf, pdfParams)
    do.call(grid.arrange, raster_parameters)
    dev.off()
  } else {
    imageParams <- list(
      "filename" = paste0(outputFileName, ".png"),
      "width" = outputImageParams[["width"]],
      "height" = outputImageParams[["height"]],
      "unit" = "px",
      "res" = outputImageParams[["resolution"]],
      "pointsize" = outputImageParams[["point.size"]]
    )
    do.call(png, imageParams)
    do.call(grid.arrange, raster_parameters)
    dev.off()
  }
}

createCombinedSankeyPlots <- function(plot_parameters, outputFileName, title = NA, legend_mm_height = 0, half_width = FALSE, addSubfigureKey = TRUE, binary = FALSE) {
  result_directory <- getSankeyResultDirectory(create = TRUE)
  single_result_directory <- getSingleSankeyResultDirectory(create = TRUE)
  outputImageParams <- getOutputImageParams(length(plot_parameters), title, legend_mm_height, half_width)

  get_plot_path <- function(plot_name, single_plot = FALSE) {
    base_directory <- result_directory
    if (single_plot) {
      base_directory <- single_result_directory
    }
    return(file.path(getwd(), base_directory, plot_name))
  }
  
  createSingleSankey <- function(plot_parameters, plot_name, outputImageParams, addSubfigureKey, binary) {
    if (binary) {
      plot_file_name <- paste0(plot_name, "_binary")
    } else {
      plot_file_name <- plot_name
    }
    plot_path <- get_plot_path(plot_file_name, single_plot = TRUE)
    title <- plot_parameters[[plot_name]][["title"]]
    if (addSubfigureKey) {
      subfigure_keys <- letters[1:length(plot_parameters)]
      subfigure_key <- subfigure_keys[match(plot_name, names(plot_parameters))]
      title <- paste0("(", subfigure_key, ") ", title) # annoying in other combinations; should not use it there
    }
    sankey_parameters <- plot_parameters[[plot_name]]
    sankey_parameters[["canvas_width"]] <- outputImageParams[["width"]] / outputImageParams[["columns"]]
    sankey_parameters[["canvas_height"]] <- outputImageParams[["height"]] / outputImageParams[["rows"]]
    sankey_parameters[["plot_path"]] <- plot_path
    sankey_parameters[["title"]] <- title
    sankey_parameters[["binary"]] <- binary
    create_sanky_plot(sankey_parameters)
    return(plot_path)
  }
  
  plotPaths <- c()
  for (index in 1:length(plot_parameters)) {
    plot_name <- names(plot_parameters)[index]
    # print(paste0("Plotting ", plot_name, "..."))
    if (grepl(
      "Residual option (negative) or number of selected options",
      plot_parameters[[plot_name]][["title"]], fixed = TRUE)) {
      next
    }
    plotPath <- createSingleSankey(plot_parameters, plot_name, outputImageParams, addSubfigureKey, binary)
    plotPaths <- c(plotPaths, plotPath)
  }
  inputFileNames <- unlist(lapply(plotPaths, get_png_path))
  combinePlots(inputFileNames, file.path(result_directory, outputFileName), title, legend_mm_height, half_width)
  combinedStats <- NULL
  for (plotPath in plotPaths) {
    plotStats <- read.csv(get_file_path(plotPath, "csv"))
    combinedStats <- rbind(combinedStats, plotStats)
  }
  combinedStats$q_values_complete <- p.adjust(combinedStats$p_values, method = "fdr")
  write.csv(combinedStats, get_file_path(file.path(result_directory, outputFileName), "csv"), row.names = FALSE)
}
