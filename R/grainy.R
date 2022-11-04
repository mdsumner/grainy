#' Grain
#' Encapsulate a grid, with dimension and extent.
#'
#' A grid is a raster, a domain with an underlying regular discretization in xy.
#' @importFrom R6 R6Class
#' @importFrom vaster align_extent buffer_extent cell_from_col cell_from_extent cell_from_row cell_from_row_col cell_from_rowcol_combine cell_from_xy col_from_cell col_from_cell extent_dimension extent_from_cell extent_vrt intersect_extent n_cell n_col n_row origin plot_extent row_from_cell row_from_y rowcol_from_cell snap_extent vaster_listxyz vaster_long vcrop x_centre x_corner x_from_cell x_from_col x_max x_min x_res xlim xy xy_from_cell y_centre y_corner y_from_cell y_from_row y_max y_min y_res ylim
#' @export
#' @name grain
#' @param dimension ncol, nrow
#' @param extent xmin,xmax,ymin,ymax
#' @param x x coordinate
#' @param snap "out", "in", "near"
#' @param res resolution
#' @param col column index
#' @param row row index
#' @param xy xy coordinates
#' @param cell cell index
#' @param world create a world longlat grid with 1 degree cells
#' @param vrt vrt file path
#' @param raster_order raster order or traditional R matrix order
#' @param data optional data values for output
#' @param ... args passed to [graphics::plot()]
#' @param y y coordinate
#' @field dimension dimension (nx, ny) of grid, number of columns, number of rows, defaults to 1x1 (unless 'world = TRUE')
#' @field extent extent of grid xmin,xmax,ymin,ymax - defaults to 0,ncol,0,nrow
grain <- R6Class("grain",
                  public = list(
                    dimension = NULL,
                    extent = NULL,


                    #' @examples
                    #' grain$new(c(360, 180), c(-180, 180, -90, 90))
                    #'  grain$new(c(360, 180), c(-180, 180, -90, 90))$align_extent(c(-179.5, 179, 0.03, 10))
                    #' g <- grain$new(world = TRUE)
                    #' g$cell_from_col(2)

                    initialize = function(dimension = NULL, extent = NULL, world = FALSE) {
                      if (is.null(dimension)) dimension = c(1L, 1L)
                      if (is.null(extent)) extent = c(0, dimension[1L], 0, dimension[2L])
                      if (world) {
                        dimension <- c(360, 180)
                        extent <- c(-180, 180, -90, 90)
                      }
                      self$dimension <- dimension
                      self$extent <- extent
                      self$greet()
                    },


                    #' @return aligned extent
                    align_extent = function(x, snap = "out") {
                      vaster::align_extent(x, self$dimension, self$extent, snap)
                    },
                    # Buffer extent
                    #
                    # Snap extent to grain of grid
                    # @return buffered extent
                    # @examples
                    # g <- grain$new(world = TRUE)
                    # g$buffer_extent(22)
                    #buffer_extent = function(res) {
                    #  vaster::buffer_extent(self$extent, res)
                    #},
                    #' @return cell index, extent, or coordinate
                    cell_from_col = function(col) {
                      vaster::cell_from_col(self$dimension, col)
                    },
                    #' @return cell index
                    #' @examples
                    #' g <- grain$new(world = TRUE)
                    #' g$cell_from_extent(c(147, 148, -43, -41))
                    cell_from_extent = function(extent) {
                       vaster::cell_from_extent(self$dimension, self$extent, extent)
                    },
                    #' @return cell index
                    cell_from_row = function(row) {
                      vaster::cell_from_row(self$dimension, row)
                    },
                    #' @return cell index
                    cell_from_col_row = function(col, row) {
                      vaster::cell_from_row_col(self$dimension, row, col)
                    },
                    #' @return cell index
                    cell_from_colrow_combine = function(col, row) {
                              vaster::cell_from_rowcol_combine(self$dimension, row, col)
                    },
                    #' @return cell index
                    cell_from_xy = function(xy) {
                      vaster::cell_from_xy(self$dimension, self$extent, xy)
                    },
                    #' @return cell index
                    col_from_cell = function(cell) {
                      vaster::col_from_cell(self$dimension, cell)
                    },
                    #' @return col index
                    col_from_x = function(x) {
                      vaster::col_from_cell(self$dimension, x)
                    },
                    #' @return col,row index
                    colrow_from_cell = function(cell) {
                      vaster::rowcol_from_cell(self$dimension, self$extent, cell)[,2:1, drop = FALSE]
                    },
                    #' @return dimension
                    extent_dimension = function(extent, snap = "out") {
                      vaster::extent_dimension(extent, self$dimension, self$extent, snap = snap)
                    },
                    #' @return extent
                    extent_from_cell = function(cell) {
                      vaster::extent_from_cell(self$dimension, self$extent, cell)
                    },
                    #' @return extents, in a matrix, xmin,xmax,ymin,ymax
                    extent_vrt = function(vrt) {
                      vaster::extent_vrt(vrt)
                    },
                    #' @return extent
                    intersect_extent = function(extent) {
                      vaster::intersect_extent(extent, self$dimension, self$extent)
                    },
                    #' @return list of x,y vectors and z matrix (like [graphics::image()])
                    listxyz = function(data = NULL) {
                      vaster::vaster_listxyz(self$dimension, self$extent, data)
                    },
                    #' @return data frame of x, y and optionaly z value
                    long = function(data = NULL, raster_order = TRUE) {
                      vaster::vaster_long(self$dimension, self$extent, data, raster_order = raster_order)
                    },
                    #' @return integer, number of cells
                    n_cell = function() {
                      vaster::n_cell(self$dimension)
                    },
                    #' @return integer, number of columns
                    n_col = function() {
                      vaster::n_col(self$dimension)
                    },
                    #' @return integer, number of rows
                    n_row = function() {
                      vaster::n_row(self$dimension)
                    },
                    #' @return coordinate of grid origin
                    origin = function() {
                      vaster::origin(self$dimension, self$extent)
                    },
                    #' @return nothing, called for side effect of plot
                    plot_extent = function( ..., extent = NULL) {
                      if (is.null(extent)) extent <- self$extent
                      vaster::plot_extent(extent, ...)
                    },
                    #' @return integer of row index
                    row_from_cell = function(cell) {
                      vaster::row_from_cell(self$dimension, cell)
                    },
                    #' @return integer of row index
                    row_from_y = function(y) {
                      vaster::row_from_y(self$dimension, self$extent, y)
                    },
                    #' @return extent
                    snap_extent = function(res) {
                      vaster::snap_extent(self$extent, res)
                    },
                    #' @return extent, dimension cropped (and snapped to grain)
                    vcrop = function(extent, snap = "out") {
                      vaster::vcrop(extent, self$dimension, self$extent, snap = snap)
                    },
                    #' @return x coordinates of centre of cell
                    x_centre = function() {
                      vaster::x_centre(self$dimension, self$extent)
                    },
                    #' @return x coordinates of corners (edges) of cell
                    x_corner = function() {
                      vaster::x_corner(self$dimension, self$extent)
                    },
                    #' @return x coordinate of cell (centre)
                    x_from_cell = function(cell) {
                      vaster::x_from_cell(self$dimension, self$extent, cell)
                    },
                    #' @return x of column (centre)
                    x_from_col = function(col) {
                      vaster::x_from_col(self$dimension, self$extent, col)
                    },
                    #' @return maximum x (right of grid)
                    x_max = function() {
                      vaster::x_max(self$dimension, self$extent)
                    },
                    #' @return minimum x (left of grid)
                    x_min = function() {
                      vaster::x_min(self$dimension, self$extent)
                    },
                    #' @return x resolution (width of cell)
                    x_res = function() {
                      vaster::x_res(self$dimension, self$extent)
                    },
                    #' @return x range of grid (leftmost edge to rightmost edge)
                    xlim = function() {
                      vaster::xlim(self$dimension, self$extent)
                    },
                    #' @return coordinates of cells (centre)
                    xy = function() {
                      vaster::xy(self$dimension, self$extent)
                    },

                    #' @examples
                    #' g <- grain$new(world = TRUE)
                    #' g$xy_from_cell(g$cell_from_extent(c(147, 148, -43, -41)))
                    xy_from_cell = function(cell) {
                      vaster::xy_from_cell(self$dimension, self$extent, cell)
                    },
                    #' @return y coordinate (centre)
                    y_centre = function() {
                      vaster::y_centre(self$dimension, self$extent)
                    },
                    #' @return y coordinates (corners)
                    y_corner = function() {
                      vaster::y_corner(self$dimension, self$extent)
                    },
                    #' @return y coordinate of cell (centre)
                    y_from_cell = function(cell) {
                      vaster::y_from_cell(self$dimension, self$extent, cell)
                    },
                    #' @return y coordinate of row (centre)
                    y_from_row = function(row) {
                      vaster::y_from_row(self$dimension, self$extent, row)
                    },
                    #' @return maximum y coordinate of grid (top edge)
                    y_max = function() {
                      vaster::y_max(self$dimension, self$extent)
                    },
                    #' @return minimum y coordinate of grid (left edge)
                    y_min = function() {
                      vaster::y_min(self$dimension, self$extent)
                    },
                    #' @return resolution y of grid (height of cell)
                    y_res = function() {
                      vaster::y_res(self$dimension, self$extent)
                    },
                    #' @return extent of grid in y (bottom most edge to topmost edge)
                    ylim = function() {
                      vaster::ylim(self$dimension, self$extent)
                    },
                    #' @return hello
                    greet = function() {
                      cat(paste0("Hello, my name is ", self$dimension, ".\n"))
                    }
                  )
)
# grain <- R7::new_class("grain",
#                    properties = list(
#                      dimension = R7::class_integer,
#                      extent = R7::class_double
#                    ),
#                    validator = function(self) {
#                      if (length(self@dimension) != 2L) {
#                        "@dimension must be length 1"
#                      } else if (length(self@extent) != 4L) {
#                        "@extent must be length 4"
#                      } else if (anyNA(self@dimension) || any(self@dimension < 1L)) {
#                        "@dimension must be valid values >= 1"
#                      }else if (anyNA(self@extent) || any(diff(self@extent)[c(1L, 3L)]) <= 0) {
#                        "@extent must be valid values xmin,xmax,ymin,ymax (xmin < xmax, ymin < ymax)"
#                      }
#                    }
# )
