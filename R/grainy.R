#' @importFrom R6 R6Class
#' @importFrom vaster cell_from_xy xy_from_cell
#' @export
grain <- R6Class("grain",
                  public = list(
                    dimension = NULL,
                    extent = NULL,
                    initialize = function(dimension = c(1L, 1L), extent = c(0, dimension[1L], 0, dimension[2L])) {
                      self$dimension <- dimension
                      self$extent <- extent
                      self$greet()
                    },
                    align_extent = function(x, snap = "out") {
                      vaster::align_extent(x, self$dimension, self$extent, snap)
                    },
                    buffer_extent = function(res) {
                      vaster::buffer_extent(self$extent, res)
                    },
                    cell_from_col = function(col) {
                      vaster::cell_from_col(self$dimension, col)
                    },
                    cell_from_extent = function(extent) {
                       vaster::cell_from_extent(self$dimension, self$extent, extent)
                    },
                    cell_from_row = function(row) {
                      vaster::cell_from_row(self$dimension, row)
                    },
                    cell_from_col_row = function(col, row) {
                      vaster::cell_from_row_col(self$dimension, row, col)
                    },
                    cell_from_colrow_combine = function(col, row) {
                              vaster::cell_from_rowcol_combine(self$dimension, row, col)
                    },

                    cell_from_xy = function(xy) {
                      vaster::cell_from_xy(self$dimension, self$extent, xy)
                    },
                    col_from_cell = function(cell) {
                      vaster::col_from_cell(self$dimension, cell)
                    },
                    col_from_x = function(x) {
                      vaster::col_from_cell(self$dimension, x)
                    },
                    colrow_from_cell = function(cell) {
                      vaster::rowcol_from_cell(self$dimension, self$extent, cell)[,2:1, drop = FALSE]
                    },
                    extent_dimension = function(extent, snap = "out") {
                      vaster::extent_dimension(extent, self$dimension, self$extent, snap = snap)
                    },
                    extent_from_cell = function(cell) {
                      vaster::extent_from_cell(self$dimension, self$extent, cell)
                    },
                    extent_vrt = function(vrt) {
                      vaster::extent_vrt(vrt)
                    },
                    intersect_extent = function(extent) {
                      vaster::intersect_extent(extent, self$dimension, self$extent)
                    },
                    listxyz = function(data = NULL) {
                      vaster::vaster_listxyz(self$dimension, self$extent, data)
                    },
                    long = function(data = NULL, raster_order = TRUE) {
                      vaster::vaster_long(self$dimension, self$extent, data, raster_order = raster_order)
                    },

                    n_cell = function() {
                      vaster::n_cell(self$dimension)
                    },
                    n_col = function() {
                      vaster::n_col(self$dimension)
                    },
                    n_row = function() {
                      vaster::n_row(self$dimension)
                    },
                    origin = function() {
                      vaster::origin(self$dimension, self$extent)
                    },
                    plot_extent = function( ..., extent = NULL) {
                      if (is.null(extent)) extent <- self$extent
                      vaster::plot_extent(extent, ...)
                    },
                    row_from_cell = function(cell) {
                      vaster::row_from_cell(self$dimension, cell)
                    },
                    row_from_y = function(y) {
                      vaster::row_from_y(self$dimension, self$extent, y)
                    },
                    snap_extent = function(res) {
                      vaster::snap_extent(self$extent, res)
                    },
                    vcrop = function(extent, snap = "out") {
                      vaster::vcrop(extent, self$dimension, self$extent, snap = snap)
                    },
                    x_centre = function() {
                      vaster::x_centre(self$dimension, self$extent)
                    },
                    x_corner = function() {
                      vaster::x_corner(self$dimension, self$extent)
                    },
                    x_from_cell = function(cell) {
                      vaster::x_from_cell(self$dimension, self$extent, cell)
                    },
                    x_from_col = function(col) {
                      vaster::x_from_col(self$dimension, self$extent, col)
                    },
                    x_max = function() {
                      vaster::x_max(self$dimension, self$extent)
                    },
                    x_min = function() {
                      vaster::x_min(self$dimension, self$extent)
                    },
                    x_res = function() {
                      vaster::x_res(self$dimension, self$extent)
                    },
                    xlim = function() {
                      vaster::xlim(self$dimension, self$extent)
                    },
                    xy = function() {
                      vaster::xy(self$dimension, self$extent)
                    },
                    xy_from_cell = function(cell) {
                      vaster::xy_from_cell(self$dimension, self$extent, cell)
                    },
                    y_centre = function() {
                      vaster::y_centre(self$dimension, self$extent)
                    },
                    y_corner = function() {
                      vaster::y_corner(self$dimension, self$extent)
                    },
                    y_from_cell = function(cell) {
                      vaster::y_from_cell(self$dimension, self$extent, cell)
                    },
                    y_from_col = function(col) {
                      vaster::y_from_col(self$dimension, self$extent, col)
                    },
                    y_max = function() {
                      vaster::y_max(self$dimension, self$extent)
                    },
                    y_min = function() {
                      vaster::y_min(self$dimension, self$extent)
                    },
                    y_res = function() {
                      vaster::y_res(self$dimension, self$extent)
                    },
                    ylim = function() {
                      vaster::ylim(self$dimension, self$extent)
                    },

                    set_dimension = function(val) {
                      self$dimension <- val
                    },
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
