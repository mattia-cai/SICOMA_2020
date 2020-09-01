# Write list to excel
write_list <-function( my_list, wb_name ) {
  # Save to excel
  wb <- createWorkbook()
  fun <- function( data, name ){
    addWorksheet( wb, name )
    writeData( wb, name, data )
  }
  Map( fun, my_list, names( my_list ) )
  saveWorkbook( wb, file = wb_name, overwrite = TRUE )
}