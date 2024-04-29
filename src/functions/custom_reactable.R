custom_reactable <- function(bd, ...){
  reactable(
    bd,
    defaultColDef = colDef(
      align = "center",
      headerStyle = list(background = "#f7f7f8")), 
    defaultPageSize = 5, onClick = 'select',
    bordered = T, highlight = T, resizable = T, showSortable = T,
    ...
  )
}