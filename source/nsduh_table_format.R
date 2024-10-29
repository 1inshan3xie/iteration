nsduh_table_format = function(html, table_number, table_name) {
  out_table = 
    html |> 
    html_table() |> 
    nth(table_number) |>
    slice(-1) |> 
    mutate(drug = table_name)
  
  return(out_table)
}