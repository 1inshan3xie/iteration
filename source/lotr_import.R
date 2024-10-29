lotr_import = function(cell_range, movie_title){
  movie_df = 
    read_excel("./data/LotR_Words.xlsx", range = cell_range) |>
    mutate(movie = movie_title) |>
    janitor::clean_names() |>
    pivot_longer(
      female:male,
      names_to = "sex",
      values_to = "words"
    ) |>
    select(movie, everything())
  
  return(movie_df)
  
}