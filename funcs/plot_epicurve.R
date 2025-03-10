# グラフ描画をする関数を定義する

plot_epicurve <- function(data, district = "All", agegroup = "malaria_tot") {
  # グラフのタイトルを作成する
  if (!("All" %in% district)) {
    data <- data %>%
      filter(District %in% district)

    plot_title_district <- stringr::str_glue("{paste0(district, collapse = ', ')} districts")
  } else {
    plot_title_district <- "all districts"
  }

  # データが残っていなければNULLを返す
  if (nrow(data) == 0) {
    return(NULL)
  }

  # 年齢群で抽出する
  data <- data %>%
    filter(age_group == agegroup)

  # データが残っていなければNULLを返す
  if (nrow(data) == 0) {
    return(NULL)
  }

  if (agegroup == "malaria_tot") {
    agegroup_title <- "All ages"
  } else {
    agegroup_title <- stringr::str_glue("{str_remove(agegroup, 'malaria_rdt')} years")
  }

  ggplot(data, aes(x = data_date, y = cases_reported)) +
    geom_col(width = 1, fill = "darkred") +
    theme_minimal() +
    labs(
      x = "date",
      y = "number of cases",
      title = stringr::str_glue("Malaria cases - {plot_title_district}"),
      subtitle = agegroup_title
    )
}
