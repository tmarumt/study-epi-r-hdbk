library(rio)
library(here)
library(tidyverse)
library(sf)
library(tmap)
library(janitor)
library(OpenStreetMap)
library(spdep)

# import clean case linelist
linelist <- import(here("data", "linelist_cleaned.rds"))

# ラインリストの行数から1000個のランダムな行番号を生成
sample_rows <- sample(nrow(linelist), 1000)

# サンプル行の全ての列だけを保持するラインリストにサブセットする
linelist <- linelist[sample_rows, ]

# sfオブジェクトを生成
linelist_sf <- linelist %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Admin Level 3 のシェープファイルを R に保存
sle_adm3_raw <- read_sf(here("data", "sle_adm3.shp"))

# ADM3 level clean
sle_adm3 <- sle_adm3_raw %>%
  janitor::clean_names() %>% # 列名の標準化
  dplyr::filter(admin2name %in% c("Western Area Urban", "Western Area Rural")) # 特定の地域を残してフィルター

# ADM3 ごとの人口
sle_adm3_pop <- import(here("data", "sle_admpop_adm3_2020.csv")) %>%
  janitor::clean_names()

# OSM 医療施設の shapefile
sle_hf <- sf::read_sf(here("data", "sle_hf.shp")) %>%
  janitor::clean_names() %>%
  dplyr::filter(amenity %in% c("hospital", "clinic", "doctors"))

tmap_mode("plot") # "view" または "plot" を選択

# 症例（点）のみ
tm_shape(linelist_sf) + tm_dots(size = 0.08, col = "blue")

# 行政境界（ポリゴン）のみ
tm_shape(sle_adm3) +               # 行政境界のシェープファイル
  tm_polygons(col = "#F7F7F7")+    # ポリゴンを薄い灰色で表示
  tm_borders(col = "#000000",      # 境界を色と線の太さで表示
             lwd = 2) +
  tm_text("admin3name")            # 各ポリゴンについて表示する列テキスト

# 上と同様、ただしバウンディングボックス引数から縮尺を指定
tm_shape(sle_adm3,
         bbox = c(-13.3, 8.43,    # 角
                  -13.2, 8.5)) +  # 角
  tm_polygons(col = "#F7F7F7") +
  tm_borders(col = "#000000", lwd = 2) +
  tm_text("admin3name")

# すべてまとめる
tm_shape(sle_adm3, bbox = c(-13.3, 8.43, -13.2, 8.5)) + #
  tm_polygons(col = "#F7F7F7") +
  tm_borders(col = "#000000", lwd = 2) +
  tm_text("admin3name") +
  tm_shape(linelist_sf) +
  tm_dots(size = 0.08, col = "blue", alpha = 0.5) +
  tm_layout(title = "Distribution of Ebola cases") # 地図にタイトルを付ける

linelist_adm <- linelist_sf %>%
  # 空間交差に基づいて行政境界を linelist に結合
  sf::st_join(sle_adm3, join = st_intersects)

linelist_adm <- linelist_sf %>%
  # 空間交差に基づいて、行政境界ファイルをラインリストに結合
  sf::st_join(sle_adm3, join = st_intersects) %>%
  # 古い列名はそのままに、新たに2つの admin の列名を追加
  select(names(linelist_sf), admin3name, admin3pcod)

# これで、各症例に ADM3 名がついたことが確認できます。
linelist_adm %>% select(case_id, admin3name, admin3pcod)

# 行政単位ごとに症例の数を含むデータフレームを新規作成
case_adm3 <- linelist_adm %>% # 新しい admin 列を含むラインリストで始める
  as_tibble() %>% # 見やすいように tibble に変換
  group_by(admin3pcod, admin3name) %>% # 名前と pcode で行政単位をグループ化
  summarise(cases = n()) %>% # 要約と行数のカウント
  arrange(desc(cases)) # 下り順に並べ替え

case_adm3

ggplot(
  data = linelist_adm, # admin unit 情報を含むラインリストで始める
  mapping = aes(
    x = fct_rev(fct_infreq(admin3name))
  )
) + # x軸は行政単位、件数で並べ替え
  geom_bar() + # 帽を作成、高さは行数
  coord_flip() + # adm を読みやすくするためにXとYを入れ替える
  theme_classic() + # 背景を単純に
  labs( # タイトルとラベル
    x = "Admin level 3",
    y = "Number of cases",
    title = "Number of cases, by adminstative unit",
    caption = "As determined by a spatial join, from 1000 randomly sampled cases from linelist"
  )

# 各症例に最も近い医療施設
linelist_sf_hf <- linelist_sf %>% # linelist shapefile で始める
  st_join(sle_hf, join = st_nearest_feature) %>% # 症例データから最も近い診療所からのデータ
  select(case_id, osm_id, name, amenity) %>% # 残しておくべき列、例えば id, name, type, と医療施設の位置情報
  rename("nearest_clinic" = "name") # 分かりやすいように名前を変更

# 医療施設ごとに症例を数える
hf_catchment <- linelist_sf_hf %>% # 最寄りの診療所データを含むラインリストで始める
  as.data.frame() %>% # shapefile をデータフレームに変換
  count(nearest_clinic, # （診療所の）"name" で行を数える
    name = "case_n"
  ) %>% # 数えたデータの列に "case_n" と命名
  arrange(desc(case_n)) # 下がり順で並べ替え

hf_catchment # console に表示

tmap_mode("view") # tmap モードをインタラクティブに設定

# 症例と診療所の点をプロット
tm_shape(linelist_sf_hf) + # 症例をプロット
  tm_dots(
    size = 0.08, # 最も近い診療所で症例を色分け
    col = "nearest_clinic"
  ) +
  tm_shape(sle_hf) + # 診療所を大きい黒い点でプロット
  tm_dots(size = 0.3, col = "black", alpha = 0.4) +
  tm_text("name") + # 施設名でオーバーレイ
  tm_view(
    set.view = c(-13.2284, 8.4699, 13), # 縮尺を調整 (中心座標, zoom)
    set.zoom.limits = c(13, 14)
  ) +
  tm_layout(title = "Cases, colored by nearest clinic")

sle_hf_2k <- sle_hf %>%
  st_buffer(dist = 0.02) # 約2.5kmに度数を変換

tmap_mode("plot")
# 円バッファを作成
tm_shape(sle_hf_2k) +
  tm_borders(col = "black", lwd = 2) +
  tm_shape(sle_hf) + # 診療所を大きい赤丸でプロット
  tm_dots(size = 0.3, col = "black")

# バッファで症例を交差
linelist_sf_hf_2k <- linelist_sf_hf %>%
  st_join(sle_hf_2k, join = st_intersects, left = TRUE) %>%
  filter(osm_id.x == osm_id.y | is.na(osm_id.y)) %>%
  select(case_id, osm_id.x, nearest_clinic, amenity.x, osm_id.y)

# どの医療施設バッファとも交差しなかった症例
linelist_sf_hf_2k %>%
  filter(is.na(osm_id.y)) %>%
  nrow()

tmap_mode("view")

# まず症例を点で表示
tm_shape(linelist_sf_hf) +
  tm_dots(size = 0.08, col = "nearest_clinic") +

  # 診療所を大きい黒点でプロット
  tm_shape(sle_hf) +
  tm_dots(size = 0.3, col = "black") +

  # 医療施設バッファをポリラインで重ねる
  tm_shape(sle_hf_2k) +
  tm_borders(col = "black", lwd = 2) +

  # どの医療施設バッファにもない症例を赤点で強調
  tm_shape(linelist_sf_hf_2k %>% filter(is.na(osm_id.y))) +
  tm_dots(size = 0.1, col = "red") +
  tm_view(set.view = c(-13.2284, 8.4699, 13), set.zoom.limits = c(13, 14)) +

  # タイトルを追加
  tm_layout(title = "Cases by clinic catchment area")

# 人口データを追加し、１万人当たりの症例数を計算
case_adm3 <- case_adm3 %>%
  left_join(sle_adm3_pop, # 人口データから列を追加
    by = c("admin3pcod" = "adm3_pcode")
  ) %>% # 二つの列の共通の値に基づく結合
  select(names(case_adm3), total) %>% # 重要な列のみ保持、総人口など
  mutate(case_10kpop = round(cases / total * 10000, 3)) # 10000あたりの症例数の列を作成、小数点以下３桁で四捨五入

case_adm3 # console に表示

case_adm3_sf <- case_adm3 %>% # 行政単位での症例と感染率で始める
  left_join(sle_adm3, by = "admin3pcod") %>% # shapefile データと共通列で結合
  select(objectid, admin3pcod, # 指定した列のみ保持
    admin3name = admin3name.x, # 一つの列名をきれいにする
    admin2name, admin1name,
    cases, total, case_10kpop,
    geometry
  ) %>% # プロットできるように座標を保持
  drop_na(objectid) %>% # NA 行を取り除く
  st_as_sf() # shapefile に変換

case_adm3_sf <- na.omit(case_adm3_sf)

# tmap mode
tmap_mode("plot") # 静的地図を表示

# ポリゴンをプロット
tm_shape(case_adm3_sf) +
  tm_polygons("cases") + # 症例数の列で色分け
  tm_text("admin3name") # 表示に名前を付ける

# 1万人あたりの症例
tmap_mode("plot") # 静的ビューモード

# プロット
tm_shape(case_adm3_sf) + # ポリゴンをプロット
  tm_polygons("case_10kpop", # 症例率を含む列で色分け
    breaks = c(0, 10, 50, 100), # 色分けの値を定義
    palette = "Purples" # 紫色のカラーパレットを使用
  ) +
  tm_text("admin3name") # テキストを表示

sle_adm3

sle_adm3_dat <- sle_adm3 %>%
  inner_join(case_adm3, by = "admin3pcod") # inner join = どちらのデータオブジェクトにもある場合にのみ残す

select(sle_adm3_dat, admin3name.x, cases) # console に選択した変数を表示

ggplot(data = sle_adm3_dat) +
  geom_col(aes(
    x = fct_reorder(admin3name.x, cases, .desc = T), # 'cases' 下がり順でx軸を並べ替え
    y = cases
  )) + # y軸は地域ごとの症例数
  theme_bw() +
  labs( # 図のテキストを設定
    title = "Number of cases, by administrative unit",
    x = "Admin level 3",
    y = "Number of cases"
  ) +
  guides(x = guide_axis(angle = 45)) # 見やすいようにx軸を45度傾ける

ggplot(data = sle_adm3_dat) +
  geom_sf(aes(fill = cases)) # 症例数で塗りつぶしが変化するように設定

ggplot(data = sle_adm3_dat) +
  geom_sf(aes(fill = cases)) +
  scale_fill_continuous(high = "#54278f", low = "#f2f0f7") + # 色の段階を変更
  theme_bw() +
  labs(
    title = "Number of cases, by administrative unit", # 図のテキストを設定
    subtitle = "Admin level 3"
  )

# 緯度経度の範囲に基図を切り取る。タイル種別を選択。
map <- OpenStreetMap::openmap(
  upperLeft = c(max(linelist$lat, na.rm = T), max(linelist$lon, na.rm = T)), # 基図のタイルの制限
  lowerRight = c(min(linelist$lat, na.rm = T), min(linelist$lon, na.rm = T)),
  zoom = NULL,
  type = c("osm", "stamen-toner", "stamen-terrain", "stamen-watercolor", "esri", "esri-topo")[1]
)

autoplot.OpenStreetMap(map)

# 座標系 WGS84
map_latlon <- openproj(map, projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# 地図をプロット。ggplot を使うためには、"autoplot" が必須。
autoplot.OpenStreetMap(map_latlon)

# 地図をプロット。ggplot と使うためには autoplot が必須。
autoplot.OpenStreetMap(map_latlon) + # 基図から始める
  geom_point( # ラインリストの経度と緯度の列から xy の点を追加
    data = linelist,
    aes(x = lon, y = lat),
    size = 1,
    alpha = 0.5,
    show.legend = FALSE
  ) + # 凡例を完全に削除
  labs(
    x = "Longitude", # タイトルとラベル
    y = "Latitude",
    title = "Cumulative cases"
  )

# 基図から始める
autoplot.OpenStreetMap(map_latlon) +

  # 密度プロットを追加
  ggplot2::stat_density_2d(
    data = linelist,
    aes(
      x = lon,
      y = lat,
      fill = ..level..,
      alpha = ..level..
    ),
    bins = 10,
    geom = "polygon",
    contour_var = "count",
    show.legend = F
  ) +

  # カラースケールを指定
  scale_fill_gradient(low = "black", high = "red") +

  # ラベル
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "Distribution of cumulative cases"
  )

# 発症した月を抽出
linelist <- linelist %>%
  mutate(date_onset_ym = format(date_onset, "%Y-%m"))

# 値を調査
table(linelist$date_onset_ym, useNA = "always")

# 基図から始める
autoplot.OpenStreetMap(map_latlon) +

  # 密度プロットを追加
  ggplot2::stat_density_2d(
    data = linelist,
    aes(
      x = lon,
      y = lat,
      fill = ..level..,
      alpha = ..level..
    ),
    bins = 10,
    geom = "polygon",
    contour_var = "count",
    show.legend = F
  ) +

  # カラースケールを指定
  scale_fill_gradient(low = "black", high = "red") +

  # ラベル
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "Distribution of cumulative cases over time"
  ) +

  # 発症した month-year でプロットを分割
  facet_wrap(~date_onset_ym, ncol = 4)

sle_nb <- spdep::poly2nb(sle_adm3_dat, queen = T) # 隣接を作成
sle_adjmat <- spdep::nb2mat(sle_nb) # 隣接関係をまとめた行列の作成
sle_listw <- spdep::nb2listw(sle_nb) # listw (重みのリスト) オブジェクトを作成 -- 後で使います

sle_nb

round(sle_adjmat, digits = 2)

plot(sle_adm3_dat$geometry) + # 地域境界をプロット
  spdep::plot.nb(sle_nb, as(sle_adm3_dat, "Spatial"), col = "grey", add = T) # 隣接関係を追加

moran_i <- spdep::moran.test(sle_adm3_dat$cases, # 指定の変数で数値ベクトル
  listw = sle_listw
) # 隣接関係を要約したlistwオブジェクト

moran_i # Moran's I 検定の結果を表示

# local Moran's I を計算
local_moran <- spdep::localmoran(
  sle_adm3_dat$cases, # 指定の変数
  listw = sle_listw # 隣接の重み付けをした listw オブジェクト
)

# 結果を sf データに結合
sle_adm3_dat <- cbind(sle_adm3_dat, local_moran)

# 地図をプロット
ggplot(data = sle_adm3_dat) +
  geom_sf(aes(fill = Ii)) +
  theme_bw() +
  scale_fill_gradient2(
    low = "#2c7bb6", mid = "#ffffbf", high = "#d7191c",
    name = "Local Moran's I"
  ) +
  labs(
    title = "Local Moran's I statistic for Ebola cases",
    subtitle = "Admin level 3 regions, Sierra Leone"
  )

# local G 分析を実行
getis_ord <- spdep::localG(
  sle_adm3_dat$cases,
  sle_listw
)

# 結果を sf データに結合
sle_adm3_dat$getis_ord <- as.numeric(getis_ord)

# 地図をプロット
ggplot(data = sle_adm3_dat) +
  geom_sf(aes(fill = getis_ord)) +
  theme_bw() +
  scale_fill_gradient2(
    low = "#2c7bb6", mid = "#ffffbf", high = "#d7191c",
    name = "Gi*"
  ) +
  labs(
    title = "Getis-Ord Gi* statistic for Ebola cases",
    subtitle = "Admin level 3 regions, Sierra Leone"
  )

sle_adm3_dat <- sle_adm3_dat %>%
  rename(population = total) # 列名 total を population に変更

tmap_mode("plot")

cases_map <- tm_shape(sle_adm3_dat) + tm_polygons("cases") +
  tm_layout(main.title = "Cases")
pop_map <- tm_shape(sle_adm3_dat) +
  tm_polygons("population") +
  tm_layout(main.title = "Population")

tmap_arrange(cases_map, pop_map, ncol = 2) # arrange into 2x1 facets

lee_test <- spdep::lee.test(
  x = sle_adm3_dat$cases, # 比較する変数１
  y = sle_adm3_dat$population, # 比較する変数２
  listw = sle_listw # 隣接重みづけのある listw オブジェクト
)

lee_test
