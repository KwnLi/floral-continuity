options(timeout = max(3600, getOption("timeout")))

download.file("https://zenodo.org/records/8256488/files/final_cat_2016.zip?download=1",
              destfile = "data/spatial/final_cat_2016.zip")
