
project_dir <- "./"

# create regionalisation project directories
data_dir <- paste(project_dir, "data/raw", sep = "/")
dir.create(data_dir, recursive = TRUE)

hyde_dir <- paste(data_dir, "hyde", sep = "/")
dir.create(hyde_dir)

url_temp <- "https://geo.public.data.uu.nl/vault-hyde-data/HYDE%203.2%5B1648738557%5D/original/anthromes.zip"
download.file(url = url_temp,
              destfile = paste(hyde_dir, "anthromes.zip", sep = "/"),
              mode = "wb")

unzip(paste(hyde_dir, "anthromes.zip", sep = "/"),
      exdir = hyde_dir)

url_temp <- "https://geo.public.data.uu.nl/vault-hyde-data/HYDE%203.2%5B1648738557%5D/original/general_files.zip"
download.file(url = url_temp,
              destfile = paste(hyde_dir, "general_files.zip", sep = "/"),
              mode = "wb")

unzip(paste(hyde_dir, "general_files.zip", sep = "/"),
      exdir = hyde_dir)