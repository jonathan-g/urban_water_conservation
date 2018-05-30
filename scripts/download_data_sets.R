download_data_from_figshare <- function(data.dir) {
  msa_file <- "msa_data.csv"
  state_file <- "state_covariates.csv"
  msa_codebook_file <- "msa_data_codebook.csv"
  state_codebook_file <- "state_covariates_codebook.csv"

  msa_data_url <- "https://ndownloader.figshare.com/files/10029751"
  state_data_url <- "https://ndownloader.figshare.com/files/10029835"
  msa_codebook_url <- "https://ndownloader.figshare.com/files/10029958"
  state_codebook_url <- "https://ndownloader.figshare.com/files/10029961"

  download.file(msa_data_url, file.path(data.dir, msa_file))
  download.file(state_data_url, file.path(data.dir, state_file))
  download.file(msa_codebook_url, file.path(data.dir, msa_codebook_file))
  download.file(state_codebook_url, file.path(data.dir, state_codebook_file))
}
