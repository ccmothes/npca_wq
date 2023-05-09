work_dir <- 'data/nhdplushr/'
hr_urls <- download_nhdplushr(work_dir, "03", download_files = TRUE)
#> [1] "https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHDPlusHR/Beta/GDB/NHDPLUS_H_0601_HU4_GDB.zip"
#> [2] "https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHDPlusHR/Beta/GDB/NHDPLUS_H_0602_HU4_GDB.zip"
#> [3] "https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHDPlusHR/Beta/GDB/NHDPLUS_H_0603_HU4_GDB.zip"
#> [4] "https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHDPlusHR/Beta/GDB/NHDPLUS_H_0604_HU4_GDB.zip"

# already downloaded:
list.files(hr_data_dir)
