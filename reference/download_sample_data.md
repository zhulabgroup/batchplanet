# Download sample data from GitHub

This function clones the `sample-data` branch of the [BatchPlanet GitHub
repository](https://github.com/zhulabgroup/BatchPlanet) into the current
working directory.

## Usage

``` r
download_sample_data()
```

## Value

Path where the data was downloaded to.

## Details

Requires that `git` is installed and available on your system PATH. If
the `sample-data` folder already exists, the function will not overwrite
it.
