# faamr

## Description

`faamr` is an R package that is used to download, read and process data from the Facility for Airborne Atmospheric Measurements ([FAAM](https://www.faam.ac.uk/)). It is not an official FAAM project, users should familiarise themselves with the [FAAM documentation](https://www.faam.ac.uk/data-centre/usage/) when working with these data.

The motivation behind this package is to provide the necessary tools for reproducible workflows when using FAAM data. As such it focuses on downloading and reading the FAAM data from their source on the Centre for Environmental Data Analysis ([CEDA](https://ceda.ac.uk/)) repository, and providing ways to read these files into R. The coverage for read functions is not exhaustive, and pull requests are welcomed to extend functionality.

## Install

```
remotes::install_github("wacl-york/faamr")
```

## Data Download Prerequisite

Users will require a CEDA account to download FAAM data. While these data are public, `faamr` makes use of the [openDAP](https://help.ceda.ac.uk/article/4431-ceda-archive-web-download-and-services) endpoint, which requires authentication. Users can register for an account on the [CEDA website](https://services.ceda.ac.uk/cedasite/register/info/). 

You will need to provide your credentials to `faamr::flight_download()`. To avoid saving these credentials in your scripts, you can use the [keyring](https://cloud.r-project.org/web/packages/keyring/index.html) package to make use of your operating systems credential store.

>[!NOTE]
>At the time of writing the CEDA openDAP fails when passwords contain non-ascii characters. If you encounter issues, first try generating a [token manually](https://services-beta.ceda.ac.uk/account/token/). If you receive a 503 error, try changing your password to contain only alphanumeric characters and simple punctuation.

## Usage

### Downloading

`faamr` tries to replicate the CEDA folder structure locally, so that it is clear where data has come from and data can be updated easily if their are revisions made to the upstream files. It is recommended that you keep this data unchanged, and save any processed data elsewhere in your project. This example will assume the following directory structure:

```
projectRoot
└── data
    ├── faam_raw
    └── processed
```

First, one needs to identify the flights of interest. FAAM flights are organised by flight number, and a list of all flights available on CEDA can be shown by running `list_flights()`. This will return a list of flight numbers and the dates of flights. In the future this package will attempt to provide more metadata about each flight, but for now users can identify flights using the [CEDA Flight finder](https://flight-finder.ceda.ac.uk/).

Then, the data available for a flight can be listed using `list_flight_data()` e.g:

```R
list_flight_data("c253")
```

returns

```R
── C251 - ACRUISE-2  Flight ───────────────────────────────────────────────────────────────────────────────────────────────
→ Flight Folder
  • file: 00README  - 00README
  • file: 00README_catalogue_and_licence.txt  - 00README.txt
  • file: flight-sum_faam_20210924_r0_c251.csv, .html, .kml, .txt  - flight-sum.csv, .html, .kml, .txt
  • file: instrument-report_faam_20210924_r0_c251.json, .txt  - instrument-report.json, .txt
  • dir: core_processed
  • dir: core_raw
  • file: flight-report_faam_20210924_r0_c251.pdf
  • dir: non-core
✔ Core raw
  • file: core_faam_20210924_r1_c251_rawdlu.zip  - core_rawdlu.zip
  • file: flight-cst_faam_20210924_r1_c251.yaml  - flight-cst.yaml
✔ Core Processed
  • file: core_faam_20210924_v005_r1_c251.nc  - core.nc
  • file: core_faam_20210924_v005_r1_c251_1hz.nc  - core_1hz.nc
  • dir: faam-video
✔ Non-core
  • file: 00README  - 00README
  • file: faam-fgga_faam_20210924_r0_c251.na  - faam-fgga.na
✖ Met Office Non-core
```

The root of the flight folder is shown in full, and then the sub directories `core_raw`, `core_processed` `non-core` and `mo-non-core` are listed (or shown to not be present). Files with multiple file types, such as the flight summary are grouped together, and only the highest revision of a file is shown. The "file type" is shown after the hyphen on each row. This is the file name without the flight number, version and revision information included, so it is a flight agnostic identifier, allowing for data from multiple flights to be downloaded at once. This file type is what must be provided to the `flight_download()`

Once flight numbers and data have been identified, a download can be triggered:

```R
flight_download(
    flight = c("c253", "c254"),
    user = "ceda_username",
    pass = keyring::key_get("ceda", "ceda_username"),
    dirOut = "data/faam_raw",
    files = c("flight-sum.txt" , "core_1hz.nc", "faam-fgga.na")
)
```

This will download flight summaries, the core 1 Hz file and the fGGA data, creating folders below the location of `dirOut`:

```
projectRoot
└── data
    ├── faam_raw
    │   ├── c251
    │   │   ├── Core Processed
    │   │   │   └── core_faam_20210927_v005_r2_c253_1hz.nc
    │   │   └── Non-core
    │   │   │    └── faam-fgga_faam_20210927_r0_c253.na
    │   │   └── flight-sum_faam_20210927_r0_c253.txt
    │   └── c252
    │       ├── Core Processed
    │       │   └── core_faam_20210928_v005_r2_c254_1hz.nc
    │       └── Non-core
    │       │    └── faam-fgga_faam_20210928_r0_c254.na
    │       └── flight-sum_faam_20210928_r0_c254.txt
    └── processed
```

### Reading

Several functions are provided to read data into R. A key part of this is that the timestamp is stored as 64 bit resolution in the `date` column using the [nanotime](https://cran.r-project.org/web/packages/nanotime/index.html) package. Some functions provide options for resampling of data on read, by providing a string that is interpretable using `nanotime::as.nanoduration()` e.g. to resample to 1 Hz `averageNanoString = "00:00:01"`, and for 10 Hz `averageNanoString = "00:00:00.1`. Users should see individual function documentation for more information, and be aware of how data flags are or are not managed when resampling. 

#### Current Read functions

- `read_faam_core()` - for core\.nc and core_1hz\.nc
- `read_faam_nitrates()` - for core-nitrates\.nc
- `read_faam_fgga()` - for faam-fgga\.na
- `read_yorknox_data()` - for york-aqdnox\.na
- `read_flight_summary()` - for flight_summary\.txt

This list can be returned by `faam_file_lookup()`


### Helpers

Some helper functions are also supplied:

- `faam_core_summary()` - returns a list of all the variables in a FAAM core file, including what samples per second (sps) grid they are on when not looking at the 1 Hz version.
- `get_core_date_origin()` - returns the date value of the "seconds since" time dimention in the core NCDF.
- `get_weight_off_wheels()` - returns a data\.frame of the time where WOW_IND (a flag for when there is or is not weight on the wheels of the aircraft) is zero. Useful for a coarse start / end time filter when using `read_faam_core()`.
- `read_nasa_ames_header()` migrated to `willdrysdale/nasaAmesR` in the future.