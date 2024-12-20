# Step 1

The first step includes collating monthly evapotranspiration time series from flux tower field sites. Thereafter, satellite-derived evapotranspiration products are used to acquire monthly evapotranspiration time series for the same flux tower field sites to assess how accurately it is able to estimate evapotranspiration across different bioclimates in South Africa. The field-measured evapotranspiration time series is essentially used to assess how accurate the satellite-derived products are.

This step is comprised of three parts including:

1.1) Collate field-measured ET

1.2) Collate satellite-derived ET

1.3) Plot time series of field-measured ET vs satellite-derived ET


## 1.1) Collate field-measured ET
Field-measured ET from the flux tower field sites come in different formats so certain processing must take place to ensure standardization before performing analyses.

** Conducted in R-studio using R-notebooks

These processing steps include:
- Date Conversion: converting all date/timestamp fields to the same date format
- Gap fill sense-check: the data is sense checked after the ReddyProc procedure to ensure that the original ET data remains unaltered and that only gaps are filled.
- ET daily aggregate to ET monthly: ET data is aggregated to a monthly timestep so that each site has a representative monthly ET (mm/month) time series dataset


## 1.2) Collate satellite-derived ET
Satellite-derived evapotranspiration come in different formats where the data is available at different timesteps (temporal resolutions: SMAP = 3 hourly, MOD16 = 8 day, etc.), so data must be aggregated into a standardised monthly time series. It must be noted that some satellite-derived products already provide evapotranspiration at a monthly time step. 

** Conducted in Google Colab using Python-notebooks

** WaPOR, SSEBop, and PT-JPL datasets acquired manually from the site links provided below

The satellite-product datasets are also available from different sources including the following:
- WaPOR: WaPOR v2 & v3 site [https://data.apps.fao.org/wapor/?lang=en]
- MOD16: Google Earth Engine [https://developers.google.com/earth-engine/datasets/catalog/MODIS_061_MOD16A2]
- FLDAS: Google Earth Engine [https://developers.google.com/earth-engine/datasets/catalog/NASA_FLDAS_NOAH01_C_GL_M_V001]
- SMAP: Google Earth Engine [https://developers.google.com/earth-engine/datasets/catalog/NASA_SMAP_SPL4SMGP_007]
- GLDAS: Google Earth Engine [https://developers.google.com/earth-engine/datasets/catalog/NASA_GLDAS_V022_CLSM_G025_DA1D]
- TerraClimate: Google Earth Engine [https://developers.google.com/earth-engine/datasets/catalog/IDAHO_EPSCOR_TERRACLIMATE]
- PT-JPL: AppEEARS - ECOSTRESS [https://appeears.earthdatacloud.nasa.gov/task/point]
- SSEBop: Climate Engine [https://app.climateengine.org/climateEngine]


## 1.3) Plot time series of field-measured vs satellite-derived ET

** Conducted in Google Colab using Python-notebooks.
This step plots the ET timeseries of field-measured vs satellite-derived at a monthly timestep.

### *..Details of the flux tower sites and satellite-derived evapotranspiration products can be found in the 'Images&Notes' folder in this repository..*
