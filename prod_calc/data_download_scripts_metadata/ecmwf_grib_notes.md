# gdalinfo summary for ecmwf data as .grib

Coordinate system:

Spheroid = sphere, 6367470,0
Prime meridian = Greenwich, 0
Units = Degree, 0.017453292199433
Pixel size 0.75*0.75 

Relevant bands:

* Band 1: Top solar radiation (W*s/m^2) - 178
* Band 2: Max 2m temp since previous post-processing (C) - 201
* Band 3: Min 2m temp since previous post-processing (C) - 202
* Band 4: Top net solar radiation clear sky (W/m^2) - 208
* Band 5: Surface net solar radiation clear sky (W/m^2) - 210
* Band 6: Total precipitation (m) - 228

After that it repeats, but not sure how it works

GRIB_FORECAST_SECONDS increases by 10800 with each repeat. This is 180 minutes or 3 hours, the measurement interval on this dataset

The dataset goes up to 43200 GRIB_FORECAST_SECONDS, i.e. 12 hours of measurements. So where are the other 12 hours? Also, each of these datasets is meant to hold data for a month of days.

So each column in the R SpatialDataFrame contains data recorded for a given band at a given time on a given date 

I checked this by subtracting the GRIB_VALID_TIME for the first entry from the GRIB_VALID_TIME from the last entry, then dividing this by the total number of seconds in a day (86400) is 30.875:

```r
(1514764800 - 1512097200) / 86400
```

This is basically 31 days, which is what we want.

Also, the total number of bands is 1488. This, divided by 6 (number of variables) and then divided by 8 (number of measurements per day) is 31, which is also what we expect:

```r
1488 / 6 / 8
```

So now the question is how to rename the bands or figure out which is which.
