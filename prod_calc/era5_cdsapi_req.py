import cdsapi
from datetime import date

target_dir = "/Users/johngodlee/google_drive/postgrad/phd/thesis/chapters/chapter_1_biomass_region_befr/prod_calc/raw_data/era5/"

start_date = date(2016, 1, 1)
end_date = date(2018, 12, 31)

date_list = [date.fromordinal(i) for i in range(start_date.toordinal(), end_date.toordinal() + 1)]

def fnServerRetrieve(date):
  year = date.strftime('%Y')
  month = date.strftime('%m')
  day = date.strftime('%d')

  c = cdsapi.Client()
  c.retrieve(
	'reanalysis-era5-single-levels',
	{
	'product_type':'reanalysis',
	'format':'netcdf',
	'variable':[
		'10m_u_component_of_wind','10m_v_component_of_wind','2m_temperature','2m_dewpoint_temperature',
		'leaf_area_index_high_vegetation','leaf_area_index_low_vegetation','maximum_2m_temperature_since_previous_post_processing',
		'minimum_2m_temperature_since_previous_post_processing','surface_net_solar_radiation','surface_net_solar_radiation_clear_sky',
		'top_net_solar_radiation','top_net_solar_radiation_clear_sky','total_cloud_cover',
		'total_precipitation'
		],
	'area':[40, -20,-40, 60], # North, West, South, East
	'year':year,
	'month':month,
	'day':day,
	'time':[
		'00:00','02:00','04:00','06:00',
		'08:00','10:00','12:00','14:00',
		'16:00','18:00','20:00','22:00'
		]
	},
	target_dir + "era5" + "_" + year + "_" + month + "_" + day + ".nc")
	return

for i in date_list:
	year = i.strftime('%Y')
	month = i.strftime('%m')
	day = i.strftime('%d')
	print("day:" + day + " month:" + month + " year:" + year)
	fnServerRetrieve(i)




