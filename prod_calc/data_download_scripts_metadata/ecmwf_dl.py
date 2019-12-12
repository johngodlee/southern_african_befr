from ecmwfapi import ECMWFDataServer

# Load in date file as two vectors 
with open('date_range.txt') as f:
	dates = [line.rstrip('\n') for line in open('date_range.txt')]

dates_start = dates[0::2]

dates_end = dates[1::2] 

print("Defining function")

def fnServerRetrieve(dateStart, dateEnd):
	server = ECMWFDataServer()
	server.retrieve({
	    "class": "ei",
	    "dataset": "interim",
	    "date": dateStart+"/to/"+dateEnd,
	    "expver": "1",
	    "grid": "0.75/0.75",
	    "levtype": "sfc",
	    "param": "166.128/168.128/178.128/201.128/202.128/208.128/210.128/228.128/169.128",
	    "step": "12",
	    "stream": "oper",
	    "time": "00:00:00/12:00:00",
	    "type": "fc",
	    "area": "40/-20/-40/60", # Africa
	    "target": "ecmwf_africa"+dateStart+"_"+dateEnd+".nc",
	})
	return

for i, j  in zip(dates_start, dates_end):
	dateStart = i
	dateEnd = j
	print("Submitting request")
	fnServerRetrieve(dateStart, dateEnd)	
	print("Download complete")
