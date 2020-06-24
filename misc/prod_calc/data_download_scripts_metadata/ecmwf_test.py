
from ecmwfapi import ECMWFDataServer
server = ECMWFDataServer()
server.retrieve({
    "class": "ei",
    "dataset": "interim",
    "date": "1987-09-01/to/1987-09-30",
    "expver": "1",
    "grid": "0.75/0.75",
    "levtype": "sfc",
    "param": "169.128",
    "step": "3/6/9/12",
    "stream": "oper",
    "time": "00:00:00/12:00:00",
    "type": "fc",
    "target": "output",
})
