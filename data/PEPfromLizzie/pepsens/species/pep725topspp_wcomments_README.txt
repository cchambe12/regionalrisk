Started 25 February 2016
Info on file pep725topspp_wcomments.csv


I (Lizzie Wolkovich) started this file. It is the output of the R file: pep725spp.R which reads in the PEP725 data received in late fall 2015 and then strips out very uncommon species.

The columns:
- sci_name
- cult_name
- N	
all come from the R script.

The following columns are all columns I created:
- common_name — the common name I found quickly online
- lifeform_bylizzie - just what I could quickly gather as the lifeform
- notes — whether I suspected it was domesticated or cultivated or if only the genus is given.
- consider_forananlysis - Ben Cook and I worked on this together. We decided to include only species that:
	- were possibly not domesticated (so anything that said ‘early cultivar’ or ‘late cultivar’ or such we did not consider
	- where the full species name was present (we did not work on things where there was only a genus name)

At the end we also removed species based on these criteria:
- No gymnosperms
- Must have first leafing (BBCH 11)
- Reasonably wide geographic distribution
- Native range overlapped with PEP725 data distribution


Which of the current 12 species we’re thinking of do we have data from the growth chamber meta-analysis on?
- Alnus glutinosa - Myking 1998, Heide 2003, Heide 1993 (2 experiments)
- Betula pendula - tons of studies
- Fagus sylvatica - tons
- Aesculus hippocastanum - in the Laube and Basler studies
- Corylus avellana - Laube and Basler studies and Heide 1993
- Fraxinus excelsior - in the Laube and Basler studies
- Quercus robur — Laube, Fu, Morin, other Laube
- Ribes rubrum - NONE!
- Sorbus aucuparia - Basler, Heide 1993
- Populus tremula - Laube (two studies), Heide 1993
- Tilia cordata - couple studies
- Tilia platyphyllos — NONE!


