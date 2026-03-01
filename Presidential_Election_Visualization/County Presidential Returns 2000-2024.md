# Codebook for 2000-2024 County Presidential Data

The data file `countypres_2000-2024` contains county-level returns for presidential elections from 2000 through 2024. The data source is official state election data records.

Replication scripts, original data files, up-to-date data sources, and quality assurance checks are in most cases available on request.

Our typical practice, set in 2016, is to report presidential election results for the three parties that achieved ballot access in 50 states during that year's presidential contest: the Democrat, Republican, and Libertarian Parties. In some cases, results for more presidential candidates were collected, but are ommitted for the sake of parsimony. If you are interested in those returns (which are partial and often subject to state data availability and formatting), please use the Dataverse contact option to request access.

For information on how we quality assure our data, please see Baltz, Agadjanian, Chin <i>et al.</i> in <i>Scientific Data</i> (2022): https://www.nature.com/articles/s41597-022-01745-0 . That article concerns quality assurance of our precinct-level data offerings, but these county-level data undergo a similar process.

Note for 2004: results in Alaska are based on official Alaska data, but it is clear the district returns significantly overstate the number of votes cast. In Alaska, the county_fips field stores a combination of state FIPS code and district.

Date updated: 2025-07-12
## Variables
The variables are listed as they appear in the data file. 

### year
- **Description**: election year	

------------------

### state 
- **Description**: state name 

-----------------

### state_po
- **Description**: U.S. postal code state abbreviation

----------------

### county_name
- **Description**: county name

----------------

### county_fips
- **Description**: county FIPS code

----------------

### office
- **Description**: President

----------------

### candidate
- **Description**: name of the candidate

----------------

### party
- **Description**: party of the candidate; takes form of DEMOCRAT, REPUBLICAN, GREEN, LIBERTARIAN, or OTHER

----------------
	
### candidatevotes 
 - **Description**: votes received by this candidate for this particular party

----------------

### totalvotes
 - **Description**: total number of votes cast in this county-year

----------------

### mode
 - **Description**: mode of ballots cast; default is TOTAL, with different modes specified for 2020

----------------

### version
- **Description**: date when dataset was finalized

----------------
