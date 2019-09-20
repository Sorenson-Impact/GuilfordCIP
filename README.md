# Guilford-CIP
The code and data standards for the Guilford County Community Indicators Project dashboard

By collecting and sharing data, a community can empower constituents to use data to address community concerns and help its own civil servants find innovative solutions to key challenges. Consistently sharing and using data to act on community interests can also lead to a culture of evidence-based decision-making in which data becomes essential for good governance.

## What Is Open Data?<sup>1</sup>
**Data** are electronic records stored in computer systems. In the simplest terms, data are lists of things such as requests for service, inventories, or incidents, which include helpful details such as dates, locations, images, video, and more.

**Open data** makes these electronic records accessible in whole or in part to the public. This practice is considered proactive disclosure - making information available without it being requested. While industry experts often refer to the Eight Principles of Open Data, it is helpful to initially focus on the following elements:

* Open data is **online**. Cities proactively provide open data through the internet, giving the public the ability to find and use open data without waiting for a response or approval.
* Open data is **free**. Cities do not require payment from anyone to obtain open data.
* Open data is **non-proprietary**. Cities do not require data consumers to have specific software programs in order to use open data.
* Open data is **unrestricted**. Cities do not restrict the use, interpretation, or redistribution of open data through copyright or other terms of use.
* Open data is **machine-readable**. Cities do not require data consumers to scrape data out of "locked-up" formats like PDFs in order to use it, instead using data formats like CSV, JSON or XML.



# Data Standards

These data standards are divided into two main sections: content and structure. The former describes issues like reproducability and documentation. The latter describes standards for how the data should be organized and maintained in order to facilitate analysis, visualization, and data sharing. 

## Content

### Reproducibility 

All data needs to have a source describing where and when the data were obtained the data and if there are any known issues with the data. This is important to ensure data integrity. 
 
Modifications to data should be fully reproducible using code (not manually adjusted). This is important in order to allow future updates to the data to be added and modified uniformly. It is also a safety protocol that allows changes to be tracked to prevent inaccurate conclusions to be drawn from the data.

### Definitions

Each data set should be accompanied by a data dictionary explaining each column:
* Column name
* Description of data
* Format of data (character, date, numeric)

## Structure

Data should be tabular with one header row starting in the top-left cell.  Cells should not be merged.

Data should be “tidy,” according to the definitions laid out in the journal article “Tidy Data” by Hadley Wickham.<sup>2</sup> 

Tidy data, Codd’s 3rd normal form (Codd 1990):

* Each variable forms a column
* Each observation forms a row
* Each type of observational unit forms a table


# Principles

#### The 8 Principles of Open Government Data<sup>3</sup>

Government data shall be considered open if it is made public in a way that complies with the principles below:

**1. Complete**

All public data is made available. Public data is data that is not subject to valid privacy, security or privilege limitations.

While non-electronic information resources, such as physical artifacts, are not subject to the Open Government Data principles, it is always encouraged that such resources be made available electronically to the extent feasible.

> “Bulk data” means that an entire dataset can be acquired. Even the simplest of applications, such as computing the sum of line items, requires access to the entire dataset. This principle also implies that bulk data should be made available before “APIs” are created because APIs typically only return small slices of the whole data.

**2. Primary**

Data is as collected at the source, with the highest possible level of granularity, not in aggregate or modified forms.

If an entity chooses to transform data by aggregation or transcoding for use on an Internet site built for end users, it still has an obligation to make the full-resolution information available in bulk for others to build their own sites with and to preserve the data for posterity.

**3. Timely**

Data is made available as quickly as necessary to preserve the value of the data.

**4. Accessible**

Data is available to the widest range of users for the widest range of purposes.

Data must be made available on the Internet so as to accommodate the widest practical range of users and uses. This means considering how choices in data preparation and publication affect access to the disabled and how it may impact users of a variety of software and hardware platforms. Data must be published with current industry standard protocols and formats, as well as alternative protocols and formats when industry standards impose burdens on wide reuse of the data.
Data is not accessible if it can be retrieved only through navigating web forms, or if automated tools are not permitted to access it because of a robots.txt file, other policy, or technological restrictions.

**5. Machine processable**

Data is reasonably structured to allow automated processing.

The ability for data to be widely used requires that the data be properly encoded. Free-form text is not a substitute for tabular and normalized records. Images of text are not a substitute for the text itself. Sufficient documentation on the data format and meanings of normalized data items must be available to users of the data.

>The Association of Computing Machinery’s Recommendation on Open Government (February 2009) stated this principle another way: “Data published by the government should be in formats and approaches that promote analysis and reuse of that data.” The most critical value of open government data comes from the public’s ability to carry out its own analyses of raw data, rather than relying on a government’s own analysis.
As part of this, the use of unique, numeric identifiers for entities mentioned in the data can help connect the data to other relevant information.

**6. Non-discriminatory**

Data is available to anyone, with no requirement of registration.

Anonymous access to the data must be allowed for public data, including access through anonymous proxies. Data should not be hidden behind “walled gardens.”

**7. Non-proprietary**

Data is available in a format over which no entity has exclusive control.

Proprietary formats add unnecessary restrictions over who can use the data, how it can be used and shared, and whether the data will be usable in the future. While some proprietary formats are nearly ubiquitous, it is nevertheless not acceptable to use only proprietary formats. Likewise, the relevant non-proprietary formats may not reach a wide audience. In these cases, it may be necessary to make the data available in multiple formats.

**8. License-free**

Data is not subject to any copyright, patent, trademark or trade secret regulation. Reasonable privacy, security and privilege restrictions may be allowed.

Because government information is a mix of public records, personal information, copyrighted work, and other non-open data, it is important to be clear about what data is available and what licensing, terms of service, and legal restrictions apply. Data for which no restrictions apply should be marked clearly as being in the public domain.
Requiring attribution to the government, even though attribution might be reasonable in other contexts, would constitute a major policy shift in the United States with significant legal implications for the press. The Creative Commons CC0 public domain dedication can make a work license-free.

Compliance must be reviewable.

**Definitions**

*“public” means:*

The Open Government Data principles do not address what data should be public and open. Privacy, security, and other concerns may legally (and rightly) prevent data sets from being shared with the public. Rather, these principles specify the conditions public data should meet to be considered “open.”

*“data” means:*

Electronically stored information or recordings. Examples include documents, databases of contracts, transcripts of hearings, and audio/visual recordings of events.
While non-electronic information resources, such as physical artifacts, are not subject to the Open Government Data principles, it is always encouraged that such resources be made available electronically to the extent feasible.

*“reviewable” means:*

A contact person must be designated to respond to people trying to use the data.
A contact person must be designated to respond to complaints about violations of the principles.
An administrative or judicial court must have the jurisdiction to review whether the agency has applied these principles appropriately.

# Additional Resources

For instructions on using R for data analysis, refer to "R for Data Science" (https://r4ds.had.co.nz/). 

For more detailed suggestions about data formatting, see this resource: https://project-open-data.cio.gov/v1.1/schema/

# Citations

[1]: Johns Hopkins University Center for Government Excellence: Open Data Getting Started Guide (https://www.gitbook.com/book/centerforgov/open-data-getting-started/details).

[2]: Journal of Statistical Software. (https://www.jstatsoft.org/article/view/v059i10), 2014-09-12.

[3]: The Annotated 8 Principles of Open Government Data (https://opengovdata.org/).
