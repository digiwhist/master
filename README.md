Digiwhist Data Collection System (DDCS)
=======================================

This project contains a collection of java components that are used by the DigiWhist project to fetch, convert, clean and reconcile data on public contracting, and associated data such as public interest declarations and country budgets. 

For more details about the design of this architecture, please see [Digiwhist Workpackage 2.8 (PDF)](https://github.com/digiwhist/wp2_documents/blob/master/d2_8.pdf).

REQUIREMENTS
-------------------------------------------------------
- Postgresql 9.4 and higher
- RabbitMQ 3.6
- Java 8
- Maven

ARCHITECTURE
-------------------------------------------------------
DDCS is organised as a series of Maven projects. These can be built using the `mvn compile` command. 

Data processing stages
-------------------------------------------------------

* Raw - downloading of raw (HTML, XML, etc.) files from internet
* Parsed - conversion of unstructured data to structured format (all values in text format)
* Clean - conversion of text values into proper data types, standardizing of enumeration values etc.
* Matched - assigning records (tenders, bodies) describing one real-world entity into the same group
* Master - creation of final image of an entity based on data contained in matched records

Workers
-------------------------------------------------------
Each above described stage of data is processed by a standalone program called worker

- TenderCrawler - crawls a website, FTP server or reads from an API and passes information of what should be downloaded to Downloader
                 - in some specific cases Crawler also serves as a Downloader
- TenderDownloader - reads information passed by a crawler and downloads and stores data to a DB. Tells parser which records can be parsed.
- TenderParser - creates structured data from unstructured data. Tells cleaner which records can be cleaned.
- TenderCleaner - does the cleaning job and tells matcher which records can be matched.
- TenderMatcher - extracts bodies record from tender records and does the body matching and tender matching job. By passing information on which groups of bodies and tenders can be matched starts separate processes of body and tender mastering
- TenderMaster - processes group of matched tenders. It's an implementation of tender mastering methodology described in D2.8
- BodyMaster - processes group of matched bodies. It's an implementation of body mastering methodology described in D2.8

Worker names are derived from a package structure of DDCS. Worker names that processes TED source are: 

- eu.digiwhist.worker.eu.raw.TedTenderCrawler
- eu.digiwhist.worker.eu.raw.TedTenderDownloader
- eu.digiwhist.worker.eu.parsed.TedTenderParser
- eu.digiwhist.worker.eu.clean.TedTenderCleaner
- eu.digiwhist.worker.eu.matched.TedTenderMatcher
- eu.digiwhist.worker.eu.master.TedTenderMaster
- eu.digiwhist.worker.eu.master.TedBodyMaster

Worker names that crawl and download data from country specific sources are named as follows (example is for Spanish sources)

- eu.digiwhist.worker.es.raw.PCETenderCrawler
- eu.digiwhist.worker.es.raw.PCETenderDownloader

These can be found in the `digiwhist-worker/serc/main/java/eu/digiwhist/worker/[COUNTRY]/raw` folders

Storage
-------------------------------------------------------
Each tender record has it's copy on each stage of data processing. These are stored in separate DB tables. The names of tables are:

- raw_data
- parsed_tender
- clean_tender
- matched_tender
- matched_body
- master_tender
- master_body

Create script is located in digiwhist-dataaccess\src\main\resources\migrations\001_base.sql

Each table row will contain meta-data about the tender, along with a blob of structured JSON. 

Communication
-------------------------------------------------------
DDCS uses RabitMQ messaging system to ensure communication between workers.
Each time some record is processed on a specific level of data processing, proper program publishes a message containing ID of a tender record which should be processed on a next level. Such message is used by a next level worker to retrieve the right record
