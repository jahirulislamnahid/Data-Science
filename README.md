# Data-Science
This is my data science course related projects which contains text pre processing from a web page(English newspaper web) then we do topic modeling and make different csv files to save then and we do in the beginning several operations on the dataset  

The document titled "Mid Term Project" for the course Introduction to Data Science (CSC4180) summarizes a data analysis project using a student placement dataset (placement_Data_Full_Class – modified.csv) containing 216 records and 16 attributes.

🔹 Summary (Short Description):
The project explores student academic and placement data to identify patterns related to employability. Key tasks include:

Data exploration: Checking dataset dimensions and structure.

Pre-processing: Removing duplicates, handling missing and invalid values, normalizing salary, and converting categorical to numeric values.

Data balancing: Ensuring equal distribution of placed and not placed students.

Data splitting: Dividing data into training and test sets.

Descriptive statistics: Calculating central tendency (mean, median, mode) and spread (range, IQR, variance, std deviation) for SSC and MBA percentages.

Overall, it’s a complete data cleaning and preparation pipeline for modeling placement prediction.

The project involves web scraping and topic modeling on 500 news articles from The Daily Star, covering five categories: Business, Sports, Entertainment, Youth, and Life & Living. The main steps are:

Data Collection:

Scraped 100 articles per category using R's rvest package.

Extracted: title, link, publication time, and description.

Text Preprocessing:

Cleaned and standardized text by:

Replacing emojis

Removing punctuation, numbers, stopwords

Applying stemming, lemmatization, and spell correction

Final processed text saved in a new column processed_description.

Topic Modeling using LDA (Latent Dirichlet Allocation):

Cleaned text was converted into a Document-Term Matrix (DTM).

Applied LDA to identify 5 topics:

Topic 1: Economy/Finance

Topic 2: Education/Academia

Topic 3: Lifestyle

Topic 4: Sports

Topic 5: Entertainment

Visualized top words per topic using ggplot2.

Topic Assignment:

Each article was assigned a dominant topic using LDA gamma probabilities.

Final dataset includes article content, dominant topic number, probability, and readable topic name.

✅ Outcome:
A well-structured, preprocessed dataset ready for NLP tasks like classification or sentiment analysis, enriched with topic labels that give insights into thematic trends in Bangladeshi news media.
