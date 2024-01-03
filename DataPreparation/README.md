## Data Preparation
Here is where I will be using r to prepare, clean, and organize my data. I will start by creating a smaller training dataset using datasets from yahoo finance.

### PriceChangeByPrevious8Months_Training.csv
The initial training dataset will have entries for every eight month sequence that can be created from each of the data sets. There will be a column for ticker, industry, and the percent price change for each of the months. As each of the datasets have 62 entries, there will be 54 table entries for each of the eight stocks.
I have chosen this naive approach because it is very difficult to get comprehensive datasets, and in my experience I have had the most success investing when I have just looked at the trajectory stocks are on, rather than trying to take in excessive amounts of information. A positive about this approach is that it only will look at trajectory trends in certain industries, keeping training and prediction simple. A negative is that a large amount of my training data will be affected by major events like covid and elections, in which the markets are likely to act differently than usual, and my models trained with this dataset will not be able to account for this.