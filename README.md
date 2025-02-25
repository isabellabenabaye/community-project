![](https://github.com/isabellabenabaye/community-project/blob/master/img/community_header.jpg?raw=true)
# IMBd ratings of all 110 Community (2009 - 2015) episodes

First of all, Community is a gem and if you haven't seen it, you should.

On May 9, 2020 I scraped the IMDb ratings of all 110 [Community](https://en.wikipedia.org/wiki/Community_(TV_series)) episodes in honor of their [May 18, 2020 reunion](https://www.cnet.com/news/the-community-reunion-with-donald-glover-is-happening/) to raise money for two coronavirus relief efforts.

### This repo contains the following R script & jupyter notebooks:

`community_plot.R` - R script that creates the plot below
![](https://github.com/isabellabenabaye/community-project/blob/master/Community%20Episodes.png?raw=true)

`scraping-episode-imdb-ratings.ipynb` - this notebook explains the scraping process step-by-step, using [this article](https://www.dataquest.io/blog/web-scraping-beautifulsoup/) as a reference (for beginners)

`community_data_scrape.ipynb` - optimized code used for scraping

`community_EDA.ipynb` - explores the ratings and total votes, plots them against each other, and contains the code that creates the heatmap below

![](https://github.com/isabellabenabaye/community-project/blob/master/Community%20Episodes%20Ratings%20-%20Heatmap.png?raw=true)     

### Dataset
You can also find the dataset on kaggle [here](https://www.kaggle.com/imbenab/community-episodes-imdb-ratings).

The variables include:   
**Season** - `season`   
**Episode number** - `episode_number`   
**Episode title** - `title`   
**Original air date** - `airdate`   
**IMBd rating** - `rating`   
**Total votes** - `total_votes`   
**Episode description** - `desc`   