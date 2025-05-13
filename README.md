# Instructions

## (Optional)
Download posts from any Mastodon server (e.g https://mastodon.social/ , https://hachyderm.io/ ) on a specific topic.   
All we need, is an access token you can get using the UI, navigate to Settings->Applications (create a new application)   
e.g https://hachyderm.io/settings/applications

Update the parameters in LoadPostsUsingMastodonAPI.py

    access_token = "XXXXXXXXXXXXXXXXXXXXXXXXXX"  # Replace with your Mastodon API access token 
    topic = "Trump"  # Replace with the topic you want to search for
    api_base_url = "https://hachyderm.io"  # Replace with your Mastodon instance URL

Then, just run the script    
 
    python LoadPostsUsingMastodonAPI.py

The Mastodon API supports generous rate limits so we can download enough  data  
https://docs.joinmastodon.org/api/rate-limits/

## Running the R script
Before executing SentimentAnalysisDemo.r, we need to update line 10 with the correct file path for our .csv file.     
We can use mastodon_posts_Trump_3000.csv which is part of this repo, or you can run the above python script to download new data.   

     mastodon_data <- read_csv("C:/dev/UTH/UTH-ANALYSIS/mastodon_posts_Trump_3000.csv")

Execute SentimentAnalysisDemo.r in your preferred environment.