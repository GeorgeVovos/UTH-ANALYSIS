from mastodon import Mastodon
from datetime import datetime, timezone
import time
import csv 

class MastodonTopicSearch:
    def __init__(self, access_token, api_base_url):
        self.mastodon = Mastodon(
            access_token=access_token,
            api_base_url=api_base_url
        )

    def search_topic(self, topic, max_results):
        try:
            results = []
            offset = 0
            while len(results) < max_results:
                print (f"Fetching posts with offset: {offset}" + f" Result number: {len(results)}")
               
                fetched_posts = self.mastodon.search_v2(
                    q=topic,
                    result_type='statuses',
                    offset=offset
                )
                if not fetched_posts or 'statuses' not in fetched_posts:
                    break
                batch = fetched_posts['statuses']
                if not batch:
                    break
                for post in batch:
                    results.append({
                        'id': post['id'],
                        'content': post['content'],
                        'created_at': post['created_at'].isoformat(),
                        'url': post['url'],
                        'account': {
                            'username': post['account']['username'],
                            'display_name': post['account']['display_name']
                        }
                    })
                    if len(results) >= max_results:
                        break
                offset += len(batch)
                time.sleep(1)  # Optional delay to avoid rate limits
            return results[:max_results]
        except Exception as e:
            print(f"Error during topic search: {str(e)}")
        return []

    def save_to_csv(self, posts, filename="mastodon_posts.csv"):
        try:
            with open(filename, mode='w', encoding='utf-8', newline='') as file:
                writer = csv.writer(file)

                writer.writerow(['Post ID', 'Content', 'Created At', 'URL', 'Username', 'Display Name'])

                for post in posts:
                    writer.writerow([
                        post['id'],
                        post['content'],
                        post['created_at'],
                        post['url'],
                        post['account']['username'],
                        post['account']['display_name']
                    ])
            print(f"Posts saved to {filename}")
        except Exception as e:
            print(f"Error saving posts to CSV: {str(e)}")


if __name__ == "__main__":
    access_token = "XXXXXXXXXXXXXXXXXXXXXXXXXX"  # Replace with your Mastodon API access token
    topic = "Trump"  # Replace with the topic you want to search for
    api_base_url = "https://hachyderm.io"  # Replace with your Mastodon instance URL
    mastodon_search = MastodonTopicSearch(access_token, api_base_url)  # Replace with your Mastodon instance URL

    posts = mastodon_search.search_topic(topic, max_results=1500)

    mastodon_search.save_to_csv(posts, filename=f"mastodon_posts_{topic}.csv")