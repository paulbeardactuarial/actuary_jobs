# %%
import requests
import re
from bs4 import BeautifulSoup
import time
import json
from urllib.parse import quote
import pandas as pd
from datetime import datetime

# %%


class WaybackJobScraper:
    def __init__(self):
        self.base_url = "https://web.archive.org/web/"
        self.target_url = "https://www.theactuaryjobs.com/jobs/#browsing"
        self.wayback_api = "https://web.archive.org/cdx/search/cdx"
        self.session = requests.Session()
        self.session.headers.update({
            'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36'
        })

    def find_available_snapshots(self):
        """
        Find all available snapshots of the target URL using Wayback Machine's CDX API
        """
        print("Searching for available snapshots...")

        params = {
            'url': self.target_url,
            'output': 'json',
            'fl': 'timestamp,original,statuscode',
            'filter': 'statuscode:200',
            'collapse': 'timestamp:8'  # Collapse to daily snapshots to reduce duplicates
        }

        try:
            response = self.session.get(
                self.wayback_api, params=params, timeout=30)
            response.raise_for_status()

            data = response.json()

            if not data:
                print("No snapshots found")
                return []

            # Skip the header row
            snapshots = data[1:] if len(data) > 1 else []

            timestamps = []
            for snapshot in snapshots:
                timestamp = snapshot[0]
                if len(timestamp) == 14:  # Ensure it's the full 14-digit format
                    timestamps.append(timestamp)

            print(f"Found {len(timestamps)} snapshots")
            return sorted(timestamps)

        except requests.RequestException as e:
            print(f"Error fetching snapshots: {e}")
            return []

    def extract_job_counts(self, html_content, timestamp):
        """
        Extract permanent and interim job counts from the HTML content
        """
        soup = BeautifulSoup(html_content, 'html.parser')

        # Look for the specific pattern in the HTML
        permanent_count = None
        interim_count = None

        # Find the ul element with the specific class
        filter_ul = soup.find(
            'ul', class_='filter__items facet-links indent block lap-larger')

        if filter_ul:
            # Find all li elements within this ul
            filter_items = filter_ul.find_all(
                'li', class_='filter__item facet-links__link lap-larger__item')

            for item in filter_items:
                link = item.find('a')
                small_tag = item.find('small')

                if link and small_tag:
                    href = link.get('href', '')
                    text = link.get_text(strip=True).lower()
                    count_text = small_tag.get_text(strip=True)

                    # Extract number from count text
                    count_match = re.search(r'\d+', count_text)
                    if count_match:
                        count = int(count_match.group())

                        # Check if this is permanent jobs
                        if 'permanent' in text and f'/web/{timestamp}/' in href:
                            permanent_count = count

                        # Check if this is interim/contract jobs
                        elif any(keyword in text for keyword in ['interim', 'contract', 'temp']) and f'/web/{timestamp}/' in href:
                            interim_count = count

        # Alternative search method if the above doesn't work
        if permanent_count is None or interim_count is None:
            # Look for any links containing the timestamp and job type
            all_links = soup.find_all('a', href=True)

            for link in all_links:
                href = link.get('href', '')
                if f'/web/{timestamp}/' in href:
                    # Find the small tag near this link
                    parent = link.parent
                    if parent:
                        small_tag = parent.find('small')
                        if small_tag:
                            count_text = small_tag.get_text(strip=True)
                            count_match = re.search(r'\d+', count_text)

                            if count_match:
                                count = int(count_match.group())
                                link_text = link.get_text(strip=True).lower()

                                if 'permanent' in href and permanent_count is None:
                                    permanent_count = count
                                elif any(keyword in href for keyword in ['interim', 'contract', 'temp']) and interim_count is None:
                                    interim_count = count

        return permanent_count, interim_count

    def scrape_snapshot(self, timestamp):
        """
        Scrape a specific snapshot and extract job counts
        """
        wayback_url = f"{self.base_url}{timestamp}/{self.target_url}"

        try:
            print(f"Scraping snapshot: {timestamp}")
            response = self.session.get(wayback_url, timeout=30)
            response.raise_for_status()

            permanent_count, interim_count = self.extract_job_counts(
                response.text, timestamp)

            # Convert timestamp to readable date
            try:
                date_obj = datetime.strptime(timestamp, '%Y%m%d%H%M%S')
                readable_date = date_obj.strftime('%Y-%m-%d %H:%M:%S')
            except:
                readable_date = timestamp

            result = {
                'timestamp': timestamp,
                'date': readable_date,
                'wayback_url': wayback_url,
                'permanent_jobs': permanent_count,
                'interim_jobs': interim_count,
                'total_jobs': (permanent_count or 0) + (interim_count or 0) if permanent_count is not None and interim_count is not None else None
            }

            print(f"  Permanent: {permanent_count}, Interim: {interim_count}")
            return result

        except requests.RequestException as e:
            print(f"  Error scraping {timestamp}: {e}")
            return {
                'timestamp': timestamp,
                'date': None,
                'wayback_url': wayback_url,
                'permanent_jobs': None,
                'interim_jobs': None,
                'total_jobs': None,
                'error': str(e)
            }

    def run_scraper(self, delay=2):
        """
        Run the complete scraping process
        """
        print("Starting Wayback Machine job scraper...")

        # Step 1: Find available snapshots
        timestamps = self.find_available_snapshots()

        if not timestamps:
            print("No snapshots found. Exiting.")
            return []

        print(f"Found {len(timestamps)} snapshots to process")

        # Step 2: Scrape each snapshot
        results = []

        for i, timestamp in enumerate(timestamps):
            result = self.scrape_snapshot(timestamp)
            results.append(result)

            # Add delay between requests to be respectful
            if i < len(timestamps) - 1:
                time.sleep(delay)

        return results

    def save_results(self, results, filename='wayback_job_stats.csv'):
        """
        Save results to CSV file
        """
        if results:
            df = pd.DataFrame(results)
            df.to_csv(filename, index=False)
            print(f"Results saved to {filename}")

            # Print summary
            valid_results = df[(df['permanent_jobs'].notna())
                               & (df['interim_jobs'].notna())]
            print(f"\nSummary:")
            print(f"Total snapshots processed: {len(results)}")
            print(
                f"Successfully extracted data from: {len(valid_results)} snapshots")

            if len(valid_results) > 0:
                print(
                    f"Date range: {valid_results['date'].min()} to {valid_results['date'].max()}")
                print(
                    f"Permanent jobs range: {valid_results['permanent_jobs'].min()} to {valid_results['permanent_jobs'].max()}")
                print(
                    f"Interim jobs range: {valid_results['interim_jobs'].min()} to {valid_results['interim_jobs'].max()}")
        else:
            print("No results to save")


def main():
    # Create scraper instance
    scraper = WaybackJobScraper()

    # Run the scraper
    results = scraper.run_scraper(delay=2)  # 2 second delay between requests

    # Save results
    scraper.save_results(results)

    # Print first few results
    if results:
        print("\nFirst few results:")
        for i, result in enumerate(results[:5]):
            print(
                f"  {result['timestamp']}: Permanent={result['permanent_jobs']}, Interim={result['interim_jobs']}")


if __name__ == "__main__":
    main()
