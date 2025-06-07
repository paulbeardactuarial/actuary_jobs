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


class LegacyWaybackJobScraper:
    def __init__(self, date_range=[None, None]):
        self.base_url = "https://web.archive.org/web/"
        self.target_url = "https://www.theactuaryjobs.com/jobs/"
        self.wayback_api = "https://web.archive.org/cdx/search/cdx"
        self.session = requests.Session()
        self.session.headers.update({
            'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36'
        })
        self.date_range = date_range
        self.web_strings = None

    def find_available_snapshots(self):
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

    def filter_date_strings(self, results):
        date_range = self.date_range
        filtered_date_range = [
            date_string for date_string in results if
            date_range[0] <= datetime.strptime(
                date_string, "%Y%m%d%H%M%S") <= date_range[1]]
        self.web_strings = filtered_date_range
        return (filtered_date_range)

    def extract_job_type_counts_legacy(self, soup, timestamp):
        """
        Extract permanent and interim job counts from the legacy HTML format
        """
        permanent_count = None
        interim_count = None

        # Look for the Contract Type header
        contract_header = soup.find(
            'h3', class_='collapsable', string=re.compile('Contract Type', re.IGNORECASE))

        if contract_header:
            # Find the associated div and ul
            content_div = contract_header.find_next_sibling('div')
            if content_div:
                expand_list = content_div.find('ul', class_='expandList')

                if expand_list:
                    list_items = expand_list.find_all('li')

                    for item in list_items:
                        link = item.find('a')
                        if link:
                            href = link.get('href', '')
                            text = link.get_text(strip=True).lower()

                            # Look for count in parentheses after the link
                            item_text = item.get_text()
                            count_match = re.search(r'\((\d+)\)', item_text)

                            if count_match and f'/web/{timestamp}/' in href:
                                count = int(count_match.group(1))

                                # Check if this is permanent jobs
                                if 'permanent' in text or 'permanent' in href:
                                    permanent_count = count

                                # Check if this is interim/contract jobs
                                elif any(keyword in text for keyword in ['interim', 'contract', 'temp']) or 'interim-contract-and-temp' in href:
                                    interim_count = count

        return permanent_count, interim_count

    def extract_sector_counts_legacy(self, soup, timestamp):
        """
        Extract job counts by sector from the legacy HTML format
        """
        sector_counts = {}

        # Look for the Sector header
        sector_header = soup.find(
            'h3', class_='collapsable', string=re.compile('Sector', re.IGNORECASE))

        if sector_header:
            # Find the associated div and ul
            content_div = sector_header.find_next_sibling('div')
            if content_div:
                expand_list = content_div.find('ul', class_='expandList')

                if expand_list:
                    list_items = expand_list.find_all('li')

                    for item in list_items:
                        link = item.find('a')
                        if link:
                            href = link.get('href', '')
                            sector_name = link.get_text(strip=True)

                            # Look for count in parentheses after the link
                            item_text = item.get_text()
                            count_match = re.search(r'\((\d+)\)', item_text)

                            if count_match and f'/web/{timestamp}/' in href:
                                count = int(count_match.group(1))
                                sector_counts[sector_name] = count

        return sector_counts

    def extract_location_counts_legacy(self, soup, timestamp):
        """
        Extract job counts by location from the legacy HTML format
        """
        location_counts = {}

        # Look for the Location header
        location_header = soup.find(
            'h3', class_='collapsable', string=re.compile('Location', re.IGNORECASE))

        if location_header:
            # Find the associated div and ul
            content_div = location_header.find_next_sibling('div')
            if content_div:
                expand_list = content_div.find('ul', class_='expandList')

                if expand_list:
                    list_items = expand_list.find_all('li')

                    for item in list_items:
                        link = item.find('a')
                        if link:
                            href = link.get('href', '')
                            location_name = link.get_text(strip=True)

                            # Look for count in parentheses after the link
                            item_text = item.get_text()
                            count_match = re.search(r'\((\d+)\)', item_text)

                            if count_match and f'/web/{timestamp}/' in href:
                                count = int(count_match.group(1))
                                location_counts[location_name] = count

        return location_counts

    def scrape_snapshot(self, timestamp):
        """
        Scrape a specific snapshot and extract all job data using legacy format parsing
        """
        wayback_url = f"{self.base_url}{timestamp}/{self.target_url}"

        try:
            print(f"Scraping legacy snapshot: {timestamp}")
            response = self.session.get(wayback_url, timeout=30)
            response.raise_for_status()

            soup = BeautifulSoup(response.text, 'html.parser')

            # Extract job type counts using legacy format
            permanent_count, interim_count = self.extract_job_type_counts_legacy(
                soup, timestamp)

            # Extract sector counts using legacy format
            sector_counts = self.extract_sector_counts_legacy(soup, timestamp)

            # Extract location counts using legacy format
            location_counts = self.extract_location_counts_legacy(
                soup, timestamp)

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
                'total_jobs': (permanent_count or 0) + (interim_count or 0) if permanent_count is not None and interim_count is not None else None,
                'sectors': sector_counts,
                'locations': location_counts
            }

            print(
                f"  Job types - Permanent: {permanent_count}, Interim: {interim_count}")
            print(f"  Sectors found: {len(sector_counts)}")
            print(f"  Locations found: {len(location_counts)}")

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
                'sectors': {},
                'locations': {},
                'error': str(e)
            }

    def filter_date_strings(self, results):
        date_range = self.date_range
        filtered_date_range = [
            date_string for date_string in results if
            date_range[0] <= datetime.strptime(
                date_string, "%Y%m%d%H%M%S") <= date_range[1]]
        self.web_strings = filtered_date_range
        return (filtered_date_range)

    def run_scraper(self, delay=2, start_year=None, end_year=None):
        """
        Run the complete scraping process for legacy format pages
        """
        print("Starting Legacy Wayback Machine job scraper...")

        # Step 1: Find available snapshots
        timestamps = self.find_available_snapshots()
        timestamps = self.filter_date_strings(timestamps)

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

    def create_summary_dataframe(self, results):
        """
        Create a summary DataFrame with basic job statistics
        """
        summary_data = []

        for result in results:
            if 'error' not in result:
                summary_data.append({
                    'timestamp': result['timestamp'],
                    'date': result['date'],
                    'permanent_jobs': result['permanent_jobs'],
                    'interim_jobs': result['interim_jobs'],
                    'total_jobs': result['total_jobs'],
                    'sectors_count': len(result['sectors']),
                    'locations_count': len(result['locations']),
                    'wayback_url': result['wayback_url']
                })

        return pd.DataFrame(summary_data)

    def create_sector_dataframe(self, results):
        """
        Create a detailed DataFrame for sector data
        """
        sector_data = []

        for result in results:
            if 'error' not in result and result['sectors']:
                for sector, count in result['sectors'].items():
                    sector_data.append({
                        'timestamp': result['timestamp'],
                        'date': result['date'],
                        'sector': sector,
                        'job_count': count
                    })

        return pd.DataFrame(sector_data)

    def create_location_dataframe(self, results):
        """
        Create a detailed DataFrame for location data
        """
        location_data = []

        for result in results:
            if 'error' not in result and result['locations']:
                for location, count in result['locations'].items():
                    location_data.append({
                        'timestamp': result['timestamp'],
                        'date': result['date'],
                        'location': location,
                        'job_count': count
                    })

        return pd.DataFrame(location_data)

    def save_results(self, results, base_filename='legacy_wayback_job_stats'):
        """
        Save results to multiple CSV files
        """
        if not results:
            print("No results to save")
            return

        # Create summary DataFrame
        summary_df = self.create_summary_dataframe(results)
        if not summary_df.empty:
            summary_filename = f"{base_filename}_summary.csv"
            summary_df.to_csv(summary_filename, index=False)
            print(f"Legacy summary results saved to {summary_filename}")

        # Create sector DataFrame
        sector_df = self.create_sector_dataframe(results)
        if not sector_df.empty:
            sector_filename = f"{base_filename}_sectors.csv"
            sector_df.to_csv(sector_filename, index=False)
            print(f"Legacy sector data saved to {sector_filename}")

        # Create location DataFrame
        location_df = self.create_location_dataframe(results)
        if not location_df.empty:
            location_filename = f"{base_filename}_locations.csv"
            location_df.to_csv(location_filename, index=False)
            print(f"Legacy location data saved to {location_filename}")

        # Print summary statistics
        self.print_summary(summary_df, sector_df, location_df)

    def print_summary(self, summary_df, sector_df, location_df):
        """
        Print summary statistics
        """
        print(f"\n=== LEGACY SCRAPING SUMMARY ===")

        if not summary_df.empty:
            valid_results = summary_df[(summary_df['permanent_jobs'].notna()) & (
                summary_df['interim_jobs'].notna())]
            print(f"Total legacy snapshots processed: {len(summary_df)}")
            print(
                f"Successfully extracted job type data from: {len(valid_results)} snapshots")

            if len(valid_results) > 0:
                print(
                    f"Date range: {valid_results['date'].min()} to {valid_results['date'].max()}")
                print(
                    f"Permanent jobs range: {valid_results['permanent_jobs'].min()} to {valid_results['permanent_jobs'].max()}")
                print(
                    f"Interim jobs range: {valid_results['interim_jobs'].min()} to {valid_results['interim_jobs'].max()}")

        if not sector_df.empty:
            unique_sectors = sector_df['sector'].nunique()
            total_sector_entries = len(sector_df)
            print(
                f"\nSector data: {total_sector_entries} entries across {unique_sectors} unique sectors")

            # Top sectors by average job count
            avg_by_sector = sector_df.groupby(
                'sector')['job_count'].mean().sort_values(ascending=False)
            print("Top 5 sectors by average job count:")
            for sector, avg_count in avg_by_sector.head().items():
                print(f"  {sector}: {avg_count:.1f}")

        if not location_df.empty:
            unique_locations = location_df['location'].nunique()
            total_location_entries = len(location_df)
            print(
                f"\nLocation data: {total_location_entries} entries across {unique_locations} unique locations")

            # Top locations by average job count
            avg_by_location = location_df.groupby(
                'location')['job_count'].mean().sort_values(ascending=False)
            print("Top 5 locations by average job count:")
            for location, avg_count in avg_by_location.head().items():
                print(f"  {location}: {avg_count:.1f}")


# %%
if __name__ == "__main__":

    # get date ranges
    date_range = []
    date_range.append(datetime.strptime("2013-05-13", "%Y-%m-%d"))
    date_range.append(datetime.strptime("2015-12-06", "%Y-%m-%d"))

    # Create legacy scraper instance
    scraper = LegacyWaybackJobScraper(date_range)

    # Run the scraper for a specific date range (e.g., 2010-2016 for older format)
    # You can adjust these years based on when the format changed
    results = scraper.run_scraper(delay=1, start_year=2010, end_year=2015)

    # Save results to multiple CSV files
    scraper.save_results(results)

    # Print sample results
    if results:
        print(f"\n=== SAMPLE LEGACY RESULTS ===")
        for i, result in enumerate(results[:3]):
            if 'error' not in result:
                print(f"\nLegacy snapshot {result['timestamp']}:")
                print(
                    f"  Job types - Permanent: {result['permanent_jobs']}, Interim: {result['interim_jobs']}")
                if result['sectors']:
                    print(
                        f"  Top sectors: {dict(list(result['sectors'].items())[:3])}")
                if result['locations']:
                    print(
                        f"  Top locations: {dict(list(result['locations'].items())[:3])}")

# %%
date_range = []
date_range.append(datetime.strptime("2013-01-01", "%Y-%m-%d"))
date_range.append(datetime.strptime("2014-01-01", "%Y-%m-%d"))
date_range
scraper = LegacyWaybackJobScraper(date_range)
snapshots = scraper.find_available_snapshots()

# %%
