# %%
import urllib.robotparser
import pandas as pd
import numpy as np
import scrapy
from scrapy.crawler import CrawlerProcess

# %%


class RobotsSpider(scrapy.Spider):
    name = 'robots_reader'
    start_urls = ["https://www.theactuaryjobs.com/"]

    def parse(self, response):
        print("robots.txt content:")
        print(response.text)


process = CrawlerProcess()
process.crawl(RobotsSpider)
process.start()


# %%

rp = urllib.robotparser.RobotFileParser()
rp.set_url("https://www.theactuaryjobs.com/robots.txt")
rp.read()

user_agent = '*'
url = 'https://www.theactuaryjobs.com/'

if rp.can_fetch(user_agent, url):
    print(f"Allowed to fetch {url}")
else:
    print(f"Disallowed to fetch {url}")
