# crawl korean herald
import requests
from bs4 import BeautifulSoup
from urllib.parse import urljoin
import time
import csv


class CrawledArticle():
    def __init__(self, title, date, publisher, author, content, url):
        self.title = title
        self.date = date
        self.publisher = publisher
        self.author = author
        self.content = content
        self.url = url

    def __str__(self):
        return "URL: %s \nTITLE: %s \nDATE: %s \nAUTHOR: %s \nCONTENT: %s" %(self.url, self.title, self.date, self.author, self.content)


class ArticleFetcher():

    # CRAWL KOREA HERALD ARTICLES
    def fetch_koreaherald(self, kwd):
        articles = []
        page_count = 0
        do_break = False;
        url = f"http://www.koreaherald.com/search/index.php?q={kwd}&sort=1&mode=list&np=1&mp=1"
        # url = "http://www.koreaherald.com/search/index.php?kr=&q2=covid-19"
        base_url = "http://www.koreaherald.com/search/index.php"

        # iterate over search result pages
        while (not do_break):
            time.sleep(.2)
            print(f" ****** PAGE {page_count}: {url} ******")
            r = requests.get(url)
            doc = BeautifulSoup(r.text, "html.parser") # read current search results page

            # iterate over articles on this search results page
            for article in doc.select(".main_sec_li li"):
                date = article.find("a").select_one(".main_l_t1_bg").select_one(".main_l_t2").find("span").text  # assign date (still from search results)
                if "2019" in date:  # break if article older than 2020
                    do_break = True
                    break

                article_url = urljoin(base_url,article.find("a").attrs["href"])  # assign url (still from search results)

                # read article page
                article_page = BeautifulSoup(requests.get(article_url).text, "lxml") #.replace("<br>", "").replace("<br/>", "").replace("<img", "<div")
                for linebreak in article_page.select('br'):
                    linebreak.extract()
                for img in article_page.select('img'):
                    img.extract()

                title = article_page.select_one(".view_tit").text  # assign article title
                author = article_page.select_one(".view_tit_byline_l").find("a").text.replace("By ", "")  # assign article author

                content = article_page.select_one("#articleText")  # assign aticle content
                i = 0
                content_str = ""
                # iterate over article content sections
                for child in content.select(".view_con_t"):
                    content_str = content_str + " " + child.text
                    # print(str(i)+": "+child.text)
                    i = i+1
                # print(article_page.select_one("#articleText"))

                crawled = CrawledArticle(title, date, "The Korea Herald", author, content_str, article_url)
                # print(crawled)
                articles.append(crawled)


            # navigate to next search result page using page navigation buttons
            buttons = doc.select(".paging li")
            for i in range(len(buttons)):  # find currently active button
                if "on" in buttons[i].find("a").attrs["class"]:
                    index_current = i
                    print(index_current)
                    break
            next_button = buttons[index_current+1]  # find button to next page
            print("-> NEXT: ", next_button.find("a").attrs["href"])

            # termination
            if ("javascript" in next_button.find("a").attrs["href"]) or do_break:  # alert is raised when last paged is reached
                print("-- BREAK --")
                break
            # move to next search result page
            else:
                url = urljoin(base_url, next_button.find("a").attrs["href"])
            page_count = page_count + 1

        print(str(page_count) + " pages of search results processed.")
        print(str(len(articles))+ " articles count.")
        return articles


    # CRAWL KOREA TIMES ARTICLES
    def fetch_koreatimes(self, kwd):
        articles = []
        page_count = 1  # set page number to start with (default: 1)
        do_break = False;
        kwd = kwd  # set keyword to search for

        url = f"https://www.koreatimes.co.kr/www2/common/search.asp?kwd={kwd}&pageNum={page_count}&pageSize=10&category=TOTAL&sort=n&startDate=20190101&endDate=20200531&date=select&srchFd=&range=&author=all&authorData=&mysrchFd=%2FDate"
        r = requests.get(url)
        doc = BeautifulSoup(r.text, "lxml")  # read page

        article_max = int(doc.select("font")[1].text.replace(" articles]", "").replace("[", "").replace(",", ""))  # read the number of search results
        page_max = int(article_max / 10) + 1  # calculate number of pages (manual reading does not work, as paging a-tags are not found)
        print(article_max)
        print(page_max)

        # iterate over search result pages
        while (page_count <= page_max and do_break is False):
            time.sleep(.2)
            url = f"https://www.koreatimes.co.kr/www2/common/search.asp?kwd={kwd}&pageNum={page_count}&pageSize=10&category=TOTAL&sort=n&startDate=20200101&endDate=20200531&date=select&srchFd=&range=&author=all&authorData=&mysrchFd=%2FDate"
            print(f" ****** PAGE {page_count}: {url} ******")
            r = requests.get(url)
            doc = BeautifulSoup(r.text, "lxml")  # read current search result page
            article_counter = 0

            # iterate over articles on current search result page
            for article in doc.select("td > a"):
                date = article.find_next_sibling("div").select_one("font").text.strip()[-10:]  # assign article date (still from search results)
                if "2019" in date:  # no further than 2019
                    do_break = True
                    break

                article_url = article.attrs["href"]  # assign article url (still from search results)


                # read article page
                article_page = BeautifulSoup(requests.get(article_url).text, "lxml")
                # for linebreak in article_page.select('br'):
                #     linebreak.extract()
                # for img in article_page.select('img'):
                #     img.extract()
                if article_page.select_one(".view_headline.HD") is None:
                    print("break - no headline")
                    continue  # ignore article if article format is non-default
                title = article_page.select_one(".view_headline.HD").text  # assign article title

                author = None # author not present on every page

                content = article_page.select("#startts > span")  # find all paragraphs
                i = 0
                content_str = ""  # build iteratively

                # iterate over article page sections
                for paragraph in content:
                    text = paragraph.text
                    if len(paragraph.findChildren())>-1:
                        tag_list = []
                        content_list = []
                        children = paragraph.findChildren()
                        # print(children)
                        start = 0

                        for c in range(start, len(children)):
                            child = children[c]
                            if "By " in child.text and author is None and c <= 2:  # find author
                                author = children[c].text.replace("By ", "")  # assign author if there is any
                            elif len(child.text.strip())>0:
                                tag_list.append(child.name)
                                content_list.append(child.text.strip())

                        #if "strong" in tag_list:  # korean content in article
                        #    english_element_ids = [j for j, tag in enumerate(tag_list) if tag == "strong"] # english content in strong tags
                        #    english_content = [content_list[p] for p in english_element_ids]
                        #    text = "".join(english_content)  # overwrite text to append to only include english

                        if " ".join(content_list):  # sometimes children cannot be read, so use them only if they are longer than parent level text
                            text = " ".join(content_list)
                        elif author is not None:  # if content list is not used, but author exists, remove author from article text manually
                            text = paragraph.text[(3+len(author)):]

                    content_str = content_str + " " + text.strip()  # article content iteratively assign
                    # print(str(i)+": "+text)
                    i = i+1  # only for printing

                crawled = CrawledArticle(title, date, "Korea Times", author, content_str.strip(), article_url)
                # print(crawled)
                article_counter = article_counter +1
                articles.append(crawled)

            print(f"{article_counter}/10 articles read.")
            page_count = page_count + 1

        print(str(page_count-1) + " pages of search results processed.")
        print(str(len(articles))+ " articles fetched.")
        return articles

class CsvWriter():
    # WRITE CRAWLED ARTICLES
    def write(self, articles, filename):
        with open(filename, "w", encoding="utf-8") as file:
            csvWriter = csv.writer(file, delimiter=";", quotechar='"')
            csvWriter.writerow(["DATE", "PUBLISHER", "TITLE", "AUTHOR", "URL", "CONTENT"])
            for article in articles:
                csvWriter.writerow([article.date, article.publisher, article.title, article.author, article.url, article.content])

af = ArticleFetcher()
cw = CsvWriter()
# cw.write(af.fetch_koreaherald("coronavirus"),"articles_koreaherald_coronavirus_may.csv")
# cw.write(af.fetch_koreaherald("covid-19"),"articles_koreaherald_covid19_may.csv")
# cw.write(af.fetch_koreatimes("coronavirus"), "articles_koreatimes_coronavirus_may2.csv")
cw.write(af.fetch_koreatimes("covid-19"), "articles_koreatimes_covid19_may.csv")
