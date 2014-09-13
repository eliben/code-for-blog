# GoogleWebSearch
# 
# Useful for executing web searches on Google and returning 
# result URLs.
#
# Depends on the following modules:
#   - BeautifulSoup:
#       For parsing the HTML results returned by Google
#   - mechanize:
#       For executing Google search queries
#
# Eli Bendersky (http://eli.thegreenplace.net)
# This code is in the public domain, given that you won't use it
# for swamping Google with search queries, since this is against
# Google's terms of service.
# It is to serve as an example and for limited personal use only.
#
import logging
import pprint
import sys
import socket
import urllib
from urllib2 import HTTPError

from BeautifulSoup import BeautifulSoup
import mechanize


class GoogleWebSearch(object):
    """ An interface to the Google web search.
        
        Usage:
        Call search() with your search string and the page number
        for Google. The page number is the number of the results
        page returned by Google for the search. If makes sense to
        set it to 1 the first time you issue a search.
        
        Then, call get_result_count() to get the total amount of
        results found by Google.
        
        get_result_urls() will return the URLs found in the last
        call to search()
    """
    def __init__(self):
        # In order not to hang forever if the server doesn't reply
        #
        socket.setdefaulttimeout(5)
        
        # You can insert your browser's header here, if you want
        # Find the header by placing:
        #   javascript:document.writeln(navigator.userAgent)
        # 
        # In the address bar
        #
        browser_header = ' '.join([
            'Mozilla/5.0',
            '(Windows; U; Windows NT 5.1; en-GB; rv:1.9.0.1)',
            'Gecko/2008070208 Firefox/3.0.1'])
        
        self.browser = mechanize.Browser()
        self.browser.addheaders = [('User-agent', browser_header)]
        self.browser.set_handle_robots(False)
        
        self.result_page = ''
        
    def search(self, search_for, page=1):
        """ Search for the string, and fetch the specified result 
            page. page must be >= 1
        """
        self.result_page = ''
        
        assert page > 0
        start = (page - 1) * 10
        
        query = urllib.urlencode(
            {
                'q': search_for,
                'start': start,
                'hl': 'en',
                'sa': 'N',
            }            
        )
        
        base = 'http://www.google.com/search?'
        query_url = base + query

        logging.info('Executing: ' + query_url)

        self.browser.open(query_url)
        self.result_page = self.browser.response().read()

    def get_result_count(self):
        soup = BeautifulSoup(self.result_page)
        
        ssb = soup.findAll('div', attrs={'id': 'ssb'})
        lines = ssb[0].p.contents
        
        for i in xrange(len(lines)):
            if lines[i].find('about') > 0 and i < len(lines) - 1:
                return int(lines[i + 1].contents[0].replace(',', ''))
                
        return 0

    def get_result_urls(self):
        soup = BeautifulSoup(self.result_page)

        res = soup.findAll('div', attrs={'id': 'res'})[0]
        glist = res.findAll('li', attrs={'class': 'g'})
        
        urls = []
        
        for g in glist:
            link = g.h3.a
            urls.append(link['href'])

        return urls


if __name__ == "__main__":
    #~ logging.basicConfig(level=logging.INFO)
    
    gws = GoogleWebSearch()
    gws.search('python game', 2)
    
    print gws.get_result_count()
    pprint.pprint(gws.get_result_urls())
