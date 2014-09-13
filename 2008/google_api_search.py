# GoogleApiSearch
# 
# A very simple interface to the Google AJAX Search API.
#
# Depends on the following modules:
#   - demjson:
#       For parsing the JSON results returned by the Google API
#
# Eli Bendersky (http://eli.thegreenplace.net)
# This code is in the public domain
#
import logging
import pprint
import urllib

import demjson

# Restrictions of the Google API
#
MAX_PAGES = 4
MAX_RESULTS_PER_PAGE = 8


class GoogleApiSearch(object):
    """ A simple interface to the Google AJAX Search API.
        http://code.google.com/apis/ajaxsearch/documentation/
        
        Usage:
        Call search() with a search string. Then, inspect how many
        results Google finds with the result_count attribute,
        and the first MAX_PAGES * MAX_RESULTS_PER_PAGE results
        with the result_urls attribute.
    """
    def __init__(self):
        self._clear_results()
    
    def search(self, what):
        self._clear_results()

        data = self._exec_query(what, 0)
        
        if not data['cursor'].has_key('estimatedResultCount'):
            return
        
        self.result_count = data['cursor']['estimatedResultCount']
        self.result_urls = self._get_result_urls(data)
        
        for page in xrange(1, MAX_PAGES):
            start = page * MAX_RESULTS_PER_PAGE
            if start > self.result_count:
                break
        
            data = self._exec_query(what, start)
            self.result_urls += self._get_result_urls(data)        
    
    ########################### PRIVATE ##########################
    
    def _clear_results(self):
        self.result_count = 0
        self.result_urls = []
    
    def _exec_query(self, search_for, start):
        query = urllib.urlencode(
                {
                    'q': search_for,
                    'v': '1.0',
                    'rsz': 'large',
                    'safe': 'off',
                    'start': start,
                }            
        )
        
        base = 'http://ajax.googleapis.com/ajax/services/search/web?'
        query_url = base + query
        
        logging.info('Executing: ' + query_url)
        
        results_json = urllib.urlopen(query_url).read()
        results_data = demjson.decode(
                        results_json, encoding='utf8')
    
        return results_data['responseData']
    
    def _get_result_urls(self, result_data):
        urls = []
        
        for hit in result_data['results']:
            urls.append(urllib.unquote(hit['url']))
        
        return urls


if __name__ == "__main__":
    #~ logging.basicConfig(level=logging.INFO)

    gas = GoogleApiSearch()
    gas.search('python game')
    print gas.result_count
    print len(gas.result_urls)
    pprint.pprint(gas.result_urls)




