import urllib
import re
import requests
from collections import OrderedDict
from bs4 import BeautifulSoup
from urllib import urlopen


### Scrape the global top 500 Websites and store the data in a txt file called "record.txt"
f=open("record.txt","a")
for i in range(20):
    url='http://www.alexa.com/topsites/global;'+ str(i)
    text = requests.get(url).text
    soup = BeautifulSoup(text)
    nameList = soup.findAll("p", {"class":"desc-paragraph"})
    for name in nameList:
      M=name.get_text()
      f.write(M)
f.close()


### Scrape the links for Every countries and store the data in a txt file called "record1.txt"
### Scrape the countries information for global Top-500 websites 
### and store the data in a txt file called "record2.txt"
f1=open("record1.txt","a")
f3=open("record2.txt","a")
CountryURL = urlopen("http://www.alexa.com/topsites/countries")
soup2 = BeautifulSoup(CountryURL.read())
countries=soup2.findAll("a",{"href":re.compile("\/topsites\/countries\/")})
for country in countries:
  M=country["href"]
  N=country.get_text()
  f1.write(M+"\n")
  f3.write(N+"\n")
f1.close()
f3.close()


### Scrape the links for every countries and store the data in a txt file called "record3.txt"
f = open('record.txt', 'r') # 'r' for read
lines3 = f.readlines()
f.close()
for i in range(len(lines3)):
  url="http://www.alexa.com/siteinfo/" + lines3[i]
  text = requests.get(url).text
  soup = BeautifulSoup(text)
  TT=soup.find("span",{"data-cat":"countryRank"}).find("a").get_text()
  f4 = open('record3.txt', 'a')
  f4.write(TT+"\n")
  f4.close()

  f2 = open('record1.txt', 'r') # 'r' for read
lines = f2.readlines()
f2.close()
print lines
## "http://www.alexa.com" + lines[1].strip()
f3 = open('record2.txt', 'r') # 'r' for read
lines2 = f3.readlines()
f3.close()


### Create an empty dictionary "M" for the storage of the Top-500 websites for each country
M=OrderedDict()
print lines2
for country in lines2:
    M[country.strip()]=[]
print M

### export the dictionary "M" as json file
import json
json_str = json.dumps(M)
print(M)
f = open('Site.json', 'wb')
f.write(json_str)
f.close()