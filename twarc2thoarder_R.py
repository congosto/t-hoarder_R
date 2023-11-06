#!/usr/bin/python
# -*- coding: iso-8859-1 -*-
# Creative commons 3.0 spain
# http://creativecommons.org/license#!/usr/bin/python
from __future__ import print_function
import codecs
import re
import simplejson as json
import csv
import argparse


  
def main():
  #reload(sys)
  #sys.setdefaultencoding('utf-8')
  #sys.stdout = codecs.getwriter('utf-8')(sys.stdout)
  parser = argparse.ArgumentParser(description='ls this script classifies users into categories according to their activity')
  parser.add_argument('file_in', type=str, help='file with twarc tweets')
  parser.add_argument('file_out', type=str,default='', help='file with thoarder tweets')
  args = parser.parse_args()
  file_in = args.file_in
  file_out = args.file_out
  dict_users={}
  input_file = codecs.open(file_in,'r',encoding='utf-8')
  f=codecs.open(file_out, 'w',encoding='utf-8',errors='ignore') 
  log_file = codecs.open(file_out+'.log', "wb",encoding='utf-8')
  last_line=''
  wrong=False
  count_wrong=0
  num_tweets=0
  writer = csv.writer(f,delimiter=',')
  head = ['id_tweet','date','author','text','app','id_user','followers','following','statuses','location','urls','geolocation','name','description','url_media','type_media','quoted',
          'relation','replied_id','user_replied','retweeted_id','user_retweeted','quoted_id','user_quoted','first_HT','lang','created_at','verified','avatar','link','retweet_count',
          'reply_count','quote_count','favorite_count','impression_count','verified_type']
  writer.writerow(head)
  for line in input_file:
      try:
        object= json.loads(line)
        data = object['data']
        includes = object['includes']
        users = includes['users']
        if 'tweets' in includes:
          tweets_related = includes['tweets']
          size_tweets_related = len (tweets_related)
        else:
          size_tweets_related =0
        size_data = len (data)
        size_users = len (users)
        dict_users ={}
        dict_tweets_related ={}
 # get users data
        for i in range (0,size_users):
          user = users [i]
          location=None
          description=None
          if 'location' in user:
            location=re.sub('[\r\n\t]+', ' ',user['location'],re.UNICODE)
          if 'description' in user:
            description=re.sub('[\r\n\t]+', ' ',user['description'],re.UNICODE)
          id_user =user['id']
          profile = (user[u'username'], user['name'],user['created_at'],description,
                     location,user['public_metrics']['followers_count'],
                     user['public_metrics']['following_count'],user['public_metrics']['tweet_count'],
                     user['verified'],user['profile_image_url'],user['verified_type'])
          dict_users[id_user] = profile
 # get tweet related
        for i in range (0,size_tweets_related):
          tweet_related = tweets_related [i]
          id_related = tweet_related['id']
          author_id_related = tweet_related ['author_id']
          text_related = re.sub('[\r\n\t]+', ' ',tweet_related ['text'])
          (author,name,since,description,location,followers_count,following_count,tweet_count,verified,avatar,verified_type) = dict_users [author_id_related]
          dict_tweets_related [id_related] = (author,text_related)
 # get data 
        for i in range (0,size_data):
          num_tweets=num_tweets +1 
          if num_tweets % 10000 == 0:
            print(num_tweets)
  # defauld values
          entities=None
          relation=None
          replied_id=None
          replied_screen_name=None
          retweeted_id=None
          retweeted_screen_name=None
          quoted_id=None
          quoted_screen_name=None
          quoted_text = None
          first_HT=None
          geoloc=None
          url_expanded =None
          url_media=None
          type_media=None
          text=None
          name=None
          retweet_count =0
          quote_count=0
          reply_count=0
          like_count =0
# iteration
          tweet = data [i]
# basic info
          id_tweet=tweet['id']
          author = user['username']
          date = re.sub('T',' ',tweet['created_at'])
          date = re.sub('.000Z',' ',date)
          if 'source' in tweet:
            app=tweet['source']
          else:
            app="None"
#get user profile
          id_user = tweet ['author_id'] 
          (author,name,since,description,location,followers_count,following_count,tweet_count,verified,avatar,verified_type) = dict_users [id_user]
          since= re.sub('T',' ',since)
          since = re.sub('.000Z',' ',since)
#get interactions Ids
          if 'referenced_tweets' in tweet:
            reference_tweet = tweet['referenced_tweets'][0]
            type_relation = reference_tweet['type']
            id_related = reference_tweet['id']
            if id_related in dict_tweets_related:
               (author_related, text_related) = dict_tweets_related[id_related]
               author_related = '@'+author_related
            else:
              author_related = None
              text_related =None
            if type_relation == 'replied_to':
              relation='reply'
              replied_id= id_related
              replied_screen_name = author_related
            elif type_relation == 'retweeted':
              relation='RT'
              retweeted_id= id_related
              retweeted_screen_name = author_related
            elif type_relation == 'quoted':
              relation='quoted'
              quoted_id= id_related
              quoted_screen_name = author_related
              quoted_text = text_related

#get entities
          if 'entities' in tweet:
            entities=tweet['entities']
            if 'urls' in entities:
              urls=entities['urls']
              if len (urls) >0:
                url_expanded= urls[0]['expanded_url']
            if 'media' in entities:
              list_media=entities['media']
              if len (list_media) >0:
                url_media= list_media[0]['media_url']
                type_media=list_media[0]['type']
            if 'hashtags' in entities:
              HTs=entities['hashtags']
              if len (HTs) >0:
                first_HT=HTs[0]['tag']
#get text
          if 'text' in tweet:
            text=re.sub('[\r\n\t]+', ' ',tweet['text'])
#get quoted if exist
#get metrics 
          if 'public_metrics' in tweet:
            metrics= tweet['public_metrics']
            retweet_count = metrics ['retweet_count']
            reply_count=  metrics['reply_count']
            quote_count= metrics['quote_count']
            like_count =  metrics['like_count']
            impression_count = metrics['impression_count']

          link_tweet= 'https://twitter.com/%s/status/%s' % (author,id_tweet)
          row=[]
          row.append(id_tweet)
          row.append(date)
          row.append('@'+author)
          row.append(text)
          row.append(app)
          row.append(id_user)
          row.append(followers_count)
          row.append(following_count)
          row.append(tweet_count)
          row.append(location),
          row.append(url_expanded)
          row.append(geoloc)
          row.append(name)
          row.append(description)
          row.append(url_media)
          row.append(type_media)
          row.append(quoted_text)
          row.append(relation)
          row.append(replied_id)
          row.append(replied_screen_name)
          row.append(retweeted_id)
          row.append(retweeted_screen_name),
          row.append(quoted_id)
          row.append(quoted_screen_name)
          row.append(first_HT)
          row.append(tweet['lang']),
          row.append(since)
          row.append(verified)
          row.append(avatar)
          row.append(link_tweet)
          row.append(retweet_count)
          row.append(reply_count)
          row.append(quote_count),
          row.append(like_count)
          row.append(impression_count)
          row.append(verified_type)
          writer.writerow(row)
      except Exception as  err:
        str_error=str(err)
        if str_error.find('Unterminated string') != -1:
          last_line=line
          wrong=True
        elif str_error.find('Invalid control') != -1:
          last_line=line[:-1]
          count_wrong +=1
          wrong=True
        else:
          log_file.write('---------------------------------------------\n')
          log_file.write('Exception Error: %s\n' % (str(err)))
          log_file.write('not match %s\n' % (line))

  f.close()
  print ('fin')
  #exit(0) 
if __name__ == '__main__':
  try:
    main()
  except KeyboardInterrupt:
    print('\nGoodbye!')
    #exit(0)
