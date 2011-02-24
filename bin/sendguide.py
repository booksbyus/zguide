#! /usr/bin/python
#
#   sendguide.py - Upload the Guide to zguide.wikidot.com
#
#   Author: Pieter Hintjens <ph@imatix.com>
#   License: public domain
#
#   syntax: bin/sendpage.py
#
#   Supply your Wikidot user name as an environment variable APISITE_USER
#   Supply your Wikidot API key as an environment variable APISITE_KEY

from xmlrpclib import ServerProxy

#   Get authentication credentials
import os
user = os.environ ['APISITE_USER']
key  = os.environ ['APISITE_KEY']

def upload (name, title):
    print " - page:" + name
    file = open (name + ".wd", "r")
    content = ""
    for line in file:
        content = content + line

    #   Create XML/RPC connection to Wikidot
    server = ServerProxy ('https://' + user + ':' + key + '@www.wikidot.com/xml-rpc-api.php')
    server.pages.save_one ({
        "site": "zguide",
        "page": "page:" + name,
        "title": title,
        "content": content})
    return

#   Send page content (create or update)
upload ("chapter1", "Chapter 1")
upload ("chapter2", "Chapter 2")
upload ("chapter3", "Chapter 3")
upload ("chapter4", "Chapter 4")
