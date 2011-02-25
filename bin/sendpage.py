#! /bin/python
#
#   sendpage.py - Send one Wikidot page to zguide.zeromq.org
#   Part of the ztools/apisite toolkit.
#
#   Author: Pieter Hintjens <ph@imatix.com>
#   License: public domain
#
#   syntax: python sendpage.py category page title
#
#   Supply your Wikidot user name as an environment variable APISITE_USER
#   Supply your Wikidot API key as an environment variable APISITE_KEY

#   This is the Wikidot site name for api.zero.mq
site = "zguide"

#   Get script arguments
import sys
category = sys.argv [1]
name     = sys.argv [2]
title    = sys.argv [3]

#   Get authentication credentials
import os
user = os.environ ['APISITE_USER']
key  = os.environ ['APISITE_KEY']

#   Create XML/RPC connection to Wikidot
from xmlrpclib import ServerProxy
server = ServerProxy ('https://' + user + ':' + key + '@www.wikidot.com/xml-rpc-api.php')

#   Send page content (create or update)
file = open (name + ".wd", "r")
content = ""
for line in file:
    content = content + line

print " - " + category + ":" + name
server.pages.save_one ({
    "site": site,
    "page": category + ":" + name,
    "title": title,
    "content": content})
