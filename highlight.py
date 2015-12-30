#!/usr/bin/env python
# $ python highlight.py source.cpp 
# $ chrome open source.cpp.html

import sys
import subprocess
from math import log10,ceil

header="""
<html>
<head>
<style type=text/css> 
a:link {
    background-color: #f8f8f8
}
  
a:hover {
    background-color: #eef;
}
span:target { 
  background-color: yellow;
} 
div.CodeBox {
  padding:2px;
}
pre.LineNumbers {
  float: left;
  padding-right:2px;
  border: solid 2px #888;
  margin-right:7px;
}
</style>
</head>
<body style=background-color:#ffffff>
<tt>
<div class="CodeBox">
"""
footer="""
</div>
</tt>
</body>
</html>
"""

def formatted_int(i,imax):
  nmax = int(ceil(log10(imax+0.1)))
  n    = int(ceil(log10(i+0.1)))
  val  =""
  for k in range(0,nmax-n):
   val += " "
  val += str(i)
  return val

fin=sys.argv[1];
fout=fin+".html"

cmd='highlight --syntax c++  --include-style --style zellner --inline-css -f %s' % fin;
p = subprocess.Popen(cmd.split(), stdout=subprocess.PIPE, stderr=subprocess.PIPE)
out,err = p.communicate()
line_count = 0;
body = out.split('\n')
body_anchored = "<pre>"
for line in body:
  line_count += 1;
  body_anchored += "<span id=\""+str(line_count)+"\"> "+line+"</span>\n"
body_anchored+="</pre>"

line_numbers = "<pre class=\"LineNumbers\">"
total_line_count = line_count;
line_count = 0;
for line in body:
  line_count += 1
  line_numbers += "<a href=\"#%s\" style=\"text-decoration:none\"><span style=\"color:#888888;\">  %s </span></a>\n" % \
    (line_count, formatted_int(line_count, total_line_count))
line_numbers += "</pre>"


print "Writing output to %s" % fout
fout = open(fout, 'w')
fout.write(header+'\n')
fout.write(line_numbers)
fout.write(body_anchored)
fout.write(footer+'\n')
fout.close()
  
     


    
 



  
