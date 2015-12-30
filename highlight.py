#!/usr/bin/env python
# $ python highlight.py source.cpp 
# $ chrome open source.cpp.html

import sys
import subprocess
from math import log10,ceil

header="""
<html>
<head>
<script src="http://ajax.aspnetcdn.com/ajax/jQuery/jquery-1.11.3.min.js"></script>
<script>
function keepLocation(oldOffset) {
  if (window.pageYOffset!= null){
    st=oldOffset;
  }
  if (document.body.scrollWidth!= null){
    st=oldOffset;
  }
  setTimeout('window.scrollTo(0,st)',0);
}
function scrollDown() {
  var url = document.location.href;
  if (url.indexOf('#') != -1)
  {
    st = $(window.location.hash).offset().top-window.innerHeight/3;
    setTimeout('window.scrollTo(0,st)',0);
  }
}
</script>
<style type=text/css> 
a:link {
    background-color: #f8f8f8
}
  
a:hover {
    background-color: #eef;
}
span:target { 
  background-color: #f6ebbb;
} 
pre { margin: 0; }
div.CodeBox {
  padding:2px;
}
pre.LineNumbers {
  float: left;
  padding-right:0px;
  border: solid 1px #ddd;
  margin-right:0px;
}
</style>
</head>
<body style=background-color:#ffffff onload="scrollDown()">
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

style='zellner'
style='solarized-light'
cmd='highlight --syntax c++  --include-style --style %s --inline-css -f %s' % (style,fin);
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
  js_line = "onclick=\"keepLocation(window.pageYOffset);\""
  line_numbers += "<a href=\"#%s\" style=\"text-decoration:none\"><span style=\"color:#888888;\" %s>  %s </span></a>\n" % \
    (line_count, js_line, formatted_int(line_count, total_line_count))
line_numbers += "</pre>"


print "Writing output to %s" % fout
fout = open(fout, 'w')
fout.write(header+'\n')
fout.write(line_numbers)
fout.write(body_anchored)
fout.write(footer+'\n')
fout.close()
  
     


    
 



  
