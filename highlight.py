#!/usr/bin/env python
# $ python highlight.py source.cpp 
# $ chrome open source.cpp.html

import sys
import subprocess
from math import log10,ceil

header="""
<html>
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
</style>
<pre><tt>
<body style=background-color:#ffffff>
"""
footer="""
</tt></pre>
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
dst = ""
for line in body:
  line_count += 1;
  dst += "<span id=\""+str(line_count)+"\"> "+line+"</span>\n"
#print dst
#sys.exit(0)

body = dst.split('\n')[:-1]
dst = ""
total_line_count = line_count;
line_count = 0;
for line in body:
  line_count += 1
  dst += "<a href=\"#%s\" style=\"text-decoration:none\"><span style=\"color:#888888;\">  %s </span></a>%s\n" % \
    (line_count, formatted_int(line_count, total_line_count), line)



print "Writing output to %s" % fout
fout = open(fout, 'w')
fout.write(header+'\n')
fout.write(dst)
fout.write(footer+'\n')
fout.close()
  
     


    
 



  
