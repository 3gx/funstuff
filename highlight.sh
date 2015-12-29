#!/bin/sh
# Usage
# $ cat source.cpp | ./highlight.sh > source.cpp.html
# $ chrome source.cpp.html
# ... enjoy :)
# Required highlight tool: http://andre-simon.de/index.php
#
echo "<html>
<style type="text/css"> 
a:hover {
    background-color: #eef;
}
a:target { 
  background-color: yellow;
} 
</style>
<pre><tt>
<body style="background-color:#ffffff">
"
highlight --syntax c++  --include-style --line-numbers --style zellner --inline-css -f | \
  awk '{print "<a href=\"#L"(NR+1)"\" name=\"L"(NR+1)"\" style=\"text-decoration:none\">"$0"</a>"; }'
echo "</tt></pre>
</body>
</html>"
