#
# Cloudway proxy rules
#

RewriteEngine On
ProxyPassInterpolateEnv On

# Set rewrite log level to 6 for debugging
#LogLevel alert rewrite:trace6

# Maps
RewriteMap lowercase int:tolower
RewriteMap cloudway-containers rnd:/var/lib/cloudway/.httpd/containers.txt
RewriteMap cloudway-mappings dbm=db:/var/lib/cloudway/.httpd/mappings.db
RewriteMap cloudway-aliases  dbm=db:/var/lib/cloudway/.httpd/aliases.db
RewriteMap cloudway-idles    dbm=db:/var/lib/cloudway/.httpd/idles.db

# Forbidden template files
RewriteRule \.cwt$ - [NS,F]

# Set X-Client-IP to the node which contacted the proxy
RewriteCond %{REMOTE_ADDR} ^(.*)$
RewriteRule ^.*$ - [E=X_CLIENT_IP:%1]
RequestHeader set X-Client-IP %{X_CLIENT_IP}e

# Resolve aliases
RewriteCond ${lowercase:%{HTTP_HOST}} ^([^:]+)
RewriteCond ${cloudway-aliases:%1} ^(.+)$ [OR]
RewriteCond ${lowercase:%{HTTP_HOST}} ^([^:]+)
RewriteRule ^.*$ - [E=V_MATCH_HOST:%1,NS]

# Randomly select a container to service the request.
# Allow override container id by setting the X-Container-Id header
RewriteCond %{HTTP:X-Container-Id} ^(.+)$ [OR]
RewriteCond ${cloudway-containers:%{ENV:V_MATCH_HOST}} ^(.+)$
RewriteRule ^.*$ - [E=V_MATCH_ID:%1,NS]

# Unidle and redirect back to the original request.
RewriteCond ${cloudway-idles:%{ENV:V_MATCH_ID}} ^(.+)$
RewriteRule ^/(.*)$ /var/www/html/unidle.php/%{ENV:V_MATCH_ID}/$1 [NS,L]

# Route resolution.
#
# Attempt to pull out up to a set number of path elements
# and populate the following variables
# 1. V_MATCH_PATH: The part of the path included in the match/route
# 2. V_PATH:       The remainder of the path
# 3. V_ROUTE:      The matched route
#
# A path element is a word with a starting slash (Apache normalizes
# before we see it). Note, We never include the trailing slash in
# the match, its always passed through.
#
# Example table entries
# www.example.com           => 127.0.250.1:8080
# www.example.com/foo       => 127.0.250.1:8080/bar
# www.example.com/bar       => REDIRECT:/baz

# Just match the host
RewriteRule ^(.*)$ - [E=V_TMP_MATCH_PATH:,E=V_TMP_PATH:$1,E=V_TMP_LVL:0,NS]

RewriteCond %{ENV:V_TMP_LVL} =0
RewriteCond %{ENV:V_MATCH_ID}%{ENV:V_TMP_MATCH_PATH} ^(.+)$
RewriteRule ^(.*)$ - [E=V_MATCH_LOOKUP:%{ENV:V_MATCH_ID}%{ENV:V_TMP_MATCH_PATH},E=V_MATCH_DEFAULT:__default__%{ENV:V_TMP_MATCH_PATH},NS]

RewriteCond %{ENV:V_TMP_LVL} =0
RewriteCond ${cloudway-mappings:%{ENV:V_MATCH_LOOKUP}} ^(.+)$ [OR]
RewriteCond ${cloudway-mappings:%{ENV:V_MATCH_DEFAULT}} ^(.+)$
RewriteRule ^(.*)$ - [E=V_ROUTE:%1,E=V_MATCH_PATH:/,E=V_PATH:%{ENV:V_TMP_PATH},NS]

# One path element (/a)
RewriteRule ^(/[^/]+)(.*)$ - [E=V_TMP_MATCH_PATH:$1,E=V_TMP_PATH:$2,E=V_TMP_LVL:1,NS]

RewriteCond %{ENV:V_TMP_LVL} =1
RewriteCond %{ENV:V_MATCH_ID}%{ENV:V_TMP_MATCH_PATH} ^(.+)$
RewriteRule ^(.*)$ - [E=V_MATCH_LOOKUP:%{ENV:V_MATCH_ID}%{ENV:V_TMP_MATCH_PATH},E=V_MATCH_DEFAULT:__default__%{ENV:V_TMP_MATCH_PATH},NS]

RewriteCond %{ENV:V_TMP_LVL} =1
RewriteCond ${cloudway-mappings:%{ENV:V_MATCH_LOOKUP}} ^(.+)$ [OR]
RewriteCond ${cloudway-mappings:%{ENV:V_MATCH_DEFAULT}} ^(.+)$
RewriteRule ^(.*)$ - [E=V_ROUTE:%1,E=V_MATCH_PATH:%{ENV:V_TMP_MATCH_PATH},E=V_PATH:%{ENV:V_TMP_PATH},NS]

# Two path elements (/a/b)
RewriteRule ^(/[^/]+/[^/]+)(.*)$ - [E=V_TMP_MATCH_PATH:$1,E=V_TMP_PATH:$2,E=V_TMP_LVL:2,NS]

RewriteCond %{ENV:V_TMP_LVL} =2
RewriteCond %{ENV:V_MATCH_ID}%{ENV:V_TMP_MATCH_PATH} ^(.+)$
RewriteRule ^(.*)$ - [E=V_MATCH_LOOKUP:%{ENV:V_MATCH_ID}%{ENV:V_TMP_MATCH_PATH},E=V_MATCH_DEFAULT:__default__%{ENV:V_TMP_MATCH_PATH},NS]

RewriteCond %{ENV:V_TMP_LVL} =2
RewriteCond ${cloudway-mappings:%{ENV:V_MATCH_LOOKUP}} ^(.+)$ [OR]
RewriteCond ${cloudway-mappings:%{ENV:V_MATCH_DEFAULT}} ^(.+)$
RewriteRule ^(.*)$ - [E=V_ROUTE:%1,E=V_MATCH_PATH:%{ENV:V_TMP_MATCH_PATH},E=V_PATH:%{ENV:V_TMP_PATH},NS]

# Three path elements (/a/b/c)
RewriteRule ^(/[^/]+/[^/]+/[^/]+)(.*)$ - [E=V_TMP_MATCH_PATH:$1,E=V_TMP_PATH:$2,E=V_TMP_LVL:3,NS]

RewriteCond %{ENV:V_TMP_LVL} =3
RewriteCond %{ENV:V_MATCH_ID}%{ENV:V_TMP_MATCH_PATH} ^(.+)$
RewriteRule ^(.*)$ - [E=V_MATCH_LOOKUP:%{ENV:V_MATCH_ID}%{ENV:V_TMP_MATCH_PATH},E=V_MATCH_DEFAULT:__default__%{ENV:V_TMP_MATCH_PATH},NS]

RewriteCond %{ENV:V_TMP_LVL} =3
RewriteCond ${cloudway-mappings:%{ENV:V_MATCH_LOOKUP}} ^(.+)$ [OR]
RewriteCond ${cloudway-mappings:%{ENV:V_MATCH_DEFAULT}} ^(.+)$
RewriteRule ^(.*)$ - [E=V_ROUTE:%1,E=V_MATCH_PATH:%{ENV:V_TMP_MATCH_PATH},E=V_PATH:%{ENV:V_TMP_PATH},NS]

# Route based on the populated variables

# Return 410 Gone
RewriteCond %{ENV:V_ROUTE} =GONE [NC]
RewriteRule ^(.*)$ - [E=V_ROUTE:127.0.0.1%{ENV:V_MATCH_PATH},NS,G]

# Return 403 Forbidden
RewriteCond %{ENV:V_ROUTE} =FORBIDDEN [NC]
RewriteRule ^(.*)$ - [E=V_ROUTE:127.0.0.1%{ENV:V_MATCH_PATH},NS,F]

# Do not proxy
RewriteCond %{ENV:V_ROUTE} =NOPROXY [NC]
RewriteRule ^(.*)$ - [E=V_ROUTE:127.0.0.1%{ENV:V_MATCH_PATH},NS,L]

# Issue a redirect
RewriteCond %{ENV:V_ROUTE} ^REDIRECT:(.*)$ [NC]
RewriteRule ^(.*)$ %1%{ENV:V_PATH} [E=V_ROUTE:127.0.0.1%{ENV:V_MATCH_PATH},R,NS,L]

# By default, attempt to proxy to the match
RewriteCond %{ENV:V_ROUTE} ^.+$
RewriteRule ^(.*)$ %{ENV:V_ROUTE}%{ENV:V_PATH} [P,NS]

# No route, set ProxyPassReverse with valid but useless data
RewriteRule ^(.*)$ - [E=V_ROUTE:127.0.0.1/,E=V_MATCH_PATH:/]

# Configure reverse proxy based on set variable
ProxyPassReverse ${V_MATCH_PATH} ${V_ROUTE} interpolate

# Log custom format for parsing
LogFormat "%h %{V_MATCH_HOST}e %l %u %t \"%r\" %>s %b \"%{Referer}i\" \"%{User-Agent}i\" %{V_ROUTE}e%{V_PATH}e" cloudway
CustomLog logs/cloudway_log cloudway env=V_PATH

# Pipe output to auto idler
CustomLog "|/usr/share/cloudway/libexec/cwautoidle.py" "%{V_MATCH_ID}e" env=V_PATH
