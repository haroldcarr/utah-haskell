#
# Created       : 2015 Sep 04 (Fri) 14:46:10 by Harold Carr.
# Last Modified : 2015 Sep 04 (Fri) 17:07:55 by Harold Carr.
#

b build : FORCE
	stack build

r run : FORCE
	stack exec utah-haskell-exe

t test : FORCE
	curl 127.0.0.1:3000
	curl 127.0.0.1:3000 -d '{ "name" : "harold", "msg" : { "msgid" : " }'

c clean : FORCE
	stack clean

FORCE ::
