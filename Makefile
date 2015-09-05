#
# Created       : 2015 Sep 04 (Fri) 14:46:10 by Harold Carr.
# Last Modified : 2015 Sep 04 (Fri) 21:57:33 by Harold Carr.
#

b build : FORCE
	stack build

r run : FORCE
	stack exec utah-haskell-exe

t test : FORCE
	curl 127.0.0.1:3000 -X POST -d '{ "name": "H", "msg": { "txt": "ignore", "msgId":99 }}'
	curl 127.0.0.1:3000 -X POST -d '{ "name": "H", "msg": { "txt": "foldA1", "msgId" :1 }}'
	curl 127.0.0.1:3000 -X POST -d '{ "name": "H", "msg": { "txt": "foldA1", "msgId" :1 }}'
	curl 127.0.0.1:3000 -X POST -d '{ "name": "H", "msg": { "txt": "foldAX", "msgId" :2 }}'
	curl 127.0.0.1:3000 -X POST -d '{ "name": "H", "msg": { "txt": "foldA2", "msgId" :2 }}'


c clean : FORCE
	stack clean

FORCE ::
