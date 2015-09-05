#
# Created       : 2015 Sep 04 (Fri) 14:46:10 by Harold Carr.
# Last Modified : 2015 Sep 04 (Fri) 22:45:51 by Harold Carr.
#

b build : FORCE
	stack build

r run : FORCE
	stack exec utah-haskell-exe

t test : FORCE
	curl -i 127.0.0.1:3000     -X POST -d '{ "name": "H", "msg": "BAD" }}'
	curl -i 127.0.0.1:3000     -X GET  -d '{ "name": "H", "msg": { "txt": "ignore", "msgId":99 }}'
	curl -i 127.0.0.1:3000/foo -X POST -d '{ "name": "H", "msg": { "txt": "ignore", "msgId":99 }}'
	curl -i 127.0.0.1:3000     -X POST -d '{ "name": "H", "msg": { "txt": "ignore", "msgId":99 }}'
	curl -i 127.0.0.1:3000     -X POST -d '{ "name": "H", "msg": { "txt": "foldA1", "msgId" :1 }}'
	curl -i 127.0.0.1:3000     -X POST -d '{ "name": "H", "msg": { "txt": "foldA1", "msgId" :1 }}'
	curl -i 127.0.0.1:3000     -X POST -d '{ "name": "H", "msg": { "txt": "foldAX", "msgId" :2 }}'
	curl -i 127.0.0.1:3000     -X POST -d '{ "name": "H", "msg": { "txt": "foldA2", "msgId" :2 }}'

c clean : FORCE
	stack clean

FORCE ::
