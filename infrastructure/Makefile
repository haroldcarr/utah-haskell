#
# Created       : 2015 Sep 04 (Fri) 14:46:10 by Harold Carr.
# Last Modified : 2015 Sep 05 (Sat) 10:52:53 by Harold Carr.
#

b build : FORCE
	stack build

r run : FORCE
	stack exec utah-haskell-exe

t test : FORCE
	curl -i 127.0.0.1:3000     -X POST -d '{ "name": "H", "msg": "BAD" }}'
	curl -i 127.0.0.1:3000     -X GET  -d '{ "name": "H", "msgId":99, "txt": "ignore" }'
	curl -i 127.0.0.1:3000/foo -X POST -d '{ "name": "H", "msgId":99, "txt": "ignore" }'
	curl -i 127.0.0.1:3000     -X POST -d '{ "name": "H", "msgId":99, "txt": "ignore" }'
	curl -i 127.0.0.1:3000     -X POST -d '{ "name": "H", "msgId" :1, "txt": "foldA1" }'
	curl -i 127.0.0.1:3000     -X POST -d '{ "name": "H", "msgId" :1, "txt": "foldA1" }'
	curl -i 127.0.0.1:3000     -X POST -d '{ "name": "H", "msgId" :2, "txt": "foldAX" }'
	curl -i 127.0.0.1:3000     -X POST -d '{ "name": "H", "msgId" :2, "txt": "foldA2" }'

h hlint : FORCE
	hlint app/*.hs src/*.hs

c clean : FORCE
	hcRReap
	stack clean

FORCE ::
