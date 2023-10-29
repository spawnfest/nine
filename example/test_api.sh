#! /bin/sh

curl -X POST http://localhost:3000/api/todo \
        --header "Content-Type: application/json" \
        -d '{"body":"foobar"}'

echo

curl http://localhost:3000/api/todo/0

echo

curl -X PUT http://localhost:3000/api/todo \
        --header "Content-Type: application/json" \
        -d '{"id": 0, "body":"barfoo"}'

echo

curl http://localhost:3000/api/todo/0

echo

curl -X DELETE http://localhost:3000/api/todo \
        --header "Content-Type: application/json" \
        -d '{"id": 0}'

echo

curl http://localhost:3000/api/todo/0

echo

curl -X POST http://localhost:3000/api/todo \
        --header "Content-Type: application/json" \
        -d '{"body":"stuff"}'

echo

curl -X POST http://localhost:3000/api/todo \
        --header "Content-Type: application/json" \
        -d '{"body":"things"}'

echo

curl http://localhost:3000/api/todos

echo
