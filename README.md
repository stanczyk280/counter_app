counter_app
=====

An OTP application

Simple app to demonstrate usage of mnesia with cowboy: classic curl, RESTapi and websockets.


curl messages examples:
-----
GET:
    curl -i http://localhost:8080/router?user_id=user123

POST:
     curl -X POST -H "Content-Type: application/json" -d '{"UserId":"user123", "Counter":"C1", "Value":10}' http://localhost:8080/router

PUT:
    curl -X PUT http://localhost:8080/router -H 'Content-Type: application/json' -d '{"UserId": "user123","Counter":"C2" ,"Value": 40}'

DELETE:
    curl -X DELETE 'http://localhost:8080/router?user_id=user123'
