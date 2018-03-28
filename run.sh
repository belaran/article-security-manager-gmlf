#!/bin/bash

readonly POLICY=${POLICY:-'main.policy'}
readonly SM=${SM:- -Djava.security.manager -Djava.security.policy=file://$(pwd)/${POLICY}}

scala -cp ~/.m2/repository/org/jsoup/jsoup/1.8.3/jsoup-1.8.3.jar ${SM} ${1}
