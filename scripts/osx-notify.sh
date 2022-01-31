#!/bin/bash

time $@

osascript -e "display notification \"\\\"$@\\\" exited with $?\" with title \"Finished execution\""
