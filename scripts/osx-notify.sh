#!/bin/bash

time $@
exit_code=$?

osascript -e "display notification \"\\\"$@\\\" exited with $exit_code\" with title \"Finished execution\""

exit $exit_code
