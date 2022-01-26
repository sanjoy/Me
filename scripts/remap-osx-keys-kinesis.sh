#!/bin/bash

KINESIS_PRODUCT_ID="0x9410"
KB_LEFT_ALT="0x7000000E2"
KB_LEFT_GUI="0x7000000E3"

hidutil property --matching "{\"ProductID\":${KINESIS_PRODUCT_ID}}" --set "
{\"UserKeyMapping\": 
 [
  {\"HIDKeyboardModifierMappingSrc\":${KB_LEFT_ALT},
   \"HIDKeyboardModifierMappingDst\":${KB_LEFT_GUI}},
  {\"HIDKeyboardModifierMappingSrc\":${KB_LEFT_GUI},
   \"HIDKeyboardModifierMappingDst\":${KB_LEFT_ALT}}
]}" > /tmp/remap-osx-keys-kinesis.log || echo "failed"
