import React from 'react'
import { useStreamReducer } from './ws'
import { TextInput, Text } from 'react-native'

function debugReducer(state, action) {
  // TODO: anything at all
  return state;
}
function streamReducer(state, action) {
  return state;
}

export default function Command() {
  const [debugState, sendDebug] = useStreamReducer(`commands/${id}/debug`, debugReducer, {})
  const [stdioState, sendStdio] = useStreamReducer(`commands/${id}/stdio`, streamReducer, {
    messages: []
  })
  return <Text>I'm a command yay</Text>
}
