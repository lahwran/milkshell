import React from 'react'
import { useStreamReducer } from './ws'
import Command from './Command'

function streamReducer(state, action) {
  switch (action.type) {
    case 'receive':
      switch (action.message.type) {
        default:
          return {...state, messages: [action.message, ...(state.messages.length > 5 ? state.messages.slice(0, 5) : state.messages)]}}
    case 'send':
      switch (action.message.type) {
        case 'inputChange':
          return {...state, value: action.message.value}
        default: return state
      }
    default: return state
  }
}

export default function CommandList() {
  //const [state, dispatch] = useReducer(wsreducer, {meta: [], value: '', error: null});
  const [state, send] = useStreamReducer("commandlist", streamReducer, {
    commands: [],
  })

  return <>
    {state.commands.map((command, idx) => <Command key={idx} idx={idx}/>)}
  </>
}
