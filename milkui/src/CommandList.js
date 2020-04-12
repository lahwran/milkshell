import React from 'react'
import { useStreamReducer } from './ws'
import Command from './Command'


function streamReducer(state, action) {
  switch (action.type) {
    case 'newState':
      return action.state
    default:
      return state
  }
}
export default function CommandList() {
  //const [state, dispatch] = useReducer(wsreducer, {meta: [], value: '', error: null});
  const [state, send] = useStreamReducer("commandlist", streamReducer, {
    commands: [],
    commandListLoading: true
  })

  return <>
    {state.commands.map((command, idx) => <Command key={idx} id={command.id} dispatch={send}/>)}
  </>
}
