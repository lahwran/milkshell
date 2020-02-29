/**
 * Sample React Native App
 * https://github.com/facebook/react-native
 *
 * @format
 * @flow
 */

import React, {Fragment, useState} from 'react';
import {
  SafeAreaView,
  View,
  Text,
  TextInput,
  StatusBar,
} from 'react-native';
import styled from 'styled-components/native';


const BASE_COLORS = {
  mainWhite: '#ede8e1'//'#e6ddce'
}
const COLORS = {
  bg: BASE_COLORS.mainWhite
}

const StatusLabel = styled(Text)`
  padding: 4px;
`;

const Monospace = styled(Text)`
  font-family: monospace;
`;

import {
  Header,
  LearnMoreLinks,
  Colors,
  DebugInstructions,
  ReloadInstructions,
} from './NewAppScreen/index';

import { useStreamReducer, useWebSocketReconnect, WSProvider } from './ws'

// TODO: POTENTIAL SECURITY VULNERABILITY
// remote code can invoke any attribute of funcs :s
// how 2 hashmap
// TODO: use more efficient protocol
// TODO: check missing/invalid fields
// TODO: print names of fields in errors
// TODO: merge typehandlers and unpackers? do we want to? typehandlers are more checked
function handler(instantiate, attrs) {
  return {
    isHandler: true, // TODO: to ensure we don't accidentally read from __proto__, can we do better?
    instantiate, // func to convert
    unpacker: (params, sd) => { // todo: make this into a more general wrapper?
      console.log("handler unpacker", params, sd)
      return attrs.reduce(
        (accum, [name, u]) => ({...accum, [name]: u(params[name], sd)}),
        {}
      )
    }
  }
}

class UnpackException extends Error {}

function RemoteTextInput({value, onChange}) {
  const [vState, setVState] = useState(value);
  return <TextInput value={vState} onChangeText={(newValue) => {setVState(newValue); onChange(newValue)}}/>
}

// type specifications
const unpack = {
  integer: (val) => parseInt(val),
  string: (val) => val, // TODO: check type
  array: (val, sd) => {
    console.log(`unpack array`, val, sd);
    return val.map((item, index) => unpack.tagged(item, sd, {index}))
  },
  tagged: ([type, val], sd) => {
    const handler = typehandlers[`${type}__type`]
    if (!handler || !handler.isHandler) { throw new UnpackException(`nonexistant type: '${type}'`) }
    const unpacked_val = handler.unpacker(val, sd);
    console.log(`unpack "${type}"`, val, sd, typehandlers, "unpacked val:", unpacked_val)
    return handler.instantiate(unpacked_val, sd);
  }
}

// type specification annotations
const wrappers = {
  required: (unpacker, allowNull=false) => (val, sd) => {
    if (val === null && !allowNull) {
      throw new UnpackException(`null not allowed here`)
    } else if (val === undefined) {
      throw new UnpackException(`field is required`)
    }
    return unpacker(val, sd);
  },
  optional: (unpacker, def=undefined, allowNull=false) => (val, sd) => {
    if (val === null && !allowNull) {
      throw new UnpackException(`null not allowed here`)
    }
    if (val === undefined || val === null) {
      return def
    }
    return unpacker(val, sd);
  }
}
// public obj type handlers
const typehandlers = {
  // postfixed with type__ to prevent possible security issue. I hate javascript, I wish I understood guarantees about object attrs better
  stream__type: handler((params, streamdispatch) => (message) => streamdispatch(params, message), [
    ["sid", wrappers.required(unpack.integer)]
  ]),
  view__type: handler(({...p}) => <View {...p}/>, [
    ["children", wrappers.optional(unpack.array)]
  ]),
  plaintext__type: handler(({value}) => <Text>{value}</Text>, [
    ["value", wrappers.required(unpack.string)]
  ]),
  textinput__type: handler((p) => <RemoteTextInput {...p}/>, [
    ["value", wrappers.required(unpack.string)],
    ["onChange", wrappers.required(unpack.tagged)],
  ]),
  //string__type: handler(({value}) => <Text>{value}</Text>, [
  //  ["value", wrappers.required(unpack.string)]
  //])
  //// seems a little dumb to duplicate this
  //integer__type: handler(({value}) => {value}, [
  //  ["value", wrappers.required(unpack.integer)]
  //])
};

function reactjoin(seq, map, joiner) {
  return seq.map((s, i) => i === 0 ? [map(s)] : [joiner(i), map(s)]).flat(1)
}
function ParsedCommand({value}) {
  return <Text>{value.join(" ")}</Text>;
}
function ParsedPipeline({value}) {
  if (!value) {
    return <Text> no parse </Text>
  }
  return reactjoin(value,
    (command, idx) => <ParsedCommand key={idx} value={command}/>,
    (idx) => <Text key={`j${idx}`}>{" | "}</Text>
  )
  
}
function ParsedTextDemo({value}) {
  if (!value) {
    return <Text> no parse </Text>
  }
  return <>{value.map((pipeline, idx) => <ParsedPipeline key={idx} value={pipeline}/>)}</>;
}

function streamReducer(state, action) {
  switch (action.type) {
    case 'receive':
      switch (action.message.type) {
        default:
          return {...state, messages: [...state.messages, action.message]}}
    case 'send':
      switch (action.message.type) {
        case 'inputChange':
          return {...state, value: action.message.value}
        default: return state
      }
    default: return state
  }
}
function Commands() {
  //const [state, dispatch] = useReducer(wsreducer, {meta: [], value: '', error: null});
  const [state, send] = useStreamReducer("default", streamReducer, {
    value: '',
    commands: [],
    messages: [],
    commandsLoaded: false
  })

  //const handleChange = useCallback(event => {
  //  const value = event.target.value;
  //  wsinfo.send(value);
  //  dispatch({type: 'changeValue', value});
  //}, [wsinfo.send, dispatch]);
  return <>
    <TextInput multiline={true} onChange={event => send({type: "inputChange", value: event.target.value})} value={state.value}/>
    {/*<ParsedTextDemo value={state.parsed}/>*/}
    <StatusLabel>{state.status}</StatusLabel>
    <Monospace>{'State:\n'}{JSON.stringify(state, null, 4)}</Monospace>
  </>
}

const App = () => {
  //const tree = ["view", {
  //  "children": [
  //    ["plaintext", {value: "hello world"}],
  //    ["textinput", {value: "derp", onChange: ["stream", {sid: 1}]}]
  //  ]
  //}];
  //console.log("hello", {}?.derp?.herp());
  //{unpack.tagged(tree, streamdispatch)}
  return <WSProvider url={"ws://localhost:13579/websocket"}>
      <MainView>
      <StatusBar barStyle="dark-content" />
      <SafeAreaView>
        <Commands/>
      </SafeAreaView>
    </MainView>
  </WSProvider>
};

const MainView = styled(View)`
  background-color: ${COLORS.bg};
  height: 100%;
`

export default App;
