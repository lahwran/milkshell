/**
 * Sample React Native App
 * https://github.com/facebook/react-native
 *
 * @format
 * @flow
 */

import React, {Fragment, useState, useCallback, useReducer} from 'react';
import {
  SafeAreaView,
  StyleSheet,
  ScrollView,
  View,
  Text,
  TextInput,
  StatusBar,
  Button,
} from 'react-native';

import {
  Header,
  LearnMoreLinks,
  Colors,
  DebugInstructions,
  ReloadInstructions,
} from './NewAppScreen/index';

import useWebSocketCustom from './ws'

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
  plaintext__type: handler(({value}) => <Text style={styles.sectionDescription}>{value}</Text>, [
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


/* original code: (
    <Fragment>
      <StatusBar barStyle="dark-content" />
      <SafeAreaView>
        <ScrollView
          contentInsetAdjustmentBehavior="automatic"
          style={styles.scrollView}>
          <Header />
          {global.HermesInternal == null ? null : (
            <View style={styles.engine}>
              <Text style={styles.footer}>Engine: Hermes</Text>
            </View>
          )}
          <View style={styles.body}>
            <View style={styles.sectionContainer}>
              <Text style={styles.sectionTitle}>Step One</Text>
              <Text style={styles.sectionDescription}>
                Edit <Text style={styles.highlight}>App.js</Text> to change this
                screen and then come back to see your edits.
              </Text>
            </View>
            <View style={styles.sectionContainer}>
              <Text style={styles.sectionTitle}>See Your Changes</Text>
              <Text style={styles.sectionDescription}>
                <ReloadInstructions />
              </Text>
            </View>
            <View style={styles.sectionContainer}>
              <Text style={styles.sectionTitle}>Debug</Text>
              <Text style={styles.sectionDescription}>
                <DebugInstructions />
              </Text>
            </View>
            <View style={styles.sectionContainer}>
              <Text style={styles.sectionTitle}>Learn More</Text>
              <Text style={styles.sectionDescription}>
                Read the docs to discover what to do next:
              </Text>
            </View>
            <LearnMoreLinks />
          </View>
        </ScrollView>
      </SafeAreaView>
    </Fragment>
)*/

function wsreducer(state, action) {
  console.log(action)
  switch (action.type) {
    case 'changeValue': return {...state, value: action.value};
    case 'open': return {...state, meta: [...state.meta, ["open", action.timeStamp]]};
    case 'message':
      switch (action.data.type) {
        case 'parsed':
          return {...state, parsed: action.data.value};
        default:
          return {...state, messages: [...state.messages, action.data]};

      }
    case 'close':
      return {
        ...state,
        meta: [...state.meta, ["close", action.value]],

      };
    case 'error': return {...state, meta: [...state.meta, ["error", action.value]]};
  }
}
function WsTest() {
  const [state, dispatch] = useReducer(wsreducer, {meta: [], messages: [], value: ''});

  const wsinfo = useWebSocketCustom("ws://localhost:8080/websocket", {
    onMessage: ({data}) => dispatch({type: 'message', data: JSON.parse(data)}),
    onError: event => dispatch({type: 'error', event}),
    onOpen: ({timeStamp}) => dispatch({type: 'open', timeStamp}),
    onClose: event => {
      dispatch({type: 'close', event})
    }
  });
  const handleChange = useCallback(event => {
    const value = event.target.value;
    console.log(value);
    wsinfo.send(value);
    dispatch({type: 'changeValue', value});
  });
  return <>
      <TextInput onChange={handleChange} value={state.value}/>
      <Text>{JSON.stringify(state)}</Text>
      <Button onPress={() => wsinfo.send("[1,2,3]")} title="derp"/>
  </>
}

const streamdispatch = (arg1, arg2) => console.log(arg1, arg2)
const App = () => {
  const tree = ["view", {
    "children": [
      ["plaintext", {value: "hello world"}],
      ["textinput", {value: "derp", onChange: ["stream", {sid: 1}]}]
    ]
  }];
  //console.log("hello", {}?.derp?.herp());
  //{unpack.tagged(tree, streamdispatch)}
  return <Fragment>
    <StatusBar barStyle="dark-content" />
    <SafeAreaView>
      <WsTest/>
    </SafeAreaView>
  </Fragment>
};

const styles = StyleSheet.create({
  scrollView: {
    backgroundColor: Colors.lighter,
  },
  engine: {
    position: 'absolute',
    right: 0,
  },
  body: {
    backgroundColor: Colors.white,
  },
  sectionContainer: {
    marginTop: 32,
    paddingHorizontal: 24,
  },
  sectionTitle: {
    fontSize: 24,
    fontWeight: '600',
    color: Colors.black,
  },
  sectionDescription: {
    marginTop: 8,
    fontSize: 18,
    fontWeight: '400',
    color: Colors.dark,
  },
  highlight: {
    fontWeight: '700',
  },
  footer: {
    color: Colors.dark,
    fontSize: 12,
    fontWeight: '600',
    padding: 4,
    paddingRight: 12,
    textAlign: 'right',
  },
});

export default App;
