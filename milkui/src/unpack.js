import { Text, TextInput, View } from 'react-native'

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
