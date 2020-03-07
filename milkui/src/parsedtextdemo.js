import { Text } from 'react-native'

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
