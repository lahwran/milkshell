
import {useCallback, useRef, useEffect, useMemo} from 'react';
import useWebSocket from 'react-use-websocket';

class SanityError extends Error {}

const CONNECTION_STATUS_CONNECTING = 0;
const CONNECTION_STATUS_OPEN = 1;
const CONNECTION_STATUS_CLOSING = 2;
const CONNECTION_STATUS_CLOSED = 3;
export const STATUS_NAMES = [
  'connecting',
  'open',
  'closing',
  'closed',
];
const noop = () => {}
export default function useWebSocketCustom(url, callbacks) {
  const optrf = useRef(null);
  optrf.current = callbacks;
  const innerOptions = useMemo(() => ({
    // {isTrusted: bool, etc, data: [message]}
    onMessage: event => (optrf?.current?.onMessage || noop)(event),
    onClose: event => (optrf?.current?.onClose || noop)(event),
    onError: event => (optrf?.current?.onError || noop)(event),

    // {isTrusted: bool, target: ws, timeStamp: ms after open (I think), type: "open"}
    onOpen: event => (optrf?.current?.onOpen || noop)(event),
  }), []);

  const [sendRaw, last, statusInt] = useWebSocket(url, innerOptions);
  const status = STATUS_NAMES[statusInt];
  const send = useCallback(message => sendRaw(JSON.stringify(message)), [sendRaw]);
  return {send, last, status, statusInt};
}
