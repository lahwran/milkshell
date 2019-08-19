
import {useCallback, useRef, useEffect, useMemo, useState} from 'react';
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
  const [last, setLastMessage] = useState(undefined);
  const innerOptions = useMemo(() => ({
    // {isTrusted: bool, etc, data: [message]}
    onMessage: event => {
      const parsed = JSON.parse(event.data);
      setLastMessage(parsed);
      (optrf?.current?.onMessage || noop)(parsed)
    },
    onClose: event => {
      (optrf?.current?.onClose || noop)(event)

    },
    onError: event => {
      (optrf?.current?.onError || noop)(event);
    },

    // {isTrusted: bool, target: ws, timeStamp: ms after open (I think), type: "open"}
    onOpen: event => {
      (optrf?.current?.onOpen || noop)(event);
    },
  }), []);

  const [sendRaw, _, statusInt, reconnect] = useWebSocket(url, innerOptions);
  const status = STATUS_NAMES[statusInt];
  const send = useCallback(message => sendRaw(JSON.stringify(message)), [sendRaw]);
  return {send, last, status, statusInt, reconnect};
}

function useDebounceFn(wait, func, guards=[]) {
  var timeout = useRef(null);
  var funcref = useRef(null);
  funcref.current = func;

  return useCallback((...args) => {
    console.log("args", args);
    clearTimeout(timeout);

    timeout.current = setTimeout(() => {
      timeout.current = null;
      console.log("args (2)", args);
      funcref.current(...(args||[]));
    }, wait);
  }, [funcref, timeout, wait, ...guards]);
};

export function useWebSocketReconnect(url, {backoffMult=1.5, backoffStep=100, backoffMax=2500, timeout=30000, ...callbacks}) {
  const wsinfo = useWebSocketCustom(url, {
    ...callbacks,
    onError: event => {
      (callbacks?.onError || noop)(event);
      reconnectwrapper()
    },
    onOpen: ({timeStamp}) => {
      (callbacks?.onOpen || noop)(timeStamp);
      setBackoff(0);
    },
    onClose: event => {
      (callbacks?.onClose || noop)(event);
      reconnectwrapper()
    }
  });
  // so we have an "atomic" boolean (with respect to the event loop anyway)
  const inhibit = useRef(false);
  const [backoff, setBackoff] = useState(0);
  const reconnectwrapper = useCallback(() => {
    if (inhibit.current) {
      console.log("debounced, not reconnecting");
      return;
    }
    const newBackoffMs = Math.min(backoffMax, backoff * backoffMult + backoffStep);
    setBackoff(newBackoffMs);
    console.log("new backoff", newBackoffMs);
    inhibit.current = true;
    setTimeout(() => {
      inhibit.current = false;
      console.log("reconnect");
      if (!wsinfo.reconnect) {
        console.log("api missing, waiting again")
        setTimeout(reconnectwrapper, 100);
        return;
      }
      wsinfo.reconnect();
    }, newBackoffMs);
  }, [wsinfo.reconnect, backoff]);
  const debouncedReconnect = useDebounceFn(1000, () => reconnectwrapper(), [reconnectwrapper]);
  function send(message) {
    const res = wsinfo.send(message);
    debouncedReconnect();
    return res;
  }
  return {send, ...wsinfo};
}
