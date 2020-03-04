
import React, {useReducer, useCallback, useRef, useContext, useEffect, useMemo, useState} from 'react';
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
    onMessage(event) {
      const parsed = JSON.parse(event.data);
      setLastMessage(parsed);
      (optrf?.current?.onMessage || noop)(parsed)
    },
    onClose(event) {
      (optrf?.current?.onClose || noop)(event)

    },
    onError(event) {
      (optrf?.current?.onError || noop)(event);
    },

    // {isTrusted: bool, target: ws, timeStamp: ms after open (I think), type: "open"}
    onOpen(event) {
      (optrf?.current?.onOpen || noop)(event);
    },
  }), []);

  const [sendRaw, _, statusInt, reconnect] = useWebSocket(url, innerOptions);
  const status = STATUS_NAMES[statusInt];
  const send = useCallback(message => sendRaw(JSON.stringify(message)), [sendRaw]);
  return {send, last, status, reconnect};
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

export function useWebSocketReconnect(url, {backoffMult=1.5, backoffStep=100, backoffMax=2500, timeout=3000, ...callbacks}) {
  const wsinfo = useWebSocketCustom(url, {
    ...callbacks,
    onError(event) {
      (callbacks?.onError || noop)(event);
      reconnectwrapper()
    },
    onOpen({timeStamp}) {
      (callbacks?.onOpen || noop)(timeStamp);
      setBackoff(0);
    },
    onClose(event) {
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
  const send = useCallback((message) => {
    const res = wsinfo.send(message);
    debouncedReconnect();
    return res;
  }, [wsinfo.send, debouncedReconnect]);
  return {send, ...wsinfo};
}

const WebsocketMultiplexContext = React.createContext(null)

export function useStream(name, callbacks) {
  const api = useContext(WebsocketMultiplexContext);
  useEffect(() => {
    return api.subscribe(name, callbacks)
  }, [api.subscribe, name, callbacks])
  return useMemo(() => {
    return {
      send(message) {
        api.send(name, message)
      },
      status: api.status
    }
  }, [name, api.send, api.status])
}

export function useStreamReducer(name, reducer, initialState) {
  const [state, dispatch] = useReducer(reducer, initialState);
  const handlers = useMemo(() => ({
    onMessage(message) {
      dispatch({type: "receive", message})
    },
    onClose(event) {
      dispatch({type: "close", event})
    },
    onOpen(ts) {
      dispatch({type: "open", ts})
    },
    onError(event) {
      dispatch({type: "error", event})
    }
  }), [dispatch])
  const {send, status} = useStream(name, handlers);
  return useMemo(() => (
    [{...state, streamStatus: status}, ((message, extra) => {
      dispatch({type: 'send', message, extra})
      send(message)
    })]
  ), [state, status, send])
}

export function WSProvider({url, children}) {
  var subscribers = useRef();
  if (subscribers.current === undefined) {
    subscribers.current = new Map();
  }

  const {send, status} = useWebSocketReconnect(url,
    {
      onOpen(ts) {
        for (const v of subscribers.current.values()) {
          v.onOpen(ts);
        }
      },
      onClose(event) {
        for (const v of subscribers.current.values()) {
          v.onClose(event);
        }
      },
      onError(event) {
        // idk what to do here t b h. maybe only the default stream should be getting notified?
        for (const v of subscribers.current.values()) {
          v.onError(event);
        }
      },
      onMessage(message) {
        const dest = message.stream_id;
        if (!subscribers.current.has(dest)) {
          // TODO: queue the message
          console.error(`message for unknown stream ${dest}:`, message) // TODO: warn someone a bit louder
          return
        }
        const subscriber = subscribers.current.get(dest)
        subscriber.onMessage(message);
      }

    }
  );

  const api = useMemo(() => ({
    send(stream_id, message) {
      send({message, stream_id})
    },
    subscribe(key, handlerRef) {
      // must be called exactly once from a useEvent(), and its return value returned
      if (subscribers.current.get(key) !== undefined) {
        throw new Exception(`key already subscribed: ${key}`)
      }
      subscribers.current.set(key, handlerRef);
      return () => {
        if (subscribers.current.get(key) !== handlerRef) {
          throw new Exception(`key subscription replaced before subscription ended: ${key}`)
        }
        subscribers.current.delete(key)
      }
    },
    url,
    status,
  }), [subscribers, url, status])

  return <WebsocketMultiplexContext.Provider value={api}>
    {children}
  </WebsocketMultiplexContext.Provider>
}
