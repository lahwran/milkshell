use futures::task::{Context, Poll};
use serde::de::DeserializeOwned;
use std::pin::Pin;
use tokio::stream::Stream;

pub struct Reduce<A, S: Stream<Item = A> + Unpin, T: Unpin + Clone, F: Unpin + Fn(T, A) -> T> {
    stream: S,
    state: T,
    reducer_func: F,
}
impl<A, S: Stream<Item = A> + Unpin, T: Unpin + Clone, F: Unpin + Fn(T, A) -> T> Unpin
    for Reduce<A, S, T, F>
{
}

impl<A, S: Stream<Item = A> + Unpin, T: Unpin + Clone, F: Unpin + Fn(T, A) -> T> Stream
    for Reduce<A, S, T, F>
{
    type Item = T;

    fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>>
    where
        Self: Unpin,
    {
        let self_ = self.get_mut();
        let a = &mut self_.stream;
        let p: Poll<Option<A>> = Pin::new(a).poll_next(cx);
        match p {
            Poll::Ready(Some(value)) => {
                self_.state = (self_.reducer_func)(self_.state.clone(), value);
                Poll::Ready(Some(self_.state.clone()))
            }
            Poll::Ready(None) => Poll::Ready(None),
            Poll::Pending => Poll::Pending,
        }
    }
}

//pub(crate) fn reducer<S, T, A, F>(
//    stream: S,
//    reducer_func: F,
//    initial_state: T,
//) -> Reduce<serde_json::Value, S, T, F>
//where
//    S: Stream<Item = A> + Unpin,
//    T: Unpin + Clone,
//    F: Unpin + Fn(T, A) -> T,
//{
//    Reduce {
//        stream,
//        state: initial_state,
//        reducer_func,
//    }
//}

pub(crate) fn reducer_json<S, T, A, F>(
    stream: S,
    reducer: F,
    initial_state: T,
) -> Reduce<serde_json::Value, S, T, impl Fn(T, serde_json::Value) -> T>
where
    F: Unpin + Fn(T, A) -> T,
    S: Stream<Item = serde_json::Value> + Unpin,
    T: Unpin + Clone,
    A: DeserializeOwned,
{
    Reduce {
        stream,
        state: initial_state,
        reducer_func: (move |state, action| {
            reducer(state, serde_json::from_value::<A>(action).expect("FIXME: this is totally possible actually, and needs to be passed through, but for now"))
        }),
    }
}
