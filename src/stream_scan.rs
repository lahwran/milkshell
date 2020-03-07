use futures::task::{Context, Poll};
use std::pin::Pin;
use tokio::stream::Stream;

pub struct Reduce<
    A,
    S: Stream<Item = A> + Unpin,
    T: Unpin + Clone,
    F: Unpin + Fn(T, Option<A>) -> T,
> {
    stream: S,
    state: T,
    reducer_func: F,
}
impl<A, S: Stream<Item = A> + Unpin, T: Unpin + Clone, F: Unpin + Fn(T, Option<A>) -> T> Unpin
    for Reduce<A, S, T, F>
{
}

impl<A, S: Stream<Item = A> + Unpin, T: Unpin + Clone, F: Unpin + Fn(T, Option<A>) -> T> Stream
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
                self_.state = (self_.reducer_func)(self_.state.clone(), Some(value));
                Poll::Ready(Some(self_.state.clone()))
            }
            Poll::Ready(None) => {
                self_.state = (self_.reducer_func)(self_.state.clone(), None);
                Poll::Ready(None)
            }
            Poll::Pending => Poll::Pending,
        }
    }
}

pub(crate) trait ReducerExt<A> {
    fn reducer<T: Unpin + Clone, F: Unpin + Fn(T, Option<A>) -> T>(
        self,
        initial_state: T,
        reducer: F,
    ) -> Reduce<A, Self, T, F>
    where
        Self: Sized + ReducerExt<A> + Stream<Item = A> + Unpin,
    {
        Reduce {
            stream: self,
            state: initial_state,
            reducer_func: reducer,
        }
    }
}
impl<St: ?Sized, A> ReducerExt<A> for St where St: Stream<Item = A> {}
