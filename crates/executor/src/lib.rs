use std::future::Future;

use bevy_tasks::TaskPool;
use lazy_static::lazy_static;

lazy_static! {
    static ref TASK_POOL: TaskPool = TaskPool::new();
}

pub fn spawn<T: Send + 'static>(future: impl Future<Output = T> + Send + 'static) -> Task<T> {
    TASK_POOL.spawn(future)
}

pub fn scope<'env, F, T>(f: F) -> Vec<T>
where
    F: for<'scope> FnOnce(&'scope Scope<'scope, 'env, T>),
    T: Send + 'static,
{
    TASK_POOL.scope(f)
}

pub type Scope<'scope, 'env, T> = bevy_tasks::Scope<'scope, 'env, T>;
pub type Task<T> = bevy_tasks::Task<T>;
