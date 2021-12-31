function promiseNoData(promise, data, error) {
  return !promise ? (
    <span>No Data yet.</span>
  ) : promise && !data && !error ? (
    <img alt='spinner' src='http://www.csc.kth.se/~cristi/loading.gif' />
  ) : promise && !data && error ? (
    <span>{error}</span>
  ) : promise && data && !error ? (
    false
  ) : (
    false
  );
}

// function promiseNoData(promise, data, error) {
//   if (!promise && !data && !error) {
//     return <span>no data</span>
//   } else if (promise && !data && !error) {
//     return <img src='http://www.csc.kth.se/~cristi/loading.gif'></img>
//   } else if (promise && !data && error) {
//     return <span>{error}</span>
//   } else if (promise && data && !error) {
//     return false
//   }
// }
