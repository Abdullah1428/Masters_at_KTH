function usePromise(promise) {
  const [searchResults, setSearchResults] = React.useState(null);
  const [error, setError] = React.useState(null);
  React.useEffect(
    function () {
      setSearchResults(null);
      setError(null);
      let cancelled = false;
      if (promise)
        promise
          .then((dt) => {
            if (!cancelled) {
              setSearchResults(dt);
            }
          })
          .catch((err) => {
            if (!cancelled) {
              setError(err);
            }
          });
      return function () {
        cancelled = true;
      };
    },
    [promise]
  );
  return [searchResults, error];
}
