function SearchPresenter(props) {
  const [promise, setPromise] = React.useState(null);
  const [searchResults, setSearchResults] = React.useState([]);
  const [searchQuery, setSearchQuery] = React.useState('');
  const [searchType, setSearchType] = React.useState('');
  const [error, setError] = React.useState(null);

  let [offset, setoffset] = React.useState(0);

  React.useEffect(function () {
    setPromise(DishSource.searchDishes({}));
  }, []);

  // const [searchResults, error] = usePromise(promise);

  React.useEffect(
    function () {
      setSearchResults([]);
      setError(null);
      let cancelled = false;
      if (promise)
        promise
          .then(function (dt) {
            if (!cancelled) {
              setSearchResults(dt);
            }
          })
          .catch(function (er) {
            if (!cancelled) setError(er);
          });
      return function () {
        cancelled = true;
      };
    },
    [promise]
  );

  // DinnerPlanner Infinite search result scroll task works
  // --------------------------------------------------------------------------------
  const listenerRef = React.useRef();

  const onScroll = async () => {
    if (listenerRef.current) {
      const { scrollTop, scrollHeight, clientHeight } = listenerRef.current;
      if (scrollTop + clientHeight === scrollHeight) {
        // call here api again to get more data
        setoffset((offset += 10));
        fetchMoreData();
      }
    }
  };

  async function fetchMoreData() {
    try {
      const data = await DishSource.searchDishes({
        type: searchType,
        query: searchQuery,
        offset: offset,
      });

      setSearchResults([...searchResults, ...data]);
    } catch (error) {
      console.log('Error :', error);
    }
  }
  // --------------------------------------------------------------------------------

  // DinnerPlanner Search-as-you-type with debouncing works
  // --------------------------------------------------------------------------------
  React.useEffect(
    function () {
      deBounceSearch(searchQuery);
    },
    [searchQuery]
  );

  const debounceFunction = (func, delay) => {
    let timer;
    return function () {
      let self = this;
      let args = arguments;
      clearTimeout(timer);
      timer = setTimeout(() => {
        func.apply(self, args);
      }, delay);
    };
  };

  const callSearchAPI = (query) => {
    console.log('called after one second');
    setPromise(DishSource.searchDishes({ type: searchType, query: query }));
  };

  // React.useCallback will return a memoized version of the callback
  // that only changes if one of the dependencies has changed since we are
  // passing empty [] this means our doBounceSearch remains the same and is not
  // invoked on each text change in the search query.
  const deBounceSearch = React.useCallback(
    debounceFunction((query) => callSearchAPI(query), 1000),
    []
  );

  return (
    <React.Fragment>
      <SearchFormView
        options={['starter', 'main course', 'dessert']}
        onSearch={() =>
          setPromise(
            DishSource.searchDishes({ type: searchType, query: searchQuery })
          )
        }
        onText={(txt) => setSearchQuery(txt)}
        onDishType={(dishType) => setSearchType(dishType)}
      />

      {promiseNoData(promise, searchResults, error) || (
        <div
          onScroll={onScroll}
          ref={listenerRef}
          style={{ height: '100%', overflowY: 'auto' }}>
          <SearchResultsView
            searchResults={searchResults}
            dishChosen={(id) => props.model.setCurrentDish(id)}
          />
        </div>
      )}
    </React.Fragment>
  );
}
