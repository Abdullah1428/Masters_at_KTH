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

  const fetchMoreData = async () => {
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
  };

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
