function SearchFormView(props) {
  // function eventPrinter(evt) {
  //   console.log(evt.target)
  // }

  return (
    <>
      <div>
        <input
          onChange={(event) => props.onText(event.target.value)}
          placeholder='search ...'
        />
        <span> </span>
        <select onChange={(event) => props.onDishType(event.target.value)}>
          <option>Choose: </option>
          {props.options.map(function (opt) {
            return <option key={opt}>{opt}</option>;
          })}
        </select>
        <span> </span>
        <button onClick={() => props.onSearch()}>Search!</button>
        <button onClick={() => (window.location.hash = '#summary')}>
          Summary
        </button>
      </div>
    </>
  );
}

function SearchResultsView(props) {
  return (
    <div>
      {props.searchResults &&
        [...props.searchResults].map((res) => (
          <div
            onClick={() => {
              props.dishChosen(res.id);
              window.location.hash = '#details';
            }}
            key={res.id}
            className='searchResult'>
            <img
              alt='recipe image'
              src={`https://spoonacular.com/recipeImages/${res.image}`}
              height={100}
            />
            <div>{res.title}</div>
          </div>
        ))}
    </div>
  );
}
