function SummaryView(props) {
  return (
    // a lonely return on a line returns undefined. Parentheses needed
    <div className='summaryView'>
      <button onClick={(e) => (window.location.hash = '#search')}>
        Back to Search
      </button>
      <h4>
        Summary for <span title='nr. guests'>{props.persons}</span> guests
      </h4>
      <table className='table'>
        <thead>
          <tr>
            <th>Ingredient</th>
            <th>Aisle</th>
            <th>Quantity</th>
          </tr>
        </thead>
        <tbody>
          {[...props.ingredients]
            .sort(function compareIngredients(a, b) {
              if (a.aisle < b.aisle) return -1;
              // TODO return 1 if a.aisle > b.aisle. Note: not >= !!!
              if (a.aisle > b.aisle) return 1;
              // At this point, we know that a.aisle===b.aisle
              if (a.name < b.name) return -1;
              // TODO compare a.name with b.name, return 1 or -1 based on that
              if (a.name > b.name) return 1;
              /* if a.name===b.name throw an error because 
              ingredient names are not unique as specified, so 
              there’s a bug */
              if (a.name === b.name) {
                throw new Error('Ingredients names are not unique!!!');
              }
            })
            .map((ingredient) => (
              <tr key={ingredient.id}>
                <td>{ingredient.name}</td>
                <td>{ingredient.aisle}</td>
                <td>
                  {(
                    ingredient.measures.metric.amount.toFixed(2) * props.persons
                  ).toFixed(2)}{' '}
                  {ingredient.measures.metric.unitShort}
                </td>
              </tr>
            ))}
        </tbody>
      </table>
    </div>
  );
}
