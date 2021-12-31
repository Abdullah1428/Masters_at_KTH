function DetailsView(props) {
  return (
    <div className='detailsView'>
      <div className='title'>
        <h1>{props.dish.title}</h1>
      </div>
      <div className='productInfo'>
        <img alt='dishImg' src={props.dish.image} />
        <div className='pricing'>
          <h4>Dish Pricing</h4>
          <p>
            price:{' '}
            {props.dish.pricePerServing &&
              props.dish.pricePerServing.toFixed(2)}
          </p>
          <p>
            For {props.people} guests:{' '}
            {(props.dish.pricePerServing * props.people).toFixed(2)}
          </p>
        </div>
      </div>
      <h4>Ingredients</h4>
      <div className='ingredients'>
        {props.dish.extendedIngredients.map((ingredient) => (
          <div key={ingredient.id}>
            <span>
              {ingredient.name}: {ingredient.amount}
            </span>
          </div>
        ))}
      </div>
      <div className='recipe'></div>
      <h4>Recipe</h4>
      <p>{props.dish.instructions}</p>
      <button>
        <a target='_blank' href={props.dish.sourceUrl}>
          More information
        </a>
      </button>
      <div className='buttons'>
        <button
          disabled={props.isDishInMenu}
          onClick={() => {
            props.dishAdded();
            window.location.hash = '#search';
          }}>
          Add to menu!
        </button>
        <button onClick={(e) => (window.location.hash = '#search')}>
          Cancel
        </button>
      </div>
    </div>
  );
}
