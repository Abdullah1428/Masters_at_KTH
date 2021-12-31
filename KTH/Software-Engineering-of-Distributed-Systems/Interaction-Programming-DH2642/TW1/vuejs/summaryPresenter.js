function SummaryPresenter(props) {
  return (
    <SummaryView
      persons={props.model.numberOfGuests}
      ingredients={getIngredients(props.model.dishes)}
    />
  )
}
