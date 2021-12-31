function SummaryPresenter(props) {
  const numberOfGuests = useModelProperty(props.model, 'numberOfGuests')
  const dishes = useModelProperty(props.model, 'dishes')

  return (
    <SummaryView
      persons={numberOfGuests}
      ingredients={getIngredients(dishes)}
    />
  )
}
