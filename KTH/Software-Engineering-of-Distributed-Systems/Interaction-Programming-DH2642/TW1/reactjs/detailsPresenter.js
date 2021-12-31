function DetailsPresenter(props) {
  const numberOfGuests = useModelProperty(props.model, 'numberOfGuests')
  const currentDishDetails = useModelProperty(props.model, 'currentDishDetails')
  const currentDishError = useModelProperty(props.model, 'currentDishError')

  return (
    <React.Fragment>
      {promiseNoData(
        props.model.currentDish,
        currentDishDetails,
        currentDishError
      ) || (
        <DetailsView
          dish={currentDishDetails}
          people={numberOfGuests}
          isDishInMenu={
            props.model.dishes &&
            props.model.dishes.find((d) => d.id === props.model.currentDish)
          }
          dishAdded={() => props.model.addToMenu(currentDishDetails)}
        />
      )}
    </React.Fragment>
  )
}
