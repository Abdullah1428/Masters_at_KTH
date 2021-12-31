function SidebarPresenter(props) {
  // using the custom react hook
  const guests = useModelProperty(props.model, 'numberOfGuests')
  const dishes = useModelProperty(props.model, 'dishes')

  return (
    <SidebarView
      guests={guests}
      dishes={dishes}
      setGuests={(num) => props.model.setNumberOfGuests(num)}
      removeDish={(dish) => props.model.removeFromMenu(dish)}
      dishChoice={(id) => props.model.setCurrentDish(id)}
    />
  )
}
