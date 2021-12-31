function SidebarPresenter(props) {
  return (
    <SidebarView
      guests={props.model.numberOfGuests}
      setGuests={(num) => props.model.setNumberOfGuests(num)}
      dishes={props.model.dishes}
      removeDish={(dish) => props.model.removeFromMenu(dish)}
      dishChoice={(id) => props.model.setCurrentDish(id)}
    />
  )
}
