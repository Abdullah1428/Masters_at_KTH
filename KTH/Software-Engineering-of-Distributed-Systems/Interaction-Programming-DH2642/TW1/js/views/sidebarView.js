function SidebarView(props) {
  const types = ['starter', 'main course', 'dessert'];
  function dishType(dish) {
    if (dish.dishTypes) {
      const tp = dish.dishTypes.filter((value) => types.includes(value));
      if (tp.length) return tp[0];
    }
    return '';
  }

  function calculateTotalPrice(dishes, guests) {
    let dishesTotal = 0;

    dishes.forEach((dish) => {
      dishesTotal += dish.pricePerServing;
      dishesTotal.toFixed(2);
    });

    return (dishesTotal * guests).toFixed(2);
  }

  return (
    <div>
      <button
        onClick={() => props.setGuests(props.guests - 1)}
        disabled={props.guests < 2}
      >
        -
      </button>
      <span> {props.guests} </span>
      <button onClick={() => props.setGuests(props.guests + 1)}>+</button>
      <table>
        <tbody>
          {[...props.dishes]
            .sort(function compareDishes(a, b) {
              let ai = types.indexOf(dishType(a));
              let bi = types.indexOf(dishType(b));

              return ai < bi ? -1 : ai > bi ? 1 : 0;
            })
            .map((dish) => (
              <tr key={dish.id}>
                <td>
                  <button onClick={() => props.removeDish(dish)}>X</button>
                </td>
                <td>
                  <a
                    href=''
                    onClick={(e) => {
                      e.preventDefault();
                      props.dishChoice(dish.id);
                      window.location.hash = '#details';
                    }}
                  >
                    {dish.title}
                  </a>
                </td>
                <td>{dish.dishType}</td>
                <td>{dish.pricePerServing.toFixed(2)}</td>
              </tr>
            ))}
          <tr>
            <td></td>
            <td>Total:</td>
            <td></td>
            <td>{calculateTotalPrice(props.dishes, props.guests)}</td>
          </tr>
        </tbody>
      </table>
    </div>
  );
}
