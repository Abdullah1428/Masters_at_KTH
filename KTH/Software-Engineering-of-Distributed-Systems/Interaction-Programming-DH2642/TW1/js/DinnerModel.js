class DinnerModel {
  constructor(guests = 2, dishes = [], currentDish = null) {
    this.setNumberOfGuests(guests)
    this.dishes = dishes
    this.currentDish = currentDish
    this.currentDishDetails = null
    this.currentDishError = null
    this.observers = []
  }

  setNumberOfGuests(x) {
    if (x > 0 && this.isInt(x)) {
      if (x !== this.numberOfGuests) {
        this.numberOfGuests = x

        this.notifyObservers()
      }
    } else {
      throw new Error('Number must be a positive intger value')
    }
  }

  addToMenu(dish) {
    const found = this.dishes.some((d) => d.id === dish.id)
    if (!found) {
      this.dishes = [...this.dishes, dish]
      this.notifyObservers()
    }
  }

  removeFromMenu(dish) {
    const found = this.dishes.some((d) => d.id === dish.id)
    if (found) {
      this.dishes = this.dishes.filter((d) => d.id !== dish.id)
      this.notifyObservers()
    }
  }

  setCurrentDish(id) {
    if (this.currentDish === id) {
      return
    } else {
      this.currentDish = id

      this.currentDishDetails = null
      this.currentDishError = null

      this.notifyObservers()

      if (this.currentDish) {
        DishSource.getDishDetails(this.currentDish)
          .then((data) => {
            if (this.currentDish === id) {
              this.currentDishDetails = data
              this.notifyObservers()
            }
          })
          .catch((error) => {
            if (this.currentDish === id) {
              this.currentDishError = error
              this.notifyObservers()
            }
          })
      }
    }
  }

  // to check number is an int value
  isInt(n) {
    return Number(n) === n && n % 1 === 0
  }

  // observer methods
  addObserver(callback) {
    // add callback to observers -> immutable way to expand arr
    this.observers = [...this.observers, callback]
  }

  removeObserver(callback) {
    this.observers = this.observers.filter((cb) => cb != callback)
  }

  notifyObservers() {
    this.observers &&
      this.observers.forEach((cb) => {
        //setTimeout(() => {
        try {
          cb()
        } catch (error) {
          console.log('Error in calling callback: ', error)
        }
        // }, 0)
      })
  }

  // firebase load up
  setDishes(dishes) {
    this.dishes = [...dishes]
    this.notifyObservers()
  }
}

function getIngredients(dishArr) {
  const result = {} // object used as mapping
  dishArr.forEach((d) =>
    d.extendedIngredients.forEach((i) => {
      if (!result[i.id]) {
        // ingredient not taken into account yet
        // associate the ingredient with the ID
        // {...i } is a *copy* of the ingredient (spread syntax)
        // we copy just in case, as we’ll change the object below
        result[i.id] = { ...i }
      } else {
        /*TODO: add i.amount to the amount of result[i.id]*/
        result[i.id].measures.metric.amount += i.measures.metric.amount
      }
    })
  )
  return Object.values(result)
}
