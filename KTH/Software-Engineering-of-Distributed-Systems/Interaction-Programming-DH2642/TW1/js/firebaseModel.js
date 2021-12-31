var timerId = undefined;
var timerId2 = undefined;

const throttleFunction1 = function (func, delay) {
  if (timerId) {
    return;
  }

  timerId = setTimeout(function () {
    func();

    timerId = undefined;
  }, delay);
};

const throttleFunction2 = function (func, delay) {
  if (timerId2) {
    return;
  }

  timerId2 = setTimeout(function () {
    func();

    timerId2 = undefined;
  }, delay);
};

function persistModel(model) {
  let loadingFromFirebase = false; // boolean flag, used in a JS closure

  const updateNow = () => {
    model.addObserver(function () {
      if (loadingFromFirebase) {
        return;
      }
      firebase.database().ref('dinnerModel').set({
        guests: model.numberOfGuests,
        dishes: model.dishes,
        currentDish: model.currentDish,
      });
    });
  };

  throttleFunction1(updateNow, 1000);

  const setNow = () => {
    firebase
      .database()
      .ref('dinnerModel')
      .on('value', function (data) {
        loadingFromFirebase = true;
        try {
          if (data.val()) {
            model.setNumberOfGuests(data.val().guests);
            model.setDishes(data.val().dishes || []);
            model.setCurrentDish(data.val().currentDish || null);
          }
        } catch (error) {
          console.log(error);
        } finally {
          loadingFromFirebase = false; // see notes for safer code
        }
      });
  };

  throttleFunction2(setNow, 1000);
}
