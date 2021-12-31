function useModelProperty(model, propertyName) {
  const [property, setProperty] = React.useState(model[propertyName])
  React.useEffect(
    function () {
      function callback() {
        setProperty(model[propertyName])
      }
      model.addObserver(callback)
      return function () {
        model.removeObserver(callback)
      }
    },
    [model]
  )
  return property
}
