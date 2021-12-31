const DishSource = {
  // JS object creation literal
  apiCall(params) {
    return (
      fetch(BASE_URL + params, {
        method: 'GET', // HTTP method
        headers: {
          // HTTP headers
          'X-Mashape-Key': API_KEY,
          'x-rapidapi-host':
            'spoonacular-recipe-food-nutrition-v1.p.rapidapi.com',
        },
      })
        .then((response) => {
          if (response.status === 200) {
            return response;
          } else {
            throw new Error(response.statusText);
          }
        })
        // from HTTP response headers to HTTP response data
        .then((response) => response.json())
    );
  }, // comma between object entries

  async searchDishes(params) {
    //{type:"main course", query:"chicken"

    const apiParams = {};

    if (params) {
      apiParams.type = params.type ? params.type : '';
      apiParams.query = params.query ? params.query : '';
      apiParams.number = params.number ? params.number : 10;
      apiParams.offset = params.offset ? params.offset : 0;
    }

    const data = await DishSource.apiCall(
      'recipes/search?' +
        new URLSearchParams({
          type: apiParams.type,
          query: apiParams.query,
          number: apiParams.number,
          offset: apiParams.offset,
        })
    );

    return data.results;
  },
  async getDishDetails(id) {
    const data = await DishSource.apiCall(`recipes/${id}/information`);

    return data;
  },
};
