/*jshint esversion: 6*/

class Parser {
  constructor() {
    this.data = {};
  }

  /* load a json from a path */
  readJson(path, contactMap) {
    var parser = this;
    let json = path || './data/simple.json';
    return new Promise(function(resolve, reject) {
      var httpRequest = new XMLHttpRequest();
      httpRequest.open('GET', json);
      httpRequest.onload = function() {
        if (httpRequest.status == 200) {
        // Resolve the promise with the response text
          let data = JSON.parse(httpRequest.responseText);
          resolve(parser.populateData(data));

        }
        else {
        // Otherwise reject with the status text
        // which will hopefully be a meaningful error
          reject(Error(httpRequest.statusText));
        }

      };
      // Handle network errors
      httpRequest.onerror = function() {
        reject(Error("Network Error"));
      };
      httpRequest.send(); 
    });
  }

  populateData(data) {
    let dataStorage = new DataStorage(data, 0);
    //console.log(dataStorage);
    return dataStorage;
  }

  removeData() {
    window.data.pop();
  }
}