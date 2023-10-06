To run tests using Node.js:

  node test/test.js

To see it in action in the browser, the local directory needs to be served in
a real HTTP server (using file:/// will cause CORS errors when loading the
local JS modules). For example, using https://github.com/eliben/static-server:

  static-server .

.. and then open http://127.0.0.1:8080/plot.html in the browser

