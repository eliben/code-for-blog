package main

import (
	"fmt"
	"log"
	"net/http"
	"strings"
	"time"
)

var page = `
<html>
 <head>
   <style> 
     #output {
       font-family: "Roboto";
       font-size: 2em;
       text-align: center;
       width: 14em;
       padding: 8px 8px; 
       border: 2px solid #000000;
       border-radius: 10px;
     }
   </style>
 </head>
 <body>
   <h2>System clock (updates every second)</h2>
   <div id="output"></div>
   <script type="text/javascript">
      let outputBox = document.querySelector('#output');

      window.addEventListener('DOMContentLoaded', (event) => {
        outputBox.innerHTML = "initializing...";
        tick();
        setInterval(tick, 1000);
      });

      function tick() {
        fetch('/time')
          .then((response) => {
            if (!response.ok) {
              throw new Error("error response");
            }
            return response.text();
          })
          .then((text) => {
            outputBox.innerHTML = text;
          })
          .catch((error) => {
            outputBox.innerHTML = "network error";
          });
      }
   </script>
 </body>
</html>
`

func rootHandler(w http.ResponseWriter, r *http.Request) {
	// The root handler "/" matches every path that wasn't match by other
	// matchers, so we have to further filter it here. Only accept actual root
	// paths.
	if path := strings.Trim(r.URL.Path, "/"); len(path) > 0 {
		http.NotFound(w, r)
		return
	}

	fmt.Fprint(w, page)
}

func timeHandler(w http.ResponseWriter, r *http.Request) {
	fmt.Fprint(w, time.Now().Format("02 Jan 2006 15:04:05 MST"))
}

func main() {
	http.HandleFunc("/time", timeHandler)
	http.HandleFunc("/", rootHandler)

	port := ":9999"
	log.Fatal(http.ListenAndServe(port, nil))
}
