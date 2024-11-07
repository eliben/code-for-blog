This sample requires running all Python code in a virtual env where JAX for GPU,
Gemma and Flask+gunicorn were installed.

The installation instructions for JAX for GPU and Gemma were taken from
https://github.com/google-deepmind/gemma/; after creating a new virtualenv:

    pip install "jax[cuda12]"
    pip install git+https://github.com/google-deepmind/gemma.git
    pip install Flask gunicorn

To run the Python server, invoke `./run-gemma-server.sh`, which sources the
virtualenv and sets up what's needed for the Python server. You'll have to
[download the Gemma model weights and tokenizer](https://github.com/google-deepmind/gemma/?tab=readme-ov-file#downloading-the-models) and
place them where the script can find them.

----

Sending `curl` requests to the service (assuming port 20200):

    curl -i --json '{"prompt": "are bees dangerous?"}' http://localhost:20200/prompt

----

To measure the server's average latency, run the Go client in
`measure-request-latency` in a separate terminal. With the server running in
the background, run:

    cd measure-request-latency
    go run . -port 20200 -n 10000
