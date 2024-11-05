from dataclasses import dataclass
import os

from flask import Flask
from flask import request

from gemma import params as params_lib
from gemma import sampler as sampler_lib
from gemma import transformer as transformer_lib
import sentencepiece as spm


# Once initialized, this will hold a sampler_lib.Sampler instance that
# can be used to generate text.
gemma_sampler = None


def initialize_gemma():
    """Initialize Gemma sampler, loading the model into the GPU."""
    model_checkpoint = os.getenv("MODEL_CHECKPOINT")
    model_tokenizer = os.getenv("MODEL_TOKENIZER")

    parameters = params_lib.load_and_format_params(model_checkpoint)
    print("Parameters loaded")
    vocab = spm.SentencePieceProcessor()
    vocab.Load(model_tokenizer)
    transformer_config = transformer_lib.TransformerConfig.from_params(
        parameters,
        cache_size=1024,
    )
    transformer = transformer_lib.Transformer(transformer_config)

    global gemma_sampler
    gemma_sampler = sampler_lib.Sampler(
        transformer=transformer,
        vocab=vocab,
        params=parameters["transformer"],
    )
    print("Sampler ready")


# Flask app setup


def create_app():
    app = Flask(__name__)

    with app.app_context():
        initialize_gemma()
    return app


app = create_app()


@app.route("/echo", methods=["POST"])
def echo():
    prompt = request.json["prompt"]
    return {"echo_prompt": prompt}


@app.route("/prompt", methods=["POST"])
def prompt():
    prompt = request.json["prompt"]

    # For total_generation_steps, 128 is a default taken from the Gemma
    # sample. It's a tradeoff between speed and quality (higher values mean
    # better quality but slower generation).
    sampling_steps = request.json.get("sampling_steps", 128)

    sampled_str = gemma_sampler(
        input_strings=[prompt],
        total_generation_steps=int(sampling_steps),
    ).text
    return {"response": sampled_str}
