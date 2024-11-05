from dataclasses import dataclass
import os

from flask import Flask
from flask import request

from gemma import params as params_lib
from gemma import sampler as sampler_lib
from gemma import transformer as transformer_lib
import sentencepiece as spm


@dataclass
class GemmaConfig:
    path_checkpoint: str
    path_tokenizer: str
    total_sampling_steps: int


gemma_config = None
gemma_sampler = None


def initialize_gemma():
    global gemma_config
    gemma_config = GemmaConfig(
        path_checkpoint=os.getenv("MODEL_CHECKPOINT"),
        path_tokenizer=os.getenv("MODEL_TOKENIZER"),
        total_sampling_steps=128,
    )

    parameters = params_lib.load_and_format_params(gemma_config.path_checkpoint)
    print("Parameters loaded.")
    vocab = spm.SentencePieceProcessor()
    vocab.Load(gemma_config.path_tokenizer)
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


def create_app():
    app = Flask(__name__)

    with app.app_context():
        initialize_gemma()
    return app


app = create_app()


@app.route("/echo", methods=["POST"])
def echo():
    request_data = request.json
    prompt = request_data["prompt"]
    return {"echo_prompt": prompt}


@app.route("/prompt", methods=["POST"])
def prompt():
    return {
        "username": "bonki",
        "theme": "moondark",
        "image": "someone.png",
    }
