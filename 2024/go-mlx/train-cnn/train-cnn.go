package main

import (
	"flag"
	"fmt"
	"time"

	"example.com/cnnmodel"
	"github.com/gomlx/gomlx/backends"
	"github.com/gomlx/gomlx/examples/cifar"
	mlxcontext "github.com/gomlx/gomlx/ml/context"
	"github.com/gomlx/gomlx/ml/context/checkpoints"
	"github.com/gomlx/gomlx/ml/layers"
	"github.com/gomlx/gomlx/ml/layers/activations"
	"github.com/gomlx/gomlx/ml/layers/regularizers"
	"github.com/gomlx/gomlx/ml/train"
	"github.com/gomlx/gomlx/ml/train/losses"
	"github.com/gomlx/gomlx/ml/train/metrics"
	"github.com/gomlx/gomlx/ml/train/optimizers"
	"github.com/gomlx/gomlx/types/tensors"
	"github.com/gomlx/gomlx/ui/commandline"
	"github.com/janpfeifer/must"

	_ "github.com/gomlx/gomlx/backends/xla"
)

var (
	flagDataDir    = flag.String("data", "", "directory to hold downloaded CIFAR data in")
	flagCheckpoint = flag.String("checkpoint", "", "directory for training checkpoints")
	flagNumSteps   = flag.Int("nsteps", 10000, "number of training steps to run")
)

func trainModel(mlxctx *mlxcontext.Context, dataDir, checkpointPath string) {
	backend := backends.New()
	fmt.Printf("Backend %q:\t%s\n", backend.Name(), backend.Description())

	// Load training and test datasets.
	batchSize := mlxcontext.GetParamOr(mlxctx, "batch_size", int(64))
	evalBatchSize := mlxcontext.GetParamOr(mlxctx, "eval_batch_size", int(128))

	baseTrain := cifar.NewDataset(backend, "Training", dataDir, cifar.C10, cifar.DType, cifar.Train)
	trainDS := baseTrain.Copy().BatchSize(batchSize, true).Shuffle().Infinite(true)

	baseTest := cifar.NewDataset(backend, "Test", dataDir, cifar.C10, cifar.DType, cifar.Test)
	testDS := baseTest.BatchSize(evalBatchSize, false)

	paramsExcludedFromSaving := []string{"data_dir", "train_steps"}

	// Checkpoints saving.
	var checkpoint *checkpoints.Handler
	if checkpointPath != "" {
		checkpoint = must.M1(checkpoints.Build(mlxctx).
			Dir(checkpointPath).
			ExcludeParams(paramsExcludedFromSaving...).
			Done())
		fmt.Printf("Checkpointing model to %q\n", checkpoint.Dir())
	}
	fmt.Println(commandline.SprintContextSettings(mlxctx))

	meanAccuracyMetric := metrics.NewSparseCategoricalAccuracy("Mean Accuracy", "#acc")
	movingAccuracyMetric := metrics.NewMovingAverageSparseCategoricalAccuracy("Moving Average Accuracy", "~acc", 0.01)

	// Create a train.Trainer: this object will orchestrate running the model,
	// feeding results to the optimizer, evaluating the metrics, etc.
	mlxctx = mlxctx.In("model")
	trainer := train.NewTrainer(backend, mlxctx, cnnmodel.C10ConvModel,
		losses.SparseCategoricalCrossEntropyLogits,
		optimizers.FromContext(mlxctx),
		[]metrics.Interface{movingAccuracyMetric}, // trainMetrics
		[]metrics.Interface{meanAccuracyMetric})   // evalMetrics

	// Use standard training loop.
	loop := train.NewLoop(trainer)
	commandline.AttachProgressBar(loop)

	// Checkpoint saving: every 3 minutes of training.
	if checkpoint != nil {
		period := time.Minute * 3
		train.PeriodicCallback(loop, period, true, "saving checkpoint", 100,
			func(loop *train.Loop, metrics []*tensors.Tensor) error {
				return checkpoint.Save()
			})
	}

	// Loop for given number of steps.
	numTrainSteps := mlxcontext.GetParamOr(mlxctx, "train_steps", 0)
	globalStep := int(optimizers.GetGlobalStep(mlxctx))
	if globalStep > 0 {
		trainer.SetContext(mlxctx.Reuse())
	}
	if globalStep < numTrainSteps {
		// The checkpoint has fewer steps, run more until train_steps is reached
		_ = must.M1(loop.RunSteps(trainDS, numTrainSteps-globalStep))
		fmt.Printf("\t[Step %d] median train step: %d microseconds\n",
			loop.LoopStep, loop.MedianTrainStepDuration().Microseconds())
	} else {
		fmt.Printf("\t - target train_steps=%d already reached. To train further, set a number additional "+
			"to current global step.\n", numTrainSteps)
	}

	// Finally, print an evaluation on train and test datasets.
	fmt.Println()
	must.M(commandline.ReportEval(trainer, testDS))
}

func main() {
	flag.Parse()

	// Create a new GoMLX context, and set hyperparameters.
	mlxctx := mlxcontext.New()
	mlxctx.RngStateReset()
	mlxctx.SetParams(map[string]any{
		"model":       "cnn",
		"train_steps": *flagNumSteps,

		// batch_size for training.
		"batch_size": 64,

		// eval_batch_size can be larger than training, it's more efficient.
		"eval_batch_size": 200,

		layers.ParamNormalization:    "none",
		optimizers.ParamOptimizer:    "adamw",
		optimizers.ParamLearningRate: 1e-3,
		optimizers.ParamAdamEpsilon:  1e-7,
		optimizers.ParamAdamDType:    "",
		activations.ParamActivation:  "swish",
		layers.ParamDropoutRate:      0.0,
		regularizers.ParamL2:         0.0,
		regularizers.ParamL1:         0.0,
	})

	// Train a model - this saves checkpoints on disk
	trainModel(mlxctx, *flagDataDir, *flagCheckpoint)
}
