package main

import (
	"bytes"
	"flag"
	"fmt"
	"os"
)

type Config struct {
	verbose  bool
	greeting string
	level    int

	// args is the non-flag command-line arguments ("positional" arguments).
	args []string
}

// parseFlags parses the command-line arguments provided to the program.
// Typically os.Args[0] is provided as 'progname' and os.args[1:] as 'args'.
// Returns the Config in case parsing succeeded, or an error. In any case, the
// output of the flag.Parse is returned in outpout.
// A special case is help requests with -h or -help: in this case the error
// flag.ErrHelp is returned and output will contain the usage message.
func parseFlags(progname string, args []string) (config *Config, output string, err error) {
	flags := flag.NewFlagSet(progname, flag.ContinueOnError)
	var buf bytes.Buffer
	flags.SetOutput(&buf)

	var conf Config
	flags.BoolVar(&conf.verbose, "verbose", false, "set verbosity")
	flags.StringVar(&conf.greeting, "greeting", "", "set greeting")
	flags.IntVar(&conf.level, "level", 0, "set level")

	err = flags.Parse(args)
	if err != nil {
		return nil, buf.String(), err
	}
	conf.args = flags.Args()
	return &conf, "", nil
}

func doWork(config *Config) {
	fmt.Printf("config = %+v\n", *config)
}

func main() {
	conf, output, err := parseFlags(os.Args[0], os.Args[1:])
	if err == flag.ErrHelp {
		fmt.Println(output)
		os.Exit(2)
	} else if err != nil {
		fmt.Println("got error:", err)
		fmt.Println("output:\n", output)
		os.Exit(1)
	} else if conf != nil {
		doWork(conf)
	}
}
