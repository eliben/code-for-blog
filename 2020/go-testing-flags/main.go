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
}

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
	return &conf, "", nil
}

func doWork(config *Config) {
	fmt.Printf("config = %+v\n", *config)
}

func main() {
	conf, output, err := parseFlags(os.Args[0], os.Args[1:])
	if err != nil {
		fmt.Println("got error:", err)
		fmt.Println(output)
		os.Exit(1)
	} else if conf != nil {
		doWork(conf)
	}
}
