package mypack

//go:generate samplegentool arg1 "multiword arg"

func PackFunc() string {
	return "mymod/mypack.PackFunc"
}
